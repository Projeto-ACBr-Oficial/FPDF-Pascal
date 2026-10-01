{
  FPDF Pascal - Unicode Helpers
  https://github.com/Projeto-ACBr-Oficial/FPDF-Pascal

  Funcoes puras para converter texto entre UTF-8 / UTF-16 (string nativa),
  codepoints Unicode e UTF-16BE (o que o PDF usa em fontes Type0/Identity-H).

  - FPC/Lazarus: "string" e UTF-8 (AnsiString) em todas as plataformas.
  - Delphi Unicode (2009+): "string" e UTF-16.
  - Delphi antigo (pre-Unicode): "string" e tratada como UTF-8.

  Sem dependencia de WinAPI, iconv ou codepage do sistema: o resultado e o mesmo
  em Windows, Linux, macOS e demais targets do Lazarus.
}

unit fpdf_unicode;

{$I fpdf.inc}

interface

uses
  SysUtils;

{$IfDef NEXTGEN}
type
  AnsiString = RawByteString;
  AnsiChar = UTF8Char;
{$EndIf}

type
  TUnicodeCodepoints = array of Cardinal;

const
  UNICODE_REPLACEMENT = $FFFD;

// "S" -> codepoints Unicode. Sequencias invalidas viram U+FFFD.
function UTF8StringToCodepoints(const S: string): TUnicodeCodepoints;

// Fatia de codepoints -> "string" nativa (UTF-16 no Delphi Unicode, UTF-8 no
// FPC e Delphi antigo). AStart e 0-based; ACount < 0 vai ate o fim.
function CodepointsToString(const ACodepoints: TUnicodeCodepoints;
  AStart: Integer = 0; ACount: Integer = -1): string;

// Windows-1252 <-> codepoints (pra texto que nao esta em UTF-8). Codepoints
// sem equivalente em CP1252 viram '?'.
function CP1252ToCodepoints(const S: AnsiString): TUnicodeCodepoints;
function CodepointsToCP1252(const ACodepoints: TUnicodeCodepoints;
  AStart: Integer = 0; ACount: Integer = -1): AnsiString;

// codepoints -> bytes UTF-16BE (pares substitutos pra U+10000..U+10FFFF).
function CodepointsToUTF16BE(const ACodepoints: TUnicodeCodepoints;
  AddBOM: Boolean = False): AnsiString;

// "S" -> bytes UTF-16BE (opcionalmente com BOM FE FF).
function UTF8ToUTF16BE(const S: string; AddBOM: Boolean = False): AnsiString;

// bytes -> string hexadecimal maiuscula ("<...>" fica por conta de quem chama).
function HexEncode(const Data: AnsiString): string;

// Escapa \ ( ) e CR/LF pra uso em string literal PDF "(...)".
function EscapePDFString(const Data: AnsiString): AnsiString;

// Largura em unidades da fonte -> escala PDF de 1000 unidades por em.
function CodepointToWidth(AWidthUnits, AUnitsPerEm: Integer): Integer;

implementation

procedure AddCodepoint(var A: TUnicodeCodepoints; var N: Integer; C: Cardinal);
begin
  if N >= Length(A) then
    SetLength(A, Length(A) + 16 + Length(A) div 2);
  A[N] := C;
  Inc(N);
end;

function UTF8StringToCodepoints(const S: string): TUnicodeCodepoints;
var
  n, i, len: Integer;
{$IfDef UNICODE}
  c, c2: Cardinal;
{$Else}
  cnt, j: Integer;
  b: Byte;
  cp, minCp: Cardinal;
  ok: Boolean;
{$EndIf}
begin
  Result := nil;
  n := 0;
  len := Length(S);
  SetLength(Result, len);

  {$IfDef UNICODE}
  // string em UTF-16 (Delphi 2009+)
  i := 1;
  while i <= len do
  begin
    c := Ord(S[i]);
    Inc(i);
    if (c >= $D800) and (c <= $DBFF) then
    begin
      if (i <= len) and (Ord(S[i]) >= $DC00) and (Ord(S[i]) <= $DFFF) then
      begin
        c2 := Ord(S[i]);
        Inc(i);
        c := $10000 + ((c - $D800) shl 10) + (c2 - $DC00);
      end
      else
        c := UNICODE_REPLACEMENT;
    end
    else if (c >= $DC00) and (c <= $DFFF) then
      c := UNICODE_REPLACEMENT;
    AddCodepoint(Result, n, c);
  end;
  {$Else}
  // string em UTF-8 (FPC e Delphi pre-Unicode)
  i := 1;
  while i <= len do
  begin
    b := Ord(S[i]);
    if b < $80 then
    begin
      AddCodepoint(Result, n, b);
      Inc(i);
      Continue;
    end;

    cnt := 0;
    cp := 0;
    minCp := 0;
    if (b and $E0) = $C0 then
    begin
      cnt := 1;
      cp := b and $1F;
      minCp := $80;
    end
    else if (b and $F0) = $E0 then
    begin
      cnt := 2;
      cp := b and $0F;
      minCp := $800;
    end
    else if (b and $F8) = $F0 then
    begin
      cnt := 3;
      cp := b and $07;
      minCp := $10000;
    end;

    ok := (cnt > 0) and (i + cnt <= len);
    if ok then
      for j := 1 to cnt do
      begin
        b := Ord(S[i + j]);
        if (b and $C0) <> $80 then
        begin
          ok := False;
          Break;
        end;
        cp := (cp shl 6) or (b and $3F);
      end;

    if ok and ((cp < minCp) or (cp > $10FFFF) or ((cp >= $D800) and (cp <= $DFFF))) then
      ok := False;

    if ok then
    begin
      AddCodepoint(Result, n, cp);
      Inc(i, cnt + 1);
    end
    else
    begin
      AddCodepoint(Result, n, UNICODE_REPLACEMENT);
      Inc(i);
    end;
  end;
  {$EndIf}

  SetLength(Result, n);
end;

function CodepointsToUTF16BE(const ACodepoints: TUnicodeCodepoints;
  AddBOM: Boolean): AnsiString;
var
  i, p: Integer;
  c: Cardinal;

  procedure PutWord(W: Cardinal);
  begin
    Result[p] := AnsiChar(W shr 8);
    Result[p + 1] := AnsiChar(W and $FF);
    Inc(p, 2);
  end;

begin
  Result := '';
  SetLength(Result, 2 + Length(ACodepoints) * 4);
  p := 1;
  if AddBOM then
    PutWord($FEFF);

  for i := 0 to High(ACodepoints) do
  begin
    c := ACodepoints[i];
    if (c > $10FFFF) or ((c >= $D800) and (c <= $DFFF)) then
      c := UNICODE_REPLACEMENT;

    if c >= $10000 then
    begin
      Dec(c, $10000);
      PutWord($D800 + (c shr 10));
      PutWord($DC00 + (c and $3FF));
    end
    else
      PutWord(c);
  end;

  SetLength(Result, p - 1);
end;

const
  Cp1252High: array[$80..$9F] of Word = (
    $20AC, 0, $201A, $0192, $201E, $2026, $2020, $2021,
    $02C6, $2030, $0160, $2039, $0152, 0, $017D, 0,
    0, $2018, $2019, $201C, $201D, $2022, $2013, $2014,
    $02DC, $2122, $0161, $203A, $0153, 0, $017E, $0178);

procedure ClampSlice(ALen: Integer; var AStart, ACount: Integer);
begin
  if AStart < 0 then
    AStart := 0;
  if (ACount < 0) or (AStart + ACount > ALen) then
    ACount := ALen - AStart;
  if ACount < 0 then
    ACount := 0;
end;

function CodepointsToString(const ACodepoints: TUnicodeCodepoints;
  AStart, ACount: Integer): string;
var
  i, n: Integer;
  c: Cardinal;
begin
  ClampSlice(Length(ACodepoints), AStart, ACount);
  {$IfDef UNICODE}
  SetLength(Result, ACount * 2);
  n := 0;
  for i := AStart to AStart + ACount - 1 do
  begin
    c := ACodepoints[i];
    if (c > $10FFFF) or ((c >= $D800) and (c <= $DFFF)) then
      c := UNICODE_REPLACEMENT;
    if c >= $10000 then
    begin
      Dec(c, $10000);
      Inc(n);
      Result[n] := Char($D800 + (c shr 10));
      Inc(n);
      Result[n] := Char($DC00 + (c and $3FF));
    end
    else
    begin
      Inc(n);
      Result[n] := Char(c);
    end;
  end;
  {$Else}
  Result := '';
  SetLength(Result, ACount * 4);
  n := 0;
  for i := AStart to AStart + ACount - 1 do
  begin
    c := ACodepoints[i];
    if (c > $10FFFF) or ((c >= $D800) and (c <= $DFFF)) then
      c := UNICODE_REPLACEMENT;
    if c < $80 then
    begin
      Inc(n);
      Result[n] := Char(c);
    end
    else if c < $800 then
    begin
      Result[n + 1] := Char($C0 or (c shr 6));
      Result[n + 2] := Char($80 or (c and $3F));
      Inc(n, 2);
    end
    else if c < $10000 then
    begin
      Result[n + 1] := Char($E0 or (c shr 12));
      Result[n + 2] := Char($80 or ((c shr 6) and $3F));
      Result[n + 3] := Char($80 or (c and $3F));
      Inc(n, 3);
    end
    else
    begin
      Result[n + 1] := Char($F0 or (c shr 18));
      Result[n + 2] := Char($80 or ((c shr 12) and $3F));
      Result[n + 3] := Char($80 or ((c shr 6) and $3F));
      Result[n + 4] := Char($80 or (c and $3F));
      Inc(n, 4);
    end;
  end;
  {$EndIf}
  SetLength(Result, n);
end;

function CP1252ToCodepoints(const S: AnsiString): TUnicodeCodepoints;
var
  i: Integer;
  b: Byte;
begin
  Result := nil;
  SetLength(Result, Length(S));
  for i := 1 to Length(S) do
  begin
    b := Ord(S[i]);
    if (b >= $80) and (b <= $9F) and (Cp1252High[b] <> 0) then
      Result[i - 1] := Cp1252High[b]
    else
      Result[i - 1] := b;
  end;
end;

function CodepointsToCP1252(const ACodepoints: TUnicodeCodepoints;
  AStart, ACount: Integer): AnsiString;
var
  i, b: Integer;
  c: Cardinal;
begin
  ClampSlice(Length(ACodepoints), AStart, ACount);
  Result := '';
  SetLength(Result, ACount);
  for i := 0 to ACount - 1 do
  begin
    c := ACodepoints[AStart + i];
    if (c < $80) or ((c >= $A0) and (c <= $FF)) then
      Result[i + 1] := AnsiChar(c)
    else
    begin
      Result[i + 1] := '?';
      for b := $80 to $9F do
        if (Cp1252High[b] <> 0) and (Cp1252High[b] = c) then
        begin
          Result[i + 1] := AnsiChar(b);
          Break;
        end;
    end;
  end;
end;

function UTF8ToUTF16BE(const S: string; AddBOM: Boolean): AnsiString;
begin
  Result := CodepointsToUTF16BE(UTF8StringToCodepoints(S), AddBOM);
end;

function HexEncode(const Data: AnsiString): string;
const
  Hex: array[0..15] of Char = '0123456789ABCDEF';
var
  i: Integer;
  b: Byte;
begin
  Result := '';
  SetLength(Result, Length(Data) * 2);
  for i := 1 to Length(Data) do
  begin
    b := Ord(Data[i]);
    Result[i * 2 - 1] := Hex[b shr 4];
    Result[i * 2] := Hex[b and $F];
  end;
end;

function EscapePDFString(const Data: AnsiString): AnsiString;
var
  i, n: Integer;
  c: AnsiChar;
begin
  Result := '';
  SetLength(Result, Length(Data) * 2);
  n := 0;
  for i := 1 to Length(Data) do
  begin
    c := Data[i];
    case c of
      '\', '(', ')':
        begin
          Inc(n);
          Result[n] := '\';
          Inc(n);
          Result[n] := c;
        end;
      #13:
        begin
          Inc(n);
          Result[n] := '\';
          Inc(n);
          Result[n] := 'r';
        end;
      #10:
        begin
          Inc(n);
          Result[n] := '\';
          Inc(n);
          Result[n] := 'n';
        end;
    else
      Inc(n);
      Result[n] := c;
    end;
  end;
  SetLength(Result, n);
end;

function CodepointToWidth(AWidthUnits, AUnitsPerEm: Integer): Integer;
begin
  if AUnitsPerEm <= 0 then
    AUnitsPerEm := 1000;
  Result := Round(AWidthUnits * 1000 / AUnitsPerEm);
end;

end.
