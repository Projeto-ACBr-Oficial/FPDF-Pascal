{
  FPDF Pascal - TrueType Font Reader & Subsetter
  https://github.com/Projeto-ACBr-Oficial/FPDF-Pascal

  Le arquivos TrueType (.ttf com outlines "glyf") e extrai metricas, larguras e o
  mapa codepoint -> glyph (cmap formatos 4 e 12). Gera um subset que preserva os
  indices originais dos glyphs (adequado a CIDFontType2 + CIDToGIDMap /Identity).

  Nao suporta TTC (ttcf) nem OpenType/CFF ("OTTO"): levantam ETTFontError.
  Independente de plataforma (so SysUtils/Classes).
}

unit ttfonts;

{$I fpdf.inc}

interface

uses
  SysUtils, Classes;

{$IfDef NEXTGEN}
type
  AnsiString = RawByteString;
  AnsiChar = UTF8Char;
{$EndIf}

type
  ETTFontError = class(Exception);

  TTTFontTable = record
    Tag: AnsiString;
    Offset: Cardinal;
    Size: Cardinal;
  end;

  TTTCharGlyph = record
    Code: Cardinal;
    Glyph: Word;
  end;

  { TTTFontFile }

  TTTFontFile = class
  private
    fFileName: string;
    fData: array of Byte;
    fTables: array of TTTFontTable;
    fCharMap: array of TTTCharGlyph;   // ordenado por Code
    fHasCmap: Boolean;
    fUnitsPerEm: Integer;
    fNumGlyphs: Integer;
    fNumHMetrics: Integer;
    fLocaLong: Boolean;
    fAscent, fDescent, fCapHeight: Integer;       // escala 1000
    fXMin, fYMin, fXMax, fYMax: Integer;          // escala 1000
    fItalicAngle: Double;
    fIsFixedPitch: Boolean;
    fUnderlinePosition, fUnderlineThickness: Integer;  // escala 1000
    fWeightClass: Integer;
    fPostScriptName: string;

    function U8(AOfs: Cardinal): Cardinal;
    function U16(AOfs: Cardinal): Cardinal;
    function S16(AOfs: Cardinal): Integer;
    function U32(AOfs: Cardinal): Cardinal;
    function FindTable(const ATag: AnsiString; out ATable: TTTFontTable): Boolean;
    function RequireTable(const ATag: AnsiString): TTTFontTable;
    function Scale1000(AValue: Integer): Integer;
    procedure ParseDirectory;
    procedure ParseHead;
    procedure ParseHhea;
    procedure ParseMaxp;
    procedure ParseOS2;
    procedure ParsePost;
    procedure ParseName;
    procedure ParseCmap;
    procedure ReadCmapFormat4(AOfs: Cardinal);
    procedure ReadCmapFormat12(AOfs: Cardinal);
    procedure AddChar(ACode: Cardinal; AGlyph: Word);
    procedure SortCharMap;
    function GlyphRange(AGlyph: Integer; const ALoca, AGlyf: TTTFontTable;
      out AStart, ASize: Cardinal): Boolean;
  public
    constructor Create;

    // Carrega o arquivo e le head, hhea, maxp, hmtx, OS/2, post, name e cmap.
    procedure GetMetrics(const AFileName: string);
    procedure LoadFromStream(AStream: TStream);

    // Indice do glyph de um codepoint (0 = .notdef se nao existe)
    function GlyphIndex(ACodepoint: Cardinal): Integer;
    // Largura de um glyph / codepoint na escala PDF (1000 unidades por em)
    function GlyphWidth(AGlyph: Integer): Integer;
    function CharWidth(ACodepoint: Cardinal): Integer;

    // TTF reduzido contendo so os glyphs dos codepoints (+ .notdef e componentes
    // de glyphs compostos). Os indices dos glyphs NAO mudam.
    function MakeSubset(const ACodepoints: array of Cardinal): AnsiString;

    function IsFixedPitchFont: Boolean;

    property FileName: string read fFileName;
    property UnitsPerEm: Integer read fUnitsPerEm;
    property NumGlyphs: Integer read fNumGlyphs;
    property Ascent: Integer read fAscent;          // escala 1000
    property Descent: Integer read fDescent;        // escala 1000 (negativo)
    property CapHeight: Integer read fCapHeight;    // escala 1000
    property XMin: Integer read fXMin;
    property YMin: Integer read fYMin;
    property XMax: Integer read fXMax;
    property YMax: Integer read fYMax;
    property ItalicAngle: Double read fItalicAngle;
    property UnderlinePosition: Integer read fUnderlinePosition;    // escala 1000
    property UnderlineThickness: Integer read fUnderlineThickness;  // escala 1000
    property WeightClass: Integer read fWeightClass;                // 100..900 (400 = normal)
    property PostScriptName: string read fPostScriptName;
  end;

implementation

const
  cSfntTrueType = $00010000;
  cSfntTrue = $74727565;       // 'true'
  cSfntTtcf = $74746366;       // 'ttcf'
  cSfntOtto = $4F54544F;       // 'OTTO'
  cHeadMagic = $5F0F3CF5;
  cEmptyTable: TTTFontTable = (Tag: ''; Offset: 0; Size: 0);

{ TTTFontFile }

constructor TTTFontFile.Create;
begin
  inherited Create;
  fUnitsPerEm := 1000;
end;

function TTTFontFile.U8(AOfs: Cardinal): Cardinal;
begin
  if AOfs >= Cardinal(Length(fData)) then
    raise ETTFontError.Create('TrueType: leitura alem do fim do arquivo');
  Result := fData[AOfs];
end;

function TTTFontFile.U16(AOfs: Cardinal): Cardinal;
begin
  Result := (U8(AOfs) shl 8) or U8(AOfs + 1);
end;

function TTTFontFile.S16(AOfs: Cardinal): Integer;
begin
  Result := Integer(U16(AOfs));
  if Result >= $8000 then
    Dec(Result, $10000);
end;

function TTTFontFile.U32(AOfs: Cardinal): Cardinal;
begin
  Result := (U16(AOfs) shl 16) or U16(AOfs + 2);
end;

function TTTFontFile.Scale1000(AValue: Integer): Integer;
begin
  Result := Round(AValue * 1000 / fUnitsPerEm);
end;

function TTTFontFile.FindTable(const ATag: AnsiString; out ATable: TTTFontTable): Boolean;
var
  i: Integer;
begin
  for i := 0 to High(fTables) do
    if fTables[i].Tag = ATag then
    begin
      ATable := fTables[i];
      Result := True;
      Exit;
    end;
  ATable.Tag := '';
  ATable.Offset := 0;
  ATable.Size := 0;
  Result := False;
end;

function TTTFontFile.RequireTable(const ATag: AnsiString): TTTFontTable;
begin
  if not FindTable(ATag, Result) then
    raise ETTFontError.Create('TrueType: tabela obrigatoria ausente: ' + string(ATag));
end;

procedure TTTFontFile.GetMetrics(const AFileName: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(fs);
  finally
    fs.Free;
  end;
  fFileName := AFileName;
end;

procedure TTTFontFile.LoadFromStream(AStream: TStream);
begin
  fFileName := '';
  SetLength(fData, AStream.Size - AStream.Position);
  if Length(fData) > 0 then
    AStream.ReadBuffer(fData[0], Length(fData));

  ParseDirectory;
  ParseHead;
  ParseHhea;
  ParseMaxp;
  ParseOS2;
  ParsePost;
  ParseName;
  ParseCmap;
end;

procedure TTTFontFile.ParseDirectory;
var
  ver, p: Cardinal;
  numTables, i: Integer;
  t: TTTFontTable;
begin
  SetLength(fTables, 0);
  ver := U32(0);
  if ver = cSfntTtcf then
    raise ETTFontError.Create('TrueType: colecoes TTC nao sao suportadas');
  if ver = cSfntOtto then
    raise ETTFontError.Create('TrueType: fontes OpenType/CFF nao sao suportadas (use TTF com glyf)');
  if (ver <> cSfntTrueType) and (ver <> cSfntTrue) then
    raise ETTFontError.Create('TrueType: arquivo de fonte invalido');

  numTables := Integer(U16(4));
  SetLength(fTables, numTables);
  for i := 0 to numTables - 1 do
  begin
    p := 12 + Cardinal(i) * 16;
    t.Tag := AnsiChar(U8(p)) + AnsiChar(U8(p + 1)) + AnsiChar(U8(p + 2)) + AnsiChar(U8(p + 3));
    t.Offset := U32(p + 8);
    t.Size := U32(p + 12);
    if (t.Offset > Cardinal(Length(fData))) or (t.Size > Cardinal(Length(fData)) - t.Offset) then
      raise ETTFontError.Create('TrueType: tabela fora do arquivo: ' + string(t.Tag));
    fTables[i] := t;
  end;

  if not FindTable('glyf', t) then
    raise ETTFontError.Create('TrueType: sem tabela glyf (nao e TrueType outline)');
end;

procedure TTTFontFile.ParseHead;
var
  t: TTTFontTable;
begin
  t := RequireTable('head');
  if (t.Size < 54) or (U32(t.Offset + 12) <> cHeadMagic) then
    raise ETTFontError.Create('TrueType: tabela head invalida');
  fUnitsPerEm := Integer(U16(t.Offset + 18));
  if fUnitsPerEm <= 0 then
    fUnitsPerEm := 1000;
  fXMin := Scale1000(S16(t.Offset + 36));
  fYMin := Scale1000(S16(t.Offset + 38));
  fXMax := Scale1000(S16(t.Offset + 40));
  fYMax := Scale1000(S16(t.Offset + 42));
  fLocaLong := S16(t.Offset + 50) <> 0;
end;

procedure TTTFontFile.ParseHhea;
var
  t: TTTFontTable;
begin
  t := RequireTable('hhea');
  if t.Size < 36 then
    raise ETTFontError.Create('TrueType: tabela hhea invalida');
  fAscent := Scale1000(S16(t.Offset + 4));
  fDescent := Scale1000(S16(t.Offset + 6));
  fNumHMetrics := Integer(U16(t.Offset + 34));
  fCapHeight := fAscent;
end;

procedure TTTFontFile.ParseMaxp;
var
  t: TTTFontTable;
begin
  t := RequireTable('maxp');
  fNumGlyphs := Integer(U16(t.Offset + 4));
  RequireTable('hmtx');
  RequireTable('loca');
end;

procedure TTTFontFile.ParseOS2;
var
  t: TTTFontTable;
begin
  t := cEmptyTable;
  fWeightClass := 400;
  if FindTable('OS/2', t) and (t.Size >= 6) then
  begin
    fWeightClass := Integer(U16(t.Offset + 4));
    if (fWeightClass < 1) or (fWeightClass > 1000) then
      fWeightClass := 400;
  end;
  if FindTable('OS/2', t) and (t.Size >= 90) and (U16(t.Offset) >= 2) then
  begin
    fCapHeight := Scale1000(S16(t.Offset + 88));
    if fCapHeight <= 0 then
      fCapHeight := fAscent;
  end;
end;

procedure TTTFontFile.ParsePost;
var
  t: TTTFontTable;
begin
  t := cEmptyTable;
  fItalicAngle := 0;
  fIsFixedPitch := False;
  fUnderlinePosition := -100;
  fUnderlineThickness := 50;
  if FindTable('post', t) and (t.Size >= 16) then
  begin
    fItalicAngle := (Integer(U32(t.Offset + 4))) / 65536;
    fUnderlinePosition := Scale1000(S16(t.Offset + 8));
    fUnderlineThickness := Scale1000(S16(t.Offset + 10));
    if fUnderlineThickness <= 0 then
      fUnderlineThickness := 50;
    fIsFixedPitch := U32(t.Offset + 12) <> 0;
  end;
end;

procedure TTTFontFile.ParseName;
var
  t: TTTFontTable;
  count, i: Integer;
  rec, platform, nameId, len, ofs, j: Cardinal;
  s: string;
begin
  t := cEmptyTable;
  fPostScriptName := '';
  if not FindTable('name', t) or (t.Size < 6) then
    Exit;

  count := Integer(U16(t.Offset + 2));
  for i := 0 to count - 1 do
  begin
    rec := t.Offset + 6 + Cardinal(i) * 12;
    platform := U16(rec);
    nameId := U16(rec + 6);
    if nameId <> 6 then
      Continue;
    len := U16(rec + 8);
    ofs := t.Offset + U16(t.Offset + 4) + U16(rec + 10);
    s := '';
    if platform = 3 then
      for j := 0 to Integer(len div 2) - 1 do
        s := s + Chr(U16(ofs + j * 2) and $7F)
    else if platform = 1 then
      for j := 0 to Integer(len) - 1 do
        s := s + Chr(U8(ofs + j) and $7F)
    else
      Continue;
    fPostScriptName := s;
    if platform = 3 then
      Break;
  end;
end;

procedure TTTFontFile.AddChar(ACode: Cardinal; AGlyph: Word);
var
  n: Integer;
begin
  if AGlyph = 0 then
    Exit;
  n := Length(fCharMap);
  SetLength(fCharMap, n + 1);
  fCharMap[n].Code := ACode;
  fCharMap[n].Glyph := AGlyph;
end;

procedure TTTFontFile.ReadCmapFormat4(AOfs: Cardinal);
var
  segX2, endP, startP, deltaP, rangeP: Cardinal;
  seg: Integer;
  i, c, st, en, delta, ro, g: Cardinal;
begin
  segX2 := U16(AOfs + 6);
  endP := AOfs + 14;
  startP := endP + segX2 + 2;
  deltaP := startP + segX2;
  rangeP := deltaP + segX2;
  for seg := 0 to Integer(segX2 div 2) - 1 do
  begin
    en := U16(endP + Cardinal(seg) * 2);
    st := U16(startP + Cardinal(seg) * 2);
    delta := U16(deltaP + Cardinal(seg) * 2);
    ro := U16(rangeP + Cardinal(seg) * 2);
    if (st > en) or (st = $FFFF) then
      Continue;
    for c := st to en do
    begin
      if c = $FFFF then
        Break;
      if ro = 0 then
        g := (c + delta) and $FFFF
      else
      begin
        i := rangeP + Cardinal(seg) * 2 + ro + (c - st) * 2;
        g := U16(i);
        if g <> 0 then
          g := (g + delta) and $FFFF;
      end;
      AddChar(c, Word(g));
    end;
  end;
end;

procedure TTTFontFile.ReadCmapFormat12(AOfs: Cardinal);
var
  nGroups: Cardinal;
  i: Integer;
  c, st, en, gl: Cardinal;
begin
  nGroups := U32(AOfs + 12);
  for i := 0 to Integer(nGroups) - 1 do
  begin
    st := U32(AOfs + 16 + Cardinal(i) * 12);
    en := U32(AOfs + 20 + Cardinal(i) * 12);
    gl := U32(AOfs + 24 + Cardinal(i) * 12);
    if (en < st) or (en - st > $10FFFF) then
      Continue;
    for c := st to en do
      AddChar(c, Word(gl + (c - st)));
  end;
end;

procedure TTTFontFile.SortCharMap;

  procedure QSort(L, R: Integer);
  var
    i, j: Integer;
    p: Cardinal;
    t: TTTCharGlyph;
  begin
    i := L;
    j := R;
    p := fCharMap[(L + R) div 2].Code;
    repeat
      while fCharMap[i].Code < p do
        Inc(i);
      while fCharMap[j].Code > p do
        Dec(j);
      if i <= j then
      begin
        t := fCharMap[i];
        fCharMap[i] := fCharMap[j];
        fCharMap[j] := t;
        Inc(i);
        Dec(j);
      end;
    until i > j;
    if L < j then
      QSort(L, j);
    if i < R then
      QSort(i, R);
  end;

begin
  if Length(fCharMap) > 1 then
    QSort(0, High(fCharMap));
end;

procedure TTTFontFile.ParseCmap;
var
  t: TTTFontTable;
  n, i: Integer;
  plat, enc, sub, fmt, bestOfs, bestScore, score: Cardinal;
begin
  t := cEmptyTable;
  SetLength(fCharMap, 0);
  fHasCmap := False;
  if not FindTable('cmap', t) or (t.Size < 4) then
    Exit;

  n := Integer(U16(t.Offset + 2));
  bestOfs := 0;
  bestScore := 0;
  for i := 0 to n - 1 do
  begin
    plat := U16(t.Offset + 4 + Cardinal(i) * 8);
    enc := U16(t.Offset + 6 + Cardinal(i) * 8);
    sub := t.Offset + U32(t.Offset + 8 + Cardinal(i) * 8);
    fmt := U16(sub);
    if (fmt <> 4) and (fmt <> 12) then
      Continue;

    score := 0;
    if (plat = 3) and (enc = 10) then
      score := 4
    else if (plat = 0) and (fmt = 12) then
      score := 3
    else if (plat = 3) and (enc = 1) then
      score := 2
    else if plat = 0 then
      score := 1;
    if score > bestScore then
    begin
      bestScore := score;
      bestOfs := sub;
    end;
  end;

  if bestScore = 0 then
    Exit;

  if U16(bestOfs) = 12 then
    ReadCmapFormat12(bestOfs)
  else
    ReadCmapFormat4(bestOfs);
  SortCharMap;
  fHasCmap := True;
end;

function TTTFontFile.GlyphIndex(ACodepoint: Cardinal): Integer;
var
  lo, hi, mid: Integer;
begin
  Result := 0;
  lo := 0;
  hi := High(fCharMap);
  while lo <= hi do
  begin
    mid := (lo + hi) div 2;
    if fCharMap[mid].Code = ACodepoint then
    begin
      Result := fCharMap[mid].Glyph;
      Exit;
    end
    else if fCharMap[mid].Code < ACodepoint then
      lo := mid + 1
    else
      hi := mid - 1;
  end;
end;

function TTTFontFile.GlyphWidth(AGlyph: Integer): Integer;
var
  t: TTTFontTable;
  idx: Integer;
begin
  Result := 0;
  if (AGlyph < 0) or (AGlyph >= fNumGlyphs) or (fNumHMetrics <= 0) then
    Exit;
  t := RequireTable('hmtx');
  idx := AGlyph;
  if idx >= fNumHMetrics then
    idx := fNumHMetrics - 1;
  Result := Scale1000(Integer(U16(t.Offset + Cardinal(idx) * 4)));
end;

function TTTFontFile.CharWidth(ACodepoint: Cardinal): Integer;
begin
  Result := GlyphWidth(GlyphIndex(ACodepoint));
end;

function TTTFontFile.IsFixedPitchFont: Boolean;
begin
  Result := fIsFixedPitch;
end;

function TTTFontFile.GlyphRange(AGlyph: Integer; const ALoca, AGlyf: TTTFontTable;
  out AStart, ASize: Cardinal): Boolean;
var
  a, b: Cardinal;
begin
  Result := False;
  AStart := 0;
  ASize := 0;
  if (AGlyph < 0) or (AGlyph >= fNumGlyphs) then
    Exit;
  if fLocaLong then
  begin
    a := U32(ALoca.Offset + Cardinal(AGlyph) * 4);
    b := U32(ALoca.Offset + Cardinal(AGlyph) * 4 + 4);
  end
  else
  begin
    a := U16(ALoca.Offset + Cardinal(AGlyph) * 2) * 2;
    b := U16(ALoca.Offset + Cardinal(AGlyph) * 2 + 2) * 2;
  end;
  if (b < a) or (b > AGlyf.Size) then
    Exit;
  AStart := AGlyf.Offset + a;
  ASize := b - a;
  Result := True;
end;

function TTTFontFile.MakeSubset(const ACodepoints: array of Cardinal): AnsiString;
type
  TOutTable = record
    Tag: AnsiString;
    Data: AnsiString;
  end;
var
  used: array of Boolean;
  stack: array of Integer;
  loca, glyf, t: TTTFontTable;
  outTabs: array of TOutTable;
  glyfData, locaData: AnsiString;
  i, j, n, g, flags, comp: Integer;
  gs, gsz, p: Cardinal;
  headData: AnsiString;
  dirSize, off, sum, adj, entrySel, searchRange: Cardinal;
  tmp: TOutTable;

  procedure Push(AGlyph: Integer);
  begin
    if (AGlyph >= 0) and (AGlyph < fNumGlyphs) and not used[AGlyph] then
    begin
      used[AGlyph] := True;
      SetLength(stack, Length(stack) + 1);
      stack[High(stack)] := AGlyph;
    end;
  end;

  function Raw(AOfs, ASize: Cardinal): AnsiString;
  begin
    Result := '';
    SetLength(Result, ASize);
    if ASize > 0 then
      Move(fData[AOfs], Result[1], ASize);
  end;

  function BE32(V: Cardinal): AnsiString;
  begin
    Result := '';
    SetLength(Result, 4);
    Result[1] := AnsiChar(V shr 24);
    Result[2] := AnsiChar((V shr 16) and $FF);
    Result[3] := AnsiChar((V shr 8) and $FF);
    Result[4] := AnsiChar(V and $FF);
  end;

  function BE16(V: Cardinal): AnsiString;
  begin
    Result := '';
    SetLength(Result, 2);
    Result[1] := AnsiChar((V shr 8) and $FF);
    Result[2] := AnsiChar(V and $FF);
  end;

  function Checksum(const S: AnsiString): Cardinal;
  var
    k: Integer;
    w: Cardinal;
    padded: AnsiString;
  begin
    padded := S;
    while (Length(padded) mod 4) <> 0 do
      padded := padded + #0;
    Result := 0;
    k := 1;
    while k <= Length(padded) do
    begin
      w := (Cardinal(Ord(padded[k])) shl 24) or (Cardinal(Ord(padded[k + 1])) shl 16) or
           (Cardinal(Ord(padded[k + 2])) shl 8) or Cardinal(Ord(padded[k + 3]));
      Result := Result + w;
      Inc(k, 4);
    end;
  end;

  procedure AddOut(const ATag, AData: AnsiString);
  begin
    SetLength(outTabs, Length(outTabs) + 1);
    outTabs[High(outTabs)].Tag := ATag;
    outTabs[High(outTabs)].Data := AData;
  end;

  procedure CopyTable(const ATag: AnsiString);
  begin
    if FindTable(ATag, t) then
      AddOut(ATag, Raw(t.Offset, t.Size));
  end;

begin
  outTabs := nil;
  t := cEmptyTable;
  if not fHasCmap then
    raise ETTFontError.Create('TrueType: fonte sem cmap utilizavel');
  glyf := RequireTable('glyf');
  loca := RequireTable('loca');

  used := nil;
  SetLength(used, fNumGlyphs);
  SetLength(stack, 0);
  Push(0);
  for i := 0 to High(ACodepoints) do
    Push(GlyphIndex(ACodepoints[i]));

  // fecha sobre os componentes de glyphs compostos
  while Length(stack) > 0 do
  begin
    g := stack[High(stack)];
    SetLength(stack, Length(stack) - 1);
    if GlyphRange(g, loca, glyf, gs, gsz) and (gsz >= 10) and (S16(gs) < 0) then
    begin
      p := gs + 10;
      repeat
        flags := Integer(U16(p));
        comp := Integer(U16(p + 2));
        Push(comp);
        Inc(p, 4);
        if (flags and 1) <> 0 then
          Inc(p, 4)
        else
          Inc(p, 2);
        if (flags and $08) <> 0 then
          Inc(p, 2)
        else if (flags and $40) <> 0 then
          Inc(p, 4)
        else if (flags and $80) <> 0 then
          Inc(p, 8);
      until (flags and $20) = 0;
    end;
  end;

  // glyf + loca (formato longo), glyphs nao usados ficam vazios
  glyfData := '';
  locaData := '';
  SetLength(locaData, (fNumGlyphs + 1) * 4);
  for n := 0 to fNumGlyphs do
  begin
    locaData[n * 4 + 1] := AnsiChar(Length(glyfData) shr 24);
    locaData[n * 4 + 2] := AnsiChar((Length(glyfData) shr 16) and $FF);
    locaData[n * 4 + 3] := AnsiChar((Length(glyfData) shr 8) and $FF);
    locaData[n * 4 + 4] := AnsiChar(Length(glyfData) and $FF);
    if (n < fNumGlyphs) and used[n] and GlyphRange(n, loca, glyf, gs, gsz) and (gsz > 0) then
    begin
      glyfData := glyfData + Raw(gs, gsz);
      while (Length(glyfData) mod 4) <> 0 do
        glyfData := glyfData + #0;
    end;
  end;

  // head: indexToLocFormat = 1, checkSumAdjustment = 0 (preenchido no fim)
  t := RequireTable('head');
  headData := Raw(t.Offset, t.Size);
  for i := 9 to 12 do
    headData[i] := #0;
  headData[51] := #0;
  headData[52] := #1;

  AddOut('head', headData);
  CopyTable('hhea');
  CopyTable('maxp');
  CopyTable('hmtx');
  CopyTable('cvt ');
  CopyTable('fpgm');
  CopyTable('prep');
  AddOut('glyf', glyfData);
  AddOut('loca', locaData);

  // diretorio ordenado por tag
  for i := 1 to High(outTabs) do
  begin
    tmp := outTabs[i];
    j := i - 1;
    while (j >= 0) and (outTabs[j].Tag > tmp.Tag) do
    begin
      outTabs[j + 1] := outTabs[j];
      Dec(j);
    end;
    outTabs[j + 1] := tmp;
  end;

  n := Length(outTabs);
  searchRange := 1;
  entrySel := 0;
  while searchRange * 2 <= Cardinal(n) do
  begin
    searchRange := searchRange * 2;
    Inc(entrySel);
  end;

  Result := BE32(cSfntTrueType) + BE16(n) + BE16(searchRange * 16) + BE16(entrySel) +
            BE16(Cardinal(n) * 16 - searchRange * 16);

  dirSize := 12 + Cardinal(n) * 16;
  off := dirSize;
  for i := 0 to n - 1 do
  begin
    Result := Result + outTabs[i].Tag + BE32(Checksum(outTabs[i].Data)) + BE32(off) +
              BE32(Length(outTabs[i].Data));
    Inc(off, (Cardinal(Length(outTabs[i].Data)) + 3) and not Cardinal(3));
  end;

  for i := 0 to n - 1 do
  begin
    Result := Result + outTabs[i].Data;
    while (Length(Result) mod 4) <> 0 do
      Result := Result + #0;
  end;

  // checkSumAdjustment da tabela head
  sum := Checksum(Result);
  adj := Cardinal($B1B0AFBA) - sum;
  off := dirSize;
  for i := 0 to n - 1 do
  begin
    if outTabs[i].Tag = 'head' then
    begin
      Result[off + 9] := AnsiChar(adj shr 24);
      Result[off + 10] := AnsiChar((adj shr 16) and $FF);
      Result[off + 11] := AnsiChar((adj shr 8) and $FF);
      Result[off + 12] := AnsiChar(adj and $FF);
      Break;
    end;
    Inc(off, (Cardinal(Length(outTabs[i].Data)) + 3) and not Cardinal(3));
  end;
end;

end.
