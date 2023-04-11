program tuto6;

uses
  Classes, SysUtils, StrUtils,
  fpdf;

type

  { TMyFPDF }

  TMyFPDF = class(TFPDF)
  protected
   HREF: String;
   FS: String;

   function FindNextTagPos(const AHtml: String; out ATag: String; OffSet: Integer): Integer;
   procedure WriteHTML(const AHtml: String);
   procedure OpenTag(const ATag: String);
   procedure CloseTag(const ATag: String);
   procedure SetStyle(const ATag: String; Enable: Boolean);
   procedure PutLink(const AURL, AText: String);
  public
    constructor Create;

  end;

{ TMyFPDF }

constructor TMyFPDF.Create;
begin
  inherited Create;
  HREF := '';
  FS := '';
end;

function TMyFPDF.FindNextTagPos(const AHtml: String; out ATag: String; OffSet: Integer): Integer;
var
  p1, p2: Integer;
begin
  ATag := '';
  p1 := PosEx('<', AHtml, OffSet);
  if (p1 > 0) then
  begin
     p2 := PosEx('>', AHtml, p1);
     if (p2 > 0) then
       ATag := copy(AHtml, p1+1, p2-p1-1) ;
  end;

  Result := p1;
end;

procedure TMyFPDF.WriteHTML(const AHtml: String);
var
  s, ATag, AText: String;
  l, lt, p1, p2: Integer;
begin
  // HTML parser
  s := StringReplace(AHtml, #13+#10, ' ', [rfReplaceAll]);
  s := StringReplace(s, #10, ' ', [rfReplaceAll]);
  l := Length(s);
  p1 := 1; p2 := 0;
  while p1 <= l do
  begin
    p2 := FindNextTagPos(AHtml, ATag, p1);
    if (p2 = 0) then
      p2 := l+1;

    AText := copy(AHtml, p1, p2-p1);
    if (AText <> '') then
    begin
      if (Self.HREF <> '') then
        PutLink(Self.HREF, AText)
      else
        Write(5, AText);
    end;

    if (ATag <> '') then
    begin
      lt := Length(ATag);
      if ATag[1] = '/' then
      begin
        Delete(ATag, 1, 1);
        CloseTag(ATag);
      end
      else
        OpenTag(ATag);

      Inc(p2, lt+2);
    end;

    p1 := p2;
  end;
end;

procedure TMyFPDF.OpenTag(const ATag: String);
var
  s: String;
  p1, p2: Integer;
begin
  // Opening tag
  s := UpperCase(ATag);
  if ((s = 'B') or (s = 'I') or (s = 'U')) then
    SetStyle(s, true)
  else if (s = 'BR') then
    Ln(5)
  else if (s[1] = 'A') then
  begin
    p1 := Pos('href="', ATag);
    if (p1 > 0) then
    begin
      Inc(p1, 6);
      p2 := PosEx('"', ATag+'"', p1+1);
      Self.HREF := copy(ATag, p1, p2-p1);
    end;
  end;
end;

procedure TMyFPDF.CloseTag(const ATag: String);
var
  s: String;
begin
  // Closing tag
  s := UpperCase(ATag);
  if ((s = 'B') or (s = 'I') or (s = 'U')) then
    SetStyle(s, False)
  else if (s = 'A') then
    Self.HREF := '';
end;

procedure TMyFPDF.SetStyle(const ATag: String; Enable: Boolean);
var
  p: Integer;
begin
  p := pos(ATag, Self.FS);
  if Enable and (p = 0) then
    Self.FS := Self.FS + ATag
  else if (not Enable) and (p > 0) then
    Delete(Self.FS, P, Length(ATag));

  SetFont('',Self.FS);
end;

procedure TMyFPDF.PutLink(const AURL, AText: String);
begin
  // Put a hyperlink
  SetTextColor(0, 0, 255);
  SetStyle('U', true);
  Write(5, AText, AURL);
  SetStyle('U', false);
  SetTextColor(0);
end;

var
  pdf: TMyFPDF;
  AHtml: String;
  link: Integer;

begin
   AHtml := 'You can now easily print text mixing different styles: <b>bold</b>, <i>italic</i>, '+
            '<u>underlined</u>, or <b><i><u>all at once</u></i></b>!<br><br>You can also insert links on '+
            'text, such as <a href="http://www.fpdf.org">www.fpdf.org</a>, or on an image: click on the logo.';

  pdf := TMyFPDF.Create;
  try
    // First page
    pdf.AddPage();
    pdf.SetFont('Arial', '', 20);
    pdf.Write(5, 'To find out what''s new in this tutorial, click ');
    pdf.SetFont('','U');
    link := pdf.AddLink();
    pdf.Write(5, 'here', IntToStr(link));
    pdf.SetFont('');

    // Second page
    pdf.AddPage();
    pdf.SetLink(link);
    pdf.Image('logo.png', 10, 12, 30, 0, 'http://www.fpdf.org');
    pdf.SetLeftMargin(45);
    pdf.SetFontSize(14);
    pdf.WriteHTML(AHtml);
    pdf.SaveToFile('c:\temp\tuto6-pas.pdf');
  finally
    pdf.Free;
  end;

end.

