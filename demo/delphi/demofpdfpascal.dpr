program demofpdfpascal;

uses
  Classes, sysutils,
  fpdf, fpdf_ext;

const
  cGITURL = 'https://github.com/Projeto-ACBr-Oficial/FPDF-Pascal';

type

  { TPDFDemo }

  TPDFDemo = class
  protected
    pdf: TFPDFExt;
    PageTitle: String;
    fpdir: String;
    fpfiledir: String;
    fpl1, fpl2, fpl3, fpl4, fpl5, fpl6, fpl7: Integer;

    procedure PrintIndex;
    procedure PrintFontsTest;
    procedure PrintImagesTest;
    procedure PrintColosrTest;
    procedure PrintShapesTest;
    procedure PrintBarCodesTest;
    procedure PrintALargeTextTest;
    procedure PrintLayersTest;
  public
    constructor Create;
    procedure CreatePDF;
    procedure PrintHeader(APDF: TFPDF);
    procedure PrintFooter(APDF: TFPDF);
  end;
var
  demo: TPDFDemo;
  hh: TDateTime;

{ TPDFDemo }

procedure TPDFDemo.PrintIndex;
begin
  if not Assigned(pdf) then
    Exit;

  PageTitle := 'FPDF Pascal Test - Index, Links Test';
  pdf.AddPage;

  fpl1 := pdf.AddLink();
  fpl2 := pdf.AddLink();
  fpl3 := pdf.AddLink();
  fpl4 := pdf.AddLink();
  fpl5 := pdf.AddLink();
  fpl6 := pdf.AddLink();
  fpl7 := pdf.AddLink();

  pdf.SetFont('Helvetica','',14);
  pdf.Write(4, '1 - ');
  pdf.SetFont('Helvetica','U',14);
  pdf.Write(4, 'Fonts Test', IntToStr(fpl1));
  pdf.Ln(8);
  pdf.SetFont('Helvetica','',14);
  pdf.Write(4, '2 - ');
  pdf.SetFont('Helvetica','U',14);
  pdf.Write(4, 'Images Test', IntToStr(fpl2));
  pdf.Ln(8);
  pdf.SetFont('Helvetica','',14);
  pdf.Write(4, '3 - ');
  pdf.SetFont('Helvetica','U',14);
  pdf.Write(4, 'Color Test', IntToStr(fpl3));
  pdf.Ln(8);
  pdf.SetFont('Helvetica','',14);
  pdf.Write(4, '4 - ');
  pdf.SetFont('Helvetica','U',14);
  pdf.Write(4, 'Shapes Test', IntToStr(fpl4));
  pdf.Ln(8);
  pdf.SetFont('Helvetica','',14);
  pdf.Write(4, '5 - ');
  pdf.SetFont('Helvetica','U',14);
  pdf.Write(4, 'BarCodes Test', IntToStr(fpl5));
  pdf.Ln(8);
  pdf.SetFont('Helvetica','',14);
  pdf.Write(4, '6 - ');
  pdf.SetFont('Helvetica','U',14);
  pdf.Write(4, 'Large Text Test', IntToStr(fpl6));
  pdf.Ln(8);
  pdf.SetFont('Helvetica','',14);
  pdf.Write(4, '7 - ');
  pdf.SetFont('Helvetica','U',14);
  pdf.Write(4, 'Layers Test', IntToStr(fpl7));
  pdf.Ln(16);
  pdf.Cell(0, 10, 'FPDF Pascal on GitHub', 'TB', 0, 'C', False, cGITURL);
  pdf.Ln(8);

end;

procedure TPDFDemo.PrintFontsTest;
var
  AllChars: AnsiString;
  i: Integer;
begin
  if not Assigned(pdf) then
    Exit;

  AllChars := '';
  for i := 0 to 255 do
    AllChars := AllChars + chr(i)+' ';

  PageTitle := 'FPDF Pascal Test - Fonts Test';
  pdf.SetUTF8(False);
  pdf.AddPage;
  pdf.SetLink(fpl1);
  pdf.SetFont('Courier','',14);
  pdf.Write(6.5,'Courier     ');
  pdf.SetFont('Courier','B',14);
  pdf.Write(6.5,'Courier - Bold     ');
  pdf.SetFont('Courier','BI',14);
  pdf.Write(6.5,'Courier - Bold Italic');
  pdf.Ln;
  pdf.SetFont('Courier', '', 10);
  pdf.MultiCell(0, 5, AllChars);
  pdf.Ln;

  pdf.Ln;
  pdf.SetFont('Helvetica','',14);
  pdf.Write(6.5,'Helvetica     ');
  pdf.SetFont('Helvetica','B');
  pdf.Write(6.5,'Helvetica - Bold     ');
  pdf.SetFont('Helvetica','BI');
  pdf.Write(6.5,'Helvetica - Bold Italic');
  pdf.Ln;
  pdf.SetFont('Helvetica', '', 10);
  pdf.MultiCell(0, 5, AllChars);
  pdf.Ln;

  pdf.Ln;
  pdf.SetFont('Times','',14);
  pdf.Write(6.5,'Times Roman     ');
  pdf.SetFont('Times','B');
  pdf.Write(6.5,'Times - Bold     ');
  pdf.SetFont('Times','BI');
  pdf.Write(6.5,'Times - Bold Italic');
  pdf.Ln;
  pdf.SetFont('Times', '', 10);
  pdf.MultiCell(0, 5, AllChars);
  pdf.Ln;

  pdf.Ln;
  pdf.SetFont('Helvetica','',14);
  pdf.Write(6.5,'Symbol');
  pdf.Ln;
  pdf.SetFont('Symbol','', 10);
  pdf.MultiCell(0, 5, AllChars);
  pdf.Ln;

  pdf.Ln;
  pdf.SetFont('Helvetica','',14);
  pdf.Write(6.5,'ZapfDingbats');
  pdf.Ln;
  pdf.SetFont('ZapfDingbats', '', 10);
  pdf.MultiCell(0, 5, AllChars);
  pdf.Ln;

  pdf.SetFont('Helvetica','',14);
end;

procedure TPDFDemo.PrintImagesTest;
var
  i, j: Integer;
  acbrlogo: String;
begin
  if not Assigned(pdf) then
    Exit;

  PageTitle := 'FPDF Pascal Test - Images Test';
  pdf.AddPage;
  pdf.SetLink(fpl2);
  pdf.SetFont('Helvetica','',8);

  pdf.Image(fpfiledir+'lena.jpg', 10, 30, 80, 80);
  pdf.Image(fpfiledir+'parrots-24.jpeg', 95, 35);

  pdf.SetXY(10, 110);
  pdf.Write(8, 'lena.jpg - 512 x 512 pixels, 72 dpi, 8 bits');
  pdf.SetXY(95, 103);
  pdf.Write(8,'parrots-24.jpeg - 386 x 256 pixels, 3 dpi, 24 bits');

  pdf.Image(fpfiledir+'image1.jpg', 10, 122, 80);
  pdf.SetXY(10, 195);
  pdf.Write(8,'image1.jpg - 680 x 632 pixels, 96 dpi, 24 bits');

  for i := 0 to 3 do
    for j := 0 to 3 do
      pdf.Image(fpfiledir+'logo.png', 95 + (i*26), 122 + (j*18));

  // URL or File
  //acbrlogo := 'https://projetoacbr.com.br/wp-content/uploads/2021/06/acbr.png';
  acbrlogo := fpfiledir+'acbr.png';
  pdf.Rotate(45, 110, 120);
  pdf.Image(acbrlogo, 75, 140, 80);
  pdf.Rotate(0);

  pdf.SetXY(127, 195);
  pdf.Write(8,'logo.png - 104 x 71 pixels, 8 bits');
  pdf.Rotate(45);
  pdf.SetXY(137, 157);
  pdf.Write(8,'acbr.png - 800 x 800 pixels, 32 bits. transparent');

end;

procedure TPDFDemo.PrintColosrTest;
var
  r, g, b, s, c, t: Integer;
  x, y, d: Double;
begin
  PageTitle := 'FPDF Pascal Test - Colors Test';
  pdf.AddPage;
  pdf.SetLink(fpl3);
  pdf.SetFont('Helvetica','',8);

  t := 30;
  c := 30;
  d := 1.3;
  s := 10;
  x := c;
  y := t;
  r := 0;
  while (r < 255) do
  begin
    g := 0;
    while (g < 255) do
    begin
      b := 0;
      while (b < 255) do
      begin
        pdf.SetFillColor(r,g,b);
        pdf.Rect(x, y, d, d, 'F');
        x := x + d;
        Inc(b, s);
      end;
      y := y + d;
      x := c;
      Inc(g, s);
    end;

    y := y + d;
    if (y > 260) then
    begin
      y := t;
      c := c + Trunc((Trunc(255/s) * d) + d*2);
      x := c;
    end;

    Inc(r, s);
  end;
end;

procedure TPDFDemo.PrintShapesTest;
begin
  PageTitle := 'FPDF Pascal Test - Shapes Test';
  pdf.AddPage;
  pdf.SetLink(fpl4);
  pdf.SetFont('Helvetica','',14);

  pdf.SetDrawColor(cBlack);
  PDF.SetLineWidth(0.3);

  pdf.SetFillColor(cRed);
  pdf.Rect(65, 50, 50, 50, 'DF');
  pdf.SetFillColor(cGreen);
  pdf.Rect(75, 60, 50, 50, 'DF');
  pdf.SetFillColor(cBlue);
  pdf.Rect(85, 70, 50, 50, 'DF');

  pdf.SetFillColor(cRed);
  pdf.RoundedRect(15, 130, 50, 50, 5, '13', 'DF');
  pdf.SetFillColor(cGreen);
  pdf.RoundedRect(75, 130, 50, 50, 5, '24', 'DF');
  pdf.SetFillColor(cBlue);
  pdf.RoundedRect(135, 130, 50, 50, 5, '1234', 'DF');

  PDF.SetLineWidth(1.3);
  pdf.SetDrawColor(cRed);
  pdf.Line(15, 190, 65, 240);
  pdf.SetDrawColor(cGreen);
  pdf.Line(65, 190, 15, 240);

  pdf.SetDrawColor(cBlue);
  pdf.Line(75, 190, 125, 240);
  pdf.SetDrawColor(cRed);
  pdf.Line(125, 190, 75, 240);

  pdf.SetDrawColor(cGreen);
  pdf.Line(135, 190, 185, 240);
  pdf.SetDrawColor(cBlue);
  pdf.Line(185, 190, 135, 240);

end;

procedure TPDFDemo.PrintBarCodesTest;
var
  code: String;
begin
  PageTitle := 'FPDF Pascal Test - BarCodes Test';
  pdf.AddPage;
  pdf.SetLink(fpl5);
  pdf.SetFont('Helvetica','',6);
  PDF.SetFillColor(cBlack);

  code := '7894900709841';
  pdf.CodeEAN13(code, 30, 30);
  pdf.SetXY(30, 38);
  pdf.Write(5,'EAN13: "'+code+'"');

  code := '123456789012';
  pdf.CodeEAN13(code, 30, 50);
  pdf.SetXY(30, 58);
  pdf.Write(5,'EAN13: "'+code+'" (whitout digit)');

  code := '96385074';
  pdf.CodeEAN8(code, 30, 70);
  pdf.SetXY(30, 78);
  pdf.Write(5,'EAN8: "'+code+'"');

  code := 'ACBR RULES 1234567890';
  pdf.Code39(code, 30, 90);
  pdf.SetXY(30, 98);
  pdf.Write(5,'CODE39: "'+code+'"');

  code := '34199929800000100000090001234500284794899000';
  pdf.CodeI25(code, 30, 110, 8, 1);
  pdf.SetXY(30, 118);
  pdf.Write(5,'CODEI25: "'+code+'"');

  //A set
  code:='CODE 128';
  pdf.Code128(code, 30, 130);
  pdf.SetXY(30, 138);
  pdf.Write(5,'A set: "'+code+'"');

  //B set
  code:='Code 128';
  pdf.Code128(code, 30, 150);
  pdf.SetXY(30, 158);
  pdf.Write(5,'B set: "'+code+'"');

  ////C set
  code:='12345678901234567890';
  pdf.Code128(code, 30, 170);
  pdf.SetXY(30, 178);
  pdf.Write(5,'C set: "'+code+'"');

  //A,C,B sets
  code:='ABCDEFG1234567890AbCdEf';
  pdf.Code128(code, 30, 190, 0, 0.9);
  pdf.SetXY(30, 198);
  pdf.Write(5,'ABC sets combined: "'+code+'"');

  code := cGITURL;
  pdf.QRCode(53, 220, code);
  pdf.SetXY(42, 235);
  pdf.MultiCell(35, 5, '"'+code+'"', '0', 'C');

  pdf.QRCode(110, 210,
               '35150811111111111111591234567890001672668828|20150820201736|118.72|05481336000137|'+
               'TCbeD81ePUpMvso4VjFqRTvs4ovqmR1ZG3bwSCumzHtW8bbMedVJjVnww103v3LxKfgckAyuizcR/9pXaKay6M4Gu8kyDef+6VH5qONIZV1cB+mFfXiaCgeZ'+
               'ALuRDCH1PRyb6hoBeRUkUk6lOdXSczRW9Y83GJMXdOFroEbzFmpf4+WOhe2BZ3mEdXKKGMfl1EB0JWnAThkGT+1Er9Jh/3En5YI4hgQP3NC2BiJVJ6oCEbKb'+
               '85s5915DSZAw4qB/MlESWViDsDVYEnS/FQgA2kP2A9pR4+agdHmgWiz30MJYqX5Ng9XEYvvOMzl1Y6+7/frzsocOxfuQyFsnfJzogw==');
  pdf.SetXY(111, 248);
  pdf.Write(5,'Brazilian Fiscal "SAT-CFe" QRCode');

end;

procedure TPDFDemo.PrintALargeTextTest;
var
  sl: TStringList;
begin
  if not Assigned(pdf) then
    Exit;

  PageTitle := 'FPDF Pascal Test - Large justified text';
  pdf.AddPage;
  pdf.SetLink(fpl6);

  sl := TStringList.Create;
  try
    sl.LoadFromFile(fpfiledir+'20k_c1.txt');
    pdf.SetFont('Helvetica','',14);
    pdf.MultiCell(0, 6.5, sl.Text);
  finally
    sl.Free;
  end;
end;

procedure TPDFDemo.PrintLayersTest;
var
  l1, l2: Integer;
begin
  if not Assigned(pdf) then
    Exit;

  PageTitle := 'FPDF Pascal Test - Layers Test';
  pdf.AddPage;
  pdf.SetLink(fpl7);

  // Define layers
  l1 := pdf.AddLayer('Layer 1');
  l2 := pdf.AddLayer('Layer 2');

  // Open layer pane in PDF viewer
  pdf.OpenLayerPane();
  pdf.SetFont('Arial','',15);
  pdf.Write(8,'This line doesn''t belong to any layer.'+LF);

  // First layer
  pdf.BeginLayer(l1);
  pdf.Write(8,'This line belongs to Layer 1.'+LF);
  pdf.EndLayer();

  // Second layer
  pdf.BeginLayer(l2);
  pdf.Write(8,'This line belongs to Layer 2.'+LF);
  pdf.EndLayer();
end;

constructor TPDFDemo.Create;
begin
  fpdir := ExtractFilePath(ParamStr(0));
  fpfiledir := fpdir +  '..' + PathDelim + 'files' + PathDelim;
end;

procedure TPDFDemo.PrintHeader(APDF: TFPDF);
var
  vw: Double;
begin
  // Arial bold 15
  APDF.SetFont('Arial','B',15);
  // Calculate width of TheTitle and position
  vw := APDF.GetStringWidth(PageTitle)+6;
  APDF.SetX((210-vw)/2);
  // Colors of frame, background and text
  APDF.SetDrawColor(0,80,180);
  APDF.SetFillColor(230,230,0);
  APDF.SetTextColor(220,50,50);
  // Thickness of frame (1 mm)
  APDF.SetLineWidth(1);
  // The Title
  APDF.Cell(vw, 9, PageTitle, '1', 1, 'C', true);
  // Line break
  APDF.Ln(10);
end;

procedure TPDFDemo.PrintFooter(APDF: TFPDF);
begin
  // Position at 1.5 cm from bottom
  APDF.SetY(-15);
  // Arial italic 8
  APDF.SetFont('Arial','I',8);
  // Text color in gray
  APDF.SetTextColor(128);
  // Page number
  APDF.Cell( 0, 10, 'Page '+IntToStr(APDF.PageNo()), '0', 0, 'C');
end;


procedure TPDFDemo.CreatePDF;
begin
  pdf := TFPDFExt.Create;
  try
    pdf.OnHeader := PrintHeader;
    pdf.OnFooter := PrintFooter;
    pdf.SetCompression(True);

    PrintIndex;
    PrintFontsTest;
    PrintImagesTest;
    PrintColosrTest;
    PrintShapesTest;
    PrintBarCodesTest;
    PrintALargeTextTest;
    PrintLayersTest;

    pdf.SaveToFile(fpdir+'FPDFPascalTest.pdf');
  finally
    pdf.Free;
  end;
end;

begin
  hh := now;
  demo := TPDFDemo.Create;
  try
    demo.CreatePDF;
    //WriteLn(FormatDateTime('hh:mm:ss:zzz',now-hh));
  finally
    demo.Free;
  end;
end.

