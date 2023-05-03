program demofpdfpascal;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, sysutils,
  fpdf, fpdf_ext;

type

  { TPDFDemo }

  TPDFDemo = class
  protected
    pdf: TFPDFExt;
    PageTitle: String;
    procedure PrintFontsTest;
    procedure PrintImageTest;
    procedure PrintALargeText;
  public
    procedure CreatePDF;
    procedure PrintHeader(APDF: TFPDF);
    procedure PrintFooter(APDF: TFPDF);
  end;
var
  demo: TPDFDemo;
  hh: TDateTime;

{ TPDFDemo }

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

procedure TPDFDemo.PrintImageTest;
begin
  if not Assigned(pdf) then
    Exit;

  pdf.Image('image1.jpg',40,40,80,75);
  pdf.Image('logo.png',40,120,120,75);
  pdf.Image('https://projetoacbr.com.br/wp-content/uploads/2021/06/acbr.png',40,150,100,100);

end;

procedure TPDFDemo.PrintALargeText;
var
  sl: TStringList;
begin
  if not Assigned(pdf) then
    Exit;

  sl := TStringList.Create;
  try
    PageTitle := 'FPDF Pascal Test - Large justified text';
    pdf.AddPage;
    pdf.SetFont('Helvetica','',14);
    pdf.MultiCell(0, 6.5, sl.Text);
  finally
    sl.Free;
  end;
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
var
  dir: String;
begin
  dir := ExtractFilePath(ParamStr(0))+PathDelim;
  pdf := TFPDFExt.Create;
  try
    sl.LoadFromFile(dir+'20k_c1.txt');

    pdf.OnHeader := @PrintHeader;
    pdf.OnFooter := @PrintFooter;
    pdf.SetCompression(True);

    PrintFontsTest;
    PrintALargeText;



    pdf.Ln;
    pdf.SetFont('Helvetica','B',18);
    pdf.Cell(0,6.5, 'UTF8Decode','B',1,'C');
    pdf.Ln;
    pdf.SetUTF8(False);
    pdf.SetFont('Courier','',16);
    pdf.Write(6.5,'SetUTF8(False) = ');
    pdf.SetFont('Times','',16);
    pdf.Write(6.5,'Avião Índio Carroça');
    pdf.Ln;
    pdf.SetUTF8(True);
    pdf.SetFont('Courier','',16);
    pdf.Write(6.5,'SetUTF8(True) = ');
    pdf.SetFont('Times','',16);
    pdf.Write(6.5,'Avião Índio Carroça');

    pdf.AddPage(poLandscape);
    pdf.SetFont('Helvetica','B',16);
    pdf.Cell(40,10,'Free PDF Pascal','0',0,'');
    pdf.Ln(0);
    pdf.SetFont('Helvetica','B',16);
    pdf.SetTextColor(98,147,199);
    pdf.Cell(80,10,'Free PDF Pascal','0',0,'');
    pdf.Line(90,10,100,100);
    pdf.SetFillColor(177,32,10);
    pdf.Rect(50,50,100,100,'F');
    pdf.SetFont('Times','BI',40);
    pdf.SetTextColor(92,255,102);
    pdf.SetUnderline(True);
    pdf.Text(20,180,'PDF for Free Pascal Web');
    pdf.AddPage(poPortrait);
    pdf.SetFont('Helvetica','B',16);
    pdf.Cell(60,10,'Free PDF Pascal','1',0,'C');
    pdf.Image('image1.jpg',40,40,80,75);
    pdf.Image('logo.png',40,120,120,75);
    pdf.Image('https://projetoacbr.com.br/wp-content/uploads/2021/06/acbr.png',40,150,100,100);
    pdf.SetFont('Courier','BIU',16);
    pdf.SetTextColor(0,0,0);
    pdf.Text(100,140,'Free PDF Pascal');

    pdf.AddPage;
    pdf.SetFillColor(cBlack);
    pdf.CodeI25('05379360100094362094200000008872000000000000000', 10, 40, 15, 1);
    pdf.SetFont('Times','BIU',16);
    pdf.Write(10,'Free Pascal is always ');
    pdf.SetFont('Times','BU',16);
    pdf.Write(10,'under development.');
    pdf.SetFont('Times','I',16);
    pdf.Write(10,' If you want to see how the development is progressing ');
    pdf.SetFont('Times','',16);
    pdf.Write(10,' you can take a peek at the developer versions.');

    pdf.SaveToFile(dir+'FPDFPascalTest.pdf');
  finally
    pdf.Free;
    sl.Free;
  end;
end;

begin
  hh := now;
  demo := TPDFDemo.Create;
  try
    demo.CreatePDF;
    WriteLn(FormatDateTime('hh:mm:ss:zzz',now-hh));
  finally
    demo.Free;
  end;
end.

