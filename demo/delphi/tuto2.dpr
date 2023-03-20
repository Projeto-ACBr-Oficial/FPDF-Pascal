program tuto2;

uses
  Classes, SysUtils,
  fpdf;

type

  { TMyFPDF }

  TMyFPDF = class(TFPDF)
  public
    procedure Header; override;
    procedure Footer; override;
  end;
var
  pdf: TMyFPDF;
  i: Integer;

{ MyFPDF }

procedure TMyFPDF.Header;
begin
  // Logo
  Image('logo.png',10,6,30);
  // Arial bold 15
  SetFont('Arial','B',15);
  // Move to the right
  Cell(80);
  // Title
  Cell(30,10,'Title','1',0,'C');
  // Line break
  Ln(20);
end;

procedure TMyFPDF.Footer;
begin
  // Position at 1.5 cm from bottom
  SetY(-15);
  // Arial italic 8
  SetFont('Arial','I',8);
  // Page number
  Cell(0,10,'Page '+IntToStr(PageNo())+'/{nb}', '0', 0, 'C');
end;

begin
  // Instanciation of inherited class
  pdf := TMyFPDF.Create();
  try
    pdf.SetAliasNbPages();
    pdf.AddPage();
    pdf.SetFont('Times','',12);
    for i := 1 to 40 do
      pdf.Cell(0,10,'Printing line number '+IntToStr(i), '0', 1);

    pdf.SaveToFile('c:\\temp\\tuto2-pas.pdf');
  finally
    pdf.Free;
  end;
end.

