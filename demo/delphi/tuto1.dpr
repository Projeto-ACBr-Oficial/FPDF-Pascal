program tuto1;

uses
  Classes, SysUtils,
  fpdf;

var
  pdf: TFPDF;
begin
  pdf := TFPDF.Create();
  try
    pdf.SetCompression(False);
    pdf.SetUTF8(False);      //  This file is ANSI CP1252 (check with NotePad++)
    pdf.AddPage();
    pdf.SetFont('Arial','B',16);
    pdf.Cell(40,10,'Hello World!');
    pdf.Cell(60,10,'аимсз');
    pdf.SaveToFile(ExtractFilePath(ParamStr(0))+PathDelim+'tuto1-pas.pdf');
  finally
    pdf.Free;
  end;
end.

