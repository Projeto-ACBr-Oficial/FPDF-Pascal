program tuto1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils,
  fpdf;

var
  pdf: TFPDF;
begin
  pdf := TFPDF.Create();
  try
    pdf.SetCompression(False);
    pdf.AddPage();
    pdf.SetFont('Arial','B',16);
    pdf.Cell(40,10,'Hello World!');
    pdf.SaveToFile(ExtractFilePath(ParamStr(0))+PathDelim+'tuto1-pas.pdf');
  finally
    pdf.Free;
  end;
end.

