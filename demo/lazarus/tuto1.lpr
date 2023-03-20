program tuto1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  fpdf;

var
  pdf: TFPDF;
begin
  pdf := TFPDF.Create();
  try
    pdf.AddPage();
    pdf.SetFont('Arial','B',16);
    pdf.Cell(40,10,'Hello World!');
    pdf.Output('F','c:\\temp\\tuto1.pdf');
  finally
    pdf.Free;
  end;
end.

