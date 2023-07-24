program dashes;

uses
  Classes,
  SysUtils,
  fpdf_ext;

var
  pdf: TFPDFExt;
begin
  pdf := TFPDFExt.Create();
  try
    pdf.AddPage();
    pdf.SetLineWidth(0.1);
    pdf.SetDash(5,5); //5mm on, 5mm off
    pdf.Line(20,20,190,20);
    pdf.SetLineWidth(0.5);
    pdf.Line(20,25,190,25);
    pdf.SetLineWidth(0.8);
    pdf.SetDash(4,2); //4mm on, 2mm off
    pdf.Rect(20,30,170,20);
    pdf.SetDash(0, 0); //restores no dash
    pdf.Line(20,55,190,55);

    pdf.SaveToFile(ExtractFilePath(ParamStr(0))+PathDelim+'dashes.pdf');
  finally
    pdf.Free;
  end;
end.

