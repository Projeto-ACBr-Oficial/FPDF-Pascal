program ex_protection;

uses
  Classes, SysUtils,
  fpdf_ext;

var
  pdf: TFPDFExt;
begin
  pdf := TFPDFExt.Create();
  try
    pdf.SetProtection([canPrint], '1234', '5678');
    pdf.AddPage();
    pdf.SetFont('Arial');
    pdf.Write(10,'You can print me but not copy my text.');
    pdf.SaveToFile(ExtractFilePath(ParamStr(0))+PathDelim+'protection-pas.pdf');
  finally
    pdf.Free;
  end;
end.

