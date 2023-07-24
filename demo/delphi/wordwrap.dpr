program wordwrap;

uses
  Classes,
  SysUtils,
  fpdf_ext;

var
  pdf: TFPDFExt;
  Text: string;
  i, nb: integer;
begin
  pdf := TFPDFExt.Create();
  try
    pdf.AddPage();
    pdf.SetFont('Arial','',12);

    Text := '';
    for i := 1 to 20 do
      Text := Text + 'this is a word wrap test ';

    nb := pdf.WordWrap(Text, 120);
    pdf.Write(5, Format('This paragraph has %d lines:%1:s%1:s', [nb, sLineBreak]));
    pdf.Write(5, Text);

    pdf.SaveToFile(ExtractFilePath(ParamStr(0))+PathDelim+'wordwrap.pdf');
  finally
    pdf.Free;
  end;
end.

