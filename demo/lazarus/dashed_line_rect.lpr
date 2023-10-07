program dashed_line_rect;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
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
    pdf.DashedLine(20,20,190,20,2);
    pdf.SetLineWidth(0.5);
    pdf.DashedLine(20,25,190,25,3);
    pdf.SetLineWidth(0.8);
    pdf.DashedRect(20,30,170,20,'',2);
    pdf.Line(20,55,190,55);

    pdf.SaveToFile(ExtractFilePath(ParamStr(0))+PathDelim+'dashed_line_rect.pdf');
  finally
    pdf.Free;
  end;
end.

