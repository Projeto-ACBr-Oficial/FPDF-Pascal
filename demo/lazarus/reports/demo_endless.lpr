program demo_endless;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  fpdf,
  fpdf_ext,
  fpdf_report,
  report_endless in 'report_endless.pas';

var
  Engine: TFPDFEngine;

{$R *.res}

begin
  Engine := TFPDFEngine.Create(TReportEndlessHeight.Create);
  try
    Engine.Compressed := True;
    Engine.SaveToFile(ExtractFilePath(ParamStr(0)) + PathDelim + 'endless.pdf');
  finally
    Engine.Free;
  end;
end.

