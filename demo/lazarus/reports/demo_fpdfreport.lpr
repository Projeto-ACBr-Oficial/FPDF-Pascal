program demo_fpdfreport;

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
  report1 in 'report1.pas';
var
  Engine: TFPDFEngine;
begin
  Engine := TFPDFEngine.Create(TReport1.Create);
  try
    Engine.Compressed := True;
    Engine.SaveToFile(ExtractFilePath(ParamStr(0)) + PathDelim + 'report1.pdf');
  finally
    Engine.Free;
  end;
end.

