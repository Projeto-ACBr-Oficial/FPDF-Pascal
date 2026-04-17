program AndroidFPDF;

uses
  System.StartUpCopy,
  FMX.Forms,
  AndroidUnit1 in 'AndroidUnit1.pas' {Form1},
  DelphiZXIngQRCode in '..\..\source\DelphiZXIngQRCode.pas',
  fpdf in '..\..\source\fpdf.pas',
  fpdf_ext in '..\..\source\fpdf_ext.pas',
  fpdf_report in '..\..\source\fpdf_report.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
