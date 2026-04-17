unit AndroidUnit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls,
  System.IOUtils,
{$IFDEF MSWINDOWS}
  Winapi.Windows, Winapi.ShellAPI,
{$ENDIF}
{$IFDEF ANDROID}
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.provider,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Net,
  Androidapi.JNI.App,
  AndroidAPI.jNI.OS,
  Androidapi.JNIBridge,
  FMX.Helpers.Android,
  Androidapi.Helpers,
  //IdUri,
  FMX.Platform.Android,
  //FMX.Dialogs.Android,
  Androidapi.JNI.Support,
{$ENDIF}
{$IFDEF LINUX}
  Posix.Stdlib,
{$ENDIF}
  fpdf;

type

  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    procedure ShowPDF(const AFile: String);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
var
  pdf: TFPDF;
  f: String;
begin
  pdf := TFPDF.Create();
  try
    pdf.SetCompression(False);
    pdf.SetUTF8(False);
    pdf.AddPage();
    pdf.SetFont('Arial','B',16);
    pdf.Cell(40,10,'Hello World!');
    pdf.Cell(60,10,'ÁÉÍÓÚ');
    f := TPath.Combine(TPath.GetSharedDocumentsPath, 'tuto1-pas.pdf');
    pdf.SaveToFile(f);
    ShowPDF(f);
    ShowMessage('Arquivo salvo em: ' + sLineBreak + f);
  finally
    pdf.Free;
  end;
end;

procedure TForm1.ShowPDF(const AFile: String);
begin
  if not TFile.Exists(AFile) then
  begin
    ShowMessage('Arquivo PDF não encontrado.');
    Exit;
  end;

{$IFDEF ANDROID}
  var Intent: JIntent;
  var J: JFile;
  Intent := TJIntent.JavaClass.init;
  Intent.setAction(TJIntent.JavaClass.ACTION_VIEW);
  J := TJFile.JavaClass.init(StringToJString(AFile));
  Intent.setDataAndType(TAndroidHelper.JFileToJURI(J), StringToJString('application/pdf'));
  Intent.setFlags(TJIntent.JavaClass.FLAG_GRANT_READ_URI_PERMISSION);
  {$IF CompilerVersion >= 35.0}
  TAndroidHelper.Activity.startActivity(Intent);
  {$ELSE}
  SharedActivity.startActivity(Intent);
  {$IFEND}
{$ELSEIF DEFINED(MSWINDOWS)}
  ShellExecute(0, 'open', PChar(AFile), nil, nil, SW_SHOWNORMAL);
{$ELSEIF DEFINED(IOS)}
  // iOS: use UIDocumentInteractionController via platform services
  ShowMessage('PDF salvo em: ' + AFile);
{$ELSEIF DEFINED(LINUX)}
  _system(PAnsiChar(AnsiString('xdg-open "' + AFile + '" &')));
{$ELSE}
  ShowMessage('PDF salvo em: ' + AFile);
{$ENDIF}
end;

end.
