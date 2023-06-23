program tuto6;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils,
  fpdf_ext;

type

  { TMyFPDF }

  TMyFPDF = class(TFPDFExt)
  protected
   fpdir: String;
   fpfiledir: String;

   procedure InternalCreate; override;
  public

  end;

{ TMyFPDF }

procedure TMyFPDF.InternalCreate;
begin
  inherited InternalCreate;
  fpdir := ExtractFilePath(ParamStr(0)) + PathDelim;
  fpfiledir := fpdir +  '..' + PathDelim + 'files' + PathDelim;
end;

var
  pdf: TMyFPDF;
  AHtml: String;
  link: Integer;

begin
   AHtml := 'You can now easily print text mixing different styles: <b>bold</b>, <i>italic</i>, '+
            '<u>underlined</u>, or <b><i><u>all at once</u></i></b>!<br><br>You can also insert links on '+
            'text, such as <a href="http://www.fpdf.org">www.fpdf.org</a>, or on an image: click on the logo.';

  pdf := TMyFPDF.Create;
  try
    // First page
    pdf.AddPage();
    pdf.SetFont('Arial', '', 20);
    pdf.Write(5, 'To find out what''s new in this tutorial, click ');
    pdf.SetFont('','U');
    link := pdf.AddLink();
    pdf.Write(5, 'here', IntToStr(link));
    pdf.SetFont('');

    // Second page
    pdf.AddPage();
    pdf.SetLink(link);
    pdf.Image(pdf.fpfiledir+'logo.png', 10, 12, 30, 0, 'http://www.fpdf.org');
    pdf.SetLeftMargin(45);
    pdf.SetFontSize(14);
    pdf.WriteHTML(AHtml);
    pdf.SaveToFile(pdf.fpdir+'tuto6-pas.pdf');
  finally
    pdf.Free;
  end;

end.

