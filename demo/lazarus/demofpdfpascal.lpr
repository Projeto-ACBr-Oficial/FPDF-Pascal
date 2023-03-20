program demofpdfpascal;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, sysutils,
  fpdf;

var
  Fpdf1: TFPDF;
  t: TStrings;
  hh: TDateTime;
  DirFiles: String;
begin
  hh := now;
  DirFiles := ExtractFilePath(ParamStr(0))+'..\files\';
  DefaultFormatSettings.DecimalSeparator := ',';
  Fpdf1 := TFPDF.Create(poDefault, puPT, pfA4);
  t := TStringList.Create;
  t.LoadFromFile(DirFiles+'20k_c1.txt');
  with Fpdf1 do begin
    SetCompression(False);
    AddPage;
    SetFont('Times','B',16);
    SetDrawColor(cRed);
    SetLineWidth(0.3);
    SetFillColor(cYellow);
    Cell(0, 10, 'JUSTIFIED TEXT','LTBR',0,'C', True);
    Ln(20);
    SetFont('Helvetica','',14);
    MultiCell(0, 6.5, t.Text);
    Ln;
    SetFont('Helvetica','B',18);
    Cell(0,6.5, 'UTF8Decode','B',1,'C');
    Ln;
    SetFont('Courier','',16);
    Writer(6.5,'SetUTF8(False) = ');
    SetFont('Times','',16);
    Writer(6.5,'Avião Índio Carroça');
    Ln;
    SetUTF8(True);
    SetFont('Courier','',16);
    Writer(6.5,'SetUTF8(True) = ');
    SetFont('Times','',16);
    Writer(6.5,'Avião Índio Carroça');
    AddPage(poLandscape);
    SetFont('Helvetica','B',16);
    Cell(40,10,'Free Jpdf Pascal','0',0,'');
    Ln(0);
    SetFont('Helvetica','B',16);
    SetTextColor(98,147,199);
    Cell(80,10,'Free Jpdf Pascal','0',0,'');
    Line(90,10,100,100);
    SetFillColor(177,32,10);
    Rect(50,50,100,100,'F');
    SetFont('Times','BI',40);
    SetTextColor(92,255,102);
    SetUnderline(True);
    Text(20,180,'PDF for Free Pascal Web');
    AddPage;
    SetFont('Helvetica','B',16);
    Cell(60,10,'Free Jpdf Pascal','1',0,'C');
    Cell(60,10,'Free Jpdf Pascal','1',0,'C');
    Image(DirFiles+'image1.jpg',40,40,80,75);
    Image(DirFiles+'image2.png',40,120,120,75);
    Image(DirFiles+'image3.png',40,200,100,60);
    //Image('https://projetoacbr.com.br/wp-content/uploads/2021/06/acbr.png',40,200,100,100);
    SetFont('Courier','BIU',16);
    SetTextColor(0,0,0);
    Text(100,140,'Free Jpdf Pascal');
    AddPage;
    //Code25(10,40,'05379360100094362094200000008872000000000000000');
    SetFont('Times','BIU',16);
    Writer(10,'Free Pascal is always ');
    SetFont('Times','BU',16);
    Writer(10,'under development.');
    SetFont('Times','I',16);
    Writer(10,' If you want to see how the development is progressing ');
    SetFont('Times','',16);
    Writer(10,' you can take a peek at the developer versions.');
    SaveToFile(ExtractFilePath(ParamStr(0)) + 'FPDFPascalTeste.pdf');
    Free;
    WriteLn(FormatDateTime('hh:mm:ss:zzz',now-hh));
  end;
end.

