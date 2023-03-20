program tuto3;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils,
  fpdf;

type

  { TMyFPDF }

  TMyFPDF = class(TFPDF)
  public
    TheTitle: String;

    procedure Header; override;
    procedure Footer; override;
    procedure ChapterTitle(Anum: Integer; Alabel: String);
    procedure ChapterBody(Afile: String);
    procedure PrintChapter(Anum: Integer; Atitle: String;  Afile: String);
  end;
var
  pdf: TMyFPDF;

{ TMyFPDF }

procedure TMyFPDF.Header;
var
  vw: Double;
begin
  // Arial bold 15
  SetFont('Arial','B',15);
  // Calculate width of TheTitle and position
  vw := GetStringWidth(TheTitle)+6;
  SetX((210-vw)/2);
  // Colors of frame, background and text
  SetDrawColor(0,80,180);
  SetFillColor(230,230,0);
  SetTextColor(220,50,50);
  // Thickness of frame (1 mm)
  SetLineWidth(1);
  // TheTitle
  Cell(vw, 9, TheTitle, '1', 1, 'C', true);
  // Line break
  Ln(10);
end;

procedure TMyFPDF.Footer;
begin
  // Position at 1.5 cm from bottom
  SetY(-15);
  // Arial italic 8
  SetFont('Arial','I',8);
  // Text color in gray
  SetTextColor(128);
  // Page number
  Cell( 0, 10, 'Page '+IntToStr(PageNo()), '0', 0, 'C');
end;

procedure TMyFPDF.ChapterTitle(Anum: Integer; Alabel: String);
begin
  // Arial 12
  SetFont('Arial','',12);
  // Background color
  SetFillColor(200,220,255);
  // TheTitle
  Cell( 0, 6, 'Chapter '+IntToStr(Anum)+' : '+Alabel, '0', 1, 'L', true);
  // Line break
  Ln(4);
end;

procedure TMyFPDF.ChapterBody(Afile: String);
var
  sl: TStringList;
begin
  // Read text file
  sl := TStringList.Create;
  try
    sl.LoadFromFile(Afile);
    // Times 12
    SetFont('Times','',12);
    // Output justified text
    MultiCell( 0, 5, sl.Text);
    // Line break
    Ln();
    // Mention in italics
    SetFont('','I');
    Cell(0,5,'(end of excerpt)');
  finally
    sl.Free;
  end;
end;

procedure TMyFPDF.PrintChapter(Anum: Integer; Atitle: String; Afile: String);
begin
  AddPage();
  ChapterTitle(Anum, Atitle);
  ChapterBody(Afile);
end;

begin

  pdf := TMyFPDF.Create;
  try
    pdf.TheTitle := '20000 Leagues Under the Seas';
    pdf.SetTitle(pdf.TheTitle);
    pdf.SetAuthor('Jules Verne');
    pdf.PrintChapter( 1, 'A RUNAWAY REEF', '20k_c1.txt');
    pdf.PrintChapter( 2, 'THE PROS AND CONS', '20k_c2.txt');
    pdf.SaveToFile('c:\temp\tuto3-pas.pdf');
  finally
    pdf.Free;
  end;

end.

