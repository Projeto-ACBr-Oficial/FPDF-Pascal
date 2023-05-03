program tuto4;

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
  protected
    col: Integer; // Current column
    y0: Double; // Ordinate of column start
    TheTitle: String;

    fpdir: String;
    fpfiledir: String;

    procedure InternalCreate; override;
  public

    procedure Header; override;
    procedure Footer; override;
    procedure SetCol(ACol: Integer);
    function AcceptPageBreak: Boolean; override;

    procedure ChapterTitle(Anum: Integer; Alabel: String);
    procedure ChapterBody(Afile: String);
    procedure PrintChapter(Anum: Integer; Atitle: String;  Afile: String);
  end;
var
  pdf: TMyFPDF;

{ TMyFPDF }

procedure TMyFPDF.InternalCreate;
begin
  inherited InternalCreate;
  Col := 0;
  y0 := 0;
  fpdir := ExtractFilePath(ParamStr(0)) + PathDelim;
  fpfiledir := fpdir +  '..' + PathDelim + 'files' + PathDelim;
end;

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
  Self.y0 := GetY();
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

procedure TMyFPDF.SetCol(ACol: Integer);
var
  vx: Integer;
begin
  // Set position at a given column
  Self.col := ACol;
  vx := 10+ACol*65;
  SetLeftMargin(vx);
  SetX(vx);
end;

function TMyFPDF.AcceptPageBreak: Boolean;
begin
  // Method accepting or not automatic page break
  if (Self.col < 2) then
  begin
    // Go to next column
    SetCol(Self.col+1);
    // Set ordinate to top
    SetY(Self.y0);
    // Keep on page
    Result := false;
  end
  else
  begin
    // Go back to first column
    SetCol(0);
    // Page break
    Result := true;
  end;
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
  // Save ordinate
  Self.y0 := GetY();
end;

procedure TMyFPDF.ChapterBody(Afile: String);
var
  sl: TStringList;
  txt: String;
begin
  // Read text file
  sl := TStringList.Create;
  try
    sl.LoadFromFile(Afile);
    txt := Trim(sl.Text);
    // Times 12
    SetFont('Times','',12);
    // Output text in a 6 cm width column
    MultiCell(60, 5, txt);
    // Line break
    Ln();
    // Mention in italics
    SetFont('','I');
    Cell(0, 5, '(end of excerpt)');
    // Go back to first column
    SetCol(0);
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
    pdf.PrintChapter( 1, 'A RUNAWAY REEF', pdf.fpfiledir+'20k_c1.txt');
    pdf.PrintChapter( 2, 'THE PROS AND CONS', pdf.fpfiledir+'20k_c2.txt');
    pdf.SaveToFile(pdf.fpdir+'tuto4-pas.pdf');
  finally
    pdf.Free;
  end;

end.

