program tuto5;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils,
  fpdf;

type

  TLine = array[0..3] of String;
  TData = array of TLine;

  { TMyFPDF }

  TMyFPDF = class(TFPDF)
  protected

  public
    function LoadData(const AFile: String): TData;
    procedure BasicTable(aheader: TLine; adata: TData);
    procedure ImprovedTable(aheader: TLine; adata: TData);
    procedure FancyTable(aheader: TLine; adata: TData);
  end;
var
  pdf: TMyFPDF;

{ TMyFPDF }

function TMyFPDF.LoadData(const AFile: String): TData;
var
  sl: TStringList;
  a: TStringArray;
  l, i: Integer;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(AFile);
    for i := 0 to sl.Count-1 do
    begin
      l := Length(Result);
      SetLength(Result, l+1);
      a := Split(sl[i], ';');
      Result[l][0] := a[0];
      Result[l][1] := a[1];
      Result[l][2] := a[2];
      Result[l][3] := a[3];
    end;
  finally
    sl.Free;
  end;
end;

procedure TMyFPDF.BasicTable(aheader: TLine; adata: TData);
var
  acol: String;
  i, j: Integer;
  arow: TLine;
begin
  // Header
  for i := 0 to Length(aheader)-1 do
  begin
    acol := aheader[i];
    Cell(40, 7, acol, '1');
  end;
  Ln();

  // Data
  for i := 0 to Length(adata)-1 do
  begin
    arow := adata[i];
    for j := 0 to Length(arow)-1 do
    begin
      acol := arow[j];
      Cell(40, 6, acol, '1');
    end;
    Ln();
  end;
end;

// Better table
procedure TMyFPDF.ImprovedTable(aheader: TLine; adata: TData);
var
  // Column widths
  vw: array[0..3] of Integer = (40, 35, 40, 45);
  i, t: Integer;
  arow: TLine;
begin
  t := 0;
  // Header
  for i := 0 to Length(aheader)-1 do
  begin
    Cell(vw[i], 7, aheader[i], '1', 0, 'C');
    t := t + vw[i];
  end;
  Ln();

  // Data
  for i := 0 to Length(adata)-1 do
  begin
    arow := adata[i];
    Cell(vw[0], 6, arow[0], 'LR');
    Cell(vw[1], 6, arow[1], 'LR');
    Cell(vw[2], 6, FormatFloat(',0.00', StrToFloat(arow[2])), 'LR', 0, 'R');
    Cell(vw[3], 6, FormatFloat(',0.00', StrToFloat(arow[3])), 'LR', 0, 'R');
    Ln();
  end;

  // Closing line
  Cell(t, 0, '', 'T');
end;

procedure TMyFPDF.FancyTable(aheader: TLine; adata: TData);
var
  // Column widths
  vw: array[0..3] of Integer = (40, 35, 40, 45);
  i, t: Integer;
  arow: TLine;
  fill: Boolean;
begin
  // Colors, line width and bold font
  SetFillColor(255, 0, 0);
  SetTextColor(255);
  SetDrawColor(128, 0, 0);
  SetLineWidth(0.3);
  SetFont('','B');

  // Header
  t := 0;
  for i := 0 to Length(aheader)-1 do
  begin
    Cell(vw[i], 7, aheader[i], '1', 0, 'C', True);
    t := t + vw[i];
  end;
  Ln();

  // Color and font restoration
  SetFillColor(224,235,255);
  SetTextColor(0);
  SetFont('');

  // Data
  fill := False;
  for i := 0 to Length(adata)-1 do
  begin
    arow := adata[i];
    Cell(vw[0], 6, arow[0], 'LR', 0, 'L', fill);
    Cell(vw[1], 6, arow[1], 'LR', 0, 'L', fill);
    Cell(vw[2], 6, FormatFloat(',0.00', StrToFloat(arow[2])), 'LR', 0, 'R', fill);
    Cell(vw[3], 6, FormatFloat(',0.00', StrToFloat(arow[3])), 'LR', 0, 'R', fill);
    Ln();
    fill := not fill;
  end;

  // Closing line
  Cell(t, 0, '', 'T');
end;

var
  // Column headings
  aheader: TLine = ('Country', 'Capital', 'Area (sq km)', 'Pop. (thousands)');
  adata: TData;
begin

  pdf := TMyFPDF.Create;
  try
    // Data loading
    adata := pdf.LoadData('countries.txt');
    pdf.SetFont('Arial', '', 14);
    pdf.AddPage();
    pdf.BasicTable(aheader, adata);
    pdf.AddPage();
    pdf.ImprovedTable(aheader, adata);
    pdf.AddPage();
    pdf.FancyTable(aheader, adata);
    pdf.SaveToFile('c:\temp\tuto5-pas.pdf');
  finally
    pdf.Free;
  end;

end.

