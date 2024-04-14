unit report_endless;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpdf,
  fpdf_ext,
  fpdf_report;

type
  TReportEndlessHeight = class(TFPDFReport)
  strict private
    FCustomer: integer;
    FProduct: integer;
    FIndent: double;
  private
    procedure DrawTopMargin(Args: TFPDFBandDrawArgs);
    procedure DrawBottomMargin(Args: TFPDFBandDrawArgs);
    procedure DrawReportHeader(Args: TFPDFBandDrawArgs);
    procedure DrawReportFooter(Args: TFPDFBandDrawArgs);
    procedure DrawPageHeader(Args: TFPDFBandDrawArgs);
    procedure DrawPageFooter(Args: TFPDFBandDrawArgs);
    procedure DrawCustomers(Args: TFPDFBandDrawArgs);
    procedure DrawProducts(Args: TFPDFBandDrawArgs);
    procedure DrawData(Args: TFPDFBandDrawArgs);
  protected
    procedure OnStartReport(Args: TFPDFReportEventArgs); override;
  public
    constructor Create; override;
  end;

implementation

{ TReportEndlessHeight }

constructor TReportEndlessHeight.Create;
var
  Page: TFPDFPage;
begin
  inherited;
  EngineOptions.DoublePass := True;
  SetMargins(10, 10);
  Page := AddPage(poPortrait, puMM, 100, 100);
  Page.EndlessHeight := True;

  AddBand(btTopMargin, 10, @DrawTopMargin);
  AddBand(btBottomMargin, 10, @DrawBottomMargin);
  AddBand(btReportHeader, 10, @DrawReportHeader);
  AddBand(btReportFooter, 10, @DrawReportFooter);
  AddBand(btPageHeader, 10, @DrawPageHeader);
  AddBand(btPageFooter, 10, @DrawPageFooter);
  AddBand(btData, 10, @DrawCustomers);
  AddBand(btData, 10, @DrawData);
  AddBand(btData, 10, @DrawProducts);
end;

procedure TReportEndlessHeight.DrawReportFooter(Args: TFPDFBandDrawArgs);
begin
  Args.PDF.SetFillColor(128, 128, 247);
  Args.PDF.Rect(0, 0, Args.Band.Width, Args.Band.Height, 'DF');

  Args.PDF.SetFont('Arial', 'B', 14);
  Args.PDF.TextBox(FIndent, 0, Args.Band.Width, Args.Band.Height,
    'Report Footer', 'C', 'L', False);
end;

procedure TReportEndlessHeight.DrawReportHeader(Args: TFPDFBandDrawArgs);
begin
  Args.PDF.SetFillColor(128, 128, 247);
  Args.PDF.Rect(0, 0, Args.Band.Width, Args.Band.Height, 'DF');

  Args.PDF.SetFont('Arial', 'B', 14);
  Args.PDF.TextBox(FIndent, 0, Args.Band.Width, Args.Band.Height,
    'Report Header', 'C', 'L', False);
end;

procedure TReportEndlessHeight.DrawTopMargin(Args: TFPDFBandDrawArgs);
begin
  Args.PDF.SetFillColor(189, 224, 219);
  Args.PDF.Rect(0, 0, Args.Band.Width, Args.Band.Height, 'DF');

  Args.PDF.SetFont('Arial', 'B', 14);
  Args.PDF.TextBox(FIndent, 0, Args.Band.Width, Args.Band.Height,
    'Top Margin', 'C', 'L', False);

  Args.PDF.SetFont('Arial', 'I', 14);
  Args.PDF.TextBox(FIndent, 0, Args.Band.Width - 2 * FIndent, Args.Band.Height,
    Format('Page %d of %d', [Args.CurrentPage, Args.TotalPages]), 'C', 'R', False);
end;

procedure TReportEndlessHeight.OnStartReport(Args: TFPDFReportEventArgs);
begin
  // Initializing data
  FCustomer := 0;
  FProduct := 0;
  FIndent := 5;
end;

procedure TReportEndlessHeight.DrawPageHeader(Args: TFPDFBandDrawArgs);
begin
  Args.PDF.SetFillColor(245, 245, 141);
  Args.PDF.Rect(0, 0, Args.Band.Width, Args.Band.Height, 'DF');

  Args.PDF.SetFont('Arial', 'B', 14);
  Args.PDF.TextBox(FIndent, 0, Args.Band.Width, Args.Band.Height,
    'Page Header', 'C', 'L', False);
end;

procedure TReportEndlessHeight.DrawProducts(Args: TFPDFBandDrawArgs);
begin
  Inc(FProduct);

  Args.PDF.SetFillColor(235, 235, 235);
  Args.PDF.Rect(0, 0, Args.Band.Width, Args.Band.Height, 'DF');

  Args.PDF.SetFont('Arial', '', 14);
  Args.PDF.TextBox(FIndent, 0, Args.Band.Width, Args.Band.Height,
    Format('Product: %d', [FProduct]), 'C', 'L', False);

  Args.DrawAgain := FProduct < 20;
end;

procedure TReportEndlessHeight.DrawData(Args: TFPDFBandDrawArgs);
begin
  Args.PDF.SetFillColor(222, 177, 177);
  Args.PDF.Rect(0, 0, Args.Band.Width, Args.Band.Height, 'DF');

  Args.PDF.SetFont('Arial', '', 14);
  Args.PDF.TextBox(FIndent, 0, Args.Band.Width, Args.Band.Height,
    'Data Band', 'C', 'L', False);
end;

procedure TReportEndlessHeight.DrawBottomMargin(Args: TFPDFBandDrawArgs);
begin
  Args.PDF.SetFillColor(189, 224, 219);
  Args.PDF.Rect(0, 0, Args.Band.Width, Args.Band.Height, 'DF');

  Args.PDF.SetFont('Arial', 'B', 14);
  Args.PDF.TextBox(FIndent, 0, Args.Band.Width, Args.Band.Height,
    'Bottom Margin', 'C', 'L', False);

  Args.PDF.SetFont('Arial', 'I', 14);
  Args.PDF.TextBox(FIndent, 0, Args.Band.Width - 2 * FIndent, Args.Band.Height,
    Format('Page %d of %d', [Args.CurrentPage, Args.TotalPages]), 'C', 'R', False);
end;

procedure TReportEndlessHeight.DrawCustomers(Args: TFPDFBandDrawArgs);
begin
  Inc(FCustomer);

  Args.PDF.SetFillColor(252, 245, 232);
  Args.PDF.Rect(0, 0, Args.Band.Width, Args.Band.Height, 'DF');

  Args.PDF.SetFont('Arial', '', 14);
  Args.PDF.TextBox(FIndent, 0, Args.Band.Width, Args.Band.Height,
    Format('Customer: %d', [FCustomer]), 'C', 'L', False);

  Args.DrawAgain := FCustomer < 10;
end;

procedure TReportEndlessHeight.DrawPageFooter(Args: TFPDFBandDrawArgs);
begin
  Args.PDF.SetFillColor(245, 245, 141);
  Args.PDF.Rect(0, 0, Args.Band.Width, Args.Band.Height, 'DF');

  Args.PDF.SetFont('Arial', 'B', 14);
  Args.PDF.TextBox(FIndent, 0, Args.Band.Width, Args.Band.Height,
    'Page Footer', 'C', 'L', False);
end;

end.
