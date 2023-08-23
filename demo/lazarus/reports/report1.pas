unit report1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpdf_report;

type
  TReport1 = class(TFPDFReport)
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
    procedure DrawFillEmptySpace(Args: TFPDFBandDrawArgs);
    procedure DrawOverlay(Args: TFPDFBandDrawArgs);
  protected
    procedure OnStartReport(Args: TFPDFReportEventArgs); override;
  public
    constructor Create; override;
  end;

implementation

{ TReport1 }

constructor TReport1.Create;
begin
  inherited;
  EngineOptions.DoublePass := True;
  SetMargins(10, 13.5);
  AddPage;

  AddBand(btTopMargin, 10, @DrawTopMargin);
  AddBand(btBottomMargin, 10, @DrawBottomMargin);
  AddBand(btReportHeader, 10, @DrawReportHeader);
  AddBand(btReportFooter, 10, @DrawReportFooter);
  AddBand(btPageHeader, 10, @DrawPageHeader);
  AddBand(btPageFooter, 10, @DrawPageFooter);
  AddBand(btData, 10, @DrawCustomers);
  AddBand(btData, 10, @DrawData);
  AddBand(btData, 10, @DrawProducts);
  AddBand(btData, 10, @DrawFillEmptySpace);
  AddBand(btOverlay, 10, @DrawOverlay);
end;

procedure TReport1.DrawReportFooter(Args: TFPDFBandDrawArgs);
begin
  Args.PDF.SetFillColor(128, 128, 247);
  Args.PDF.Rect(0, 0, Args.Band.Width, Args.Band.Height, 'DF');

  Args.PDF.SetFont('Arial', 'B', 14);
  Args.PDF.TextBox(FIndent, 0, Args.Band.Width, Args.Band.Height,
    'Report Footer', 'C', 'L', False);
end;

procedure TReport1.DrawReportHeader(Args: TFPDFBandDrawArgs);
begin
  Args.PDF.SetFillColor(128, 128, 247);
  Args.PDF.Rect(0, 0, Args.Band.Width, Args.Band.Height, 'DF');

  Args.PDF.SetFont('Arial', 'B', 14);
  Args.PDF.TextBox(FIndent, 0, Args.Band.Width, Args.Band.Height,
    'Report Header', 'C', 'L', False);
end;

procedure TReport1.DrawTopMargin(Args: TFPDFBandDrawArgs);
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

procedure TReport1.OnStartReport(Args: TFPDFReportEventArgs);
begin
  // Initializing data
  FCustomer := 0;
  FProduct := 0;
  FIndent := 5;
end;

procedure TReport1.DrawPageHeader(Args: TFPDFBandDrawArgs);
begin
  Args.PDF.SetFillColor(245, 245, 141);
  Args.PDF.Rect(0, 0, Args.Band.Width, Args.Band.Height, 'DF');

  Args.PDF.SetFont('Arial', 'B', 14);
  Args.PDF.TextBox(FIndent, 0, Args.Band.Width, Args.Band.Height,
    'Page Header', 'C', 'L', False);
end;

procedure TReport1.DrawProducts(Args: TFPDFBandDrawArgs);
begin
  Inc(FProduct);

  Args.PDF.SetFillColor(235, 235, 235);
  Args.PDF.Rect(0, 0, Args.Band.Width, Args.Band.Height, 'DF');

  Args.PDF.SetFont('Arial', '', 14);
  Args.PDF.TextBox(FIndent, 0, Args.Band.Width, Args.Band.Height,
    Format('Product: %d', [FProduct]), 'C', 'L', False);

  Args.DrawAgain := FProduct < 20;
end;

procedure TReport1.DrawData(Args: TFPDFBandDrawArgs);
begin
  Args.PDF.SetFillColor(222, 177, 177);
  Args.PDF.Rect(0, 0, Args.Band.Width, Args.Band.Height, 'DF');

  Args.PDF.SetFont('Arial', '', 14);
  Args.PDF.TextBox(FIndent, 0, Args.Band.Width, Args.Band.Height,
    'Data Band', 'C', 'L', False);
end;

procedure TReport1.DrawFillEmptySpace(Args: TFPDFBandDrawArgs);
begin
  Args.Band.Height := Args.FreeSpace - Args.ReservedSpace;
  Args.PDF.SetFillColor(250, 250, 250);
  Args.PDF.Rect(0, 0, Args.Band.Width, Args.Band.Height, 'DF');

  Args.PDF.SetFont('Arial', '', 14);
  Args.PDF.TextBox(0, 0, Args.Band.Width, Args.Band.Height,
    'Fill Empty Space', 'C', 'C', False);
end;

procedure TReport1.DrawOverlay(Args: TFPDFBandDrawArgs);
var
  PreviousTextColor: string;
begin
  PreviousTextColor := Args.PDF.TextColor;
  try
    Args.PDF.SetTextColor(200, 200, 200);
    Args.PDF.SetFont('Arial', 'B', 48);
    Args.PDF.Rotate(45, Args.PageWidth / 2, Args.PageHeight / 2);
    try
      Args.PDF.TextBox(
        0, (Args.PageHeight / 2) - 10,
        Args.PageWidth, 20,
        'OVERLAY', 'C', 'C', False
      );
    finally
      Args.PDF.Rotate(0, Args.PageWidth / 2, Args.PageHeight / 2);
    end;
  finally
    Args.PDF.TextColor := PreviousTextColor;
  end;
end;

procedure TReport1.DrawBottomMargin(Args: TFPDFBandDrawArgs);
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

procedure TReport1.DrawCustomers(Args: TFPDFBandDrawArgs);
begin
  Inc(FCustomer);

  Args.PDF.SetFillColor(252, 245, 232);
  Args.PDF.Rect(0, 0, Args.Band.Width, Args.Band.Height, 'DF');

  Args.PDF.SetFont('Arial', '', 14);
  Args.PDF.TextBox(FIndent, 0, Args.Band.Width, Args.Band.Height,
    Format('Customer: %d', [FCustomer]), 'C', 'L', False);

  Args.DrawAgain := FCustomer < 10;
end;

procedure TReport1.DrawPageFooter(Args: TFPDFBandDrawArgs);
begin
  Args.PDF.SetFillColor(245, 245, 141);
  Args.PDF.Rect(0, 0, Args.Band.Width, Args.Band.Height, 'DF');

  Args.PDF.SetFont('Arial', 'B', 14);
  Args.PDF.TextBox(FIndent, 0, Args.Band.Width, Args.Band.Height,
    'Page Footer', 'C', 'L', False);
end;

end.
