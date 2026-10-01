{

 FPDF Pascal Charts
 https://github.com/Projeto-ACBr-Oficial/FPDF-Pascal

 Graficos de barras (horizontal/vertical, series agrupadas) desenhados com
 as primitivas de TFPDFExt (Rect, Line, Text). Nao depende de fontes TTF/Unicode.

 Os textos seguem a mesma regra do resto da biblioteca (SetUTF8 + core fonts).
 Todas as medidas internas sao proporcionais a PDF.FontSize, entao funcionam
 em qualquer unidade de pagina (mm, pt, cm, in).

}

unit fpdf_charts;

{$I fpdf.inc}

interface

uses
  Classes, SysUtils,
  fpdf, fpdf_ext;

type
  TIntArray = array of Integer;

  TFPDFChartColor = record
    R, G, B: Byte;
  end;

  TFPDFChartOrientation = (coHorizontal, coVertical);

  TFPDFChartSeries = record
    Name: string;
    Color: TFPDFChartColor;
  end;

  TFPDFChartCategory = record
    Caption: string;
    Values: array of Double;   // um valor por serie; faltantes contam como 0
  end;

  { TFPDFBarChart }

  TFPDFBarChart = class
  private
    fTitle: string;
    fOrientation: TFPDFChartOrientation;
    fSeries: array of TFPDFChartSeries;
    fCategories: array of TFPDFChartCategory;
    fSortDescending: Boolean;
    fMaxCategories: Integer;
    fShowValues: Boolean;
    fShowLegend: Boolean;
    fShowGrid: Boolean;
    fFontSizePt: Double;
    fTitleSizePt: Double;
    fBarThickness: Double;
    fValueFormat: string;
    fEmptyText: string;
    fFontFamily: string;
    fTitleStyle: string;
    fAxisMax: Double;
    fMaxLabelFraction: Double;
    fTitleColor: TFPDFChartColor;
    fTextColor: TFPDFChartColor;
    fGridColor: TFPDFChartColor;

    function GetSeriesCount: Integer;
    function GetCategoryCount: Integer;
    function VisibleOrder: TIntArray;
    function MaxValue: Double;
    function FormatValue(AValue: Double): string;
  protected
    function ShowLegendNow: Boolean;
    procedure DrawHorizontal(APDF: TFPDFExt; vX, vY, vWidth, vHeight: Double;
      const AOrder: TIntArray);
    procedure DrawVertical(APDF: TFPDFExt; vX, vY, vWidth, vHeight: Double;
      const AOrder: TIntArray);
  public
    constructor Create;

    // Series / dados
    function AddSeries(const AName: string): Integer; overload;
    function AddSeries(const AName: string; AColor: TFPDFChartColor): Integer; overload;
    function AddCategory(const ACaption: string; const AValues: array of Double): Integer;
    procedure Clear;

    // Desenha o grafico em (vX, vY) com vWidth de largura. vHeight = 0 calcula
    // a altura necessaria (barras horizontais). Devolve a altura usada, pra paginar.
    function Draw(APDF: TFPDFExt; vX, vY, vWidth: Double; vHeight: Double = 0): Double;
    // Altura que Draw(vHeight = 0) usaria, sem desenhar. Precisa do PDF so pra
    // converter pontos de fonte na unidade da pagina.
    function CalcHeight(APDF: TFPDFExt; vWidth: Double): Double;

    property Title: string read fTitle write fTitle;
    property Orientation: TFPDFChartOrientation read fOrientation write fOrientation;
    property SeriesCount: Integer read GetSeriesCount;
    property CategoryCount: Integer read GetCategoryCount;
    // Ordena as categorias da maior soma pra menor
    property SortDescending: Boolean read fSortDescending write fSortDescending;
    // 0 = todas; senao mostra so as N primeiras (depois de ordenar)
    property MaxCategories: Integer read fMaxCategories write fMaxCategories;
    property ShowValues: Boolean read fShowValues write fShowValues;
    // Legenda aparece sozinha com 2+ series; True/False forca
    property ShowLegend: Boolean read fShowLegend write fShowLegend;
    property ShowGrid: Boolean read fShowGrid write fShowGrid;
    property FontSizePt: Double read fFontSizePt write fFontSizePt;
    property TitleSizePt: Double read fTitleSizePt write fTitleSizePt;
    // Espessura de cada barra, em multiplos da fonte (padrao 1.0)
    property BarThickness: Double read fBarThickness write fBarThickness;
    // Mascara de FormatFloat para valores e eixo (padrao '0.##')
    property ValueFormat: string read fValueFormat write fValueFormat;
    property EmptyText: string read fEmptyText write fEmptyText;
    // Familia da fonte (padrao 'Helvetica'). Pode ser uma TrueType do AddFont,
    // pra rotulos Unicode; nesse caso adicione tambem o estilo do titulo.
    property FontFamily: string read fFontFamily write fFontFamily;
    // Estilo da fonte do titulo (padrao 'B')
    property TitleStyle: string read fTitleStyle write fTitleStyle;
    // Fixa o fim do eixo; 0 = automatico ("numeros bonitos" 1-2-5)
    property AxisMax: Double read fAxisMax write fAxisMax;
    // Fracao maxima da largura que os rotulos de categoria (horizontal) ocupam
    property MaxLabelFraction: Double read fMaxLabelFraction write fMaxLabelFraction;
    property TitleColor: TFPDFChartColor read fTitleColor write fTitleColor;
    property TextColor: TFPDFChartColor read fTextColor write fTextColor;
    property GridColor: TFPDFChartColor read fGridColor write fGridColor;
  end;

function ChartColor(R, G, B: Byte): TFPDFChartColor;

// "Numeros bonitos" (1-2-5): devolve o fim do eixo e o passo pra cobrir AMax
// com cerca de ATicks divisoes.
procedure NiceScale(AMax: Double; ATicks: Integer; out ANiceMax, AStep: Double);

implementation

uses
  Math;

type
  // acesso aos campos protegidos de TFPDF (estado grafico a restaurar)
  TPDFAccess = class(TFPDFExt);

// Acesso aos membros protegidos do TFPDF. O cast passa por Pointer porque o
// objeto nao e um TPDFAccess de verdade (o FPC com -CR acusaria EInvalidCast).
function PDFAccess(APDF: TFPDFExt): TPDFAccess;
begin
  Result := TPDFAccess(Pointer(APDF));
end;

const
  cPalette: array[0..5] of TFPDFChartColor = (
    (R: 255; G: 192; B: 0),     // amarelo
    (R: 84;  G: 170; B: 80),    // verde
    (R: 220; G: 60;  B: 52),    // vermelho
    (R: 70;  G: 114; B: 196),   // azul
    (R: 160; G: 120; B: 200),   // lilas
    (R: 130; G: 130; B: 130));  // cinza

function ChartColor(R, G, B: Byte): TFPDFChartColor;
begin
  Result.R := R;
  Result.G := G;
  Result.B := B;
end;

procedure NiceScale(AMax: Double; ATicks: Integer; out ANiceMax, AStep: Double);
var
  raw, mag, frac, nice: Double;
begin
  if (ATicks < 2) then
    ATicks := 2;
  if (AMax <= 0) then
  begin
    AStep := 1;
    ANiceMax := ATicks - 1;
    Exit;
  end;

  raw := AMax / (ATicks - 1);
  mag := Power(10, Floor(Log10(raw)));
  frac := raw / mag;
  if (frac <= 1) then
    nice := 1
  else if (frac <= 2) then
    nice := 2
  else if (frac <= 5) then
    nice := 5
  else
    nice := 10;

  AStep := nice * mag;
  ANiceMax := Ceil(AMax / AStep - 1e-9) * AStep;
end;

// Corta AText com '...' ate caber em AMaxWidth
function FitText(APDF: TFPDFExt; const AText: string; AMaxWidth: Double): string;
var
  n: Integer;
begin
  Result := AText;
  if (AMaxWidth <= 0) then
  begin
    Result := '';
    Exit;
  end;
  if (APDF.GetStringWidth(Result) <= AMaxWidth) then
    Exit;

  n := Length(Result);
  while (n > 0) do
  begin
    Dec(n);
    {$IfDef FPC}
    // string UTF-8: nao corta no meio de um caractere multibyte
    while (n > 0) and ((Ord(Result[n+1]) and $C0) = $80) do
      Dec(n);
    {$EndIf}
    if (APDF.GetStringWidth(Copy(AText, 1, n) + '...') <= AMaxWidth) then
    begin
      Result := Copy(AText, 1, n) + '...';
      Exit;
    end;
  end;
  Result := '';
end;

procedure SetColors(APDF: TFPDFExt; const AColor: TFPDFChartColor; Fill, Draw, Text: Boolean);
begin
  if Fill then
    APDF.SetFillColor(AColor.R, AColor.G, AColor.B);
  if Draw then
    APDF.SetDrawColor(AColor.R, AColor.G, AColor.B);
  if Text then
    APDF.SetTextColor(AColor.R, AColor.G, AColor.B);
end;

{ TFPDFBarChart }

constructor TFPDFBarChart.Create;
begin
  inherited Create;
  fTitle := '';
  fOrientation := coHorizontal;
  SetLength(fSeries, 0);
  SetLength(fCategories, 0);
  fSortDescending := False;
  fMaxCategories := 0;
  fShowValues := True;
  fShowLegend := False;
  fShowGrid := True;
  fFontSizePt := 8;
  fTitleSizePt := 10;
  fBarThickness := 1.0;
  fValueFormat := '0.##';
  fFontFamily := 'Helvetica';
  fTitleStyle := 'B';
  fEmptyText := 'Sem dados no per' + Chr(237) + 'odo';
  fAxisMax := 0;
  fMaxLabelFraction := 0.4;
  fTitleColor := ChartColor(0, 0, 0);
  fTextColor := ChartColor(60, 60, 60);
  fGridColor := ChartColor(215, 215, 215);
end;

function TFPDFBarChart.GetSeriesCount: Integer;
begin
  Result := Length(fSeries);
end;

function TFPDFBarChart.GetCategoryCount: Integer;
begin
  Result := Length(fCategories);
end;

function TFPDFBarChart.AddSeries(const AName: string; AColor: TFPDFChartColor): Integer;
begin
  Result := Length(fSeries);
  SetLength(fSeries, Result + 1);
  fSeries[Result].Name := AName;
  fSeries[Result].Color := AColor;
end;

function TFPDFBarChart.AddSeries(const AName: string): Integer;
begin
  Result := AddSeries(AName, cPalette[Length(fSeries) mod Length(cPalette)]);
end;

function TFPDFBarChart.AddCategory(const ACaption: string;
  const AValues: array of Double): Integer;
var
  i: Integer;
begin
  Result := Length(fCategories);
  SetLength(fCategories, Result + 1);
  fCategories[Result].Caption := ACaption;
  SetLength(fCategories[Result].Values, Length(AValues));
  for i := 0 to High(AValues) do
    fCategories[Result].Values[i] := AValues[i];
end;

procedure TFPDFBarChart.Clear;
begin
  SetLength(fSeries, 0);
  SetLength(fCategories, 0);
end;

function TFPDFBarChart.ShowLegendNow: Boolean;
begin
  Result := fShowLegend or (Length(fSeries) > 1);
  if Result then
    Result := Length(fSeries) > 0;
end;

// Indices das categorias na ordem de exibicao (ordenada e limitada a MaxCategories)
function TFPDFBarChart.VisibleOrder: TIntArray;
var
  i, j, t, n: Integer;
  sums: array of Double;
  s: Double;
  k: Integer;
begin
  n := Length(fCategories);
  Result := nil;
  SetLength(Result, n);
  SetLength(sums, n);
  for i := 0 to n - 1 do
  begin
    Result[i] := i;
    s := 0;
    for k := 0 to High(fCategories[i].Values) do
      s := s + fCategories[i].Values[k];
    sums[i] := s;
  end;

  if fSortDescending then
    // insercao: estavel, n pequeno
    for i := 1 to n - 1 do
    begin
      t := Result[i];
      j := i - 1;
      while (j >= 0) and (sums[Result[j]] < sums[t]) do
      begin
        Result[j + 1] := Result[j];
        Dec(j);
      end;
      Result[j + 1] := t;
    end;

  if (fMaxCategories > 0) and (n > fMaxCategories) then
    SetLength(Result, fMaxCategories);
end;

function TFPDFBarChart.MaxValue: Double;
var
  i, k: Integer;
begin
  Result := 0;
  for i := 0 to High(fCategories) do
    for k := 0 to High(fCategories[i].Values) do
      if (k < Length(fSeries)) and (fCategories[i].Values[k] > Result) then
        Result := fCategories[i].Values[k];
end;

function TFPDFBarChart.FormatValue(AValue: Double): string;
begin
  Result := FormatFloat(fValueFormat, AValue);
end;

function TFPDFBarChart.CalcHeight(APDF: TFPDFExt; vWidth: Double): Double;
var
  fs, ts, band, h: Double;
  nCat, nSer: Integer;
begin
  fs := fFontSizePt / PDFAccess(APDF).k;
  ts := fTitleSizePt / PDFAccess(APDF).k;
  nCat := Length(VisibleOrder);
  nSer := Length(fSeries);
  if (nSer < 1) then
    nSer := 1;

  h := 0;
  if (fTitle <> '') then
    h := h + ts * 1.8;
  if ShowLegendNow then
    h := h + fs * 2.0;

  if (nCat = 0) then
    h := h + fs * 3
  else if (fOrientation = coHorizontal) then
  begin
    band := nSer * fs * fBarThickness + fs * 0.7;
    h := h + nCat * band + fs * 1.8;     // + faixa dos numeros do eixo
  end
  else
    h := h + vWidth * 0.55;

  Result := h;
end;

function TFPDFBarChart.Draw(APDF: TFPDFExt; vX, vY, vWidth, vHeight: Double): Double;
var
  acc: TPDFAccess;
  oldFamily, oldStyle, oldDraw, oldFill, oldText: string;
  oldSize, oldLW, fs, ts, y, itemW, lx: Double;
  order: TIntArray;
  i: Integer;
begin
  acc := PDFAccess(APDF);
  oldFamily := acc.FontFamily;
  oldStyle := acc.FontStyle;
  oldSize := acc.FontSizePt;
  oldLW := acc.LineWidth;
  oldDraw := acc.DrawColor;
  oldFill := acc.FillColor;
  oldText := acc.TextColor;

  if (vHeight <= 0) then
    vHeight := CalcHeight(APDF, vWidth);
  Result := vHeight;

  order := VisibleOrder;
  y := vY;
  APDF.SetLineWidth(0.5 / acc.k);
  try
    // titulo
    if (fTitle <> '') then
    begin
      APDF.SetFont(fFontFamily, fTitleStyle, fTitleSizePt);
      ts := PDFAccess(APDF).FontSize;
      SetColors(APDF, fTitleColor, False, False, True);
      APDF.Text(vX, y + ts, FitText(APDF, fTitle, vWidth));
      y := y + ts * 1.8;
    end;

    APDF.SetFont(fFontFamily, '', fFontSizePt);
    fs := PDFAccess(APDF).FontSize;

    // legenda (uma linha; itens alem da largura do grafico sao cortados pela pagina, mantenha nomes curtos)
    if ShowLegendNow then
    begin
      lx := vX;
      for i := 0 to High(fSeries) do
      begin
        itemW := fs * 1.4 + APDF.GetStringWidth(fSeries[i].Name) + fs * 1.2;
        SetColors(APDF, fSeries[i].Color, True, False, False);
        APDF.Rect(lx, y + fs * 0.1, fs * 0.9, fs * 0.9, 'F');
        SetColors(APDF, fTextColor, False, False, True);
        APDF.Text(lx + fs * 1.2, y + fs * 0.85, fSeries[i].Name);
        lx := lx + itemW;
      end;
      y := y + fs * 2.0;
    end;

    if (Length(order) = 0) or (Length(fSeries) = 0) then
    begin
      SetColors(APDF, fTextColor, False, False, True);
      APDF.Text(vX + (vWidth - APDF.GetStringWidth(fEmptyText)) / 2,
        y + fs * 1.8, fEmptyText);
    end
    else if (fOrientation = coHorizontal) then
      DrawHorizontal(APDF, vX, y, vWidth, vY + vHeight - y, order)
    else
      DrawVertical(APDF, vX, y, vWidth, vY + vHeight - y, order);
  finally
    APDF.SetFont(oldFamily, oldStyle, oldSize);
    APDF.SetLineWidth(oldLW);
    acc._out(oldDraw);
    acc._out(oldFill);
    acc._out(oldText);
  end;
end;

procedure TFPDFBarChart.DrawHorizontal(APDF: TFPDFExt; vX, vY, vWidth,
  vHeight: Double; const AOrder: TIntArray);
var
  fs, labelW, valueW, px0, px1, plotW, axisMax, step, bandH, barH, gap, ph, tx, w, cy, by: Double;
  nCat, nSer, i, s: Integer;
  txt: string;
  v: Double;
begin
  fs := PDFAccess(APDF).FontSize;
  nCat := Length(AOrder);
  nSer := Length(fSeries);

  // coluna dos rotulos: largura do maior rotulo, limitada a uma fracao do total
  labelW := 0;
  for i := 0 to nCat - 1 do
    labelW := Max(labelW, APDF.GetStringWidth(fCategories[AOrder[i]].Caption));
  labelW := Min(labelW, vWidth * fMaxLabelFraction) + fs * 0.8;

  // reserva pro texto do valor depois da maior barra
  valueW := 0;
  if fShowValues then
    valueW := APDF.GetStringWidth(FormatValue(MaxValue)) + fs * 0.8;

  px0 := vX + labelW;
  px1 := vX + vWidth - valueW;
  plotW := px1 - px0;
  if (plotW < fs) then
    Exit;

  if (fAxisMax > 0) then
  begin
    axisMax := fAxisMax;
    NiceScale(axisMax, 6, tx, step);
  end
  else
    NiceScale(MaxValue, 6, axisMax, step);

  ph := vHeight - fs * 1.8;           // area das barras (sem a faixa do eixo)
  bandH := ph / nCat;
  barH := Min(nSer * fs * fBarThickness, bandH * 0.85) / nSer;
  gap := bandH - barH * nSer;

  // grade + numeros do eixo
  w := 0;
  while (w <= axisMax + step * 1e-6) do
  begin
    tx := px0 + plotW * w / axisMax;
    if fShowGrid then
    begin
      SetColors(APDF, fGridColor, False, True, False);
      APDF.Line(tx, vY, tx, vY + ph);
    end;
    txt := FormatValue(w);
    SetColors(APDF, fTextColor, False, False, True);
    APDF.Text(tx - APDF.GetStringWidth(txt) / 2, vY + ph + fs * 1.2, txt);
    w := w + step;
  end;

  // barras
  for i := 0 to nCat - 1 do
  begin
    cy := vY + i * bandH;
    SetColors(APDF, fTextColor, False, False, True);
    txt := FitText(APDF, fCategories[AOrder[i]].Caption, labelW - fs * 0.8);
    APDF.Text(vX, cy + bandH / 2 + fs * 0.35, txt);

    by := cy + gap / 2;
    for s := 0 to nSer - 1 do
    begin
      v := 0;
      if (s < Length(fCategories[AOrder[i]].Values)) then
        v := fCategories[AOrder[i]].Values[s];
      w := plotW * Min(Max(v, 0.0), axisMax) / axisMax;
      if (w > 0) then
      begin
        SetColors(APDF, fSeries[s].Color, True, False, False);
        APDF.Rect(px0, by, w, barH, 'F');
      end;
      if fShowValues then
      begin
        SetColors(APDF, fTextColor, False, False, True);
        APDF.Text(px0 + w + fs * 0.3, by + barH / 2 + fs * 0.3, FormatValue(v));
      end;
      by := by + barH;
    end;
  end;

  // eixo
  SetColors(APDF, fTextColor, False, True, False);
  APDF.Line(px0, vY, px0, vY + ph);
end;

procedure TFPDFBarChart.DrawVertical(APDF: TFPDFExt; vX, vY, vWidth,
  vHeight: Double; const AOrder: TIntArray);
var
  fs, axisW, valueH, py0, py1, plotH, axisMax, step, bandW, barW, gap, ty, w, bx, h, labelH: Double;
  nCat, nSer, i, s: Integer;
  txt: string;
  v: Double;
begin
  fs := PDFAccess(APDF).FontSize;
  nCat := Length(AOrder);
  nSer := Length(fSeries);

  if (fAxisMax > 0) then
    NiceScale(fAxisMax, 6, axisMax, step)
  else
    NiceScale(MaxValue, 6, axisMax, step);

  axisW := APDF.GetStringWidth(FormatValue(axisMax)) + fs * 0.8;
  labelH := fs * 1.6;
  valueH := 0;
  if fShowValues then
    valueH := fs * 1.2;

  py0 := vY + valueH;                   // topo da area das barras
  py1 := vY + vHeight - labelH;         // base
  plotH := py1 - py0;
  if (plotH < fs) then
    Exit;

  bandW := (vWidth - axisW) / nCat;
  barW := Min(bandW * 0.85 / nSer, fs * 3 * fBarThickness);
  gap := bandW - barW * nSer;

  // grade horizontal + numeros do eixo
  w := 0;
  while (w <= axisMax + step * 1e-6) do
  begin
    ty := py1 - plotH * w / axisMax;
    if fShowGrid then
    begin
      SetColors(APDF, fGridColor, False, True, False);
      APDF.Line(vX + axisW, ty, vX + vWidth, ty);
    end;
    txt := FormatValue(w);
    SetColors(APDF, fTextColor, False, False, True);
    APDF.Text(vX + axisW - fs * 0.4 - APDF.GetStringWidth(txt), ty + fs * 0.3, txt);
    w := w + step;
  end;

  for i := 0 to nCat - 1 do
  begin
    bx := vX + axisW + i * bandW + gap / 2;
    for s := 0 to nSer - 1 do
    begin
      v := 0;
      if (s < Length(fCategories[AOrder[i]].Values)) then
        v := fCategories[AOrder[i]].Values[s];
      h := plotH * Min(Max(v, 0.0), axisMax) / axisMax;
      if (h > 0) then
      begin
        SetColors(APDF, fSeries[s].Color, True, False, False);
        APDF.Rect(bx, py1 - h, barW, h, 'F');
      end;
      if fShowValues then
      begin
        txt := FormatValue(v);
        SetColors(APDF, fTextColor, False, False, True);
        APDF.Text(bx + barW / 2 - APDF.GetStringWidth(txt) / 2, py1 - h - fs * 0.25, txt);
      end;
      bx := bx + barW;
    end;

    txt := FitText(APDF, fCategories[AOrder[i]].Caption, bandW - fs * 0.2);
    SetColors(APDF, fTextColor, False, False, True);
    APDF.Text(vX + axisW + i * bandW + (bandW - APDF.GetStringWidth(txt)) / 2,
      py1 + fs * 1.2, txt);
  end;

  SetColors(APDF, fTextColor, False, True, False);
  APDF.Line(vX + axisW, py0, vX + axisW, py1);
  APDF.Line(vX + axisW, py1, vX + vWidth, py1);
end;

end.
