program graficos;

{$APPTYPE CONSOLE}

{ Graficos de barras (fpdf_charts): horizontal e vertical, uma ou varias
  series, ordenacao, limite de categorias, grade, eixo fixo e paginacao
  usando CalcHeight. }

uses
  Classes, SysUtils,
  fpdf, fpdf_ext, fpdf_charts;

const
  Margem = 15;
  Largura = 180;

var
  pdf: TFPDFExt;
  chart: TFPDFBarChart;
  y, h: Double;
  i: Integer;

// Desenha o grafico em y; se nao couber na pagina, comeca outra
procedure Desenhar(AX, AWidth, AHeight: Double; AvancaY: Boolean = True);
begin
  if AHeight <= 0 then
    h := chart.CalcHeight(pdf, AWidth)
  else
    h := AHeight;
  if (y + h > pdf.GetPageHeight - Margem) then
  begin
    pdf.AddPage();
    y := Margem;
  end;
  chart.Draw(pdf, AX, y, AWidth, AHeight);
  if AvancaY then
    y := y + h + 8;
end;

begin
  pdf := TFPDFExt.Create();
  chart := TFPDFBarChart.Create;
  try
    pdf.SetTitle('Graficos de barras');
    pdf.AddPage();
    pdf.SetFont('Helvetica', 'B', 16);
    pdf.Cell(0, 8, 'Gráficos de barras - TFPDFBarChart', '0', 1, 'C');
    y := 28;

    { 1. Horizontal, 3 series, ordenado, com legenda e valores }
    chart.Title := 'Chamados por equipamento (ordenado pelo total)';
    chart.AddSeries('Abertos', ChartColor(255, 192, 0));
    chart.AddSeries('Resolvidos', ChartColor(84, 170, 80));
    chart.AddSeries('Pendentes', ChartColor(220, 60, 52));
    chart.AddCategory('Câmara de congelados', [905, 870, 35]);
    chart.AddCategory('Rack de resfriados', [312, 280, 32]);
    chart.AddCategory('Expositor de carnes com um nome bem comprido que vai ser cortado', [120, 118, 2]);
    chart.AddCategory('Ilha de laticínios', [210, 180, 30]);
    chart.AddCategory('Step-in de congelados', [45, 40, 5]);
    chart.SortDescending := True;
    Desenhar(Margem, Largura, 0);

    { 2. Uma serie, so as 5 maiores, valores com 1 casa }
    chart.Clear;
    chart.Title := 'Top 5 - temperatura média (°C)';
    chart.AddSeries('Média', ChartColor(70, 114, 196));
    chart.AddCategory('Sala de cortes', [11.8]);
    chart.AddCategory('Antecâmara', [6.2]);
    chart.AddCategory('Expositor 01', [3.4]);
    chart.AddCategory('Expositor 02', [2.9]);
    chart.AddCategory('Balcão de frios', [4.75]);
    chart.AddCategory('Câmara 01', [0.6]);
    chart.AddCategory('Câmara 02', [1.1]);
    chart.SortDescending := True;
    chart.MaxCategories := 5;
    chart.ValueFormat := '0.0';
    Desenhar(Margem, 88, 0, False);

    { 3. Mesmo dado, sem valores e com eixo fixo em 20 }
    chart.Title := 'Mesmo dado, eixo fixo (AxisMax = 20)';
    chart.ShowValues := False;
    chart.AxisMax := 20;
    Desenhar(Margem + 92, 88, 0);

    { 4. Vertical, 2 series, altura fixa }
    chart.Clear;
    chart.Title := 'Vendas por dia da semana (vertical)';
    chart.Orientation := coVertical;
    chart.SortDescending := False;
    chart.MaxCategories := 0;
    chart.ValueFormat := '0';
    chart.ShowValues := True;
    chart.AxisMax := 0;
    chart.AddSeries('Semana 1', ChartColor(91, 155, 213));
    chart.AddSeries('Semana 2', ChartColor(237, 125, 49));
    chart.AddCategory('Seg', [32, 41]);
    chart.AddCategory('Ter', [45, 38]);
    chart.AddCategory('Qua', [28, 52]);
    chart.AddCategory('Qui', [51, 47]);
    chart.AddCategory('Sex', [66, 71]);
    chart.AddCategory('Sáb', [80, 92]);
    chart.AddCategory('Dom', [24, 30]);
    Desenhar(Margem, Largura, 70);

    { 5. Cores e fontes personalizadas, sem grade }
    chart.Clear;
    chart.Title := 'Estilo personalizado';
    chart.Orientation := coHorizontal;
    chart.ShowGrid := False;
    chart.FontSizePt := 9;
    chart.TitleSizePt := 13;
    chart.BarThickness := 1.6;
    chart.TitleColor := ChartColor(70, 40, 120);
    chart.TextColor := ChartColor(90, 90, 90);
    chart.AddSeries('Pontos', ChartColor(160, 120, 200));
    chart.AddCategory('Equipe A', [87]);
    chart.AddCategory('Equipe B', [64]);
    chart.AddCategory('Equipe C', [92]);
    Desenhar(Margem, 88, 0, False);

    { 6. Sem dados: mostra EmptyText }
    chart.Clear;
    chart.Title := 'Gráfico sem dados';
    chart.ShowGrid := True;
    chart.FontSizePt := 8;
    chart.TitleSizePt := 11;
    chart.BarThickness := 1;
    chart.TitleColor := ChartColor(0, 0, 0);
    chart.AddSeries('Qtd');
    chart.EmptyText := 'Nenhum registro no período';
    Desenhar(Margem + 92, 88, 0);

    { 7. Muitas categorias: CalcHeight + Desenhar quebram a pagina sozinhos }
    chart.Clear;
    chart.Title := 'Consumo por loja (40 categorias, quebra de página)';
    chart.AddSeries('kWh', ChartColor(84, 170, 80));
    for i := 1 to 40 do
      chart.AddCategory(Format('Loja %.2d', [i]), [100 + ((i * 37) mod 23) * 15]);
    chart.SortDescending := True;
    Desenhar(Margem, Largura, 0);

    pdf.SaveToFile(ExtractFilePath(ParamStr(0)) + PathDelim + 'graficos.pdf');
  finally
    chart.Free;
    pdf.Free;
  end;
end.
