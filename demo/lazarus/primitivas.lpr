program primitivas;

{$mode objfpc}{$H+}

{ Primitivas geometricas do TFPDFExt: Circle, Ellipse, Arc, Sector, Polygon,
  PolyLine e Curve. Estilo: 'D' contorno, 'F' preenchido, 'DF' os dois. }

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils,
  fpdf, fpdf_ext;

const
  ColEsq = 15;
  ColDir = 107;
  LargQuadro = 88;
  AltQuadro = 60;

  Valores: array[0..4] of Double = (35, 25, 18, 14, 8);
  Regioes: array[0..4] of string = ('Norte', 'Sul', 'Leste', 'Oeste', 'Centro');
  Cores: array[0..4, 0..2] of Byte = (
    (70, 114, 196), (237, 125, 49), (165, 165, 165), (255, 192, 0), (91, 155, 213));

function P(AX, AY: Double): TFPDFPoint;
begin
  Result.X := AX;
  Result.Y := AY;
end;

procedure Quadro(APDF: TFPDFExt; AX, AY: Double; const ATitulo: string);
begin
  APDF.SetLineWidth(0.2);
  APDF.SetDrawColor(200, 200, 200);
  APDF.RoundedRect(AX, AY, LargQuadro, AltQuadro, 2, '1234', 'D');
  APDF.SetFont('Helvetica', 'B', 9);
  APDF.SetTextColor(40, 40, 40);
  APDF.Text(AX + 3, AY + 6, ATitulo);
  APDF.SetFont('Helvetica', '', 7);
  APDF.SetTextColor(90, 90, 90);
  APDF.SetDrawColor(0, 0, 0);
end;

// Texto centralizado em ACx
procedure Rotulo(APDF: TFPDFExt; ACx, AY: Double; const ATexto: string);
begin
  APDF.Text(ACx - APDF.GetStringWidth(ATexto) / 2, AY, ATexto);
end;

var
  pdf: TFPDFExt;
  pts: array of TFPDFPoint;
  i: Integer;
  x, y, cx, cy, ang, total, ini, fim, r: Double;
begin
  pdf := TFPDFExt.Create();
  try
    pdf.SetTitle('Primitivas geometricas');
    pdf.AddPage();

    pdf.SetFont('Helvetica', 'B', 16);
    pdf.Cell(0, 8, 'Primitivas geométricas', '0', 1, 'C');
    pdf.SetFont('Helvetica', '', 9);
    pdf.Cell(0, 5, 'TFPDFExt: Circle, Ellipse, Arc, Sector, Polygon, PolyLine e Curve', '0', 1, 'C');

    { 1. Circle }
    x := ColEsq; y := 30;
    Quadro(pdf, x, y, 'Circle(x, y, raio, estilo)');
    pdf.SetLineWidth(0.6);
    pdf.SetFillColor(70, 114, 196);
    pdf.Circle(x + 17, y + 30, 12, 'D');
    pdf.Circle(x + 44, y + 30, 12, 'F');
    pdf.Circle(x + 71, y + 30, 12, 'DF');
    Rotulo(pdf, x + 17, y + 52, '''D''');
    Rotulo(pdf, x + 44, y + 52, '''F''');
    Rotulo(pdf, x + 71, y + 52, '''DF''');

    { 2. Ellipse }
    x := ColDir;
    Quadro(pdf, x, y, 'Ellipse(x, y, raioX, raioY, estilo)');
    pdf.SetLineWidth(0.4);
    pdf.SetFillColor(255, 192, 0);
    pdf.Ellipse(x + 22, y + 31, 16, 8, 'DF');
    pdf.SetFillColor(91, 155, 213);
    pdf.Ellipse(x + 50, y + 31, 7, 16, 'DF');
    // aneis concentricos
    for i := 0 to 4 do
    begin
      pdf.SetFillColor(230 - i * 35, 240 - i * 25, 255);
      pdf.Ellipse(x + 74, y + 31, 11 - i * 2, 15 - i * 2.8, 'F');
    end;
    Rotulo(pdf, x + 44, y + 54, 'cores e raios diferentes');

    { 3. Arc }
    x := ColEsq; y := y + AltQuadro + 6;
    Quadro(pdf, x, y, 'Arc(x, y, rx, ry, inicio, fim, estilo)');
    pdf.SetLineWidth(0.8);
    pdf.SetDrawColor(220, 60, 52);
    pdf.Arc(x + 15, y + 32, 10, 10, 0, 90, 'D');
    pdf.Arc(x + 37, y + 32, 10, 10, 0, 180, 'D');
    pdf.Arc(x + 60, y + 32, 10, 10, 30, 330, 'D');
    pdf.SetLineWidth(0.3);
    pdf.SetDrawColor(0, 0, 0);
    pdf.SetFillColor(84, 170, 80);
    pdf.Arc(x + 78, y + 32, 6, 10, 90, 270, 'DF');
    Rotulo(pdf, x + 15, y + 50, '0..90');
    Rotulo(pdf, x + 37, y + 50, '0..180');
    Rotulo(pdf, x + 60, y + 50, '30..330');
    Rotulo(pdf, x + 78, y + 50, '''DF''');
    Rotulo(pdf, x + 44, y + 56, 'graus anti-horários a partir das 3h; ''F'' fecha com a corda');

    { 4. Sector: grafico de pizza }
    x := ColDir;
    Quadro(pdf, x, y, 'Sector: pizza (horário, 0 = topo)');
    total := 0;
    for i := 0 to High(Valores) do
      total := total + Valores[i];
    cx := x + 25; cy := y + 33; r := 19;
    ini := 0;
    pdf.SetLineWidth(0.5);
    pdf.SetDrawColor(255, 255, 255);
    for i := 0 to High(Valores) do
    begin
      fim := ini + Valores[i] / total * 360;
      pdf.SetFillColor(Cores[i, 0], Cores[i, 1], Cores[i, 2]);
      pdf.Sector(cx, cy, r, ini, fim, 'FD');
      // legenda
      pdf.Rect(x + 52, y + 15 + i * 7, 4, 4, 'F');
      pdf.Text(x + 58, y + 18.3 + i * 7,
        Format('%s  %.0f%%', [Regioes[i], Valores[i] / total * 100]));
      ini := fim;
    end;
    pdf.SetDrawColor(0, 0, 0);

    { 5. Sector: rosca e medidor }
    x := ColEsq; y := y + AltQuadro + 6;
    Quadro(pdf, x, y, 'Sector: rosca e medidor');
    // rosca = pizza + circulo branco no meio
    cx := x + 22; cy := y + 33; r := 17;
    ini := 0;
    pdf.SetDrawColor(255, 255, 255);
    pdf.SetLineWidth(0.5);
    for i := 0 to High(Valores) do
    begin
      fim := ini + Valores[i] / total * 360;
      pdf.SetFillColor(Cores[i, 0], Cores[i, 1], Cores[i, 2]);
      pdf.Sector(cx, cy, r, ini, fim, 'FD');
      ini := fim;
    end;
    pdf.SetFillColor(255, 255, 255);
    pdf.Circle(cx, cy, 9, 'F');
    pdf.SetFont('Helvetica', 'B', 10);
    pdf.SetTextColor(40, 40, 40);
    Rotulo(pdf, cx, cy + 1.5, FloatToStr(total));
    // medidor: de -90 (esquerda) a +90 (direita), sentido horario
    cx := x + 66; cy := y + 40; r := 18;
    pdf.SetFillColor(84, 170, 80);
    pdf.Sector(cx, cy, r, -90, -30, 'FD');
    pdf.SetFillColor(255, 192, 0);
    pdf.Sector(cx, cy, r, -30, 30, 'FD');
    pdf.SetFillColor(220, 60, 52);
    pdf.Sector(cx, cy, r, 30, 90, 'FD');
    pdf.SetFillColor(255, 255, 255);
    pdf.Sector(cx, cy, 10, -91, 91, 'F');
    // ponteiro em 72% da escala
    ang := (-90 + 0.72 * 180) * Pi / 180;
    pdf.SetDrawColor(40, 40, 40);
    pdf.SetLineWidth(0.8);
    pdf.Line(cx, cy, cx + 16 * Sin(ang), cy - 16 * Cos(ang));
    pdf.SetFillColor(40, 40, 40);
    pdf.Circle(cx, cy, 1.5, 'F');
    pdf.SetFont('Helvetica', '', 7);
    Rotulo(pdf, cx, cy + 8, '72%');
    pdf.SetDrawColor(0, 0, 0);

    { 6. Polygon }
    x := ColDir;
    Quadro(pdf, x, y, 'Polygon(pontos, estilo)');
    pdf.SetLineWidth(0.4);
    // triangulo
    pdf.SetFillColor(84, 170, 80);
    pdf.Polygon([P(x + 6, y + 48), P(x + 18, y + 18), P(x + 30, y + 48)], 'DF');
    // hexagono
    SetLength(pts, 6);
    for i := 0 to 5 do
      pts[i] := P(x + 46 + 12 * Cos(i * Pi / 3), y + 33 + 12 * Sin(i * Pi / 3));
    pdf.SetFillColor(91, 155, 213);
    pdf.Polygon(pts, 'DF');
    // estrela de 5 pontas (raios alternados)
    SetLength(pts, 10);
    for i := 0 to 9 do
    begin
      ang := -Pi / 2 + i * Pi / 5;
      if Odd(i) then r := 5.5 else r := 13;
      pts[i] := P(x + 74 + r * Cos(ang), y + 34 + r * Sin(ang));
    end;
    pdf.SetFillColor(255, 192, 0);
    pdf.Polygon(pts, 'DF');

    { 7. PolyLine }
    x := ColEsq; y := y + AltQuadro + 6;
    Quadro(pdf, x, y, 'PolyLine(pontos): linha aberta');
    // eixos
    pdf.SetLineWidth(0.2);
    pdf.SetDrawColor(160, 160, 160);
    pdf.Line(x + 6, y + 33, x + 84, y + 33);
    pdf.Line(x + 6, y + 12, x + 6, y + 54);
    // seno e cosseno
    SetLength(pts, 61);
    for i := 0 to 60 do
      pts[i] := P(x + 6 + i * 1.3, y + 33 - 16 * Sin(i * 2 * Pi / 30));
    pdf.SetLineWidth(0.6);
    pdf.SetDrawColor(70, 114, 196);
    pdf.PolyLine(pts);
    for i := 0 to 60 do
      pts[i] := P(x + 6 + i * 1.3, y + 33 - 16 * Cos(i * 2 * Pi / 30));
    pdf.SetDrawColor(237, 125, 49);
    pdf.SetDash(1, 1);
    pdf.PolyLine(pts);
    pdf.SetDash(0, 0);
    pdf.SetDrawColor(0, 0, 0);

    { 8. Curve }
    x := ColDir;
    Quadro(pdf, x, y, 'Curve: Bézier cúbica (p0, c1, c2, p3)');
    // pontos de controle
    pdf.SetLineWidth(0.2);
    pdf.SetDrawColor(170, 170, 170);
    pdf.SetDash(0.8, 0.8);
    pdf.Line(x + 8, y + 50, x + 22, y + 12);
    pdf.Line(x + 66, y + 12, x + 80, y + 50);
    pdf.SetDash(0, 0);
    pdf.SetFillColor(220, 60, 52);
    pdf.Circle(x + 22, y + 12, 1.2, 'F');
    pdf.Circle(x + 66, y + 12, 1.2, 'F');
    pdf.SetFillColor(40, 40, 40);
    pdf.Circle(x + 8, y + 50, 1.2, 'F');
    pdf.Circle(x + 80, y + 50, 1.2, 'F');
    // curva
    pdf.SetLineWidth(0.8);
    pdf.SetDrawColor(70, 114, 196);
    pdf.Curve(x + 8, y + 50, x + 22, y + 12, x + 66, y + 12, x + 80, y + 50, 'D');
    // curva fechada preenchida (gota)
    pdf.SetLineWidth(0.3);
    pdf.SetDrawColor(0, 0, 0);
    pdf.SetFillColor(255, 192, 0);
    pdf.Curve(x + 44, y + 52, x + 30, y + 40, x + 58, y + 40, x + 44, y + 52, 'DF');
    Rotulo(pdf, x + 44, y + 57, 'pontos vermelhos = controle');

    pdf.SaveToFile(ExtractFilePath(ParamStr(0)) + PathDelim + 'primitivas.pdf');
  finally
    pdf.Free;
  end;
end.
