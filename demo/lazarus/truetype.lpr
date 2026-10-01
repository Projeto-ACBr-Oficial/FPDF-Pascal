program truetype;

{$mode objfpc}{$H+}

{ Fontes TrueType com texto Unicode: AddFont embute so os glyphs usados
  (subset, Type0/Identity-H) e o texto pode ser copiado do PDF (ToUnicode).

  A fonte e procurada em demo/files e nas pastas de fontes do Windows, Linux
  e macOS. Para usar outra, coloque o .ttf em demo/files com os nomes abaixo. }

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils,
  fpdf, fpdf_ext;

const
  // regular, negrito, italico (o primeiro trio encontrado e usado)
  Candidatas: array[0..5, 0..2] of string = (
    ('..\files\DejaVuSans.ttf', '..\files\DejaVuSans-Bold.ttf', '..\files\DejaVuSans-Oblique.ttf'),
    ('C:\Windows\Fonts\arial.ttf', 'C:\Windows\Fonts\arialbd.ttf', 'C:\Windows\Fonts\ariali.ttf'),
    ('/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf', '/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf', '/usr/share/fonts/truetype/dejavu/DejaVuSans-Oblique.ttf'),
    ('/usr/share/fonts/dejavu/DejaVuSans.ttf', '/usr/share/fonts/dejavu/DejaVuSans-Bold.ttf', '/usr/share/fonts/dejavu/DejaVuSans-Oblique.ttf'),
    ('/usr/share/fonts/TTF/DejaVuSans.ttf', '/usr/share/fonts/TTF/DejaVuSans-Bold.ttf', '/usr/share/fonts/TTF/DejaVuSans-Oblique.ttf'),
    ('/System/Library/Fonts/Supplemental/Arial.ttf', '/System/Library/Fonts/Supplemental/Arial Bold.ttf', '/System/Library/Fonts/Supplemental/Arial Italic.ttf'));

  Idiomas: array[0..9, 0..1] of string = (
    ('Português', 'Ação, coração, informação e pão de queijo'),
    ('Español', '¿Qué tal? El niño come piñata en año nuevo'),
    ('Français', 'Œuvre, garçon, été, naïve, cœur'),
    ('Deutsch', 'Größe, Straße, Übung, schön'),
    ('Polski', 'Zażółć gęślą jaźń'),
    ('Čeština', 'Příliš žluťoučký kůň úpěl ďábelské ódy'),
    ('Türkçe', 'Işık, şeker, dağ, göz'),
    ('Ελληνικά', 'Καλημέρα κόσμε, αλφάβητο ΑΒΓΔ'),
    ('Русский', 'Привет, мир! Съешь же ещё этих булок'),
    ('Símbolos', '€ £ ¥ © ® ™ ± ≠ ≤ ≥ ∞ √ ← → ↑ ↓ • … “ ” – —'));

var
  pdf: TFPDFExt;
  dir: string;
  i, j: Integer;
  achou: Boolean;
  arq: array[0..2] of string;

function Caminho(const AArquivo: string): string;
begin
  Result := AArquivo;
  if (Pos(':', Result) = 0) and (Copy(Result, 1, 1) <> '/') then
    Result := dir + Result;
  Result := StringReplace(Result, '\', PathDelim, [rfReplaceAll]);
end;

begin
  dir := ExtractFilePath(ParamStr(0));

  // regular e negrito sao obrigatorios, italico e opcional
  achou := False;
  i := 0;
  while (not achou) and (i <= High(Candidatas)) do
  begin
    achou := FileExists(Caminho(Candidatas[i, 0])) and FileExists(Caminho(Candidatas[i, 1]));
    if achou then
      for j := 0 to 2 do
        arq[j] := Caminho(Candidatas[i, j]);
    Inc(i);
  end;
  if not achou then
  begin
    WriteLn('Nenhuma fonte TrueType encontrada. Copie DejaVuSans.ttf e DejaVuSans-Bold.ttf para demo/files.');
    Halt(1);
  end;

  pdf := TFPDFExt.Create();
  try
    pdf.SetTitle('Fontes TrueType com Unicode');

    // Uma familia por estilo, como no FPDF original
    pdf.AddFont('Sans', '', arq[0]);
    pdf.AddFont('Sans', 'B', arq[1]);
    if FileExists(arq[2]) then
      pdf.AddFont('Sans', 'I', arq[2]);

    pdf.AddPage();

    // Titulo
    pdf.SetFont('Sans', 'B', 18);
    pdf.Cell(0, 10, 'Fontes TrueType com texto Unicode', '0', 1, 'C');
    pdf.SetFont('Sans', '', 9);
    pdf.SetTextColor(110, 110, 110);
    pdf.Cell(0, 5, 'Fonte: ' + ExtractFileName(arq[0]), '0', 1, 'C');
    pdf.SetTextColor(0, 0, 0);
    pdf.Ln(4);

    // MultiCell justificado
    pdf.SetFont('Sans', '', 11);
    pdf.MultiCell(0, 5.5,
      'AddFont registra um arquivo .ttf e SetFont passa a usá-lo como qualquer outra fonte. ' +
      'Só os caracteres que aparecem no documento são embutidos no PDF, então o arquivo ' +
      'continua pequeno, e o texto pode ser selecionado e copiado com acentos, letras gregas, ' +
      'cirílicas e símbolos. Este parágrafo usa MultiCell com alinhamento justificado.',
      '0', 'J');
    pdf.Ln(3);

    // Tabela com Cell
    pdf.SetFont('Sans', 'B', 10);
    pdf.SetFillColor(70, 114, 196);
    pdf.SetTextColor(255, 255, 255);
    pdf.Cell(35, 7, 'Idioma', '1', 0, 'L', True);
    pdf.Cell(0, 7, 'Exemplo', '1', 1, 'L', True);
    pdf.SetTextColor(0, 0, 0);
    pdf.SetFont('Sans', '', 10);
    for i := 0 to High(Idiomas) do
    begin
      if Odd(i) then
        pdf.SetFillColor(235, 241, 250)
      else
        pdf.SetFillColor(255, 255, 255);
      pdf.Cell(35, 7, Idiomas[i, 0], '1', 0, 'L', True);
      pdf.Cell(0, 7, Idiomas[i, 1], '1', 1, 'L', True);
    end;
    pdf.Ln(4);

    // Alinhamentos e sublinhado
    pdf.Cell(60, 7, 'À esquerda ←', '1', 0, 'L');
    pdf.Cell(60, 7, '↔ Centralizado ↔', '1', 0, 'C');
    pdf.Cell(0, 7, 'À direita →', '1', 1, 'R');
    pdf.SetFont('Sans', 'U', 10);
    pdf.Cell(0, 8, 'Texto sublinhado com ç, ã, Ω e Ж', '0', 1, 'L');
    if FileExists(arq[2]) then
    begin
      pdf.SetFont('Sans', 'I', 10);
      pdf.Cell(0, 6, 'Itálico: «citação» em outra variação da mesma família', '0', 1, 'L');
    end;
    pdf.Ln(2);

    // Write: texto corrido misturando estilos e um link
    pdf.SetFont('Sans', '', 11);
    pdf.Write(6, 'Write() escreve texto corrido e quebra a linha sozinho. Dá para trocar ');
    pdf.SetFont('Sans', 'B', 11);
    pdf.Write(6, 'o estilo no meio da frase');
    pdf.SetFont('Sans', '', 11);
    pdf.Write(6, ' e criar ');
    pdf.SetTextColor(0, 0, 200);
    pdf.SetFont('Sans', 'U', 11);
    pdf.Write(6, 'links como este', 'https://github.com/Projeto-ACBr-Oficial/FPDF-Pascal');
    pdf.SetTextColor(0, 0, 0);
    pdf.SetFont('Sans', '', 11);
    pdf.Write(6, '. Fórmulas: E = m·c², Σx ≥ 0, área = π·r².');
    pdf.Ln(10);

    // Text com rotacao
    pdf.SetFont('Sans', 'B', 12);
    pdf.SetTextColor(220, 60, 52);
    pdf.Rotate(6, 25, pdf.GetY + 14);
    pdf.Text(25, pdf.GetY + 14, 'Text() girado 6°: Ωmega, Ψυχή, Жизнь');
    pdf.Rotate(0);
    pdf.SetTextColor(0, 0, 0);
    pdf.SetY(pdf.GetY + 22);

    pdf.SaveToFile(dir + PathDelim + 'truetype.pdf');
  finally
    pdf.Free;
  end;
end.
