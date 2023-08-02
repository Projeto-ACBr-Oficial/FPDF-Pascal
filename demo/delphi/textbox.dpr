program textbox;

uses
  Classes,
  SysUtils,
  fpdf,
  fpdf_ext;

var
  pdf: TFPDFExt;
  Text: string;
  x, y, w, h: double;


procedure AlignmentTest(const vAlign, hAlign: char);
var
  vA, hA: string;
begin
  case vAlign of
    'C': vA := 'Center';
    'B': vA := 'Bottom';
  else
    vA := 'Top';
  end;

  case hAlign of
    'C': hA := 'Center';
    'R': hA := 'Right';
  else
    hA := 'Left';
  end;

  Text := Format('%s vertical alignment' + sLineBreak + '%s horizontal alignment', [vA, hA]);

  pdf.TextBox(x, y, w, h, Text, vAlign, hAlign, True, True, True);
end;

begin
  pdf := TFPDFExt.Create();
  try
    pdf.AddPage();
    pdf.SetLineWidth(0.1);

    // ######################################
    // Border, WordWrap and Stretched
    // ######################################
    pdf.SetFont('Arial', 'B', 14);
    pdf.SetXY(10, 10);
    pdf.WriteHTML('<u><b>Border, WordWrap and Stretched</b></u>');

    Text :=
      'Lorem Ipsum is simply dummy text of the printing and typesetting ' +
      'industry. Lorem Ipsum has been the industry''s standard dummy text ' +
      'ever since the 1500s, when an unknown printer took a galley of type ' +
      'and scrambled it to make a type specimen book. It has survived not ' +
      'only five centuries, but also the leap into electronic typesetting, ' +
      'remaining essentially unchanged. It was popularised in the 1960s with ' +
      'the release of Letraset sheets containing Lorem Ipsum passages, and ' +
      'more recently with desktop publishing software like Aldus PageMaker ' +
      'including versions of Lorem Ipsum.';
    pdf.SetFont('Arial', '', 14);
    pdf.TextBox(10, 20, 90, 30, Text, 'T', 'L', True, True, True);
    pdf.TextBox(105, 20, 60, 30, Text, 'T', 'L', True, True, True);
    pdf.TextBox(170, 20, 30, 55, Text, 'T', 'L', True, True, True);
    pdf.TextBox(10, 55, 155, 20, Text, 'T', 'L', True, True, True);
    pdf.TextBox(10, 80, 190, 40, Text, 'T', 'L', True, True, True);

    x := 35;
    y := 130;
    h := 20;
    Text :=
      'This text is short enough.' + sLineBreak +
      'This text is way too long. This text is way too long. This text is way too long. This text is way too long.';
    pdf.SetFont('Arial', '', 16);
    pdf.TextBox(x, y, 140, h, Text, 'T', 'L', True, False);
    y := y + h;
    pdf.TextBox(x, y, 140, h, Text, 'C', 'C', True, False, True);
    y := y + h;
    pdf.TextBox(x, y, 140, h, Text, 'B', 'R', True, False, True);


    // ######################################
    // Horizontal and vertical alignment
    // ######################################
    pdf.AddPage();

    pdf.SetFont('Arial', 'B', 14);
    pdf.SetXY(10, 10);
    pdf.WriteHTML('<u><b>Horizontal and vertical alignment</b></u>');

    pdf.SetFont('Arial', '', 12);
    pdf.SetDrawColor(cRed);
    pdf.SetLineWidth(0.3);

    x := 10;
    y := 20;
    w := (pdf.GetPageWidth - 30) / 3;
    h := 30;

    AlignmentTest('T', 'L');
    x := x + w + 5;
    AlignmentTest('T', 'C');
    x := x + w + 5;
    AlignmentTest('T', 'R');

    x := 10;
    y := y + h + 5;

    AlignmentTest('C', 'L');
    x := x + w + 5;
    AlignmentTest('C', 'C');
    x := x + w + 5;
    AlignmentTest('C', 'R');

    x := 10;
    y := y + h + 5;

    AlignmentTest('B', 'L');
    x := x + w + 5;
    AlignmentTest('B', 'C');
    x := x + w + 5;
    AlignmentTest('B', 'R');

    // ######################################
    // Line spacing
    // ######################################
    x := 10;
    y := y + 35;

    pdf.SetFont('Arial', 'B', 14);
    pdf.SetDrawColor(cBlue);
    pdf.SetLineWidth(0.3);
    pdf.SetXY(x, y);
    pdf.WriteHTML('<u><b>Line spacing</b></u>');

    x := 10;
    y := y + 10;
    w := (pdf.GetPageWidth - 30) / 3;
    h := 30;

    Text :=
      'Line 1' + sLineBreak +
      'Line 2' + sLineBreak +
      'Line 3';

    pdf.SetFont('Arial', '', 10);
    pdf.TextBox(x, y, w, h, Text, 'T', 'L', True, True, False, 1);
    x := x + w + 5;
    pdf.TextBox(x, y, w, h, Text, 'C', 'C', True, True, False, 3);
    x := x + w + 5;
    pdf.TextBox(x, y, w, h, Text, 'B', 'R', True, True, False, 5);

    pdf.SaveToFile(ExtractFilePath(ParamStr(0))+PathDelim+'textbox.pdf');
  finally
    pdf.Free;
  end;
end.

