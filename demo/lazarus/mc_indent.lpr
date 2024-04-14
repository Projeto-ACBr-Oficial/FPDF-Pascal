program mc_indent;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  fpdf_ext;

var
  pdf: TFPDFExt;
  InterLigne: integer;
  Txt: string;
  TxtLength, milieu: double;
begin
  pdf := TFPDFExt.Create();
  try
    InterLigne := 7;

    pdf.AddPage();
    pdf.SetMargins(30,10,30);
    pdf.SetFont('Arial','',12);

    txt := 'Cher Pierre';
    TxtLength := pdf.GetStringWidth(Txt);
    milieu := (210-TxtLength)/2;
    pdf.SetX(milieu);
    pdf.Write(5,txt);

    pdf.ln(30);
    txt := 'Voici venu le temps pour toi de renouveler ta licence-assurance, en effet celle-ci expire le 28/9 prochain. Tu trouveras joint à ce document le certificat d''aptitude à faire remplir par le médecin.';
    pdf.MultiCell(0,InterLigne,txt,'0','J',False,15);

    pdf.ln(10);
    txt := 'Je me permets de te rappeler que cette licence est obligatoire et nécessaire ' +
      'à la pratique de notre sport favori, tant à l''occasion de nos entraînements qu''à ' +
      'toutes autres manifestations auxquelles tu peux participer telles que compétitions, ' +
      'cours fédéraux ou visites amicales dans un autre club.';
    pdf.MultiCell(0,InterLigne,txt,'0','J',False,15);

    pdf.ln(10);
    txt := 'Dès lors, je te saurais gré de bien vouloir me retourner le certificat ' +
      'd''aptitude dûment complété par le médecin accompagné de ton paiement de ' +
      '31$ ou de la preuve de celui-ci par virement bancaire. Le tout dans les ' +
      'plus brefs délais afin de ne pas interrompre la couverture de ladite assurance ' +
      'et par la même occasion de t''empêcher de participer à nos cours le temps de la ' +
      'régularisation. Il y va de ta sécurité.';
    pdf.MultiCell(0,InterLigne,txt,'0','J',False,15);

    pdf.ln(10);
    txt := 'Merci de la confiance que tu mets en notre club pour ton épanouissement sportif.';
    pdf.MultiCell(0,InterLigne,txt,'0','J',False,15);

    pdf.ln(10);
    txt := 'Le comité';
    pdf.MultiCell(0,InterLigne,txt,'0','R',False);

    pdf.SaveToFile(ExtractFilePath(ParamStr(0))+PathDelim+'mc_indent.pdf');
  finally
    pdf.Free;
  end;
end.

