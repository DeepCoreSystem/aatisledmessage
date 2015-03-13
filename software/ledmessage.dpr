program ledmessage;

uses
  Forms,
  main in 'main.pas' {Form1},
  communication in 'communication.pas',
  language in 'language.pas',
  about in 'about.pas' {Form2};

{$R *.RES}

begin
  Application.Initialize;
  InitTranslation;
  Application.Title := 'LED Message';
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
