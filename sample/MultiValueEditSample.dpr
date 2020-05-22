program MultiValueEditSample;

uses
  Forms,
  Main in 'Main.pas' {Form9},
  MultiValueEdit in '..\MultiValueEdit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm9, Form9);
  Application.Run;
end.
