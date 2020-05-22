unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, MultiValueEdit, Buttons, ExtCtrls;

type
  TForm9 = class(TForm)
    MultiValueEdit1: TMultiValueEdit;
    Label1: TLabel;
    Button1: TButton;
    Label2: TLabel;
    MultiValueEdit2: TMultiValueEdit;
    RadioGroup1: TRadioGroup;
    MultiValueEdit3: TMultiValueEdit;
    procedure RadioGroup1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form9: TForm9;

implementation

{$R *.dfm}

procedure TForm9.Button1Click(Sender: TObject);
begin
  ShowMessage(MultiValueEdit1.DelimitedText);
end;

procedure TForm9.FormCreate(Sender: TObject);
begin
  RadioGroup1.ItemIndex := 1;
  MultiValueEdit2.Delimiter := ';';
  MultiValueEdit2.DelimitedText := '1234;'
                                 + '1234;'
                                 + '1234;'
                                 + '1234;'
                                 + '1234;'
                                 + '1234;'
                                 + '1234;'
                                 + '1234;'
                                 + '1234;'
                                 + '1234;'
                                 + '1234;'
                                 + '1234;'
                                 + '1234;'
                                 + '1234;'
                                 + '1234;'
                                 + '1234;'
                                 + '1234;'
                                 + '1234;'
                                 + '1234;';
end;

procedure TForm9.RadioGroup1Click(Sender: TObject);
begin
  case RadioGroup1.ItemIndex of
    0: MultiValueEdit3.InputTextAligned := false;
    1: MultiValueEdit3.InputTextAligned := true;
  end;
end;

end.
