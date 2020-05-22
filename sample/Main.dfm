object Form9: TForm9
  Left = 0
  Top = 0
  Caption = 'Form9'
  ClientHeight = 139
  ClientWidth = 588
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 9
    Width = 202
    Height = 13
    Caption = 'Pressione "Enter" para adicionar o valor...'
  end
  object Label2: TLabel
    Left = 8
    Top = 85
    Width = 450
    Height = 11
    Caption = 
      'Utilize os comandos "seta esquerda <-", "seta direita ->, Home e' +
      ' End" para navegar entre os componentes.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object MultiValueEdit1: TMultiValueEdit
    Left = 8
    Top = 23
    Width = 446
    Height = 22
    TabOrder = 0
  end
  object Button1: TButton
    Left = 460
    Top = 20
    Width = 75
    Height = 25
    Caption = 'Exibir'
    TabOrder = 1
    OnClick = Button1Click
  end
  object MultiValueEdit2: TMultiValueEdit
    Left = 8
    Top = 62
    Width = 446
    Height = 22
    TabOrder = 2
  end
  object RadioGroup1: TRadioGroup
    Left = 464
    Top = 100
    Width = 113
    Height = 33
    Caption = 'Entrada de texto'
    Columns = 2
    Items.Strings = (
      'Inicio'
      'Fim')
    TabOrder = 3
    OnClick = RadioGroup1Click
  end
  object MultiValueEdit3: TMultiValueEdit
    Left = 8
    Top = 111
    Width = 446
    Height = 22
    TabOrder = 4
  end
end
