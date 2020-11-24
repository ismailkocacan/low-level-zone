object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Events'
  ClientHeight = 604
  ClientWidth = 449
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 32
    Top = 48
    Width = 169
    Height = 137
    Caption = 'Test WAIT_OBJECT_0'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 224
    Top = 48
    Width = 161
    Height = 137
    Caption = 'Test WAIT_TIMEOUT'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 32
    Top = 216
    Width = 169
    Height = 137
    Caption = 'Create Named Event'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 224
    Top = 216
    Width = 161
    Height = 137
    Caption = 'Reset Event'
    TabOrder = 3
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 32
    Top = 383
    Width = 169
    Height = 130
    Caption = 'Create Named Event'
    TabOrder = 4
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 224
    Top = 383
    Width = 161
    Height = 130
    Caption = 'Set Event'
    TabOrder = 5
    OnClick = Button6Click
  end
end
