object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'ThreadPool'
  ClientHeight = 314
  ClientWidth = 416
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnRegister: TButton
    Left = 24
    Top = 48
    Width = 177
    Height = 65
    Caption = 'RegisterWaitForSingleObject'
    TabOrder = 0
    OnClick = btnRegisterClick
  end
  object btnUnRegisterWait: TButton
    Left = 207
    Top = 48
    Width = 177
    Height = 65
    Caption = 'UnregisterWait'
    TabOrder = 1
    OnClick = btnUnRegisterWaitClick
  end
  object Memo1: TMemo
    Left = 0
    Top = 130
    Width = 416
    Height = 184
    Align = alBottom
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object cBoxFlags: TComboBox
    Left = 24
    Top = 21
    Width = 177
    Height = 21
    Style = csDropDownList
    TabOrder = 3
  end
end
