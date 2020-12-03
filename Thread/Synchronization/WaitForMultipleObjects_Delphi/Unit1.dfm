object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Spor'
  ClientHeight = 417
  ClientWidth = 488
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnSetEvent: TButton
    Left = 232
    Top = 21
    Width = 209
    Height = 71
    Caption = 'Stop Threads'
    TabOrder = 0
    OnClick = btnSetEventClick
  end
  object btnCreateThreads: TButton
    Left = 8
    Top = 20
    Width = 201
    Height = 73
    Caption = 'Create Threads'
    TabOrder = 1
    OnClick = btnCreateThreadsClick
  end
  object mLog: TMemo
    Left = 0
    Top = 112
    Width = 488
    Height = 305
    Align = alBottom
    Color = clBtnFace
    ScrollBars = ssVertical
    TabOrder = 2
  end
end
