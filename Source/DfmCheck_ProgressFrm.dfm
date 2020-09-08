object DfmCheck_FormProgress: TDfmCheck_FormProgress
  Left = 365
  Top = 244
  ActiveControl = BtnAbort
  BorderStyle = bsDialog
  Caption = 'Delphi DFM Check'
  ClientHeight = 88
  ClientWidth = 381
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 13
  object LblStatus: TLabel
    Left = 8
    Top = 11
    Width = 44
    Height = 13
    Caption = 'LblStatus'
  end
  object ProgressBar: TProgressBar
    Left = 8
    Top = 30
    Width = 365
    Height = 17
    TabOrder = 0
  end
  object BtnAbort: TButton
    Left = 153
    Top = 53
    Width = 75
    Height = 25
    Caption = '&Abort'
    TabOrder = 1
    OnClick = BtnAbortClick
  end
end
