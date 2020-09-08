object DfmCheck_FormOptions: TDfmCheck_FormOptions
  Left = 391
  Top = 289
  BorderStyle = bsDialog
  Caption = 'Delphi DFM Check - Options'
  ClientHeight = 168
  ClientWidth = 325
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnlButtons: TPanel
    Left = 0
    Top = 127
    Width = 325
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object BtnOk: TButton
      Left = 163
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object BtnCancel: TButton
      Left = 244
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object PageControl: TPageControl
    Left = 8
    Top = 8
    Width = 311
    Height = 121
    ActivePage = TabSheetGeneral
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    object TabSheetGeneral: TTabSheet
      Caption = ' General '
      object cbxAutoCheckBeforeCompile: TCheckBox
        Left = 8
        Top = 8
        Width = 281
        Height = 17
        Caption = '&Run Delphi DFM Check before every compile'
        TabOrder = 0
        OnClick = cbxAutoCheckBeforeCompileClick
      end
      object cbxAutoCheckBeforeBuildOnly: TCheckBox
        Left = 24
        Top = 32
        Width = 265
        Height = 17
        Caption = 'Only when &building the project'
        TabOrder = 1
      end
      object cbxShowHintForUnnamedComponents: TCheckBox
        Left = 8
        Top = 63
        Width = 281
        Height = 17
        Caption = '&Show hint message for unnamed components'
        TabOrder = 2
        OnClick = cbxAutoCheckBeforeCompileClick
      end
    end
  end
end
