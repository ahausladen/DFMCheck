unit DfmCheck_OptionsFrm;

{$I DfmCheck.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls;

type
  TDfmCheck_FormOptions = class(TForm)
    pnlButtons: TPanel;
    BtnOk: TButton;
    BtnCancel: TButton;
    PageControl: TPageControl;
    TabSheetGeneral: TTabSheet;
    cbxAutoCheckBeforeCompile: TCheckBox;
    cbxAutoCheckBeforeBuildOnly: TCheckBox;
    cbxShowHintForUnnamedComponents: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure cbxAutoCheckBeforeCompileClick(Sender: TObject);
  private
    { Private-Deklarationen }
    function InternExecute: Boolean;
  public
    { Public-Deklarationen }
    class function Execute: Boolean;
  end;

{var
  DfmCheck_FormOptions: TDfmCheck_FormOptions;}

implementation

uses
  DfmCheck_Options;

{$R *.dfm}

class function TDfmCheck_FormOptions.Execute: Boolean;
begin
  with TDfmCheck_FormOptions.Create(nil) do
  try
    Result := InternExecute;
  finally
    Free;
  end;
end;

procedure TDfmCheck_FormOptions.FormCreate(Sender: TObject);
begin
  PageControl.ActivePageIndex := 0;
end;

function TDfmCheck_FormOptions.InternExecute: Boolean;
begin
  cbxAutoCheckBeforeCompile.Checked := Options.AutoCheckBeforeCompile;
  cbxAutoCheckBeforeBuildOnly.Checked := Options.AutoCheckBeforeBuildOnly;
  cbxAutoCheckBeforeCompileClick(cbxAutoCheckBeforeCompile);
  cbxShowHintForUnnamedComponents.Checked := Options.ShowHintForUnnamedComponents;

  Result := ShowModal = mrOk;
  if Result then
  begin
    Options.AutoCheckBeforeCompile := cbxAutoCheckBeforeCompile.Checked;
    Options.AutoCheckBeforeBuildOnly := cbxAutoCheckBeforeBuildOnly.Checked;
    Options.ShowHintForUnnamedComponents := cbxShowHintForUnnamedComponents.Checked;
  end;
end;

procedure TDfmCheck_FormOptions.cbxAutoCheckBeforeCompileClick(Sender: TObject);
begin
  cbxAutoCheckBeforeBuildOnly.Enabled := cbxAutoCheckBeforeCompile.Checked;
end;

end.
