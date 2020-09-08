{******************************************************************************}
{*                                                                            *}
{* DfmCheck                                                                   *}
{*                                                                            *}
{* (C) 2006 Andreas Hausladen                                                 *}
{*                                                                            *}
{******************************************************************************}

unit DfmCheck_ProgressFrm;

{$I DfmCheck.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TProgressProc = function: Boolean of object;

  TDfmCheck_FormProgress = class(TForm)
    ProgressBar: TProgressBar;
    BtnAbort: TButton;
    LblStatus: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BtnAbortClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FProc: TProgressProc;
    FResultValue: Boolean;
    FIsException: Boolean;
    FIsAborted: Boolean;
    function GetPercentage: Integer;
    function GetStatus: string;
    procedure SetPercentage(const Value: Integer);
    procedure SetStatus(const Value: string);
  public
    function Execute(AProc: TProgressProc): Boolean;

    property Percentage: Integer read GetPercentage write SetPercentage;
    property Status: string read GetStatus write SetStatus;

    property IsException: Boolean read FIsException;
    property IsAborted: Boolean read FIsAborted;
  end;

var
  DfmCheck_FormProgress: TDfmCheck_FormProgress;

implementation

{$R *.dfm}

procedure TDfmCheck_FormProgress.BtnAbortClick(Sender: TObject);
begin
  FIsAborted := True;
end;

function TDfmCheck_FormProgress.Execute(AProc: TProgressProc): Boolean;
begin
  FIsAborted := False;
  FIsException := False;
  FProc := AProc;
  Tag := 0;
  ShowModal;
  Result := FResultValue;
end;

procedure TDfmCheck_FormProgress.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := Tag <> 0;
end;

procedure TDfmCheck_FormProgress.FormCreate(Sender: TObject);
begin
  LblStatus.Caption := 'Searching for DFM files...';
  Position := poDesigned;
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2 - 80;
end;

procedure TDfmCheck_FormProgress.FormPaint(Sender: TObject);
begin
  OnPaint := nil;
  try
    FResultValue := FProc();
    Tag := 1;
    ModalResult := mrOk;
  except
    Tag := 1;
    ModalResult := mrCancel;
    FIsException := True;
    Application.HandleException(Self);
  end;
end;

function TDfmCheck_FormProgress.GetPercentage: Integer;
begin
  Result := ProgressBar.Position;
end;

function TDfmCheck_FormProgress.GetStatus: string;
begin
  Result := LblStatus.Caption;
end;

procedure TDfmCheck_FormProgress.SetPercentage(const Value: Integer);
begin
  if Value <> ProgressBar.Position then
  begin
    ProgressBar.Position := Value;
    Application.ProcessMessages;
  end;
end;

procedure TDfmCheck_FormProgress.SetStatus(const Value: string);
begin
  LblStatus.Caption := Value;
  LblStatus.Update;
end;

end.
