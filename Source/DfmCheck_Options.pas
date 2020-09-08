{******************************************************************************}
{*                                                                            *}
{* DfmCheck                                                                   *}
{*                                                                            *}
{* (C) 2006 Andreas Hausladen                                                 *}
{*                                                                            *}
{******************************************************************************}

unit DfmCheck_Options;

{$I DfmCheck.inc}

interface

uses
  Windows, Registry, SysUtils, Classes, Contnrs;

type
  { TOptions manages the configuration of the plugin. }

  TChangedMethod = procedure of object;

  TOptionKind = (
    okSeparator, okText, okDirectory, okFilename, okBoolean, okDropDownList,
    okMakeTool
  );

  TOptions = class(TObject)
  private
    FModified: Boolean;
    FAutoCheckBeforeCompile: Boolean;
    FAutoCheckBeforeBuildOnly: Boolean;
    FShowHintForUnnamedComponents: Boolean;
    procedure SetAutoCheckBeforeCompile(const Value: Boolean);
    procedure SetAutoCheckBeforeBuildOnly(const Value: Boolean);
    procedure SetShowHintForUnnamedComponents(const Value: Boolean);
  protected
    procedure Changed; virtual;
    function ReadBounds(Ini: TRegIniFile; const Section: string): TRect;
    procedure WriteBounds(Ini: TRegIniFile; const Section: string; const Value: TRect);
  public
    procedure SaveToReg(const RootKey: string);
    procedure LoadFromReg(const RootKey: string);

    procedure ResetFormPositions;

    property AutoCheckBeforeCompile: Boolean read FAutoCheckBeforeCompile write SetAutoCheckBeforeCompile;
    property AutoCheckBeforeBuildOnly: Boolean read FAutoCheckBeforeBuildOnly write SetAutoCheckBeforeBuildOnly;
    property ShowHintForUnnamedComponents: Boolean read FShowHintForUnnamedComponents write SetShowHintForUnnamedComponents;

    property Modified: Boolean read FModified;
  end;

var
  Options: TOptions = nil;

implementation

uses
  DfmCheck_AppConsts;

{$IFDEF COMPILER5}
const
  sLineBreak = #13#10;
{$ENDIF COMPILER5}

{ TOptions }

procedure TOptions.Changed;
begin
  FModified := True;
end;

procedure TOptions.LoadFromReg(const RootKey: string);
var
  Ini: TRegIniFile;
begin
  Ini := TRegIniFile.Create;
  try
    Ini.RootKey := HKEY_CURRENT_USER;
    if Ini.OpenKey(RootKey + '\DfmCheck', False) then
      ;

    begin
      FAutoCheckBeforeCompile := Ini.ReadBool('Options', 'AutoCheckBeforeCompile', False);
      FAutoCheckBeforeBuildOnly := Ini.ReadBool('Options', 'AutoCheckBeforeBuildOnly', True);
      FShowHintForUnnamedComponents := Ini.ReadBool('Options', 'ShowHintForUnnamedComponents', False);
    end;
  finally
    Ini.Free;
  end;
  FModified := False;
end;

procedure TOptions.SaveToReg(const RootKey: string);
var
  Ini: TRegIniFile;
begin
  Ini := TRegIniFile.Create;
  try
    Ini.RootKey := HKEY_CURRENT_USER;
    if Ini.OpenKey(RootKey + '\DfmCheck', True) then
    begin
      Ini.WriteBool('Options', 'AutoCheckBeforeCompile', FAutoCheckBeforeCompile);
      Ini.WriteBool('Options', 'AutoCheckBeforeBuildOnly', FAutoCheckBeforeBuildOnly);
      Ini.WriteBool('Options', 'ShowHintForUnnamedComponents', FShowHintForUnnamedComponents);
    end;
  finally
    Ini.Free;
  end;
  FModified := False;
end;

procedure TOptions.ResetFormPositions;
begin
end;

function TOptions.ReadBounds(Ini: TRegIniFile;
  const Section: string): TRect;
begin
  Result.Left := Ini.ReadInteger(Section, 'Left', -1);
  Result.Top := Ini.ReadInteger(Section, 'Top', -1);
  Result.Right := Result.Left + Ini.ReadInteger(Section, 'Width', 0);
  Result.Bottom := Result.Top + Ini.ReadInteger(Section, 'Height', 0);
end;

procedure TOptions.WriteBounds(Ini: TRegIniFile; const Section: string;
  const Value: TRect);
begin
  Ini.WriteInteger(Section, 'Left', Value.Left);
  Ini.WriteInteger(Section, 'Top', Value.Top);
  Ini.WriteInteger(Section, 'Width', Value.Right - Value.Left);
  Ini.WriteInteger(Section, 'Height', Value.Bottom - Value.Top);
end;

procedure TOptions.SetAutoCheckBeforeCompile(const Value: Boolean);
begin
  if Value <> FAutoCheckBeforeCompile then
  begin
    FAutoCheckBeforeCompile := Value;
    Changed;
  end;
end;

procedure TOptions.SetShowHintForUnnamedComponents(const Value: Boolean);
begin
  if Value <> FShowHintForUnnamedComponents then
  begin
    FShowHintForUnnamedComponents := Value;
    Changed;
  end;
end;

procedure TOptions.SetAutoCheckBeforeBuildOnly(const Value: Boolean);
begin
  if Value <> FAutoCheckBeforeBuildOnly then
  begin
    FAutoCheckBeforeBuildOnly := Value;
    Changed;
  end;
end;

end.
