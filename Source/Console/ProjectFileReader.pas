unit ProjectFileReader;

interface

uses
  SysUtils, Classes, Contnrs;

type
  TProjectFile = class(TObject)
  public
    FLibSuffix: string;
    FLibPrefix: string;
    FLibVersion: string;
    FFileExt: string;
    FName: string;
    function GetDestName: string;
  public
    property LibSuffix: string read FLibSuffix;
    property LibPrefix: string read FLibPrefix;
    property LibVersion: string read FLibVersion;
    property FileExt: string read FFileExt;

    property DestName: string read GetDestName;
    property Name: string read FName;
  end;

  TDpkReader = class(TProjectFile)
  private
    FIsDesignTime: Boolean;
    FIsRunTime: Boolean;
    FImplicitBuild: Boolean;
    FRequires: TStrings;
    FContains: TStrings;
    function GetContainHasForm(Index: Integer): Boolean;
    function GetIsValid: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile(const Filename: string);

    property ImplicitBuild: Boolean read FImplicitBuild;
    property IsDesignTime: Boolean read FIsDesignTime;
    property IsRunTime: Boolean read FIsRunTime;
    property Requires: TStrings read FRequires;
    property Contains: TStrings read FContains;
    property ContainHasForm[Index: Integer]: Boolean read GetContainHasForm;

    property IsValid: Boolean read GetIsValid;
  end;

  TDprReader = class(TProjectFile)
  private
    FIsLibrary: Boolean;
    FUnits: TStrings;
    function GetUnitHasForm(Index: Integer): Boolean;
    function GetIsValid: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile(const Filename: string);

    property Units: TStrings read FUnits;
    property UnitHasForm[Index: Integer]: Boolean read GetUnitHasForm;
    property IsLibrary: Boolean read FIsLibrary;
    property IsValid: Boolean read GetIsValid;
  end;

function ExtractString(const S: string): string;

implementation

uses
  StrUtils;

function ExtractString(const S: string): string;
begin
  Result := Trim(S);
  if (Result <> '') and (Result[Length(Result)] = '}') then
    Result := Trim(Copy(S, 1, Length(S) - 1));
  if (Length(Result) > 1) and (Result[1] in ['"', '''']) and (Result[Length(Result)] = Result[1]) then
    Result := Copy(Result, 2, Length(Result) - 2);
end;

{ TProjectFile }

function TProjectFile.GetDestName: string;
begin
  Result := LibPrefix + Name + LibSuffix + '.' + FileExt;
  if LibVersion <> '' then
    Result := Result + '.' + LibVersion;
end;

{ TDpkReader }

constructor TDpkReader.Create;
begin
  inherited Create;
  FRequires := TStringList.Create;
  FContains := TStringList.Create;
end;

destructor TDpkReader.Destroy;
begin
  FRequires.Free;
  FContains.Free;
  inherited Destroy;
end;

function TDpkReader.GetContainHasForm(Index: Integer): Boolean;
begin
  Result := FContains.Objects[Index] <> nil;
end;

function TDpkReader.GetIsValid: Boolean;
begin
  Result := IsDesignTime or IsRunTime;
end;

procedure TDpkReader.LoadFromFile(const Filename: string);
type
  TMode = (mDefault, mRequires, mContains);
var
  Lines: TStrings;
  S: string;
  Mode: TMode;
  i, ps: Integer;
begin
  FFileExt := 'bpl';
  FLibVersion := '';
  FLibSuffix := '';
  FLibPrefix := '';
  FRequires.Clear;
  FContains.Clear;

  if not FileExists(Filename) then
  begin
    FIsDesignTime := False;
    FIsRunTime := False;
    Exit;
  end;

  FIsDesignTime := True;
  FIsRunTime := True;

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(Filename);
    Mode := mDefault;
    for i := 0 to Lines.Count - 1 do
    begin
      S := Trim(Lines[i]);
      if S = '' then
        Continue;
      if SameText(S, '{$IMPLICITBUILD ON}') then
        FImplicitBuild := True;
      if SameText(S, '{$DESIGNONLY}') then
        FIsRunTime := False
      else if SameText(S, '{$RUNONLY}') then
        FIsDesignTime := False
      else if AnsiStartsText('{$E ', S) then
        FFileExt := ExtractString(Copy(S, 4, MaxInt))
      else if AnsiStartsText('{$EXTENSION ', S) then
        FFileExt := ExtractString(Copy(S, 11, MaxInt))
      else if AnsiStartsText('{$LIBSUFFIX ', S) then
        FLibSuffix := ExtractString(Copy(S, 12, MaxInt))
      else if AnsiStartsText('{$LIBPREFIX ', S) then
        FLibPrefix := ExtractString(Copy(S, 12, MaxInt))
      else if AnsiStartsText('{$LIBVERSION ', S) then
        FLibVersion := ExtractString(Copy(S, 13, MaxInt))
      else if AnsiStartsStr('{', S) or AnsiStartsStr('(*', S)  then
        Continue
      else if SameText(S, 'end.') then
        Mode := mDefault
      else if AnsiStartsText('package ', S) then
      begin
        FName := Trim(Copy(S, 8, MaxInt));
        if (FName <> '') and (FName[Length(FName)] = ';') then
          FName := Trim(Copy(FName, 1, Length(FName) - 1));
      end
      else if AnsiStartsText('program ', S) or AnsiStartsText('library', S) then
        Exit
      else if SameText(S, 'requires') then
        Mode := mRequires
      else if SameText(S, 'contains') then
        Mode := mContains
      else
      begin
        if AnsiEndsStr(',', S) or AnsiEndsStr(';', S) then
          S := TrimRight(Copy(S, 1, Length(S) - 1));
        if Mode = mRequires then
          FRequires.Add(S)
        else if Mode = mContains then
        begin
          ps := Pos(' in ''', AnsiLowerCase(S));
          if ps > 0 then
          begin
            S := Copy(S, ps + 5, MaxInt);
            ps := Pos('''', S);
            if ps > 0 then
            begin
              if Pos('{', S) > 0 then
                Contains.AddObject(Copy(S, 1, ps - 1), Pointer(1))
              else
                Contains.AddObject(Copy(S, 1, ps - 1), nil);
            end;
          end
          else
            Contains.AddObject(S, nil);
        end;
      end;
    end;
  finally
    Lines.Free;
  end;
end;

{ TDprReader }

constructor TDprReader.Create;
begin
  inherited Create;
  FUnits := TStringList.Create;
end;

destructor TDprReader.Destroy;
begin
  FUnits.Free;
  inherited Destroy;
end;

function TDprReader.GetUnitHasForm(Index: Integer): Boolean;
begin
  Result := FUnits.Objects[Index] <> nil;
end;

function TDprReader.GetIsValid: Boolean;
begin
  Result := Name <> '';
end;

procedure TDprReader.LoadFromFile(const Filename: string);
type
  TMode = (mDefault, mUses);
var
  Lines: TStrings;
  S: string;
  Mode: TMode;
  i, ps: Integer;
  IsUsesEnd: Boolean;
begin
  FIsLibrary := False;
  FFileExt := '';
  FLibVersion := '';
  FLibSuffix := '';
  FLibPrefix := '';
  FUnits.Clear;

  if not FileExists(Filename) then
    Exit;

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(Filename);
    Mode := mDefault;
    for i := 0 to Lines.Count - 1 do
    begin
      S := Trim(Lines[i]);
      if S = '' then
        Continue;
      if AnsiStartsText('{$E ', S) then
        FFileExt := ExtractString(Copy(S, 4, MaxInt))
      else if AnsiStartsText('{$EXTENSION ', S) then
        FFileExt := ExtractString(Copy(S, 11, MaxInt))
      else if AnsiStartsText('{$LIBSUFFIX ', S) then
        FLibSuffix := ExtractString(Copy(S, 12, MaxInt))
      else if AnsiStartsText('{$LIBPREFIX ', S) then
        FLibPrefix := ExtractString(Copy(S, 12, MaxInt))
      else if AnsiStartsText('{$LIBVERSION ', S) then
        FLibVersion := ExtractString(Copy(S, 13, MaxInt))
      else if AnsiStartsStr('{', S) or AnsiStartsStr('(*', S)  then
        Continue
      else if SameText(S, 'end.') then
        Mode := mDefault
      else if AnsiStartsText('library ', S) or AnsiStartsText('program ', S) then
      begin
        FIsLibrary := AnsiStartsText('library ', S);
        FName := Trim(Copy(S, 8, MaxInt));
        if (FName <> '') and (FName[Length(FName)] = ';') then
          FName := Trim(Copy(FName, 1, Length(FName) - 1));
      end
      else if SameText(S, 'uses') then
        Mode := mUses
      else
      begin
        IsUsesEnd := AnsiEndsStr(';', S);
        if AnsiEndsStr(',', S) or AnsiEndsStr(';', S) then
          S := TrimRight(Copy(S, 1, Length(S) - 1));
        if Mode = mUses then
        begin
          ps := Pos(' in ''', AnsiLowerCase(S));
          if ps > 0 then
          begin
            S := Copy(S, ps + 5, MaxInt);
            ps := Pos('''', S);
            if ps > 0 then
            begin
              if Pos('{', S) > 0 then
                FUnits.AddObject(Copy(S, 1, ps - 1), Pointer(1))
              else
                FUnits.AddObject(Copy(S, 1, ps - 1), nil);
            end;
          end
          else
            FUnits.AddObject(S, nil);
          if IsUsesEnd then
            Mode := mDefault;
        end;
      end;
    end;
  finally
    Lines.Free;
  end;
  if FFileExt = '' then
  begin
    if IsLibrary then
      FFileExt := 'dll'
    else
      FFileExt := 'exe';
  end;
end;

end.
 
