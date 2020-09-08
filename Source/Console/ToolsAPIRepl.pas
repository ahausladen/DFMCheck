unit ToolsAPIRepl;

interface

uses
  SysUtils, Classes, Contnrs, ActiveX, StrUtils;

type
  IOTAEditor = interface
    ['{747A692F-53C9-483B-A8F6-1287081F354F}']
    function GetFileName: string;
    procedure MarkModified;
    function GetModified: Boolean;

    property Modified: Boolean read GetModified;
    property FileName: string read GetFileName;
  end;

  IOTAFormEditor = interface(IOTAEditor)
    ['{16CBA3C4-6988-4683-B139-32B311DD4BEB}']
    procedure GetFormResource(const Stream: IStream);
  end;

  IOTAEditBuffer = interface(IOTAEditor)
    ['{43388E99-74D7-49A8-9364-62E87AF74224}']
    procedure SetContent(const Value: AnsiString);
    function GetContent: AnsiString;

    property Content: AnsiString read GetContent write SetContent;
  end;

  IOTAModule = interface
    ['{7405AF3D-C3E8-4D2D-A42B-D67CFE3333A3}']
    function GetName: string;
    function GetFileName: string;
    function GetFormName: string;
    function GetModuleCount: Integer;
    function GetModule(Index: Integer): IOTAModule;
    function GetModuleFileCount: Integer;
    function GetModuleFileEditor(Index: Integer): IOTAEditor;

    property Name: string read GetName;
    property FormName: string read GetFormName;
    property FileName: string read GetFileName;
    property ModuleCount: Integer read GetModuleCount;
    property Modules[Index: Integer]: IOTAModule read GetModule;
    property ModuleFileCount: Integer read GetModuleFileCount;
    property ModuleFileEditors[Index: Integer]: IOTAEditor read GetModuleFileEditor;
  end;

  IOTAProject = interface(IOTAModule)
    ['{0B704E1F-0625-442B-AB5C-002984018621}']
  end;

function LoadProject(const FileName: string): IOTAProject;

implementation

uses
  ProjectFileReader;

type
  TIOTAEditor = class(TInterfacedObject, IOTAEditor)
  private
    FFileName: string;
    FModified: Boolean;
    function GetModified: Boolean;
  protected
    function GetFileName: string;
  public
    constructor Create(const AFileName: string);
    procedure MarkModified;
    property Modified: Boolean read GetModified;
    property FileName: string read GetFileName;
  end;

  TIOTAModule = class(TInterfacedObject, IOTAModule)
  private
    FName: string;
    FFileName: string;
    FFormName: string;
    FModules: TInterfaceList;
    FFileEditors: TInterfaceList;
  protected
    function GetName: string;
    function GetFileName: string;
    function GetFormName: string;
    function GetModuleCount: Integer;
    function GetModule(Index: Integer): IOTAModule;
    function GetModuleFileCount: Integer;
    function GetModuleFileEditor(Index: Integer): IOTAEditor;
  public
    constructor Create(const AName, AFileName, AFormName: string);
    destructor Destroy; override;

    property Name: string read GetName;
    property FormName: string read GetFormName;
    property FileName: string read GetFileName;
    property ModuleCount: Integer read GetModuleCount;
    property Modules[Index: Integer]: IOTAModule read GetModule;
    property ModuleFileCount: Integer read GetModuleFileCount;
    property ModuleFileEditors[Index: Integer]: IOTAEditor read GetModuleFileEditor;
  end;

  TIOTAProject = class(TIOTAModule, IOTAProject)
  protected
    procedure LoadProject;
  public
    constructor Create(AFileName: string);
  end;

function LoadProject(const FileName: string): IOTAProject;
begin
  Result := TIOTAProject.Create(FileName);
end;

{ TIOTAModule }

constructor TIOTAModule.Create(const AName, AFileName, AFormName: string);
begin
  inherited Create;
  FFileEditors := TInterfaceList.Create;
  FModules := TInterfaceList.Create;
  FName := AName;
  FFileName := AFileName;
  FFormName := AFormName;
end;

destructor TIOTAModule.Destroy;
begin
  FModules.Free;
  FFileEditors.Free;
  inherited Destroy;
end;

function TIOTAModule.GetModuleFileCount: Integer;
begin
  Result := FFileEditors.Count;
end;

function TIOTAModule.GetName: string;
begin
  Result := FName;
end;

function TIOTAModule.GetFileName: string;
begin
  Result := FFileName;
end;

function TIOTAModule.GetModuleCount: Integer;
begin
  Result := FModules.Count;
end;

function TIOTAModule.GetModule(Index: Integer): IOTAModule;
begin
  Result := IOTAModule(FModules[Index]);
end;

function TIOTAModule.GetModuleFileEditor(Index: Integer): IOTAEditor;
begin
  Result := IOTAEditor(FFileEditors[Index]);
end;

function TIOTAModule.GetFormName: string;
begin
  Result := FFormName;
end;

{ TIOTAEditor }

constructor TIOTAEditor.Create(const AFileName: string);
begin
  inherited Create;
  FFileName := AFileName;
end;

function TIOTAEditor.GetFileName: string;
begin
  Result := FFileName;
end;

function TIOTAEditor.GetModified: Boolean;
begin
  Result := FModified;
end;

procedure TIOTAEditor.MarkModified;
begin
  FModified := True;
end;

{ TIOTAProject }

constructor TIOTAProject.Create(AFileName: string);
begin
  AFileName := ExpandFileName(AFileName);
  inherited Create(ExtractFileName(AFileName), AFileName, '');
  LoadProject;
end;

procedure TIOTAProject.LoadProject;
var
  Ext: string;
  Dpr: TDprReader;
  Dpk: TDpkReader;
  i: Integer;
  CurDir: string;
  ModuleFileName: string;
begin
  FModules.Clear;
  FFileEditors.Clear;

  CurDir := GetCurrentDir;
  try
    SetCurrentDir(ExtractFileDir(FileName));

    Ext := ExtractFileExt(FileName);
    if SameText(Ext, '.dpr') then
    begin
      Dpr := TDprReader.Create;
      try
        Dpr.LoadFromFile(FileName);
        FName := Dpr.Name;
        //FFileEditors.Add(TIOTAEditBuffer.Create(FileName) as IOTAEditBuffer);

        for i := 0 to Dpr.Units.Count - 1 do
        begin
          ModuleFileName := Dpr.Units[i];
          if ExtractFileExt(ModuleFileName) = '' then
            FModules.Add(TIOTAModule.Create(ModuleFileName, '', '') as IOTAModule)
          else
            FModules.Add(TIOTAModule.Create(ExtractFileName(ModuleFileName), ExpandFileName(ModuleFileName), '') as IOTAModule);
        end;
      finally
        Dpr.Free;
      end;
    end
    else if SameText(Ext, '.dpk') then
    begin
      Dpk := TDpkReader.Create;
      Dpk.LoadFromFile(FileName);
      try
        FName := Dpk.Name;
        //FFileEditors.Add(TIOTAEditBuffer.Create(FileName) as IOTAEditBuffer);

        for i := 0 to Dpk.Contains.Count - 1 do
        begin
          ModuleFileName := Dpk.Contains[i];
          if ExtractFileExt(ModuleFileName) = '' then
            FModules.Add(TIOTAModule.Create(ModuleFileName, '', '') as IOTAModule)
          else
            FModules.Add(TIOTAModule.Create(ExtractFileName(ModuleFileName), ExpandFileName(ModuleFileName), '') as IOTAModule);
        end;

      finally
        Dpk.Free;
      end;
    end
    else
      raise Exception.CreateFmt('Unknown project file format: %s', [Ext]);
  finally
    SetCurrentDir(CurDir)
  end;
end;

end.

