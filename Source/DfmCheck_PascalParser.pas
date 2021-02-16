unit DfmCheck_PascalParser;

interface

{$IFDEF UNICODE}
  {$WARN WIDECHAR_REDUCED OFF}
{$ENDIF UNICODE}

uses
  SysUtils, Classes;

const
  WhiteChars = [#1..#32];
  OneSymbolChars = ['(', ')', '[', ']', ';', '@', '+', '-', '"', '/', '^', '.', ',', '*'];
  NumberChars = ['0'..'9'];
  HexNumberChars = NumberChars + ['A'..'F', 'a'..'f'];
  IdentFirstChars = ['a'..'z', 'A'..'Z', '_'];
  IdentChars = IdentFirstChars + NumberChars;
  SymbolChars = [#1..#$FF] - (WhiteChars + IdentChars + OneSymbolChars + ['{', '''']);

  MaxCachedTokens = 3; // do not change because Macros.GetReplacement() requires 3 Tokens at once

type
  TPascalParser = class;

  TTokenKind = (tkNone, tkIdent, tkSymbol, tkComment, tkString, tkNumber);
  TTokenExKind = (tekNone, tekHex, tekInt, tekFloat, tekComment, tekOption);

  PTokenInfo = ^TTokenInfo;
  TTokenInfo = record
    Kind: TTokenKind;
    ExKind: TTokenExKind;
    pFilename: PString;
    StartLine, EndLine: Integer;
    StartIndex, EndIndex: Integer;
    Value: string;
    Parser: TPascalParser;
  end;

  TPascalParser = class(TObject)
  private
    FFileName: string;
    FText: string;
    FIndex: Integer;
    FTextLen: Integer;
    FLineNum: Integer;
    FModified: Boolean;

    FTokenIndex: Integer;
    FTokens: array[0..MaxCachedTokens - 1] of PTokenInfo; // collects all parsed tokens in a ring buffer

    procedure SetText(const Value: string);
    procedure SetIndex(const Value: Integer);
    function GetCurToken: PTokenInfo;
    function GetPreToken: PTokenInfo;
  public
    constructor Create(const AFileName, AText: string; StartLineNum: Integer = 1);
    destructor Destroy; override;

    function GetToken(const ALookAhead: Boolean = False): PTokenInfo; overload;
    function GetToken(out p: PTokenInfo; const ALookAhead: Boolean = False): Boolean; overload;

    procedure Delete(StartIndex, Count: Integer);
    procedure Insert(Index: Integer; const S: string);
    procedure Replace(StartIndex, Count: Integer; const S: string); overload;
    procedure Replace(StartToken, EndToken: PTokenInfo; const S: string); overload;
    procedure ReplaceParseNext(StartIndex, Count: Integer; const S: string); overload;
    procedure ReplaceParseNext(StartToken, EndToken: PTokenInfo; const S: string); overload;
    function GetPlainText(StartIndex, EndIndex: Integer): string; overload;
    function GetPlainText(StartToken, EndToken: PTokenInfo): string; overload;

    procedure ClearCache;

    property Index: Integer read FIndex write SetIndex;
    property IndexNoClear: Integer read FIndex write FIndex;
    property Text: string read FText write SetText;
    property LineNum: Integer read FLineNum write FLineNum;
    property Filename: string read FFilename;
    property Modified: Boolean read FModified;

    property PreToken: PTokenInfo read GetPreToken;
    property CurToken: PTokenInfo read GetCurToken;
  end;

implementation

{ TPascalParser }

function TPascalParser.GetToken(const ALookAhead: Boolean = False): PTokenInfo;
var
  PText, F, P: PChar;
  IndexAdd: Integer;
  IsDecimal: Boolean;
  IsExp: Boolean;
  IsExpSign: Boolean;

  Idx: Integer;
  LineNum: Integer;
  TokenIdx: Integer;
begin
  Result := nil;
  if FIndex > FTextLen then
    Exit;

  LineNum := FLineNum;
  TokenIdx := FTokenIndex;

  PText := Pointer(FText);
  P := PText + FIndex - 1;
 // go to next token and skip white chars
  while P[0] in WhiteChars do
  begin
    if P[0] = #10 then
      Inc(LineNum);
    Inc(P);
  end;

  if P[0] = #0 then
    Exit;

  Inc(TokenIdx);
  if TokenIdx >= MaxCachedTokens then
    TokenIdx := 0; // ring buffer

  Result := FTokens[TokenIdx];
  Result.StartLine := LineNum;
  Result.StartIndex := P - PText + 1;
  Result.ExKind := tekNone;

  F := P;
  IndexAdd := 0;
  if P[0] = '''' then
  begin
    Inc(P);
    // string
    while True do
    begin
      case P[0] of
        #0:
          Break;
        '''':
          begin
            if (P[1] = '''') then
              Inc(P)
            else
              Break;
          end;
        #10, #13:
          begin
            Dec(P);
            Break; // line end is string end in pascal
          end;
      end;
      Inc(P);
    end;
    if P[0] <> #0 then
      Inc(P); // include P[0] which is now P[-1]
    Result.Kind := tkString;
  end
  else if (P[0] = '{') then
  begin
    // comment { ... } -> find comment end
    Inc(P);
    if P[0] = '$' then
    begin
      Result.ExKind := tekOption;
      Inc(P);
    end
    else
      Result.ExKind := tekComment;

    while True do
    begin
      case P[0] of
        #0, '}':
          Break;
        #10:
          Inc(LineNum);
      end;
      Inc(P);
    end;
    Result.Kind := tkComment;
    if P[0] <> #0 then
      Inc(P); // include P[0] which is now P[-1]
  end
  else if (P[0] = '(') and (P[1] = '*') then
  begin
    // comment (* ... *) -> find comment end
    Inc(P, 2);
    if P[0] = '$' then
    begin
      Result.ExKind := tekOption;
      Inc(P);
    end
    else
      Result.ExKind := tekComment;

    while (P[0] <> #0) and not ((P[0] = '*') and (P[1] = ')')) do
    begin
      if P[0] = #10 then
        Inc(LineNum);
      Inc(P);
    end;
    Result.Kind := tkComment;
    if P[0] <> #0 then
      Inc(P, 2); // include P[0],P[1] which is now P[-2],P[-1]
  end
  else if (P[0] = '/') and (P[1] = '/') then
  begin
    // comment "// ..." -> find comment end
    Inc(P, 2);
    while not (P[0] in [#0, #10, #13]) do
      Inc(P);
    Result.Kind := tkComment;
    if P[0] <> #0 then
    begin
      if P[0] = #13 then
        IndexAdd := 1; {do not parse the #13 again}
      Inc(LineNum);
      Inc(IndexAdd); {do not parse the #10 again}
    end;
  end
  else if P[0] in IdentFirstChars then
  begin
    // identifier
    Inc(P);
    while P[0] in IdentChars do
      Inc(P);
    Result.Kind := tkIdent;
  end
  else if P[0] in NumberChars then
  begin
    // number
    Inc(P);
    IsDecimal := False;
    IsExp := False;
    IsExpSign := False;
    repeat
      case P[0] of
        '0'..'9': ;

        '.':
          begin
            if P[1] = '.' then // "1..2"
              Break
            else
            if IsDecimal or IsExp then
              Break
            else
              IsDecimal := True;
          end;

        '+', '-':
          if not IsExp or IsExpSign then
            Break
          else
            IsExpSign := True;

        'e', 'E':
          if IsDecimal or IsExp then
            Break
          else
            IsExp := True;

      else
        Break;
      end;
      Inc(P);
    until False;
    Result.Kind := tkNumber;
    if IsExp or IsDecimal then
      Result.ExKind := tekFloat
    else
      Result.ExKind := tekInt;
  end
  else if (P[0] = '$') and (P[1] in HexNumberChars) then
  begin
    // hex number
    Inc(P, 2);
    while P[0] in HexNumberChars do
      Inc(P);
    Result.Kind := tkNumber;
    Result.ExKind := tekHex;
  end
  else if (P[0] = '#') and ((P[1] = '$') or (P[1] in NumberChars)) then
  begin
    // char
    Inc(P, 2);
    if P[-1] = '$' then
    begin
      while P[0] in HexNumberChars do
        Inc(P);
    end
    else
    begin
      while P[0] in NumberChars do
        Inc(P);
    end;
    Result.Kind := tkString;
  end
  else if (P[0] = '.') and (P[1] = '.') then
  begin
    Inc(P, 2);
    Result.Kind := tkSymbol;
  end
  else if P[0] in OneSymbolChars then
  begin
    Inc(P);
    Result.Kind := tkSymbol;
  end
  else
  begin
    Inc(P);
    while P[0] in SymbolChars do
      Inc(P);
    Result.Kind := tkSymbol;
  end;

  Idx := P - PText + 1;

  Result.EndLine := LineNum;
  Result.EndIndex := Idx - 1;
  SetString(Result.Value, F, P - F);

  if (not ALookAhead) then
  begin
    FIndex := Idx;
    FLineNum := LineNum;
    FTokenIndex := TokenIdx;

    Inc(FIndex, IndexAdd); // skip some chars if necessary
  end;
end;

constructor TPascalParser.Create(const AFilename, AText: string;
  StartLineNum: Integer);
var
  i: Integer;
begin
  inherited Create;
  FFilename := AFilename;

 // alloc all cacheable Tokens
  for i := 0 to MaxCachedTokens - 1 do
  begin
    New(FTokens[i]);
    FillChar(FTokens[i]^, SizeOf(TTokenInfo), 0);
    FTokens[i].pFilename := @FFilename;
    FTokens[i].Parser := Self;
  end;
  FTokenIndex := -1;

  SetText(AText);
  FLineNum := StartLineNum;
end;

destructor TPascalParser.Destroy;
var
  i: Integer;
begin
  for i := 0 to MaxCachedTokens - 1 do
    Dispose(FTokens[i]);
  inherited Destroy;
end;

procedure TPascalParser.SetText(const Value: string);
begin
  ClearCache;
  FText := Value;
  FIndex := 1;
  FLineNum := 1;
  FTextLen := Length(FText);
  FModified := False;
end;

procedure TPascalParser.Delete(StartIndex, Count: Integer);
begin
  if Count > 0 then
  begin
    System.Delete(FText, StartIndex, Count);
    FModified := True;
    FTextLen := Length(FText);
  end;
end;

procedure TPascalParser.Insert(Index: Integer; const S: string);
begin
  if S <> '' then
  begin
    System.Insert(S, FText, Index);
    FModified := True;
    FTextLen := Length(FText);
  end;
end;

procedure TPascalParser.Replace(StartIndex, Count: Integer;
  const S: string);
begin
  Delete(StartIndex, Count);
  Insert(StartIndex, S);
end;

procedure TPascalParser.ClearCache;
var
  i: Integer;
begin
  for i := 0 to MaxCachedTokens - 1 do
    FTokens[i].Kind := tkNone;
  FTokenIndex := -1;
end;

function TPascalParser.GetToken(out p: PTokenInfo; const ALookAhead: Boolean = False): Boolean;
begin
  p := GetToken(ALookAhead);
  Result := p <> nil;
end;

procedure TPascalParser.Replace(StartToken, EndToken: PTokenInfo;
  const S: string);
begin
  FIndex := StartToken.StartIndex;
  FLineNum := StartToken.StartLine;
  Replace(StartToken.StartIndex, EndToken.EndIndex - StartToken.StartIndex + 1, S);
end;

procedure TPascalParser.ReplaceParseNext(StartToken, EndToken: PTokenInfo;
  const S: string);
begin
  Replace(StartToken, EndToken, S);
  FIndex := StartToken.StartIndex + Length(S);
end;

procedure TPascalParser.ReplaceParseNext(StartIndex, Count: Integer;
  const S: string);
begin
  Replace(StartIndex, Count, S);
  FIndex := StartIndex + Length(S);
end;

function TPascalParser.GetPlainText(StartIndex, EndIndex: Integer): string;
begin
  Result := Copy(FText, StartIndex, EndIndex - StartIndex + 1);
end;

function TPascalParser.GetPlainText(StartToken, EndToken: PTokenInfo): string;
begin
  Result := GetPlainText(StartToken.StartIndex, EndToken.EndIndex);
end;

procedure TPascalParser.SetIndex(const Value: Integer);
begin
  FIndex := Value;
  ClearCache;
end;

function TPascalParser.GetCurToken: PTokenInfo;
begin
  Result := nil;
  if FTokenIndex = -1 then
    Exit;
  Result := FTokens[FTokenIndex];
  if Result.Kind = tkNone then
    Result := nil;
end;

function TPascalParser.GetPreToken: PTokenInfo;
var
  Index: Integer;
begin
  Result := nil;
  if FTokenIndex = -1 then
    Exit;
  Index := FTokenIndex - 1;
  if Index < 0 then
    Index := MaxCachedTokens - 1;
  Result := FTokens[Index];
  if Result.Kind = tkNone then
    Result := nil;
end;

end.
