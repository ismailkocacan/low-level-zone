unit Ast;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Defaults,
  System.Generics.Collections;

type

  TSymbolTable = class(TDictionary<string, Variant>)
  end;

  TASTNodeType = (Assignment = 0, Variable = 1, Expression = 2);

  TASTNode = class
  private
    FLeft: TASTNode;
    FRight: TASTNode;
    FNodeType: TASTNodeType;
  public
    property Left: TASTNode read FLeft write FLeft;
    property Right: TASTNode read FRight write FRight;
    property NodeType: TASTNodeType read FNodeType write FNodeType;
    constructor Create(ANodeType: TASTNodeType); overload;
    destructor Destroy; override;
  end;

  TExpressionNodeType = (NtUndefined = 0, NtPlus = 1, NtMinus = 2, NtDiv = 3,
    NtMul = 4);

  TExpressionNode = class(TASTNode)
  private
    FValue: double;
    FNodeType: TExpressionNodeType;
    FLeft: TExpressionNode;
    FRight: TExpressionNode;
  public
    property Value: double read FValue write FValue;
    property NodeType: TExpressionNodeType read FNodeType write FNodeType;
    property Left: TExpressionNode read FLeft write FLeft;
    property Right: TExpressionNode read FRight write FRight;
  public
    constructor Create(); overload;
    destructor Destroy; override;
  end;

  TAssignmentNode = class(TASTNode)
  public
    constructor Create();
  end;

  TVariableNode = class(TASTNode)
  private
    FName: string;
  public
    property Name: string read FName write FName;
  public
    constructor Create(AName: string); overload;
  end;

  TASTInterpreter = class
  private
    FSymbolTable: TSymbolTable;
    function IsNodeTypeEqual(ANode: TExpressionNode;
      AType: TExpressionNodeType): Boolean;
    function Evaluate(ANode: TExpressionNode): double;
  public
    function Interpret(ANode: TASTNode): double;
  public
    constructor Create;
  end;

implementation

{ TASTNode }
constructor TASTNode.Create(ANodeType: TASTNodeType);
begin
  inherited Create;
  Left := nil;
  Right := nil;
end;

destructor TASTNode.Destroy;
begin
  inherited;
  if FLeft <> nil then
    FLeft.Free;
  if FRight <> nil then
    FRight.Free;
end;

{ TExpressionNode }
constructor TExpressionNode.Create();
begin
  inherited Create(TASTNodeType.Expression);
  FValue := 0;
  FLeft := nil;
  FRight := nil;
  FNodeType := NtUndefined;
end;

destructor TExpressionNode.Destroy;
begin
  inherited;
  FLeft.Free;
  FRight.Free;
end;

{ TAssignmentNode }
constructor TAssignmentNode.Create;
begin
  inherited Create(TASTNodeType.Assignment);
end;

{ TVariableNode }
constructor TVariableNode.Create(AName: string);
begin
  inherited Create(TASTNodeType.Variable);
  FName := AName;
end;

{ TASTInterpreter }
constructor TASTInterpreter.Create;
begin
  FSymbolTable := TSymbolTable.Create();
end;

{ TASTInterpreter }
function TASTInterpreter.IsNodeTypeEqual(ANode: TExpressionNode;
  AType: TExpressionNodeType): Boolean;
begin
  Result := ANode.NodeType = AType;
end;

{ TASTInterpreter }
function TASTInterpreter.Evaluate(ANode: TExpressionNode): double;
begin
  if (ANode.Value <> 0) then
    Result := ANode.Value;

  if IsNodeTypeEqual(ANode, NtPlus) then
    Result := Evaluate(ANode.Left) + Evaluate(ANode.Right);

  if IsNodeTypeEqual(ANode, NtMinus) then
    Result := Evaluate(ANode.Left) - Evaluate(ANode.Right);

  if IsNodeTypeEqual(ANode, NtDiv) then
    Result := Evaluate(ANode.Left) / Evaluate(ANode.Right);

  if IsNodeTypeEqual(ANode, NtMul) then
    Result := Evaluate(ANode.Left) * Evaluate(ANode.Right);
end;

{ TASTInterpreter }
function TASTInterpreter.Interpret(ANode: TASTNode): double;
var
  VariableName: string;
begin
  if (ANode = nil) then
    Result := 0.0;

  if ANode.NodeType = TASTNodeType.Assignment then
  begin
    VariableName := (ANode.Left as TVariableNode).Name;
    if not FSymbolTable.ContainsKey(VariableName) then
      FSymbolTable.Add(VariableName, 0);

    FSymbolTable[VariableName] := Evaluate(ANode.FRight as TExpressionNode);
    Result := FSymbolTable[VariableName];
  end;

  if ANode.NodeType = TASTNodeType.Expression then
  begin
    Result := Evaluate(ANode as TExpressionNode);
  end;

end;

end.
