unit Ast;

interface

uses
  System.Classes,
  System.SysUtils;

type

  TASTNodeType = (NtUndefined = 0, NtPlus = 1, NtMinus = 2, NtDiv = 3, NtMul = 4);

  PASTNode = ^TASTNode;

  // Heap way
  TASTNode = class
  private
    FValue: double;
    FNodeType: TASTNodeType;
    FLeft: TASTNode;
    FRight: TASTNode;
  published
    property Value: double read FValue write FValue;
    property NodeType: TASTNodeType read FNodeType write FNodeType;
    property Left: TASTNode read FLeft write FLeft;
    property Right: TASTNode read FRight write FRight;
  public
    constructor Create(); overload;
    destructor Destroy; override;
  end;

  PASTNode2 = ^TASTNode2;

  // Stack way
  TASTNode2 = record
  private
    FValue: double;
    FNodeType: TASTNodeType;
    FLeft: PASTNode2;
    FRight: PASTNode2;
  public
    property Value: double read FValue write FValue;
    property NodeType: TASTNodeType read FNodeType write FNodeType;
    property Left: PASTNode2 read FLeft write FLeft;
    property Right: PASTNode2 read FRight write FRight;
  end;

function IsNodeTypeEqual(ANode: TASTNode; AType: TASTNodeType): Boolean; overload;
function IsNodeTypeEqual(ANode: PASTNode2; AType: TASTNodeType): Boolean; overload;
function Evaluate(ANode: TASTNode): double; overload;
function Evaluate(ANode: PASTNode2): double; overload;

implementation

{ TASTNode }
constructor TASTNode.Create();
begin
  inherited;
  FValue := 0;
  FLeft := nil;
  FRight := nil;
  FNodeType := NtUndefined;
end;

function IsNodeTypeEqual(ANode: TASTNode; AType: TASTNodeType): Boolean;
begin
  Result := ANode.NodeType = AType;
end;

function IsNodeTypeEqual(ANode: PASTNode2; AType: TASTNodeType): Boolean; overload;
begin
   Result := ANode^.NodeType = AType;
end;

function Evaluate(ANode: TASTNode): double;
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

function Evaluate(ANode: PASTNode2): double;
begin
  if (ANode^.Value <> 0) then
    Result := ANode^.Value;

  if IsNodeTypeEqual(ANode, NtPlus) then
    Result := Evaluate(ANode.Left) + Evaluate(ANode.Right);

  if IsNodeTypeEqual(ANode, NtMinus) then
    Result := Evaluate(ANode.Left) - Evaluate(ANode.Right);

  if IsNodeTypeEqual(ANode, NtDiv) then
    Result := Evaluate(ANode.Left) / Evaluate(ANode.Right);

  if IsNodeTypeEqual(ANode, NtMul) then
    Result := Evaluate(ANode.Left) * Evaluate(ANode.Right);
end;


destructor TASTNode.Destroy;
begin
  inherited;
  FLeft.Free;
  FRight.Free;
end;



end.
