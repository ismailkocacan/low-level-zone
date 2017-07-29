unit Ast;

interface

uses
  System.Classes,
  System.SysUtils;

type

  TASTNodeType = (NtUndefined = 0, NtPlus = 1, NtMinus = 2, NtDiv = 3, NtMul = 4);

  PASTNode = ^TASTNode;

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

function IsNodeTypeEqual(ANode: TASTNode; AType: TASTNodeType): Boolean;
function Evualate(ANode: TASTNode): double;

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

function Evualate(ANode: TASTNode): double;
begin
  if (ANode.Value <> 0) then
    Result := ANode.Value;

  if IsNodeTypeEqual(ANode, NtPlus) then
    Result := Evualate(ANode.Left) + Evualate(ANode.Right);

  if IsNodeTypeEqual(ANode, NtMinus) then
    Result := Evualate(ANode.Left) - Evualate(ANode.Right);

  if IsNodeTypeEqual(ANode, NtDiv) then
    Result := Evualate(ANode.Left) / Evualate(ANode.Right);

  if IsNodeTypeEqual(ANode, NtMul) then
    Result := Evualate(ANode.Left) * Evualate(ANode.Right);
end;

destructor TASTNode.Destroy;
begin
  inherited;
  FLeft.Free;
  FRight.Free;
end;

end.
