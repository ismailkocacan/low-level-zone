{
   1 + 2 * 3

   	'+'
   /   \
 '1'    *
      /   \
    '2'   '3'

}

program astexpr;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Winapi.Windows,
  System.Generics.Defaults,
  System.Generics.Collections,
  Ast in 'Ast.pas';

var
  ANodePlus,
  ANodeMul,
  ANodeValue_1,
  ANodeValue_2,
  ANodeValue_3: TExpressionNode;

  ANodeAssignment : TASTNode;
  Result: Double;

  Start,Finish : Integer;
var
  AstInterpreter : TASTInterpreter;
begin
  ReportMemoryLeaksOnShutdown := true;

  Start := GetTickCount;

  ANodePlus := TExpressionNode.Create;
  ANodePlus.NodeType := NtPlus;

  ANodeValue_1 := TExpressionNode.Create;
  ANodeValue_1.Value := 1;

  ANodeMul := TExpressionNode.Create;
  ANodeMul.NodeType := NtMul;

  ANodeValue_2 := TExpressionNode.Create;
  ANodeValue_2.Value := 2;

  ANodeValue_3 := TExpressionNode.Create;
  ANodeValue_3.Value := 3;

  ANodePlus.Left := ANodeValue_1;
  ANodePlus.Right := ANodeMul;

  ANodeMul.Left := ANodeValue_2;
  ANodeMul.Right := ANodeValue_3;


  ANodeAssignment := TAssignmentNode.Create;
  ANodeAssignment.Left := TVariableNode.Create('x');
  ANodeAssignment.Right := ANodePlus;


  AstInterpreter := TASTInterpreter.Create;
  Result := AstInterpreter.Interpret(ANodeAssignment);

  ANodePlus.Free;
  AstInterpreter.Free;


  Finish := GetTickCount;
  Writeln('Result : ' + FloatToStr(Result));
  Writeln('Time Diff Heap (ms): ' + IntToStr(Finish-Start));

end.
