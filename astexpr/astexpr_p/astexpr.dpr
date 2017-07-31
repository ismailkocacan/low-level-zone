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
  Ast in 'Ast.pas';

var
  ANodePlus,
  ANodeMul,
  ANodeValue_1,
  ANodeValue_2,
  ANodeValue_3: TASTNode;


  A2NodePlus,
  A2NodeMul,
  A2NodeValue_1,
  A2NodeValue_2,
  A2NodeValue_3: TASTNode2;

  Result: Double;

  Start,Finish : Integer;
begin
  ReportMemoryLeaksOnShutdown := true;


  Start := GetTickCount;

  ANodePlus := TASTNode.Create;
  ANodePlus.NodeType := NtPlus;

  ANodeValue_1 := TASTNode.Create;
  ANodeValue_1.Value := 1;

  ANodeMul := TASTNode.Create;
  ANodeMul.NodeType := NtMul;

  ANodeValue_2 := TASTNode.Create;
  ANodeValue_2.Value := 2;

  ANodeValue_3 := TASTNode.Create;
  ANodeValue_3.Value := 3;

  ANodePlus.Left := ANodeValue_1;
  ANodePlus.Right := ANodeMul;

  ANodeMul.Left := ANodeValue_2;
  ANodeMul.Right := ANodeValue_3;

  Result := Evaluate(ANodePlus);
  Finish := GetTickCount;
  Writeln('Result : ' + FloatToStr(Result));
  Writeln('Time Diff Heap (ms): ' + IntToStr(Finish-Start));
  ANodePlus.Free;



  Start := GetTickCount;
  A2NodePlus.NodeType := NtPlus;
  A2NodeValue_1.Value := 1;

  A2NodeMul.NodeType := NtMul;
  A2NodeValue_2.Value := 2;
  A2NodeValue_3.Value := 3;

  A2NodeMul.Left :=  @A2NodeValue_2;
  A2NodeMul.Right := @A2NodeValue_3;

  A2NodePlus.Left := @A2NodeValue_1;
  A2NodePlus.Right := @A2NodeMul;

  Result := Evaluate(@A2NodePlus);
  Finish := GetTickCount;

  Writeln('Result : ' + FloatToStr(Result));
  Writeln('Time Diff Stack (ms): ' + IntToStr(Finish-Start));


end.
