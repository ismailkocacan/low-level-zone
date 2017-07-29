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
  Ast in 'Ast.pas';

var
  ANodePlus,
  ANodeMul,
  ANodeValue_1,
  ANodeValue_2,
  ANodeValue_3: TASTNode;

  Result: Double;

begin
  ReportMemoryLeaksOnShutdown := true;

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

  Result := Evualate(ANodePlus);

  Writeln('Sonuç : ' + FloatToStr(Result));

  ANodePlus.Free;

end.
