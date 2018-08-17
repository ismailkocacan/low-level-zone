program Project2;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  DynamicArray in 'DynamicArray.pas';

var
  MyArray : TDynamicArray<Integer>;
begin
  MyArray := TDynamicArray<Integer>.Create(2);
  try
    { TODO -oUser -cConsole Main : Insert code here }
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  MyArray.Free;
end.
