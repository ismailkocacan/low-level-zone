program Project2;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  DynamicArray in 'DynamicArray.pas';

var
  MyArray : TDynamicArray<Integer>;
  Value : Integer;
begin
  MyArray := TDynamicArray<Integer>.Create(2);
  MyArray[0] := 34;
  MyArray[1] := 35;
  Value := MyArray[3];
  MyArray.Free;
end.
