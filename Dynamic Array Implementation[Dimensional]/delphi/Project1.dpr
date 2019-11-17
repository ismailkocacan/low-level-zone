{
  Author   : isocan
  Purpose  : How to implement dimensional dynamic array using pointers.
  DateTime : 17.11.2019

  Write Great Code: Volume 1: Understanding the Machine
  Composite Data Types and Memory Objects
  Type variable[Col_Size][Row_Size]
  Element_Adress = Base_Adress + (Col_Index * Row_Size + Row_Index) * Element_Size
}

program Project1;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  DimensionalArray in 'DimensionalArray.pas';

var
  AValue: NativeInt;
  ATwoDimensionArray: TDimensionalArray<NativeInt>;

begin
  ATwoDimensionArray := TDimensionalArray<NativeInt>.Create(2, 2);
  try
    ATwoDimensionArray[0, 0] := 31;
    AValue := ATwoDimensionArray[0, 0];
  finally
    ATwoDimensionArray.Free;
  end;

end.
