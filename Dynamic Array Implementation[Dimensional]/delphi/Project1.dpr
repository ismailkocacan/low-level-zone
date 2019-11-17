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
  Vcl.Dialogs,
  DimensionalArray in 'DimensionalArray.pas';

var
  AValue: NativeInt;
  ColIndex,RowIndex : NativeInt;
  ATwoDimensionArray: TDimensionalArray<NativeInt>;

begin
  ATwoDimensionArray := TDimensionalArray<NativeInt>.Create(2, 2);
  try
    ATwoDimensionArray[0, 0] := 31;
    ATwoDimensionArray[1, 0] := 32;
    ATwoDimensionArray[0, 1] := 33;
    ATwoDimensionArray[1, 1] := 34;

    //test
    for ColIndex := 0 to Pred(ATwoDimensionArray.ColCount) do
      for RowIndex := 0 to Pred(ATwoDimensionArray.RowCount) do
        ShowMessage(ATwoDimensionArray[ColIndex, RowIndex].ToString());

  finally
    ATwoDimensionArray.Free;
  end;

end.
