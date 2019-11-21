{
  Author   : isocan
  Purpose  : How to implement Three-dimensional dynamic array using pointers.
  DateTime : 21.11.2019

  Write Great Code: Volume 1: Understanding the Machine
  Composite Data Types and Memory Objects

  Type variable[Depth_Size][Col_size][Row_size];
  Element_Adress = Base_Adress + ( (Depth_Index * Col_Size + Col_Index) * Row_Size + Row_Index) * Element_Size
}

program Project1;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  Vcl.Dialogs,
  ThreeDimensionalArray in 'ThreeDimensionalArray.pas';

var
  AValue: NativeInt;
  DepthIndex,ColIndex,RowIndex : NativeInt;
  AnArray: TThreeDimensionalArray<NativeInt>;
begin
  // // 2 adet, 2 sütun, 2 satırlık tablo
  AnArray := TThreeDimensionalArray<NativeInt>.Create(2,2,2);
  //AnArray.MemorySize;
  try

    for DepthIndex := 0 to Pred(AnArray.DepthSize) do
       for ColIndex := 0 to Pred(AnArray.ColCount) do
           for RowIndex := 0 to Pred(AnArray.RowCount) do
              AnArray[DepthIndex,ColIndex, RowIndex] := DepthIndex + ColIndex + RowIndex;


    for DepthIndex := 0 to Pred(AnArray.DepthSize) do
       for ColIndex := 0 to Pred(AnArray.ColCount) do
           for RowIndex := 0 to Pred(AnArray.RowCount) do
              ShowMessage(
                Format('[DepthIndex,ColIndex,RowIndex] [%d,%d,%d] -> %d',
                 [DepthIndex,ColIndex,RowIndex, AnArray[DepthIndex,ColIndex, RowIndex]])
               );

  finally
    AnArray.Free;
  end;

end.
