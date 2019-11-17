{
  Author   : isocan
  Purpose  : How to implement dimensional dynamic array using pointers.
  DateTime : 17.11.2019

  Write Great Code: Volume 1: Understanding the Machine
  Composite Data Types and Memory Objects
  Type variable[Col_Size][Row_Size]
  Element_Adress = Base_Adress + (Col_Index * Row_Size + Row_Index) * Element_Size
}

unit DimensionalArray;

interface

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils;

const IndexOutOfRangeException = 'IndexOutOfRangeException at %d';

type

  TDimensionalArray<T> = class
  type
    PT = ^T;
  strict private
    FColCount: NativeInt;
    FRowCount: NativeInt;
    FBaseAdress: PT;
  strict private
    function Offset(AColIndex, ARowIndex: NativeInt): NativeInt;
    function GetElement(AColIndex, ARowIndex: NativeInt): T;
    procedure SetElement(AColIndex, ARowIndex: NativeInt; const Value: T);
    function CalculateElementAdress(AColIndex, ARowIndex: NativeInt): PT;
  public
    constructor Create(AColCount, ARowCount: NativeInt); overload;
    destructor Destroy();
  public
    property ColCount: NativeInt read FColCount;
    property RowCount: NativeInt read FRowCount;
    property Element[AColIndex, ARowIndex: NativeInt]: T read GetElement write SetElement; default;
  end;

implementation

{ TDimensionalArray<T> }
constructor TDimensionalArray<T>.Create(AColCount, ARowCount: NativeInt);
begin
  FColCount := AColCount;
  FRowCount := ARowCount;
  GetMem(FBaseAdress,  SizeOf(T) * (FColCount * FRowCount));
end;

destructor TDimensionalArray<T>.Destroy;
begin
  FreeMem(FBaseAdress);
end;

function TDimensionalArray<T>.Offset(AColIndex, ARowIndex: NativeInt)
  : NativeInt;
begin
  if (AColIndex < 0) or (AColIndex > FColCount-1 ) then
     raise Exception.Create(Format(IndexOutOfRangeException,[AColIndex]));

  if (ARowIndex < 0) or (ARowIndex > FRowCount-1 ) then
     raise Exception.Create(Format(IndexOutOfRangeException,[ARowIndex]));

  Result := (AColIndex * FRowCount + ARowIndex) * SizeOf(T);
end;

function TDimensionalArray<T>.CalculateElementAdress(AColIndex,
  ARowIndex: NativeInt): PT;
begin
  Result := PT(PByte(FBaseAdress) + Offset(AColIndex, ARowIndex));
end;

function TDimensionalArray<T>.GetElement(AColIndex, ARowIndex: NativeInt): T;
begin
  Result := CalculateElementAdress(AColIndex, ARowIndex)^;
end;

procedure TDimensionalArray<T>.SetElement(AColIndex, ARowIndex: NativeInt;
  const Value: T);
begin
  CalculateElementAdress(AColIndex, ARowIndex)^ := Value;
end;

end.
