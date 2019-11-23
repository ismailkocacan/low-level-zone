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


type

  TDimensionalArray<T> = class
   const IndexOutOfRangeException = 'IndexOutOfRangeException at %d';
  type
    PT = ^T;
  strict private
    FColCount: NativeInt;
    FRowCount: NativeInt;
    FMemorySize : NativeInt;
    FBaseAdress: PT;
    FPtrWrapper : TPtrWrapper;
  strict private
    procedure RangeCheck(AColIndex, ARowIndex: NativeInt);
    function Offset(AColIndex, ARowIndex: NativeInt): NativeInt;
    function GetElement(AColIndex, ARowIndex: NativeInt): T;
    procedure SetElement(AColIndex, ARowIndex: NativeInt; const Value: T);
    function CalculateElementAdress(AColIndex, ARowIndex: NativeInt): PT;
    procedure CalculateMemorySize();
    procedure SetColAndRowCount(AColCount, ARowCount: NativeInt);
    procedure MemoryAllocate();
    procedure MemoryFree();
  public
    constructor Create(AColCount, ARowCount: NativeInt);
    destructor Destroy();
  public
    procedure ReSize(AColCount, ARowCount: NativeInt);
  public
    property ColCount: NativeInt read FColCount;
    property RowCount: NativeInt read FRowCount;
    property Element[AColIndex, ARowIndex: NativeInt]: T read GetElement write SetElement; default;
  end;

implementation

{ TDimensionalArray<T> }
constructor TDimensionalArray<T>.Create(AColCount, ARowCount: NativeInt);
begin
  SetColAndRowCount(AColCount,ARowCount);
  MemoryAllocate();
  inherited Create;
end;

destructor TDimensionalArray<T>.Destroy;
begin
  TMarshal.FreeMem(FPtrWrapper);
  inherited Destroy;
end;


procedure TDimensionalArray<T>.RangeCheck(AColIndex, ARowIndex: NativeInt);
begin
  if (AColIndex < 0) or (AColIndex > FColCount-1 ) then
     raise Exception.Create(Format(IndexOutOfRangeException,[AColIndex]));

  if (ARowIndex < 0) or (ARowIndex > FRowCount-1 ) then
     raise Exception.Create(Format(IndexOutOfRangeException,[ARowIndex]));
end;

function TDimensionalArray<T>.Offset(AColIndex, ARowIndex: NativeInt)
  : NativeInt;
begin
  RangeCheck(AColIndex,ARowIndex);
  Result := (AColIndex * FRowCount + ARowIndex) * SizeOf(T);
end;

procedure TDimensionalArray<T>.ReSize(AColCount, ARowCount: NativeInt);
begin
  MemoryFree();
  SetColAndRowCount(AColCount,ARowCount);
  CalculateMemorySize();
  FPtrWrapper := TMarshal.ReallocMem(FPtrWrapper,FMemorySize);
  FBaseAdress := FPtrWrapper.ToPointer;
end;

procedure TDimensionalArray<T>.SetColAndRowCount(AColCount, ARowCount: NativeInt);
begin
  FColCount := AColCount;
  FRowCount := ARowCount;
end;

procedure TDimensionalArray<T>.MemoryAllocate();
begin
  CalculateMemorySize();
  FPtrWrapper := TMarshal.AllocMem(FMemorySize);
  FBaseAdress := FPtrWrapper.ToPointer;
end;

procedure TDimensionalArray<T>.MemoryFree;
begin
  TMarshal.FreeMem(FPtrWrapper);
end;

function TDimensionalArray<T>.CalculateElementAdress(AColIndex,
  ARowIndex: NativeInt): PT;
begin
  Result := PT(PByte(FBaseAdress) + Offset(AColIndex, ARowIndex));
end;

procedure TDimensionalArray<T>.CalculateMemorySize();
begin
  FMemorySize := SizeOf(T) * (FColCount * FRowCount);
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
