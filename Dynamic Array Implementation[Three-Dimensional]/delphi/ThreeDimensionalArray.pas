{
  Author   : isocan
  Purpose  : How to implement Three-dimensional dynamic array using pointers.
  DateTime : 21.11.2019

  Write Great Code: Volume 1: Understanding the Machine
  Composite Data Types and Memory Objects

  Type variable[Depth_Size][Col_Size][Row_Size];
  Element_Adress = Base_Adress + ( (Depth_Index * Col_Size + Col_Index) * Row_Size + Row_Index) * Element_Size
}

unit ThreeDimensionalArray;

interface

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils;

const IndexOutOfRangeException = 'IndexOutOfRangeException at %s %d';

type

  TThreeDimensionalArray<T> = class
  type
    PT = ^T;
  strict private
    FDepthSize :NativeInt;
    FColCount: NativeInt;
    FRowCount: NativeInt;
    FMemorySize : NativeInt;
    FBaseAdress: PT;
    FPtrWrapper : TPtrWrapper;
  strict private
    function Offset(ADepthIndex,AColIndex, ARowIndex: NativeInt): NativeInt;
    function GetElement(ADepthIndex,AColIndex, ARowIndex: NativeInt): T;
    procedure SetElement(ADepthIndex,AColIndex, ARowIndex: NativeInt; const Value: T);
    function CalculateElementAdress(ADepthIndex,AColIndex, ARowIndex: NativeInt): PT;
    function CalculateMemorySize(ADepthSize,AColCount, ARowCount: NativeInt):NativeInt;
    procedure SetDepthAndColAndRowCount(ADepthSize,AColCount, ARowCount: NativeInt);
    procedure MemoryAllocate();
    procedure MemoryFree();
  public
    constructor Create(ADepthSize,AColCount, ARowCount: NativeInt);
    destructor Destroy();
  public
    procedure ReSize(ADepthSize,AColCount, ARowCount: NativeInt);
  public
    property DepthSize: NativeInt read FDepthSize;
    property ColCount: NativeInt read FColCount;
    property RowCount: NativeInt read FRowCount;
    property MemorySize: NativeInt read FMemorySize;
    property Element[ADepthIndex,AColIndex, ARowIndex: NativeInt]: T read GetElement write SetElement; default;
  end;

implementation

{ TDimensionalArray<T> }
constructor TThreeDimensionalArray<T>.Create(ADepthSize,AColCount, ARowCount: NativeInt);
begin
  SetDepthAndColAndRowCount(ADepthSize,AColCount,ARowCount);
  MemoryAllocate();
  inherited Create;
end;

destructor TThreeDimensionalArray<T>.Destroy;
begin
  TMarshal.FreeMem(FPtrWrapper);
  inherited Destroy;
end;

function TThreeDimensionalArray<T>.Offset(ADepthIndex,AColIndex, ARowIndex: NativeInt)
  : NativeInt;
begin
  if (ADepthIndex < 0) or (ADepthIndex > Pred(FDepthSize) ) then
     raise Exception.Create(Format(IndexOutOfRangeException,['ADepthIndex',ADepthIndex]));

  if (AColIndex < 0) or (AColIndex > Pred(FColCount) ) then
     raise Exception.Create(Format(IndexOutOfRangeException,['AColIndex',AColIndex]));

  if (ARowIndex < 0) or (ARowIndex > Pred(FRowCount) ) then
     raise Exception.Create(Format(IndexOutOfRangeException,['ARowIndex',ARowIndex]));

  Result := ( (ADepthIndex * FColCount + AColIndex) * FRowCount + ARowIndex) * SizeOf(T);
end;

procedure TThreeDimensionalArray<T>.ReSize(ADepthSize,AColCount, ARowCount: NativeInt);
begin
  MemoryFree();
  SetDepthAndColAndRowCount(ADepthSize,AColCount,ARowCount);
  FMemorySize := CalculateMemorySize(ADepthSize,AColCount, ARowCount);
  FPtrWrapper := TMarshal.ReallocMem(FPtrWrapper,FMemorySize);
  FBaseAdress := FPtrWrapper.ToPointer;
end;

procedure TThreeDimensionalArray<T>.SetDepthAndColAndRowCount(ADepthSize,AColCount, ARowCount: NativeInt);
begin
  FDepthSize := ADepthSize;
  FColCount := AColCount;
  FRowCount := ARowCount;
end;

procedure TThreeDimensionalArray<T>.MemoryAllocate();
begin
  FMemorySize := CalculateMemorySize(FDepthSize,FColCount, FRowCount);
  FPtrWrapper := TMarshal.AllocMem(FMemorySize);
  FBaseAdress := FPtrWrapper.ToPointer;
end;

procedure TThreeDimensionalArray<T>.MemoryFree;
begin
  TMarshal.FreeMem(FPtrWrapper);
end;

function TThreeDimensionalArray<T>.CalculateElementAdress(ADepthIndex,AColIndex,
  ARowIndex: NativeInt): PT;
begin
  Result := PT(PByte(FBaseAdress) + Offset(ADepthIndex,AColIndex, ARowIndex));
end;

function TThreeDimensionalArray<T>.CalculateMemorySize(ADepthSize,AColCount,
 ARowCount: NativeInt): NativeInt;
begin
  Result := SizeOf(T) * ADepthSize * (AColCount * ARowCount);
end;

function TThreeDimensionalArray<T>.GetElement(ADepthIndex,AColIndex, ARowIndex: NativeInt): T;
begin
  Result := CalculateElementAdress(ADepthIndex,AColIndex, ARowIndex)^;
end;

procedure TThreeDimensionalArray<T>.SetElement(ADepthIndex,AColIndex, ARowIndex: NativeInt;
  const Value: T);
begin
  CalculateElementAdress(ADepthIndex,AColIndex, ARowIndex)^ := Value;
end;

end.
