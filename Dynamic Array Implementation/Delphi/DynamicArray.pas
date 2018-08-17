{
  Author : isocan
  Purpose : How to implement dynamic array using pointer math and more generic.


  http://docwiki.embarcadero.com/RADStudio/Tokyo/en/Memory_Management
  http://docwiki.embarcadero.com/Libraries/Tokyo/en/System.NativeInt
  http://docwiki.embarcadero.com/RADStudio/Tokyo/en/Properties_(Delphi)
  http://docwiki.embarcadero.com/RADStudio/Tokyo/en/Operator_Overloading_(Delphi)
}

unit DynamicArray;

interface

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils;


type

  TDynamicArray<T> = class
  type
    PT = ^T;
  private
    FLength: NativeInt;
    FSize :NativeInt;
    FMemBlock: PT;
    FRangeChecking : Boolean;
  private
    function GetElement(Index: Integer): T;
    procedure SetElement(Index: Integer; Value: T);
    function Offset(Index:Integer):Integer;
  public
    constructor Create(ALength: NativeInt); overload;
    constructor Create(const AFilePath:string); overload;
    destructor Destroy();
  public
    procedure Serialize(const AFilePath:string);
    function  GetElementPointer(Index:Integer):PT;
  public
    property RangeChecking: Boolean read FRangeChecking write FRangeChecking;
    property MemorySize: NativeInt read FSize;
    property Length: NativeInt read FLength;
    property Element[Index: Integer]: T read GetElement write SetElement; default;
  end;

const IndexOutOfRangeException = 'IndexOutOfRangeException at %d';

implementation

{ TDynamicArray<T> }
constructor TDynamicArray<T>.Create(ALength: NativeInt);
begin
   FRangeChecking := true;
   FLength := ALength;
   FSize := sizeof(T) * FLength;
   {$IFDEF Win32}
     FMemBlock := VirtualAlloc(nil,FSize,MEM_COMMIT,PAGE_READWRITE);
   {$ELSE}
    //
   {$ENDIF}
end;

constructor TDynamicArray<T>.Create(const AFilePath: string);
var
  AFileStream : TFileStream;
  TypeSize : Integer;
  FileSize : Integer;
begin
  AFileStream := TFileStream.Create(AFilePath,fmOpenRead);
  try
    AFileStream.Read(TypeSize,SizeOf(Integer));
    // to do
  finally
   AFileStream.Free;
  end;
end;

destructor TDynamicArray<T>.Destroy;
begin
   {$IFDEF Win32}
     VirtualFree(FMemBlock,FSize,MEM_RELEASE);
   {$ELSE}
    //
   {$ENDIF}
end;

function TDynamicArray<T>.GetElement(Index: Integer): T;
var
 P : PT;
begin
  P := PT(PByte(FMemBlock) + Offset(Index));
  Result := P^;
end;

procedure TDynamicArray<T>.SetElement(Index: Integer; Value: T);
var
 P : PT;
begin
  P := PT(PByte(FMemBlock) + Offset(Index));
  P^ := Value;
end;

function TDynamicArray<T>.Offset(Index: Integer): Integer;
begin
  if (FRangeChecking) then
   if (Index < 0) or (Index > FLength-1 ) then
     raise Exception.Create(Format(IndexOutOfRangeException,[Index]));
  Result := SizeOf(T) * Index;
end;

procedure TDynamicArray<T>.Serialize(const AFilePath: string);
var
  AFileStream : TFileStream;
  TypeSize : Integer;
begin
 AFileStream := TFileStream.Create(AFilePath,fmCreate);
 try
   TypeSize := SizeOf(T);
   AFileStream.Write(TypeSize,SizeOf(Integer)); //  first 4 byte is size of data type.
   AFileStream.WriteBuffer(FMemBlock,FSize);
 finally
  AFileStream.Free;
 end;
end;

function TDynamicArray<T>.GetElementPointer(Index: Integer): PT;
var
 P : PT;
begin
  P := PT(PByte(FMemBlock) + Offset(Index));
  Result := P;
end;



end.
