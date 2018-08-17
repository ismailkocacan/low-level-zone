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
  System.SysUtils;


type

  TDynamicArray<T> = class
  private
    FLength: NativeInt;
    FSize :NativeInt;
    FMemBlock: ^T;
    FRangeChecking : Boolean;
  private
    function GetData(Index: Integer): T;
    procedure SetData(Index: Integer; Value: T);
    function Offset(Index:Integer):Integer;
  public
    constructor Create(ALength: NativeInt);
    destructor Destroy();
  public
    property RangeChecking: Boolean read FRangeChecking write FRangeChecking;
    property MemorySize: NativeInt read FSize;
    property Length: NativeInt read FLength;
    property Item[Index: Integer]: T read GetData write SetData; default;
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

destructor TDynamicArray<T>.Destroy;
begin
   {$IFDEF Win32}
     VirtualFree(FMemBlock,FSize,MEM_RELEASE);
   {$ELSE}
    //
   {$ENDIF}
end;

function TDynamicArray<T>.GetData(Index: Integer): T;
var
 P : ^T;
begin
  P := Pointer(PByte(FMemBlock) + Offset(Index));
  Result := P^;
end;

procedure TDynamicArray<T>.SetData(Index: Integer; Value: T);
var
 P : ^T;
begin
  P := Pointer(PByte(FMemBlock) + Offset(Index));
  P^ := Value;
end;

function TDynamicArray<T>.Offset(Index: Integer): Integer;
begin
  if (FRangeChecking) then
   if (Index < 0) or (Index > FLength-1 ) then
     raise Exception.Create(Format(IndexOutOfRangeException,[Index]));
  Result := SizeOf(T) * Index;
end;


end.
