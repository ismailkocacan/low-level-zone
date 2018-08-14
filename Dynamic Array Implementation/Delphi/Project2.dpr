{
 Author : isocan
 Purpose : How to implement dynamic array using pointer math and more generic.


 http://docwiki.embarcadero.com/RADStudio/Tokyo/en/Memory_Management
 http://docwiki.embarcadero.com/Libraries/Tokyo/en/System.NativeInt
 http://docwiki.embarcadero.com/RADStudio/Tokyo/en/Properties_(Delphi)
 http://docwiki.embarcadero.com/RADStudio/Tokyo/en/Operator_Overloading_(Delphi)
}

program Project2;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils;

type

 TDynamicArray<T> = class
 private
  FLength : NativeInt;
  FMemBlock: ^T;
 private
  function GetData(Index:Integer):T;
  procedure SetData(Index:Integer;Value:T);
 public
   constructor Create(ALength: NativeInt);
   property Item[Index:Integer]: T read GetData write SetData;
 end;


{ TDynamicArray<T> }
constructor TDynamicArray<T>.Create(ALength: NativeInt);
begin
  // to do
end;


function TDynamicArray<T>.GetData(Index: Integer): T;
begin

end;

procedure TDynamicArray<T>.SetData(Index: Integer; Value: T);
begin

end;


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
