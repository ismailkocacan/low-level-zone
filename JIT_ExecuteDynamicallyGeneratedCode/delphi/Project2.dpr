program Project2;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  Winapi.Windows,
  System.SysUtils;

type
  TFunctionPtr = function: Integer;

const
  BYTE_CODE: Array [1 .. 9] of Byte = ($B8, $05, $00, $00, $00, $83, $C0, $04, $C3);
  CODE_SIZE = sizeof(BYTE_CODE);

var
  P: Pointer;
  Result: Integer;
  FunctionPtr: TFunctionPtr;
begin
  P := VirtualAlloc(nil, CODE_SIZE, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
  CopyMemory(P, @BYTE_CODE, CODE_SIZE);
  FunctionPtr := TFunctionPtr(P);
  Result := FunctionPtr();
  VirtualFree(P, MEM_RELEASE, CODE_SIZE);
end.
