program First;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  Winapi.Windows;

const
  BUFFER_SIZE: DWORD = 256;
  MAP_NAME = 'Global\MyFileMappingObject';
  BufferMsg: PWChar = 'Message from first process.';

var
  hFileMappingObject: THandle;
  pBuffer: LPCTSTR;
  Length: NativeInt;
begin

  hFileMappingObject := CreateFileMapping(INVALID_HANDLE_VALUE, nil,
    PAGE_READWRITE, 0, BUFFER_SIZE, MAP_NAME);
  if hFileMappingObject = 0 then
    RaiseLastOSError;

  pBuffer := MapViewOfFile(hFileMappingObject, FILE_MAP_ALL_ACCESS, 0, 0,
    BUFFER_SIZE);
  if not Assigned(pBuffer) then
    RaiseLastOSError;

  Length := StrLen(BufferMsg) * SizeOf(PWChar);

  CopyMemory(pBuffer, BufferMsg, Length);
  Writeln(pBuffer);
  Readln;

  UnmapViewOfFile(pBuffer);
  CloseHandle(hFileMappingObject);

end.
