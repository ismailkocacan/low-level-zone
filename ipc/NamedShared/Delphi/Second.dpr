program Second;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  Winapi.Windows;

const
  MAP_NAME = 'Global\MyFileMappingObject';
  BUFFER_SIZE: DWORD = 256;

var
  hFileMappingObject: THandle;

  pBuffer: LPCTSTR;
  Length: NativeUInt;
begin

  hFileMappingObject := OpenFileMapping(FILE_MAP_ALL_ACCESS, FALSE, MAP_NAME);
  if hFileMappingObject = 0 then
    RaiseLastOSError;

  pBuffer := MapViewOfFile(hFileMappingObject, FILE_MAP_ALL_ACCESS, 0, 0,
    BUFFER_SIZE);
  if not Assigned(pBuffer) then
    RaiseLastOSError;

  MessageBox(0, pBuffer, 'Proccess2', MB_OK);

  Readln;
  UnmapViewOfFile(pBuffer);
  CloseHandle(hFileMappingObject);
end.
