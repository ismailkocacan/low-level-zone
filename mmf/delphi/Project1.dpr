program Project1;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  Winapi.Windows,
  System.Classes,
  Vcl.Dialogs;

function GetFileSizeEx(hFile: THandle; var lpFileSize: Int64): BOOL; stdcall;
  external 'kernel32.dll';


procedure CreateSampleTestData(FilePath: string);
var
  I: Integer;
  Data: AnsiString;
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FilePath, fmCreate);
  try
    for I := 0 to 30000000 do
    begin
      Data := 'Test Data ' + IntToStr(I) + sLineBreak;
      Stream.WriteBuffer(PAnsiChar(Data)^, ByteLength(Data));
    end;
  finally
    Stream.Free;
  end;
  Beep(500, 1000);
end;

var
  FileSize: Int64;
  FileHandle: THandle;
  FileMapHandle: THandle;
  BaseAdress: LPSTR;

  CharCount: Integer;
  PCurrent, PLast: LPSTR;
  P, PBegin: LPSTR;

  Line: array [1 .. 1024] of AnsiChar;
  Start, Stop, Diff: DWORD;

  FilePath: string;
begin

  FilePath := ParamStr(1);
  if not FileExists(FilePath) then
    CreateSampleTestData(FilePath);

  FileHandle := CreateFile(PCHAR(FilePath), GENERIC_READ, FILE_SHARE_READ, 0,
    OPEN_EXISTING, 0, 0);

  if (FileHandle = INVALID_HANDLE_VALUE) then
    RaiseLastOSError();

  FileMapHandle := CreateFileMapping(FileHandle, nil, PAGE_READONLY, 0, 0, 0);
  if FileMapHandle = 0 then
    RaiseLastOSError;

  if not GetFileSizeEx(FileHandle, FileSize) then
    RaiseLastOSError();

  BaseAdress := MapViewOfFile(FileMapHandle, FILE_MAP_READ, 0, 0, 0);
  if not Assigned(BaseAdress) then
    RaiseLastOSError;

  PCurrent := BaseAdress;
  PLast := PCurrent + FileSize;

  PBegin := PCurrent;
  P := PCurrent;

  Start := GetTickCount();

  while (P < PLast) do
  begin
    if (P^ = #13) then
    begin
      CharCount := P - PBegin;
      if (CharCount > 0) then
      begin
        CopyMemory(@Line, PBegin, CharCount);
        PBegin := P + 1;
        // ShowMessage(Trim(Line));
      end;
    end;
    Inc(P);
  end;

  Stop := GetTickCount();
  Diff := Stop - Start;

  ShowMessage(Trim(Line));
  ShowMessage(IntToStr(Diff) + ' ms');

  UnmapViewOfFile(BaseAdress);
  CloseHandle(FileHandle);

end.
