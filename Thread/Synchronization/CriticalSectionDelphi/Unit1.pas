unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  Total: Integer = 0;
  lpCriticalSection: TRTLCriticalSection;

implementation

{$R *.dfm}

function ThreadFunc1(lpThreadParameter: PVOID): DWORD; stdcall;
var
  I: Integer;
begin
  EnterCriticalSection(lpCriticalSection);
  for I := 1 to 10 do
    Total := + I;
  LeaveCriticalSection(lpCriticalSection);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  H: THandle;
  lpThreadId: DWORD;
begin
  {
    function CreateThread(lpThreadAttributes: Pointer;
    dwStackSize: SIZE_T; lpStartAddress: TFNThreadStartRoutine;
    lpParameter: Pointer; dwCreationFlags: DWORD; var lpThreadId: DWORD): THandle; stdcall;
  }
  InitializeCriticalSection(lpCriticalSection);

  H := CreateThread(nil, 0, @ThreadFunc1, nil, 0, lpThreadId);
  WaitForSingleObject(H, INFINITE);
  CloseHandle(H);
  DeleteCriticalSection(lpCriticalSection);

  ShowMessage(IntToStr(Total));
end;

end.
