unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  ASystemTime : SYSTEMTIME;
  ALocalTime : FILETIME;
  AUtcTime : FILETIME;

  lpDueTime: Int64;
  WaitResult : Cardinal;
  TimerHandle: THandle;
begin

  TimerHandle := CreateWaitableTimer(nil, true, nil);
  if TimerHandle <> 0 then
  begin
      ASystemTime.wYear := 2020;
      ASystemTime.wMonth := 12;
      ASystemTime.wDay := 6;
      ASystemTime.wHour := 3;
      ASystemTime.wMinute := 23;
      ASystemTime.wSecond := 0;
      ASystemTime.wMilliseconds := 0;

      SystemTimeToFileTime(ASystemTime, ALocalTime);
      LocalFileTimeToFileTime(ALocalTime,AUtcTime);

      LARGE_INTEGER(lpDueTime).LowPart := AUtcTime.dwLowDateTime;
      LARGE_INTEGER(lpDueTime).HighPart := AUtcTime.dwHighDateTime;

      SetWaitableTimer(TimerHandle, lpDueTime, 0, nil, nil, false);

      TThread.CreateAnonymousThread(
       procedure
       begin

          TThread.Queue(nil,
           procedure
           begin
             Memo1.Lines.Add('>>>>>>>>> waiting');
           end
          );

          WaitResult := WaitForSingleObject(TimerHandle, INFINITE);

          TThread.Queue(nil,
           procedure
           begin
             Memo1.Lines.Add('>>>>>>>>> worked at '+ FormatDateTime('dd.MM.yyyy hh:mm:ss',Now()));
           end
          );

          CloseHandle(TimerHandle);

       end
      ).Start;
  end;


end;

end.
