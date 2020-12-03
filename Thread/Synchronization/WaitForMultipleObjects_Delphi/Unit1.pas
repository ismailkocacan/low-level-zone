unit Unit1;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    btnSetEvent: TButton;
    btnCreateThreads: TButton;
    mLog: TMemo;
    procedure btnSetEventClick(Sender: TObject);
    procedure btnCreateThreadsClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TTestThread = class(TThread)
  private
    FName: string;
    FEventHandle: THandle;
  protected
    procedure Execute; override;
  public
    constructor Create(const AName: string;const AEventHandle:THandle);
  end;


const
  cEventName = 'KernelFlag';
  cThreadCount = 64;

var
  Form1: TForm1;
  ThreadHandles: array[0..cThreadCount - 1] of THandle;

implementation

{$R *.dfm}

procedure L(const AMsg:string);
begin
  Form1.mLog.Lines.Add(AMsg);
end;


procedure TForm1.btnSetEventClick(Sender: TObject);
var
  EventObjectHandle: THandle;
begin
  EventObjectHandle := OpenEvent(EVENT_ALL_ACCESS,true,cEventName);

  if EventObjectHandle = 0 then
    RaiseLastOSError;

  if not SetEvent(EventObjectHandle) then
    RaiseLastOSError;
end;


procedure TForm1.btnCreateThreadsClick(Sender: TObject);
var
  I: Integer;
  EventObjectHandle: THandle;
  WaitResult : Cardinal;
  TestThread : TTestThread;
begin
  EventObjectHandle := CreateEvent(nil, true, false, cEventName);
  if EventObjectHandle = 0 then
   Exit;

  ResetEvent(EventObjectHandle);

  for I := Low(ThreadHandles) to High(ThreadHandles) do
  begin
    TestThread := TTestThread.Create(IntToStr(I)+ '. Thread', EventObjectHandle);
    ThreadHandles[I] := TestThread.Handle;
    TestThread.Start;
  end;

 
  TThread.CreateAnonymousThread(
   procedure
   begin
        WaitForMultipleObjects(cThreadCount,
                               @ThreadHandles,
                               True,
                               INFINITE);
        CloseHandle(EventObjectHandle);
        TThread.Queue(nil,
         procedure
         begin
           L('Wait finished');
         end
        );
   end
  ).Start;


end;

{ TTestThread }
constructor TTestThread.Create(const AName: string;const AEventHandle:THandle);
begin
  inherited Create(true);
  FName := AName;
  FreeOnTerminate := True;
  FEventHandle := AEventHandle;
end;

procedure TTestThread.Execute;
var
 WaitResult : Cardinal;
begin
  inherited;
  OutputDebugString(PChar(FName + ' Beklemede.'));
  WaitResult := WaitForSingleObject(FEventHandle,INFINITE);
  if WaitResult = WAIT_OBJECT_0 then
    OutputDebugString(PChar(FName + ' Signaled...'));
end;

end.
