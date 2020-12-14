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
  Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    btnSetEvent: TButton;
    btnCreateThreads: TButton;
    mLog: TMemo;
    Timer1: TTimer;
    Label1: TLabel;
    Label2: TLabel;
    procedure btnSetEventClick(Sender: TObject);
    procedure btnCreateThreadsClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
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
  cThreadCount = 100;

var
  Form1: TForm1;

  EventObjectHandle: THandle;
    
  ActiveThreadCount : Integer = 0;
  CallbackFuncCallCount : Integer = 0;
  ThreadHandles: array[0..cThreadCount - 1] of THandle;
  WaitHandles: array[0..cThreadCount - 1] of THandle;

  
implementation

{$R *.dfm}

procedure L(const AMsg:string);
begin
  Form1.mLog.Lines.Add(AMsg);
end;


procedure WaitOrTimerCallback(Context: Pointer; Success: Boolean) stdcall;
begin

  if TThread.CurrentThread.ThreadID <> MainThreadID then
  begin
     OutputDebugString('>>>>>>>>>>>>>>>>>>>>>>> Threadin Kendisi >>>>');
  end else
  begin
     OutputDebugString('>>>>>>>>>>>>>>>>>>>>>>> Main Thread >>>>');
  end;

  InterlockedAdd(CallbackFuncCallCount, 1);

  {
    The function compares the Destination value with the Comparand value.
    If the Destination value is equal to the Comparand value,
    the Exchange value is stored in the address specified by Destination. 
    Otherwise, no operation is performed.
  }
  if InterlockedCompareExchange(CallbackFuncCallCount,
                                CallbackFuncCallCount,
                                cThreadCount) = cThreadCount then
  begin
    InterlockedExchange(CallbackFuncCallCount, 0);
    L('Tüm threadlerin iþi bitti');
  end;

  OutputDebugString('>>>>>>>>>>>>>>>>>>>>>>> WaitOrTimerCallback >>>>');
end;

procedure TForm1.btnSetEventClick(Sender: TObject);
var
  LEventObjectHandle: THandle;
begin
  LEventObjectHandle := OpenEvent(EVENT_ALL_ACCESS,true,cEventName);

  if LEventObjectHandle = 0 then
    RaiseLastOSError;

  if not SetEvent(LEventObjectHandle) then
    RaiseLastOSError;
end;


procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
var
 WaitHandle:  THandle;
begin

  if EventObjectHandle <> 0  then
    CloseHandle(EventObjectHandle);
    
  for WaitHandle in WaitHandles do
    if WaitHandle <> 0 then
      UnregisterWait(WaitHandle);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   Label1.Caption := 'Active Thread Count:' + ActiveThreadCount.ToString();
   Label2.Caption := 'Callback call Count:' + CallbackFuncCallCount.ToString();
end;

procedure TForm1.btnCreateThreadsClick(Sender: TObject);
var
  I: Integer;  
  TestThread : TTestThread;
  hWaitObject : THandle;
begin
  EventObjectHandle := CreateEvent(nil, true, false, cEventName);
  if EventObjectHandle = 0 then
   Exit;

  ResetEvent(EventObjectHandle);

  for I := Low(ThreadHandles) to High(ThreadHandles) do
  begin
    TestThread := TTestThread.Create(IntToStr(I)+ '. Thread', EventObjectHandle);
    ThreadHandles[I] := TestThread.Handle;

    if RegisterWaitForSingleObject(hWaitObject,
                                   TestThread.Handle,
                                   @WaitOrTimerCallback,
                                   @TestThread.Handle,
                                   INFINITE,
                                   WT_EXECUTEONLYONCE) then 
      WaitHandles[I] := hWaitObject;
    
    TestThread.Start;
  end;
   L('Threadlerin create edilmesi tamamlandý');

  {
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
  }

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

  InterlockedAdd(ActiveThreadCount, 1);

  WaitResult := WaitForSingleObject(FEventHandle,INFINITE);

  if WaitResult = WAIT_OBJECT_0 then
  begin
    InterlockedDecrement(ActiveThreadCount);

    OutputDebugString(PChar(FName + ' Signaled...'));
  end;

end;

end.
