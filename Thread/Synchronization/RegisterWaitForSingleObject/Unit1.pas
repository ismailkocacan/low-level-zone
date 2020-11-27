unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,DateUtils;

type
  TForm1 = class(TForm)
    btnRegister: TButton;
    btnUnRegisterWait: TButton;
    Memo1: TMemo;
    cBoxFlags: TComboBox;
    procedure btnRegisterClick(Sender: TObject);
    procedure btnUnRegisterWaitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TFlag = record
     Flag: DWORD;
     Name: string;
  end;

const

  cDwFlags :  array[0..6] of TFlag = (
     (Flag: WT_EXECUTEDEFAULT; Name:'WT_EXECUTEDEFAULT'),
     (Flag: WT_EXECUTEINIOTHREAD; Name:'WT_EXECUTEINIOTHREAD'),
     (Flag: WT_EXECUTEINPERSISTENTTHREAD; Name:'WT_EXECUTEINPERSISTENTTHREAD'),
     (Flag: WT_EXECUTEINWAITTHREAD; Name:'WT_EXECUTEINWAITTHREAD'),
     (Flag: WT_EXECUTELONGFUNCTION; Name:'WT_EXECUTELONGFUNCTION'),
     (Flag: WT_EXECUTEONLYONCE; Name:'WT_EXECUTEONLYONCE'),
     (Flag: WT_TRANSFER_IMPERSONATION; Name:'WT_TRANSFER_IMPERSONATION')
  );

var
  Form1: TForm1;

  hEventObject : THandle;
  hWaitObject: THandle;
  bResult: BOOL;




implementation

{$R *.dfm}

procedure L(const AMsg:string);
begin
  Form1.Memo1.Lines.Add(AMsg + ' >>>> '+DateTimeToStr(Now()));
end;

{
 The RegisterWaitForSingleObject function differs from the other wait functions
 in that the wait operation is performed by a thread from the thread pool.

 When the specified conditions are met, the callback function is executed by a worker thread from the thread pool.
}

// TWaitOrTimerCallback = procedure (Context: Pointer; Success: Boolean) stdcall;
procedure WaitOrTimerCallback(Context: Pointer; Success: Boolean) stdcall;
var
 AEventHandle: THandle;
begin
  OutputDebugString('>>>>>>>>>>>>>>>>>>>>>>> WaitOrTimerCallback');
  L('WaitOrTimerCallback');
  if Assigned(Context) then
  begin
    AEventHandle := PHandle(Context)^;
    //ResetEvent(AEventHandle); // nosignaled
  end;
end;

procedure TForm1.btnRegisterClick(Sender: TObject);
var
 dwFlags: DWORD;
begin
  hEventObject := CreateEvent(nil,
                             false,
                             false,
                             nil);
  if hEventObject = 0 then
    RaiseLastOSError;

  dwFlags := cDwFlags[cBoxFlags.ItemIndex].Flag;
  bResult := RegisterWaitForSingleObject(hWaitObject,
                                         hEventObject,
                                         @WaitOrTimerCallback,
                                         @hEventObject,
                                         2000,
                                         dwFlags);
end;

procedure TForm1.btnUnRegisterWaitClick(Sender: TObject);
begin
  if bResult then
  begin
    bResult := UnregisterWait(hWaitObject);
    if bResult then
      CloseHandle(hEventObject);
  end;

end;

procedure TForm1.FormCreate(Sender: TObject);
var
 Flag: TFlag;
begin
  for Flag in cDwFlags do
   cBoxFlags.Items.Add(Flag.Name);
  cBoxFlags.ItemIndex := 0;
end;

end.
