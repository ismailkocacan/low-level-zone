unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

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
  AWaitObject: THandle = 0;

implementation

{$R *.dfm}

procedure L(const AMsg:string);
begin
  Form1.Memo1.Lines.Add(AMsg);
end;

procedure WaitOrTimerCallback(Context: Pointer; Success: Boolean) stdcall;
begin
  L('>>>>>>>>> WaitOrTimerCallback ');
  UnregisterWait(AWaitObject);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  AThread: TThread;
begin

  AThread := TThread.CreateAnonymousThread(
    procedure
    begin
       TThread.Queue(nil,
        procedure
        begin
          L('>>>>>>>>> TThread.begin ');
        end
       );

       Sleep(10000);

       TThread.Queue(nil,
        procedure
        begin
          L('>>>>>>>>> TThread.end ');
        end
       );

    end);
  AThread.Start;

  { Directs a wait thread in the thread pool to wait on the object }
  if not RegisterWaitForSingleObject(AWaitObject,
                                    AThread.Handle,
                                    @WaitOrTimerCallback,
                                    nil,
                                    INFINITE,
                                    WT_EXECUTEONLYONCE) then
    RaiseLastOSError;

end;

end.
