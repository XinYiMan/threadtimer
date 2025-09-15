unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Spin, StdCtrls,
  ThreadTimer, DateUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Memo1: TMemo;
    SpinEdit1: TSpinEdit;
    ThreadTimer1: TThreadTimer;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ThreadTimer1AfterTimer(Sender: TObject; Data: TObject);
    procedure ThreadTimer1Timer(Sender: TObject; out Data: TObject);
  private
    FMyNow: TDateTime;
    FStarted : boolean;
    FShouldInc: Boolean; // variabile di appoggio per passare il risultato
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
     FStarted := false;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
     if (not FStarted) then
     begin
       FMyNow := Now;
       Self.Label1.Caption := DateTimeToStr(FMyNow);

       // Configuro il timer
       ThreadTimer1.Interval := 10000;  // 10 secondi
       ThreadTimer1.Enabled := True;
       FStarted := true;
       Self.Button1.Caption := 'Stop';
     end else begin
       ThreadTimer1.Interval := 0;
       ThreadTimer1.Enabled := False;
       FStarted := false;
       Self.Label1.Caption := '-';
       Self.Button1.Caption := 'Start';
     end;

end;

procedure TForm1.ThreadTimer1AfterTimer(Sender: TObject; Data: TObject);
begin
  try
    SpinEdit1.Value := SpinEdit1.Value + 1;
    Self.Label1.Caption := TStringList(Data).Text;
  finally
    Data.Free; // ricordati di liberare sempre l’oggetto passato
  end;
end;

procedure TForm1.ThreadTimer1Timer(Sender: TObject; out Data: TObject);
var
  SL: TStringList;
begin
  if Abs(MinutesBetween(Now , FMyNow)) >= 1 then
  begin
    FMyNow := Now;
    SL := TStringList.Create;
    SL.Text := DateTimeToStr(FMyNow);
    Data := SL;
    ThreadTimer1.TriggerAfter := True; // segnala che va chiamato OnAfterTimer
  end;
end;

end.

