unit ThreadTimer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TThreadTimer = class;

  // evento chiamato nel thread secondario
  TThreadTimerEvent = procedure(Sender: TObject; out Data: TObject) of object;
  // evento chiamato nel main thread
  TThreadTimerBeforeEvent = procedure(Sender: TObject) of object;
  // evento chiamato nel main thread
  TThreadTimerAfterEvent = procedure(Sender: TObject; Data: TObject) of object;
  // evento chiamato nel main thread
  TThreadTimerErrorEvent = procedure(Sender: TObject; Data: TObject) of object;

  TThreadTimerThread = class(TThread)
  private
    FOwner: TThreadTimer;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TThreadTimer);
  end;

  TThreadTimer = class(TComponent)
  private
    FInterval: Cardinal;
    FEnabled: Boolean;
    FOnTimer: TThreadTimerEvent;
    FOnError: TThreadTimerErrorEvent;
    FOnAfterTimer: TThreadTimerAfterEvent;
    FOnBeforeTimer: TThreadTimerBeforeEvent;
    FThread: TThreadTimerThread;
    FData: TObject;         // buffer tra OnTimer e OnAfterTimer
    FTriggerBefore: Boolean; // flag per decidere se chiamare BeforeTimer    
    FTriggerAfter: Boolean; // flag per decidere se chiamare AfterTimer
    FTriggerError: Boolean; // flag per decidere se chiamare Error
    FState: Variant; // variabile interna più flessibile
    procedure SetEnabled(AValue: Boolean);
    procedure SetInterval(AValue: Cardinal);
  protected
    procedure DoTimer;        // chiamato nel thread secondario
    procedure DoBeforeTimer;   // sincronizzato col main thread
    procedure DoAfterTimer;   // sincronizzato col main thread
    procedure DoError;        // sincronizza con main thread
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // proprietà per permettere all’utente di segnalare se BeforeTimer va invocato
    property TriggerBefore: Boolean read FTriggerBefore write FTriggerAfter;
    // proprietà per permettere all’utente di segnalare se AfterTimer va invocato
    property TriggerAfter: Boolean read FTriggerAfter write FTriggerAfter;
    // proprietà per permettere all’utente di segnalare se ErrorTimer va invocato
    property TriggerError: Boolean read FTriggerError write FTriggerError;
    property State: Variant read FState write FState; // proprietà pubblica
  published
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    property OnTimer: TThreadTimerEvent read FOnTimer write FOnTimer;
    property OnBeforeTimer: TThreadTimerBeforeEvent read FOnBeforeTimer write FOnBeforeTimer;
    property OnAfterTimer: TThreadTimerAfterEvent read FOnAfterTimer write FOnAfterTimer;
    property OnError: TThreadTimerErrorEvent read FOnError write FOnError;
  end;

procedure Register;

implementation

{ TThreadTimerThread }

constructor TThreadTimerThread.Create(AOwner: TThreadTimer);
begin
  inherited Create(True); // Crea sospeso
  FreeOnTerminate := False;
  FOwner := AOwner;
end;

procedure TThreadTimerThread.Execute;
begin
  while not Terminated do
  begin
    Sleep(FOwner.FInterval);
    if Terminated then Exit;
    if FOwner.FEnabled and Assigned(FOwner.FOnTimer) then
    begin
      if FOwner.FTriggerBefore and Assigned(FOwner.FOnBeforeTimer) then
        Synchronize(@FOwner.DoBeforeTimer); // chiama nel main thread solo se richiesto
      FOwner.DoTimer; // esegue logica nel thread secondario
      if FOwner.FTriggerAfter and Assigned(FOwner.FOnAfterTimer) then
        Synchronize(@FOwner.DoAfterTimer); // chiama nel main thread solo se richiesto
      if FOwner.FTriggerError and Assigned(FOwner.FOnError) then
        Synchronize(@FOwner.DoError); // chiama nel main thread solo se richiesto
    end;
  end;
end;

{ TThreadTimer }

constructor TThreadTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInterval := 1000;
  FEnabled := False;
  FData := nil;
  FTriggerAfter := False;
  FThread := TThreadTimerThread.Create(Self);
  FThread.Start;
end;

destructor TThreadTimer.Destroy;
begin
  if Assigned(FThread) then
  begin
    FThread.Terminate;
    FThread.WaitFor;
    FreeAndNil(FThread);
  end;
  inherited Destroy;
end;

procedure TThreadTimer.DoTimer;
begin
  // eseguito nel thread secondario
  FData := nil;
  FTriggerAfter := False;
  if Assigned(FOnTimer) then
    FOnTimer(Self, FData); // l’utente decide se assegnare Data e settare TriggerAfter
end;

procedure TThreadTimer.DoBeforeTimer;
begin
  // eseguito nel main thread
  if Assigned(FOnBeforeTimer) then
    FOnBeforeTimer(Self);
  FTriggerBefore := False;
end;


procedure TThreadTimer.DoAfterTimer;
begin
  // eseguito nel main thread
  if Assigned(FOnAfterTimer) then
    FOnAfterTimer(Self, FData);

  // Nota: NON libero FData qui.
  // Deve farlo l’utente dentro OnAfterTimer.
  FData := nil;
  FTriggerAfter := False;
end;

procedure TThreadTimer.DoError;
begin
  // eseguito nel main thread
  if Assigned(FOnError) then
    FOnError(Self, FData);

  // Nota: NON libero FData qui.
  // Deve farlo l’utente dentro OnError.
  FData := nil;
  FTriggerError := False;
end;

procedure TThreadTimer.SetEnabled(AValue: Boolean);
begin
  if FEnabled = AValue then Exit;
  FEnabled := AValue;
end;

procedure TThreadTimer.SetInterval(AValue: Cardinal);
begin
  if AValue < 1 then AValue := 1;
  FInterval := AValue;
end;

procedure Register;
begin
  RegisterComponents('Threads', [TThreadTimer]);
end;

end.

