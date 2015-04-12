program avrsim;

uses {$ifdef UNIX}
  cthreads, {$endif UNIX}
  Classes,
  SysUtils,
  syncobjs,
  avr,
  gdbserver;

const
  VMA_FLASH:longword	 =  $00000000;
  VMA_RAM:longword		 =  $00800000;

type
  TRunnerState = (rsBreak, rsRunning);

  TAVRRunner = class;

  TBreakableAVR = class(TAVR)
  private
    FOnBreak: TBreakNotify;
    fRunner: TAVRRunner;
  protected
    procedure BreakHit; override;
  public
    constructor Create();

    property Runner: TAVRRunner read fRunner write fRunner;
    property OnBreak: TBreakNotify read FOnBreak write FOnBreak;
  end;

  TAVRRunner = class(TThread)
  private
    fAvr: TAvr;
    fState: TRunnerState;
    fLock: TCriticalSection;
    fBreakpoints: array of longword;
  protected
    procedure Execute; override;
  public
    procedure AddBreak(AAddr: longword);
    procedure RemoveBreak(AAddr: longword);

    function DoBreak: TRunnerState;
    procedure UnBreak(AOldState: TRunnerState);
    procedure SingleStep;
    procedure Continue;

    constructor Create(AAvr: TAvr);
  end;

  TDebugAVR = class(TInterfacedObject, IGDBHandler)
  private
    fAVR: TAvr;
    fRunner: TAVRRunner;

    function ReadByte(AAddr: longword): byte;
    procedure WriteByte(AAddr: longword; val: byte);
  public
    function Continue: TStopReply;
    procedure DoBreak;
    function GetRegisterString: string;
    function GetStatus: TStopReply;
    function Read(var ABuffer; AAddr, ALen: int64): boolean;
    function ReadReg(AAddr: int64; var AVal: int64): boolean;
    procedure RemoveBreakpoint(AType: TBreakpointType; AAddr: int64; AKind: longint);
    procedure Reset;
    procedure SetBreakpoint(AType: TBreakpointType; AAddr: int64; AKind: longint);
    function SingleStep: TStopReply;
    procedure StepCycles(ACycles: longint);
    function Write(const ABuffer; AAddr, ALen: int64): boolean;
    function WriteReg(AAddr, AVal: int64): boolean;
    procedure SetBreakHit(AEvent: TBreakNotify);

    constructor Create;
    destructor Destroy; override;

    property AVR: TAvr read fAVR;
  end;

var
  fserv: TGDBServerListener;
  fHandler: TDebugAVR;
  x: TAvr;
  port: String;

  procedure TBreakableAVR.BreakHit;
    begin
      inherited BreakHit;

      writeln('Hit breakpoint at ',hexstr(fRunner.fAvr.PC,4));

      fRunner.DoBreak;
      if assigned(OnBreak) then
        OnBreak;
    end;

  constructor TBreakableAVR.Create();
    begin
      inherited Create;
      FOnBreak:=nil;
      fRunner:=nil;
    end;

  procedure TAVRRunner.Execute;
    begin
      while not Terminated do
      begin
        fLock.Enter;

        if fState = rsRunning then
          fAvr.Step(1);

        fLock.Leave;
      end;
    end;

  procedure TAVRRunner.AddBreak(AAddr: longword);
    begin
    end;

  procedure TAVRRunner.RemoveBreak(AAddr: longword);
    begin
    end;

  function TAVRRunner.DoBreak: TRunnerState;
    begin
      fLock.Enter;
      Result := fState;

      if fState <> rsBreak then
        fState := rsBreak;
      fLock.Leave;
    end;

  procedure TAVRRunner.UnBreak(AOldState: TRunnerState);
    begin
      fLock.Enter;
      fState := AOldState;
      fLock.Leave;
    end;

  procedure TAVRRunner.SingleStep;
    begin
      fLock.Enter;
      DoBreak;
      fAvr.Step(1);
      fLock.Leave;
    end;

  procedure TAVRRunner.Continue;
    begin
      fLock.Enter;
      if fState <> rsRunning then
        fState := rsRunning;
      fLock.Leave;
    end;

  constructor TAVRRunner.Create(AAvr: TAvr);
    begin
      inherited Create(True);
      fState := rsBreak;
      fAvr := AAvr;

      fLock := TCriticalSection.Create;
    end;

  function TDebugAVR.ReadByte(AAddr: longword): byte;
    begin
      if AAddr >= VMA_RAM then
        result:=fAVR.RAM[AAddr-VMA_RAM]
      else if AAddr >= VMA_FLASH then
        result:=fAVR.Flash[AAddr-VMA_FLASH]
      else
        result:=0;
    end;

  procedure TDebugAVR.WriteByte(AAddr: longword; val: byte);
    begin
      if AAddr >= VMA_RAM then
        fAVR.RAM[AAddr-VMA_RAM]:=val
      else if AAddr >= VMA_FLASH then
        fAVR.Flash[AAddr-VMA_FLASH]:=val;
    end;

  function TDebugAVR.Continue: TStopReply;
    begin
      result:=srOK;
      fRunner.Continue;
    end;

  procedure TDebugAVR.DoBreak;
    begin
      fRunner.DoBreak;
    end;

  function TDebugAVR.GetRegisterString: string;
    var
      old: TRunnerState;
      i: Integer;
    begin
      old:=fRunner.DoBreak;

      result:='';
      for i := 0 to 31 do
        result:=result+hexstr(fAVR.RAM[i],2);

      result:=result+hexstr(fAVR.SREG,2); // SReg

      result:=result+hexstr(fAVR.StackPointer and $FF,2); // SP
      result:=result+hexstr((fAVR.StackPointer shr 8) and $FF,2);

      result:=result+hexstr(fAVR.PC and $FF,2); // SP
      result:=result+hexstr((fAVR.PC shr 8) and $FF,2);
      result:=result+hexstr((fAVR.PC shr 16) and $FF,2);
      result:=result+hexstr((fAVR.PC shr 24) and $FF,2);

      fRunner.UnBreak(old);
    end;

  function TDebugAVR.GetStatus: TStopReply;
    var
      old: TRunnerState;
    begin
      result:=srOK;

      old:=fRunner.DoBreak;

      if old=rsBreak then
        result:=srSigInt;

      fRunner.UnBreak(old);
    end;

  function TDebugAVR.Read(var ABuffer; AAddr, ALen: int64): boolean;
    var
      old: TRunnerState;
      i: Integer;
    begin
      old:=fRunner.DoBreak;

      result:=true;
      for i := 0 to ALen-1 do
        pbyte(@Abuffer)[i]:=ReadByte(AAddr+i);

      fRunner.UnBreak(old);
    end;

  function TDebugAVR.ReadReg(AAddr: int64; var AVal: int64): boolean;
    begin
      //AVal:=fAVR.get;
      result:=false;
    end;

  procedure TDebugAVR.RemoveBreakpoint(AType: TBreakpointType; AAddr: int64; AKind: longint);
    begin

    end;

  procedure TDebugAVR.Reset;
    begin
    end;

  procedure TDebugAVR.SetBreakpoint(AType: TBreakpointType; AAddr: int64; AKind: longint);
    begin

    end;

  function TDebugAVR.SingleStep: TStopReply;
    begin
      fRunner.SingleStep;
      Result:=srSigInt;
    end;

  procedure TDebugAVR.StepCycles(ACycles: longint);
    var
      old: TRunnerState;
    begin
      old:=fRunner.DoBreak;

      fAVR.Step(ACycles);

      fRunner.UnBreak(old);
    end;

  function TDebugAVR.Write(const ABuffer; AAddr, ALen: int64): boolean;
    var
      old: TRunnerState;
      i: Integer;
    begin
      old:=fRunner.DoBreak;

      writeln('Writing ', alen, ' bytes to ',hexstr(AAddr,4));

      result:=true;
      for i := 0 to ALen-1 do
        WriteByte(AAddr+i, pbyte(@Abuffer)[i]);

      fRunner.UnBreak(old);
    end;

  function TDebugAVR.WriteReg(AAddr, AVal: int64): boolean;
    begin
      result:=false;
    end;

  procedure TDebugAVR.SetBreakHit(AEvent: TBreakNotify);
    begin
      TBreakableAVR(fAVR).OnBreak:=AEvent;
    end;

  constructor TDebugAVR.Create;
    begin
      inherited Create;

      fAVR := TBreakableAVR.Create();

      fRunner := TAVRRunner.Create(fAVR);
      TBreakableAVR(fAVR).Runner:=fRunner;
      fRunner.DoBreak;
      fRunner.Start;
    end;

  destructor TDebugAVR.Destroy;
    begin
      fRunner.DoBreak;
      fRunner.Terminate;
      fRunner.WaitFor;
      fRunner.Free;

      fAVR.Free;

      inherited Destroy;
    end;

begin
  if (not (Paramcount in [1,2])) or
     ((Paramcount=2) and (pos('-d',ParamStr(1))<>1)) then
    begin
      writeln('Simulator: Invalid command line');
      writeln('Usage: avrsim [-d<port>] <bin-file>');
      halt(-100001);
    end;

  if not FileExists(ParamStr(Paramcount)) then
    begin
      writeln('Simulator: File not found');
      halt(-100000);
    end;

  try
    if (Paramcount=2) then
      begin
        fHandler:=TDebugAVR.Create;
        try
          fHandler.AVR.LoadFlashBinary(ParamStr(2));

          port:=ParamStr(1);
          delete(port,1,2);

          fserv := TGDBServerListener.Create(strtoint(port), fHandler);
          try
            fserv.Start;

            fserv.Waitfor;

            ExitCode := fHandler.AVR.ExitCode;
          finally
            fserv.Free;
          end;
        finally;
          fHandler.Free;
        end;
      end
    else
      begin
        x := TAvr.Create;
        try
           x.LoadFlashBinary(ParamStr(1));

           while not x.DoExit do
              x.Step(10);

           ExitCode := x.ExitCode;
        finally
           x.Free;
        end;
      end;
  except
    on e: Exception do
      begin
        writeln('Simulator: Exception happened!');
        writeln(e.Message);
        halt(-100002);
      end;
  end;
end.
