{
   Pretty much a direct port of avrsim by Michel Pollet
   
   -- Original header
   sim_core.c

	Copyright 2008, 2009 Michel Pollet <buserror@gmail.com>

 	This file is part of simavr.

	simavr is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	simavr is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with simavr.  If not, see <http://www.gnu.org/licenses/>.
   --/ Original header
   
   Ported to Object Pascal by Jeppe Johansen - jeppe@j-software.dk
}
unit avr;

{$mode objfpc}{$H+}

interface

uses sysutils;

type
   TFlashAddr = word;

   TAvrState = (cpu_Sleeping, cpu_StepDone);

   TSregField = (S_C, S_Z, S_N, S_V, S_S, S_H, S_T, S_I);

   { TAvr }

   TAvr = class
   private
    fData: array of byte;
    fFLASH: array of byte;
    fPC: TFlashAddr;
    fSREG: array[TSregField] of boolean;
    fState: TAvrState;

    fRAMPZ, fEIND: word;

    fExitRequested: boolean;
    fExitCode: byte;

    function GetData(AIndex: longint): byte;
    function GetFlash(AIndex: longint): byte;
    function GetSREG: byte;
    procedure SetData(AIndex: longint; AValue: byte);
    procedure SetFlash(AIndex: longint; AValue: byte);

    function Is32bitInstr(apc: TFlashAddr): boolean;
    procedure SetSREG(AValue: byte);

    procedure WriteSREG(r: byte);
    procedure ReadSREG(var r: byte);

    procedure avr_core_watch_write(r: word; v: byte);
    function avr_core_watch_read(r: word): byte;

    function GetIO(r: word; var v: byte): boolean;
    function SetIO(r, v: byte): boolean;
    procedure SetReg(r, v: byte);

    function GetRAM(addr: word): byte;
    procedure SetRAM(addr: word; v: byte);
    procedure SetSP(sp: word);
    function GetSP: word;

    procedure Push8(v: byte);
    function Pop8: byte;
    procedure Push16(v: word);
    function Pop16(): word;

    procedure get_r_d_10(o: word; out r, d, vd, vr: byte);
    procedure get_r_dd_10(o: word; out r, d, vr: byte);
    procedure get_k_r16(o: word; out r, k: byte);

    function InterruptPending: boolean;

    function RunOne: TFlashAddr;
   protected
    procedure InvalidOpcode; virtual;
    procedure WatchdogReset; virtual;
    procedure BreakHit; virtual;
    procedure SPM; virtual;
   public
    procedure Step(Count: longint);

    procedure WriteFlash(const Data; Count, Offset: longint);
    procedure LoadFlashBinary(const AFilename: string);

    constructor Create(AFlashSize: longint = 1024*1024; ARamSize: longint = 64*1024);

    property RAM[AIndex: longint]: byte read GetData write SetData;
    property Flash[AIndex: longint]: byte read GetFLASH write SetFLASH;

    property SREG: byte read GetSREG write SetSREG;
    property StackPointer: word read GetSP write SetSP;

    property PC: TFlashAddr read fPC write fPC;

    property DoExit: boolean read fExitRequested;
    property ExitCode: byte read fExitCode;
   end;

const
   R_XL = $1A;
   R_XH = $1B;
   R_YL = $1C;
   R_YH = $1D;
   R_ZL = $1E;
   R_ZH = $1F;

   R_SPL  = 32+$3D;
   R_SPH  = 32+$3E;
   R_SREG = 32+$3F;

implementation

function TAvr.Is32bitInstr(apc: TFlashAddr): boolean;
var
   o: word;
begin
   o := (fFLASH[apc] or (fFLASH[apc + 1] shl 8)) and $fc0f;
   Result := (o = $9200) or // STS ! Store Direct to fData Space
      (o = $9000) or // LDS Load Direct from fData Space
      (o = $940c) or // JMP Long Jump
      (o = $940d) or // JMP Long Jump
      (o = $940e) or  // CALL Long Call to sub
      (o = $940f); // CALL Long Call to sub
end;

procedure TAvr.SetSREG(AValue: byte);
begin
   WriteSREG(AValue);
end;

function TAvr.GetFlash(AIndex: longint): byte;
begin
   result := fFLASH[AIndex];
end;

function TAvr.GetSREG: byte;
begin
   result:=0;
   ReadSREG(result);
end;

procedure TAvr.SetFlash(AIndex: longint; AValue: byte);
begin
   fFLASH[AIndex] := AValue;
end;

function TAvr.GetData(AIndex: longint): byte;
begin
   Result := fData[AIndex];
end;

procedure TAvr.SetData(AIndex: longint; AValue: byte);
begin
   fData[AIndex] := AValue;
end;

procedure TAvr.WriteSREG(r: byte);
var
   i: TSregField;
begin
   for i := low(TSregField) to high(TSregField) do
      fSREG[i] := odd(r shr Ord(i));
end;

procedure TAvr.ReadSREG(var r: byte);
var
   i: TSregField;
begin
   r := 0;
   for i := low(i) to high(i) do
      if fSREG[i] then
         r := r or (1 shl Ord(i));
end;

procedure TAvr.avr_core_watch_write(r: word; v: byte);
begin
   fData[r] := v;
end;

function TAvr.avr_core_watch_read(r: word): byte;
begin
   result := fData[r];
end;

function TAvr.GetIO(r: word; var v: byte): boolean;
begin
   result := false;
end;

function TAvr.SetIO(r, v: byte): boolean;
begin
   result := true;
   case r of
      32: write(char(v));
      33: fExitCode := v;
      34: fExitRequested := ((v and 1) = 1);
   else
      result := false;
   end;
end;

procedure TAvr.SetReg(r, v: byte);
begin
   //REG_TOUCH(r);
   if (r = R_SREG) then
   begin
      fData[R_SREG] := v;
      // unsplit the fSREG
      WriteSREG(v);
   end;
   if (r > 31) then
   begin
      if not SetIO(r,v) then
         fData[r] := v;
      // TODO
      {if (fIO[io].w.c) then
         fIO[io].w.c(r, v, fIO[io].w.param);
      else
         fData[r] := v;
      if (fIO[io].irq) then begin
         avr_raise_irq(fIO[io].irq + AVR_IOMEM_IRQ_ALL, v);
         for (int i := 0; i < 8; i++)
            avr_raise_irq(fIO[io].irq + i, (v shr i) and 1);
      end;}
   end
   else
      fData[r] := v;
end;

{
 * Stack pointer access
 }
function TAvr.GetSP: word;
begin
   Result := fData[R_SPL] or (fData[R_SPH] shl 8);
end;

procedure TAvr.SetSP(sp: word);
begin
   SetReg(R_SPL, sp);
   SetReg(R_SPH, sp shr 8);
end;

{
 * Set any address to a value; split between registers and SRAM
 }
procedure TAvr.SetRAM(addr: word; v: byte);
begin
   if (addr < 256) then
      SetReg(addr, v)
   else
      avr_core_watch_write(addr, v);
end;

{
 * Get a value from SRAM.
 }
function TAvr.GetRAM(addr: word): byte;
var
   r: byte;
begin
   if (addr = R_SREG) then
   begin
      {
       * fSREG is special it's reconstructed when read
       * while the core itself uses the 'shortcut' array
       }
      ReadSREG(fData[R_SREG]);
   end
   else if ((addr > 31) and (addr < 256)) then
   begin
      r:=0;
      if GetIO(addr, r) then
         fData[Addr] := r;

      // TODO
      {if (fIO[io].r.c) then
         fData[addr] := fIO[io].r.c(addr, fIO[io].r.param);

      if (fIO[io].irq) then begin
         uint8_t v := fData[addr];
         avr_raise_irq(fIO[io].irq + AVR_IOMEM_IRQ_ALL, v);
         for (int i := 0; i < 8; i++)
            avr_raise_irq(fIO[io].irq + i, (v shr i) and 1);
      end;}
   end;
   Result := avr_core_watch_read(addr);
end;

{
 * Stack push accessors. Push/pop 8 and 16 bits
 }
procedure TAvr.Push8(v: byte);
var
   sp: word;
begin
   sp := GetSP();
   SetRAM(sp, v);
   SetSP(sp - 1);
end;

function TAvr.Pop8: byte;
var
   sp: word;
   res: byte;
begin
   sp := GetSP() + 1;
   res := GetRAM(sp);
   SetSP(sp);
   Result := res;
end;

procedure TAvr.Push16(v: word);
begin
   Push8(v);
   Push8(v shr 8);
end;

function TAvr.Pop16(): word;
var
   res: word;
begin
   res := Pop8() shl 8;
   res := res or Pop8();
   Result := res;
end;

{
 * Called when an invalid opcode is decoded
 }
procedure TAvr.InvalidOpcode;
begin
{#if CONFIG_SIMAVR_TRACE then
   printf( FONT_RED '*** %04x: %-25s Invalid Opcode SP:=%04x O:=%04x \n' FONT_DEFAULT,
         fPC, trace_data^.codeline[pcshr1]^.symbol, GetSP(), fFLASH[fPC] or (fFLASH[fPC+1]shl8));
#else
   AVR_LOG(LOG_ERROR, FONT_RED 'CORE: *** %04x: Invalid Opcode SP:=%04x O:=%04x \n' FONT_DEFAULT,
         fPC, GetSP(), fFLASH[fPC] or (fFLASH[fPC+1]shl8));
#endif then}
end;

procedure TAvr.get_r_d_10(o: word; out r, d, vd, vr: byte);
begin
   r := ((o shr 5) and $10) or (o and $f);
   d := (o shr 4) and $1f;
   vd := fData[d];
   vr := fData[r];
end;

procedure TAvr.get_r_dd_10(o: word; out r, d, vr: byte);
begin
   r := ((o shr 5) and $10) or (o and $f);
   d := (o shr 4) and $1f;
   vr := fData[r];
end;

procedure TAvr.get_k_r16(o: word; out r, k: byte);
begin
   r := 16 + ((o shr 4) and $f);
   k := byte((o and $0f00) shr 4) or (o and $f);
end;

function TAvr.InterruptPending: boolean;
begin
   result := false;
end;

{***************************************************************************\
 *
 * Helper functions for calculating the status register bit values.
 * See the Atmel data sheet for the instruction set for more info.
 *
\***************************************************************************}

function get_add_carry(res, rd, rr: byte; b: integer): boolean;
var
   resb, rdb, rrb: boolean;
begin
   resb := odd(res shr b);
   rdb := odd(rd shr b);
   rrb := odd(rr shr b);
   Result := (rdb and rrb) or (rrb and (not resb)) or ((not resb) and rdb);
end;

function get_add_overflow(res, rd, rr: byte): boolean;
var
   res7, rd7, rr7: boolean;
begin
   res7 := odd(res shr 7);
   rd7 := odd(rd shr 7);
   rr7 := odd(rr shr 7);
   Result := (rd7 and rr7 and (not res7)) or ((not rd7) and (not rr7) and res7);
end;

function get_sub_carry(res, rd, rr: byte; b: integer): boolean;
var
   resb, rdb, rrb: boolean;
begin
   resb := odd(res shr b);
   rdb := odd(rd shr b);
   rrb := odd(rr shr b);
   Result := ((not rdb) and rrb) or (rrb and resb) or (resb and (not rdb));
end;

function get_sub_overflow(res, rd, rr: byte): boolean;
var
   res7, rd7, rr7: boolean;
begin
   res7 := odd(res shr 7);
   rd7 := odd(rd shr 7);
   rr7 := odd(rr shr 7);
   Result := (rd7 and (not rr7) and (not res7)) or ((not rd7) and rr7 and res7);
end;

function get_compare_carry(res, rd, rr: byte; b: integer): boolean;
var
   resb, rdb, rrb: boolean;
begin
   resb := odd(res shr b);
   rdb := odd(rd shr b);
   rrb := odd(rr shr b);
   Result := ((not rdb) and rrb) or (rrb and resb) or (resb and (not rdb));
end;

function get_compare_overflow(res, rd, rr: byte): boolean;
var resb,rdb,rrb: boolean;
begin
   resb := odd(res shr 7);
   rdb := odd(rd shr 7);
   rrb := odd(rr shr 7);
    { The atmel data sheet says the second term is not rd7 for CP
     * but that doesn't make any sense. You be the judge. }
   Result := (rdb and (not rrb) and (not resb)) or ((not rdb) and rrb and resb);
end;

function TAvr.RunOne: TFlashAddr;
var
   opcode: longword;
   new_pc: TFlashAddr;
   cycle: longint;
   b, r, d, vd, vr, q, rd, rdl, rdh, io, s: byte;
   res: byte;
   res16: smallint;
   c: integer;
   k: byte;
   x, y, v: word;
   z, res32: longword;
   op, e, p: longint;
   _set: boolean;
   a: TFlashAddr;
   o: smallint;
   branch: boolean;
begin
   opcode := (fFLASH[fPC + 1] shl 8) or fFLASH[fPC];
   new_pc := fPC + 2; // future 'default' fPC
   cycle := 1;
   case (opcode and $f000) of
      $0000:
      begin
         case (opcode) of
            $0000:
            begin // NOP
               //fState('nop\n');
            end
            else
            begin
               case (opcode and $fc00) of
                  $0400:
                  begin // CPC compare with carry 0000 01rd dddd rrrr
                     get_r_d_10(opcode, r, d, vd, vr);
                     res := vd - vr - ord(fSREG[S_C]);
                     //fState('cpc %s[%02x], %s[%02x] := %02x\n', avr_regname(d), vd, avr_regname(r), vr, res);
                     if (res <> 0) then
                        fSREG[S_Z] := false;
                     fSREG[S_H] := get_compare_carry(res, vd, vr, 3);
                     fSREG[S_V] := get_compare_overflow(res, vd, vr);
                     fSREG[S_N] := odd(res shr 7);
                     fSREG[S_C] := get_compare_carry(res, vd, vr, 7);
                     fSREG[S_S] := fSREG[S_N] xor fSREG[S_V];

                     //writeln('CPC ', vd, ',',vr,'=',res, ' C',fSREG[S_N], ' N',fSREG[S_N], ' V',fSREG[S_V]);
                  end;
                  $0c00:
                  begin // ADD without carry 0000 11 rd dddd rrrr
                     get_r_d_10(opcode, r, d, vd, vr);
                     res := vd + vr;
                     if (r = d) then
                     begin
                        //fState('lsl %s[%02x] := %02x\n', avr_regname(d), vd, res and $ff);
                     end
                     else
                     begin
                        //fState('add %s[%02x], %s[%02x] := %02x\n', avr_regname(d), vd, avr_regname(r), vr, res);
                     end;
                     SetReg(d, res);
                     fSREG[S_Z] := (res = 0);
                     fSREG[S_H] := get_add_carry(res, vd, vr, 3);
                     fSREG[S_V] := get_add_overflow(res, vd, vr);
                     fSREG[S_N] := odd(res shr 7);
                     fSREG[S_C] := get_add_carry(res, vd, vr, 7);
                     fSREG[S_S] := fSREG[S_N] xor fSREG[S_V];
                  end;
                  $0800:
                  begin // SBC subtract with carry 0000 10rd dddd rrrr
                     get_r_d_10(opcode, r, d, vd, vr);
                     res := vd - vr - ord(fSREG[S_C]);
                     //fState('sbc %s[%02x], %s[%02x] := %02x\n', avr_regname(d), fData[d], avr_regname(r), fData[r], res);
                     SetReg(d, res);
                     if (res <> 0) then
                        fSREG[S_Z] := false;
                     fSREG[S_H] := get_sub_carry(res, vd, vr, 3);
                     fSREG[S_V] := get_sub_overflow(res, vd, vr);
                     fSREG[S_N] := odd(res shr 7);
                     fSREG[S_C] := get_sub_carry(res, vd, vr, 7);
                     fSREG[S_S] := fSREG[S_N] xor fSREG[S_V];
                  end;
                  else
                     case (opcode and $ff00) of
                        $0100:
                        begin // MOVW – Copy Register Word 0000 0001 dddd rrrr
                           d := ((opcode shr 4) and $f) shl 1;
                           r := ((opcode) and $f) shl 1;
                           //fState('movw %s:%s, %s:%s[%02x%02x]\n', avr_regname(d), avr_regname(d+1), avr_regname(r), avr_regname(r+1), fData[r+1], fData[r]);
                           SetReg(d, fData[r]);
                           SetReg(d + 1, fData[r + 1]);
                        end;
                        $0200:
                        begin // MULS – Multiply Signed 0000 0010 dddd rrrr
                           r := 16 + (opcode and $f);
                           d := 16 + ((opcode shr 4) and $f);
                           res16 := shortint(fData[r]) * shortint(fData[d]);
                           //fState('muls %s[%d], %s[%02x] := %d\n', avr_regname(d), shortint(fData[d]), avr_regname(r), shortint(fData[r]), res);
                           SetReg(0, res16);
                           SetReg(1, res16 shr 8);
                           fSREG[S_C] := odd(res16 shr 15);
                           fSREG[S_Z] := (res16 = 0);
                           Inc(cycle);
                        end;
                        $0300:
                        begin // MUL Multiply 0000 0011 fddd frrr
                           r := 16 + (opcode and $7);
                           d := 16 + ((opcode shr 4) and $7);
                           res16 := 0;
                           c := 0;
                           case (opcode and $88) of
                              $00:
                              begin // MULSU – Multiply Signed Unsigned 0000 0011 0ddd 0rrr
                                 res16 := byte(fData[r]) * shortint(fData[d]);
                                 c := (res16 shr 15) and 1;
                              end;
                              $08:
                              begin // FMUL Fractional Multiply Unsigned 0000 0011 0ddd 1rrr
                                 res16 := byte(fData[r]) * byte(fData[d]);
                                 c := (res16 shr 15) and 1;
                                 res16 := res16 shl 1;
                              end;
                              $80:
                              begin // FMULS – Multiply Signed 0000 0011 1ddd 0rrr
                                 res16 := shortint(fData[r]) * shortint(fData[d]);
                                 c := (res16 shr 15) and 1;
                                 res16 := res16 shl 1;
                              end;
                              $88:
                              begin // FMULSU – Multiply Signed Unsigned 0000 0011 1ddd 1rrr
                                 res16 := byte(fData[r]) * shortint(fData[d]);
                                 c := (res16 shr 15) and 1;
                                 res16 := res16 shl 1;
                              end;
                           end;
                           Inc(cycle);
                           //fState('%s %s[%d], %s[%02x] := %d\n', name, avr_regname(d), shortint(fData[d]), avr_regname(r), shortint(fData[r]), res16);
                           SetReg(0, res16);
                           SetReg(1, res16 shr 8);
                           fSREG[S_C] := odd(c);
                           fSREG[S_Z] := (res16 = 0);
                        end;
                        else
                           InvalidOpcode();
                     end;
               end;
            end;
         end;
      end;
      $1000:
      begin
         case (opcode and $fc00) of
            $1800:
            begin // SUB without carry 0000 10 rd dddd rrrr
               get_r_d_10(opcode, r, d, vd, vr);
               res := vd - vr;
               //fState('sub %s[%02x], %s[%02x] := %02x\n', avr_regname(d), vd, avr_regname(r), vr, res);
               SetReg(d, res);
               fSREG[S_Z] := (res = 0);
               fSREG[S_H] := get_sub_carry(res, vd, vr, 3);
               fSREG[S_V] := get_sub_overflow(res, vd, vr);
               fSREG[S_N] := odd(res shr 7);
               fSREG[S_C] := get_sub_carry(res, vd, vr, 7);
               fSREG[S_S] := fSREG[S_N] xor fSREG[S_V];
            end;
            $1000:
            begin // CPSE Compare, skip if equal 0000 00 rd dddd rrrr
               get_r_d_10(opcode, r, d, vd, vr);
               res := Ord(vd = vr);
               //fState('cpse %s[%02x], %s[%02x]\t; Will%s skip\n', avr_regname(d), fData[d], avr_regname(r), fData[r], res ? '':' not');
               if (res <> 0) then
               begin
                  if (Is32bitInstr(new_pc)) then
                  begin
                     new_pc := new_pc + 4;
                     Inc(cycle, 2);
                  end
                  else
                  begin
                     new_pc := new_pc + 2;
                     Inc(cycle);
                  end;
               end;
            end;
            $1400:
            begin // CP Compare 0000 01 rd dddd rrrr
               get_r_d_10(opcode, r, d, vd, vr);
               res := vd - vr;
               //fState('cp %s[%02x], %s[%02x] := %02x\n', avr_regname(d), vd, avr_regname(r), vr, res);
               fSREG[S_Z] := (res = 0);
               fSREG[S_H] := get_compare_carry(res, vd, vr, 3);
               fSREG[S_V] := get_compare_overflow(res, vd, vr);
               fSREG[S_N] := odd(res shr 7);
               fSREG[S_C] := get_compare_carry(res, vd, vr, 7);
               fSREG[S_S] := fSREG[S_N] xor fSREG[S_V];

               //writeln('CP ', vd, ',',vr,'=',res, ' C',fSREG[S_N], ' N',fSREG[S_N], ' V',fSREG[S_V]);
            end;
            $1c00:
            begin // ADD with carry 0001 11 rd dddd rrrr
               get_r_d_10(opcode, r, d, vd, vr);
               res := vd + vr + ord(fSREG[S_C]);
               if (r = d) then
               begin
                  //fState('rol %s[%02x] := %02x\n', avr_regname(d), fData[d], res);
               end
               else
               begin
                  //fState('addc %s[%02x], %s[%02x] := %02x\n', avr_regname(d), fData[d], avr_regname(r), fData[r], res);
               end;
               SetReg(d, res);
               fSREG[S_Z] := (res = 0);
               fSREG[S_H] := get_add_carry(res, vd, vr, 3);
               fSREG[S_V] := get_add_overflow(res, vd, vr);
               fSREG[S_N] := odd(res shr 7);
               fSREG[S_C] := get_add_carry(res, vd, vr, 7);
               fSREG[S_S] := fSREG[S_N] xor fSREG[S_V];
            end;
            else
               InvalidOpcode();
         end;
      end;
      $2000:
      begin
         case (opcode and $fc00) of
            $2000:
            begin // AND 0010 00rd dddd rrrr
               get_r_d_10(opcode, r, d, vd, vr);
               res := vd and vr;
               if (r = d) then
               begin
                  //fState('tst %s[%02x]\n', avr_regname(d), fData[d]);
               end
               else
               begin
                  //fState('and %s[%02x], %s[%02x] := %02x\n', avr_regname(d), vd, avr_regname(r), vr, res);
               end;
               SetReg(d, res);
               fSREG[S_Z] := (res = 0);
               fSREG[S_N] := odd(res shr 7);
               fSREG[S_V] := false;
               fSREG[S_S] := fSREG[S_N] xor fSREG[S_V];
            end;
            $2400:
            begin // EOR 0010 01rd dddd rrrr
               get_r_d_10(opcode, r, d, vd, vr);
               res := vd xor vr;
               if (r = d) then
               begin
                  //fState('clr %s[%02x]\n', avr_regname(d), fData[d]);
               end
               else
               begin
                  //fState('eor %s[%02x], %s[%02x] := %02x\n', avr_regname(d), vd, avr_regname(r), vr, res);
               end;
               SetReg(d, res);
               fSREG[S_Z] := (res = 0);
               fSREG[S_N] := odd(res shr 7);
               fSREG[S_V] := false;
               fSREG[S_S] := fSREG[S_N] xor fSREG[S_V];
            end;
            $2800:
            begin // OR Logical OR 0010 10rd dddd rrrr
               get_r_d_10(opcode, r, d, vd, vr);
               res := vd or vr;
               //fState('or %s[%02x], %s[%02x] := %02x\n', avr_regname(d), vd, avr_regname(r), vr, res);
               SetReg(d, res);
               fSREG[S_Z] := (res = 0);
               fSREG[S_N] := odd(res shr 7);
               fSREG[S_V] := false;
               fSREG[S_S] := fSREG[S_N] xor fSREG[S_V];
            end;
            $2c00:
            begin // MOV 0010 11rd dddd rrrr
               get_r_dd_10(opcode, r, d, vr);
               res := vr;
               //fState('mov %s, %s[%02x] := %02x\n', avr_regname(d), avr_regname(r), vr, res);
               SetReg(d, res);
            end;
            else
               InvalidOpcode();
         end;
      end;
      $3000:
      begin // CPI 0011 KKKK rrrr KKKK
         get_k_r16(opcode, r, k);
         vr := fData[r];
         res := vr - k;
         //fState('cpi %s[%02x], $%02x\n', avr_regname(r), vr, k);
         fSREG[S_Z] := (res = 0);
         fSREG[S_H] := get_compare_carry(res, vr, k, 3);
         fSREG[S_V] := get_compare_overflow(res, vr, k);
         fSREG[S_N] := odd(res shr 7);
         fSREG[S_C] := get_compare_carry(res, vr, k, 7);
         fSREG[S_S] := fSREG[S_N] xor fSREG[S_V];
      end;
      $4000:
      begin // SBCI Subtract Immediate With Carry 0101 10 kkkk dddd kkkk
         get_k_r16(opcode, r, k);
         vr := fData[r];
         res := vr - k - ord(fSREG[S_C]);
         //fState('sbci %s[%02x], $%02x := %02x\n', avr_regname(r), fData[r], k, res);
         SetReg(r, res);
         if (res <> 0) then
            fSREG[S_Z] := false;
         fSREG[S_N] := odd(res shr 7);
         fSREG[S_C] := ((k + ord(fSREG[S_C])) > vr);
         fSREG[S_S] := fSREG[S_N] xor fSREG[S_V];
      end;
      $5000:
      begin // SUB Subtract Immediate 0101 10 kkkk dddd kkkk
         get_k_r16(opcode, r, k);
         vr := fData[r];
         res := vr - k;
         //fState('subi %s[%02x], $%02x := %02x\n', avr_regname(r), fData[r], k, res);
         SetReg(r, res);
         fSREG[S_Z] := (res = 0);
         fSREG[S_N] := odd(res shr 7);
         fSREG[S_C] := (k > vr);
         fSREG[S_S] := fSREG[S_N] xor fSREG[S_V];
      end;
      $6000:
      begin // ORI aka SBR Logical AND with Immediate 0110 kkkk dddd kkkk
         get_k_r16(opcode, r, k);
         res := fData[r] or k;
         //fState('ori %s[%02x], $%02x\n', avr_regname(r), fData[r], k);
         SetReg(r, res);
         fSREG[S_Z] := (res = 0);
         fSREG[S_N] := odd(res shr 7);
         fSREG[S_V] := false;
         fSREG[S_S] := fSREG[S_N] xor fSREG[S_V];
      end;
      $7000:
      begin // ANDI Logical AND with Immediate 0111 kkkk dddd kkkk
         get_k_r16(opcode, r, k);
         res := fData[r] and k;
         //fState('andi %s[%02x], $%02x\n', avr_regname(r), fData[r], k);
         SetReg(r, res);
         fSREG[S_Z] := (res = 0);
         fSREG[S_N] := odd(res shr 7);
         fSREG[S_V] := false;
         fSREG[S_S] := fSREG[S_N] xor fSREG[S_V];
      end;
      $a000,
      $8000:
      begin
         case (opcode and $d008) of
            $a000,
            $8000:
            begin // LD (LDD) – Load Indirect using Z 10q0 qq0r rrrr 0qqq
               v := fData[R_ZL] or (fData[R_ZH] shl 8);
               r := (opcode shr 4) and $1f;
               q := ((opcode and $2000) shr 8) or ((opcode and $0c00) shr 7) or (opcode and $7);
               if (opcode and $0200) <> 0 then
               begin
                  //fState('st (Z+%d[%04x]), %s[%02x]\n', q, v+q, avr_regname(r), fData[r]);
                  SetRAM(v + q, fData[r]);
               end
               else
               begin
                  //fState('ld %s, (Z+%d[%04x]):=[%02x]\n', avr_regname(r), q, v+q, fData[v+q]);
                  SetReg(r, GetRAM(v + q));
               end;
               Inc(cycle); // 2 cycles, 3 for tinyavr
            end;
            $a008,
            $8008:
            begin // LD (LDD) – Load Indirect using Y 10q0 qq0r rrrr 1qqq
               v := fData[R_YL] or (fData[R_YH] shl 8);
               r := (opcode shr 4) and $1f;
               q := ((opcode and $2000) shr 8) or ((opcode and $0c00) shr 7) or (opcode and $7);
               if (opcode and $0200) <> 0 then
               begin
                  //fState('st (Y+%d[%04x]), %s[%02x]\n', q, v+q, avr_regname(r), fData[r]);
                  SetRAM(v + q, fData[r]);
               end
               else
               begin
                  //fState('ld %s, (Y+%d[%04x]):=[%02x]\n', avr_regname(r), q, v+q, fData[v+q]);
                  SetReg(r, GetRAM(v + q));
               end;
               cycle := cycle + 1; // 2 cycles, 3 for tinyavr
            end;
            else
               InvalidOpcode();
         end;
      end;
      $9000:
      begin
         { this is an annoying special , but at least these lines handle all the fSREG set/clear opcodes }
         if ((opcode and $ff0f) = $9408) then
         begin
            b := (opcode shr 4) and 7;
            //fState('%s%c\n', opcode and $0080 ? 'cl' : 'se', _sreg_bit_name[b]);
            fSREG[TSregField(b)] := ((opcode and $0080) = 0);
         end
         else
            case (opcode) of
               $9588:
               begin // SLEEP
                  //fState('sleep\n');
               { Don't sleep if there are interrupts about to be serviced. then
               * Without this check, it was possible to incorrectly enter a fState
               * in which the cpu was sleeping and interrupts were disabled. For more
               * details, see the commit message. }
                  if ((not InterruptPending()) or (not fSREG[S_I])) then
                     fState := cpu_Sleeping;
               end;
               $9598:
               begin // BREAK
                  BreakHit;
                  new_pc:=fPC;
                  //fState('break\n');
               {if (gdb) then begin
                  // if gdb is on, we break here as in here then
                  // and we do so until gdb restores the instruction
                  // that was here before
                  fState := cpu_StepDone;
                  new_pc := fPC;
                  cycle := 0;
               end;}
               end;
               $95a8:
               begin // WDR
                  //fState('wdr\n');
                  WatchdogReset;
               end;
               $95e8:
               begin // SPM
                  //fState('spm\n');
                  SPM;
               end;
               $9409, // IJMP Indirect jump 1001 0100 0000 1001
               $9419, // EIJMP Indirect jump 1001 0100 0001 1001 bit 4 is 'indirect'
               $9509, // ICALL Indirect Call to Subroutine 1001 0101 0000 1001
               $9519:
               begin // EICALL Indirect Call to Subroutine 1001 0101 0001 1001 bit 8 is 'push pc'
                  e := opcode and $10;
                  p := opcode and $100;
                  if (e and (not fEIND)) <> 0 then
                     InvalidOpcode();
                  z := fData[R_ZL] or (fData[R_ZH] shl 8);
                  if (e <> 0) then
                     z := z or (fData[fEIND] shl 16);
                  //fState('%si%s Z[%04x]\n', e?'e':'', p?'call':'jmp', z shl 1);
                  if (p <> 0) then
                  begin
                     Inc(cycle);
                     Push16(new_pc shr 1);
                  end;
                  new_pc := z shl 1;
                  Inc(cycle);
               end;
               $9518, // RETI
               $9508:
               begin // RET
                  new_pc := Pop16() shl 1;
                  if (opcode and $10) <> 0 then // reti
                     fSREG[S_I] := true;
                  cycle := cycle + 3;
                  //fState('ret%s\n', opcode and $10 ? 'i' : '');
               end;
               $95c8:
               begin // LPM Load Program Memory R0 <- (Z)
                  z := fData[R_ZL] or (fData[R_ZH] shl 8);
                  //fState('lpm %s, (Z[%04x])\n', avr_regname(0), z);
                  cycle := cycle + 2; // 3 cycles
                  SetReg(0, fFLASH[z]);
               end;
               $9408, $9418, $9428, $9438, $9448, $9458, $9468,
               $9478:
               begin // BSET 1001 0100 0ddd 1000
                  b := (opcode shr 4) and 7;
                  fSREG[TSregField(b)] := true;
                  //fState('bset %c\n', _sreg_bit_name[b]);
               end;
               $9488, $9498, $94a8, $94b8, $94c8, $94d8, $94e8,
               $94f8: // bit 7 is 'clear vs set'
               begin // BCLR 1001 0100 1ddd 1000
                  b := (opcode shr 4) and 7;
                  fSREG[TSregField(b)] := false;
                  //fState('bclr %c\n', _sreg_bit_name[b]);
               end;
               else
               begin
                  case (opcode and $fe0f) of
                     $9000:
                     begin // LDS Load Direct from fData Space, 32 bits
                        r := (opcode shr 4) and $1f;
                        x := (fFLASH[new_pc + 1] shl 8) or fFLASH[new_pc];
                        new_pc := new_pc + 2;
                        //fState('lds %s[%02x], $%04x\n', avr_regname(r), fData[r], x);
                        SetReg(r, GetRAM(x));
                        Inc(cycle); // 2 cycles
                     end;
                     $9005,
                     $9004:
                     begin // LPM Load Program Memory 1001 000d dddd 01oo
                        z := fData[R_ZL] or (fData[R_ZH] shl 8);
                        r := (opcode shr 4) and $1f;
                        op := opcode and 3;
                        //fState('lpm %s, (Z[%04x]%s)\n', avr_regname(r), z, opcode?'+':'');
                        SetReg(r, fFLASH[z]);
                        if (op = 1) then
                        begin
                           Inc(z);
                           SetReg(R_ZH, z shr 8);
                           SetReg(R_ZL, z);
                        end;
                        cycle := cycle + 2; // 3 cycles
                     end;
                     $9006,
                     $9007:
                     begin // ELPM Extended Load Program Memory 1001 000d dddd 01oo
                        if (not fRAMPZ) <> 0 then
                           InvalidOpcode();
                        z := fData[R_ZL] or (fData[R_ZH] shl 8) or (fData[fRAMPZ] shl 16);
                        r := (opcode shr 4) and $1f;
                        op := opcode and 3;
                        //fState('elpm %s, (Z[%02x:%04x]%s)\n', avr_regname(r), z shr 16, zand$ffff, opcode?'+':'');
                        SetReg(r, fFLASH[z]);
                        if (op = 3) then
                        begin
                           Inc(z);
                           SetReg(fRAMPZ, z shr 16);
                           SetReg(R_ZH, z shr 8);
                           SetReg(R_ZL, z);
                        end;
                        cycle := cycle + 2; // 3 cycles
                     end;
                  {
                  * Load store instructions
                  *
                  * 1001 00sr rrrr iioo
                  * s := 0 := load, 1 := store
                  * ii := 16 bits register index, 11 := Z, 10 := Y, 00 := X
                  * oo := 1) post increment, 2) pre-decrement
                  }
                     $900c,
                     $900d,
                     $900e:
                     begin // LD Load Indirect from fData using X 1001 000r rrrr 11oo
                        op := opcode and 3;
                        res16 := (opcode shr 4) and $1f;
                        x := (fData[R_XH] shl 8) or fData[R_XL];
                        //fState('ld %s, %sX[%04x]%s\n', avr_regname(res16), op = 2 ? '--' : '', x, op = 1 ? '++' : '');
                        Inc(cycle); // 2 cycles (1 for tinyavr, except with inc/dec 2)
                        if (op = 2) then
                           Dec(x);
                        SetReg(res16, GetRAM(x));
                        if (op = 1) then
                           Inc(x);
                        SetReg(R_XH, x shr 8);
                        SetReg(R_XL, x);
                     end;
                     $920c,
                     $920d,
                     $920e:
                     begin // ST Store Indirect fData Space X 1001 001r rrrr 11oo
                        op := opcode and 3;
                        r := (opcode shr 4) and $1f;
                        x := (fData[R_XH] shl 8) or fData[R_XL];
                        //fState('st %sX[%04x]%s, %s[%02x] \n', op = 2 ? '--' : '', x, op = 1 ? '++' : '', avr_regname(r), fData[r]);
                        Inc(cycle); // 2 cycles, except tinyavr
                        if (op = 2) then
                           Dec(x);
                        SetRAM(x, fData[r]);
                        if (op = 1) then
                           Inc(x);
                        SetReg(R_XH, x shr 8);
                        SetReg(R_XL, x);
                     end;
                     $9009,
                     $900a:
                     begin // LD Load Indirect from fData using Y 1001 000r rrrr 10oo
                        op := opcode and 3;
                        r := (opcode shr 4) and $1f;
                        y := (fData[R_YH] shl 8) or fData[R_YL];
                        //fState('ld %s, %sY[%04x]%s\n', avr_regname(r), op = 2 ? '--' : '', y, op = 1 ? '++' : '');
                        Inc(cycle); // 2 cycles, except tinyavr
                        if (op = 2) then
                           Dec(y);
                        SetReg(r, GetRAM(y));
                        if (op = 1) then
                           Inc(y);
                        SetReg(R_YH, y shr 8);
                        SetReg(R_YL, y);
                     end;
                     $9209,
                     $920a:
                     begin // ST Store Indirect fData Space Y 1001 001r rrrr 10oo
                        op := opcode and 3;
                        r := (opcode shr 4) and $1f;
                        y := (fData[R_YH] shl 8) or fData[R_YL];
                        //fState('st %sY[%04x]%s, %s[%02x]\n', op = 2 ? '--' : '', y, op = 1 ? '++' : '', avr_regname(r), fData[r]);
                        Inc(cycle);
                        if (op = 2) then
                           Dec(y);
                        SetRAM(y, fData[r]);
                        if (op = 1) then
                           Inc(y);
                        SetReg(R_YH, y shr 8);
                        SetReg(R_YL, y);
                     end;
                     $9200:
                     begin // STS not  Store Direct to fData Space, 32 bits
                        r := (opcode shr 4) and $1f;
                        x := (fFLASH[new_pc + 1] shl 8) or fFLASH[new_pc];
                        new_pc := new_pc + 2;
                        //fState('sts $%04x, %s[%02x]\n', x, avr_regname(r), fData[r]);
                        Inc(cycle);
                        SetRAM(x, fData[r]);
                     end;
                     $9001,
                     $9002:
                     begin // LD Load Indirect from fData using Z 1001 001r rrrr 00oo
                        op := opcode and 3;
                        r := (opcode shr 4) and $1f;
                        z := (fData[R_ZH] shl 8) or fData[R_ZL];
                        //fState('ld %s, %sZ[%04x]%s\n', avr_regname(r), op = 2 ? '--' : '', z, op = 1 ? '++' : '');
                        Inc(cycle);
                        ; // 2 cycles, except tinyavr
                        if (op = 2) then
                           Dec(z);
                        SetReg(r, GetRAM(z));
                        if (op = 1) then
                           Inc(z);
                        SetReg(R_ZH, z shr 8);
                        SetReg(R_ZL, z);
                     end;
                     $9201,
                     $9202:
                     begin // ST Store Indirect fData Space Z 1001 001r rrrr 00oo
                        op := opcode and 3;
                        r := (opcode shr 4) and $1f;
                        z := (fData[R_ZH] shl 8) or fData[R_ZL];
                        //fState('st %sZ[%04x]%s, %s[%02x] \n', op = 2 ? '--' : '', z, op = 1 ? '++' : '', avr_regname(r), fData[r]);
                        Inc(cycle); // 2 cycles, except tinyavr
                        if (op = 2) then
                           Dec(z);
                        SetRAM(z, fData[r]);
                        if (op = 1) then
                           Inc(z);
                        SetReg(R_ZH, z shr 8);
                        SetReg(R_ZL, z);
                     end;
                     $900f:
                     begin // POP 1001 000d dddd 1111
                        r := (opcode shr 4) and $1f;
                        SetReg(r, Pop8());
                        //fState('pop %s (@%04x)[%02x]\n', avr_regname(r), sp, fData[sp]);
                        Inc(cycle);
                     end;
                     $920f:
                     begin // PUSH 1001 001d dddd 1111
                        r := (opcode shr 4) and $1f;
                        Push8(fData[r]);
                        //fState('push %s[%02x] (@%04x)\n', avr_regname(r), fData[r], sp);
                        Inc(cycle);
                     end;
                     $9400:
                     begin // COM – One’s Complement
                        r := (opcode shr 4) and $1f;
                        res := $ff - fData[r];
                        //fState('com %s[%02x] := %02x\n', avr_regname(r), fData[r], res);
                        SetReg(r, res);
                        fSREG[S_Z] := (res = 0);
                        fSREG[S_N] := (res shr 7)<>0;
                        fSREG[S_V] := false;
                        fSREG[S_C] := true;
                        fSREG[S_S] := fSREG[S_N] xor fSREG[S_V];
                     end;
                     $9401:
                     begin // NEG – Two’s Complement
                        r := (opcode shr 4) and $1f;
                        rd := fData[r];
                        res := $00 - rd;
                        //fState('neg %s[%02x] := %02x\n', avr_regname(r), rd, res);
                        SetReg(r, res);
                        fSREG[S_H] := odd((res shr 3) or (rd shr 3));
                        fSREG[S_Z] := (res = 0);
                        fSREG[S_N] := (res shr 7)<>0;
                        fSREG[S_V] := (res = $80);
                        fSREG[S_C] := (res <> 0);
                        fSREG[S_S] := fSREG[S_N] xor fSREG[S_V];
                     end;
                     $9402:
                     begin // SWAP – Swap Nibbles
                        r := (opcode shr 4) and $1f;
                        res := (fData[r] shr 4) or (fData[r] shl 4);
                        //fState('swap %s[%02x] := %02x\n', avr_regname(r), fData[r], res);
                        SetReg(r, res);
                     end;
                     $9403:
                     begin // INC – Increment
                        r := (opcode shr 4) and $1f;
                        res := fData[r] + 1;
                        //fState('inc %s[%02x] := %02x\n', avr_regname(r), fData[r], res);
                        SetReg(r, res);
                        fSREG[S_Z] := (res = 0);
                        fSREG[S_N] := (res shr 7)<>0;
                        fSREG[S_V] := (res = $80);
                        fSREG[S_S] := fSREG[S_N] xor fSREG[S_V];
                     end;
                     $9405:
                     begin // ASR – Arithmetic Shift Right 1001 010d dddd 0101
                        r := (opcode shr 4) and $1f;
                        vr := fData[r];
                        res := (vr shr 1) or (vr and $80);
                        //fState('asr %s[%02x]\n', avr_regname(r), vr);
                        SetReg(r, res);
                        fSREG[S_Z] := (res = 0);
                        fSREG[S_C] := odd(vr);
                        fSREG[S_N] := (res shr 7)<>0;
                        fSREG[S_V] := fSREG[S_N] xor fSREG[S_C];
                        fSREG[S_S] := fSREG[S_N] xor fSREG[S_V];
                     end;
                     $9406:
                     begin // LSR 1001 010d dddd 0110
                        r := (opcode shr 4) and $1f;
                        vr := fData[r];
                        res := vr shr 1;
                        //fState('lsr %s[%02x]\n', avr_regname(r), vr);
                        SetReg(r, res);
                        fSREG[S_Z] := (res = 0);
                        fSREG[S_C] := odd(vr);
                        fSREG[S_N] := false;
                        fSREG[S_V] := fSREG[S_N] xor fSREG[S_C];
                        fSREG[S_S] := fSREG[S_N] xor fSREG[S_V];
                     end;
                     $9407:
                     begin // ROR 1001 010d dddd 0111
                        r := (opcode shr 4) and $1f;
                        vr := fData[r];
                        if fSREG[S_C] then
                           res := $80 or vr shr 1
                        else
                           res := vr shr 1;
                        //fState('ror %s[%02x]\n', avr_regname(r), vr);
                        SetReg(r, res);
                        fSREG[S_Z] := (res = 0);
                        fSREG[S_C] := odd(vr);
                        fSREG[S_N] := (res shr 7)<>0;
                        fSREG[S_V] := fSREG[S_N] xor fSREG[S_C];
                        fSREG[S_S] := fSREG[S_N] xor fSREG[S_V];
                     end;
                     $940a:
                     begin // DEC – Decrement
                        r := (opcode shr 4) and $1f;
                        res := fData[r] - 1;
                        //fState('dec %s[%02x] := %02x\n', avr_regname(r), fData[r], res);
                        SetReg(r, res);
                        fSREG[S_Z] := (res = 0);
                        fSREG[S_N] := (res shr 7)<>0;
                        fSREG[S_V] := (res = $7f);
                        fSREG[S_S] := fSREG[S_N] xor fSREG[S_V];
                     end;
                     $940c,
                     $940d:
                     begin // JMP Long Call to sub, 32 bits
                        a := ((opcode and $01f0) shr 3) or (opcode and 1);
                        x := (fFLASH[new_pc + 1] shl 8) or fFLASH[new_pc];
                        a := (a shl 16) or x;
                        //fState('jmp $%06x\n', a);
                        new_pc := a shl 1;
                        cycle := cycle + 2;
                     end;
                     $940e,
                     $940f:
                     begin // CALL Long Call to sub, 32 bits
                        a := ((opcode and $01f0) shr 3) or (opcode and 1);
                        x := (fFLASH[new_pc + 1] shl 8) or fFLASH[new_pc];
                        a := (a shl 16) or x;
                        //fState('call $%06x\n', a);
                        new_pc := new_pc + 2;
                        Push16(new_pc shr 1);
                        new_pc := a shl 1;
                        cycle := cycle + 3; // 4 cycles; FIXME 5 on devices with 22 bit fPC
                     end;
                     else
                     begin
                        case (opcode and $ff00) of
                           $9600:
                           begin // ADIW - Add Immediate to Word 1001 0110 KKdd KKKK
                              r := 24 + ((opcode shr 3) and $6);
                              k := ((opcode and $00c0) shr 2) or (opcode and $f);
                              rdl := fData[r];
                              rdh := fData[r + 1];
                              res32 := rdl or (rdh shl 8);
                              //fState('adiw %s:%s[%04x], $%02x\n', avr_regname(r), avr_regname(r+1), res32, k);
                              res32 := res32 + k;
                              SetReg(r + 1, res32 shr 8);
                              SetReg(r, res32);
                              fSREG[S_V] := (not odd(rdh shr 7)) and (odd(res32 shr 15));
                              fSREG[S_Z] := ((res32 and $ffff) = 0);
                              fSREG[S_N] := odd(res32 shr 15);
                              fSREG[S_C] := (not (odd(res32 shr 15))) and odd(rdh shr 7);
                              fSREG[S_S] := fSREG[S_N] xor fSREG[S_V];
                              Inc(cycle);
                           end;
                           $9700:
                           begin // SBIW - Subtract Immediate from Word 1001 0110 KKdd KKKK
                              r := 24 + ((opcode shr 3) and $6);
                              k := ((opcode and $00c0) shr 2) or (opcode and $f);
                              rdl := fData[r];
                              rdh := fData[r + 1];
                              res32 := rdl or (rdh shl 8);
                              //fState('sbiw %s:%s[%04x], $%02x\n', avr_regname(r), avr_regname(r+1), res, k);
                              res32 := res32 - k;
                              SetReg(r + 1, res32 shr 8);
                              SetReg(r, res32);
                              fSREG[S_V] := odd(rdh shr 7) and (odd(not (res32 shr 15)));
                              fSREG[S_Z] := ((res32 and $ffff) = 0);
                              fSREG[S_N] := odd(res32 shr 15);
                              fSREG[S_C] := (odd(res32 shr 15)) and odd((not rdh) shr 7);
                              fSREG[S_S] := fSREG[S_N] xor fSREG[S_V];
                              Inc(cycle);
                           end;
                           $9800:
                           begin // CBI - Clear Bit in I/O Register 1001 1000 AAAA Abbb
                              io := ((opcode shr 3) and $1f) + 32;
                              b := opcode and $7;
                              res := GetRAM(io) and (not (1 shl b));
                              //fState('cbi %s[%04x], $%02x := %02x\n', avr_regname(io), fData[io], 1shlb, res);
                              SetRAM(io, res);
                              Inc(cycle);
                           end;
                           $9900:
                           begin // SBIC - Skip if Bit in I/O Register is Cleared 1001 0111 AAAA Abbb
                              io := ((opcode shr 3) and $1f) + 32;
                              b := opcode and $7;
                              res := GetRAM(io) and (1 shl b);
                              //fState('sbic %s[%04x], $%02x\t; Will%s branch\n', avr_regname(io), fData[io], 1shlb, not res?'':' not');
                              if (not (res <> 0)) then
                              begin
                                 if (Is32bitInstr(new_pc)) then
                                 begin
                                    new_pc := new_pc + 4;
                                    cycle := cycle + 2;
                                 end
                                 else
                                 begin
                                    new_pc := new_pc + 2;
                                    Inc(cycle);
                                 end;
                              end;
                           end;
                           $9a00:
                           begin // SBI - Set Bit in I/O Register 1001 1000 AAAA Abbb
                              io := ((opcode shr 3) and $1f) + 32;
                              b := opcode and $7;
                              res := GetRAM(io) or (1 shl b);
                              //fState('sbi %s[%04x], $%02x := %02x\n', avr_regname(io), fData[io], 1shlb, res);
                              SetRAM(io, res);
                              Inc(cycle);
                           end;
                           $9b00:
                           begin // SBIS - Skip if Bit in I/O Register is Set 1001 1011 AAAA Abbb
                              io := ((opcode shr 3) and $1f) + 32;
                              b := opcode and $7;
                              res := GetRAM(io) and (1 shl b);
                              //fState('sbis %s[%04x], $%02x\t; Will%s branch\n', avr_regname(io), fData[io], 1shlb, res?'':' not');
                              if (res <> 0) then
                              begin
                                 if (Is32bitInstr(new_pc)) then
                                 begin
                                    new_pc := new_pc + 4;
                                    cycle := cycle + 2;
                                 end
                                 else
                                 begin
                                    new_pc := new_pc + 2;
                                    Inc(cycle);
                                 end;
                              end;
                           end;
                           else
                              case (opcode and $fc00) of
                                 $9c00:
                                 begin // MUL - Multiply Unsigned 1001 11rd dddd rrrr
                                    get_r_d_10(opcode, r, d, vd, vr);
                                    res16 := vd * vr;
                                    //fState('mul %s[%02x], %s[%02x] := %04x\n', avr_regname(d), vd, avr_regname(r), vr, res);
                                    Inc(cycle);
                                    SetReg(0, res16);
                                    SetReg(1, res16 shr 8);
                                    fSREG[S_Z] := (res16 = 0);
                                    fSREG[S_C] := (res16 shr 15)<>0;
                                 end;
                                 else
                                    InvalidOpcode();
                              end;
                        end;
                     end;
                  end;
               end;
            end;
      end;
      $b000:
      begin
         case (opcode and $f800) of
            $b800:
            begin // OUT A,Rr 1011 1AAr rrrr AAAA
               r := (opcode shr 4) and $1f;
               d := ((((opcode shr 9) and 3) shl 4) or ((opcode) and $f)) + 32;
               //fState('out %s, %s[%02x]\n', avr_regname(A), avr_regname(r), fData[r]);
               SetRAM(d, fData[r]);
            end;
            $b000:
            begin // IN Rd,A 1011 0AAr rrrr AAAA
               r := (opcode shr 4) and $1f;
               d := ((((opcode shr 9) and 3) shl 4) or ((opcode) and $f)) + 32;
               //fState('in %s, %s[%02x]\n', avr_regname(r), avr_regname(A), fData[A]);
               SetReg(r, GetRAM(d));
            end;
            else
               InvalidOpcode();
         end;
      end;
      $c000:
      begin
         // RJMP 1100 kkkk kkkk kkkk
         // int16_t o := ((int16_t)(opcode shl 4)) shr 4; // CLANG BUGnot
         o := (smallint((opcode shl 4) and $ffff)) shr 4;
         //fState('rjmp .%d [%04x]\n', o, new_pc + (o shl 1));
         new_pc := new_pc + (o shl 1);
         Inc(cycle);
      end;
      $d000:
      begin
         // RCALL 1100 kkkk kkkk kkkk
         // int16_t o := ((int16_t)(opcode shl 4)) shr 4; // CLANG BUGnot
         o := (smallint((opcode shl 4) and $ffff)) shr 4;
         //fState('rcall .%d [%04x]\n', o, new_pc + (o shl 1));
         Push16(new_pc shr 1);
         new_pc := new_pc + (o shl 1);
         cycle := cycle + 2;
         // 'rcall .1' is used as a cheap 'push 16 bits of room on the stack'
      end;
      $e000:
      begin // LDI Rd, K 1110 KKKK RRRR KKKK -- aka SER (LDI r, $ff)
         d := 16 + ((opcode shr 4) and $f);
         k := ((opcode and $0f00) shr 4) or (opcode and $f);
         //fState('ldi %s, $%02x\n', avr_regname(d), k);
         SetReg(d, k);
      end;
      $f000:
      begin
         case (opcode and $fe00) of
            $f000,
            $f200,
            $f400,
            $f600:
            begin // All the fSREG branches
               o := (smallint(opcode shl 6)) shr 9; // offset
               s := opcode and 7;
               _set := (opcode and $0400) = 0; // this bit means BRXC otherwise BRXS
               branch := ((fSREG[TSregField(s)]) and _set) or ((not (fSREG[TSregField(s)])) and (not _set));
               {const char *names[2][8] := begin
                  begin 'brcc', 'brne', 'brpl', 'brvc', NULL, 'brhc', 'brtc', 'brid'end,;
                  begin 'brcs', 'breq', 'brmi', 'brvs', NULL, 'brhs', 'brts', 'brie'end,;
               end;;
               if (names[_set][s]) then begin
                  //fState('%s .%d [%04x]\t; Will%s branch\n', names[_set][s], o, new_pc + (o shl 1), branch ? '':' not');
               end else begin
                  //fState('%s%c .%d [%04x]\t; Will%s branch\n', _set ? 'brbs' : 'brbc', _sreg_bit_name[s], o, new_pc + (o shl 1), branch ? '':' not');
               end;}
               if (branch) then
               begin
                  Inc(cycle); // 2 cycles if taken, 1 otherwise then
                  new_pc := new_pc + (o shl 1);
               end;
            end;
            $f800,
            $f900:
            begin // BLD – Bit Store from T into a Bit in Register 1111 100r rrrr 0bbb
               r := (opcode shr 4) and $1f; // register index
               s := opcode and 7;
               if fSREG[s_t] then
                  v := (fData[r] and (not (1 shl s))) or (1 shl s)
               else
                  v := (fData[r] and (not (1 shl s))) or 0;
               //fState('bld %s[%02x], $%02x := %02x\n', avr_regname(r), fData[r], 1 shl s, v);
               SetReg(r, v);
            end;
            $fa00,
            $fb00:
            begin // BST – Bit Store into T from bit in Register 1111 100r rrrr 0bbb
               r := (opcode shr 4) and $1f; // register index
               s := opcode and 7;
               //fState('bst %s[%02x], $%02x\n', avr_regname(r), fData[r], 1 shl s);
               fSREG[S_T] := odd(fData[r] shr s);
            end;
            $fc00,
            $fe00:
            begin // SBRS/SBRC – Skip if Bit in Register is Set/Clear 1111 11sr rrrr 0bbb
               r := (opcode shr 4) and $1f; // register index
               s := opcode and 7;
               _set := (opcode and $0200) <> 0;
               branch := (((fData[r] and (1 shl s)) <> 0) and _set) or ((not ((fData[r] and (1 shl s)) <> 0)) and (not _set));
               //fState('%s %s[%02x], $%02x\t; Will%s branch\n', _set ? 'sbrs' : 'sbrc', avr_regname(r), fData[r], 1 shl s, branch ? '':' not');
               if (branch) then
               begin
                  if (Is32bitInstr(new_pc)) then
                  begin
                     Inc(new_pc, 4);
                     Inc(cycle, 2);
                  end
                  else
                  begin
                     Inc(new_pc, 2);
                     Inc(cycle);
                  end;
               end;
            end;
            else
               InvalidOpcode();
         end;
      end;
      else
         InvalidOpcode();
   end;
   cycle := cycle + cycle;
   Result := new_pc;
end;

procedure TAvr.WatchdogReset;
begin

end;

procedure TAvr.BreakHit;
begin

end;

procedure TAvr.SPM;
begin
   fFLASH[(fData[R_ZL] shl 8)+fData[R_ZH]] := fData[0];
   fFLASH[(fData[R_ZL] shl 8)+fData[R_ZH]+1] := fData[1];
end;

procedure TAvr.Step(Count: longint);
var i: longint;
    newPc: TFlashAddr;
begin
   for i := 0 to Count - 1 do
   begin
      newPc := RunOne;
      //writeln(inttohex(fPC, 4)+'->', IntToHex(newPc, 4));
      fPC := newPc;
   end;
end;

procedure TAvr.WriteFlash(const Data; Count, Offset: longint);
begin
   move(Data, fFLASH[Offset], Count);
end;

procedure TAvr.LoadFlashBinary(const AFilename: string);
var fil: File;
begin
   AssignFile(fil, AFilename);
   reset(fil,1);

   BlockRead(fil, fFLASH[0], FileSize(fil));

   CloseFile(fil);
end;

constructor TAvr.Create(AFlashSize: longint; ARamSize: longint);
begin
   inherited Create;

   setlength(fFLASH, AFlashSize);
   setlength(fData, ARamSize);

   fPC := 0;
   fExitRequested := false;
   fExitCode := 0;
end;

end.
