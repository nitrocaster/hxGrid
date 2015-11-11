unit SafeFPU;

interface
function TRUNC(s:single):int64;
function INT(s:single):single;
function frac(s:single):single;
procedure DisableFPUExceptions ;
function ceil(f:single):single;
function FloatToInt(s:single):integer;

implementation

const
 mychop: word =$1fbf;
 const05 : single = 0.5;
 DownRoundCW : WORD = $1CBF; 
 DefaultCW : WORD = $13BF;                          

{$O+}
function       TRUNC(s:single):int64;
asm
        SUB     ESP,12
        FSTCW   [ESP]
        FWAIT
        FLDCW   myChop
        FLD     dword ptr [ESP+20]
        FISTP   qword ptr [ESP+4]
        FWAIT
        FLDCW   [ESP]
        POP     ECX
        POP     EAX
        POP     EDX
end;

function       INT(s:single):single;
asm
        SUB     ESP,4
        FSTCW   [ESP]
        FWAIT
        FLDCW   myChop
        FLD     dword ptr [ESP+12]
        FRNDINT
        FWAIT
        FLDCW   [ESP]
        ADD     ESP,4
end;

function       frac(s:single):single;
asm
        FLD     dword ptr [esp+8]
        FLD     st(0)
        SUB     ESP,4
        FSTCW   [ESP]
        FWAIT
        FLDCW   myChop
        FRNDINT
        FWAIT
        FLDCW   [ESP]
        ADD     ESP,4
        FSUBP   st(1),st(0)
end;

(*
function FloatToInt(s:single):integer;
asm
        FLD     DWORD PTR [ESP+8]    //st(0)=s

        SUB     ESP,8

        FADD    DWORD PTR const05   //st(0)= s+0.5

        FNSTCW  [ESP].Word     // save
        FNSTCW  [ESP+2].Word   // scratch
        FWAIT
        OR      [ESP+2].Word, $0F00  // trunc toward zero, full precision
        FLDCW   [ESP+2].Word

        FISTP   dword ptr [ESP+4]
        FWAIT

        FLDCW   [ESP].Word
        POP     EAX
        POP     EAX
end;
*)

function FloatToInt(s:single):integer;
asm
        FLD     DWORD PTR [ESP+8]    //st(0)=s

        SUB     ESP,4

        FADD    DWORD PTR const05   //st(0)= s+0.5

        FLDCW   DownRoundCW;
        FISTP   dword ptr [ESP]
        FWAIT

        FLDCW   DefaultCW;
        POP     EAX
end;

procedure DisableFPUExceptions ;
var
  FPUControlWord: WORD ;
asm
  FSTCW   FPUControlWord ;
  OR      FPUControlWord, $bf ; { Divide by zero + invalid operation }
  FNCLEX
  FLDCW   FPUControlWord ;
end ;

function ceil(f:single):single;
begin
 if frac(f)=0 then ceil:=int(f) else ceil:=int(f)+1;
end;


begin
 Set8087CW($13BF);
end.




