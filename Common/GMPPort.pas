//================================================================
// Simple port of GMP library for Delphi
//
// Copyright (C) 2007 by Roman Lut under LGPL
//================================================================


unit GMPPort;

interface
uses Windows;

type mpf_t = pointer;

procedure MPF_set_default_prec(prec : DWORD); cdecl; external 'GMPPort.dll';

function MPF_Create(): mpf_t; cdecl; external 'GMPPort.dll';

procedure MPF_Delete(p: mpf_t); cdecl; external 'GMPPort.dll';

function MPF_set_str(rop: mpf_t; str: pchar): boolean; cdecl; external 'GMPPort.dll';

procedure MPF_add(rop: mpf_t; op1: mpf_t; op2: mpf_t); cdecl; external 'GMPPort.dll';

procedure MPF_add_ui(rop: mpf_t; op1: mpf_t; op2: DWORD); cdecl; external 'GMPPort.dll';

procedure MPF_sub(rop: mpf_t; op1: mpf_t; op2: mpf_t); cdecl; external 'GMPPort.dll';

procedure MPF_ui_sub(rop: mpf_t; op1 : DWORD; op2: mpf_t); cdecl; external 'GMPPort.dll';

procedure MPF_sub_ui(rop: mpf_t; op1: mpf_t; op2: DWORD); cdecl; external 'GMPPort.dll';

procedure MPF_mul(rop: mpf_t; op1: mpf_t; op2: mpf_t); cdecl; external 'GMPPort.dll';

procedure MPF_mul_ui(rop: mpf_t; op1: mpf_t; op2: DWORD); cdecl; external 'GMPPort.dll';

procedure MPF_div(rop: mpf_t; op1: mpf_t; op2: mpf_t); cdecl; external 'GMPPort.dll';

procedure MPF_ui_div(rop: mpf_t;  op1: DWORD; op2: mpf_t); cdecl; external 'GMPPort.dll';

procedure MPF_div_ui(rop: mpf_t; op1: mpf_t; op2: DWORD); cdecl; external 'GMPPort.dll';

procedure MPF_sqrt(rop: mpf_t; op: DWORD); cdecl; external 'GMPPort.dll';

procedure MPF_sqrt_ui(rop: mpf_t; op: DWORD); cdecl; external 'GMPPort.dll';

procedure MPF_pow_ui(rop: mpf_t; op1: mpf_t; op2: DWORD); cdecl; external 'GMPPort.dll';

procedure MPF_neg(rop: mpf_t; op: mpf_t); cdecl; external 'GMPPort.dll';

procedure MPF_abs(rop: mpf_t; op: mpf_t); cdecl; external 'GMPPort.dll';

function MPF_cmp(op1 : mpf_t; op2: mpf_t): integer; cdecl; external 'GMPPort.dll';

function MPF_cmp_d(op1: mpf_t; op2: double): integer; cdecl; external 'GMPPort.dll';

function  MPF_cmp_ui(op1: mpf_t; op2: DWORD): integer; cdecl; external 'GMPPort.dll';

function  MPF_cmp_si(op1: mpf_t; op2: integer): integer; cdecl; external 'GMPPort.dll';

procedure MPF_get_str(buf: pchar; bufsize: DWORD; op: mpf_t); cdecl; external 'GMPPort.dll';

function MPF_get_String(op: mpf_t): string;

procedure MPF_set(rop : mpf_t; op : mpf_t); cdecl; external 'GMPPort.dll';

procedure MPF_set_d(rop : mpf_t; op : double); cdecl; external 'GMPPort.dll';



implementation

function MPF_get_String(op: mpf_t): string;
var
 buf: array [0..2048] of char;
begin
 mpf_get_str(buf,2049,op);
 buf[2048]:=#0;
 result:=buf;
end;

end.
