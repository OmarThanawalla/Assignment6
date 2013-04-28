/* genasm.h         Header for X86 code generation     ; 08 Aug 12    */

/* Copyright (c) 2012 Gordon S. Novak Jr. and The University of Texas at Austin
    */

/* 
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.

; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with this program; if not, see <http://www.gnu.org/licenses/>.
 */
/* ; 31 Jul 12; 02 Aug 12 */


/* Define register constants */

/* Registers are numbered 0-31 for general registers, 32-63 for floating */
#define RBASE 0         /* First local register to use = %eax */
#define RMAX  3         /* Last  local register to use */
#define FBASE 8         /* First F register to use = %xmm0 */
#define FMAX  23        /* Last  F register to use */

/* Define register names */

#define  EAX   0        /* General integer register */
#define  RAX   0        /* General integer register, 64-bit */
#define  ECX   1        /* General integer register */
#define  EDX   2        /* General integer register */
#define  EBX   3        /* General integer register, out of order: */
                        /* since EBX is callee-saved, we put it last.  */
#define  ESI   4        /* Source Index for string */
#define  EDI   5        /* Destination Index for string */
#define  ESP   6        /* Stack Pointer */
#define  RSP   6        /* Stack Pointer, 64-bit */
#define  EBP   7        /* Base Pointer */
#define  RBP   7        /* Base Pointer, 64-bit */
#define  XMM0  8        /* Float register: function result */

#define MINIMMEDIATE -2147483648   /* Minimum value of immediate constant */
#define MAXIMMEDIATE  2147483647   /* Maximum value of immediate constant */

/* Define symbolic constants for kind of data */

#define BYTE     0
#define HALFWORD 1
#define WORD     2
#define FLOAT    3
#define ADDR     4

#define WORDSIZE      4             /* Integer */
#define FLOATSIZE     8             /* Integer */
#define POINTERSIZE  16             /* Integer */

/* Define op code constants */

#define  JMP  0        /*  jump                  */
#define  JNE  1        /*  jump not equal        */
#define  JE   2        /*  jump equal            */
#define  JGE  3        /*  jump greater or equal */
#define  JL   4        /*  jump less             */
#define  JG   5        /*  jump greater          */
#define  JLE  6        /*  jump less or equal    */

/* Define op codes for other instructions */

#define MOVL   0      /* Move long (32 bits) */
#define MOVSD  1      /* Move double */
#define MOVQ   2      /* Move quad (64 bits) */
#define CLTQ   3      /* sign-extend eax to rax */
#define ADDL   4      /* Add integer */
#define SUBL   5      /* Subtract */
#define IMULL  6      /* Multiply */
#define DIVL    7
#define ANDL    8
#define NEGL    9
#define ORL    10    /* OR */

#define CMPL   12

#define ADDSD  13
#define SUBSD  14
#define MULSD  15
#define DIVSD  16
#define NEGSD  17
#define CMPQ   18    /* cmpq s2,s1 compares based on (s1 - s2) */
#define CMPSD  19

int roundup(int n, int m);
int asmentry(char name[], int size);
void asmlabel(int labeln);
void asmnop();
void asmcall(char name[]);
void asmjump(int code, int labeln);
void asmimmed(int inst, int ival, int dstreg);
void asmrr(int inst, int srcreg, int dstreg);
void asmrrr(int inst, int srcreg, int srcregb, int dstreg);
void asmld(int inst, int off, int reg, char str[]);
void asmst(int inst, int reg, int off, char str[]);
void asmsttemp( int reg );
void asmldtemp( int reg );
void asmldr(int inst, int offset, int reg, int dstreg, char str[]);
void asmldrr(int inst, int offset, int reg, int dstreg, char str[]);
void asmldrrm(int inst, int offset, int reg, int mult, int dstreg, char str[]);
void asmstr(int inst, int srcreg, int offset, int reg, char str[]);
void asmstrr(int inst, int srcreg, int offset, int reg, char str[]);
void asmstrrm(int inst, int srcreg, int offset, int reg, int mult, char str[]);
void asmldflit(int inst, int label, int dstreg);
void asmldilit(int inst, int label, int dstreg);
void asmlitarg(int labeln, int dstreg);
void asmlshift(int srcreg, int n, int dstreg);
void asmfloat(int reg, int freg);
int lefth(double d); /* Linux note: you may need to switch -- see genasm.c */
int righth(double d);
void asmexit(char name[]);
void makeilit(int n, int labeln);
void makeflit(float f, int labeln);
void makeblit(char s[], int labeln);
void outlits();