/* codegen.c       Generate Assembly Code for x86         24 Apr 13   */

/* Copyright (c) 2013 Gordon S. Novak Jr. and The University of Texas at Austin
 */

/* Starter file for CS 375 Code Generation assignment.           */
/* Written by Gordon S. Novak Jr.                  */

/* This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any later version.
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License (file gpl.text) for more details.
 
 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "token.h"
#include "symtab.h"
#include "genasm.h"

void genc(TOKEN code);

/* Set DEBUGGEN to 1 for debug printouts of code generation */
#define DEBUGGEN 0

int nextlabel;    /* Next available label number */
int stkframesize;   /* total stack frame size */
int registerArray[24] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};


/* Top-level entry for code generator.
 pcode    = pointer to code:  (program foo (output) (progn ...))
 varsize  = size of local storage in bytes
 maxlabel = maximum label number used so far
 
 Add this line to the end of your main program:
 gencode(parseresult, blockoffs[blocknumber], labelnumber);
 The generated code is printed out; use a text editor to extract it for
 your .s file.
 */

void gencode(TOKEN pcode, int varsize, int maxlabel)
{  TOKEN name, code;
    name = pcode->operands;
    code = name->link->link;
    nextlabel = maxlabel + 1;
    stkframesize = asmentry(name->stringval,varsize);
    genc(code);
    asmexit(name->stringval);
}

/* Trivial version: always returns RBASE + 0 */
/* Get a register.   */
/* Need a type parameter or two versions for INTEGER or REAL */
int getreg(int typeofReg)
{
    /*     ***** fix this *****   */
    if(typeofReg == 0)
    {
        //pick up the first available int reg
        int i;
        for(i = 0; i < 8; i++)
        {
            if(registerArray[i]== 0)
            {
                registerArray[i] = 1;
                return i;
            }
        }
    }
    else //real
    {
        int i;
        for(i = 8; i < sizeof(registerArray); i++)
        {
            if(registerArray[i] == 0)
            {
                registerArray[i] = 1;
                return i;
            }
        }
    }
    return RBASE;
}

/* Trivial version */
/* Generate code for arithmetic expression, return a register number */
int genarith(TOKEN code)
{
    int num, reg;
    int lhsr;
    int rhsr;
    SYMBOL sym;
    if (DEBUGGEN)
    { 
        dbugprinttok(code);
    };
    switch ( code->tokentype )
    {
        case NUMBERTOK:     //if the token is a number token
            switch (code->datatype)
        {
            case INTEGER:
                num = code->intval;
                reg = getreg(0);
                if ( num >= MINIMMEDIATE && num <= MAXIMMEDIATE )
                    asmimmed(MOVL, num, reg);
                break;
            case REAL:
                
                /*     ***** fix this *****   */
                break;
        }
            break;
        case IDENTIFIERTOK:  //if the token is an identifier
            /*     ***** fix this *****   */
            sym = searchst(code->stringval);
            num = sym->offset;
            //get a register (specifiy real or int, depending upon i
            reg = getreg(code->datatype);
            //move relative address into register
            asmld(MOVL, -num, reg,code->stringval);
            break;
        case OPERATOR:
            /*     ***** fix this *****   */
            switch(code->whichval )
            {
                case LEOP: // <=
                    lhsr = genarith(code->operands);//i
                    rhsr = genarith(code->operands->link);//32
                     asmrr(CMPL,lhsr,rhsr);  //cmpl	%ecx,%eax           	#  compare %eax - %ecx
                break;
            }
            break;
    };
    return reg;
}


/* Generate code for a Statement from an intermediate-code form */
void genc(TOKEN code)
{
    TOKEN tok, lhs, rhs;
    int reg, offs;
    SYMBOL sym;
    
    if (DEBUGGEN)
    {
        printf("genc\n");
        dbugprinttok(code);
    };
    if ( code->tokentype != OPERATOR )
    {
        printf("Bad code token");
        dbugprinttok(code);
    };
    switch ( code->whichval )
    {
        case PROGNOP:
            tok = code->operands;
            while ( tok != NULL )
            {
                genc(tok);
                tok = tok->link;
            };
            break;
        case ASSIGNOP:                   /* Trivial version: handles I := e */
            lhs = code->operands;
            rhs = lhs->link;
            reg = genarith(rhs);              /* generate rhs into a register */
            //printf("Reg is: %i \n",reg);
            sym = lhs->symentry;              /* assumes lhs is a simple var  */
            offs = sym->offset - stkframesize; /* net offset of the var   */
            switch (code->datatype)            /* store value into lhs  */
            {
            case INTEGER:
                //printf("You are in this case INTEGER \n");
                //printf("This is what offs looks like: %i \n",offs);
                asmst(MOVL, reg, offs, lhs->stringval);
                break;
                /* ...  */
            };
            break;
        case LABELOP:
            //printf("you are in labelop \n");
            lhs = code->operands;
            //printf("The intval of label is: %i \n", lhs->intval);
            asmlabel(lhs->intval);
            //||pause see pink sticky note
            break;
        case IFOP:
            lhs = code->operands; //(<= i 32)
            genarith(lhs);

            break;
    };
}







