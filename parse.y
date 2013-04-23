%{     

//Omar Thanawalla
//UT EID: ort76
//CS account ID: omart




/* parse.y    Pascal Parser      Gordon S. Novak Jr.  ; 02 Aug 12   */

/* Copyright (c) 2012 Gordon S. Novak Jr. and
   The University of Texas at Austin. */

/* 14 Feb 01; 01 Oct 04; 02 Mar 07; 27 Feb 08; 24 Jul 09 */

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


/* NOTE:   Copy your lexan.l lexical analyzer to this directory.      */

       /* To use:
                     make pars1y              has 1 shift/reduce conflict
                     pars1y                   execute the parser
                     i:=j .
                     ^D                       control-D to end input

                     pars1y                   execute the parser
                     begin i:=j; if i+j then x:=a+b*c else x:=a*b+c; k:=i end.
                     ^D

                     pars1y                   execute the parser
                     if x+y then if y+z then i:=j else k:=2.
                     ^D

           You may copy pars1.y to be parse.y and extend it for your
           assignment.  Then use   make parser   as above.
        */

        /* Yacc reports 1 shift/reduce conflict, due to the ELSE part of
           the IF statement, but Yacc's default resolves it in the right way.*/

#include <stdio.h>
#include <ctype.h>
#include "token.h"
#include "lexan.h"
#include "symtab.h"
#include "parse.h"
#include <string.h>

        /* define the type of the Yacc stack element to be TOKEN */

#define YYSTYPE TOKEN

TOKEN parseresult;
/* --------------------------------------------DONT FORGET TO CHANGE LEXAN.L WITH THE OTHER LEXAN.L FROM ASSIGNMENT 2*/

%}

/* Order of tokens corresponds to tokendefs.c; do not change */

%token IDENTIFIER STRING NUMBER   /* token types */

%token PLUS MINUS TIMES DIVIDE    /* Operators */
%token ASSIGN EQ NE LT LE GE GT POINT DOT AND OR NOT DIV MOD IN

%token COMMA                      /* Delimiters */
%token SEMICOLON COLON LPAREN RPAREN LBRACKET RBRACKET DOTDOT

%token ARRAY BEGINBEGIN           /* Lex uses BEGIN */
%token CASE CONST DO DOWNTO ELSE END FILEFILE FOR FUNCTION GOTO IF LABEL NIL
%token OF PACKED PROCEDURE PROGRAM RECORD REPEAT SET THEN TO TYPE UNTIL
%token VAR WHILE WITH


%%

  program    : PROGRAM IDENTIFIER LPAREN IDENTIFIER RPAREN SEMICOLON cblock DOT { printf("exectued program action \n"); parseresult = $7;}
             ;
  idlist     : IDENTIFIER COMMA idlist			{ printf("executed idlist action1 \n"); $$ = cons($1, $3); }
	 		 | IDENTIFIER 						{  printf("executed idlist action2 \n"); $$ = cons($1, NULL); }
			 ;           
  arglist    : expr COMMA arglist         { printf("called arglist with multiple arguments \n"); $$ = cons($1, $3); }
             | expr                       { printf("called arglist with no arguments \n"); $$ = cons($1, NULL);}
             ;
  cblock     : CONST congroup vblock            { printf("called cblock with congroup option \n"); $$ = $3;}       
             | vblock                           { printf("called cblock with no option \n"); $$ = $1;} 
             ;

  vblock 	 :   VAR varspecs block 			{ printf("executed vblock action with varspecs \n"); $$ = $3; }
  		     |   block                          { printf("executed vblock action no varspecs \n"); $$ = $1; }
			 ;           
  varspecs   : vargroup SEMICOLON varspecs		{ printf("executed varspecs action \n");}
             | vargroup SEMICOLON               { printf("exectued varspecs action vargroup SEMICOLON option \n"); }
             ;
congroup     : IDENTIFIER EQ NUMBER SEMICOLON congroup { printf("executed congroup action with more congroups \n"); instconstant($1,$3); }
             | IDENTIFIER EQ NUMBER SEMICOLON          { printf("executed congroup action one constant assignment \n"); instconstant($1,$3);}
             ;

  vargroup   : idlist COLON type				{  printf("executed vargroup action \n"); instvars($1, $3); }
			 ;
  type 		 : simpletype						 { printf("executed type action \n");  $$ = $1;}
			 ;
  simpletype : IDENTIFIER 						{ printf("the simple type action ran \n" ); $$ = findtype($1); }
			 ;			 

  block      :  BEGINBEGIN statementlist endpart     { printf("BLOCK action was called \n"); $$ = makeprogn($1,cons($2, $3)); }
             ;

  statementlist: statement SEMICOLON statementlist {printf("STATEMENTLIST multiple statements \n"); $$ = cons($1,$3);}
             |   statement                         {printf("STATEMENTLIST single statements \n");  $$ = $1; }
             ;

  statement  :  BEGINBEGIN statementlist endpart   { printf("you called STATEMENT action completing BEGINBEGIN... \n"); $$ = makeprogn($1,cons($2, $3)); }
             |  IF expr THEN statementlist endif   { printf("you called STATEMENT action completing IF..THEN.. \n"); $$ = makeif($1, $2, $4, $5); }
             |  FOR assignment TO expr DO statementlist {printf("You called STATEMENT action for loop \n");$$ = makefor(1,$1,$2,$3,$4,$5,$6) ;}
             |  FOR assignment DOWNTO expr DO statementlist {printf("You called STATEMENT action for downto loop \n"); $$ = makefor(-1,$1,$2,$3,$4,$5,$6);}
             |  assignment                     { printf("you called STATEMENT action completing assignment \n");  $$ = $1;}
             |  IDENTIFIER LPAREN arglist RPAREN {printf("you called STATEMENT action completing funcall \n"); $$ = makefuncall($2, $1, 
             $3);}
             |  REPEAT statementlist UNTIL expr {printf("you called STATEMENT action completing REPEAT call \n");$$ = makerepeat( $1, $2,  $3, $4);}
             ;

  endpart    :  SEMICOLON statementlist endpart    {printf("You called ENDPART action \n"); $$ = cons($2, $3); }
             |  END                            { printf("You called ENDPART action \n");$$ = NULL; }
             ;
  endif      :  ELSE statementlist                 { printf("You called ENDIF action \n"); $$ = $2; }
             |  /* empty */                    { printf("You called ENDIF action \n"); $$ = NULL; }
             ;
  assignment :  IDENTIFIER ASSIGN expr         { printf("you called ASSIGNMENT action \n"); $$ = binop($2, $1, $3); }
             ;
  expr       :  expr PLUS smplExpr                 { printf("you called EXPR action addition \n"); $$ = binop($2, $1, $3); }
             |  expr TIMES smplExpr                { printf("you called EXPR action multiplication \n"); $$ = binop($2, $1, $3); }
             |  expr EQ smplExpr                  {printf("you called EXPR action equality \n"); $$ = binop($2,$1,$3);}
             |  smplExpr                           { printf("you called EXPR action term option\n");  $$ = $1;}
             ;
  smplExpr   :  MINUS term                         {printf("you called smplExpr - MINUS term \n"); $$ = onenop($1,$2);}
             |  term                               {printf("you called smplexpr - term \n");  $$ = $1;}
             ;
  term       :  term TIMES factor              { printf("you called TERM action \n"); $$ = binop($2, $1, $3); }
             |  factor                          { printf("you called TERM action factor option \n");  $$ = $1;}
             ;
  factor     :  LPAREN expr RPAREN             { $$ = $2; }
             |  IDENTIFIER LPAREN arglist RPAREN {printf("you called factor action completing funcall \n"); $$ = makefuncall($2, $1, $3);}
             |  IDENTIFIER                      { printf("You called factor action identifier option \n"); $$ = $1;}
             |  NUMBER                          { printf("You called factor action number option \n"); $$ = $1;}
             |  STRING                          { printf("You called factor action string option \n"); $$ = $1;}
             ;

%%

/* You should add your own debugging flags below, and add debugging
   printouts to your programs.

   You will want to change DEBUG to turn off printouts once things
   are working.
  */

#define DEBUG        31             /* set bits here for debugging, 0 = off  */
#define DB_CONS       1             /* bit to trace cons */
#define DB_BINOP      2             /* bit to trace binop */
#define DB_MAKEIF     4             /* bit to trace makeif */
#define DB_MAKEPROGN  8             /* bit to trace makeprogn */
#define DB_PARSERES  16             /* bit to trace parseresult */

#define WORDSIZE   4             /* alignment size in address units (bytes)  */
#define ALIGNSIZE 16             /* record/array alignment size    */

 int labelnumber = 0;  /* sequential counter for internal label numbers */

   /*  Note: you should add to the above values and insert debugging
       printouts in your routines similar to those that are shown here.     */

TOKEN cons(TOKEN item, TOKEN list)           /* add item to front of list */
  { 
  printf("you called cons method \n");
  //pretty print item and pretty print list
      printf("Here is ppexpr of item: \n");
  ppexpr(item);
      printf("Here is ppexpr of list: \n");
  ppexpr(list);
      
  item->link = list;
//    if (DEBUG & DB_CONS)
//       { printf("cons\n");
//         dbugprinttok(item);
//         dbugprinttok(list);
//       };
      printf("Here is ppexpr of item after linking: \n");
 ppexpr(item);
  printf("you finished calling the cons method \n");
    return item;
  }
  
  								/* install variables in symbol table */
void instvars(TOKEN idlist, TOKEN typetok)
{ 
        printf("You called instvars \n");
    
		SYMBOL sym, typesym; int align;
        printf("You called instvars2 \n");
    
		typesym = typetok->symtype;
        printf("You called instvars3 \n");
    
		align = (typesym->size > WORDSIZE) ?
		ALIGNSIZE : typesym->size ;
        printf("You called instvars4 \n");
    
		while ( idlist != NULL ) /* for each id */
		{ 
			sym = insertsym(idlist->stringval);
			sym->kind = VARSYM;
			sym->offset = wordaddress(blockoffs[blocknumber],align);
			sym->size = typesym->size;
			blockoffs[blocknumber] = sym->offset + sym->size;
			sym->datatype = typesym;
			sym->basicdt = typesym->basicdt;
            
			idlist = idlist->link;
		};
        printf("You FINISHED calling instvars \n");
}

void instconstant(TOKEN id, TOKEN constant)
{
    printf("You called instconstant \n");
    
    SYMBOL sym;
    sym = insertsym(id->stringval);
    //setup kind
    sym->kind = CONSTSYM;
    //set up the basicdt (INTEGER REAL etc)
    sym->basicdt = constant->datatype;
    
    //set up the size and actual value
    if(sym->basicdt == 1) //real
    {
        sym->constval.realnum = constant->realval;
        sym->size = 8;
    }
    if(sym->basicdt == 0) //int
    {
        sym->constval.intnum = constant->intval;
        sym->size = 4;
    }
    
    //sym->size = basicsizes[constant->datatype];
    
    printf("You finished calling instconstant \n");
}

TOKEN findtype(TOKEN tok)
{
    printf("You are CALLING the findtype method \n");
    //printst();   
    printf("The token you are looking at is: %s \n ",tok->stringval);
    SYMBOL sym, typ;        /* symbol table entry and its type */
     sym = searchst(tok->stringval);     /* look up the name in the table */
     printf("This is the basicdt of the symbol: %i \n",sym->basicdt);
        
     //tok->symentry = sym;                /* point token to its sym entry  */
    //printf("This is what symentry looks like: %")
     //typ = sym->datatype;                /* get the type of the symbol    */

     
     if(sym->kind == BASICTYPE ) //then the token is itself a basic datatype (Integer, Real, String, Bool)
     {
        printf("typ is null \n");
        tok->symtype = sym; 
        tok->datatype = sym->basicdt; 
          
     }
    else if(sym->kind == TYPESYM)
    {
        printf("TYPESYM is called: %c \n",sym->kind);
        tok->symtype = sym->datatype;
        
    }
    else //error
    {
        printf("Your findtype recieved bad type \n");
    }

	 printf("You FINISHED calling the findtype method in parse.y \n");
     return tok;
}


TOKEN binop(TOKEN op, TOKEN lhs, TOKEN rhs)        /* reduce binary operator */
  {
    printf("You called binop function \n");
    op->operands = lhs;          /* link operands to operator       */
    lhs->link = rhs;             /* link second operand to first    */
    rhs->link = NULL;            /* terminate operand list          */
    if (DEBUG & DB_BINOP)
       { printf("binop\n");
         dbugprinttok(op);
         dbugprinttok(lhs);
         dbugprinttok(rhs);
       };
      printf("You finished calling binop function \n");
    return op;
  }


TOKEN onenop(TOKEN op, TOKEN lhs)        /* reduce binary operator */
{
  printf("You called oneop function \n");
  op->operands = lhs;          /* link operands to operator       */
  lhs->link = NULL;             /* link second operand to first    */
  if (DEBUG & DB_BINOP)
  { printf("onenop: \n");
      dbugprinttok(op);
      dbugprinttok(lhs);
  };
  printf("You finished calling oneop function \n");
  return op;
}

TOKEN makeif(TOKEN tok, TOKEN exp, TOKEN thenpart, TOKEN elsepart)
  {  tok->tokentype = OPERATOR;  /* Make it look like an operator   */
     tok->whichval = IFOP;
     if (elsepart != NULL) elsepart->link = NULL;
     thenpart->link = elsepart;
     exp->link = thenpart;
     tok->operands = exp;
     if (DEBUG & DB_MAKEIF)
        { printf("makeif\n");
          dbugprinttok(tok);
          dbugprinttok(exp);
          dbugprinttok(thenpart);
          dbugprinttok(elsepart);
        };
     return tok;
   }
/* makefor makes structures for a for statement.
                  sign is 1 for normal loop, -1 for downto.
                  asg is an assignment statement, e.g. (:= i 1)
                  endexpr is the end expression
                  tok, tokb and tokc are (now) unused tokens that are recycled. */
TOKEN makefor(int sign, TOKEN tok, TOKEN asg, TOKEN tokb, TOKEN endexpr, TOKEN tokc, TOKEN statement)
{
    printf("You called the makefor function \n");
    //what am i suppose to in this function
    //create progn operator token
    
    //Set up progn
    tok->tokentype = OPERATOR;
    tok->whichval = PROGNOP;
    printf("setted up tok \n");
    //progn link to asg
    tok->operands = asg;
    printf("tok operanded \n");
    
    //setup tokb to be label
    tokb->tokentype = OPERATOR;
    tokb->whichval = LABELOP;
    printf("setted up tokb \n");
    
    //link asg to label
    asg->link=tokb;
    
    
    //create integer token
    TOKEN tokz = talloc();
    tokz->tokentype = NUMBERTOK;
    tokz->datatype = INTEGER;
    int lbl = labelnumber++;
    tokz->intval = lbl; // 0 in the diagram
    printf("setted up tokz \n");
    
    //link tokb to tokz (integer token)
    tokb->operands = tokz;
    
    //create ifop token
    tokc->tokentype = OPERATOR;
    tokc->whichval = IFOP;
    printf("setted up tokc \n");
    
    //link tokb to tokc (label to if)
    tokb->link = tokc;
    
    //create <= tok
    TOKEN tokd = talloc();
    tokd->tokentype = OPERATOR;
    tokd->whichval = LEOP;
    printf("setted up tokd \n");
    
    //link ifop (tokc) to <= tok 
    tokc->operands = tokd;
    
    //tokd->link =
    //tokd->operands =
    
    //create 'i' token - which is copying token i from asg tok
    TOKEN tokf = talloc();
    tokf->tokentype = asg->operands->tokentype;
    printf("the value for asg operands token type is: %s \n",asg->operands->stringval);
    strcpy (tokf->stringval,asg->operands->stringval);
    //tokf->stringval = asg->operands->stringval;
    //connect tokf to endexpr
    tokf->link = endexpr;
    
    //connect <= operator to i
    tokd->operands = tokf;
    
    //Set up progn
    TOKEN toke = talloc();
    toke->tokentype = OPERATOR;
    toke->whichval = PROGNOP;
    printf("setted up toke \n");
    
    //link tokd with progn (less than equal to, to , progn)
    tokd->link = toke;
    
    //progn link to statement
    toke->operands = statement;
    printf("tok operanded \n");
    
    //statement link to :=
    TOKEN tokg = talloc();
    tokg->tokentype = OPERATOR;
    tokg->whichval = ASSIGNOP;
    statement->link = tokg;
    printf("statement linked to := \n");
    
    //:= to i
    //create 'i' token - which is copying token i from asg tok
    TOKEN tokh = talloc();
    tokh->tokentype = asg->operands->tokentype;
    
    strcpy (tokh->stringval,asg->operands->stringval);
    //link tokg(:=) to identiifer token i
    tokg->operands = tokh;
    printf("operanded := with i \n");
    
    //i to +
    TOKEN toki = talloc();
    toki->tokentype = OPERATOR;
    toki->whichval = PLUSOP;
    tokh->link = toki;
    printf("linked i with + \n");
    
    //+ to i
    TOKEN tokj = talloc();
    tokj->tokentype = asg->operands->tokentype;
    strcpy (tokj->stringval,asg->operands->stringval);
    toki->operands = tokj;
    printf("operaended + with 1 \n");
    
    //i to 1
    TOKEN tokl = talloc();
    tokl->tokentype = NUMBERTOK;
    tokl->datatype = INTEGER;
    tokl->intval = 1;
    tokj->link = tokl;
    printf("linked i and 1 \n");
    
    //tokg link to goto
    TOKEN tokq = talloc();
    tokq->tokentype = OPERATOR;
    tokq->whichval = GOTOOP;
    tokg->link = tokq;
    printf("linked := with goto \n");
    
    //goto operand to 0 (lbl)
    TOKEN tokm = talloc();
    tokm->tokentype = NUMBERTOK;
    tokm->datatype = INTEGER;
    tokm->intval = lbl;
    printf("operanded goto and labelnumber \n");
    
    tokq->operands = tokm;
    
    if (DEBUG)
    { printf("makefor\n");
        dbugprinttok(tok);
        dbugprinttok(asg);
        dbugprinttok(tokb);
        dbugprinttok(endexpr);
        dbugprinttok(tokc);
        dbugprinttok(statement);
    };
    printf("the following shows what tok looks like in makefor function using pretty print function \n");
    
    ppexpr(tok); 
    printf("\n ppexpr just ran \n");
    printf("You finished calling the makefor function \n");
    return tok;
}

TOKEN makerepeat(TOKEN tok, TOKEN statementlist, TOKEN tokx, TOKEN expr)
{
    printf("You called the MAKEREPEAT FUNCTION \n");
    //set up progn
    TOKEN toka = talloc();
    toka->tokentype = OPERATOR;
    toka->whichval = PROGNOP;
    
    //setup tokb to be label
    TOKEN tokb = talloc();
    tokb->tokentype = OPERATOR;
    tokb->whichval = LABELOP;
    
    //create integer token
    TOKEN tokc = talloc();
    tokc->tokentype = NUMBERTOK;
    tokc->datatype = INTEGER;
    int lbl = labelnumber++;
    tokc->intval = lbl; // 0 in the diagram
    
    //perform the operand of a b and c
    toka->operands = tokb;
    tokb->operands = tokc;
    
    toka->link= statementlist;
    
    
    //create ifop
    TOKEN tokd = talloc();
    tokd->tokentype = OPERATOR;
    tokd->whichval = IFOP;
    
    //statementlist to if
    statementlist->link = tokd;
    
    //if to expr
    tokd->operands = expr;
    
    //create another progn
    //set up progn
    TOKEN toke = talloc();
    toke->tokentype = OPERATOR;
    toke->whichval = PROGNOP;
    
    //expr to progn
    expr->link = toke;
    
    //create goto
    TOKEN tokq = talloc();
    tokq->tokentype = OPERATOR;
    tokq->whichval = GOTOOP;
    
    //lionk progn with goto
    toke->link = tokq;
    
    //create int
    //create integer token
    TOKEN tokr = talloc();
    tokr->tokentype = NUMBERTOK;
    tokr->datatype = INTEGER;
    tokr->intval = lbl; // 0 in the diagram
    
    //link goto with label value
    tokq->operands = tokr;
    
    
    printf("You finished calling the makerepeat function \n");
}

TOKEN makefuncall(TOKEN tok, TOKEN fn, TOKEN args)
{//tok will be recyclable
    printf("You called the makefuncall action \n");
    tok->tokentype = OPERATOR;
    tok->whichval = FUNCALLOP;
    
    //link fn to args
    fn->link = args;
    //link tok to fn
    tok->operands
    
      = fn;

    printf("You finished calling the makefuncall action \n");
    return tok;
}
                 
TOKEN makeprogn(TOKEN tok, TOKEN statements)
  { 
    printf("You called makeProgn \n");
    tok->tokentype = OPERATOR;
     tok->whichval = PROGNOP;
     tok->operands = statements;
     if (DEBUG & DB_MAKEPROGN)
       { printf("makeprogn\n");
         dbugprinttok(tok);
         dbugprinttok(statements);
       };
     return tok;
    printf("You FINISHED calling makeProgn \n");
   }

int wordaddress(int n, int wordsize)
  { return ((n + wordsize - 1) / wordsize) * wordsize; }
 
yyerror(s)
  char * s;
  { 
  fputs(s,stderr); putc('\n',stderr);
  }

main()
  { 
    printf("You are calling MAIN \n");
    int res;
    initsyms();
    res = yyparse();
    printst();
    printf("yyparse result1 = %8d\n", res);
    if (DEBUG & DB_PARSERES) dbugprinttok(parseresult);
    ppexpr(parseresult);           /* Pretty-print the result tree */
    printf("You are finished calling MAIN \n");
  }