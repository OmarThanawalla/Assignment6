/*  pprint.c                Gordon S. Novak Jr.          ; 07 Aug 12  */

/*  Pretty-print a token expression tree in Lisp-like prefix form    */

/* Copyright (c) 2012 Gordon S. Novak Jr. and
   The University of Texas at Austin. */

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

/*  For PROGRAM, the code should look like:
     PROGRAM
       /
     GRAPH1---->PROGN---->code
                  /
               OUTPUT                  */

/* 09 Feb 01  */

#include <ctype.h>
#include <stdio.h>
#include "token.h"

#define PRINTEXPRDEBUG 0     /* Set to 1 to print each node in printexpr */

char* opprint[]  = {" ", "+", "-", "*", "/", ":=", "=", "<>", "<", "<=",
                      ">=", ">",  "^", ".", "and", "or", "not", "div", "mod",
                      "in", "if", "goto", "progn", "label", "funcall",
                      "aref", "program", "float", "fix"};
int opsize[] = {1, 1, 1, 1, 1, 2, 1, 2, 1, 2,
                  2, 1, 1, 1, 3, 2, 3, 3, 3,
                  2, 2, 4, 5, 5, 7,
                  4, 7, 5, 3 };

void debugprinttok(TOKEN tok)           /* print a token for debugging */
  { if (tok == NULL)
       printf(" token NULL%ld\n", (long)tok);
     else printf(
      " token %ld  typ %2d  whic %3d  dty %3d  sty %ld lnk %ld  opnds %ld\n",
      (long)tok, tok->tokentype, tok->whichval, tok->datatype,
      (long)tok->symtype, (long)tok->link, (long)tok->operands);
  }

int strlength(char str[])           /* find length of a string */
  {  int i, n;
     n = 16;
     for (i = 0; i < 16; i++)
         if ( str[i] == '\0' && n == 16 ) n = i;
     return n;
   }

void printtok(TOKEN tok)             /* print a token in abbreviated form */
  { switch (tok->tokentype)
	{case IDENTIFIERTOK:
           printf ("%s", tok->stringval);
           break;
         case STRINGTOK:
           printf ("'%s'", tok->stringval);
           break;
         case NUMBERTOK:
           switch (tok->datatype)
             {case INTEGER: case POINTER:
                printf ("%d", tok->intval);
                break;
	      case REAL:
                printf ("%e", tok->realval);
                break; }
	 case DELIMITER: case RESERVED: case OPERATOR:
	   break;
	 }
  }

void dbugprinttok(TOKEN tok)  /* print a token in 'nice' debugging form */
  { if (tok == NULL)
       printf(" token %ld  NULL\n", (long)tok);
       else switch (tok->tokentype)
	     { case IDENTIFIERTOK:
	              printf(" token %ld  ID  %12s  dtype %2d  link %ld\n",
                             (long)tok, tok->stringval, tok->datatype,
                             (long)tok->link);
		      break;
	       case STRINGTOK:
	              printf(" token %ld  STR %12s  dtype %2d  link %ld\n",
                     (long)tok, tok->stringval, tok->datatype, (long)tok->link);
		      break;
	       case NUMBERTOK:
		 switch (tok->datatype)
		   {case INTEGER: case POINTER:
		      printf(" token %ld  NUM %12d  dtype %2d  link %ld\n",
                      (long)tok, tok->intval, tok->datatype, (long)tok->link);
		      break;
		    case REAL:
		      printf(" token %ld  NUM %12e  dtype %2d  link %ld\n",
                      (long)tok, tok->realval, tok->datatype, (long)tok->link);
		      break; };
                      break;
		    case OPERATOR:
	     printf(" token %ld  OP  %12s  dtype %2d  link %ld  operands %ld\n",
                      (long)tok, opprint[tok->whichval], tok->datatype,
                      (long)tok->link, (long)tok->operands);
		      break;
		    case DELIMITER: case RESERVED:
		      debugprinttok(tok);
		      break;
	 }
  }

void printexpr(TOKEN tok, int col)     /* print an expression in prefix form */
  { TOKEN opnds; int nextcol, start, i;
    if (PRINTEXPRDEBUG != 0)
      { printf ("printexpr: col %d\n", col);
        dbugprinttok(tok);
      };
    if (tok->tokentype == OPERATOR)
      { printf ("(%s", opprint[tok->whichval]);
        nextcol = col + 2 + opsize[tok->whichval];
        opnds = tok->operands;
	start = 0;
	while (opnds != NULL)
	  { if (start == 0) 
	       printf(" ");
	       else { printf("\n");
		      for (i = 0; i < nextcol; i++) printf(" ");
		    }
	    printexpr(opnds, nextcol);
	    if ( opnds->tokentype == IDENTIFIERTOK && nextcol < 60 )
	       nextcol = nextcol + 1 + strlength(opnds->stringval);
	       else start = 1;
	    opnds = opnds->link;
	  }
        printf (")");
      }
      else printtok(tok);
  }

void ppexpr(TOKEN tok)              /* print an expression in prefix form */
  { if ( (long) tok <= 0 )
      { printf("ppexpr called with bad pointer %ld\n", (long)tok);
	return; };
    printexpr(tok, 0);
    printf("\n");
  }

void dbugplist(TOKEN tok)           /* print a list of tokens for debugging */
  { while (tok != NULL)
      { dbugprinttok(tok);
        tok = tok->link;
      };
  }

void dbugbprinttok(TOKEN tok)    /* print rest of token for debugging */
  { if (tok != NULL)
      printf("  toktyp %6d  which  %6d  symtyp %ld  syment %ld  opnds %ld\n",
	     tok->tokentype, tok->whichval, (long)tok->symtype,
             (long)tok->symentry, (long)tok->operands);
  }

void dbugprintexpr(TOKEN tok) /* print an expression in 'nice' debugging form */
  { TOKEN opnds;
    dbugprinttok(tok);
    if (tok->tokentype == OPERATOR)
      { opnds = tok->operands;
	while (opnds != NULL)
	      { dbugprintexpr(opnds);
		opnds = opnds->link;
	      }
      }
  }