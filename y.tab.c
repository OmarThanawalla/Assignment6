/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.3"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     IDENTIFIER = 258,
     STRING = 259,
     NUMBER = 260,
     PLUS = 261,
     MINUS = 262,
     TIMES = 263,
     DIVIDE = 264,
     ASSIGN = 265,
     EQ = 266,
     NE = 267,
     LT = 268,
     LE = 269,
     GE = 270,
     GT = 271,
     POINT = 272,
     DOT = 273,
     AND = 274,
     OR = 275,
     NOT = 276,
     DIV = 277,
     MOD = 278,
     IN = 279,
     COMMA = 280,
     SEMICOLON = 281,
     COLON = 282,
     LPAREN = 283,
     RPAREN = 284,
     LBRACKET = 285,
     RBRACKET = 286,
     DOTDOT = 287,
     ARRAY = 288,
     BEGINBEGIN = 289,
     CASE = 290,
     CONST = 291,
     DO = 292,
     DOWNTO = 293,
     ELSE = 294,
     END = 295,
     FILEFILE = 296,
     FOR = 297,
     FUNCTION = 298,
     GOTO = 299,
     IF = 300,
     LABEL = 301,
     NIL = 302,
     OF = 303,
     PACKED = 304,
     PROCEDURE = 305,
     PROGRAM = 306,
     RECORD = 307,
     REPEAT = 308,
     SET = 309,
     THEN = 310,
     TO = 311,
     TYPE = 312,
     UNTIL = 313,
     VAR = 314,
     WHILE = 315,
     WITH = 316
   };
#endif
/* Tokens.  */
#define IDENTIFIER 258
#define STRING 259
#define NUMBER 260
#define PLUS 261
#define MINUS 262
#define TIMES 263
#define DIVIDE 264
#define ASSIGN 265
#define EQ 266
#define NE 267
#define LT 268
#define LE 269
#define GE 270
#define GT 271
#define POINT 272
#define DOT 273
#define AND 274
#define OR 275
#define NOT 276
#define DIV 277
#define MOD 278
#define IN 279
#define COMMA 280
#define SEMICOLON 281
#define COLON 282
#define LPAREN 283
#define RPAREN 284
#define LBRACKET 285
#define RBRACKET 286
#define DOTDOT 287
#define ARRAY 288
#define BEGINBEGIN 289
#define CASE 290
#define CONST 291
#define DO 292
#define DOWNTO 293
#define ELSE 294
#define END 295
#define FILEFILE 296
#define FOR 297
#define FUNCTION 298
#define GOTO 299
#define IF 300
#define LABEL 301
#define NIL 302
#define OF 303
#define PACKED 304
#define PROCEDURE 305
#define PROGRAM 306
#define RECORD 307
#define REPEAT 308
#define SET 309
#define THEN 310
#define TO 311
#define TYPE 312
#define UNTIL 313
#define VAR 314
#define WHILE 315
#define WITH 316




/* Copy the first part of user declarations.  */
#line 1 "parse.y"
     

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



/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif

#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 300 "y.tab.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int i)
#else
static int
YYID (i)
    int i;
#endif
{
  return i;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  4
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   111

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  62
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  21
/* YYNRULES -- Number of rules.  */
#define YYNRULES  46
/* YYNRULES -- Number of states.  */
#define YYNSTATES  104

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   316

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint8 yyprhs[] =
{
       0,     0,     3,    12,    16,    18,    22,    24,    28,    30,
      34,    36,    40,    43,    49,    54,    58,    60,    62,    66,
      70,    72,    76,    82,    89,    96,    98,   103,   108,   112,
     114,   117,   118,   122,   126,   130,   134,   138,   140,   143,
     145,   149,   151,   155,   160,   162,   164
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      63,     0,    -1,    51,     3,    28,     3,    29,    26,    66,
      18,    -1,     3,    25,    64,    -1,     3,    -1,    79,    25,
      65,    -1,    79,    -1,    36,    69,    67,    -1,    67,    -1,
      59,    68,    73,    -1,    73,    -1,    70,    26,    68,    -1,
      70,    26,    -1,     3,    11,     5,    26,    69,    -1,     3,
      11,     5,    26,    -1,    64,    27,    71,    -1,    72,    -1,
       3,    -1,    34,    75,    76,    -1,    75,    26,    74,    -1,
      75,    -1,    34,    75,    76,    -1,    45,    79,    55,    75,
      77,    -1,    42,    78,    56,    79,    37,    75,    -1,    42,
      78,    38,    79,    37,    75,    -1,    78,    -1,     3,    28,
      65,    29,    -1,    53,    74,    58,    79,    -1,    26,    75,
      76,    -1,    40,    -1,    39,    75,    -1,    -1,     3,    10,
      79,    -1,    79,     6,    80,    -1,    79,     7,    80,    -1,
      79,     8,    80,    -1,    79,    11,    80,    -1,    80,    -1,
       7,    81,    -1,    81,    -1,    81,     8,    82,    -1,    82,
      -1,    28,    79,    29,    -1,     3,    28,    65,    29,    -1,
       3,    -1,     5,    -1,     4,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint8 yyrline[] =
{
       0,    91,    91,    93,    94,    96,    97,    99,   100,   103,
     104,   106,   107,   109,   110,   113,   115,   117,   120,   123,
     124,   127,   128,   129,   130,   131,   132,   134,   137,   138,
     140,   141,   143,   145,   146,   147,   148,   149,   151,   152,
     154,   155,   157,   158,   159,   160,   161
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "IDENTIFIER", "STRING", "NUMBER", "PLUS",
  "MINUS", "TIMES", "DIVIDE", "ASSIGN", "EQ", "NE", "LT", "LE", "GE", "GT",
  "POINT", "DOT", "AND", "OR", "NOT", "DIV", "MOD", "IN", "COMMA",
  "SEMICOLON", "COLON", "LPAREN", "RPAREN", "LBRACKET", "RBRACKET",
  "DOTDOT", "ARRAY", "BEGINBEGIN", "CASE", "CONST", "DO", "DOWNTO", "ELSE",
  "END", "FILEFILE", "FOR", "FUNCTION", "GOTO", "IF", "LABEL", "NIL", "OF",
  "PACKED", "PROCEDURE", "PROGRAM", "RECORD", "REPEAT", "SET", "THEN",
  "TO", "TYPE", "UNTIL", "VAR", "WHILE", "WITH", "$accept", "program",
  "idlist", "arglist", "cblock", "vblock", "varspecs", "congroup",
  "vargroup", "type", "simpletype", "block", "statementlist", "statement",
  "endpart", "endif", "assignment", "expr", "smplExpr", "term", "factor", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    62,    63,    64,    64,    65,    65,    66,    66,    67,
      67,    68,    68,    69,    69,    70,    71,    72,    73,    74,
      74,    75,    75,    75,    75,    75,    75,    75,    76,    76,
      77,    77,    78,    79,    79,    79,    79,    79,    80,    80,
      81,    81,    82,    82,    82,    82,    82
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     8,     3,     1,     3,     1,     3,     1,     3,
       1,     3,     2,     5,     4,     3,     1,     1,     3,     3,
       1,     3,     5,     6,     6,     1,     4,     4,     3,     1,
       2,     0,     3,     3,     3,     3,     3,     1,     2,     1,
       3,     1,     3,     4,     1,     1,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     0,     0,     0,     1,     0,     0,     0,     0,     0,
       0,     0,     0,     8,    10,     0,     0,     0,     0,     0,
       0,    25,     0,     0,     4,     0,     0,     0,     2,     0,
       0,     0,     0,     0,    44,    46,    45,     0,     0,     0,
      37,    39,    41,     0,    20,     0,    29,    18,     0,     7,
       0,     0,     9,    12,    32,     0,     6,    21,     0,     0,
       0,    38,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     3,    17,    15,    16,    11,    26,     0,
       0,     0,     0,    42,    33,    34,    35,    36,    31,    40,
      27,    19,    28,    14,     5,     0,     0,    43,     0,    22,
      13,    24,    23,    30
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,     2,    25,    55,    12,    13,    26,    23,    27,    75,
      76,    14,    43,    44,    47,    99,    21,    56,    40,    41,
      42
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -50
static const yytype_int8 yypact[] =
{
     -48,     3,    15,     0,   -50,    31,    34,    21,   -20,     1,
      37,    66,    50,   -50,   -50,    10,     1,    72,    46,     1,
      -3,   -50,    70,   -32,    58,    61,    51,    64,   -50,    46,
      46,    -3,    81,     6,    67,   -50,   -50,    28,    46,     2,
     -50,    84,   -50,    35,    68,     1,   -50,   -50,    91,   -50,
      66,    94,   -50,    66,    65,    69,    59,   -50,    46,    46,
      46,    84,    53,    46,    46,    46,    46,     1,    28,    46,
       1,    -3,    73,   -50,   -50,   -50,   -50,   -50,   -50,    46,
      11,    18,    71,   -50,   -50,   -50,   -50,   -50,    62,   -50,
      65,   -50,   -50,    37,   -50,     1,     1,   -50,     1,   -50,
     -50,   -50,   -50,   -50
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -50,   -50,    52,   -49,   -50,    80,    54,    12,   -50,   -50,
     -50,    78,    36,    -9,   -26,   -50,    92,   -17,    14,    74,
      40
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const yytype_uint8 yytable[] =
{
      20,    39,     9,     1,    15,    57,     3,    31,    63,    64,
      65,    82,    54,    66,     9,     4,    10,    63,    64,    65,
      29,    62,    66,    45,    63,    64,    65,    11,     5,    66,
      94,    34,    35,    36,     6,    16,    71,    46,    30,    11,
      22,    80,    81,    17,    58,    92,    18,     8,    95,    34,
      35,    36,    90,    37,    19,    96,    38,    67,    88,    63,
      64,    65,    59,     7,    66,    63,    64,    65,    28,    24,
      66,    63,    64,    65,    38,    32,    66,    84,    85,    86,
      87,    48,    83,    50,    79,     9,   101,   102,    51,   103,
      53,    29,    68,    69,    70,    60,    72,    74,    78,    93,
      97,    98,    73,    49,    52,   100,    91,    77,    89,    33,
       0,    61
};

static const yytype_int8 yycheck[] =
{
       9,    18,    34,    51,     3,    31,     3,    16,     6,     7,
       8,    60,    29,    11,    34,     0,    36,     6,     7,     8,
      10,    38,    11,    26,     6,     7,     8,    59,    28,    11,
      79,     3,     4,     5,     3,    34,    45,    40,    28,    59,
       3,    58,    59,    42,    38,    71,    45,    26,    37,     3,
       4,     5,    69,     7,    53,    37,    28,    55,    67,     6,
       7,     8,    56,    29,    11,     6,     7,     8,    18,     3,
      11,     6,     7,     8,    28,     3,    11,    63,    64,    65,
      66,    11,    29,    25,    25,    34,    95,    96,    27,    98,
      26,    10,     8,    58,    26,    28,     5,     3,    29,    26,
      29,    39,    50,    23,    26,    93,    70,    53,    68,    17,
      -1,    37
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    51,    63,     3,     0,    28,     3,    29,    26,    34,
      36,    59,    66,    67,    73,     3,    34,    42,    45,    53,
      75,    78,     3,    69,     3,    64,    68,    70,    18,    10,
      28,    75,     3,    78,     3,     4,     5,     7,    28,    79,
      80,    81,    82,    74,    75,    26,    40,    76,    11,    67,
      25,    27,    73,    26,    79,    65,    79,    76,    38,    56,
      28,    81,    79,     6,     7,     8,    11,    55,     8,    58,
      26,    75,     5,    64,     3,    71,    72,    68,    29,    25,
      79,    79,    65,    29,    80,    80,    80,    80,    75,    82,
      79,    74,    76,    26,    65,    37,    37,    29,    39,    77,
      69,    75,    75,    75
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *bottom, yytype_int16 *top)
#else
static void
yy_stack_print (bottom, top)
    yytype_int16 *bottom;
    yytype_int16 *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      fprintf (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      fprintf (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The look-ahead symbol.  */
int yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  int yystate;
  int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int yytoken = 0;
#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  yytype_int16 yyssa[YYINITDEPTH];
  yytype_int16 *yyss = yyssa;
  yytype_int16 *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  YYSTYPE *yyvsp;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     look-ahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to look-ahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 91 "parse.y"
    { printf("exectued program action \n"); parseresult = (yyvsp[(7) - (8)]);}
    break;

  case 3:
#line 93 "parse.y"
    { printf("executed idlist action1 \n"); (yyval) = cons((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])); }
    break;

  case 4:
#line 94 "parse.y"
    {  printf("executed idlist action2 \n"); (yyval) = cons((yyvsp[(1) - (1)]), NULL); }
    break;

  case 5:
#line 96 "parse.y"
    { printf("called arglist with multiple arguments \n"); (yyval) = cons((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])); }
    break;

  case 6:
#line 97 "parse.y"
    { printf("called arglist with no arguments \n"); (yyval) = cons((yyvsp[(1) - (1)]), NULL);}
    break;

  case 7:
#line 99 "parse.y"
    { printf("called cblock with congroup option \n"); (yyval) = (yyvsp[(3) - (3)]);}
    break;

  case 8:
#line 100 "parse.y"
    { printf("called cblock with no option \n"); (yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 9:
#line 103 "parse.y"
    { printf("executed vblock action with varspecs \n"); (yyval) = (yyvsp[(3) - (3)]); }
    break;

  case 10:
#line 104 "parse.y"
    { printf("executed vblock action no varspecs \n"); (yyval) = (yyvsp[(1) - (1)]); }
    break;

  case 11:
#line 106 "parse.y"
    { printf("executed varspecs action \n");}
    break;

  case 12:
#line 107 "parse.y"
    { printf("exectued varspecs action vargroup SEMICOLON option \n"); }
    break;

  case 13:
#line 109 "parse.y"
    { printf("executed congroup action with more congroups \n"); instconstant((yyvsp[(1) - (5)]),(yyvsp[(3) - (5)])); }
    break;

  case 14:
#line 110 "parse.y"
    { printf("executed congroup action one constant assignment \n"); instconstant((yyvsp[(1) - (4)]),(yyvsp[(3) - (4)]));}
    break;

  case 15:
#line 113 "parse.y"
    {  printf("executed vargroup action \n"); instvars((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])); }
    break;

  case 16:
#line 115 "parse.y"
    { printf("executed type action \n");  (yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 17:
#line 117 "parse.y"
    { printf("the simple type action ran \n" ); (yyval) = findtype((yyvsp[(1) - (1)])); }
    break;

  case 18:
#line 120 "parse.y"
    { printf("BLOCK action was called \n"); (yyval) = makeprogn((yyvsp[(1) - (3)]),cons((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)]))); }
    break;

  case 19:
#line 123 "parse.y"
    {printf("STATEMENTLIST multiple statements \n"); (yyval) = cons((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));}
    break;

  case 20:
#line 124 "parse.y"
    {printf("STATEMENTLIST single statements \n");  (yyval) = (yyvsp[(1) - (1)]); }
    break;

  case 21:
#line 127 "parse.y"
    { printf("you called STATEMENT action completing BEGINBEGIN... \n"); (yyval) = makeprogn((yyvsp[(1) - (3)]),cons((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)]))); }
    break;

  case 22:
#line 128 "parse.y"
    { printf("you called STATEMENT action completing IF..THEN.. \n"); (yyval) = makeif((yyvsp[(1) - (5)]), (yyvsp[(2) - (5)]), (yyvsp[(4) - (5)]), (yyvsp[(5) - (5)])); }
    break;

  case 23:
#line 129 "parse.y"
    {printf("You called STATEMENT action for loop \n");(yyval) = makefor(1,(yyvsp[(1) - (6)]),(yyvsp[(2) - (6)]),(yyvsp[(3) - (6)]),(yyvsp[(4) - (6)]),(yyvsp[(5) - (6)]),(yyvsp[(6) - (6)])) ;}
    break;

  case 24:
#line 130 "parse.y"
    {printf("You called STATEMENT action for downto loop \n"); (yyval) = makefor(-1,(yyvsp[(1) - (6)]),(yyvsp[(2) - (6)]),(yyvsp[(3) - (6)]),(yyvsp[(4) - (6)]),(yyvsp[(5) - (6)]),(yyvsp[(6) - (6)]));}
    break;

  case 25:
#line 131 "parse.y"
    { printf("you called STATEMENT action completing assignment \n");  (yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 26:
#line 132 "parse.y"
    {printf("you called STATEMENT action completing funcall \n"); (yyval) = makefuncall((yyvsp[(2) - (4)]), (yyvsp[(1) - (4)]), 
             (yyvsp[(3) - (4)]));}
    break;

  case 27:
#line 134 "parse.y"
    {printf("you called STATEMENT action completing REPEAT call \n");(yyval) = makerepeat( (yyvsp[(1) - (4)]), (yyvsp[(2) - (4)]),  (yyvsp[(3) - (4)]), (yyvsp[(4) - (4)]));}
    break;

  case 28:
#line 137 "parse.y"
    {printf("You called ENDPART action more statements \n"); (yyval) = cons((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)])); }
    break;

  case 29:
#line 138 "parse.y"
    { printf("You called ENDPART action end \n");(yyval) = NULL; }
    break;

  case 30:
#line 140 "parse.y"
    { printf("You called ENDIF action \n"); (yyval) = (yyvsp[(2) - (2)]); }
    break;

  case 31:
#line 141 "parse.y"
    { printf("You called ENDIF action \n"); (yyval) = NULL; }
    break;

  case 32:
#line 143 "parse.y"
    { printf("you called ASSIGNMENT action \n"); (yyval) = binop((yyvsp[(2) - (3)]), (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])); }
    break;

  case 33:
#line 145 "parse.y"
    { printf("you called EXPR action addition \n"); (yyval) = binop((yyvsp[(2) - (3)]), (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])); }
    break;

  case 34:
#line 146 "parse.y"
    { printf("you called EXPR action minus \n"); (yyval) = binop((yyvsp[(2) - (3)]), (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])); }
    break;

  case 35:
#line 147 "parse.y"
    { printf("you called EXPR action multiplication \n"); (yyval) = binop((yyvsp[(2) - (3)]), (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])); }
    break;

  case 36:
#line 148 "parse.y"
    {printf("you called EXPR action equality \n"); (yyval) = binop((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));}
    break;

  case 37:
#line 149 "parse.y"
    { printf("you called EXPR action smplExpr option\n");  (yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 38:
#line 151 "parse.y"
    {printf("you called smplExpr - MINUS term \n"); (yyval) = onenop((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)]));}
    break;

  case 39:
#line 152 "parse.y"
    {printf("you called smplexpr - term \n");  (yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 40:
#line 154 "parse.y"
    { printf("you called TERM action \n"); (yyval) = binop((yyvsp[(2) - (3)]), (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])); }
    break;

  case 41:
#line 155 "parse.y"
    { printf("you called TERM action factor option \n");  (yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 42:
#line 157 "parse.y"
    { (yyval) = (yyvsp[(2) - (3)]); }
    break;

  case 43:
#line 158 "parse.y"
    {printf("you called factor action completing funcall \n"); (yyval) = makefuncall((yyvsp[(2) - (4)]), (yyvsp[(1) - (4)]), (yyvsp[(3) - (4)]));}
    break;

  case 44:
#line 159 "parse.y"
    { printf("You called factor action identifier option \n"); (yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 45:
#line 160 "parse.y"
    { printf("You called factor action number option \n"); (yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 46:
#line 161 "parse.y"
    { printf("You called factor action string option \n"); (yyval) = (yyvsp[(1) - (1)]);}
    break;


/* Line 1267 of yacc.c.  */
#line 1814 "y.tab.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse look-ahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEOF && yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}


#line 164 "parse.y"


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
  if(list == NULL)
      printf("list is NULL \n");
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
    
    printf("This is what statement looks like: \n");
    ppexpr(statement);
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
    printf("This is what statementlist looks like in makerepeat function: \n");
    ppexpr(statementlist);
    
    printf("And this is what expr looks like: \n");
    ppexpr(expr);
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
    
    //return something
    return toka;
    printf("You finished calling the makerepeat function \n");
}

TOKEN makefuncall(TOKEN tok, TOKEN fn, TOKEN args)
{//tok will be recyclable
    printf("You called the makefuncall action \n");
    printf("This is what args looks like: \n");
    ppexpr(args);
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
      
      printf("this is was ppexpr for statements looks like: \n");
     if (DEBUG & DB_MAKEPROGN)
       { printf("makeprogn\n");
         dbugprinttok(tok);
         dbugprinttok(statements);
       };
      
     printf("You FINISHED calling makeProgn \n");
      ppexpr(tok);
     return tok;
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
