/* parse.h     Gordon S. Novak Jr.    */
/* 16 Apr 04; 23 Feb 05; 17 Nov 05; 18 Apr 06; 26 Jul 12 */

/* You may use the function headers below if you wish, or you may
   replace them if you wish.  */

//added some new functions:
/*onenop does what binop does but with only two args */
TOKEN onenop(TOKEN op, TOKEN lhs);
/* instcontstant adds constants into the symbol table */
void instconstant(TOKEN id, TOKEN constant);


/* cons links a new item onto the front of a list. */
TOKEN cons(TOKEN item, TOKEN list);
/* nconc concatenates two lists, destructively, by making the last link
   of lista point to listb. */
TOKEN nconc(TOKEN lista, TOKEN listb);
/* binop links a binary operator op to two operands, lhs and rhs. */
TOKEN binop(TOKEN op, TOKEN lhs, TOKEN rhs);
/* unaryop links a unary operator op to one operand, lhs */
TOKEN unaryop(TOKEN op, TOKEN lhs);
/* makeop makes a new operator token with operator number opnum. */
TOKEN makeop(int opnum);
/* makefloat forces the item tok to be floating, by floating a constant
   or by inserting a FLOATOP operator */
TOKEN makefloat(TOKEN tok);
/* makefix forces the item tok to be integer, by truncating a constant
   or by inserting a FIXOP operator */
TOKEN makefix(TOKEN tok);
/* fillintc makes tok into an integer constant with num as its value */
TOKEN fillintc(TOKEN tok, int num);
/* makeintc makes a new token with num as its value */
TOKEN makeintc(int num);
/* copytok makes a new token that is a copy of origtok */
TOKEN copytok(TOKEN origtok);
/* makeif makes an IF operator and links it to its arguments.
   tok is a (now) unused token that is recycled to become an IFOP operator */
TOKEN makeif(TOKEN tok, TOKEN exp, TOKEN thenpart, TOKEN elsepart);
/* makeprogn makes a PROGN operator and links it to the list of statements.
   tok is a (now) unused token that is recycled. */
TOKEN makeprogn(TOKEN tok, TOKEN statements);
/* makepnb is like makeprogn, except that if statements is already a progn,
   it just returns statements.  This is optional. */
TOKEN makepnb(TOKEN tok, TOKEN statements);
/* appendst makes a progn containing statements followed by more */
TOKEN appendst(TOKEN statements, TOKEN more);
/* dogoto is the action for a goto statement.
   tok is a (now) unused token that is recycled. */
TOKEN dogoto(TOKEN tok, TOKEN labeltok);
/* makelabel makes a new label, using labelnumber++ */
TOKEN makelabel();
/* dolabel processes a label of the form   <number>: <statement>
   tok is a (now) unused token that is recycled. */
TOKEN dolabel(TOKEN labeltok, TOKEN tok, TOKEN statement);
/* instlabel installs a user label in the label table */
void  instlabel (TOKEN num);
/* makegoto makes a GOTO operator to go to the specified label.
   The label is put into a number token. */
TOKEN makegoto(int label);
/* settoktype sets up the type fields of token tok */
void  settoktype(TOKEN tok, SYMBOL typ, SYMBOL ent);
/* makefuncall makes a FUNCALL operator and links it to the fn and args.
   tok is a (now) unused token that is recycled. */
TOKEN makefuncall(TOKEN tok, TOKEN fn, TOKEN args);
/* makeprogram makes the tree structures for the top-level program */
TOKEN makeprogram(TOKEN name, TOKEN args, TOKEN statements);
/* makewhile makes structures for a while statement.
   tok and tokb are (now) unused tokens that are recycled. */
TOKEN makewhile(TOKEN tok, TOKEN expr, TOKEN tokb, TOKEN statement);
/* makerepeat makes structures for a repeat statement.
   tok and tokb are (now) unused tokens that are recycled. */
TOKEN makerepeat(TOKEN tok, TOKEN statements, TOKEN tokb, TOKEN expr);
/* makefor makes structures for a for statement.
   sign is 1 for normal loop, -1 for downto.
   asg is an assignment statement, e.g. (:= i 1)
   endexpr is the end expression
   tok, tokb and tokc are (now) unused tokens that are recycled. */
TOKEN makefor(int sign, TOKEN tok, TOKEN asg, TOKEN tokb, TOKEN endexpr,TOKEN tokc, TOKEN statement);
/* findid finds an identifier in the symbol table, sets up symbol table
   pointers, changes constant to number */
TOKEN findid(TOKEN tok);
/* instconst installs a constant in the symbol table */
void  instconst(TOKEN idtok, TOKEN consttok);
/* makesubrange makes a SUBRANGE symbol table entry, puts the pointer to it
   into tok, and returns tok. */
TOKEN makesubrange(TOKEN tok, int low, int high);
/* instenum installs an enumerated subrange in the symbol table,
   e.g., type color = (red, white, blue)
   by calling makesubrange and returning the token it returns. */
TOKEN instenum(TOKEN idlist);
/* instdotdot installs a .. subrange in the symbol table.
   dottok is a (now) unused token that is recycled. */
TOKEN instdotdot(TOKEN lowtok, TOKEN dottok, TOKEN hightok);
/* findtype looks up a type name in the symbol table, puts the pointers
   into tok, returns tok. */
TOKEN findtype(TOKEN tok);
/* wordaddress pads the offset n to be a multiple of wordsize.
   wordsize should be 4 for integer, 8 for real and records,
   wordsize of element type (or 8 if you wish) for arrays. */
int   wordaddress(int n, int wordsize);
/* instvars will install variables in symbol table.
   typetok is a token containing symbol table pointers. */
void  instvars(TOKEN idlist, TOKEN typetok);
/* searchins will search for symbol, insert if not there. */
SYMBOL searchins(char name[]);
/* insttype will install a type name in symbol table.
   typetok is a token containing symbol table pointers. */
void  insttype(TOKEN typename, TOKEN typetok);
/* instpoint will install a pointer type in symbol table */
TOKEN instpoint(TOKEN tok, TOKEN typename);
/* instrec will install a record definition.  Each token in the linked list
   argstok has a pointer its type. */
TOKEN instrec(TOKEN rectok, TOKEN argstok);
/* instfields will install type in a list idlist of field name tokens:
   re, im: real    put the pointer to REAL in RE, IM tokens.
   typetok is a token whose symtype is a symbol table pointer.
   Note that nconc() can be used to combine these lists after instrec() */
TOKEN instfields(TOKEN idlist, TOKEN typetok);
/* makeplus makes a + operator.
   tok (if not NULL) is a (now) unused token that is recycled. */
TOKEN makeplus(TOKEN lhs, TOKEN rhs, TOKEN tok);
/* addint adds integer off to expression exp, possibly using tok */
TOKEN addint(TOKEN exp, TOKEN off, TOKEN tok);
/* addoffs adds offset, off, to an aref expression, exp */
TOKEN addoffs(TOKEN exp, TOKEN off);
/* mulint multiplies expression exp by integer n */
TOKEN mulint(TOKEN exp, int n);
/* makearef makes an array reference operation.
   off is be an integer constant token
   tok (if not NULL) is a (now) unused token that is recycled. */
TOKEN makearef(TOKEN var, TOKEN off, TOKEN tok);
/* reducedot handles a record reference.
   dot is a (now) unused token that is recycled. */
TOKEN reducedot(TOKEN var, TOKEN dot, TOKEN field);
/* arrayref processes an array reference a[i]
   subs is a list of subscript expressions.
   tok and tokb are (now) unused tokens that are recycled. */
TOKEN arrayref(TOKEN arr, TOKEN tok, TOKEN subs, TOKEN tokb);
/* dopoint handles a ^ operator.
   tok is a (now) unused token that is recycled. */
TOKEN dopoint(TOKEN var, TOKEN tok);
/* instarray installs an array declaration into the symbol table.
   bounds points to a SUBRANGE symbol table entry.
   The symbol table pointer is returned in token typetok. */
TOKEN instarray(TOKEN bounds, TOKEN typetok);
/* talloc allocates a new TOKEN record. */
TOKEN talloc();