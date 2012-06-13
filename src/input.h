/* Arunachala Siva Arunachala Ramana */
extern Cell varMinus;			/* (-)				   */
extern Cell varNegate;			/* negate			   */
extern Cell varFlip;			/* flip				   */
extern Cell varFrom;			/* [_..]			   */
extern Cell varFromTo;			/* [_.._]			   */
extern Cell varFromThen;		/* [_,_..]			   */
extern Cell varFromThenTo;		/* [_,_.._]			   */
extern Cell yylval;
extern Int   row, column, startColumn;
extern Text textDot;
extern Cell varDot;
extern Cell typeLhs;

extern Void syntaxError Args((String s));
extern Void goOffside	  Args((Int));
extern Void unOffside	  Args((Void));
extern Bool canUnOffside	  Args((Void));
extern Int yylex 	  Args((Void));
