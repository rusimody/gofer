#define ap2(f,x,y)	(ap(ap(f,x),y))
#define ap3(f,x,y,z)	(ap(ap(ap(f,x),y),z))
#define cons3(a,b,c)	(cons(a,cons(b,c)))
#define isDot(op) (textOf(op) == textDot)

extern Char apChar[];
extern String typeStr[], consStr[], bindStr[], uptoStr[];
extern Text textDot;
extern Text textBind[2], textCons[2];
extern Cell varDot;

extern Void renameName(Text oldTxt, Text newTxt);
