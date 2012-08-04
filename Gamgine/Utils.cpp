
#define __POS__        (__FILE__ ++ ":" ++ show __LINE__)
#define ERROR          error $ __POS__ ++ " -> " ++

#define SHOW(a)        (#a ++ "=" ++ (show (a)))
#define S(a)           SHOW(a)
#define SHOW2(a,b)     (SHOW(a) ++ " " ++ SHOW(b))
#define S2(a,b)        SHOW2(a,b)
#define SHOW3(a,b,c)   (SHOW(a) ++ " " ++ SHOW(b) ++ " " ++ SHOW(c))
#define S3(a,b,c)      SHOW3(a,b,c)
#define SHOW4(a,b,c,d) (SHOW(a) ++ " " ++ SHOW(b) ++ " " ++ SHOW(c) ++ " " ++ SHOW(d))
#define S4(a,b,c,d)    SHOW4(a,b,c,d)

#define SHOWLN(x)      (#x ++ "=" ++ (show (x)) ++ "\n")

#define LENS(field)    field##L = lens field (\value record -> record {field=value})
