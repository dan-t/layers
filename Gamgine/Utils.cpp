
#define __POS__        (__FILE__ ++ ":" ++ show __LINE__)
#define ERROR          error $ __POS__ ++ " -> " ++

#define SHOW(a)        (#a ++ "=" ++ (show (a)))
#define S(a)           SHOW(a)
#define SHOW2(a,b)     (S(a) ++ " " ++ S(b))
#define S2(a,b)        SHOW2(a,b)
#define SHOW3(a,b,c)   (S2(a,b) ++ " " ++ S(c))
#define S3(a,b,c)      SHOW3(a,b,c)
#define SHOW4(a,b,c,d) (S3(a,b,c) ++ " " ++ S(d))
#define S4(a,b,c,d)    SHOW4(a,b,c,d)

#define PRINT(a)        putStrLn S(a)
#define P(a)            PRINT(a)
#define PRINT2(a,b)     putStrLn S2(a,b)
#define P2(a,b)         PRINT2(a,b)
#define PRINT3(a,b,c)   putStrLn S3(a,b,c)
#define P3(a,b,c)       PRINT3(a,b,c)
#define PRINT4(a,b,c,d) putStrLn S4(a,b,c,d)
#define P4(a,b,c,d)     PRINT4(a,b,c,d)

#define SHOWLN(x)       (#x ++ "=" ++ (show (x)) ++ "\n")

#define IMPORT_LENS     import qualified Data.Lens.Strict as LE; \
                        import Control.Category ((.)); \
                        import Prelude hiding ((.)); \

#define LENS(field)    field##L = LE.lens field (\value record -> record {field=value})
