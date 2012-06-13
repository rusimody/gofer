-- This file contains the example program from section 7.7 of the Haskell
-- report (version 1.1) for a program using synchronisation.

main :: Dialogue
main  = readChan stdin abort (\userInput -> readNums (lines userInput))

readNums           :: [String] -> Dialogue
readNums inputLines = readInt "Enter first number: " inputLines
                        (\num1 inputLines1 ->
                          readInt "Enter second number: " inputLines1
                            (\num2 _ -> reportResult num1 num2))

reportResult       :: Int -> Int -> Dialogue
reportResult num1 num2
  = appendChan stdout ("Their sum is: "++ show (num1 + num2)) abort done
                                  

-- readInt prints a prompt and then reads a line of input.  If the
-- line contains an integer, the value of the integer is passed to the
-- success continuation.  If a line cannot be parsed as an integer,
-- an error message is printed and the user is asked to try again.
-- If EOF is detected, the program is aborted.

readInt :: String -> [String] -> (Int -> [String] -> Dialogue) -> Dialogue
readInt prompt inputLines succ
  = appendChan stdout prompt abort
      (case inputLines of
         (l1 : rest) -> case (intRead l1) of
                          [(n,"")] -> succ n rest
                          _        -> appendChan stdout
                                       "Error - retype the number\n" abort
                                       (readInt prompt rest succ)
         _           -> appendChan stdout "Early EOF" abort done)

-- Since the Gofer standard prelude does not include the reads function in
-- the Text class, we have explicitly specified intRead in the definition
-- above (rather than "reads" as used in the Haskell report).
-- A straightforward (if rather crude) definition of this function follows:

intRead   :: String -> [(Int,String)]
intRead "" = []
intRead s  = loop 0 s
             where loop n []        = [(n,"")]
                   loop n s@(d:ds)
                       | isDigit d  = loop (10*n+(ord d - ord '0')) ds
                       | otherwise  = [(n,s)]
