import Parser

--Help function for converting to Code
input :: String -> Code
input xs = map charToOp (removeWhiteSpace (filterComments xs))

--auxiliary function Tests
clTest =  (length (getCallLetters (input ":a:++:%:ab:a:%:aa:b:")))==5 &&
          (length (getCallLetters (input ":a:++::ab:a::aa:b:")))==5 &&
          (length (getCallLetters (input ":a:++::aq:a::aa:b:")))==6

fbsTest = getFunctionByScope [(RealOp 'a')] (getFunctions (input ":a:+::aa:-:")) (RealOp 'a')
                    == [RealOp 'a', RealOp 'a'] &&
          getFunctionByScope [] (getFunctions (input ":a:+::aa:-:")) (RealOp 'a')
                              == [RealOp 'a']



--First test some of the auxiliary functions
main = do
  if not clTest
    then (print ("getCallLetters does not work"))
    else return ()
  if not fbsTest
    then (print ("getFunctionByScope does not work"))
    else return ()
