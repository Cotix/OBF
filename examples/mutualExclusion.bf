%
this test uses a main turing tape with 5 cells. The middle cell is a lock,
the middle left and right cell are strings. Two threads are trying to print their
string a few times. They will use the lock to prevent them from printing through
each others output. If everything works correctly, you will be seeing the words
apple and banana being printing. If it breaks, you'll see foo and bar,
and mashups of it.

the outer cells are counters for how many times they need to print.
%


b?>S?>L?>S?>b                             %define the main tape%
|2<                                       %puts down a '2'(char). 50 in decimal%
*|a?>|p?>|p?>|l?>|e?>b++++++++++?>^b&     %defines apple onto the right string%
<<
*|b?>|a?>|n?>|a?>|n?>|a?>b++++++++++?>^b& %defines banana onto the left string%
<|2
/[->>l<p>u<<]\                            %starts the first thread%
>>>>
[-<<l>p<u>>]                              %second thread%
