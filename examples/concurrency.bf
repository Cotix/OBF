%
This example is a clone of the mutialExclusion example, except that it does NOT
use a lock. AKA it is NOT mutual exclusive. This example is to show that there
is actual real multithreading going on.

Please look at the mutualExclusion example to see concurrency going right!

this test uses a main turing tape with 4 cells. The middle left and right cells
are strings. Two threads are trying to print their string a few times.
If it would use locks for mutual exclusion, you would be seeing the words apple
and banana being printing. However that is the mutualExclusion example.
In this program you'll see foo and bar, and mashups of it.

the outer cells are counters for how many times they need to print.
%
b?>S?>S?>b                                %define the main tape%
|2<                                       %puts down a '2'(char). This is 50 in decimal%
*|a?>|p?>|p?>|l?>|e?>b++++++++++?>^b&     %defines apple onto the right string%
<
*|b?>|a?>|n?>|a?>|n?>|a?>b++++++++++?>^b& %defines banana onto the left string%
<|2
/[->p<]\                                  %starts the first thread%
>>>
[-<p>]                                    %second thread%
