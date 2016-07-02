%

This example program shows simple use of a standard array. In this case a string.
A String is just an array of bytes ended by a 0 byte. The stl defines the following
functions on Strings:

:S:^*b&:
:S+:*[>]?>b<$c>[-$+$]!$&:
:Sa:$*[$p+$>]$:
:Sp:*[.>]&:

As you can see there is one constructor and 3 functions defined for a String.

+ adds a character (byte) to the end of the string.
a appends another String/array to the end of the string.
p prints the string.

Note how the append function utilises the + function.


The example program will build up the strings 'foo ' and 'bar\n' and print them
seperatly.

NOTE: This program might be slow. it runs in about a minute on my laptop.
This is due to the fact that assignment is O(n). Unfortunally we could not find
the time to implement an optimising compiler.
%


% initialise the main tape with a few characters %
% we will use these to build our string. %
% the tape now looks as follows: %
% [f,o,b,a,r, ,'\n'] %
|f?>|o?>|b?>|a?>|r?>|!-?>b++++++++++
%Insert two strings%
?>S?>S
%Set our pointer to the string, walk to f and jump back and then add the f%
<@<<<<<<<$+
%jump back and walk to the O, jump to the string and add that%
%Note that all of this works because when jumping, you set your pointer to your%
%current position.%
$>$++ %Add the O twice%
%Now the space%
$>>>>$+
%Alright lets print it%
p

%Now repeat this for the second string (the bar string)%
>$<<<$+$>$+$>$+$>>$+p
