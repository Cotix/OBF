%

Banking example. This is a sample program that shows of a simple naive bank System
To achieve this we use objects and functions. We have defined a few objects:
 - Account: this object contains a lock and the amount of money someone has
 - Bank: This object contains 2 accounts
 - Transaction : This object defines a transaction.
        It includes the amount, account id sender and receiver and a done flag.

It also shows a simple function definition on a Transaction. We have defined
Tx (Transaction eXecute) to execute the transation.

If the transaction fails due to insufficient funds, it will cancel itself.

In the example code underneath, it first defines these types, and then it will
setup an example bank. In our case the bank has only two accounts, but the functions
have support for multiple accounts. The transaction object takes an account index
as arguments.

After the types are defined, it gives both accounts some money. The first account
gets the ascii value of 'a' and the second 'z'. Then it setups the transactions.
There are 10 transactions in this sample code, 5 from account 0 to 1 and 5 from
1 to 0. 9 of the transactions transact 4 euro, one transacts only 3 euro. This
means that one of them will end up with an euro more and the other with an euro less.

After it has setup everything, it loops over the transactions and executes them
in threads. 10 thread jobs will be started.

After it has started all the threads, it walks through all the transactions,
and waits on the done flag. This way it will be sure all transactions are done
before printing the final balances.

Correct output is 'by'. (the accounts started with a and z, a gains 1 euro, and
z loses 1 euro.)

Note that running this example might take a while. On my laptop it takes about
40 seconds with 3 Sprockell 'cores'. Increasing the amount of sprockells, does
not necesarily increase the performance since extra threads do not actually add
extra computing power, since Sprockell only uses one thread to do it's simulation
This means that when you add more sprockells, you just split the same computing
power over different threads. This example only uses 2 accounts, and it uses locks
to prevent race conditions. So actually only 2 transactions can be executed in paralel
the other threads will be busy waiting on the locks to come available.
%

:A: *b?>b& :
:B: *A?>A& :
:T: *b?>b?>b?>b+&: % AMOUNT | A1 | A2 | DONE %
:Tx:
    $*$*>c>[$>$-]!  %Find first account%
    <$*~+>g(        %if account has more money than transaction amount%
      <s>           %subtract amount from his balance%
    )[$^b$^b]       %otherwise set amount to 0%
    !<-              %remove result square and free the lock%
    &&*            %pointer now points back to start of bank again%
    $>>c>[$>$-]!    %Find second account%
    <<$*~+>a<-&&$&*>>>^b&:

B**>|a&>*>|z&&@ %Setting up the bank accounts%
%Now going to setup all the transactions%
%Notice the alternating + pattern for the sender/receiver account%
?>T*++++>>+&
?>T*++++>+>&
?>T*+++>>+&
?>T*++++>+>&
?>T*++++>>+&
?>T*++++>+>&
?>T*++++>>+&
?>T*++++>+>&
%Start the threads, and wait on the done flags%
/x\</x\</x\</x\</x\</x\</x\</x\
*>>>~&>
*>>>~&>
*>>>~&>
*>>>~&>
*>>>~&>
*>>>~&>
*>>>~&>
*>>>~&
%Print output!%
$**>.&>*>.&
