RPL
( C:\Users\cmorgan\OneDrive - National Nonwovens\Documents\Debug4x\Projects\primes\sys\primes.s, part of the primes.hpp project, created by <> on 2/4/2025 )

INCLUDE primes.h

RPL
xNAME PRIMES
::
	 CK1NOLASTWD
	 CK&DISPATCH0
  real :: ( if the argument from stack pos 1 is a real )
	 		{ LAM n } BIND	( grab the argument from stack pos 1 and store to var n )
	 									
 	( **note the following form of "LAM var func"
 			I beleive this is basically treating the subroutine like a
    function from a procedural language
 			the return is then left in position 1 of the stack
 			if 'LAM var STO is used, the function changes it's input )		
				LAM n LESS_THAN_4 ( check if n < 1 or n == 2 or n == 3 )

*			if n is a list, this is our answer. time to exit
				DUPTYPELIST? IT ::
						LAM n
						SWAP
						GRACEFUL_EXIT
				;				
				
*			create list, A and begin Seive of Eratothenes Algorithm
				{ FALSE } ( push a 0 on the begining of a list to represent 1 )
*			let a be a list of true bools in the amount of 2 through n				
				{ LAM A } BIND
				::
						LAM n %1 %+ COERCE #2
						DO
				  		LAM A TRUE >TCOMP
				  		' LAM A STO
						LOOP
				;
				
*			for i = 2, 3, 4, ..., not exceeding ƒn n do				
				::
						LAM n %SQRT %IP COERCE #1 #+ #2 ( stop value is always -1 )
						DO
								LAM A INDEX@ NTHELCOMP DROP IT :: ( if A[i] is true, i is INDEX@ )
										:: ( for j = i^2, i^2 +i, i^2 + 2i, i^2 + 3i, ..., not exceeding n do )
												%0 #0
												{ LAM b LAM c } BIND
												INDEX@ UNCOERCE %2 %^ COERCE ( i^2 )
														LAM b COERCE INDEX@ #* #+					( + b * i )
														' LAM c STO															( update c )
												
												LAM n COERCE LAM c
												DO
										  		LAM A JINDEX@ NTHELCOMP DROP IT ::
										  						FALSE LAM c LAM A PUTLIST
										  						' LAM A STO ( udate the list of prime bools )
														;
														LAM b %1 %+ ' LAM b STO
														JINDEX@ UNCOERCE %2 %^ COERCE ( i^2 )
																LAM b COERCE JINDEX@ #* #+	( + b * i )
																' LAM c STO															( update c )
														LAM n COERCE LAM c #< IT ::
																ExitAtLOOP
														;
												LOOP
										  ABND ( free b and c )
										; 	
								;
						LOOP
				;
	
				::						
						NULL{} { LAM primes } BIND
						LAM n COERCE #1
						DO ( z = for 1 to n )
						  LAM A INDEX@ NTHELCOMP DROP IT :: ( if A[z] True )
										LAM primes INDEX@ UNCOERCE >TCOMP ' LAM primes STO 				
						;
						LOOP
				;
				
				LAM primes ( Display Primes )
    ABND	( free primes )
				ABND	( free A )		

				GRACEFUL_EXIT
		;
;

* a function that takes a real from pos 1
* if the argument is greater than 4, it passes it back unchanged
* if the argument is less than 4, it passes back a list
* that represents what the primes are up to that argument
NULLNAME LESS_THAN_4		
::
{ NULLLAM } BIND
		::		
				1GETLAM %2 %<	case ::
						NULL{}
				;
	 	 1GETLAM %2 %= case ::
						%2 ONE{}N
	 		;
	 		1GETLAM %3 %= case ::
						%2 %3 TWO{}N
				;
				1GETLAM
		;
		ABND			
;

*	graceful exit (free all local variables) and end program
NULLNAME GRACEFUL_EXIT
		::
				ABND
				xKILL
		;
