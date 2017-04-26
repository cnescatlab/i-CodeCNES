	module mod1
		real :: a=2
		logical :: blocr = .false.
   		logical :: bloct = .false.
   		logical :: validr = .false. , validt = .false.
		
		contains
			
			function f1()
				real :: b
			end function
			
			
			function f2()
				real :: b
			end function
			
	end module
	
	module mod2
		real :: a=2
		INTEGER NUMLEN
       	PARAMETER ( NUMLEN = 16 )
       	CHARACTER*(NUMLEN) CODE
		
		contains
			
			contains
			
				subroutine s1()
					real :: b
					
					function f3()
						real :: c
						real :: d
					end function
			
				end function
			
	end module