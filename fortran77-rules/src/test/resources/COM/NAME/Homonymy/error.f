	module mod1
		real :: y=2
		
		contains
			
			function f1()
				real :: y
			end function
			
			
			function f2()
				real :: z
			end function
			
	end module
	
	module mod2
		real :: x=2
		
		contains
			
			contains
			
				subroutine s1()
					real :: x
					
					function f3()
						real :: w
						real :: x
					end function
			
				end function
			
	end module