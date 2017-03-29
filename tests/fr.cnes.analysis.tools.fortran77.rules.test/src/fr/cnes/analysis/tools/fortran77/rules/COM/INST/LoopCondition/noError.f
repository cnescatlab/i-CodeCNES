 PROGRAM ESSAI

      	integer :: i=0
		
		do while (resid .GT. 5.0)
		   resid = abs(x(i))
		   write (*,*) ' Continue execution'
		   i = i+1
		end do
		do while (resid >= 5.0)
		   resid = abs(x(i))
		   write (*,*) ' Continue execution'
		   i = i+1
		end do
	   
  END PROGRAM
