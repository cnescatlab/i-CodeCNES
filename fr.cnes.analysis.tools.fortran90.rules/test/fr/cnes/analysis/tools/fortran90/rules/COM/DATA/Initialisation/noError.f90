       PROGRAM TEST

       INTEGER I,J,x
	   
	   I = 0
       J = 0
	   L = 0
	   
	   	   call AffecteValeur(x,0)
	   
       DO I = 1,10
			J= J+ 1
			K =K -1
			L=2*L
       END DO
		
       END PROGRAM TEST
       
       subroutine AffecteValeur(variable,ival)
   			double precision, intent(out)  :: variable
   			integer , intent(in) :: ival
   			variable = int(ival) 
   	   end subroutine AffecteValeur      


       