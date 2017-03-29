       PROGRAM TEST
       
	   integer ival3(5),ival4(5)
       INTEGER I,J,K,L,X,Y
	   K = 0
	   L = 2
	   
	   call AffecteValeur(Y,0)
       DO I = 1,10
	      DO J = 2,8
			    K =K -1 * X
			    L=(2*L)/K + Y
		  END DO
       END DO
	   forall(i=1:5,ival3(i).NE.0) .TRUE.
	   where(ival3 == 1) .TRUE.  
       END PROGRAM TEST
	  
	   subroutine AffecteValeur(variable,ival)
   			double precision, intent(in)  :: variable
   			integer , intent(in) :: ival
   			variable = int(ival) 
   	   end subroutine AffecteValeur