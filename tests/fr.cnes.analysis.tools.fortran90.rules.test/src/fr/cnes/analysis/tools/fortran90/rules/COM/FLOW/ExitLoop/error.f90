       DO I = 1,COND
	       EXIT
       END
	   
       DO I = 1,10
	       GO TO 1000
       END DO

       FUNCTION TEST

       DO I = 1,2       	   
           DO J = 1,3 
	           IF (2 > 3) THEN
                   STOP 0
	           ELSE
		           CYCLE
			   END
	       END
       END

       END FUNCTION