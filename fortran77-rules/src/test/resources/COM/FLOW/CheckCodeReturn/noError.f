       PROGRAM MAIN
	   
	   INTEGER Y
	   INTEGER RES
	   
	   Y = 0
	   RES = TEST(Y)
	   
	   IF (RES < 0)
			WRITE(*,*) 'ERROR'
	   END IF
	   
	   END PROGRAM MAIN

       FUNCTION TEST(X)

       INTEGER TEST
	   INTEGER X
	   TEST = 2*X

       END FUNCTION TEST