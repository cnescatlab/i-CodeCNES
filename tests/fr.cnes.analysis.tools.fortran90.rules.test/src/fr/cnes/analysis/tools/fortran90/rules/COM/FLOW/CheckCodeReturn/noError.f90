       PROGRAM MAIN
	   
	   INTEGER Y
	   INTEGER RES
	   
	   Y = 0
	   RES = TEST(Y)
	   
       SELECT CASE(RES)
	       CASE Y
		       WRITE(*,*) 'ERROR'
		   CASE DEFAULT
		       RES = RES + 1
	   END SELECT
	   
	   END PROGRAM MAIN

       FUNCTION TEST(X)

       INTEGER TEST
	   INTEGER X
	   TEST = 2*X

       END FUNCTION TEST