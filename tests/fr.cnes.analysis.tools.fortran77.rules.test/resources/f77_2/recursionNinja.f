      PROGRAM RECURSE
      
C     An example of legal recursion in Fortran 77      
        INTEGER M,N,FACT      
        EXTERNAL FACT
        
C     Calculate a Factorial using recursion. Of course you can't
C     make N too big or you will get an overflow.
C     Note that a pointer to FACT is passed to function FACT.
C     This is the "Cunning Plan" as Baldrick would say.      
        N = 5      
        M = FACT(N,FACT)      
        PRINT *,'FACTORIAL OF ',N,' IS ', M      
      END      
      
      FUNCTION FACT(N, FUNC)      
        INTEGER FACT, N, FUNC      
        EXTERNAL FUNC      
        IF (N .EQ. 0) THEN         
          FACT = 1      
        ELSE         
          FACT = N * FUNC(N-1, FUNC)      
        ENDIF      
      END 
