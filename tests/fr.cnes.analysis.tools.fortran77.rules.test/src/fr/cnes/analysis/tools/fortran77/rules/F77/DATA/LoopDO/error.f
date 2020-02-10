      PROGRAM ESSAI

      REAL    R
      REAL    R_MAX
      REAL    R_STEP

C
      R_MAX = 45.22
      R_STEP = 6.5

      DO    10,    R  =  0, R_MAX, R_STEP
         WRITE(*,*) 'R=', R
10    CONTINUE	

      STOP
      END PROGRAM

