      PROGRAM ESSAI

      INTEGER I
      INTEGER I_MAX
      INTEGER I_STEP

C
      I_MAX = 40
      I_STEP = 8

      DO    10,    I  =  0, I_MAX, I_STEP
         WRITE(*,*) 'I=', I
10    CONTINUE	

      STOP
      END PROGRAM

