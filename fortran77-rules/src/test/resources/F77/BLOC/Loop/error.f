      PROGRAM ESSAI

      INTEGER I
      INTEGER I_MIN
      INTEGER I_MAX
      INTEGER I_STEP
      INTEGER J
      INTEGER J_MIN
      INTEGER J_MAX
      INTEGER J_STEP
C
      I_MIN = 0
      I_MAX = 24
      I_STEP = 12
C
      J_MIN = 0
      J_MAX = 30
      J_STEP = 10

      WRITE(*,*) 'I_MIN=', I_MIN, 'I_MAX=', I_MAX, 'I_STEP=', I_STEP
      WRITE(*,*) 'J_MIN=', J_MIN, 'J_MAX=', J_MAX, 'J_STEP=', J_STEP
      DO    10,    I  =  I_MIN, I_MAX, I_STEP
         DO    10,    J  =  J_MIN, J_MAX, J_STEP
            WRITE(*,*) 'I=', I, 'J=', J
10    CONTINUE

      STOP
      END PROGRAM

