      PROGRAM ESSAI

      INTEGER*4 I_TAB(100)
      INTEGER*4 I_MAT(100,20)

      REAL*4 X(100)
      COMPLEX*8 Y(100,20,4)
      DOUBLE PRECISION W(100,20)
      LOGICAL*2 Z(100)

      DO I = 1, 100, 1
         I_TAB(I) = I
         Z(I) = MOD(I,2)
         X(I) = REAL(I) / 2.0
         DO J = 1, 20, 1
            I_MAT(I,J) = I*100 + J
            W(I,J) = REAL(I)*100.0 + REAL(J)
            DO K = 1, 4, 1
               Y(I,J,K) = I
            END DO
         END DO
      END DO

      WRITE(*,*) 'I_TAB(12)=', I_TAB(12)
      WRITE(*,*) 'I_MAT(12,12)=', I_MAT(12,12)
      WRITE(*,*) 'X(12)=', X(12)
      WRITE(*,*) 'Y(12,12,2)=',  Y(12,12,2)
      WRITE(*,*) 'Z(12)=', Z(12)
      WRITE(*,*) 'Z(13)=', Z(13)
      WRITE(*,*) 'W(12,12)=', W(12,12)

      END PROGRAM ESSAI

