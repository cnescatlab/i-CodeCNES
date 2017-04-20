      PROGRAM ESSAI

      REAL X
      COMPLEX Y
      INTEGER I_TAB(100)
      INTEGER I_MAT(100,20)

      DIMENSION X(100), Y(100,20,4)

      DO I = 1, 100, 1
         I_TAB(I) = I
         X(I) = REAL(I) / 2.0
         DO J = 1, 20, 1
            I_MAT(I,J) = I*100 + J
            DO K = 1, 4, 1
               Y(I,J,K) = I
            END DO
         END DO
      END DO

      WRITE(*,*) 'I_TAB(12)=', I_TAB(12)
      WRITE(*,*) 'I_MAT(12,12)=', I_MAT(12,12)
      WRITE(*,*) 'X(12)=', X(12)
      WRITE(*,*) 'Y(12,12,2)=',  Y(12,12,2)

      END PROGRAM ESSAI

