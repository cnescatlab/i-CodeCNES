      PROGRAM ESSAI

      INTEGER total (3,2)
      INTEGER sum (6)
      EQUIVALENCE (sum, total)

      DO I = 1, 6, 1
         sum(I) = I**2
      END DO

      DO J = 1, 2, 1
         DO I = 1, 3, 1
            K = (J-1) * 3 + I
            WRITE (*, *) I, J, K, sum(K), total(I,J)
         END DO
      END DO 

C     INTEGER T_COMPLET(10)
C     INTEGER T_PARTIE1(3)
C     INTEGER T_PARTIE2(7)

C     EQUIVALENCE (T_COMPLET(1), T_PARTIE1(1))
C     EQUIVALENCE (T_COMPLET(4), T_PARTIE2(1))
    
C     DO I = 1, 10, 1
C        T_COMPLET(I) = I**2
C     END DO

C     WRITE (*, *) 'T_COMPLET(1)=', T_COMPLET(1), 'T_PARTIE1(1)=', T_PARTIE1(1)
C     WRITE (*, *) 'T_COMPLET(4)=', T_COMPLET(4), 'T_PARTIE2(1)=', T_PARTIE2(1)

      STOP
      END PROGRAM ESSAI
