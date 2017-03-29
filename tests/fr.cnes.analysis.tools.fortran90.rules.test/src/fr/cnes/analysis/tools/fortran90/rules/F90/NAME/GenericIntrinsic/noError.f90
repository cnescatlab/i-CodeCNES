PROGRAM ESSAI 

      USE precision
      IMPLICIT NONE

!
! ---- Les noms generiques des fonctions intrinseques sont les suivants : 
! ---    INT, REAL, DBLE, CMPLX
! ---    AINT, ANINT, NINT, ABS, MOD, SIGN, DIM, CMPLX, MAX, MIN
! ---    SQRT, EXP, LOG, LOG10
! ---    SIN, COS, TAN, ASIN, ACOS, ATAN, ATAN2, SINH, COSH, TANH
! ---
!

!
! --- Dans l'hypothese ou le calcul devrait etre effectue avec plus de precision, 
! --- le changement de type des des variables en double precision est necessaire et suffisant 
!

!     REAL              ARGUMENT
!     REAL              DIVISEUR
!     REAL              RESULTAT
      REAL(DOUBLE)      ARGUMENT
      REAL(DOUBLE)      DIVISEUR
      REAL(DOUBLE)      RESULTAT

      ARGUMENT = 12.5D0
      DIVISEUR = 3.48D0
      RESULTAT = MOD (ARGUMENT,DIVISEUR)

      WRITE(*,*) 'ARGUMENT=', ARGUMENT
      WRITE(*,*) 'DIVISEUR=', DIVISEUR
      WRITE(*,*) 'RESULTAT=', RESULTAT

      STOP
END PROGRAM ESSAI
