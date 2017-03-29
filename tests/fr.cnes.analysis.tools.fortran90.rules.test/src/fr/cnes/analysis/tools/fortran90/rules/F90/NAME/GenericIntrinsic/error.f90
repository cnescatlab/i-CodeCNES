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
      REAL              ARGUMENT
      REAL              DIVISEUR
      REAL              RESULTAT

!
! --- Dans l'hypothese ou le calcul devrait etre effectue avec plus de precision, 
! --- le changement de type des des variables en double precision est necessaire
! --- Mais il faut egalement modifier le nom de la fonction AMOD -> DMOD 
! --- ou bien utiliser le nom generique MOD (c'est ce que prevoit la regle) 

!     REAL              ARGUMENT            =>    REAL(DOUBLE)  ARGUMENT
!     REAL              DIVISEUR            =>    REAL(DOUBLE)  DIVISEUR
!     REAL              RESULTAT            =>    REAL(DOUBLE)  RESULTAT
!     RESULTAT = AMOD (ARGUMENT,DIVISEUR)   =>    RESULTAT = DMOD (ARGUMENT,DIVISEUR)

      ARGUMENT = 12.5
      DIVISEUR = 3.48
      RESULTAT = AMOD (ARGUMENT,DIVISEUR)

      WRITE(*,*) 'ARGUMENT=', ARGUMENT
      WRITE(*,*) 'DIVISEUR=', DIVISEUR
      WRITE(*,*) 'RESULTAT=', RESULTAT

      STOP
END PROGRAM ESSAI
