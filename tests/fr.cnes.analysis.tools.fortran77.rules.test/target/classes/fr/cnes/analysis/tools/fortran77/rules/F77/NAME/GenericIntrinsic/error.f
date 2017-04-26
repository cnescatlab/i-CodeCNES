      PROGRAM ESSAI 
C
C ---- Les noms generiques des fonctions intrinseques sont les suivants : 
C ---    INT, REAL, DBLE, CMPLX
C ---    AINT, ANINT, NINT, ABS, MOD, SIGN, DIM, CMPLX, MAX, MIN
C ---    SQRT, EXP, LOG, LOG10
C ---    SIN, COS, TAN, ASIN, ACOS, ATAN, ATAN2, SINH, COSH, TANH
C ---
C
      REAL              ARGUMENT
      REAL              DIVISEUR
      REAL              RESULTAT

C
C --- Dans l'hypothese ou le calcul devrait etre effectue avec plus de precision, 
C --- le changement de type des des variables en double precision est necessaire
C --- Mais il faut egalement modifier le nom de la fonction AMOD -> DMOD 
C --- ou bien utiliser le nom generique MOD (c'est ce que prevoit la regle) 

C     REAL              ARGUMENT            =>    DOUBLE PRECISION  ARGUMENT
C     REAL              DIVISEUR            =>    DOUBLE PRECISION  DIVISEUR
C     REAL              RESULTAT            =>    DOUBLE PRECISION  RESULTAT
C     RESULTAT = AMOD (ARGUMENT,DIVISEUR)   =>    RESULTAT = DMOD (ARGUMENT,DIVISEUR)

      ARGUMENT = 12.5
      DIVISEUR = 3.48
      RESULTAT = AMOD (ARGUMENT,DIVISEUR)

      WRITE(*,*) 'ARGUMENT=', ARGUMENT
      WRITE(*,*) 'DIVISEUR=', DIVISEUR
      WRITE(*,*) 'RESULTAT=', RESULTAT

      STOP
      END PROGRAM ESSAI
