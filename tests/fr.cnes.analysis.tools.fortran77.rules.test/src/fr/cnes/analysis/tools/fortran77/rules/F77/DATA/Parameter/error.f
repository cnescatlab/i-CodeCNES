      PROGRAM ESSAI

      REAL ARGUMENT
      REAL RESULTAT_1
      REAL RESULTAT_2

      ARGUMENT = 4.0
      CALL  MOUV(F(X), ARGUMENT, RESULTAT_1)	

C --- 4.0 + 3.0 : 7.0
      PRINT *, ARGUMENT, RESULTAT_1
      RESULTAT_2 = 3. + ARGUMENT

C --- 4.0 + 3.0 : 12.0
      PRINT * , ARGUMENT, RESULTAT_2

C --- La constante 3. est devenue la constante 8 
      END
C
      SUBROUTINE MOUV(ARG1, ARG2, RESULT)

      REAL   ARG1
      REAL   ARG2
      REAL   RESULT

      RESULT = ARG1 + ARG2
C
C --- Avec le code genere par le compilateur gfortran, l'instruction ci-dessous 
C --- provoque une faute memoire : SIGSEGV  
C
      ARG1 = 8.
      END

