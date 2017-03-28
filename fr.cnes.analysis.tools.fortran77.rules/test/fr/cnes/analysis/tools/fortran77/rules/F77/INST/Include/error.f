C
C --- Le common 'CONTROL' defini ici contient 4 REAL dans un BLOCK DATA
C
      INCLUDE 'F77_ok_Tr.Include.d'
C
C --- Main
C
      PROGRAM ESSAI  
      CALL MY_SUB1
      STOP
      END PROGRAM ESSAI

C
C --- Ci-dessous on va inclure le code associe a la subroutine MY_SUB1
C
      INCLUDE 'F77_ko_Tr.Include_Subroutine.f'
C
