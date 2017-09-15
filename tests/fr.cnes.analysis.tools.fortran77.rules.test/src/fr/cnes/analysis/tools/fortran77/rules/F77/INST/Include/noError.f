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
C --- Subroutine 
C
      SUBROUTINE MY_SUB1
C
C --- Dans la definition du common nomme, il faut veiller au type des variables et a 
C --- leur ordre de definition : l'utilisation dans un INCLUDE est recommandee
C
      INCLUDE 'F77_ok_Tr.Include.h'
C
      WRITE(*,*) 'C=', C, 'D=', D
      RETURN
      END
