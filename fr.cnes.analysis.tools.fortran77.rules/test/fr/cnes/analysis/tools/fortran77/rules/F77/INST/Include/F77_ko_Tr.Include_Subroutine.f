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

