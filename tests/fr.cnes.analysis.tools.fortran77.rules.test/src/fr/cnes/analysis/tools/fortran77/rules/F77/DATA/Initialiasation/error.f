C
C --- Main
C
      PROGRAM ESSAI  
      COMMON /CONTROL/ A, B, C, D
	  COMMON /CONTROL2/ E, F, G
	  
	  DATA  E, F / 1.0, 2.0/

C --- Initialisation a la main 
C --- Il est preferable d'effectuer cette operation d'init dans un BLOCK DATA
      A = 1.0
      B = 2.0
      C = 3.0
      D = 4.0
	  E = 5.0
	  F = 6.0
	  G = 7.0
C
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
C --- On ne le fait pas ici, afin que l'exemple soit explicite 
C
      COMMON /CONTROL/ A, B, C, D
C
      WRITE(*,*) 'C=', C, 'D=', D
      RETURN
      END
