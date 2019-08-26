      PROGRAM ESSAI  

C
C --- Le common blanc defini ici contient 4 REAL
C
      COMMON A, B
      COMMON C, D

      A=1.0
      B=2.0
      C=3.0
      D=4.0

      CALL MY_SUB1
      STOP
      END PROGRAM ESSAI

      SUBROUTINE MY_SUB1
C
      INTEGER C, D
C
C --- Le common blanc defini ici 2 INTEGER .
C --- Or comme il s'agit de la meme zone memoire que celle definie dans le MAIN
C --- 
C
      COMMON C, D
C
C --- On obtient un double defaut : 
C --- . Pas le meme type pour les variables C et D portant le meme nom dans MAIN et SUBROUTINE
C --- . Position dans le common : les emplacements memoire de C et D dans la routine correspondent a A dans le main  
C
      WRITE(*,*) 'C=', C, 'D=', D
      RETURN
      END
