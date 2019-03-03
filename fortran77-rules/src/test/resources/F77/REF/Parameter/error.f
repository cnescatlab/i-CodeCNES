C
C --- Le common 'CONTROL' defini ici contient 4 REAL A, B, C, D
C
      BLOCK DATA  ABCD_VAL
      COMMON /CONTROL/ A, B, C, D
      DATA  A,B,C,D / 1.0, 2.0, 3.0, 4.0 /
      END
C
C --- Main
C
      PROGRAM ESSAI  
      CALL MY_SUB1 (A, B, C, D) 
      STOP
      END PROGRAM ESSAI
C
C --- Subroutine 
C
      SUBROUTINE MY_SUB1 (COMMON_CONTROL_A, COMMON_CONTROL_B,           &
     1COMMON_CONTROL_C, COMMON_CONTROL_D) 
C
C --- Les 4 arguments passes a la subroutine ayant ete declarees dans le common nomme,'CONTROL',
C --- n'auraient pas du etre transmis de la sorte . 
C --- Dans la subroutine, ils ont des valeurs erronees qui ne correspondent pas a celles avec 
C --- laquelle ils ont ete initialises respectivement .
C
C ---   La bonne methode consiste a referencer le common dans la subroutine et a decrire son 
C ---   organisation : les donnees seront accedees directement, sans necessiter de passage d'arguments .
C
      REAL    COMMON_CONTROL_A
      REAL    COMMON_CONTROL_B
      REAL    COMMON_CONTROL_C
      REAL    COMMON_CONTROL_D
C
      WRITE(*,*) 'COMMON_CONTROL_A=', COMMON_CONTROL_A
      WRITE(*,*) 'COMMON_CONTROL_B=', COMMON_CONTROL_B
      WRITE(*,*) 'COMMON_CONTROL_C=', COMMON_CONTROL_C
      WRITE(*,*) 'COMMON_CONTROL_D=', COMMON_CONTROL_D
C
      RETURN
      END
