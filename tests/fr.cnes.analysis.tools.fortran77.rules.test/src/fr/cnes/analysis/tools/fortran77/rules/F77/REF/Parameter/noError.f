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
      CALL MY_SUB1
      STOP
      END PROGRAM ESSAI
C
C --- Subroutine 
C
      SUBROUTINE MY_SUB1
C
C --- Les donnees presentes dans le common nomme, sont accedees dans la subroutine en 
C --- referencant le common par son nom : 'CONTROL' et en decrivant son organisation 
C --- c'est a dire compose de 4 variables de type REAL .

C --- Dans la description de l'organisation du common il faut veiller au type des variables et a 
C --- leur ordre de definition : l'utilisation dans un INCLUDE est recommandee
C --- On ne le fait pas ici, afin que l'exemple soit explicite 
C
      REAL    COMMON_CONTROL_A
      REAL    COMMON_CONTROL_B
      REAL    COMMON_CONTROL_C
      REAL    COMMON_CONTROL_D

      COMMON /CONTROL/ COMMON_CONTROL_A, COMMON_CONTROL_B,              &
     1COMMON_CONTROL_C, COMMON_CONTROL_D
C
      WRITE(*,*) 'COMMON_CONTROL_A=', COMMON_CONTROL_A
      WRITE(*,*) 'COMMON_CONTROL_B=', COMMON_CONTROL_B
      WRITE(*,*) 'COMMON_CONTROL_C=', COMMON_CONTROL_C
      WRITE(*,*) 'COMMON_CONTROL_D=', COMMON_CONTROL_D
C
      RETURN
      END
