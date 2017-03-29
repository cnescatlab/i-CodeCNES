!
! --- Le module 'ma_precision' defini ici permet de standardiser les types de variables
! --- utilises dans l'unite de programme avec leur presision associee
!
MODULE ma_precision

     integer, parameter :: DOUBLE = selected_real_kind(15)
     integer, parameter :: SIMPLE = selected_real_kind(6)
     integer, parameter :: LONG   = selected_int_kind(18)
     integer, parameter :: ENTIER = selected_int_kind(9)
     integer, parameter :: COURT  = selected_int_kind(4)

END MODULE ma_precision

!
! --- Le programme ci-dessous appelle successivement les fonctions SUB1 puis SUB2.
! --- on peut observer, l'effet de la precision (simple ou double) de la constante Pi
! --- sur le resultat obtenu (calcul circonference cercle) 
!
PROGRAM ESSAI

     USE ma_precision
     IMPLICIT NONE

     REAL(SIMPLE) :: DIAM
     REAL(SIMPLE) :: RAYON
     REAL(DOUBLE) :: CIRC_R
     REAL(DOUBLE) :: FUNC_1

     REAL(DOUBLE) :: CIRC_D
     REAL(DOUBLE) :: FUNC_2

! On fixe la valeur du diametre du cercle
     DIAM = 2.0
! On fixe le rayon 
     RAYON = 1.0

     CIRC_R = FUNC_1(RAYON) 
     WRITE(*,*) 'Circonference (simple precision) a partir du rayon =', CIRC_R
     CIRC_D = FUNC_2(DIAM) 
     WRITE(*,*) 'Circonference (double precision) a partir du diametre =', CIRC_D
!
! !!! On observe un ecart, sur le resultat du calcul de circonference de cercle du fait 
! !!! de la difference de precision de la constante Pi dans chaque fonction .
! !!! La fonction FUNC_2 retournant la valeur la + precise, puisque utilisant une valeur de Pi plus precise.
! !!! 

     STOP

END PROGRAM ESSAI

!
! --- Dans la fonction FUNC_1, la constante Pi est declaree localement avec une valeur en simple precision  
! --- En parametre d'entree, le rayon du cercle 
! --- En sortie de la fonction, la circonference du cercle
!
DOUBLE PRECISION FUNCTION FUNC_1(RAYON)

      USE ma_precision
      IMPLICIT NONE

      REAL(SIMPLE) RAYON
      REAL(SIMPLE) CONSTANTE_PI
      PARAMETER (CONSTANTE_PI = 3.141592653589793239_SIMPLE)
! !!! Dans l'instruction ci-dessus, la regle 'Don.Constante' n'est pas respectee. 
! !!! La declaration de la constante Pi devrait etre faite dans un module (ma_precision par exemple)  

      WRITE(*,*) 'PI (Simple precision)=', CONSTANTE_PI
      FUNC_1 = 2.0 * CONSTANTE_PI * RAYON
      RETURN

END FUNCTION FUNC_1

!
! --- Dans la fonction FUNC_2, la constante Pi est declaree localement avec une valeur en double precision  
! --- En parametre d'entree, le diametre du cercle 
! --- En sortie de la fonction, la circonference du cercle
!
DOUBLE PRECISION FUNCTION FUNC_2(DIAM)

      USE ma_precision
      IMPLICIT NONE

      REAL(SIMPLE) DIAM
      REAL(DOUBLE) CONSTANTE_PI
      PARAMETER (CONSTANTE_PI = 3.141592653589793239_DOUBLE)
! !!! Dans l'instruction ci-dessus, la regle 'Don.Constante' n'est pas respectee. 
! !!! La declaration de la constante Pi devrait etre faite dans un module (ma_precision par exemple)  

      WRITE(*,*) 'PI (Double precision)=', CONSTANTE_PI
      FUNC_2 = CONSTANTE_PI * DIAM
      RETURN

END FUNCTION FUNC_2
