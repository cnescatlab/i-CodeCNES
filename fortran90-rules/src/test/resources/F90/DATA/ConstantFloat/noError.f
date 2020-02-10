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

     real(DOUBLE), parameter :: CONSTANTE_PI = 3.141592653589793239_DOUBLE

END MODULE ma_precision

! 
! --- Cette unite de programme effectue la declaration de deux constantes reelles : 
! ---    - x1 : dont la valeur est donnee en SIMPLE PRECISION => ecart 
! ---    - x2 : dont la valeur est donnee en DOUBLE PRECISION => juste
! 
PROGRAM ESSAI 

      USE ma_precision
      IMPLICIT NONE
 
      real(DOUBLE) :: x2 = 0.1_DOUBLE
! +++ Regle 'Don.ConstFlottant' respectee ci-dessus : la precision de la constante est fixee en double precision, 
! +++    la valeur resultante est donc juste . 

      print *, 'x1=', x1, 'x2=', x2, 'egalite x1/x2:', (x1==x2)

! affiche 0.10000000149011612   0.1000000000000000 F

END PROGRAM ESSAI 

