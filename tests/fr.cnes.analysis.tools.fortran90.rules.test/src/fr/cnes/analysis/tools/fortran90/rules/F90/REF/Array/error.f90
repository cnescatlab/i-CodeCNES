!
! Cette unite de programme (appel sans parametre) effectue un calcul 
! en utilisant des tableaux complets .
! 

PROGRAM ESSAI

      USE ma_precision
      IMPLICIT NONE

      INTEGER(ENTIER), parameter :: N = 10

      REAL(DOUBLE), parameter :: X = 1.34_DOUBLE
      REAL(DOUBLE), parameter :: B = 2.05_DOUBLE

      REAL(DOUBLE), DIMENSION(N) :: Y
      REAL(DOUBLE), DIMENSION(N)  :: A
      REAL(DOUBLE), DIMENSION(N)  :: C

      DATA A / 2*3.25, 3*1.18, 2*0.75, 3*2.15 /
      DATA C / 2*0.25, 2*0.28, 2*0.36, 2*0.44, 2*0.66 / 

      Y = A*X + B - C
! Ci-dessus, la regle 'Tr.TableauComplet' n'est pas respectee .
! Cette regle recommande d'utiliser la notation de type T(:) qui offre 
! le meilleur compromis entre clarte et concision.
! La notation de type : Y(1:NDim) = A(1:NDim)*X + B - C(1:NDim), n'est pas recommandee non plus 

      PRINT *, 'Y=', Y

END PROGRAM ESSAI

