!
! Cette subroutine affiche sur stdout le contenu de deux variables reelles :
!    - d'abord avec le format libre '*'
!    - puis avec un format defini par l'utilisateur 
! => On constate qu'il y a un Ã©cart
!

subroutine format_etoile

      USE ma_precision
      IMPLICIT NONE

      INTEGER(ENTIER)          :: i_stdout = 6
      REAL(DOUBLE)             :: x
      REAL(DOUBLE)             :: y

      x = 0.1
      y = 10.0 * x


      write (i_stdout,'(2F8.3)') x,y
! !!! Sur la ligne ci-dessus, la regle 'Int.FormatFlottant' est respectee (utilisation d'un format) 
! *    0.100   1.000     dans les trois cas 

end subroutine format_etoile

!
! Cette unite de programme (appel dans parametre) effectue l'appel a 
!    la subroutine  'formatÃ_etoile' qui affiche sur stdout le contenu de deux variables reelles : 
!    - dans un premier temps avec le format libre '*'
!    - dans un second temps avec un format defini par l'utilisateur 
! 
 
PROGRAM ESSAI

      CALL format_etoile 

END PROGRAM ESSAI
