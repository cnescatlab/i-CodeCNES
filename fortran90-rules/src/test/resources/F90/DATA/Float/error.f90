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

      write (i_stdout,*) x, y
! !!! Sur la ligne ci-dessus, non respect de la regle 'Int.FormatFlottant' qui interdit l'utilisation 
! !!! du format '*' pour les nombres flottants 
! * affichage sur LINUX DEBIAN SQUEEZE 32b 
!               Compilateur gfortran : 0.10000000149011612     1.0000000149011612
! * affichage sur Sparc/Solaris :
!               compilateur NAG      : 0.1000000               1.0000000
!               compilateur CRAY     : 0.100000001             1.

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
