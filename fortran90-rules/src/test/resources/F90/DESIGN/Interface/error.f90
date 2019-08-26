!
! la regle 'Org.ModuleInterface' definit le contenu d'un module d'interface qui se limite a : 
! - des clauses USE, eventuellement avec renommage, 
! - une declaration PRIVATE,
! - une declaration PUBLIC specifiant ce qui doit etre reexporte, 
! - des blocs d'interface.
!
! or ici dans ce contre-exemple nous avons un module 'PLUSIEURS_USAGES' qui contient a 
! la fois des elements declaratifs d'interface (pour la fonction f_syslog) et 
! de l'implementation de code avec la routine s_mean_variance qui effectue 
! le calcul de moyenne, variance, ecart-type sur un jeu de donnees passe en parametre
! 

module MESSAGE_SYSLOG

  IMPLICIT NONE 

  ! definition d'un type de variable qui est public (visible en dehors du module)
  TYPE opendata_type
    CHARACTER(len=48) message_string
  END TYPE opendata_type
  PUBLIC opendata_type 

end module MESSAGE_SYSLOG

module PLUSIEURS_USAGES

  IMPLICIT NONE 
  PRIVATE

  ! definition de variables privees (non visibles en dehors du module)
  real closedata_b


  ! interface avec un sous-programme f_syslog ecrit en C qui permet de generer des 
  ! messages de trace vers SYSLOG 
  interface

     subroutine f_syslog(cdata)
         use MESSAGE_SYSLOG

         TYPE(opendata_type), intent(in)   :: cdata

     end subroutine f_syslog

  end interface
  PUBLIC f_syslog                             ! cette interface est publique 
  PUBLIC s_mean_variance                      ! cette interface est publique

  CONTAINS

! --------------------------------------------------------------------
! SUBROUTINE  s_mean_variance():
!    Cette routine calcule la moyenne, la variance et l'ecart-type 
!       sur un tableau de donnees fourni en parametre .
! --------------------------------------------------------------------

   SUBROUTINE  s_mean_variance(Data, SIZE, n, Mean, Variance, StdDev)
      IMPLICIT  NONE
      INTEGER, INTENT(IN)                 :: SIZE
      INTEGER, INTENT(IN)                 :: n
      REAL, DIMENSION(1:SIZE), INTENT(IN) :: Data
      REAL, INTENT(OUT)                   :: Mean, Variance, StdDev
      INTEGER                             :: i

      Mean = 0.0
      DO i = 1, n
         Mean = Mean + Data(i)
      END DO
      Mean = Mean / n

      Variance = 0.0
      DO i = 1, n
         Variance = Variance + (Data(i) - Mean)**2
      END DO
      Variance = Variance / n
      StdDev   = SQRT(Variance)
   END SUBROUTINE  s_mean_variance

end module PLUSIEURS_USAGES

!
!
! 

PROGRAM ESSAI

      USE MESSAGE_SYSLOG
      USE PLUSIEURS_USAGES
      IMPLICIT NONE

      INTEGER, PARAMETER :: MAX_SIZE = 7 
      TYPE(opendata_type) :: c_string
      REAL :: Mean, Var, Std
      REAL, DIMENSION(MAX_SIZE)::R_T_a = (/ 302.0, 1194.0, 500.0, 107.0, 1542.0, 599.0, 418.0 /)

! --- Init du message de trace avec message par defaut  
      c_string = opendata_type('Exemple d une chaine ...')

! --- Appel de la fonction de log
      call f_Syslog(c_string)

! --- Trace dans la console
      write(*, '(A,A)') 'Dans le programme principal: ', c_string

! --- Quelques calculs mathematiques sur un tableau  
      CALL  s_mean_variance(R_T_a, SIZE(R_T_a), MAX_SIZE, Mean, Var, Std)

! --- On affiche les resultats
      WRITE(*,*)  "Nombre de donnees  = ", MAX_SIZE 
      WRITE(*,*)  "Moyenne            = ", Mean
      WRITE(*,*)  "Variance           = ", Var
      WRITE(*,*)  "Ecart Type         = ", Std

      stop

END PROGRAM ESSAI
