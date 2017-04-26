!
! la regle 'Org.ModuleInterface' definit le contenu d'un module d'interface qui se limite a : 
! - des clauses USE, eventuellement avec renommage, 
! - une declaration PRIVATE,
! - une declaration PUBLIC specifiant ce qui doit etre reexporte, 
! - des blocs d'interface.
!

module MESSAGE_SYSLOG

  IMPLICIT NONE 

  ! definition d'un type de variable qui est public (visible en dehors du module)
  TYPE opendata_type
    CHARACTER(len=48) message_string
  END TYPE opendata_type
  PUBLIC opendata_type 

end module MESSAGE_SYSLOG

module INTERFACE_SYSLOG

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

end module INTERFACE_SYSLOG

!
!
! 

PROGRAM ESSAI

      USE MESSAGE_SYSLOG
      USE INTERFACE_SYSLOG 
      IMPLICIT NONE

      TYPE(opendata_type) :: c_string

! --- Init du message de trace avec message par defaut  
      c_string = opendata_type('Exemple d une chaine ...')

! --- Appel de la fonction de log
      call f_Syslog(c_string)

! --- Trace dans la console
      write(*, '(A,A)') 'Dans le programme principal: ', c_string

      stop

END PROGRAM ESSAI
