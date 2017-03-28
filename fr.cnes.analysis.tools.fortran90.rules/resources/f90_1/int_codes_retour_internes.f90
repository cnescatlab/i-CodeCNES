module int_codes_retour_internes
implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: int_codes_retour_internes.f90 69 2012-09-11 08:33:34Z ffsm $'

public
interface
     subroutine mzipro_numero_routine ( routine, nom, identification, retour )

       use mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                                  :: routine        ! numero de la routine
character(len=pm_nom_routine),intent(out)            :: nom            ! nom de la routine
character(len=pm_identification_routine),intent(out) :: identification ! role succinct de la routine
integer, intent(out)                                 :: retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mzipro_numero_routine
     subroutine mzipro_val_retour ( valeur, signification, retour )

       use mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


integer, intent(in)                                      :: valeur        ! valeur a traiter
character(len=pm_signification_code_retour), intent(out) :: signification ! chaine indiquant la signification du code retour

integer, intent(out)  :: retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mzipro_val_retour
end interface

end module int_codes_retour_internes
