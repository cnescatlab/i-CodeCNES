module int_codes_retour
implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: int_codes_retour.f90 69 2012-09-11 08:33:34Z ffsm $'

public
interface
     subroutine mzpro_code_retour (code_retour, nom_biblio, nom_routine, identification_routine, &
                                   signification_code_retour, retour_mzpro, message, version_MSLIB90, version_MSPRO)
       use mslib
       use parametre_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_code_retour), intent(in)          :: code_retour ! code retour a analyser

character(len=pm_nom_biblio), intent(out) :: nom_biblio  ! nom de la bibliotheque a laquelle appartient 

character(len=pm_nom_routine), intent(out):: nom_routine ! nom de la routine ayant emis le code retour a analyser

character(len=pm_identification_routine), intent(out)    :: identification_routine ! role succinct de cette routine

character(len=pm_signification_code_retour), intent(out) :: signification_code_retour ! signification du champ

type(tm_code_retour), intent(out)                        :: retour_mzpro    ! code retour de mzpro_code_retour

character(len=pm_message), intent(out), optional         :: message         ! signification du champ

character(len=pm_numero_version), intent(out), optional  :: version_MSLIB90 ! version de la bibliotheque 

character(len=pm_numero_version), intent(out), optional  :: version_MSPRO   ! version de la bibliotheque 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mzpro_code_retour
     subroutine mzpro_traiter_retour ( code_retour, chaine_libre, fichier_edition, message_warn, stop_err, stop_warn, stop_pb )

       use parametre_mspro
       use mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


type(tm_code_retour), intent(in)                       :: code_retour     ! code retour de la routine posant probleme

character(len = pm_chaine_libre), intent(in), optional :: chaine_libre    ! message utilisateur

integer, intent(in), optional                          :: fichier_edition ! numero d'unite logique du fichier d'impression des messages

logical, intent(in), optional                          :: message_warn    ! indicateur d'affichage des avertissements

logical, intent(in), optional                          :: stop_err        ! indicateur d'arret en cas d'erreur

logical, intent(in), optional                          :: stop_warn       ! indicateur d'arret en cas de warning

logical, intent(in), optional                          :: stop_pb         ! indicateur d'arret si la routine mzpro_traiter_retour connait elle-meme un probleme.


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mzpro_traiter_retour
end interface

end module int_codes_retour
