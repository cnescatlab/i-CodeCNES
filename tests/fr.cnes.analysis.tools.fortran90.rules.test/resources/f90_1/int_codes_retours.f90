module int_codes_retours

! (C) Copyright CNES - MSLIB - 2007
!************************************************************************
!
! But:  Definition des constantes et interface des fonctions du thème Z
! ===
!
! Note d'utilisation:
! ==================
!   Module en principe utilisé uniquement par l'intermédiaire du module global
!   "mslib90"
!
!$Historique
! ==========
!   
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!
!   + Version 6.6 : rajout du cartouche
!                   (Date: 05/2007 - Realisation: Atos origin)
!
!VERSION:V6.13:FA-ID:1410:30/09/2010:Ajout marqueur fin historique
!
!Revision 362 2013/02/15 bbjc
!DM-ID 1513: Suppression des warnings de compilation
!
!$FinHistorique
!
!************************************************************************

use longueur_chaine_mslib
implicit none

! SVN Source File Id
  character(len=256), private, parameter :: SVN_VER =  '$Id: int_codes_retours.f90 362 2013-02-15 18:01:28Z bbjc $'

public
interface
     subroutine mz_numero_routine (routine, nom, identification, code_retour)

       use type_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                                       :: routine        ! numero de la routine
character(len=pm_nom_routine),intent(out)                 :: nom            ! nom de la routine
character(len=pm_identification_routine),intent(out)      :: identification ! role succinct de la routine
type(tm_code_retour), intent(out)                         :: code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mz_numero_routine
     subroutine mz_val_code_retour (valeur, signification, code_retour)

       use type_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
integer, intent(in)                                      :: valeur        ! valeur a traiter
character(len=pm_signification_code_retour), intent(out) :: signification ! chaine indiquant la signification du code retour
type(tm_code_retour), intent(out)                        :: code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     end subroutine mz_val_code_retour
end interface

  character(len=pm_longueur_rcs_id), private, parameter :: &
  rcs_id =' $Id: int_codes_retours.f90 362 2013-02-15 18:01:28Z bbjc $ '

end module int_codes_retours
