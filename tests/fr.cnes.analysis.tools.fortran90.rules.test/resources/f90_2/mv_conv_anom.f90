subroutine mv_conv_anom (e, type_anom1, anom1, type_anom2, anom2, code_retour)

! (C) Copyright CNES - MSLIB - 2004

!************************************************************************
!
! But:  Conversion d'anomalies excentrique, moyenne et vraie 
! ===   dans les cas elliptique, hyperbolique, et parabolique
!
! Note d'utilisation:  On utilise le terme d'anomalie excentrique E 
! ==================   pour le cas hyperbolique (H) et parabolique (D = tan(v/2))
!
!                      La conversion anomalie moyenne --> anomalie excentrique
!                      peut aussi se faire par mv_kepler_bar pour les 3 types de coniques.
!$Historique
! ==========
!   + Version 6.2 : creation 
!     (Date: 10/2004 - Realisation: Veronique Lepine, Guylaine Prat, Bruno Revelin)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : DM-ID 616 remplacement du module code_anomalies_mslib
!     par une sélection de int_chgmnt_variables
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

! Modules
! =======
use int_var_internes, only : mvi_conv_anom_ellip
use int_var_internes, only : mvi_conv_anom_hyperb
use int_var_internes, only : mvi_conv_anom_parab

use int_chgmnt_variables, only : pm_anom_E,pm_anom_v,pm_anom_M
use test_mslib

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in)         :: e          ! excentricite
integer, intent(in)               :: type_anom1 ! type de l'anomalie a convertir
real(pm_reel), intent(in)         :: anom1      ! valeur de l'anomalie a convertir
integer, intent(in)               :: type_anom2 ! type de l'anomalie de sortie
real(pm_reel), intent(out)        :: anom2      ! valeur de l'anomalie de sortie
type(tm_code_retour), intent(out) :: code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================
integer    ::  retour

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSLIB mv_conv_anom.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mv_conv_anom.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisations
! ===============
code_retour%valeur = pm_OK

! Verifications
! =============

if (type_anom1 == type_anom2) then ! conversion identite

   ! les 2 clefs sont identiques: il suffit de tester la validite d'une seule
   if (type_anom1 /= pm_anom_E .AND. type_anom1 /= pm_anom_M &
                               .AND. type_anom1 /= pm_anom_v) then
      code_retour%valeur = pm_err_type_anom
      go to 6000
   end if

   ! pas de conversion a effectuer
   code_retour%valeur = pm_warn_conv_identite
   anom2 = anom1
   go to 6000

end if

if (e < 0._pm_reel) then
   code_retour%valeur = pm_err_e_negatif
   go to 6000
end if

! calculs
! =======

if (e <= (1._pm_reel - pm_eps_parab)) then ! cas elliptique

   call mvi_conv_anom_ellip (e, type_anom1, anom1, type_anom2, anom2, retour)

else if (e >= (1._pm_reel + pm_eps_parab))  then ! cas hyperbolique

   call mvi_conv_anom_hyperb (e, type_anom1, anom1, type_anom2, anom2, retour)

else ! cas parabolique

   call mvi_conv_anom_parab (type_anom1, anom1, type_anom2, anom2, retour)

end if

! affectation du code retour
code_retour%valeur = retour

6000 continue

code_retour%routine = pm_num_mv_conv_anom
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mv_conv_anom

