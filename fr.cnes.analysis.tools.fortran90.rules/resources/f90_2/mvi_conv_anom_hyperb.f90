subroutine mvi_conv_anom_hyperb (e, type_anom1, anom1, type_anom2, anom2, retour)

! (C) Copyright CNES - MSLIB - 2005

!************************************************************************
!
! But:  Conversion d'anomalies dans le cas hyperbolique
! ===
!
! Note d'utilisation:  l'appelant doit avoir teste que e > 1 (strictement)
! ==================   Aucun test n'est effectue ici.
!
!$Historique
! ==========
!   + Version 6.2 : creation
!     (Date: 01/2005 - Realisation: Guylaine Prat, Bruno Revelin et Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : DM-ID 616 remplacement du module code_anomalies_mslib) par 
!     une sélection de int_chgmnt_variables
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
use int_var_internes, only : mvi_kepler_hyperb

use int_chgmnt_variables, only : pm_anom_E,pm_anom_v,pm_anom_M

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in) :: e          ! excentricite
integer, intent(in)       :: type_anom1 ! type de l'anomalie d'entree
real(pm_reel), intent(in) :: anom1      ! anomalie d'entree
integer, intent(in)       :: type_anom2 ! type d'anomalie demande
real(pm_reel), intent(out):: anom2      ! anomalie demandee
integer, intent(out)      :: retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations: variables intermediaires
! -------------------

real(pm_reel) :: moins_un_sur_e, tan_vs2, anom_H ! variables intermediaires
real(pm_reel) :: tanh_Hs2, rapport_tanh_Hs2, pseudo_beta ! variables intermediaires

intrinsic tanh, sinh, sqrt, cos, tan, atan, log

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSLIB mvi_conv_anom_hyperb.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mvi_conv_anom_hyperb.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation
! ..............
retour = pm_OK

! avec e > 1
pseudo_beta = sqrt((e + 1._pm_reel)/(e - 1._pm_reel))

! calculs selon les types d'anomalies
! ...................................

select case (type_anom1)

case (pm_anom_E)  ! conversion d'une anomalie excentrique H

   select case (type_anom2)

   case (pm_anom_M)
      anom2 = e*sinh(anom1) - anom1 ! equation de Kepler pour l'hyperbole

   case(pm_anom_v)
      tan_vs2 = pseudo_beta*tanh(anom1/2._pm_reel)
      anom2 = 2._pm_reel*atan(tan_vs2) ! resultat dans ]-pi, +pi[

   case default ! type_anom2 incorrect
      retour = pm_err_type_anom

   end select

case (pm_anom_v)  ! conversion d'une anomalie vraie

   ! test de compatibilite de v avec l'excentricite e
   ! il faut que : cos(v) > ou = -1/e avec e > 1
   moins_un_sur_e = - 1._pm_reel/e

   if (cos(anom1) < moins_un_sur_e) then ! attention pas de valeur absolue
      retour = pm_err_anom_v_incompatible_e
      go to 6000
   end if

   ! avec e > 1 et cos(v) > -1/e ==> tan(v/2) n'est pas infini
   tanh_Hs2 = tan(anom1/2._pm_reel) / pseudo_beta
   rapport_tanh_Hs2 = (1._pm_reel + tanh_Hs2) / (1._pm_reel - tanh_Hs2)
   anom_H = log(rapport_tanh_Hs2)

   select case (type_anom2)

   case (pm_anom_M)
      anom2 = e*sinh(anom_H) - anom_H ! equation de Kepler pour l'hyperbole

   case(pm_anom_E)
      anom2 = anom_H     

   case default ! type_anom2 incorrect
      retour = pm_err_type_anom

   end select

case (pm_anom_M)  ! conversion d'une anomalie moyenne

   call mvi_kepler_hyperb (anom1, e, anom_H, retour)
   if (retour /= pm_OK) go to 6000

   select case (type_anom2)

   case (pm_anom_v)
      tan_vs2 = pseudo_beta*tanh(anom_H/2._pm_reel)
      anom2 = 2._pm_reel* atan(tan_vs2) ! resultat dans ]-pi, +pi[

   case(pm_anom_E)
      anom2 = anom_H

   case default ! type_anom2 incorrect
      retour = pm_err_type_anom

   end select

case default ! type_anom1 incorrect

   retour = pm_err_type_anom

end select

6000 continue

end subroutine mvi_conv_anom_hyperb
