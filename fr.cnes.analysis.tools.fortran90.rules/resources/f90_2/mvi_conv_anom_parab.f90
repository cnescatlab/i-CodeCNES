subroutine mvi_conv_anom_parab (type_anom1, anom1, type_anom2, anom2, retour)

! (C) Copyright CNES - MSLIB - 2005

!************************************************************************
!
! But:  Conversion d'anomalies dans le cas parabolique
! ===
!
! Note d'utilisation:  
! ==================
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
use int_var_internes, only : mvi_barker

use int_chgmnt_variables, only : pm_anom_E,pm_anom_v,pm_anom_M

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)        :: type_anom1 ! type de l'anomalie d'entree
real(pm_reel), intent(in)  :: anom1      ! anomalie d'entree
integer, intent(in)        :: type_anom2 ! type d'anomalie demande
real(pm_reel), intent(out) :: anom2      ! anomalie demandee
integer, intent(out)       :: retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel) :: anom_D ! variables intermediaires de calcul

intrinsic atan, tan

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSLIB mvi_conv_anom_parab.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mvi_conv_anom_parab.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation
! ..............
retour = pm_OK

! calculs selon les types d'anomalies
! ...................................

select case (type_anom1)

case (pm_anom_E)  ! conversion d'une anomalie "excentrique" D = tan(v/2)

   select case (type_anom2)

   case (pm_anom_M)
      anom2 = anom1*(0.5_pm_reel + anom1*anom1/6._pm_reel)

   case(pm_anom_v)
      anom2 = 2._pm_reel*atan(anom1) ! resultat dans ]-pi, +pi[

   case default ! type_anom2 incorrect
      retour = pm_err_type_anom

   end select

case (pm_anom_v)  ! conversion d'une anomalie vraie

   anom_D = tan(anom1/2._pm_reel)

   select case (type_anom2)

   case (pm_anom_M)
      anom2 = anom_D*(0.5_pm_reel + anom_D*anom_D/6._pm_reel)

   case(pm_anom_E)
      anom2 = anom_D

   case default ! type_anom2 incorrect
      retour = pm_err_type_anom

   end select

case (pm_anom_M)  ! conversion d'une anomalie moyenne

   call mvi_barker (anom1, anom_D, retour)
   if (retour /= pm_OK) go to 6000

   select case (type_anom2)

   case (pm_anom_v)
      anom2 = 2._pm_reel*atan(anom_D) ! resultat dans ]-pi, +pi[

   case(pm_anom_E)
      anom2 = anom_D

   case default ! type_anom2 incorrect
      retour = pm_err_type_anom

   end select

case default ! type_anom1 incorrect

   retour = pm_err_type_anom

end select

6000 continue

end subroutine mvi_conv_anom_parab
