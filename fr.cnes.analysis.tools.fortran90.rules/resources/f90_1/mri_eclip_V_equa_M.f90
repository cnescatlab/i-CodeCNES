subroutine mri_eclip_V_equa_M (model, jul1950_t1, jul1950_t2, mat_pass, retour)

! (C) Copyright CNES - MSLIB - 1998

!************************************************************************
!
! But:  Calcul de la matrice de passage du repere ECLIPtique VRAI a T1 au repere EQUAtorial MOYen a T2.
! ===
!
! Note d'utilisation: - Routine interne
! ==================  
!
!$Historique
! ==========
!   + Version 1.0 (SP 276 ed01 rev00): creation a partir de la routine MRCQVM de la MSLIB f77
!                         (Date: 09/1998 - Realisation: Veronique Lepine)
!   + Version 2.0 (DE 362 ed01 rev00): suppression des commentaires sur la limitation sur les dates et des codes retour
!                         (Date: 08/1999 - Realisation: Sylvain Vresk)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Bruno Revelin, Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : DM-ID 616 remplacement des modules de constantes *_mslib 
!       par le module global parametre_mslib
!                   (Date: 05/2007 - Realisation: Atos origin)
!   + Version 6.8 : DM-ID 859 : utilisation de matmul3
!                   (Date: 03/2008 - Realisation: Atos origin)
!   + Version 6.9 : DM-ID 1058 : Suppression des warnings G95
!                   (Date: 09/2008 - Realisation: Atos origin)
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
use int_rep_internes, only : mri_equa_M_eclip_V


use parametre_mslib
use type_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                 :: model      ! indicateur du modele -->pm_lieske_wahr: LIESKE + WAHR
type(tm_jour_sec), intent(in)       :: jul1950_t1 ! date du repere initial
type(tm_jour_sec), intent(in)       :: jul1950_t2 ! date du repere final

real(pm_reel), dimension(3,3), intent(out) :: mat_pass ! matrice de passage
integer, intent(out)                ::  retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel), dimension(3,3)  :: mat_pass1             ! matrice intermediaire
type(tm_code_retour) :: code_retour_local

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mri_eclip_V_equa_M.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mri_eclip_V_equa_M.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

if (model == pm_lieske_wahr) then 

   ! calcul de la matrice de passage du repere ecliptique vrai a t1 au repere equatorial moyen a t2
   ! avec le modele de precession de Lieske et le modele de nutation de Wahr (epoque de base : J2000)

   ! ===============================================================
   ! calcul de la matrice de passage du repere equatorial moyen a t2
   ! au repere  ecliptique vrai a t1
   ! ===============================================================

   call mri_equa_M_eclip_V (model, jul1950_t2, jul1950_t1, mat_pass1, retour)
   ! pas de test de retour car il ne peut etre que nul (controles deja effectues)

   ! =======================
   ! transposition de matrice
   ! =======================

   call mu_transpose3(mat_pass1,mat_pass,code_retour_local)

else    ! modele inconnu

   retour = pm_err_ind_model

end if

end subroutine mri_eclip_V_equa_M
