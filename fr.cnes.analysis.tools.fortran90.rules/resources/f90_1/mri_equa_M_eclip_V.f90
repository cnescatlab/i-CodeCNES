subroutine mri_equa_M_eclip_V (model, jul1950_t1, jul1950_t2, mat_pass, retour)

! (C) Copyright CNES - MSLIB - 1998-2003

!************************************************************************
!
! But:  Calcul de la matrice de passage du repere EQUAtorial MOYen a T1 au repere ECLIPtique VRAI a T2.
! ===
!
! Note d'utilisation: - Routine interne
! ================== 
!
!$Historique
! ==========
!   + Version 1.0 (SP 267 ed01 rev00): creation a partir de la routine MRQCMV de la MSLIB f77
!                         (Date: 09/1998 - Realisation: Veronique Lepine)
!   + Version 2.0 (DE 362 ed01 rev00): suppression des commentaires sur la limitation sur les dates et des codes retour
!                         (Date: 08/1999 - Realisation: Sylvain Vresk)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : DM-ID 616 remplacement des modules de constantes *_mslib 
!       par le module global parametre_mslib
!                   (Date: 05/2007 - Realisation: Atos origin)
!   + Version 6.8 : DM-ID 859 : utilisation de matmul3
!                   (Date: 03/2008 - Realisation: Atos origin)
!   + Version 6.9 : DM-ID 1058 : Suppression des warnings G95
!                   (Date: 09/2008 - Realisation: Atos origin)

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
use int_rep_internes, only : mri_equa_M_eclip_M
use int_rep_fondamentaux, only : mr_nuta

use type_mslib
use parametre_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                 :: model      ! indicateur du modele --> pm_lieske: LIESKE 
type(tm_jour_sec), intent(in)       :: jul1950_t1 ! date du repere initial
type(tm_jour_sec), intent(in)       :: jul1950_t2 ! date du repere final

real(pm_reel), dimension(3,3), intent(out) :: mat_pass ! matrice de passage
integer, intent(out)                ::  retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel), dimension(3,3)  ::  mat_pass1,mat_pass2  ! matrices intermediaires
type(tm_nuta)                  ::  nuta                 ! nutation en longitude et en obliquite
type(tm_code_retour)           ::  code_retour, code_retour_local
integer                        ::  modele_precession, modele_nutation  ! indicateurs des modeles de precession et de nutation
                                                                       ! associes a "model" en entree

intrinsic cos, sin

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mri_equa_M_eclip_V.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mri_equa_M_eclip_V.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

if (model == pm_lieske_wahr) then ! calcul de la matrice de passage du repere equatorial moyen a t1 au repere ecliptique vrai a t2
                                 ! avec le modele de precession de Lieske et le modele de nutation de Wahr (epoque de base : J2000)
   modele_precession = pm_lieske
   modele_nutation   = pm_wahr

   ! ===============================================================
   ! calcul de la matrice de passage du repere equatorial moyen a t1
   ! au repere  ecliptique moyen a t2
   ! ===============================================================

   call mri_equa_M_eclip_M (modele_precession, jul1950_t1, jul1950_t2, mat_pass1, retour)
   ! pas de test de retour car il ne peut etre que nul (controles deja effectues)

   ! ===========================
   ! nutation en longitude a t2
   ! ===========================

   call mr_nuta (modele_nutation, jul1950_t2, nuta, code_retour)
   ! pas de test de retour car il ne peut etre que nul (controles deja effectues)

   ! ===================
   ! matrice de nutation
   ! ===================

   mat_pass2(1,1) = cos( nuta%long )
   mat_pass2(1,2) =-sin( nuta%long )
   mat_pass2(1,3) = 0._pm_reel

   mat_pass2(2,1) = sin( nuta%long )
   mat_pass2(2,2) = cos( nuta%long )
   mat_pass2(2,3) = 0._pm_reel

   mat_pass2(3,1) = 0._pm_reel
   mat_pass2(3,2) = 0._pm_reel
   mat_pass2(3,3) = 1._pm_reel

   !     =================
   ! produit matriciel
   !     =================
   call mu_matmul3(mat_pass2,mat_pass1,mat_pass,code_retour_local)
   ! pas d'erreur possible donc pas de test dur code retour

else   !  modele inconnu

   retour = pm_err_ind_model

end if

end subroutine mri_equa_M_eclip_V
