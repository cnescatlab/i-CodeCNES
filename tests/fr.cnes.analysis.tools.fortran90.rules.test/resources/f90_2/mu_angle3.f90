subroutine mu_angle3 (vect_a, vect_b, angle, code_retour)

! (C) Copyright CNES - MSLIB - 1998-2003

!************************************************************************
!
! But:  Calcul de l'angle non oriente de deux vecteurs a et b de R3. 
! ===
!
! Note d'utilisation: L'angle calcule est exprime en radians et appartient a l'intervalle [0,pi]
! ==================
!
!$Historique
! ==========
!   + Version 1.0 (SP 203 ed01 rev00): creation a partir de la routine MUAVES de la MSLIB f77
!                         (Date: 05/1998 - Realisation: Veronique Lepine)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Veronique Lepine)
!   + Version 6.3 (DM-ID 239) : Performances en temps de calcul
!                 (Date: 10/2005 - Realisation: ATOS ORIGIN) 
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : DM-ID 616 remplacement du module math_mslib
!     par une sélection de int_constantes
!                   (Date: 05/2007 - Realisation: Atos origin)
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
use int_constantes, only : pm_pi,pm_deux_pi,pm_pi_sur2,pm_deg_rad,pm_rad_deg

use int_utilitaires, only : mu_angle2
use int_utilitaires, only : mu_prod_vect
use int_utilitaires, only : mu_norme
use int_util_internes, only : mui_dot_product3

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
real(pm_reel), dimension(3), intent(in) :: vect_a ! vecteur a
real(pm_reel), dimension(3), intent(in) :: vect_b ! vecteur b

real(pm_reel), intent(out)              :: angle  ! angle non oriente entre les vecteurs
type(tm_code_retour), intent(out)       :: code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------
real(pm_reel) :: rscal,rvect      !produit scalaire des deux vecteurs en entree et norme de leur produit vectoriel
real(pm_reel), dimension(3) :: rc !produit vectoriel des deux vecteurs en entree
real(pm_reel) :: res
real(pm_reel) :: retour

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mu_angle3.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mu_angle3.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

! calcul du produit scalaire et du produit vectoriel
! ==================================================
call mui_dot_product3 ( vect_a , vect_b , rscal , retour )

call mu_prod_vect(vect_a, vect_b, rc, code_retour)       ! produit vectoriel 
                                                         ! pas de code retour a tester car toujours nul dans mu_prod_vect
                    
call mu_norme(rc,rvect,code_retour)  ! norme du produit vectoriel (pas de code retour en erreur)

! calcul de l'angle non oriente entre 0 et pi (sin(angle) >= 0)
! =============================================================

call mu_angle2 (rscal,rvect,res,code_retour)
if (code_retour%valeur == pm_err_vect_nul) then
   angle = 0._pm_reel
else
   angle = res
end if

code_retour%routine = pm_num_mu_angle3
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mu_angle3
