subroutine mo_def_qsw (pos_car, vit_car, q, s, w, code_retour)

! (C) Copyright CNES - MSLIB - 1999-2003

!************************************************************************
!
! But:  calcul des coordonnees des vecteurs directeurs du repere orbital 
! ===   local (q,s,w), dans un repere geocentrique inertiel
!
!$Historique
! ==========
!   + Version 2.0 (SP 335 ed01 rev00): creation a partir de la routine MVAXEQSW de la MSLIB f77
!                         (Date: 07/1999 - Realisation: Sylvain Vresk)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
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
use int_utilitaires, only : mu_prod_vect
use int_utilitaires, only : mu_norme

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), dimension(3), intent (in)  :: pos_car      ! position du satellite dans un repere inertiel
real(pm_reel), dimension(3), intent (in)  :: vit_car      ! vitesse du satellite dans un repere inertiel
real(pm_reel), dimension(3), intent (out) :: q            ! vecteur unitaire q du repere orbital local exprime
real(pm_reel), dimension(3), intent (out) :: s            ! vecteur unitaire s du repere orbital local exprime
real(pm_reel), dimension(3), intent (out) :: w            ! vecteur unitaire w du repere orbital local exprime
type(tm_code_retour), intent(out)         ::  code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------
real(pm_reel)               :: norme
real(pm_reel), dimension(3) :: pos(3)
real(pm_reel), dimension(3) :: vit(3)
real(pm_reel), dimension(3) :: wo(3)
real(pm_reel), dimension(3) :: q_intermediaire(3)
real(pm_reel), dimension(3) :: s_intermediaire(3)
real(pm_reel), dimension(3) :: w_intermediaire(3)
type(tm_code_retour)        :: code_retour_intermediaire

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mo_def_qsw.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mo_def_qsw.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK
pos(:) = pos_car(:)
vit(:) = vit_car(:)

! calcul du vecteur q_intermediaire tangent au vecteur position
! .............................................................
call mu_norme ( pos, norme, code_retour_intermediaire, vect_norme=q_intermediaire )

if (code_retour_intermediaire%valeur < 0) then
   code_retour%valeur = code_retour_intermediaire%valeur
   go to 6000
else
   code_retour%valeur = code_retour_intermediaire%valeur
end if

! calcul du produit vectoriel p^v
! ...............................
call mu_prod_vect (pos, vit, wo, code_retour_intermediaire) ! pas de test du code retour : toujours nul

! calcul du vecteur w_intermediaire designant la direction du moment cinetique
! ............................................................................
call mu_norme ( wo, norme, code_retour_intermediaire, vect_norme=w_intermediaire )

if (code_retour_intermediaire%valeur < 0) then
   code_retour%valeur = code_retour_intermediaire%valeur
   go to 6000
else
   code_retour%valeur = code_retour_intermediaire%valeur
end if

! calcul du produit vectoriel s_intermediaire=w_intermediaire^q_intermediaire
! ...........................................................................
call mu_prod_vect ( w_intermediaire, q_intermediaire, s_intermediaire, code_retour_intermediaire )  ! pas de test du code retour : toujours nul

! Affectation des variables en sortie
! ===================================
q(:) = q_intermediaire(:)
s(:) = s_intermediaire(:)
w(:) = w_intermediaire(:)

6000 continue

code_retour%routine = pm_num_mo_def_qsw
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mo_def_qsw
