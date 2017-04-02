subroutine mu_norme (vect, norme, code_retour ,vect_norme)

! (C) Copyright CNES - MSLIB - 1998

!************************************************************************
!
! But:  Norme euclidienne d'un vecteur dans l'espace
! ===
!
!$Historique
! ==========
!   + Version 1.0 (SP 204 ed01 rev00): creation a partir de la routine MUNORM de la MSLIB f77
!                         (Date: 05/1998 - Realisation: Veronique Lepine)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Bruno Revelin, Veronique Lepine)
!   + Version 6.3 (DM-ID 239) : Performances en temps de calcul
!                 (Date: 10/2005 - Realisation: ATOS ORIGIN) 
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
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

real(pm_reel), dimension(3), intent(in)              :: vect        ! vecteur a normer

real(pm_reel), intent(out)                           :: norme       ! norme du vecteur en entree
type(tm_code_retour), intent(out)                    :: code_retour
real(pm_reel),dimension(3),intent(out), optional     :: vect_norme  ! vecteur en entree norme                  

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel) :: presque_zero   !variable de test des reels
real(pm_reel) :: norme_carre    !carre de la norme du vecteur vect en entree
real(pm_reel) :: norme_vect     !norme du vecteur vect en entree (variable intermediaire)
real(pm_reel) :: retour

intrinsic tiny, sqrt, present

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mu_norme.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mu_norme.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

! initialisation constante de test
!.................................

presque_zero = tiny(1._pm_reel)  ! recherche du plus petit reel positif non nul

! -----------------------------
! calcul de la norme du vecteur 
! -----------------------------
call mui_dot_product3 ( vect , vect , norme_carre , retour )

norme_vect = sqrt(norme_carre)
norme = norme_vect

! --------------------------------------------
! calcul optionnel du vecteur norme vect_norme
! --------------------------------------------

if (present(vect_norme)) then          !     Calcul du vecteur norme.
   if (norme_vect < presque_zero) then !     test si la norme = 0
      code_retour%valeur = pm_err_vect_nul
      go to 6000
   else
      vect_norme(:) = vect(:)/norme_vect
   end if
end if

6000 continue

code_retour%routine = pm_num_mu_norme
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mu_norme
