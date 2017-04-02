subroutine mu_prod_vect (vect_a, vect_b, vect_c, code_retour)

! (C) Copyright CNES - MSLIB - 1998-2003

!************************************************************************
!
! But:  Dans un repere orthonorme, calcul du produit vectoriel de deux vecteurs.
! ===  
!
!$Historique
! ==========
!   + Version 1.0 (SP 205 ed01 rev00): creation a partir de la routine MUPVEC de la MSLIB f77
!                         (Date: 05/1998 - Realisation: Veronique Lepine)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
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

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel), dimension(3), intent(in)               :: vect_a  ! vecteur a
real(pm_reel), dimension(3), intent(in)               :: vect_b  ! vecteur b

real(pm_reel), dimension(3), intent(out)              :: vect_c  ! vecteur produit vectoriel c = a x b
type(tm_code_retour), intent(out)                     :: code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mu_prod_vect.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mu_prod_vect.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

!     ==================================================
! calcul du produit vectoriel vect_c = vect_a * vect_b
!          ( a1            ( b1            ( c1 = a2*b3 - a3*b2
!   vect_a ( a2  *  vect_b ( b2  =  vect_c ( c2 = a3*b1 - a1*b3
!          ( a3            ( b3            ( c3 = a1*b2 - a2*b1
!     ==================================================

vect_c(1) = (vect_a(2) * vect_b(3)) - (vect_a(3) * vect_b(2))
vect_c(2) = (vect_a(3) * vect_b(1)) - (vect_a(1) * vect_b(3))
vect_c(3) = (vect_a(1) * vect_b(2)) - (vect_a(2) * vect_b(1))

code_retour%routine = pm_num_mu_prod_vect
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mu_prod_vect
