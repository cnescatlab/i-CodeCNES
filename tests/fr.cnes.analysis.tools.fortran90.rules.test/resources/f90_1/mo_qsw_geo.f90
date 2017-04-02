subroutine mo_qsw_geo (pos_car, vit_car, vect_qsw, vect_geo, code_retour)

! (C) Copyright CNES - MSLIB - 1998

!************************************************************************
!
! But:  Passage dans le repere geocentrique inertiel d'un vecteur exprime dams le 
! ===   repere orbital local (q,s,w).
!
! Note d'utilisation: La transformation inverse peut se faire par mo_geo_qsw
! ==================   
!
!$Historique
! ==========
!   + Version 1.0 (SP 219 ed01 rev00): creation a partir de la routine MVRA0R de la MSLIB f77
!                         (Date: 07/1998 - Realisation: Veronique Lepine)
!   + Version 2.0 (DE 370 ed01 rev00): utilisation de la routine mo_def_qsw
!                                      suppression du parametre eps100 (non utilise)
!                         (Date: 08/1999 - Realisation: Sylvain Vresk)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Bruno Revelin, Veronique Lepine)
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
use int_rep_orbitaux, only : mo_def_qsw

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel),        dimension(3), intent(in)  :: pos_car     ! position du satellite en coordonnees cartesiennes dans le repere geocentrique inertiel
real(pm_reel),        dimension(3), intent(in)  :: vit_car     ! idem pour la vitesse 
real(pm_reel),        dimension(3), intent(in)  :: vect_qsw    ! projections (Gq,Gs,Gw) du vecteur G suivant les axes du repere orbital local (q,s,w)
real(pm_reel),        dimension(3), intent(out) :: vect_geo    ! composantes (Gx,Gy,Gz) du vecteur G dans le repere geocentrique inertiel
type(tm_code_retour),               intent(out) :: code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel), dimension(3) :: q, s, w                        ! directions du repere orbital local
type(tm_code_retour)        :: code_retour_local

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mo_qsw_geo.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mo_qsw_geo.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

call mo_def_qsw ( pos_car, vit_car, q, s, w, code_retour_local)
if (code_retour_local%valeur < 0) then
   code_retour%valeur = code_retour_local%valeur
   go to 6000
else
   code_retour%valeur = code_retour_local%valeur
end if

! calcul des composantes du vecteur dans le repere geocentrique inertiel

vect_geo(:) = vect_qsw(1)*q(:) + vect_qsw(2)*s(:) + vect_qsw(3)*w(:)

6000 continue

code_retour%routine = pm_num_mo_qsw_geo
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mo_qsw_geo
