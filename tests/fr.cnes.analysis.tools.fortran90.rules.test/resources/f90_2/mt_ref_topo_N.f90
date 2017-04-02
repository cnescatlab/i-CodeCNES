subroutine mt_ref_topo_N (orig_topo, r_equa, apla, pos_ref, pos_topo, code_retour, vit_ref, vit_topo, jacob)

! (C) Copyright CNES - MSLIB - 1998

!************************************************************************
!
! But:  Passage du repere terrestre de REFerence a un repere TOPOcentrique Nord (convention axe Ox vers le Nord).
! ===
!
! Note d'utilisation:  La transformation inverse peut s'effectuer par mt_topo_N_ref
! ==================
!
!$Historique
! ==========
!   + Version 1.0 (SP 252 ed01 rev00): creation a partir de la routine MVGETO de la MSLIB f77
!                         (Date: 08/1998 - Realisation: Veronique Lepine)
!   + Version 2.0 (FA 347 ed01 rev00): Modification du test sur l'aplatissement
!                         (Date: 09/1999 - Realisation: Sylvain Vresk)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Bruno Revelin, Veronique Lepine)
!   + Version 6.3 (DM-ID 239) : Performances en temps de calcul
!                 (Date: 10/2005 - Realisation: ATOS ORIGIN) 
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : (Date : 02/05/2007 - Realisation: Sandrine Avril - Atos origin)
!                   DM-ID 636 : contrôle de la latitude pour la fonction mt_geoc_car
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
use int_chgmnt_reperes, only : mt_def_topo_N
use int_chgmnt_reperes, only : mt_geod_car
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

type(tm_geodesique), intent(in)                      :: orig_topo  ! coordonnees geodesiques de l'origine du repere topocentrique
real(pm_reel), intent(in)                            :: r_equa     ! rayon equatorial
real(pm_reel), intent(in)                            :: apla       ! aplatissement
real(pm_reel), dimension(3), intent(in)              :: pos_ref    ! position dans le repere terrestre de reference
real(pm_reel), dimension(3), intent(out)             :: pos_topo   ! position dans le repere topocentrique Nord
type(tm_code_retour), intent(out)                    ::  code_retour
real(pm_reel), dimension(3), intent(in), optional    :: vit_ref    ! vitesse dans le repere terrestre de reference
real(pm_reel), dimension(3), intent(out),optional    :: vit_topo   ! vitesse dans le repere topocentrique Nord
real(pm_reel), dimension(6,6), intent(out), optional :: jacob      ! jacobien de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------
real(pm_reel), dimension(3)   ::   vectu,vectv,vectw ! composantes dans le repere geocentrique des trois vecteurs (u,v,w)
real(pm_reel), dimension(3)   ::   sta               ! coordonnees cartesiennes de l'origine du repere topocentrique
real(pm_reel), dimension(3)   ::   pos               ! coordonnees intermediaires de calcul
real (pm_reel)                ::   retour

intrinsic  present

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mt_ref_topo_N.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mt_ref_topo_N.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

! controle des donnees d'entree
! .............................
if (apla >= 1._pm_reel) then    ! aplatissement terrestre superieur ou egal a 1
   code_retour%valeur = pm_err_apla_sup1
   go to 6000
end if

if (r_equa <= 1._pm_reel) then ! rayon équatorial inférieur ou egal à 1
   code_retour%valeur = pm_err_r_equa_inf_egal1
   go to 6000
end if

! -------------------------------------------------------
! calcul des composantes des vecteurs (u,v,w) constituant
! le repere topocentrique de la station 
! -------------------------------------------------------
call mt_def_topo_N (orig_topo%lat,orig_topo%long,vectu,vectv,vectw,code_retour) 
! code retour toujours OK

!-------------------------------------------------------------------------
! calcul des coordonnees cartesiennes de l'origine du repere topocentrique
!-------------------------------------------------------------------------   
call mt_geod_car(orig_topo, r_equa, apla, sta, code_retour)
! code retour toujours OK puisque apla deja teste.

pos(:) = pos_ref(:) - sta(:)!calcul des variables intermediaires : 
                            !positions du satellite -coordonnees de la station dans le repere geocentrique terrestre

!-------------------------------------------------------
! calcul des composantes du vecteur position du satellite
! dans le repere topocentrique
!-------------------------------------------------------
call mui_dot_product3 ( vectu , pos , pos_topo(1) , retour )
call mui_dot_product3 ( vectv , pos , pos_topo(2) , retour )
call mui_dot_product3 ( vectw , pos , pos_topo(3) , retour )

!---------------------------------------------------
! calculs optionnels
!---------------------------------------------------
!---------------------------------------------------------
! calcul optionel du jacobien : calcul des derivees partielles des 
! vecteurs position et vitesse
!---------------------------------------------------------

if (present(jacob)) then

   jacob(:,:) = 0._pm_reel

   jacob(1,1:3) = vectu(:)   !     derivee partiellle de x 
   jacob(2,1:3) = vectv(:)   !     derivee partiellle de y 
   jacob(3,1:3) = vectw(:)   !     derivee partiellle de z 
   jacob(4,4:6) = vectu(:)   !     derivee partiellle de vx 
   jacob(5,4:6) = vectv(:)   !     derivee partiellle de vy
   jacob(6,4:6) = vectw(:)   !     derivee partiellle de vz 

end if

! verification du parametrage pour le calcul de la vitesse
!=========================================================

if (present(vit_topo).and..not.present(vit_ref)) then  ! mauvais parametrage

   code_retour%valeur = pm_err_para_option
   go to 6000

else if (present(vit_topo).and.present(vit_ref)) then  ! calcul optionel des composantes du vecteur vitesse du satellite
                                                       ! dans le repere topocentrique

      call mui_dot_product3 ( vectu , vit_ref , vit_topo(1) , retour )
      call mui_dot_product3 ( vectv , vit_ref , vit_topo(2) , retour )
      call mui_dot_product3 ( vectw , vit_ref , vit_topo(3) , retour )

end if
if (present(vit_ref).and..not.present(vit_topo)) code_retour%valeur = pm_warn_para_option

6000 continue

code_retour%routine = pm_num_mt_ref_topo_N
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mt_ref_topo_N
