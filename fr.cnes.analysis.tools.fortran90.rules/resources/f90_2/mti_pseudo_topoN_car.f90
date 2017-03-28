subroutine mti_pseudo_topoN_car (r_equa, apla, pos_geod, vit_pseudo_topoN, pos_car, vit_car, retour, jacobtopoNcar )

! (C) Copyright CNES - MSPRO - 2001-2003
!************************************************************************
!
! But: Passage du repere pseudo topocentrique Nord aux coordonnees 
! ===  cartesiennes dans le repere de reference. 
! 
!
! Note d'utilisation:
! ==================
!  Pour plus d'explications: se reporter a la documentation utilisateur
!                                     et a la note algorithmique.
!   
!$Historique
! ===========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 2.0 : creation a partir de rien
!                         (Date: 01/2002 - Realisation: Mickael Hazak et Guylaine Prat)
!   + Version 3.1 (DE globale 4) : Modifications suite aux remarques qualite ATV
!                         (Date: 07/2003 - Realisation: Bruno Revelin)
!   + Version 3.1 (DE 1) : Utilisation du calcul de jacobienne de mt_geod_car
!                         (Date: 08/2003 - Realisation: Bruno Revelin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======
use mslib

use type_mspro
use valeur_code_retour_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       
real(pm_reel), intent(in)                                :: r_equa        ! rayon equatorial
real(pm_reel),intent (in)                                :: apla          ! aplatissement 
type(tm_geodesique), intent(in)                          :: pos_geod          ! position geodesique             
real(pm_reel),dimension (3), intent(in)                  :: vit_pseudo_topoN  ! vitesse dans le repere pseudo topo Nord
 
real(pm_reel),dimension (3), intent(out)                 :: pos_car       ! position cartesienne
real(pm_reel),dimension (3), intent(out)                 :: vit_car       ! vitesse cartesienne        
integer, intent(out)                                     :: retour

real(pm_reel), dimension (6,6), intent(out), optional    :: jacobtopoNcar  ! jacobien de la transformation
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------
real(pm_reel), dimension(3) ::  vecti,vectj,vectk ! composantes des vecteurs directeurs du repere pseudo "topocentrique" Nord 

real(pm_reel), dimension(3,3) :: jacob_geod_car   ! jacobienne du bloc positions fournie par mt_geod_car

type(tm_code_retour) :: code_local                ! code retour local

intrinsic present

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSPRO  mti_pseudo_topoN_car.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation
! ..............
retour = pm_OK

! controle des donnees d'entree
! .............................

if (apla >= 1._pm_reel) then    ! aplatissement  superieur ou egal a 1
   retour = pm_err_apla_sup1
   go to 6000
end if

! Calcul des coordonnees cartesiennes de l'origine du repere pseudo "topocentrique"
! ==================================================================================
! (confondu avec le point considere en entree)

if (present(jacobtopoNcar)) then
   call mt_geod_car ( pos_geod, r_equa, apla, pos_car, code_local, jacob = jacob_geod_car ) 
else
   call mt_geod_car ( pos_geod, r_equa, apla, pos_car, code_local ) 
end if
! aucun test sur le code retour necessaire (apla deja teste)

! Calcul des composantes de la vitesse cartesienne dans le repere de reference
! ============================================================================

! calcul des composantes des vecteurs (u,v,w) constituant
! le repere pseudo "topocentrique" (vecteur exprimes dans le repere de reference) 
! ..............................................................................

call mt_def_topo_N (pos_geod%lat,pos_geod%long,vecti,vectj,vectk,code_local)
! aucun test sur le code retour, le code retour est toujours OK

! calcul des composantes de la vitesse cartesienne dans le repere de reference
! ............................................................................

vit_car(:) = vecti(:) * vit_pseudo_topoN(1) + vectj(:) * vit_pseudo_topoN(2) + vectk(:) * vit_pseudo_topoN(3) 

! Calcul du jacobien analytique (pour tous les aplatissements)
!=============================================================

if (present(jacobtopoNcar)) then

   jacobtopoNcar(1:3,1:3) = jacob_geod_car(:,:)

   jacobtopoNcar(1:3,4:6)= 0._pm_reel

   jacobtopoNcar(4:6,3)= 0._pm_reel

   jacobtopoNcar(4,1)= - vectk(1)* vit_pseudo_topoN(1)+ vecti(1) * vit_pseudo_topoN(3)
   jacobtopoNcar(4,2)= - (vecti(2)* vit_pseudo_topoN(1)+ vectj(2)* vit_pseudo_topoN(2)+ vectk(2)* vit_pseudo_topoN(3))

   jacobtopoNcar(5,1)= - vectk(2)* vit_pseudo_topoN(1)+ vecti(2) * vit_pseudo_topoN(3)
   jacobtopoNcar(5,2)= vecti(1)* vit_pseudo_topoN(1)+ vectj(1)* vit_pseudo_topoN(2)+ vectk(1)* vit_pseudo_topoN(3)

   jacobtopoNcar(6,1)= - vectk(3)* vit_pseudo_topoN(1)+vecti(3)*vit_pseudo_topoN(3)
   jacobtopoNcar(6,2)= 0._pm_reel

   jacobtopoNcar(4,4)= vecti(1)
   jacobtopoNcar(4,5)= vectj(1) 
   jacobtopoNcar(4,6)= vectk(1)

   jacobtopoNcar(5,4)= vecti(2) 
   jacobtopoNcar(5,5)= vectj(2)
   jacobtopoNcar(5,6)= vectk(2)

   jacobtopoNcar(6,4)= vecti(3)   
   jacobtopoNcar(6,5)= vectj(3)    ! vectj(3) = 0 par definition
   jacobtopoNcar(6,6)= vectk(3)

end if

6000 continue

end subroutine mti_pseudo_topoN_car
