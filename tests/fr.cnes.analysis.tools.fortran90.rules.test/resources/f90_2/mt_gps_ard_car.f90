subroutine mt_gps_ard_car (r_equa, apla, pos_geod, vit_gps, pos_car, vit_car, code_retour, jacob)

! (C) Copyright CNES - MSPRO - 2001

!************************************************************************
!
! But: Passage d'un point GPS ARD aux position-vitesse en coordonnees 
! ===  cartesiennes dans le repere de reference. 
!       
!   
! Note d'utilisation:  
! ===================   
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
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 5.9 : DM-ID 859 : utilisation de mu_matmul6
!                   (Date: 03/2008 - Realisation: Atos origin)
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======
use mslib

use type_mspro

use int_rep_internes_mspro, only : mti_pseudo_topoN_car

use valeur_code_retour_mspro
use numero_routine_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       
real(pm_reel), intent(in)                      :: r_equa   ! rayon equatorial
real(pm_reel),intent (in)                      :: apla     ! aplatissement 
type(tm_geodesique), intent(in)                :: pos_geod ! position geodesique
type(tm_vit_gps_ard), intent(in)               :: vit_gps  ! vitesse gps ARD        
real(pm_reel),dimension (3), intent(out)       :: pos_car  ! position cartesienne                                       
real(pm_reel),dimension (3), intent(out)       :: vit_car  ! vitesse cartesienne        
type(tm_code_retour), intent(out)              :: code_retour
real(pm_reel), dimension (6,6), intent(out), optional    :: jacob ! jacobien de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------
! concernant calcul pseudo-topoN > ref 
integer                       ::   retour                      ! code retour
real(pm_reel), dimension(3)   ::   vit_pseudo_topoN            ! vecteur vitesse dans le repere pseudo topo Nord              

real(pm_reel), dimension(6,6) :: jacobGPStopoN, jacobtopoNcar  ! jacobiennes GPS -> topo Nord, topo Nord -> cartesien
type(tm_code_retour)          :: code_retour_local

intrinsic present

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSPRO mt_gps_ard_car.f90: derniere modification V5.15 >'

!************************************************************************
!
! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

! initialisation de vit_pseudo_topoN
! ..................................
vit_pseudo_topoN(1)=  vit_gps%sn
vit_pseudo_topoN(2)= -vit_gps%oe
vit_pseudo_topoN(3)= -vit_gps%hb

! Calcul de la position-vitesse dans le repere de ref
! ===================================================

if (present(jacob)) then

   ! calcul de la position-vitesse dans le repere de ref et du jacobien de la transfo (topoN ==> car) 
   ! ................................................................................................
   call mti_pseudo_topoN_car (r_equa, apla, pos_geod, vit_pseudo_topoN, pos_car, vit_car, retour, &
                              jacobtopoNcar=jacobtopoNcar) 
   if (retour /= pm_ok) then 
      code_retour%valeur = retour 
      if  (code_retour%valeur<pm_ok) go to 6000
   end if

   ! calcul du jacobien de la transformation GPS ==> topoN
   ! .....................................................

   jacobGPStopoN(:,:)=  0._pm_reel
   jacobGPStopoN(1,1)=  1._pm_reel
   jacobGPStopoN(2,2)=  1._pm_reel
   jacobGPStopoN(3,3)=  1._pm_reel
   jacobGPStopoN(4,4)=  1._pm_reel
   jacobGPStopoN(5,5)= -1._pm_reel  
   jacobGPStopoN(6,6)= -1._pm_reel

   ! calcul de la jacobienne demandee
   ! ................................

   ! jacob transfo (GPS ==> car) = jacob transfo (topoN ==> car) * transfo (GPS ==> topoN)
   call mu_matmul6(jacobtopoNcar,jacobGPStopoN,jacob,code_retour_local)
   ! Pas d'erreur possible donc le code retour n est pas teste

else ! jacobien non demande

   ! calcul de la position-vitesse dans le repere de ref 
   ! ...................................................
   call mti_pseudo_topoN_car (r_equa, apla, pos_geod, vit_pseudo_topoN, pos_car, vit_car, retour) 
   if (retour /= pm_ok) then 
      code_retour%valeur = retour 
      if  (code_retour%valeur<pm_ok) go to 6000
   end if

end if

6000 continue

code_retour%routine = pm_num_mt_gps_ard_car
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' ' 

end subroutine mt_gps_ard_car
