subroutine mt_car_gps_ard (r_equa, apla, pos_car, vit_car, pos_geod, vit_gps, code_retour, jacob)

! (C) Copyright CNES - MSPRO - 2001

!************************************************************************
!
! But: Passage des position-vitesse en  coordonnees cartesiennes dans le 
! ===  repere de reference en point GPS ARD.
!   
! Note d'utilisation:  
! ===================   
!  Definition du repere pseudo topocentrique Nord: 
!   il correspond a un repere topocentrique Nord dont l'origine est la position courante 
!   du satellite.
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
!   + Version 5.9 : DM-ID 859 : appel a mu_matmul6
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

use int_rep_internes_mspro, only : mti_car_pseudo_topoN

use valeur_code_retour_mspro
use numero_routine_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       
real(pm_reel), intent(in)                      :: r_equa   ! rayon equatorial
real(pm_reel),intent (in)                      :: apla     ! aplatissement 
real(pm_reel),dimension (3), intent(in)        :: pos_car  ! position cartesienne                                      
real(pm_reel),dimension (3), intent(in)        :: vit_car  ! vitesse cartesienne
type(tm_geodesique), intent(out)               :: pos_geod ! position geodesique        
type(tm_vit_gps_ard), intent(out)              :: vit_gps  ! vitesse gps ARD          
type(tm_code_retour), intent(out)              :: code_retour
real(pm_reel), dimension (6,6), intent(out), optional    :: jacob ! jacobien de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------
! concernant calcul ref > pseudo-topoN
integer                       :: retour             ! code retour
real(pm_reel), dimension(3)   :: vit_pseudo_topoN   ! vecteur vitesse pseudo topo Nord
real(pm_reel), dimension(6,6) :: jacobcartopoN      ! jacobienne cartesien -> topo Nord
real(pm_reel), dimension(6,6) :: jacobtopoNGPS      ! jacobienne topo Nord -> GPS
type(tm_code_retour)          :: code_retour_local

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSPRO mt_car_gps_ard.f90: derniere modification V5.15 >'

!************************************************************************
!
! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

! Calcul de position geodesique et de la vitesse dans le repere pseudo topocentrique Nord
! ========================================================================================

if (present(jacob)) then

   ! calcul de la position geodesique et vitesse topo N et du jacobien de la transfo (car ==> topoN) 
   ! ................................................................................................
   call mti_car_pseudo_topoN (r_equa, apla, pos_car, vit_car, pos_geod, vit_pseudo_topoN, retour, &
                              jacobcartopoN=jacobcartopoN)

   if (retour /= pm_OK) then
      code_retour%valeur = retour
      if (code_retour%valeur < pm_OK) go to 6000 ! en particulier si jacobien non calculable: 
                                                 ! jacobcartopoN non initialise
   end if

   ! calcul du jacobien de la transformation GPS ==> topoN
   ! ..........................................................

   jacobtopoNGPS(:,:)= 0._pm_reel
   jacobtopoNGPS(1,1)= 1._pm_reel
   jacobtopoNGPS(2,2)= 1._pm_reel
   jacobtopoNGPS(3,3)= 1._pm_reel
   jacobtopoNGPS(4,4)=  1._pm_reel
   jacobtopoNGPS(5,5)= -1._pm_reel  
   jacobtopoNGPS(6,6)= -1._pm_reel

   ! calcul de la jacobienne demandee
   ! ................................

   ! jacob transfo (car ==> GPS) = jacob transfo (topoN ==> GPS) * transfo (car ==> topoN)

   call mu_matmul6(jacobtopoNGPS,jacobcartopoN,jacob,code_retour_local)
   ! Pas d'erreur possible donc le code retour n est pas teste

else  ! jacobien non demande

   call mti_car_pseudo_topoN (r_equa, apla, pos_car, vit_car, pos_geod, vit_pseudo_topoN, retour)   

   if (retour /= pm_OK) then
      code_retour%valeur = retour
      if (code_retour%valeur < pm_OK) go to 6000
   end if

end if

! le calcul des composantes de la vitesse dans le repere (sn, oe, hb)
!====================================================================

vit_gps%sn =    vit_pseudo_topoN(1) 
vit_gps%oe = -  vit_pseudo_topoN(2)
vit_gps%hb = -  vit_pseudo_topoN(3)

6000 continue

code_retour%routine = pm_num_mt_car_gps_ard
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' ' 

end subroutine mt_car_gps_ard
