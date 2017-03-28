subroutine mt_meca_vol_car (r_equa, apla, pos_geod, vit_meca_vol, pos_car, vit_car, code_retour, jacob)

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But: Passage des position-vitesse en coordonnees mecanique du vol
!      aux position-vitesse en coordonnees cartesiennes dans le repere    
!      de reference. 
!       
!   
! Note d'utilisation:  
! ===================   
! la routine ne verifie pas: si la pente de la vitesse est dans [-pi/2, pi/2]         
!                            si la norme de la vitesse est positive 
! Pour plus d'explications:  se reporter a la documentation utilisateur
!                                     et a la note algorithmique.
!   
!$Historique
! ===========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 2.0 : creation a partir de rien
!                         (Date: 04/2002 - Realisation: Mickael Hazak et Guylaine Prat)
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

       
real(pm_reel), intent(in)                      :: r_equa      ! rayon equatorial
real(pm_reel),intent (in)                      :: apla        ! aplatissement  

type(tm_geodesique), intent(in)                :: pos_geod    ! position geodesique                  
type(tm_vit_meca_vol), intent(in)             :: vit_meca_vol ! vitesse mecanique du vol          
real(pm_reel),dimension (3), intent(out)        :: pos_car    ! position cartesienne                                                                                  
real(pm_reel),dimension (3), intent(out)        :: vit_car    ! vitesse cartesienne         
type(tm_code_retour), intent(out)              :: code_retour
real(pm_reel), dimension (6,6), intent(out), optional    :: jacob ! jacobien de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------
! concernant calcul pseudo-topoN > ref 
integer                       :: retour                   ! code retour
real(pm_reel), dimension(3)   :: vit_pseudo_topoN         ! vitesse dans le repere pseudo "topocentrique" Nord

real(pm_reel) :: cos_pente, sin_pente, cos_azimut, sin_azimut ! cos et sin des pente et azimuth de vitesse
real(pm_reel), dimension (6,6)   :: jacobmecavoltopoN     ! jacobienne meca vol -> topo Nord
real(pm_reel), dimension (6,6)   :: jacobtopoNcar         ! jacobienne topo Nord -> cartesien         
type(tm_code_retour)             :: code_local

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSPRO mt_meca_vol_car.f90: derniere modification V5.15 >'

!************************************************************************
!
! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

! Calcul des composantes de la vitesse dans
! le repere pseudo "topocentrique" Nord 
! ==========================================

vit_pseudo_topoN(1)=  vit_meca_vol%norme*cos (vit_meca_vol%pente)*cos (vit_meca_vol%azimut)
vit_pseudo_topoN(2)= -vit_meca_vol%norme*cos (vit_meca_vol%pente)*sin (vit_meca_vol%azimut)
vit_pseudo_topoN(3)=  vit_meca_vol%norme*sin (vit_meca_vol%pente)

! Calcul de la position-vitesse dans le repere de ref
! ===================================================

if (present(jacob)) then

   ! calcul de la position-vitesse dans le repere de ref et du jacobien de la transfo (topoN ==> car) 
   ! ................................................................................................
   call mti_pseudo_topoN_car (r_equa, apla, pos_geod, vit_pseudo_topoN, pos_car, vit_car, retour, &
                              jacobtopoNcar=jacobtopoNcar)
 
   if (retour /= pm_OK) then 
      code_retour%valeur = retour 
      if  (code_retour%valeur < pm_OK) go to 6000
   end if

   ! initialisation de lignes trigo
   cos_pente = cos(vit_meca_vol%pente)
   sin_pente = sin(vit_meca_vol%pente)
   cos_azimut = cos(vit_meca_vol%azimut)
   sin_azimut = sin (vit_meca_vol%azimut)

   ! calcul du jacobien de la transformation meca vol ==> topoN
   ! ..........................................................

   jacobmecavoltopoN(:,:)= 0._pm_reel

   jacobmecavoltopoN(1,1)= 1._pm_reel
   jacobmecavoltopoN(2,2)= 1._pm_reel
   jacobmecavoltopoN(3,3)= 1._pm_reel

   jacobmecavoltopoN(4,4)= -vit_meca_vol%norme*sin_pente*cos_azimut
   jacobmecavoltopoN(4,5)=  vit_pseudo_topoN(2)
   jacobmecavoltopoN(4,6)=  cos_pente*cos_azimut

   jacobmecavoltopoN(5,4)=   vit_meca_vol%norme*sin_pente*sin_azimut   
   jacobmecavoltopoN(5,5)= -vit_pseudo_topoN(1)
   jacobmecavoltopoN(5,6)= -cos_pente*sin_azimut

   jacobmecavoltopoN(6,4)=  vit_meca_vol%norme*cos_pente
   ! jacobmecavoltopoN(6,5) est nul
   jacobmecavoltopoN(6,6)=  sin_pente

   ! calcul de la jacobienne demandee
   ! ................................

   ! jacob transfo (meca vol ==> car) = jacob transfo (topoN ==> car) * transfo (meca vol ==> topoN)

   call mu_matmul6(jacobtopoNcar,jacobmecavoltopoN,jacob,code_local)
   ! Pas d'erreur possible donc le code retour n est pas teste

else ! jacobien non demande

   ! Calcul de la position-vitesse dans le repere de ref (sans jacobien)
   ! ...................................................................
   call mti_pseudo_topoN_car (r_equa, apla, pos_geod, vit_pseudo_topoN, pos_car, vit_car, retour) 
   if (retour /= pm_OK) then 
      code_retour%valeur = retour 
      if  (code_retour%valeur < pm_OK) go to 6000
   end if  

end if

6000 continue

code_retour%routine = pm_num_mt_meca_vol_car
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' ' 

end subroutine mt_meca_vol_car
