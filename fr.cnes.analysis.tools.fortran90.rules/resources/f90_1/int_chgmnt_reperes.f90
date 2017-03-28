module int_chgmnt_reperes

! (C) Copyright CNES - MSLIB - 2007
!************************************************************************
!
! But:  Definition des constantes et interface des fonctions du thème T
! ===
!
! Note d'utilisation:
! ==================
!   Module en principe utilisé uniquement par l'intermédiaire du module global
!   "mslib90"
!
!$Historique
! ==========
!   
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!
!  + Version 6.6 : DM-ID 616 (option) : rajout du cartouche
!                   (Date: 05/2007 - Realisation: Atos origin)    
!
!VERSION:V6.13:FA-ID:1410:30/09/2010:Ajout marqueur fin historique
!
!Revision 362 2013/02/15 bbjc
!DM-ID 1513: Suppression des warnings de compilation
!
!$FinHistorique
!
!************************************************************************

use longueur_chaine_mslib
implicit none

! SVN Source File Id
  character(len=256), private, parameter :: SVN_VER =  '$Id: int_chgmnt_reperes.f90 362 2013-02-15 18:01:28Z bbjc $'

public
interface
     subroutine mt_car_geoc( pos_car, pos_geoc, code_retour, vit_car, vit_geoc, jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), dimension(3), intent(in)              ::  pos_car  ! coordonnees cartesiennes dans le repere de reference
type(tm_geocentrique), intent(out)                   ::  pos_geoc ! latitude, longitude et distance geocentriques 
type(tm_code_retour), intent(out)                    ::  code_retour

real(pm_reel), dimension(3), intent(in), optional    ::  vit_car  ! vitesse cartesienne dans le repere de reference
type(tm_geocentrique), intent(out), optional         ::  vit_geoc ! vitesse en geocentrique
real(pm_reel), dimension(6,6), intent(out), optional ::  jacob    ! jacobienne de la transfomation


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mt_car_geoc
     subroutine mt_car_geod( pos_car, r_equa, apla, pos_geod, code_retour, jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel), dimension(3), intent(in)              ::  pos_car      ! position en coordonnees cartesiennes
real(pm_reel), intent(in)                            ::  r_equa       ! rayon equatorial terrestre
real(pm_reel), intent(in)                            ::  apla         ! aplatissement terrestre
type(tm_geodesique), intent(out)                     ::  pos_geod     ! latitude, longitude, hauteur geodesiques
type(tm_code_retour), intent(out)                    ::  code_retour
real(pm_reel), dimension(3,3), intent(out), optional ::  jacob        ! jacobienne de la transformation


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mt_car_geod
     subroutine mt_def_topo_N( lat, long, vect_i, vect_j, vect_k, code_retour )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in)                  ::  lat     ! latitude geodesique de l'origine du repere topocentrique
real(pm_reel), intent(in)                  ::  long    ! longitude geodesique de l'origine du repere topocentrique
real(pm_reel), intent(out), dimension(3)   ::  vect_i  ! vecteur i dans le repere de reference
real(pm_reel), intent(out), dimension(3)   ::  vect_j  ! vecteur j dans le repere de reference
real(pm_reel), intent(out), dimension(3)   ::  vect_k  ! vecteur k dans le repere de reference
type(tm_code_retour), intent(out)           ::  code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mt_def_topo_N
     subroutine mt_geoc_car( pos_geoc, pos_car, code_retour, vit_geoc, vit_car, jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_geocentrique), intent(in)                    ::  pos_geoc     ! latitude, longitude, distance geocentriques             
real(pm_reel), dimension(3), intent(out)             ::  pos_car      ! position en coordonnees cartesiennes
type(tm_code_retour), intent(out)                    ::  code_retour

type(tm_geocentrique), intent(in), optional          ::  vit_geoc     ! vitesse en geocentrique
real(pm_reel), dimension(3), intent(out), optional   ::  vit_car      ! vitesse en cartesien
real(pm_reel), dimension(6,6), intent(out), optional ::  jacob        ! jacobienne de la transfomation


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mt_geoc_car
     subroutine mt_geod_car( pos_geod, r_equa, apla, pos_car, code_retour, jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_geodesique), intent(in)                      ::  pos_geod     ! latitude, longitude,  hauteur geodesiques                                         
real(pm_reel), intent(in)                            ::  r_equa       ! rayon equatorial terrestre
real(pm_reel), intent(in)                            ::  apla         ! aplatissement terrestre
real(pm_reel), dimension(3), intent(out)             ::  pos_car      ! position en coordonnees cartesiennes
type(tm_code_retour), intent(out)                    ::  code_retour
real(pm_reel), dimension(3,3), intent(out), optional ::  jacob        ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mt_geod_car
     subroutine mt_iner_ref(vit_rot, long, sec, pos_iner, pos_ref, code_retour, vit_iner, vit_ref, jacob)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel) , intent(in)                            :: vit_rot ! vitesse de rotation de la Terre
real(pm_reel) , intent(in)                            :: long    ! longitude de l'axe Ox du repere inertiel a la date t0
real(pm_reel) , intent(in)                            :: sec     ! nombre de secondes ecoulees depuis la date t0
real(pm_reel) , intent(in), dimension(3)              :: pos_iner! position dans le repere inertiel
real(pm_reel) , intent(out), dimension(3)             :: pos_ref ! position dans le repere terrestre de reference
type(tm_code_retour), intent(out)                     :: code_retour
real(pm_reel) , intent(in), dimension(3), optional    :: vit_iner! vitesse dans le repere inertiel
real(pm_reel) , intent(out), dimension(3), optional   :: vit_ref ! vitesse dans le repere terrestre de reference
real(pm_reel) , intent(out), dimension(6,6), optional :: jacob   ! jacobien de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mt_iner_ref
     subroutine mt_ref_iner( vit_rot, long, sec, pos_ref, pos_iner, code_retour, vit_ref, vit_iner, jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel) , intent(in)                   :: vit_rot ! vitesse de rotation de la Terre
real(pm_reel) , intent(in)                   :: long    ! longitude de l'axe Ox du repere inertiel a la date t0
real(pm_reel) , intent(in)                   :: sec     ! nombre de secondes ecoulees depuis la date t0
real(pm_reel) , intent(in), dimension(3)     :: pos_ref ! position dans le repere terrestre de reference
real(pm_reel) , intent(out), dimension(3)    :: pos_iner! position dans le repere inertiel
type(tm_code_retour), intent(out)            :: code_retour
real(pm_reel) , intent(in), dimension(3), optional    :: vit_ref ! vitesse dans le repere terrestre de reference
real(pm_reel) , intent(out), dimension(3), optional   :: vit_iner! vitesse dans le repere inertiel
real(pm_reel) , intent(out), dimension(6,6), optional :: jacob   ! jacobien de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mt_ref_iner
     subroutine mt_ref_topo_N( orig_topo, r_equa, apla, pos_ref, pos_topo, code_retour, vit_ref, vit_topo, jacob )

       use type_mslib
       use precision_mslib



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


     end subroutine mt_ref_topo_N
     subroutine mt_topo_E_car_sgd(pos_car, pos_sgd, code_retour, vit_car, vit_sgd, jacob)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel), dimension(3), intent(in)              :: pos_car  ! position dans le repere topo Est en cartesien
type(tm_sgd), intent(out)                            :: pos_sgd  ! position dans le repere topo Est en sgd
type(tm_code_retour), intent(out)                    :: code_retour

real(pm_reel), dimension(3), intent(in), optional    :: vit_car  ! vitesse dans le repere topo Est en cartesien
type(tm_sgd), intent(out), optional                  :: vit_sgd  ! vitesse dans le repere topo Est en sgd
real(pm_reel), dimension(6,6), intent(out), optional :: jacob    ! jacobienne de la transfomation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mt_topo_E_car_sgd
     subroutine mt_topo_E_sgd_car(pos_sgd, pos_car, code_retour, vit_sgd, vit_car, jacob)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
type(tm_sgd), intent(in)                             :: pos_sgd  ! position dans le repere topo Est en sgd
real(pm_reel), dimension(3), intent(out)             :: pos_car  ! position dans le repere topo Est en cartesien
type(tm_code_retour), intent(out)                    :: code_retour

type(tm_sgd), intent(in), optional                   :: vit_sgd  ! vitesse dans le repere topo Est en sgd
real(pm_reel), dimension(3), intent(out), optional   :: vit_car  ! vitesse dans le repere topo Est en cartesien
real(pm_reel), dimension(6,6), intent(out), optional :: jacob    ! jacobienne de la transfomation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mt_topo_E_sgd_car
     subroutine mt_topo_N_car_sgd(pos_car, pos_sgd, code_retour, vit_car, vit_sgd, jacob)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel), dimension(3), intent(in)              :: pos_car  ! position dans le repere topo Nord en cartesien
type(tm_sgd), intent(out)                            :: pos_sgd  ! position dans le repere topo Nord en sgd
type(tm_code_retour), intent(out)                    :: code_retour

real(pm_reel), dimension(3), intent(in), optional    :: vit_car  ! vitesse dans le repere topo Nord en cartesien
type(tm_sgd), intent(out), optional                  :: vit_sgd  ! vitesse dans le repere topo Nord en sgd
real(pm_reel), dimension(6,6), intent(out), optional :: jacob    ! jacobienne de la transfomation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mt_topo_N_car_sgd
     subroutine mt_topo_N_ref(orig_topo, r_equa, apla, pos_topo, pos_ref, code_retour, vit_topo, vit_ref, jacob)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


type(tm_geodesique), intent(in)                      :: orig_topo  ! coordonnees geodesiques de l'origine du repere topocentrique
real(pm_reel), intent(in)                            :: r_equa     ! rayon equatorial
real(pm_reel), intent(in)                            :: apla       ! aplatissement
real(pm_reel), dimension(3), intent(in)              :: pos_topo   ! position dans le repere topocentrique Nord
real(pm_reel), dimension(3), intent(out)             :: pos_ref    ! position dans le repere terrestre de reference
type(tm_code_retour), intent(out)                    ::  code_retour
real(pm_reel), dimension(3), intent(in),optional     :: vit_topo   ! vitesse dans le repere topocentrique Nord
real(pm_reel), dimension(3), intent(out), optional   :: vit_ref    ! vitesse dans le repere terrestre de reference
real(pm_reel), dimension(6,6), intent(out), optional :: jacob      ! jacobien de la transformation





!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mt_topo_N_ref
     subroutine mt_topo_N_sgd_car(pos_sgd, pos_car, code_retour, vit_sgd, vit_car, jacob)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
type(tm_sgd), intent(in)                              :: pos_sgd  ! position dans le repere topo Nord en sgd
real(pm_reel), dimension(3), intent(out)              :: pos_car  ! position dans le repere topo Nord en cartesien
type(tm_code_retour), intent(out)                     :: code_retour

type(tm_sgd), intent(in), optional                    :: vit_sgd  ! vitesse dans le repere topo Nord en sgd
real(pm_reel), dimension(3), intent(out), optional    :: vit_car  ! vitesse dans le repere topo Nord en cartesien
real(pm_reel), dimension(6,6), intent(out), optional  :: jacob    ! jacobienne de la transfomation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mt_topo_N_sgd_car
     subroutine mt_topo_car_E_N(pos_E, pos_N, code_retour, vit_E, vit_N, jacob)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), dimension(3), intent(in)               ::  pos_E  ! position cartesienne avec la convention axe Ox vers l'Est
real(pm_reel), dimension(3), intent(out)              ::  pos_N  ! position cartesienne avec la convention axe Ox vers le Nord
type(tm_code_retour), intent(out)                     ::  code_retour
real(pm_reel), dimension(3), intent(in), optional     ::  vit_E  ! vitesse cartesienne avec la convention axe Ox vers l'Est
real(pm_reel), dimension(3), intent(out) , optional   ::  vit_N  ! vitesse cartesienne avec la convention axe Ox vers le Nord
real(pm_reel), dimension(6,6), intent(out), optional  ::  jacob   ! jacobien de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mt_topo_car_E_N
     subroutine mt_topo_car_N_E(pos_N, pos_E, code_retour, vit_N, vit_E, jacob)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), dimension(3), intent(in)              ::  pos_N  ! position cartesienne avec la convention axe Ox vers le Nord
real(pm_reel), dimension(3), intent(out)             ::  pos_E  ! position cartesienne avec la convention axe Ox vers l'Est
type(tm_code_retour), intent(out)                    ::  code_retour
real(pm_reel), dimension(3), intent(in) , optional   ::  vit_N  ! vitesse cartesienne avec la convention axe Ox vers le Nord
real(pm_reel), dimension(3), intent(out), optional   ::  vit_E  ! vitesse cartesienne avec la convention axe Ox vers l'Est
real(pm_reel), dimension(6,6), intent(out), optional ::  jacob   ! jacobien de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mt_topo_car_N_E
     subroutine mt_topo_sgd_E_N(pos_E, pos_N, code_retour, vit_E, vit_N, jacob)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_sgd), intent(in)               ::  pos_E  ! position en site, gisement, distance avec la convention axe Ox vers l'Est
type(tm_sgd), intent(out)              ::  pos_N  ! position en site, gisement, distance avec la convention axe Ox vers le Nord
type(tm_code_retour), intent(out)      ::  code_retour
type(tm_sgd), intent(in), optional     ::  vit_E  ! vitesse en site, gisement, distance avec la convention axe Ox vers l'Est
type(tm_sgd), intent(out) , optional   ::  vit_N  ! vitesse en site, gisement, distance avec la convention axe Ox vers le Nord
real(pm_reel), dimension(6,6), intent(out), optional  ::  jacob   ! jacobien de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mt_topo_sgd_E_N
     subroutine mt_topo_sgd_N_E(pos_N, pos_E, code_retour, vit_N, vit_E, jacob)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_sgd), intent(in)                ::  pos_N  ! position en site, gisement, distance avec la convention axe Ox vers le Nord
type(tm_sgd), intent(out)               ::  pos_E  ! position en site, gisement, distance avec la convention axe Ox vers l'Est
type(tm_code_retour), intent(out)       ::  code_retour
type(tm_sgd), intent(in) , optional     ::  vit_N  ! vitesse en site, gisement, distance avec la convention axe Ox vers le Nord
type(tm_sgd), intent(out), optional     ::  vit_E  ! vitesse en site, gisement, distance avec la convention axe Ox vers l'Est
real(pm_reel), dimension(6,6), intent(out), optional  ::  jacob   ! jacobien de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mt_topo_sgd_N_E
end interface

  character(len=pm_longueur_rcs_id), private, parameter :: &
  rcs_id =' $Id: int_chgmnt_reperes.f90 362 2013-02-15 18:01:28Z bbjc $ '

end module int_chgmnt_reperes
