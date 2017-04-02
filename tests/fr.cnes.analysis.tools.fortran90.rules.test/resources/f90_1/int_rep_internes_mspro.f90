module int_rep_internes_mspro
implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: int_rep_internes_mspro.f90 69 2012-09-11 08:33:34Z ffsm $'

public
interface
     subroutine  mti_car_pseudo_topoN (r_equa, apla, pos_car, vit_car, pos_geod, vit_pseudo_topoN, retour, &
                                       epsreq, jacobcartopoN)


       use mslib
       use type_mspro

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       
real(pm_reel), intent(in)                             :: r_equa        ! rayon equatorial
real(pm_reel),intent (in)                             :: apla          ! aplatissement
real(pm_reel),dimension (3), intent(in)               :: pos_car       ! position cartesienne
real(pm_reel),dimension (3), intent(in)               :: vit_car       ! vitesse cartesienne

type(tm_geodesique), intent(out)                      :: pos_geod          ! position geodesique             
real(pm_reel),dimension (3), intent(out)              :: vit_pseudo_topoN  ! vitesse dans le repere topo Nord
integer, intent(out)                                  :: retour

real(pm_reel),                 intent(out), optional  :: epsreq        ! epsilon test par rapport a r_equa
real(pm_reel), dimension(6,6), intent(out), optional  :: jacobcartopoN ! jacobien de la transformation
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mti_car_pseudo_topoN
     subroutine  mti_pseudo_topoN_car (r_equa, apla, pos_geod, vit_pseudo_topoN, pos_car, vit_car, retour, jacobtopoNcar)
 
       use mslib
       use type_mspro

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


     end subroutine mti_pseudo_topoN_car
     subroutine mti_rot_axeZ ( angle, pos_ref ,pos_tourn, retour, vit_ref, vit_tourn, jacob )

       use mslib


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel),              intent(in)            ::  angle      ! angle du changement de repere
real(pm_reel), dimension(3),intent(in)            ::  pos_ref    ! position en entree
real(pm_reel), dimension(3),intent(out)           ::  pos_tourn  ! position en sortie
integer,                    intent(out)           ::  retour     
real(pm_reel), dimension(3),intent(in),optional   ::  vit_ref    ! vitesse en entree    
real(pm_reel), dimension(3),intent(out),optional  ::  vit_tourn  ! vitesse en sortie
real(pm_reel),dimension(6,6),intent(out),optional ::  jacob      ! jacobienne de la transformation


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mti_rot_axeZ
end interface

end module int_rep_internes_mspro
