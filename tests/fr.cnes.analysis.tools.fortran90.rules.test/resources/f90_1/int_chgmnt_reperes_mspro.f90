module int_chgmnt_reperes_mspro
implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: int_chgmnt_reperes_mspro.f90 69 2012-09-11 08:33:34Z ffsm $'

public
interface
     subroutine mt_car_gps_ard (r_equa, apla, pos_car, vit_car, pos_geod, vit_gps, code_retour, jacob)


       use mslib
       use type_mspro



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


     end subroutine mt_car_gps_ard 
     subroutine mt_car_meca_vol (r_equa, apla, pos_car, vit_car, pos_geod, vit_meca_vol, code_retour, jacob)


       use mslib
       use type_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       
real(pm_reel), intent(in)                      :: r_equa        ! rayon equatorial
real(pm_reel),intent (in)                      :: apla          ! aplatissement 
real(pm_reel),dimension (3), intent(in)        :: pos_car       ! position cartesienne                                              
real(pm_reel),dimension (3), intent(in)        :: vit_car       ! vitesse cartesienne
type(tm_geodesique), intent(out)               :: pos_geod      ! position geodesique             
type(tm_vit_meca_vol), intent(out)             :: vit_meca_vol  ! vitesse mecanique du vol          
type(tm_code_retour), intent(out)              :: code_retour
real(pm_reel), dimension (6,6), intent(out), optional    :: jacob! jacobien de la transformation


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mt_car_meca_vol
     
subroutine mt_gps_ard_car (r_equa, apla, pos_geod, vit_gps, pos_car, vit_car, code_retour, jacob)


       use mslib
       use type_mspro



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


     end subroutine mt_gps_ard_car 
     subroutine  mt_meca_vol_car (r_equa, apla, pos_geod, vit_meca_vol, pos_car, vit_car, code_retour, jacob)


       use mslib
       use type_mspro



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


     end subroutine mt_meca_vol_car
end interface

end module int_chgmnt_reperes_mspro
