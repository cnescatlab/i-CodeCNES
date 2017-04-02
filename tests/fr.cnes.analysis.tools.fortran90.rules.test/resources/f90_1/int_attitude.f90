module int_attitude
implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: int_attitude.f90 69 2012-09-11 08:33:34Z ffsm $'

public
interface
     subroutine ma_avion_sol ( incidence, derapage, pente_vit, azimut_vit, gite_vit, &
                               azimut_avion, assiette_avion, gite_avion, code_retour )

       use mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel) , intent(in)  :: incidence  ! angle d'incidence
real(pm_reel) , intent(in)  :: derapage   ! angle de derapage
real(pm_reel) , intent(in)  :: pente_vit  ! pente de la vitesse
real(pm_reel) , intent(in)  :: azimut_vit ! azimut de la vitesse
real(pm_reel) , intent(in)  :: gite_vit   ! gite (rotation autour du vecteur vitesse)

real(pm_reel) , intent(out)  :: azimut_avion   ! azimut de l'avion (fuselage)
real(pm_reel) , intent(out)  :: assiette_avion ! assiette longitudinale de l'avion (fuselage)
real(pm_reel) , intent(out)  :: gite_avion     ! gite de l'avion (fuselage) 

type(tm_code_retour), intent(out) ::  code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine ma_avion_sol
     subroutine ma_avion_vit ( pente_vit, azimut_vit, azimut_avion, assiette_avion, &
                               gite_avion, incidence, derapage, gite_vit, code_retour )

       use mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel) , intent(in) :: pente_vit      ! pente de la vitesse  
real(pm_reel) , intent(in) :: azimut_vit     ! azimut de la vitesse 
real(pm_reel) , intent(in) :: azimut_avion   ! azimut de l'avion (fuselage) 
real(pm_reel) , intent(in) :: assiette_avion ! assiette longitudinale de l'avion (fuselage) 
real(pm_reel) , intent(in) :: gite_avion     ! gite de l'avion (fuselage) 

real(pm_reel) , intent(out) :: incidence  ! angle d'incidence 
real(pm_reel) , intent(out) :: derapage   ! angle de derapage 
real(pm_reel) , intent(out) :: gite_vit   ! gite (rotation autour du vecteur vitesse)
type(tm_code_retour), intent(out) ::  code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine ma_avion_vit
end interface

end module int_attitude
