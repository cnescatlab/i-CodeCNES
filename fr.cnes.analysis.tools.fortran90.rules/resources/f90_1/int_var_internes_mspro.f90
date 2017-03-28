module int_var_internes_mspro
implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: int_var_internes_mspro.f90 69 2012-09-11 08:33:34Z ffsm $'

public
interface
     subroutine mvi_cir_equa_cir ( cir_equa, cir, retour, jacob )

       use mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


type(tm_orb_cir_equa),          intent(in)               :: cir_equa  ! parametre adaptes circulaire equatorial
type(tm_orb_cir),               intent(out)              :: cir       ! parametre adaptes circulaire
integer,                        intent(out)              :: retour
real(pm_reel), dimension (6,6), intent(out), optional    :: jacob     ! jacobien de la transformation


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mvi_cir_equa_cir
     subroutine mvi_cir_equa_equa ( cir_equa, equa, retour, jacob )

       use mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_orb_cir_equa),          intent(in)               :: cir_equa  ! parametre circulaire equatorial
type(tm_orb_equa),              intent(out)              :: equa      ! parametre equatorial
integer,                        intent(out)              :: retour
real(pm_reel), dimension (6,6), intent(out), optional    :: jacob     ! jacobien de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mvi_cir_equa_equa
     subroutine mvi_hpha_kep (r_equa, hpha, kep, retour, jacob)

       use mslib
       use type_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel)     , intent(in)                           :: r_equa   ! rayon equatorial
type(tm_orb_hpha) , intent(in)                           :: hpha     ! parametres perigee/apogee
type(tm_orb_kep)  , intent(out)                          :: kep      ! parametres kepleriens
integer           , intent(out)                          :: retour
real(pm_reel), dimension (6,6), intent(out), optional    :: jacob    ! jacobien de la transformation


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mvi_hpha_kep
     subroutine mvi_kep_hpha (r_equa, kep, hpha, retour, jacob)

       use mslib
       use type_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel)     , intent(in)                           :: r_equa   ! rayon equatorial
type(tm_orb_kep)  , intent(in)                           :: kep      ! parametres kepleriens
type(tm_orb_hpha) , intent(out)                          :: hpha     ! parametres perigee/apogee
integer           , intent(out)                          :: retour
real(pm_reel), dimension (6,6), intent(out), optional    :: jacob    ! jacobien de la transformation


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mvi_kep_hpha
end interface

end module int_var_internes_mspro
