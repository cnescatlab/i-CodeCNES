module int_chgmnt_variables_mspro
implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: int_chgmnt_variables_mspro.f90 69 2012-09-11 08:33:34Z ffsm $'

public
interface
     subroutine mv_Bplane_kep (mu, M, Bplane, kep, date, code_retour)

       use mslib
       use type_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


real(pm_reel), intent(in)                            ::  mu     ! constante de la gravitation universelle
real(pm_reel), intent(in)                            ::  M      ! anomalie moyenne
type(tm_orb_Bplane),intent(in)                       ::  Bplane ! parametres de B-plane
type(tm_orb_kep),intent(out)                         ::  kep    ! parametres kepleriens
type(tm_jour_sec), intent(out)                       ::  date   ! date du passage a la position (JJCNES)
type(tm_code_retour), intent(out)                    ::  code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mv_Bplane_kep
     subroutine mv_Vinf_kep (mu, Vinf, kep, code_retour, jacob)

       use mslib
       use type_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


real(pm_reel), intent(in)                            ::  mu     ! constante de la gravitation universelle
type(tm_orb_Vinf),intent(in)                         ::  Vinf   ! parametres hyperboliques
type(tm_orb_kep),intent(out)                         ::  kep    ! parametres kepleriens
type(tm_code_retour), intent(out)                    ::  code_retour
real(pm_reel), dimension(6,6),intent(out),optional   ::  jacob  ! jacobien de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mv_Vinf_kep
     subroutine mv_Vinfarr_kep (mu, Vinfarr, kep, code_retour, jacob)

       use mslib
       use type_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


real(pm_reel), intent(in)                            ::  mu     ! constante de la gravitation universelle
type(tm_orb_Vinf),intent(in)                         ::  Vinfarr   ! parametres hyperboliques
type(tm_orb_kep),intent(out)                         ::  kep    ! parametres kepleriens
type(tm_code_retour), intent(out)                    ::  code_retour
real(pm_reel), dimension(6,6),intent(out),optional   ::  jacob  ! jacobien de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mv_Vinfarr_kep
     subroutine mv_kep_Bplane ( mu, date, kep, Bplane, code_retour, jacob, jacobJPL )

       use mslib
       use type_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in)                            ::  mu     ! constante de la gravitation universelle
type(tm_jour_sec), intent(in)                        ::  date   ! date du passage a la position kep (JJCNES)
type(tm_orb_kep),intent(in)                          ::  kep    ! parametres kepleriens
type(tm_orb_Bplane),intent(out)                      ::  Bplane ! parametres de B-plane
type(tm_code_retour), intent(out)                    ::  code_retour
real(pm_reel), dimension(6,6),intent(out),optional   ::  jacob     ! jacobien de la transformation
real(pm_reel), dimension(6,6),intent(out),optional   ::  jacobJPL  ! jacobien avec les parametres JPL



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mv_kep_Bplane
     subroutine mv_kep_Vinf (mu, kep, Vinf, code_retour, jacob)

       use mslib
       use type_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in)                            ::  mu     ! constante de la gravitation universelle
type(tm_orb_kep),intent(in)                          ::  kep    ! parametres kepleriens
type(tm_orb_Vinf),intent(out)                        ::  Vinf   ! parametres hyperboliques
type(tm_code_retour), intent(out)                    ::  code_retour
real(pm_reel), dimension(6,6),intent(out),optional   ::  jacob  ! jacobien de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mv_kep_Vinf
     subroutine mv_kep_Vinfarr (mu, kep, Vinfarr, code_retour, jacob)

       use mslib
       use type_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in)                            ::  mu     ! constante de la gravitation universelle
type(tm_orb_kep),intent(in)                          ::  kep    ! parametres kepleriens
type(tm_orb_Vinf),intent(out)                        ::  Vinfarr   ! parametres hyperboliques
type(tm_code_retour), intent(out)                    ::  code_retour
real(pm_reel), dimension(6,6),intent(out),optional   ::  jacob  ! jacobien de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mv_kep_Vinfarr
end interface

end module int_chgmnt_variables_mspro
