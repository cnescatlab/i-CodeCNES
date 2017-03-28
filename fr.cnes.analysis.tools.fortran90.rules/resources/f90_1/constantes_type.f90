! constantes_type.f90 --
!
!           Project: SPS_GENERIC
!           Authors: NOVELTIS/B.TOURNIER
!              Date: october 2009
!           Version: $Revision: 1.3 $
! Last modification: $Date: 2011-03-07 15:54:59 $
!
! type_srf -- Module
!
! * Purpose
!   Module for constantes declaration
!
module constantes_type
!
   use precision_type
!
   implicit none
!
   public ::          &
           Pi,        &
           twoPi,     &
           Cst_T_Ref, &
           Cst_P_Ref, &
           Cst_RO_0,  &
           Cst_T_0,   &
           Cst_h,     &
           Cst_c,     &
           Cst_k,     &
           Cst_sca1,  &
           Cst_sca2,  &
           Cst_chel
!
  real(kind=DOUBLE), parameter :: Pi        = 3.14159265358979323846_DOUBLE
  real(kind=DOUBLE), parameter :: twoPi     = 6.28318530717958647692_DOUBLE
  real(kind=DOUBLE), parameter :: Cst_T_Ref = 280.d+00        ! Kelvin
  real(kind=DOUBLE), parameter :: Cst_P_Ref = 1013.25d+00     ! hectoPascal
  real(kind=DOUBLE), parameter :: Cst_RO_0  = 1.293d+00 !air density (kg.m-3)
  real(kind=DOUBLE), parameter :: Cst_T_0   = 273.16d+00 ! Ref temperature (K)
  real(kind=DOUBLE), parameter :: Cst_h     = 6.6260755d-34   ! Joules*Secondes
  real(kind=DOUBLE), parameter :: Cst_c     = 2.99792458d+8   ! m/s
  real(kind=DOUBLE), parameter :: Cst_k     = 1.380658d-23    ! Joules/Kelvin
  real(kind=DOUBLE), parameter :: Cst_sca1  = 2*Cst_h*Cst_c**2
  real(kind=DOUBLE), parameter :: Cst_sca2  = Cst_h*Cst_c/Cst_k
  real(kind=DOUBLE), parameter :: Cst_chel  = 1.60217733d-19  ! Coulombs
!
!
end module constantes_type
