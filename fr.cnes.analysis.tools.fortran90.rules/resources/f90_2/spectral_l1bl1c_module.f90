!!#   spectral_l1bl1c_module.f90 --
!!# 
!!#            Project: SPS_GENERIC
!!#            Authors: NOVELTIS/B.TOURNIER
!!#               Date: march 2010
!!#            Version: $Revision: 1.9 $
!!#  Last modification: $Date: 2012-02-08 10:19:15 $
!!#
!!#  Language:  F90
!!#  Standards: Noveltis
!!#
!!# --
!!#
!!
!>  spectral calibration 1b 1c -- Module
!!
!! * Purpose
!!
!!   Module for spectrum 1b and 1c computation
!!
!! * Description
!!      
!!   The objective of this module is to compute level 1b and level 1c spectra.
!!
!! * Sub-routines and functions
!!
!! -  spectral_l1b_init       : initialisation of the spectral l1b parameters
!! -  spectral_l1c_init       : initialisation of the spectral l1c parameters
!! -  spectrum_calib_l1b      : level 1b spectrum computation
!! -  spectrum_apodiz_ref_l1c : l1c spectrum computation by l1b spectrum apodisation
!! -  spectrum_apodiz_opt_l1c : l1c spectrum optimised computation by l1b spectrum apodisation 
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : AD
!!

module spectral_l1bl1c_module
   use precision_type
   use error_type
   use constantes_type
   use spectrum_type
   use srf_type
   use apf_type
   use spectral_calib_type
   use spectral_l1b_type
   use spectral_l1c_type
   use spectral_l1b_mb_type
   use spectral_l1c_mb_type
   use fft_module
   use math_module
   use plancklib_module
   use spectrum_module
   use spectrum_oversamp_module
!
   implicit none
!
!
   public ::                         &
             spectrum_post_calib,    &
             spectrum_calib_l1b,     &
             spectrum_apodspa_ref_l1c,&
             spectrum_apodspa_opt_l1c,&
             spectrum_apodspa_l1a_l1c,&
             spectrum_apodspa_l1b_l1c,&
             spectral_l1b_init,      &
             spectral_l1b_mb_init,   &
             spectral_l1c_init,      &
             spectral_l1c_mb_init
             
!
   contains
!
!

!
!
!> spectral_l1b_init -- Public
!!
!! * Purpose
!!
!!     Initialisation of the spectral l1b parameters
!!
!! * Description
!! 
!!     This subroutine allows to initialise the spectral l1b parameters.
!!     It updates the type dedicated to the spectral l1b parameters.
!!
!! * Inputs
!!
!!     - File_L1b_Param : character / input file with definition of spectral l1b parameters
!!
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Spectral_L1b : type_Spectral_L1b / type for spectral l1b parameters declaration
!!                      and allocation.
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : AD
!!

   subroutine spectral_l1b_init( File_L1b_Param, &
                                 Spectral_L1b    )
     implicit none
     character(len=*)          ,intent(in)                :: File_L1b_Param
     type(type_Spectral_L1b)   ,intent(out)               :: Spectral_L1b
     integer(kind=LONG)                                   :: iFile
     integer(kind=LONG)                                   :: iPos
     integer(kind=LONG)                                   :: SB
     integer(kind=LONG)                                   :: Ns
!
     iFile = 10
     iPos = 1
     open(iFile,file=File_L1b_Param, err=999)
     Spectral_L1b%filename = File_L1b_Param
     iPos = 2
     read(iFile,*,err=999) Spectral_L1b%NsFft
     iPos = 3
     read(iFile,*,err=999) Spectral_L1b%OSFactor_zeropad
     iPos = 4
     read(iFile,*,err=999) Spectral_L1b%OSFactor_dephas
     iPos = 5
     read(iFile,*,err=999) Spectral_L1b%OSFactor_spline
     iPos = 6
     read(iFile,*,err=999) Spectral_L1b%SigS
     iPos = 7
     read(iFile,*,err=999) Spectral_L1b%Nband
     call alloc_Spectral_L1b( Spectral_L1b )
     do SB = 1, Spectral_L1b%Nband
        iPos = 8
        read(iFile,*,err=999) Spectral_L1b%WnS(SB)
        read(iFile,*,err=999) Spectral_L1b%WnE(SB)
     end do
     iPos = 9
     read(iFile,*,err=999) Spectral_L1b%dWn
     close(unit=iFile)
!
!    adjust wave numbers limits
     do SB = 1, Spectral_L1b%Nband
        Ns = idnint(Spectral_L1b%WnS(SB)/Spectral_L1b%dWn) + 1
        Spectral_L1b%WnS(SB) = dble(Ns-1) * Spectral_L1b%dWn
        Ns = idnint(Spectral_L1b%WnE(SB)/Spectral_L1b%dWn) + 1
        Spectral_L1b%WnE(SB) = dble(Ns-1) * Spectral_L1b%dWn
     end do
!
     return
!
999  write(*,*) 'parameters reading error',iPos
     write(*,*) 'Spectral_L1b_init Fatal Error'
     call exit(1)
   end subroutine spectral_l1b_init
!
!

!
!
!> spectral_l1c_init -- Public
!!
!! * Purpose
!!
!!     Initialisation of the spectral l1c parameters
!!
!! * Description
!! 
!!     This subroutine allows to initialise the spectral l1c parameters.
!!     It updates the type dedicated to the spectral l1c parameters.
!!
!! * Inputs
!!
!!     - File_L1c_Param : character / input file with definition of spectral l1c parameters 
!!
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Spectral_L1c : type_Spectral_L1c / type for spectral l1b parameters declaration
!!                      and allocation.
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : AD
!!
   subroutine spectral_l1c_init( File_L1c_Param, &
                                 Spectral_L1c    )
     implicit none
     character(len=*)          ,intent(in)                :: File_L1c_Param
     type(type_Spectral_L1c)   ,intent(out)               :: Spectral_L1c
     integer(kind=LONG)                                   :: iFile
     integer(kind=LONG)                                   :: iPos
     integer(kind=LONG)                                   :: SB
     integer(kind=LONG)                                   :: Ns
!
     iFile = 10
     iPos = 1
     open(iFile,file=File_L1c_Param, err=999)
     Spectral_L1c%filename = File_L1c_Param
     iPos = 2
     read(iFile,'(a)',err=999) Spectral_L1c%Mode
     write(*,*) 'Spectral_L1c%Mode : ',Spectral_L1c%Mode
     iPos = 3
     read(iFile,*,err=999) Spectral_L1c%NsFft
     write(*,*) 'Spectral_L1c%NsFft',Spectral_L1c%NsFft
     iPos = 4
     read(iFile,*,err=999) Spectral_L1c%SigS
     write(*,*) 'Spectral_L1c%SigS',Spectral_L1c%SigS
     iPos = 5
     read(iFile,*,err=999) Spectral_L1c%SigI
     write(*,*) 'Spectral_L1c%SigI',Spectral_L1c%SigI
     iPos = 6
     read(iFile,*,err=999) Spectral_L1c%Nband
     write(*,*) 'Spectral_L1c%Nband',Spectral_L1c%Nband
     call alloc_Spectral_L1c( Spectral_L1c )
     do SB = 1, Spectral_L1c%Nband
        iPos = 6+SB
        read(iFile,*,err=999) Spectral_L1c%WnS(SB)
        read(iFile,*,err=999) Spectral_L1c%WnE(SB)
     write(*,*) 'Spectral_L1c%WnS(SB)',Spectral_L1c%WnS(SB)
     write(*,*) 'Spectral_L1c%WnE(SB)',Spectral_L1c%WnE(SB)
     end do
     iPos = 11
     read(iFile,*,err=999) Spectral_L1c%dWn
     close(unit=iFile)
!
!    adjust wave numbers limits
     do SB = 1, Spectral_L1c%Nband
        Ns = idnint(Spectral_L1c%WnS(SB)/Spectral_L1c%dWn) + 1
        Spectral_L1c%WnS(SB) = dble(Ns-1) * Spectral_L1c%dWn
        Ns = idnint(Spectral_L1c%WnE(SB)/Spectral_L1c%dWn) + 1
        Spectral_L1c%WnE(SB) = dble(Ns-1) * Spectral_L1c%dWn
     end do
!
     return
!
999  write(*,*) 'parameters reading error',iPos
     write(*,*) 'Spectral_L1c_init Fatal Error'
     call exit(1)
   end subroutine spectral_l1c_init
!
!
   subroutine spectral_l1b_mb_init( File_L1b_Param, &
                                    Spectral_L1b    )
     implicit none
     character(len=*)          ,intent(in)                :: File_L1b_Param
     type(type_Spectral_L1b_mb),intent(out)               :: Spectral_L1b
     integer(kind=LONG)                                   :: iFile
     integer(kind=LONG)                                   :: iPos
     integer(kind=LONG)                                   :: SB
     integer(kind=LONG)                                   :: Ns
!
     iFile = 10
     iPos = 1
     open(iFile,file=File_L1b_Param, err=999)
     Spectral_L1b%filename = File_L1b_Param
     iPos = 2
     read(iFile,*,err=999) Spectral_L1b%Nband
     write(*,*) 'Spectral_L1b%Nband',Spectral_L1b%Nband
     call alloc_Spectral_L1b_mb( Spectral_L1b )
     do SB = 1, Spectral_L1b%Nband
        iPos = SB + 3
        read(iFile,*,err=999) Spectral_L1b%NsFft(SB)
        iPos = SB + 4
        read(iFile,*,err=999) Spectral_L1b%OSFactor_zeropad(SB)
        iPos = SB + 5
        read(iFile,*,err=999) Spectral_L1b%OSFactor_dephas(SB)
        iPos = SB + 6
        read(iFile,*,err=999) Spectral_L1b%OSFactor_spline(SB)
        iPos = SB + 7
        read(iFile,*,err=999) Spectral_L1b%SigS(SB)
        iPos = SB + 8
        read(iFile,*,err=999) Spectral_L1b%WnS(SB)
        iPos = SB + 9
        read(iFile,*,err=999) Spectral_L1b%WnE(SB)
        iPos = SB + 10
        read(iFile,*,err=999) Spectral_L1b%dWn(SB)
     end do
     close(unit=iFile)
!
!    adjust wave numbers limits
     do SB = 1, Spectral_L1b%Nband
        Ns = idnint(Spectral_L1b%WnS(SB)/Spectral_L1b%dWn(SB)) + 1
        Spectral_L1b%WnS(SB) = dble(Ns-1) * Spectral_L1b%dWn(SB)
        Ns = idnint(Spectral_L1b%WnE(SB)/Spectral_L1b%dWn(SB)) + 1
        Spectral_L1b%WnE(SB) = dble(Ns-1) * Spectral_L1b%dWn(SB)
     end do
!
     return
!
999  write(*,*) 'parameters reading error',iPos
     write(*,*) 'Spectral_L1b_mb_init Fatal Error'
     call exit(1)
   end subroutine spectral_l1b_mb_init
!
!
!
!
   subroutine spectral_l1c_mb_init( File_L1c_Param, &
                                    Spectral_L1c    )
     implicit none
     character(len=*)          ,intent(in)                :: File_L1c_Param
     type(type_Spectral_L1c_mb),intent(out)               :: Spectral_L1c
     integer(kind=LONG)                                   :: iFile
     integer(kind=LONG)                                   :: iPos
     integer(kind=LONG)                                   :: SB
     integer(kind=LONG)                                   :: Ns
!
     iFile = 10
     iPos = 0
     open(iFile,file=File_L1c_Param, err=999)
     Spectral_L1c%filename = File_L1c_Param
     iPos = 1
     read(iFile,*,err=999) Spectral_L1c%Nband
     write(*,*) 'Spectral_L1c%Nband',Spectral_L1c%Nband
     call alloc_Spectral_L1c_mb( Spectral_L1c )
     do SB = 1, Spectral_L1c%Nband
        iPos = 2*SB
        read(iFile,'(a)',err=999) Spectral_L1c%Mode(SB)
        write(*,*) 'Spectral_L1c%Mode : ',Spectral_L1c%Mode(SB)
        iPos = 3*SB
        read(iFile,*,err=999) Spectral_L1c%NsFft(SB)
        write(*,*) 'Spectral_L1c%NsFft',Spectral_L1c%NsFft(SB)
        iPos = 4*SB
        read(iFile,*,err=999) Spectral_L1c%SigS(SB)
        write(*,*) 'Spectral_L1c%SigS',Spectral_L1c%SigS(SB)
        iPos = 5*SB
        read(iFile,*,err=999) Spectral_L1c%SigI(SB)
        write(*,*) 'Spectral_L1c%SigI',Spectral_L1c%SigI(SB)
        iPos = 6*SB
        read(iFile,*,err=999) Spectral_L1c%WnS(SB)
        write(*,*) 'Spectral_L1c%WnS(SB)',Spectral_L1c%WnS(SB)
        iPos = 7*SB
        read(iFile,*,err=999) Spectral_L1c%WnE(SB)
        write(*,*) 'Spectral_L1c%WnE(SB)',Spectral_L1c%WnE(SB)
        iPos = 8*SB
        read(iFile,*,err=999) Spectral_L1c%dWn(SB)
        write(*,*) 'Spectral_L1c%dWn(SB)',Spectral_L1c%dWn(SB)
     end do
     close(unit=iFile)
!
!    adjust wave numbers limits
     do SB = 1, Spectral_L1c%Nband
        Ns = idnint(Spectral_L1c%WnS(SB)/Spectral_L1c%dWn(SB)) + 1
        Spectral_L1c%WnS(SB) = dble(Ns-1) * Spectral_L1c%dWn(SB)
        Ns = idnint(Spectral_L1c%WnE(SB)/Spectral_L1c%dWn(SB)) + 1
        Spectral_L1c%WnE(SB) = dble(Ns-1) * Spectral_L1c%dWn(SB)
     end do
!
     return
!
999  write(*,*) 'parameters reading error',iPos
     write(*,*) 'Spectral_L1c_mb_init Fatal Error'
     call exit(1)
   end subroutine spectral_l1c_mb_init
!
!
   subroutine spectrum_post_calib( Interf_Param,&
                                   PN,          &
                                   V_Doppler,   &
                                   dV_Doppler,  &
                                   Srf,         &
                                   Spectrum_L1a )
     implicit none
     type(type_Interf_Param) ,intent(in)                  :: Interf_Param
     integer(kind=LONG)      ,intent(in)                  :: PN
     real(kind=DOUBLE)       ,intent(in)                  :: V_Doppler
     real(kind=DOUBLE)       ,intent(in)                  :: dV_Doppler
     type(type_Srf)          ,intent(in)                  :: Srf
     type(type_Spectrum)     ,intent(inout)               :: Spectrum_L1a
     real(kind=DOUBLE)       ,dimension(:),allocatable    :: B_BB
     real(kind=DOUBLE)       ,dimension(:),allocatable    :: B_BB_C
     real(kind=DOUBLE)       ,dimension(:),allocatable    :: Wn
     real(kind=DOUBLE)       ,dimension(:),allocatable    :: Fcs
     integer(kind=LONG)                                   :: Ns
     integer(kind=LONG)                                   :: n
     integer(kind=LONG)                                   :: ioalloc
     integer(kind=LONG)                                   :: ErrCode
!
!    allocation
     ioalloc = 0
     allocate( B_BB(Spectrum_L1a%N_Sample),    stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = 1
     allocate( B_BB_C(Spectrum_L1a%N_Sample),  stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = 2
     allocate( Wn(Spectrum_L1a%N_Sample),      stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = 3
     allocate( Fcs(Spectrum_L1a%N_Sample),     stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = 4
     if( ioalloc /= 0 ) then
        write(*,'(a,i2)') 'Allocation Error',ioalloc
        write(*,'(a)') 'spectrum_post_calib Fatal Error'
        call exit(1)
     end if
!
!    Spectral calibration function sampling on L1a basis
     call intspline( Srf%NsWn0,            &
                     Srf%Wn0,              &
                     Srf%Fcs1a(1,PN),      &
                     Spectrum_L1a%N_Sample,&
                     Spectrum_L1a%Wn,      &
                     Fcs                   )
!
!    Calibrated wave numbers
     Wn(1:Spectrum_L1a%N_Sample) =                        &
                 Spectrum_L1a%Wn(1:Spectrum_L1a%N_Sample) &
               * ( 1.d+00 + Fcs(1:Spectrum_L1a%N_Sample)  &
                         - ((V_Doppler+dV_Doppler)/Cst_c) )
!
!    Un calibrated Planck function
     do Ns = Spectrum_L1a%Ns_First, Spectrum_L1a%Ns_Last
        n = Ns - Spectrum_L1a%Ns_First + 1
        call plkdirect( Interf_Param%T_BB, Spectrum_L1a%Wn(n), B_BB(n) )
     end do
!
!    Calibrated Planck function
     do Ns = Spectrum_L1a%Ns_First, Spectrum_L1a%Ns_Last
        n = Ns - Spectrum_L1a%Ns_First + 1
        call plkdirect( Interf_Param%T_BB, Wn(n), B_BB_C(n) )
     end do
!
!    Spectral Calibration of the calibration coefficient
     Spectrum_L1a%Real_Part(1:Spectrum_L1a%N_Sample) =   &
         Spectrum_L1a%Real_Part(1:Spectrum_L1a%N_Sample) &
                         / B_BB(1:Spectrum_L1a%N_Sample) &
                         * B_BB_C(1:Spectrum_L1a%N_Sample)
!
!    deallocation
     deallocate( B_BB )
     deallocate( B_BB_C )
     deallocate( Wn )
     deallocate( Fcs )
     return
   end subroutine spectrum_post_calib
!
!
!> spectrum_calib_l1b -- Public
!!
!! * Purpose
!!
!!     Level 1b spectrum computation
!!
!! * Description
!! 
!!     This subroutine allows to compute level 1b spectrum. It updates the type dedicated
!!     to the level 1b spectrum thanks to the following steps:
!!      - The first step consists in level 1a spectrum oversampling either by zero padding, 
!!        by dephasing or by spline interpolation.
!!      - Then the oversampled spectral basis is computed by spline interpolation and the 
!!        level 1a spectrum basis is calibrated.
!!      - At last, after the level 1b spectrum type is initialised, the level 1b spectrum
!!        is sampled thanks to a spline interpolation.
!!
!! * Inputs
!!
!!     - Spectral_L1b : type_Spectral_L1b / type for spectral l1b parameters declaration
!!                      and allocation
!!     - PN           : Pixel
!!     - V_Doppler    : Doppler speed
!!     - dV_Doppler   : Doppler speed defect
!!     - SB           : spectral band
!!     - Srf          : type_Srf / type for declaration and allocation of srf  
!!     - Spectrum_1a  : type_Spectrum / type for declaration and allocation of spectrum 
!!
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Spectrum_1b : type_Spectrum / type for declaration and allocation of spectrum 
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : AD
!!

   subroutine spectrum_calib_l1b( Spectral_L1b,&
                                  SB,PN,       &
                                  V_Doppler,   &
                                  dV_Doppler,  &
                                  Srf,         &
                                  Spectrum_1a, &
                                  Spectrum_1b  )
     implicit none
     type(type_Spectral_L1b)   ,intent(inout)             :: Spectral_L1b
     integer(kind=LONG)        ,intent(in)                :: SB
     integer(kind=LONG)        ,intent(in)                :: PN
     real(kind=DOUBLE)         ,intent(in)                :: V_Doppler
     real(kind=DOUBLE)         ,intent(in)                :: dV_Doppler
     type(type_Srf)            ,intent(in)                :: Srf
     type(type_Spectrum)       ,intent(in)                :: Spectrum_1a
     type(type_Spectrum)       ,intent(out)               :: Spectrum_1b
     type(type_Spectrum)                                  :: Spectrum_os
     type(type_Spectrum)                                  :: Spectrum_tmp
     real(kind=DOUBLE)         ,dimension(:) ,allocatable :: Fcs_os
     real(kind=DOUBLE)         ,dimension(:) ,allocatable :: Wn_1a_Calib
     integer(kind=LONG)                                   :: ErrCode
!
!    spectrum l1a transfert
     call spectrum_transfert( Spectrum_1a, Spectrum_tmp )
!
!    spectrum edges smoothing
     if( Spectral_L1b%SigS /= 0 ) then
        call windowing( Spectrum_tmp%N_Sample, &
                        Spectrum_tmp%Real_Part,&
                        Spectral_L1b%SigS,     &
                        Spectrum_tmp%dWn       )
     end if
!
!    spectrum level 1a oversampling
     if(     Spectral_L1b%OSFactor_zeropad > 1 ) then
        call spectrum_os_zero_padding( Spectrum_tmp%Wn(1),                    &
                                       Spectrum_tmp%Wn(Spectrum_tmp%N_Sample),&
                                       Spectral_L1b%NsFft,                    &
                                       Spectral_L1b%OSFactor_zeropad,         &
                                       Spectrum_tmp,                          &
                                       Spectrum_os                            )
        write(*,*) 'End spectrum_os_zero_padding'
     else if( Spectral_L1b%OSFactor_dephas > 1 ) then
        call spectrum_os_dephasing( Spectrum_tmp%Wn(1),                    &
                                    Spectrum_tmp%Wn(Spectrum_tmp%N_Sample),&
                                    Spectral_L1b%NsFft,                    &
                                    Spectral_L1b%OSFactor_dephas,          &
                                    Spectrum_tmp,                          &
                                    Spectrum_os                            )
        write(*,*) 'End spectrum_os_dephasing'
     else if( Spectral_L1b%OSFactor_spline > 1 ) then
        call spectrum_os_spline( Spectrum_tmp%Wn(1),                    &
                                 Spectrum_tmp%Wn(Spectrum_tmp%N_Sample),&
                                 Spectral_L1b%OSFactor_spline,          &
                                 Spectrum_tmp,                          &
                                 Spectrum_os                            )
        write(*,*) 'End spectrum_os_spline'
     else
        write(*,*) 'Over Sampling Error'
        write(*,*) 'spectrum_calib_l1b Fatal Error'
        call exit(1)
     end if
     write(*,*) 'Spectrum_os%N_Sample',Spectrum_os%N_Sample
     write(*,*) 'Spectrum_os%Ns_First',Spectrum_os%Ns_First
     write(*,*) 'Spectrum_os%Ns_Last ',Spectrum_os%Ns_Last
     write(*,*) 'Spectrum_os%Wn_First',Spectrum_os%Wn_First
     write(*,*) 'Spectrum_os%Wn_Last ',Spectrum_os%Wn_Last
     write(*,*) 'Spectrum_os%dWn     ',Spectrum_os%dWn
     write(*,*) 'Spectrum_os%Wn 1    ',Spectrum_os%Wn(1)
     write(*,*) 'Spectrum_os%Wn N_Sample',Spectrum_os%Wn(Spectrum_os%N_Sample)
!
!    spectral calibration function sampling on the os basis
     allocate( Fcs_os(Spectrum_os%N_Sample), stat=ErrCode )
     if( ErrCode /= 0 ) then
        write(*,*) 'spectrum_calib_l1b allocation error'
        call exit(1)
     end if
     call intspline( Srf%NsWn0,            &
                     Srf%Wn0,              &
                     Srf%Fcs1a(1,PN),      &
                     Spectrum_os%N_Sample, &
                     Spectrum_os%Wn,       &
                     Fcs_os                )
     write(*,*) 'Fcs_os 1       ',Fcs_os(1)
     write(*,*) 'Fcs_os N_Sample',Fcs_os(Spectrum_os%N_Sample)
     write(*,*) 'End intspline Fcs_os'
!
!    spectrum level 1a basis calibration
     allocate( Wn_1a_Calib(Spectrum_os%N_Sample), stat=ErrCode )
     if( ErrCode /= 0 ) then
        write(*,*) 'spectrum_calib_l1b allocation error'
        call exit(1)
     end if
     Wn_1a_Calib(1:Spectrum_os%N_Sample) =                &
                 Spectrum_os%Wn(1:Spectrum_os%N_Sample)   &
               * (1.d+00 + Fcs_os(1:Spectrum_os%N_Sample) &
                         - ((V_Doppler+dV_Doppler)/Cst_c) )
!
!    spectrum level 1b initialisation
     Spectrum_1b%Type     = 'R'
     Spectrum_1b%dWn      = Spectral_L1b%dWn
     Spectrum_1b%Ns_First = idnint(Spectral_L1b%WnS(SB)/Spectrum_1b%dWn) + 1
     Spectrum_1b%Ns_Last  = idnint(Spectral_L1b%WnE(SB)/Spectrum_1b%dWn) + 1
     Spectrum_1b%N_Sample = Spectrum_1b%Ns_Last-Spectrum_1b%Ns_First+1
     Spectrum_1b%Wn_First = Spectrum_1b%dWn*(Spectrum_1b%Ns_First-1)
     Spectrum_1b%Wn_Last  = Spectrum_1b%dWn*(Spectrum_1b%Ns_Last-1)
     Spectrum_1b%WnMax    = Spectrum_1b%Wn_Last
     write(*,*) 'Spectrum_1b%N_Sample',Spectrum_1b%N_Sample
     write(*,*) 'Spectrum_1b%Ns_First',Spectrum_1b%Ns_First
     write(*,*) 'Spectrum_1b%Ns_Last ',Spectrum_1b%Ns_Last
     write(*,*) 'Spectrum_1b%Wn_First',Spectrum_1b%Wn_First
     write(*,*) 'Spectrum_1b%Wn_Last ',Spectrum_1b%Wn_Last
     write(*,*) 'Spectrum_1b%dWn     ',Spectrum_1b%dWn
     call alloc_Spectrum( Spectrum_1b )
     call Spectrum_Basis( Spectrum_1b )
!
!    spectrum level 1b sampling
     write(*,*) 'Spectrum_os%N_Sample          ',Spectrum_os%N_Sample
     write(*,*) 'Wn_1a_Calib 1                 ',Wn_1a_Calib(1)
     write(*,*) 'Wn_1a_Calib N_Sample          ',&
                 Wn_1a_Calib(Spectrum_os%N_Sample)
     write(*,*) 'Spectrum_os%Real_Part 1       ',Spectrum_os%Real_Part(1)
     write(*,*) 'Spectrum_os%Real_Part N_Sample',&
                 Spectrum_os%Real_Part(Spectrum_os%N_Sample)
     call intspline( Spectrum_os%N_Sample,  &
                     Wn_1a_Calib,           &
                     Spectrum_os%Real_Part, &
                     Spectrum_1b%N_Sample,  &
                     Spectrum_1b%Wn,        &
                     Spectrum_1b%Real_Part  )
     write(*,*) 'End intspline Spectrum_1b%Real_Part'
     call dalloc_Spectrum( Spectrum_tmp )
     call dalloc_Spectrum( Spectrum_os )
     deallocate( Fcs_os )
     deallocate( Wn_1a_Calib )
!
     return
   end subroutine spectrum_calib_l1b
!
!

!> spectrum_apodspa_ref_l1c -- Public
!!
!! * Purpose
!!
!!     L1c spectrum computation by l1b spectrum apodisation. 
!!
!! * Description
!! 
!!     This subroutine allows to provide to the users with a level 1c spectrum independent
!!     of instrument state. The level 1c spectrum is the result of the convolution between
!!     level 1b spectrum and the Fourier transform of the apodisation functions. 
!!     In the interferogram space, the algorithm multiply the Fourier transform of the level
!!     1b spectrum by an apodisation function. 
!!     The following steps are required:
!!      - The first step consists in level 1c spectrum and interferogram initialisation.
!!      - Then the interferogram is duplicated in an temporary interferogram, and the flip-flop
!!        spectra are initialised
!!      - After the required tables allocation, the level 1b spectrum is smoothed by windowing.
!!      - Level 1b spectrum Fourier transform is computed, and the first and last apodisation 
!!        functions are searched by dichotomy.
!!      - After the computation of sample corresponding to the apodisation function, this one
!!        is resampled by spline interpolation and is normalised.
!!      - Then the apodisation function is applied to the interferogram and the inverse Fourier
!!        transform is computed in order to obtain a spectrum, for which the spectrum sample 
!!        are interpolated.
!!      - At last, flip-flop spectrum transfert is done.
!!
!! * Inputs
!!
!!     - Spectral_L1b : type_Spectral_L1b / type for spectral l1b parameters declaration
!!                      and allocation
!!     - SB           : spectral band
!!     - Apf          : type_Srf / type for declaration and allocation of srf  
!!     - Spectrum_1b  : type_Spectrum / type for declaration and allocation of spectrum 
!!
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Spectrum_1c : type_Spectrum / type for declaration and allocation of spectrum 
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : AD
!!

   subroutine spectrum_apodspa_ref_l1c( Spectral_L1c,&
                                       SB,PN,       &
                                       Srf,         &
                                       Apf,         &
                                       Spectrum_1b, &
                                       Spectrum_1c  )
     implicit none
     type(type_Spectral_L1c)   ,intent(in)                :: Spectral_L1c
     integer(kind=LONG)        ,intent(in)                :: SB
     integer(kind=LONG)        ,intent(in)                :: PN
     type(type_Srf)            ,intent(in)                :: Srf
     type(type_Apf)            ,intent(in)                :: Apf
     type(type_Spectrum)       ,intent(in)                :: Spectrum_1b
     type(type_Spectrum)       ,intent(out)               :: Spectrum_1c
     type(type_Spectrum)                                  :: Spectrum_smooth
     type(type_Spectrum)                                  :: Spectrum1
     type(type_Spectrum)                                  :: Spectrum2
     type(type_Interferogram)                             :: Interf
     type(type_Interferogram)                             :: Interf_temp
     integer(kind=LONG)                                   :: NF
     integer(kind=LONG)                                   :: NsSW1
     integer(kind=LONG)                                   :: NsSW2
     integer(kind=LONG)                                   :: Ns
     integer(kind=LONG)                                   :: NsFirst
     integer(kind=LONG)                                   :: NsLast
     integer(kind=LONG)                                   :: ErrCode
     integer(kind=LONG)                                   :: ioalloc
     real(kind=DOUBLE)                                    :: WnS
     real(kind=DOUBLE)                                    :: WnE
     real(kind=DOUBLE)         ,dimension(:) ,allocatable :: L1cTF
     real(kind=DOUBLE)         ,dimension(:) ,allocatable :: Spatia_Mod
     real(kind=DOUBLE)         ,dimension(:) ,allocatable :: Spatia_Arg
     complex(kind=DOUBLE)      ,dimension(:) ,allocatable :: Spatia_Cplx
     real(kind=DOUBLE)                                    :: pds1
     real(kind=DOUBLE)                                    :: pds2
     integer(kind=LONG)                                   :: n1
     integer(kind=LONG)                                   :: n2
     integer(kind=LONG)                                   :: NFstart
     integer(kind=LONG)                                   :: NFend
     real(kind=DOUBLE)                                    :: Opd0
     real(kind=DOUBLE)                                    :: Mod0
     real(kind=DOUBLE)                                    :: Arg0
     integer(kind=LONG)                                   :: Ns_Erase
!
     ioalloc = 0
!
!    spectrum level 1c initialisation
     Spectrum_1c%Type     = 'RI'
     Spectrum_1c%dWn      = Spectral_L1c%dWn
     Spectrum_1c%Ns_First = idnint(Spectral_L1c%WnS(SB)/Spectrum_1c%dWn) + 1
     Spectrum_1c%Ns_Last  = idnint(Spectral_L1c%WnE(SB)/Spectrum_1c%dWn) + 1
     Spectrum_1c%N_Sample = Spectrum_1c%Ns_Last-Spectrum_1c%Ns_First+1
     Spectrum_1c%Wn_First = Spectrum_1c%dWn*(Spectrum_1c%Ns_First-1)
     Spectrum_1c%Wn_Last  = Spectrum_1c%dWn*(Spectrum_1c%Ns_Last-1)
     Spectrum_1c%WnMax    = Spectrum_1c%Wn_Last
     write(*,*) 'Spectrum_1c%N_Sample',Spectrum_1c%N_Sample
     write(*,*) 'Spectrum_1c%Ns_First',Spectrum_1c%Ns_First
     write(*,*) 'Spectrum_1c%Ns_Last ',Spectrum_1c%Ns_Last
     write(*,*) 'Spectrum_1c%Wn_First',Spectrum_1c%Wn_First
     write(*,*) 'Spectrum_1c%Wn_Last ',Spectrum_1c%Wn_Last
     write(*,*) 'Spectrum_1c%dWn     ',Spectrum_1c%dWn
     call alloc_Spectrum( Spectrum_1c )
     call Spectrum_Basis( Spectrum_1c )
!
!    coherence check
     if( Spectrum_1c%dWn /= Spectrum_1b%dWn ) then
        write(*,*) 'Wrong L1c spectral sampling', &
                    Spectrum_1b%dWn,Spectrum_1c%dWn
        call exit(1)
     end if
!
!    interferogram initialisation
     Interf%Type     = 'C'
     Interf%N_Sample = Spectral_L1c%NsFft+1
     Interf%dOpd     = 1.d+00 / ( Spectrum_1c%dWn*dble(Spectral_L1c%NsFft) )
     Interf%OpdMax   = Interf%dOpd * dble(Spectral_L1c%NsFft/2)
     write(*,*) 'Interf%N_Sample',Interf%N_Sample
     write(*,*) 'Interf%dOpd    ',Interf%dOpd
     write(*,*) 'Interf%OpdMax  ',Interf%OpdMax
     call alloc_Interferogram( Interf )
     call Interferogram_Basis( Interf )
     Opd0 = Interf%Opd(int(Interf%N_Sample/2)+1) 
     Interf%Opd(1:Interf%N_Sample) = Interf%Opd(1:Interf%N_Sample) - Opd0
!
!    Apodisation resampling
     allocate( L1cTF(Interf%N_Sample), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     call intspline( Apf%NsOpd, Apf%Opd, Apf%L1cTF,     &
                     Interf%N_Sample, Interf%Opd, L1cTF )
!
!    temporary interferogram
     Interf_temp%Type     = Interf%Type
     Interf_temp%N_Sample = Interf%N_Sample
     Interf_temp%dOpd     = Interf%dOpd
     Interf_temp%OpdMax   = Interf%OpdMax
     call alloc_Interferogram( Interf_temp )
     call Interferogram_Basis( Interf_temp )
     Interf_temp%Opd(1:Interf_temp%N_Sample) = &
                       Interf%Opd(1:Interf%N_Sample)
!
!    Interferogram transfert
     Interf_temp = Interf
!
!    flip flop spectra initialisation
     Spectrum1%Type     = 'RI'
     Spectrum1%dWn      = Spectrum_1b%dWn
     Spectrum1%Ns_First = Spectrum_1b%Ns_First
     Spectrum1%Ns_Last  = Spectrum_1b%Ns_Last
     Spectrum1%N_Sample = Spectrum_1b%N_Sample
     Spectrum1%Wn_First = Spectrum_1b%Wn_First
     Spectrum1%Wn_Last  = Spectrum_1b%Wn_Last
     Spectrum1%WnMax    = Spectrum_1b%WnMax
     write(*,*) 'Spectrum1%N_Sample',Spectrum1%N_Sample
     write(*,*) 'Spectrum1%Ns_First',Spectrum1%Ns_First
     write(*,*) 'Spectrum1%Ns_Last ',Spectrum1%Ns_Last
     write(*,*) 'Spectrum1%Wn_First',Spectrum1%Wn_First
     write(*,*) 'Spectrum1%Wn_Last ',Spectrum1%Wn_Last
     write(*,*) 'Spectrum1%dWn     ',Spectrum1%dWn
     call alloc_Spectrum( Spectrum1 )
     call Spectrum_Basis( Spectrum1 )
     Spectrum2%Type     = 'RI'
     Spectrum2%dWn      = Spectrum_1b%dWn
     Spectrum2%Ns_First = Spectrum_1b%Ns_First
     Spectrum2%Ns_Last  = Spectrum_1b%Ns_Last
     Spectrum2%N_Sample = Spectrum_1b%N_Sample
     Spectrum2%Wn_First = Spectrum_1b%Wn_First
     Spectrum2%Wn_Last  = Spectrum_1b%Wn_Last
     Spectrum2%WnMax    = Spectrum_1b%WnMax
     write(*,*) 'Spectrum2%N_Sample',Spectrum2%N_Sample
     write(*,*) 'Spectrum2%Ns_First',Spectrum2%Ns_First
     write(*,*) 'Spectrum2%Ns_Last ',Spectrum2%Ns_Last
     write(*,*) 'Spectrum2%Wn_First',Spectrum2%Wn_First
     write(*,*) 'Spectrum2%Wn_Last ',Spectrum2%Wn_Last
     write(*,*) 'Spectrum2%dWn     ',Spectrum2%dWn
     call alloc_Spectrum( Spectrum2 )
     call Spectrum_Basis( Spectrum2 )
!
!    allocation
     allocate( Spatia_Mod(Interf%N_Sample), stat=ErrCode)
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Spatia_Arg(Interf%N_Sample), stat=ErrCode)
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Spatia_Cplx(Interf%N_Sample), stat=ErrCode)
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     if( ioalloc /= 0 ) then
        write(*,*) 'spectrum_apodspa_ref_l1c Allocation Error',ioalloc
        call exit(1)
     end if
!
!    extract spectrum
     if( Spectral_L1c%SigS /= 0 ) then
        WnS = Spectrum_1c%Wn_First &
            - dble(2*Spectral_L1c%SigS+1) * Spectrum_1b%dWn
        WnE = Spectrum_1c%Wn_Last  &
            + dble(2*Spectral_L1c%SigS+1) * Spectrum_1b%dWn
        call spectrum_extract( WnS, WnE, Spectrum_1b, Spectrum_smooth )
        call windowing( Spectrum_smooth%N_Sample, &
                        Spectrum_smooth%Real_Part,&
                        Spectral_L1c%SigS,        &
                        Spectrum_smooth%dWn       )
     else
        WnS = Spectrum_1c%Wn_First
        WnE = Spectrum_1c%Wn_Last
        call spectrum_extract( WnS, WnE, Spectrum_1b, Spectrum_smooth )
     end if
!
!    Spectrum l1b Fourier transform
     call sptoif( Spectrum_smooth, Interf )
     write(*,*) 'end sptoif Interf'
!
!    search for first apodisation function
     n1 = 1
     n2 = Apf%NsWn0
     call dichotomd( Spectrum_1c%Wn_First, Apf%Wn0, n1, n2 )
     NFstart = n1
!
!    search for last apodisation function
     n1 = 1
     n2 = Apf%NsWn0
     call dichotomd( Spectrum_1c%Wn_Last, Apf%Wn0, n1, n2 )
     NFend = n2
     write(*,*) 'NFstart         ',NFstart
     write(*,*) 'NFend          ',NFend
     write(*,*) 'Apf%Wn0(NFstart)',Apf%Wn0(NFstart)
     write(*,*) 'Apf%Wn0(NFend) ',Apf%Wn0(NFend)
!
!    Apodisation function coherence check
     if( Spectrum_1c%Wn_First < Apf%Wn0(NFstart) .or. &
         Apf%Wn0(NFend) < Spectrum_1c%Wn_Last ) then
        write(*,*) 'Insufficent Apf spectral coverage'
        call exit(1)
     end if
!
!    Effective Opd introduction
     n1 = 1
     n2 = Interf%N_Sample
     call dichotomd( Srf%Opd_effective, Interf%Opd(n1), n1, n2 )
     if( n1 == Interf%N_Sample ) then
        Ns_Erase = 0
     else
        Ns_Erase = Interf%N_Sample - n1
     end if
     write(*,*) 'Spatia_ref_l1c Ns_Erase',Ns_Erase
!
!    Sample corresponding to the first apodisation function
     NsSW2  = idnint(Apf%Wn0(NFstart)   / Spectrum_1b%dWn) + 1
!
!    Apodisation function loop
     do NF =  NFstart, NFend
        write(*,*) 'Apf%Wn0',NF, Apf%Wn0(NF)
!
!       Sample corresponding to the apodisation functions
        NsSW1 = NsSW2
        NsSW2 = idnint( Apf%Wn0(NF) / Spectrum_1b%dWn ) + 1
        write(*,*) 'NF,NsSW1,NsSW2 ',NF,NsSW1,NsSW2
        if( (NsSW2-NsSW1) > int(Spectral_L1c%NsFft/2) ) then
           write(*,*) 'Spectral window too small'
           write(*,*) 'spectrum_apodspa_opt_l1c Fatal Error'
           call exit(1)
        end if
!
!       Apodisation Function resampling
        Spatia_Mod(1:Interf%N_Sample) = 0.d+00
        Spatia_Arg(1:Interf%N_Sample) = 0.d+00
        call intspline( Apf%NsOpd,       &
                        Apf%Opd,         &
                        Apf%Mod(1,NF,PN),&
                        Interf%N_Sample, &
                        Interf%Opd,      &
                        Spatia_Mod       )
!
        call intspline( Apf%NsOpd,       &
                        Apf%Opd,         &
                        Apf%Arg(1,NF,PN),&
                        Interf%N_Sample, &
                        Interf%Opd,      &
                        Spatia_Arg       )
!
        select case ( trim(Spectral_L1c%Mode) )
        case ( 'CALREF' )
           write(*,*) 'Despatialisation'
        case default
           write(*,*) 'Despatialisation & apodisation'
           Spatia_Mod(1:Interf%N_Sample) = Spatia_Mod(1:Interf%N_Sample)&
                                         * L1cTF(1:Interf%N_Sample)
        end select
!
!       Normalisation
        Mod0 = Spatia_Mod(int(Interf%N_Sample/2)+1)
        Arg0 = Spatia_Arg(int(Interf%N_Sample/2)+1)
        Spatia_Mod(1:Interf%N_Sample) = Spatia_Mod(1:Interf%N_Sample)/Mod0
        Spatia_Arg(1:Interf%N_Sample) = Spatia_Arg(1:Interf%N_Sample)-Arg0
        call modargtocplx( Spatia_Cplx,    &
                           Spatia_Mod,     &
                           Spatia_Arg,     &
                           Interf%N_Sample )
        if( Ns_Erase /= 0 ) then
           Spatia_Cplx(1:Ns_Erase) = dcmplx(0.d+00,0.d+00)
           Spatia_Cplx(Interf%N_Sample-Ns_Erase+1:Interf%N_Sample) = &
                                                 dcmplx(0.d+00,0.d+00)
        end if
        write(*,*) 'End intspline Spatia_Arg'
!
!       Apodisation Function application
        Interf_temp%Complex(1:Interf%N_Sample) =          &
                         Interf%Complex(1:Interf%N_Sample)&
                       * Spatia_Cplx(1:Interf%N_Sample)
!
!       Inverse Fourier transform
        call iftosp_open( Interf_temp, Spectrum2 )
        write(*,*) 'End iftosp Spectrum2'
!
!       spectrum sample interpolation loop
        if( NF /= NFstart ) then
           if( (NF-1) == NFstart ) then
              NsFirst = Spectrum_1c%Ns_First
           else
              NsFirst = NsSW1
           end if
           if( (NF) == NFend ) then
              NsLast = Spectrum_1c%Ns_Last
           else
              NsLast = NsSW2
           end if
           do Ns = NsFirst, NsLast
              pds2 = dble(Ns - NsSW1) / dble(NsSW2 - NsSW1)
              pds1 = 1.d+00 - pds2
              Spectrum_1c%Real_Part(Ns-Spectrum_1c%Ns_First+1) =        &
                 ( pds1 * Spectrum1%Real_Part(Ns-Spectrum1%Ns_First+1) )&
               + ( pds2 * Spectrum2%Real_Part(Ns-Spectrum2%Ns_First+1) )
              Spectrum_1c%Imag_Part(Ns-Spectrum_1c%Ns_First+1) =        &
                 ( pds1 * Spectrum1%Imag_Part(Ns-Spectrum1%Ns_First+1) )&
               + ( pds2 * Spectrum2%Imag_Part(Ns-Spectrum2%Ns_First+1) )
           end do ! End spectrum sample interpolation loop
        end if
!
!       flip flop spectrum transfert
        Spectrum1 = Spectrum2
     end do ! End Apodisation function loop
     write(*,*) 'End Apodisation'
!
!    deallocation
     deallocate( Spatia_Cplx )
     deallocate( Spatia_Mod )
     deallocate( Spatia_Arg )
     deallocate( L1cTF )
     call dalloc_Interferogram( Interf )
     call dalloc_Interferogram( Interf_temp )
     call dalloc_Spectrum( Spectrum1 )
     call dalloc_Spectrum( Spectrum2 )
     call dalloc_Spectrum( Spectrum_smooth )
!
     return
   end subroutine spectrum_apodspa_ref_l1c
!
!


!
!
!> spectrum_apodspa_opt_l1c -- Public
!!
!! * Purpose
!!
!!     L1c spectrum optimised computation by l1b spectrum apodisation. 
!!
!! * Description
!! 
!!     This subroutine allows to provide to the users with a level 1c spectrum independent
!!     of instrument state. The level 1c spectrum is the result of the convolution between
!!     level 1b spectrum and the Fourier transform of the apodisation functions.
!!     This algorithm consists in an optimised computation.
!!     In the interferogram space, the algorithm multiply the Fourier transform of the level
!!     1b spectrum by an apodisation function. 
!!     The following steps are required:
!!      - The first step consists in level 1c spectrum and interferogram initialisation.
!!      - After the required tables allocation, the first and last apodisation 
!!        functions are searched by dichotomy.
!!      - After the computation of sample corresponding to the apodisation function, spectrum
!!        corresponding to the spectral window is extracted and the spectrum edge are smoothed.
!!      - Then the Fourier transform is computed in order to obtain an interferogram
!!      - Then the apodisation function is resampled by a spline interpolation and normalised.
!!      - Then the apodisation function is applied to the interferogram and the inverse Fourier
!!        transform is computed in order to obtain a spectrum, for which the spectrum sample 
!!        are interpolated.
!!      - At last, before all tables deallocation, flip-flop spectrum transfert is done.
!!
!! * Inputs
!!
!!     - Spectral_L1b : type_Spectral_L1b / type for spectral l1b parameters declaration
!!                      and allocation
!!     - SB           : spectral band
!!     - Apf          : type_Srf / type for declaration and allocation of srf  
!!     - Spectrum_1b  : type_Spectrum / type for declaration and allocation of spectrum 
!!
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Spectrum_1c : type_Spectrum / type for declaration and allocation of spectrum 
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : AD
!!

   subroutine spectrum_apodspa_opt_l1c( Spectral_L1c,&
                                       SB,PN,       &
                                       Srf,         &
                                       Apf,         &
                                       Spectrum_1b, &
                                       Spectrum_1c  )
     implicit none
     type(type_Spectral_L1c)   ,intent(in)                :: Spectral_L1c
     integer(kind=LONG)        ,intent(in)                :: SB
     integer(kind=LONG)        ,intent(in)                :: PN
     type(type_Srf)            ,intent(in)                :: Srf
     type(type_Apf)            ,intent(in)                :: Apf
     type(type_Spectrum)       ,intent(in)                :: Spectrum_1b
     type(type_Spectrum)       ,intent(out)               :: Spectrum_1c
     type(type_Spectrum)                                  :: Spectrum_SW
     type(type_Spectrum)                                  :: Spectrum1
     type(type_Spectrum)                                  :: Spectrum2
     type(type_Interferogram)                             :: Interf
     integer(kind=LONG)                                   :: NF
     integer(kind=LONG)                                   :: NsSW1
     integer(kind=LONG)                                   :: NsSW2
     real(kind=DOUBLE)                                    :: WnS
     real(kind=DOUBLE)                                    :: WnE
     integer(kind=LONG)                                   :: Ns
     integer(kind=LONG)                                   :: NsFirst
     integer(kind=LONG)                                   :: NsLast
     integer(kind=LONG)                                   :: ErrCode
     integer(kind=LONG)                                   :: ioalloc
     real(kind=DOUBLE)         ,dimension(:) ,allocatable :: L1cTF
     real(kind=DOUBLE)         ,dimension(:) ,allocatable :: Spatia_Mod
     real(kind=DOUBLE)         ,dimension(:) ,allocatable :: Spatia_Arg
     complex(kind=DOUBLE)      ,dimension(:) ,allocatable :: Spatia_Cplx
     real(kind=DOUBLE)                                    :: pds1
     real(kind=DOUBLE)                                    :: pds2
     integer(kind=LONG)                                   :: n1
     integer(kind=LONG)                                   :: n2
     integer(kind=LONG)                                   :: NFstart
     integer(kind=LONG)                                   :: NFend
     integer(kind=LONG)                                   :: Sens
     real(kind=DOUBLE)                                    :: Opd0
     real(kind=DOUBLE)                                    :: Mod0
     real(kind=DOUBLE)                                    :: Arg0
     integer(kind=LONG)                                   :: Ns_Erase
!
     ioalloc = 0
!
!    spectrum level 1c initialisation
     Spectrum_1c%Type     = 'RI'
     Spectrum_1c%dWn      = Spectral_L1c%dWn
     Spectrum_1c%Ns_First = idnint(Spectral_L1c%WnS(SB)/Spectrum_1c%dWn) + 1
     Spectrum_1c%Ns_Last  = idnint(Spectral_L1c%WnE(SB)/Spectrum_1c%dWn) + 1
     Spectrum_1c%N_Sample = Spectrum_1c%Ns_Last-Spectrum_1c%Ns_First+1
     Spectrum_1c%Wn_First = Spectrum_1c%dWn*(Spectrum_1c%Ns_First-1)
     Spectrum_1c%Wn_Last  = Spectrum_1c%dWn*(Spectrum_1c%Ns_Last-1)
     Spectrum_1c%WnMax    = Spectrum_1c%Wn_Last
     write(*,*) 'Spectrum_1c%N_Sample',Spectrum_1c%N_Sample
     write(*,*) 'Spectrum_1c%Ns_First',Spectrum_1c%Ns_First
     write(*,*) 'Spectrum_1c%Ns_Last ',Spectrum_1c%Ns_Last
     write(*,*) 'Spectrum_1c%Wn_First',Spectrum_1c%Wn_First
     write(*,*) 'Spectrum_1c%Wn_Last ',Spectrum_1c%Wn_Last
     write(*,*) 'Spectrum_1c%dWn     ',Spectrum_1c%dWn
     call alloc_Spectrum( Spectrum_1c )
     call Spectrum_Basis( Spectrum_1c )
!
!    coherence check
     if( Spectrum_1c%dWn /= Spectrum_1b%dWn ) then
        write(*,*) 'Wrong L1c spectral sampling', &
                    Spectrum_1b%dWn,Spectrum_1c%dWn
        call exit(1)
     end if
!
!    interferogram initialisation
     Interf%Type     = 'C'
     Interf%N_Sample = Spectral_L1c%NsFft+1
     Interf%dOpd     = 1.d+00 / ( Spectrum_1c%dWn*dble(Spectral_L1c%NsFft) )
     Interf%OpdMax   = Interf%dOpd * dble(Spectral_L1c%NsFft/2)
     write(*,*) 'Interf%N_Sample',Interf%N_Sample
     write(*,*) 'Interf%dOpd    ',Interf%dOpd
     write(*,*) 'Interf%OpdMax  ',Interf%OpdMax
     call alloc_Interferogram( Interf )
     call Interferogram_Basis( Interf )
     Opd0 = Interf%Opd(int(Interf%N_Sample/2)+1) 
     Interf%Opd(1:Interf%N_Sample) = Interf%Opd(1:Interf%N_Sample) - Opd0
!
!    Apodisation resampling
     allocate( L1cTF(Interf%N_Sample), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     call intspline( Apf%NsOpd, Apf%Opd, Apf%L1cTF,     &
                     Interf%N_Sample, Interf%Opd, L1cTF )
!
!    allocation
     allocate( Spatia_Mod(Interf%N_Sample), stat=ErrCode)
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Spatia_Arg(Interf%N_Sample), stat=ErrCode)
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Spatia_Cplx(Interf%N_Sample), stat=ErrCode)
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     if( ioalloc /= 0 ) then
        write(*,*) 'spectrum_apodspa_opt_l1c Allocation Error',ioalloc
        call exit(1)
     end if
     Spectrum1%Type     = 'C'
     Spectrum1%N_Sample = Spectral_L1c%NsFft+1
     call alloc_Spectrum( Spectrum1 )
     Spectrum2%Type     = 'C'
     Spectrum2%N_Sample = Spectral_L1c%NsFft+1
     call alloc_Spectrum( Spectrum2 )
!
!    search for first apodisation function
     n1 = 1
     n2 = Apf%NsWn0
     call dichotomd( Spectrum_1c%Wn_First, Apf%Wn0, n1, n2 )
     NFstart = n1
!
!    search for last apodisation function
     n1 = 1
     n2 = Apf%NsWn0
     call dichotomd( Spectrum_1c%Wn_Last, Apf%Wn0, n1, n2 )
     NFend = n2
     write(*,*) 'NFstart         ',NFstart
     write(*,*) 'NFend          ',NFend
     write(*,*) 'Apf%Wn0(NFstart)',Apf%Wn0(NFstart)
     write(*,*) 'Apf%Wn0(NFend) ',Apf%Wn0(NFend)
!
!    Apodisation function coherence check
     if( Spectrum_1c%Wn_First < Apf%Wn0(NFstart) .or. &
         Apf%Wn0(NFend) < Spectrum_1c%Wn_Last ) then
        write(*,*) 'Insufficent Apf spectral coverage'
        call exit(1)
     end if
!
!    Effective Opd introduction
     n1 = 1
     n2 = Interf%N_Sample
     call dichotomd( Srf%Opd_effective, Interf%Opd(n1), n1, n2 )
     if( n1 == Interf%N_Sample ) then
        Ns_Erase = 0
     else
        Ns_Erase = Interf%N_Sample - n1
     end if
     write(*,*) 'Apodspa_opt_l1c Ns_Erase',Ns_Erase
!
!    Sample corresponding to the first apodisation function
     NsSW2  = idnint(Apf%Wn0(NFstart) / Spectrum_1b%dWn) + 1
!
!    Apodisation function loop
     do NF = NFstart, NFend
!
!       Sample corresponding to the apodisation functions
        NsSW1 = NsSW2
        NsSW2 = idnint( Apf%Wn0(NF) / Spectrum_1b%dWn ) + 1
        write(*,*) 'NF,NsSW1,NsSW2 ',NF,NsSW1,NsSW2
        if( (NsSW2-NsSW1) > int(Spectral_L1c%NsFft/2) ) then
           write(*,*) 'Spectral window too small'
           write(*,*) 'spectrum_apodspa_opt_l1c Fatal Error'
           call exit(1)
        end if
!
!       Spectral window extraction
        NsFirst = NsSW2 - int(Spectral_L1c%NsFft/2)
        NsLast  = NsSW2 + int(Spectral_L1c%NsFft/2)
        WnS     = dble(NsFirst-1) * Spectrum_1b%dWn
        WnE     = dble(NsLast-1)  * Spectrum_1b%dWn
        call spectrum_extract( WnS, WnE, Spectrum_1b, Spectrum_SW )
        write(*,*) ' End spectrum_extract',NF
        if( Spectrum_SW%N_Sample /= (NsLast-NsFirst+1) ) then
           write(*,*) 'SW Sampling Error', &
                       Spectrum_SW%N_Sample,NsLast-NsFirst+1
           call exit(1)
        end if
!
!       edge spectrum smoothing
        if( Spectral_L1c%SigS /= 0 ) then
           call windowing( Spectrum_SW%N_Sample, &
                           Spectrum_SW%Real_Part,&
                           Spectral_L1c%SigS,    &
                           Spectrum_1b%dWn       )
           write(*,*) 'End SW windowing'
        end if
!
!       Spectrum l1b Fourier transform
        Sens = +1
        call fft_r2c_open( Spectral_L1c%NsFft,   &
                           Sens,                 &
                           Spectrum_SW%Real_Part,&
                           Spectrum_SW%dWn,      &
                           Interf%dOpd,          &
                           Interf%Complex        )
        write(*,*) ' End fft_r2c'
!
!       Apodisation Function resampling
        Spatia_Mod(1:Interf%N_Sample) = 0.d+00
        Spatia_Arg(1:Interf%N_Sample) = 0.d+00
        call intspline( Apf%NsOpd,       &
                        Apf%Opd,         &
                        Apf%Mod(1,NF,PN),&
                        Interf%N_Sample, &
                        Interf%Opd,      &
                        Spatia_Mod       )
        call intspline( Apf%NsOpd,       &
                        Apf%Opd,         &
                        Apf%Arg(1,NF,PN),&
                        Interf%N_Sample, &
                        Interf%Opd,      &
                        Spatia_Arg       )
!
        select case ( trim(Spectral_L1c%Mode) )
        case ( 'CALOPT' )
           write(*,*) 'Despatialisation'
        case default
           write(*,*) 'Despatialisation & apodisation'
           Spatia_Mod(1:Interf%N_Sample) = Spatia_Mod(1:Interf%N_Sample)&
                                         * L1cTF(1:Interf%N_Sample)
        end select
!
!       Normalisation
        Mod0 = Spatia_Mod(int(Interf%N_Sample/2)+1)
        Arg0 = Spatia_Arg(int(Interf%N_Sample/2)+1)
        Spatia_Mod(1:Interf%N_Sample) = Spatia_Mod(1:Interf%N_Sample)/Mod0
        Spatia_Arg(1:Interf%N_Sample) = Spatia_Arg(1:Interf%N_Sample)-Arg0
        call modargtocplx( Spatia_Cplx,    &
                           Spatia_Mod,     &
                           Spatia_Arg,     &
                           Interf%N_Sample )
        if( Ns_Erase /= 0 ) then
           Spatia_Cplx(1:Ns_Erase) = dcmplx(0.d+00,0.d+00)
           Spatia_Cplx(Interf%N_Sample-Ns_Erase+1:Interf%N_Sample) = &
                                                 dcmplx(0.d+00,0.d+00)
        end if
        write(*,*) ' End intspline Apodspa'
!
!       Apodisation Function application
        Interf%Complex(1:Interf%N_Sample) =                    &
                    dconjg( Interf%Complex(1:Interf%N_Sample) )&
                  * Spatia_Cplx(1:Interf%N_Sample)
!
!       flip flop spectra initialisation
        Spectrum2%dWn      = Spectrum_SW%dWn
        Spectrum2%Ns_First = Spectrum_SW%Ns_First
        Spectrum2%Ns_Last  = Spectrum_SW%Ns_Last
        Spectrum2%Wn_First = Spectrum_SW%Wn_First
        Spectrum2%Wn_Last  = Spectrum_SW%Wn_Last
        Spectrum2%WnMax    = Spectrum_SW%WnMax
        write(*,*) 'Spectrum2%Ns_First',Spectrum2%Ns_First
        write(*,*) 'Spectrum2%Ns_Last ',Spectrum2%Ns_Last
        write(*,*) 'Spectrum2%Wn_First',Spectrum2%Wn_First
        write(*,*) 'Spectrum2%Wn_Last ',Spectrum2%Wn_Last
        write(*,*) 'Spectrum2%dWn     ',Spectrum2%dWn
        if( Spectrum2%N_Sample /=                        &
           (Spectrum2%Ns_Last-Spectrum2%Ns_First+1) ) then
           write(*,*) 'spectrum_apodspa_opt_l1c N_Sample Error'
           call exit(1)
        end if
        call Spectrum_Basis( Spectrum2 )
!
!       Inverse Fourier transform
        Sens = -1
        call fft_c2c_open( Spectral_L1c%NsFft, &
                           Sens,               &
                           Interf%Complex,     &
                           Interf%dOpd,        &
                           Spectrum2%dWn,      &
                           Spectrum2%Complex   )
        write(*,*) 'End fft_c2c Spectrum2%Complex'
!
!       spectrum sample interpolation loop
        if( NF /= NFstart ) then
           if( (NF-1) == NFstart ) then
              NsFirst = Spectrum_1c%Ns_First
           else
              NsFirst = NsSW1
           end if
           if( (NF) == NFend ) then
              NsLast = Spectrum_1c%Ns_Last
           else
              NsLast = NsSW2
           end if
           do Ns = NsFirst, NsLast
              pds2 = dble(Ns - NsSW1) / dble(NsSW2 - NsSW1)
              pds1 = 1.d+00 - pds2
              Spectrum_1c%Real_Part(Ns-Spectrum_1c%Ns_First+1) =      &
                 ( pds1 *                                             &
                   dreal(Spectrum1%Complex(Ns-Spectrum1%Ns_First+1)) )&
               + ( pds2 *                                             &
                   dreal(Spectrum2%Complex(Ns-Spectrum2%Ns_First+1)) )
              Spectrum_1c%Imag_Part(Ns-Spectrum_1c%Ns_First+1) =      &
                 ( pds1 *                                             &
                   dimag(Spectrum1%Complex(Ns-Spectrum1%Ns_First+1)) )&
               + ( pds2 *                                             &
                   dimag(Spectrum2%Complex(Ns-Spectrum2%Ns_First+1)) )
           end do ! End spectrum sample interpolation loop
        end if
!
!       flip flop spectrum transfert
        Spectrum1%Ns_First = Spectrum2%Ns_First
        Spectrum1%Ns_Last  = Spectrum2%Ns_Last
        if( Spectrum1%N_Sample /= &
            (Spectrum1%Ns_Last-Spectrum1%Ns_First+1) ) then
           write(*,*) 'flip flop spectrum transfert Error'
           call exit(1)
        end if
        Spectrum1%dWn      = Spectrum2%dWn
        Spectrum1%Wn_First = Spectrum2%Wn_First
        Spectrum1%Wn_Last  = Spectrum2%Wn_Last
        Spectrum1%WnMax    = Spectrum2%WnMax
        Spectrum1%Wn(1:Spectrum1%N_Sample) = &
            Spectrum2%Wn(1:Spectrum2%N_Sample)
        Spectrum1%Complex(1:Spectrum1%N_Sample) = &
            Spectrum2%Complex(1:Spectrum2%N_Sample)
        call dalloc_Spectrum( Spectrum_SW )
     end do ! End Apodisation function loop
     write(*,*) 'End Apodisation function loop'
!
!    deallocation
     call dalloc_Interferogram( Interf )
     deallocate( L1cTF )
     deallocate( Spatia_Mod )
     deallocate( Spatia_Arg )
     call dalloc_Spectrum( Spectrum1 )
     call dalloc_Spectrum( Spectrum2 )
!
     return
   end subroutine spectrum_apodspa_opt_l1c
!
!
   subroutine spectrum_apodspa_l1a_l1c( Spectral_L1c,&
                                        SB,PN,       &
                                        Srf,         &
                                        Apf,         &
                                        Spectrum_1a, &
                                        Spectrum_1c  )
     implicit none
     type(type_Spectral_L1c)   ,intent(in)                :: Spectral_L1c
     integer(kind=LONG)        ,intent(in)                :: SB
     integer(kind=LONG)        ,intent(in)                :: PN
     type(type_Srf)            ,intent(in)                :: Srf
     type(type_Apf)            ,intent(in)                :: Apf
     type(type_Spectrum)       ,intent(in)                :: Spectrum_1a
     type(type_Spectrum)       ,intent(out)               :: Spectrum_1c
     type(type_Spectrum)                                  :: Spectrum_SW
     type(type_Spectrum)                                  :: Spectrum
     type(type_Interferogram)                             :: Interf
     real(kind=DOUBLE)                                    :: WnS
     real(kind=DOUBLE)                                    :: WnE
     integer(kind=LONG)                                   :: Ns
     integer(kind=LONG)                                   :: NsFirst
     integer(kind=LONG)                                   :: NsLast
     integer(kind=LONG)                                   :: ErrCode
     integer(kind=LONG)                                   :: ioalloc
     real(kind=DOUBLE)                                    :: Srf_WnShift_temp
     real(kind=DOUBLE)         ,dimension(:) ,allocatable :: Apf_Mod_temp
     real(kind=DOUBLE)         ,dimension(:) ,allocatable :: Apf_Arg_temp
     real(kind=DOUBLE)         ,dimension(:) ,allocatable :: Spatia_Mod
     real(kind=DOUBLE)         ,dimension(:) ,allocatable :: Spatia_Arg
     complex(kind=DOUBLE)      ,dimension(:) ,allocatable :: Spatia_Cplx
     real(kind=DOUBLE)                                    :: pds1
     real(kind=DOUBLE)                                    :: pds2
     integer(kind=LONG)                                   :: m
     integer(kind=LONG)                                   :: n
     integer(kind=LONG)                                   :: n1
     integer(kind=LONG)                                   :: n2
     integer(kind=LONG)                                   :: NFstart
     integer(kind=LONG)                                   :: NFend
     integer(kind=LONG)                                   :: Sens
     real(kind=DOUBLE)                                    :: Opd0
     real(kind=DOUBLE)                                    :: Mod0
     real(kind=DOUBLE)                                    :: Arg0
     integer(kind=LONG)                                   :: Ns_Erase
!
     ioalloc = 0
!
!    spectrum level 1c initialisation
     Spectrum_1c%Type     = 'RI'
     Spectrum_1c%dWn      = Spectral_L1c%dWn
     Spectrum_1c%Ns_First = idnint(Spectral_L1c%WnS(SB)/Spectrum_1c%dWn) + 1
     Spectrum_1c%Ns_Last  = idnint(Spectral_L1c%WnE(SB)/Spectrum_1c%dWn) + 1
     Spectrum_1c%N_Sample = Spectrum_1c%Ns_Last-Spectrum_1c%Ns_First+1
     Spectrum_1c%Wn_First = Spectrum_1c%dWn*(Spectrum_1c%Ns_First-1)
     Spectrum_1c%Wn_Last  = Spectrum_1c%dWn*(Spectrum_1c%Ns_Last-1)
     Spectrum_1c%WnMax    = Spectrum_1c%Wn_Last
     write(*,*) 'Spectrum_1c%N_Sample',Spectrum_1c%N_Sample
     write(*,*) 'Spectrum_1c%Ns_First',Spectrum_1c%Ns_First
     write(*,*) 'Spectrum_1c%Ns_Last ',Spectrum_1c%Ns_Last
     write(*,*) 'Spectrum_1c%Wn_First',Spectrum_1c%Wn_First
     write(*,*) 'Spectrum_1c%Wn_Last ',Spectrum_1c%Wn_Last
     write(*,*) 'Spectrum_1c%dWn     ',Spectrum_1c%dWn
     call alloc_Spectrum( Spectrum_1c )
     call Spectrum_Basis( Spectrum_1c )
!
!    interferogram initialisation
     Interf%Type     = 'C'
     Interf%N_Sample = Spectral_L1c%NsFft+1
     Interf%dOpd     = 1.d+00 / ( Spectrum_1a%dWn*dble(Spectral_L1c%NsFft) )
     Interf%OpdMax   = Interf%dOpd * dble(Spectral_L1c%NsFft/2)
     write(*,*) 'Interf%N_Sample',Interf%N_Sample
     write(*,*) 'Interf%dOpd    ',Interf%dOpd
     write(*,*) 'Interf%OpdMax  ',Interf%OpdMax
     call alloc_Interferogram( Interf )
     call Interferogram_Basis( Interf )
     Opd0 = Interf%Opd(int(Interf%N_Sample/2)+1) 
     Interf%Opd(1:Interf%N_Sample) = Interf%Opd(1:Interf%N_Sample) - Opd0
!
!    allocation
     allocate( Apf_Mod_temp(Apf%NsOpd), stat=ErrCode)
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Apf_Arg_temp(Apf%NsOpd), stat=ErrCode)
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Spatia_Mod(Interf%N_Sample), stat=ErrCode)
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Spatia_Arg(Interf%N_Sample), stat=ErrCode)
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Spatia_Cplx(Interf%N_Sample), stat=ErrCode)
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     if( ioalloc /= 0 ) then
        write(*,*) 'spectrum_apodspa_l1a_l1c Allocation Error',ioalloc
        call exit(1)
     end if
     Spectrum%Type     = 'C'
     Spectrum%N_Sample = Spectral_L1c%NsFft+1
     call alloc_Spectrum( Spectrum )
!
!    search for first apodisation function
     n1 = 1
     n2 = Apf%NsWn0
     call dichotomd( Spectrum_1c%Wn_First, Apf%Wn0, n1, n2 )
     NFstart = n1
!
!    search for last apodisation function
     n1 = 1
     n2 = Apf%NsWn0
     call dichotomd( Spectrum_1c%Wn_Last, Apf%Wn0, n1, n2 )
     NFend = n2
     write(*,*) 'NFstart         ',NFstart
     write(*,*) 'NFend          ',NFend
     write(*,*) 'Apf%Wn0(NFstart)',Apf%Wn0(NFstart)
     write(*,*) 'Apf%Wn0(NFend) ',Apf%Wn0(NFend)
!
!    Apodisation function coherence check
     if( Spectrum_1c%Wn_First < Apf%Wn0(NFstart) .or. &
         Apf%Wn0(NFend) < Spectrum_1c%Wn_Last ) then
        write(*,*) 'Insufficent Apf spectral coverage'
        call exit(1)
     end if
!
!    Effective Opd introduction
     n1 = 1
     n2 = Interf%N_Sample
     call dichotomd( Srf%Opd_effective, Interf%Opd(n1), n1, n2 )
     if( n1 == Interf%N_Sample ) then
        Ns_Erase = 0
     else
        Ns_Erase = Interf%N_Sample - n1
     end if
     write(*,*) 'Apodspa_l1a_l1c Ns_Erase',Ns_Erase
!
!    Spectral Sample loop
     do Ns = Spectrum_1c%Ns_First, Spectrum_1c%Ns_Last
        n = Ns - Spectrum_1c%Ns_First + 1
!
!       Interpolation between two Apf
        n1 = 1
        n2 = Apf%NsWn0
        call dichotomd(Spectrum_1c%Wn(n),Apf%Wn0(n1),n1,n2) 
        if( n1 == n2 ) then
           pds2 = 0.d+00
           pds1 = 1.d+00
           Apf_Mod_temp(1:Apf%NsOpd) = Apf%Mod(1:Apf%NsOpd,n1,PN)
           Apf_Arg_temp(1:Apf%NsOpd) = Apf%Arg(1:Apf%NsOpd,n1,PN)
           Srf_WnShift_temp = Srf%WnShift1a(n1,PN)
        else
           pds2 = (Spectrum_1c%Wn(n)-Apf%Wn0(n1))/(Apf%Wn0(n2)-Apf%Wn0(n1))
           pds1 = 1.d+00-pds2
           Apf_Mod_temp(1:Apf%NsOpd) =             &
                 pds1 * Apf%Mod(1:Apf%NsOpd,n1,PN) &
               + pds2 * Apf%Mod(1:Apf%NsOpd,n2,PN)
           Apf_Arg_temp(1:Apf%NsOpd) =             &
                 pds1 * Apf%Arg(1:Apf%NsOpd,n1,PN) &
               + pds2 * Apf%Arg(1:Apf%NsOpd,n2,PN)
           Srf_WnShift_temp =                &
                 pds1 * Srf%WnShift1a(n1,PN) &
               + pds2 * Srf%WnShift1a(n2,PN)
        end if
        select case ( trim(Spectral_L1c%Mode) )
        case ( 'CALL1A' )
           write(*,*) 'Despatialisation'
        case default
           write(*,*) 'Despatialisation & apodisation'
           Apf_Mod_temp(1:Apf%NsOpd) = Apf_Mod_temp(1:Apf%NsOpd)&
                                     * Apf%L1cTF(1:Apf%NsOpd)
        end select
!
!       Spectral calibration
        Apf_Arg_temp(1:Apf%NsOpd) =                               &
                    Apf_Arg_temp(1:Apf%NsOpd)                     &
                   +( twoPi*Apf%Opd(1:Apf%NsOpd)*Srf_WnShift_temp )
!
!       Spectral window extraction
        NsFirst = Ns - int(Spectral_L1c%NsFft/2)
        NsLast  = Ns + int(Spectral_L1c%NsFft/2)
        WnS     = dble(NsFirst-1) * Spectrum_1a%dWn
        WnE     = dble(NsLast-1)  * Spectrum_1a%dWn
        call spectrum_extract( WnS, WnE, Spectrum_1a, Spectrum_SW )
        if( Spectrum_SW%N_Sample /= (NsLast-NsFirst+1) ) then
           write(*,*) 'SW Sampling Error', &
                       Spectrum_SW%N_Sample,NsLast-NsFirst+1
           call exit(1)
        end if
!
!       edge spectrum smoothing
        if( Spectral_L1c%SigS /= 0 ) then
           call windowing( Spectrum_SW%N_Sample, &
                           Spectrum_SW%Real_Part,&
                           Spectral_L1c%SigS,    &
                           Spectrum_1a%dWn       )
           write(*,*) 'End SW windowing'
        end if
!
!       Spectrum l1a Fourier transform
        Sens = +1
        call fft_r2c_open( Spectral_L1c%NsFft,   &
                           Sens,                 &
                           Spectrum_SW%Real_Part,&
                           Spectrum_SW%dWn,      &
                           Interf%dOpd,          &
                           Interf%Complex        )
        write(*,*) ' End fft_r2c'
!
!       Apodisation Function resampling
        Spatia_Mod(1:Interf%N_Sample) = 0.d+00
        Spatia_Arg(1:Interf%N_Sample) = 0.d+00
        call intspline( Apf%NsOpd,       &
                        Apf%Opd,         &
                        Apf_Mod_temp,    &
                        Interf%N_Sample, &
                        Interf%Opd,      &
                        Spatia_Mod       )
        call intspline( Apf%NsOpd,       &
                        Apf%Opd,         &
                        Apf_Arg_temp,    &
                        Interf%N_Sample, &
                        Interf%Opd,      &
                        Spatia_Arg       )
!
!       Normalisation
        Mod0 = Spatia_Mod(int(Interf%N_Sample/2)+1)
        Arg0 = Spatia_Arg(int(Interf%N_Sample/2)+1)
        Spatia_Mod(1:Interf%N_Sample) = Spatia_Mod(1:Interf%N_Sample)/Mod0
        Spatia_Arg(1:Interf%N_Sample) = -(Spatia_Arg(1:Interf%N_Sample)-Arg0)
        call modargtocplx( Spatia_Cplx,    &
                           Spatia_Mod,     &
                           Spatia_Arg,     &
                           Interf%N_Sample )
        if( Ns_Erase /= 0 ) then
           Spatia_Cplx(1:Ns_Erase) = dcmplx(0.d+00,0.d+00)
           Spatia_Cplx(Interf%N_Sample-Ns_Erase+1:Interf%N_Sample) = &
                                                 dcmplx(0.d+00,0.d+00)
        end if
        write(*,*) ' End intspline Apodspa'
!
!       Apodisation Function application
        Interf%Complex(1:Interf%N_Sample) =                &
                    Interf%Complex(1:Interf%N_Sample)      &
                  * Spatia_Cplx(1:Interf%N_Sample)
!
!       temporary spectra initialisation
        Spectrum%dWn      = Spectrum_SW%dWn
        Spectrum%Ns_First = Spectrum_SW%Ns_First
        Spectrum%Ns_Last  = Spectrum_SW%Ns_Last
        Spectrum%Wn_First = Spectrum_SW%Wn_First
        Spectrum%Wn_Last  = Spectrum_SW%Wn_Last
        Spectrum%WnMax    = Spectrum_SW%WnMax
        write(*,*) 'Spectrum%Ns_First',Spectrum%Ns_First
        write(*,*) 'Spectrum%Ns_Last ',Spectrum%Ns_Last
        write(*,*) 'Spectrum%Wn_First',Spectrum%Wn_First
        write(*,*) 'Spectrum%Wn_Last ',Spectrum%Wn_Last
        write(*,*) 'Spectrum%dWn     ',Spectrum%dWn
        if( Spectrum%N_Sample /=                       &
           (Spectrum%Ns_Last-Spectrum%Ns_First+1) ) then
           write(*,*) 'spectrum_apodspa_l1a_l1c N_Sample Error'
           call exit(1)
        end if
        call Spectrum_Basis( Spectrum )
!
!       Inverse Fourier transform
        Sens = -1
        call fft_c2c_open( Spectral_L1c%NsFft,&
                           Sens,              &
                           Interf%Complex,    &
                           Interf%dOpd,       &
                           Spectrum%dWn,      &
                           Spectrum%Complex   )
        m = Ns - Spectrum%Ns_First + 1
        write(*,*) 'End fft_c2c Spectrum%Complex'
        Spectrum_1c%Real_Part(n) = dreal(Spectrum%Complex(m))
        Spectrum_1c%Imag_Part(n) = dimag(Spectrum%Complex(m))
        call dalloc_Spectrum( Spectrum_SW )
     end do ! End Spectrum Sample loop
     write(*,*) 'End Spectrum Sample loop'
!
!    deallocation
     call dalloc_Interferogram( Interf )
     deallocate( Apf_Mod_temp )
     deallocate( Apf_Arg_temp )
     deallocate( Spatia_Mod )
     deallocate( Spatia_Arg )
     call dalloc_Spectrum( Spectrum )
!
     return
   end subroutine spectrum_apodspa_l1a_l1c
!
!
   subroutine spectrum_apodspa_l1b_l1c( Spectral_L1c,&
                                        SB,PN,       &
                                        Srf,         &
                                        Apf,         &
                                        Spectrum_1b, &
                                        Spectrum_1c  )
     implicit none
     type(type_Spectral_L1c)   ,intent(in)                :: Spectral_L1c
     integer(kind=LONG)        ,intent(in)                :: SB
     integer(kind=LONG)        ,intent(in)                :: PN
     type(type_Srf)            ,intent(in)                :: Srf
     type(type_Apf)            ,intent(in)                :: Apf
     type(type_Spectrum)       ,intent(in)                :: Spectrum_1b
     type(type_Spectrum)       ,intent(out)               :: Spectrum_1c
     type(type_Spectrum)                                  :: Spectrum_SW
     type(type_Spectrum)                                  :: Spectrum
     type(type_Interferogram)                             :: Interf
     real(kind=DOUBLE)                                    :: WnS
     real(kind=DOUBLE)                                    :: WnE
     integer(kind=LONG)                                   :: Ns
     integer(kind=LONG)                                   :: NsFirst
     integer(kind=LONG)                                   :: NsLast
     integer(kind=LONG)                                   :: ErrCode
     integer(kind=LONG)                                   :: ioalloc
     real(kind=DOUBLE)         ,dimension(:) ,allocatable :: Apf_Mod_temp
     real(kind=DOUBLE)         ,dimension(:) ,allocatable :: Apf_Arg_temp
     real(kind=DOUBLE)         ,dimension(:) ,allocatable :: Spatia_Mod
     real(kind=DOUBLE)         ,dimension(:) ,allocatable :: Spatia_Arg
     complex(kind=DOUBLE)      ,dimension(:) ,allocatable :: Spatia_Cplx
     real(kind=DOUBLE)                                    :: pds1
     real(kind=DOUBLE)                                    :: pds2
     integer(kind=LONG)                                   :: m
     integer(kind=LONG)                                   :: n
     integer(kind=LONG)                                   :: n1
     integer(kind=LONG)                                   :: n2
     integer(kind=LONG)                                   :: NFstart
     integer(kind=LONG)                                   :: NFend
     integer(kind=LONG)                                   :: Sens
     real(kind=DOUBLE)                                    :: Opd0
     real(kind=DOUBLE)                                    :: Mod0
     real(kind=DOUBLE)                                    :: Arg0
     integer(kind=LONG)                                   :: Ns_Erase
!
     ioalloc = 0
!
!    spectrum level 1c initialisation
     Spectrum_1c%Type     = 'RI'
     Spectrum_1c%dWn      = Spectral_L1c%dWn
     Spectrum_1c%Ns_First = idnint(Spectral_L1c%WnS(SB)/Spectrum_1c%dWn) + 1
     Spectrum_1c%Ns_Last  = idnint(Spectral_L1c%WnE(SB)/Spectrum_1c%dWn) + 1
     Spectrum_1c%N_Sample = Spectrum_1c%Ns_Last-Spectrum_1c%Ns_First+1
     Spectrum_1c%Wn_First = Spectrum_1c%dWn*(Spectrum_1c%Ns_First-1)
     Spectrum_1c%Wn_Last  = Spectrum_1c%dWn*(Spectrum_1c%Ns_Last-1)
     Spectrum_1c%WnMax    = Spectrum_1c%Wn_Last
     write(*,*) 'Spectrum_1c%N_Sample',Spectrum_1c%N_Sample
     write(*,*) 'Spectrum_1c%Ns_First',Spectrum_1c%Ns_First
     write(*,*) 'Spectrum_1c%Ns_Last ',Spectrum_1c%Ns_Last
     write(*,*) 'Spectrum_1c%Wn_First',Spectrum_1c%Wn_First
     write(*,*) 'Spectrum_1c%Wn_Last ',Spectrum_1c%Wn_Last
     write(*,*) 'Spectrum_1c%dWn     ',Spectrum_1c%dWn
     call alloc_Spectrum( Spectrum_1c )
     call Spectrum_Basis( Spectrum_1c )
!
!    interferogram initialisation
     Interf%Type     = 'C'
     Interf%N_Sample = Spectral_L1c%NsFft+1
     Interf%dOpd     = 1.d+00 / ( Spectrum_1b%dWn*dble(Spectral_L1c%NsFft) )
     Interf%OpdMax   = Interf%dOpd * dble(Spectral_L1c%NsFft/2)
     write(*,*) 'Interf%N_Sample',Interf%N_Sample
     write(*,*) 'Interf%dOpd    ',Interf%dOpd
     write(*,*) 'Interf%OpdMax  ',Interf%OpdMax
     call alloc_Interferogram( Interf )
     call Interferogram_Basis( Interf )
     Opd0 = Interf%Opd(int(Interf%N_Sample/2)+1) 
     Interf%Opd(1:Interf%N_Sample) = Interf%Opd(1:Interf%N_Sample) - Opd0
!
!    allocation
     allocate( Apf_Mod_temp(Apf%NsOpd), stat=ErrCode)
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Apf_Arg_temp(Apf%NsOpd), stat=ErrCode)
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Spatia_Mod(Interf%N_Sample), stat=ErrCode)
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Spatia_Arg(Interf%N_Sample), stat=ErrCode)
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Spatia_Cplx(Interf%N_Sample), stat=ErrCode)
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     if( ioalloc /= 0 ) then
        write(*,*) 'spectrum_apodspa_l1b_l1c Allocation Error',ioalloc
        call exit(1)
     end if
     Spectrum%Type     = 'C'
     Spectrum%N_Sample = Spectral_L1c%NsFft+1
     call alloc_Spectrum( Spectrum )
!
!    search for first apodisation function
     n1 = 1
     n2 = Apf%NsWn0
     call dichotomd( Spectrum_1c%Wn_First, Apf%Wn0, n1, n2 )
     NFstart = n1
!
!    search for last apodisation function
     n1 = 1
     n2 = Apf%NsWn0
     call dichotomd( Spectrum_1c%Wn_Last, Apf%Wn0, n1, n2 )
     NFend = n2
     write(*,*) 'NFstart         ',NFstart
     write(*,*) 'NFend          ',NFend
     write(*,*) 'Apf%Wn0(NFstart)',Apf%Wn0(NFstart)
     write(*,*) 'Apf%Wn0(NFend) ',Apf%Wn0(NFend)
!
!    Apodisation function coherence check
     if( Spectrum_1c%Wn_First < Apf%Wn0(NFstart) .or. &
         Apf%Wn0(NFend) < Spectrum_1c%Wn_Last ) then
        write(*,*) 'Insufficent Apf spectral coverage'
        call exit(1)
     end if
!
!    Effective Opd introduction
     n1 = 1
     n2 = Interf%N_Sample
     call dichotomd( Srf%Opd_effective, Interf%Opd(n1), n1, n2 )
     if( n1 == Interf%N_Sample ) then
        Ns_Erase = 0
     else
        Ns_Erase = Interf%N_Sample - n1
     end if
     write(*,*) 'Apodspa_l1b_l1c Ns_Erase',Ns_Erase
!
!    Spectral Sample loop
     do Ns = Spectrum_1c%Ns_First, Spectrum_1c%Ns_Last
        n = Ns - Spectrum_1c%Ns_First + 1
!
!       Interpolation between two Apf
        n1 = 1
        n2 = Apf%NsWn0
        call dichotomd(Spectrum_1c%Wn(n),Apf%Wn0(n1),n1,n2) 
        if( n1 == n2 ) then
           pds2 = 0.d+00
           pds1 = 1.d+00
           Apf_Mod_temp(1:Apf%NsOpd) = Apf%Mod(1:Apf%NsOpd,n1,PN)
           Apf_Arg_temp(1:Apf%NsOpd) = Apf%Arg(1:Apf%NsOpd,n1,PN)
        else
           pds2 = (Spectrum_1c%Wn(n)-Apf%Wn0(n1))/(Apf%Wn0(n2)-Apf%Wn0(n1))
           pds1 = 1.d+00-pds2
           Apf_Mod_temp(1:Apf%NsOpd) =             &
                 pds1 * Apf%Mod(1:Apf%NsOpd,n1,PN) &
               + pds2 * Apf%Mod(1:Apf%NsOpd,n2,PN)
           Apf_Arg_temp(1:Apf%NsOpd) =             &
                 pds1 * Apf%Arg(1:Apf%NsOpd,n1,PN) &
               + pds2 * Apf%Arg(1:Apf%NsOpd,n2,PN)
        end if
        select case ( trim(Spectral_L1c%Mode) )
        case ( 'CALL1B' )
           write(*,*) 'Despatialisation'
        case default
           write(*,*) 'Despatialisation & apodisation'
           Apf_Mod_temp(1:Apf%NsOpd) = Apf_Mod_temp(1:Apf%NsOpd)&
                                     * Apf%L1cTF(1:Apf%NsOpd)
        end select
!
!       Spectral window extraction
        NsFirst = Ns - int(Spectral_L1c%NsFft/2)
        NsLast  = Ns + int(Spectral_L1c%NsFft/2)
        WnS     = dble(NsFirst-1) * Spectrum_1b%dWn
        WnE     = dble(NsLast-1)  * Spectrum_1b%dWn
        call spectrum_extract( WnS, WnE, Spectrum_1b, Spectrum_SW )
        if( Spectrum_SW%N_Sample /= (NsLast-NsFirst+1) ) then
           write(*,*) 'SW Sampling Error', &
                       Spectrum_SW%N_Sample,NsLast-NsFirst+1
           call exit(1)
        end if
!
!       edge spectrum smoothing
        if( Spectral_L1c%SigS /= 0 ) then
           call windowing( Spectrum_SW%N_Sample, &
                           Spectrum_SW%Real_Part,&
                           Spectral_L1c%SigS,    &
                           Spectrum_1b%dWn       )
           write(*,*) 'End SW windowing'
        end if
!
!       Spectrum l1b Fourier transform
        Sens = +1
        call fft_r2c_open( Spectral_L1c%NsFft,   &
                           Sens,                 &
                           Spectrum_SW%Real_Part,&
                           Spectrum_SW%dWn,      &
                           Interf%dOpd,          &
                           Interf%Complex        )
        write(*,*) ' End fft_r2c'
!
!       Apodisation Function resampling
        Spatia_Mod(1:Interf%N_Sample) = 0.d+00
        Spatia_Arg(1:Interf%N_Sample) = 0.d+00
        call intspline( Apf%NsOpd,       &
                        Apf%Opd,         &
                        Apf_Mod_temp,    &
                        Interf%N_Sample, &
                        Interf%Opd,      &
                        Spatia_Mod       )
        call intspline( Apf%NsOpd,       &
                        Apf%Opd,         &
                        Apf_Arg_temp,    &
                        Interf%N_Sample, &
                        Interf%Opd,      &
                        Spatia_Arg       )
!
!       Normalisation
        Mod0 = Spatia_Mod(int(Interf%N_Sample/2)+1)
        Arg0 = Spatia_Arg(int(Interf%N_Sample/2)+1)
        Spatia_Mod(1:Interf%N_Sample) = Spatia_Mod(1:Interf%N_Sample)/Mod0
        Spatia_Arg(1:Interf%N_Sample) = -(Spatia_Arg(1:Interf%N_Sample)-Arg0)
        call modargtocplx( Spatia_Cplx,    &
                           Spatia_Mod,     &
                           Spatia_Arg,     &
                           Interf%N_Sample )
        if( Ns_Erase /= 0 ) then
           Spatia_Cplx(1:Ns_Erase) = dcmplx(0.d+00,0.d+00)
           Spatia_Cplx(Interf%N_Sample-Ns_Erase+1:Interf%N_Sample) = &
                                                 dcmplx(0.d+00,0.d+00)
        end if
        write(*,*) ' End intspline Apodspa'
!
!       Apodisation Function application
        Interf%Complex(1:Interf%N_Sample) =          &
                    Interf%Complex(1:Interf%N_Sample)&
                  * Spatia_Cplx(1:Interf%N_Sample)
!
!       temporary spectra initialisation
        Spectrum%dWn      = Spectrum_SW%dWn
        Spectrum%Ns_First = Spectrum_SW%Ns_First
        Spectrum%Ns_Last  = Spectrum_SW%Ns_Last
        Spectrum%Wn_First = Spectrum_SW%Wn_First
        Spectrum%Wn_Last  = Spectrum_SW%Wn_Last
        Spectrum%WnMax    = Spectrum_SW%WnMax
        write(*,*) 'Spectrum%Ns_First',Spectrum%Ns_First
        write(*,*) 'Spectrum%Ns_Last ',Spectrum%Ns_Last
        write(*,*) 'Spectrum%Wn_First',Spectrum%Wn_First
        write(*,*) 'Spectrum%Wn_Last ',Spectrum%Wn_Last
        write(*,*) 'Spectrum%dWn     ',Spectrum%dWn
        if( Spectrum%N_Sample /=                       &
           (Spectrum%Ns_Last-Spectrum%Ns_First+1) ) then
           write(*,*) 'spectrum_apodspa_l1b_l1c N_Sample Error'
           call exit(1)
        end if
        call Spectrum_Basis( Spectrum )
!
!       Inverse Fourier transform
        Sens = -1
        call fft_c2c_open( Spectral_L1c%NsFft,&
                           Sens,              &
                           Interf%Complex,    &
                           Interf%dOpd,       &
                           Spectrum%dWn,      &
                           Spectrum%Complex   )
        m = Ns - Spectrum%Ns_First + 1
        write(*,*) 'End fft_c2c Spectrum%Complex'
        Spectrum_1c%Real_Part(n) = dreal(Spectrum%Complex(m))
        Spectrum_1c%Imag_Part(n) = dimag(Spectrum%Complex(m))
        call dalloc_Spectrum( Spectrum_SW )
     end do ! End Spectrum Sample loop
     write(*,*) 'End Spectrum Sample loop'
!
!    deallocation
     call dalloc_Interferogram( Interf )
     deallocate( Apf_Mod_temp )
     deallocate( Apf_Arg_temp )
     deallocate( Spatia_Mod )
     deallocate( Spatia_Arg )
     call dalloc_Spectrum( Spectrum )
!
     return
   end subroutine spectrum_apodspa_l1b_l1c
!
!
end module spectral_l1bl1c_module
