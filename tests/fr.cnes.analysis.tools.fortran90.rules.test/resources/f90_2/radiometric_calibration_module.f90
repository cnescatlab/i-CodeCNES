!!#  radiometric_calibration_module.f90 --
!!# 
!!#            Project: SPS_GENERIC
!!#            Authors: NOVELTIS/B.TOURNIER
!!#               Date: october 2009
!!#            Version: $Revision: 1.9 $
!!#  Last modification: $Date: 2011-11-02 14:57:50 $
!!#
!!#  Language:  F90
!!#  Standards: Noveltis
!!#
!!# --
!!#
!!
!>  radiometric calibration -- Module
!!
!! * Purpose
!!
!!   Module for complex radiometric calibration
!!
!! * Description
!!      
!!   This module aims to apply a radiometric calibration algorithm. 
!!   Indeed, after non-linearity correction the instrument is considered as linear in energy. 
!!   The radiometric calibration algorithm uses two black bodies measurements; their 
!!   temperatures, T_blackbody and T_coldspace, are precisely monitored, and provide the hot 
!!   reference spectrum and the cold reference spectrum. The radiometric calibration for a wave
!!   number is then performed in the complex space.	
!!
!! * Sub-routines and functions
!!
!!   - radio_cal_bilatere   : radiometric calibration algorithm applied to two-side
!!                            interferogram configuration
!!   - radio_cal_monolatere : radiometric calibration algorithm applied to one-side
!!                            interferogram configuration
!!   - radio_cal_decim  : radiometric calibration algorithm using aliasing compensation for 
!!                            the calibration coefficients computation
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.5.10
!!


module radiometric_calibration_module
   use precision_type
   use error_type
   use constantes_type
   use srf_type
   use decim_one_fir_type
   use interf_param_type
   use resampling_param_type
   use interferogram_type
   use spectrum_type
   use apf_type
   use spectral_l1c_type
   use radio_calibration_type
   use math_module
   use fft_module
   use plancklib_module
!
   implicit none
!
!
   public ::                            &
             radio_cal_bilatere,        &
             radio_cal_monomertz,       &
             radio_cal_decim,       &
             radio_cal_decim_1cs,       &
             radio_cal_init

!
   contains
!
!

!> radio_cal_bilatere -- Public
!!
!! * Purpose
!!
!!     Radiometric calibration algorithm applied to two-side interferogram configuration.
!!
!! * Description
!! 
!!     This subroutine aims to apply a radiometric calibration algorithm in two-side
!!     interferogram configuration. The calibration algorithm respects the following steps:
!!      - the first one consists in the interferogram coherence verification in term of 
!!        samples number, interferogram type and optical path difference
!!      - then, constants, scaling factors and spectral limits are initialized and all required
!!        tables are allocated;
!!      - The Planck functions are computed for the two targets hot blackbody and cold space;
!!      - In order to define the calibration coefficients, the Fourier transformation is then 
!!        applied to Cold Space, Hot BlackBody and Earth View, 
!!      - At last, before all tables deallocation, the calibrated coefficients are applied to 
!!        the Earth View spectrum in order to obtain the calibrated spectrum
!!
!! * Inputs
!!                  
!!     - Interf_Param : type_Interf_Param / type for declaration and allocation of 
!!                      interferogram
!!     - Interf_CS    : type_Interferogram / type for declaration and allocation of 
!!                      interferogram 
!!     - Interf_BB    : type_Interferogram / type for declaration and allocation of 
!!                      interferogram
!!     - Interf_EW    : type_Interferogram / type for declaration and allocation of 
!!                      interferogram
!!     - SB           : spectral band
!!
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - S_Cal : type_Spectrum / type for declaration and allocation of spectrum
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.5.10
!!

   subroutine radio_cal_bilatere( Interf_Param, &
                                  ReSamp_Param, &
                                  Interf_CS,    &
                                  Interf_BB,    &
                                  Interf_EW,    &
                                  SB,           &
                                  S_Cal         )
   implicit none
     type(type_Interf_Param) , intent(in)             :: Interf_Param
     type(type_Resampling_Param)                      :: ReSamp_Param
     type(type_Interferogram), intent(in)             :: Interf_CS
     type(type_Interferogram), intent(in)             :: Interf_BB
     type(type_Interferogram), intent(in)             :: Interf_EW
     integer(kind=LONG)      , intent(in)             :: SB
     type(type_Spectrum)     , intent(out)            :: S_Cal
     integer(kind=LONG)                               :: Sens
     integer(kind=LONG)                               :: NsFft
     integer(kind=LONG)                               :: NsFftp
     integer(kind=LONG)                               :: Ns
     integer(kind=LONG)                               :: NsDel
     integer(kind=LONG)                               :: n
     real(kind=DOUBLE)    , dimension(:), allocatable :: SMod
     real(kind=DOUBLE)    , dimension(:), allocatable :: SArg
     real(kind=DOUBLE)    , dimension(:), allocatable :: B_CS
     real(kind=DOUBLE)    , dimension(:), allocatable :: B_BB
     complex(kind=DOUBLE) , dimension(:), allocatable :: Spc_CS
     complex(kind=DOUBLE) , dimension(:), allocatable :: Spc_BB
     complex(kind=DOUBLE) , dimension(:), allocatable :: Spc_EW
     complex(kind=DOUBLE) , dimension(:), allocatable :: Slope
     complex(kind=DOUBLE) , dimension(:), allocatable :: Offset
!
!    Coherence verification
     if( (Interf_CS%N_Sample /= Interf_BB%N_Sample) .or. &
         (Interf_CS%N_Sample /= Interf_EW%N_Sample) ) then
        write(*,*) 'Interferograms N_Sample Error'
        write(*,*) 'radio_cal_bilatere Fatal Error'
        call exit(1)
     end if
     if( (Interf_CS%Type /= Interf_BB%Type) .or. &
         (Interf_CS%Type /= Interf_EW%Type) ) then
        write(*,*) 'Interferograms Type Error'
        write(*,*) 'radio_cal_bilatere Fatal Error'
        call exit(1)
     end if
     if( (Interf_CS%dOpd /= Interf_BB%dOpd) .or. &
         (Interf_CS%dOpd /= Interf_EW%dOpd) ) then
        write(*,*) 'Interferograms dOpd Error'
        write(*,*) 'radio_cal_bilatere Fatal Error'
        call exit(1)
     end if
     if( (Interf_CS%OpdMax /= Interf_BB%OpdMax) .or. &
         (Interf_CS%OpdMax /= Interf_EW%OpdMax) ) then
        write(*,*) 'Interferograms OpdMax Error'
        write(*,*) 'radio_cal_bilatere Fatal Error'
        call exit(1)
     end if
!
!    Constant initialisation
     Sens   = -1
     NsFft  = ReSamp_Param%Ns_Fft
     if( Interf_CS%N_Sample-1 < NsFft ) then
        write(*,*) ' NsFft N_Sample ERROR'
        call exit(1)
     end if
     NsFftp = int(NsFft/2)
     NsDel = int( Interf_CS%N_Sample/2) - NsFftp
     write(*,*) 'radio_cal_bilatere : NsDel,NsFftp,NsFft',NsDel,NsFftp,NsFft
!
!    Scaling factors
     S_Cal%Type = 'C'
     S_Cal%dWn   = 1.d+00 / ( Interf_CS%dOpd * dble(NsFft) )
     S_Cal%WnMax = 1.d+00 / ( 2.d+00 * Interf_CS%dOpd )
!
!    Spectral limits and spectrum allocation
     S_Cal%Ns_First = idnint( Interf_Param%Wn_Usefull_First(SB) &
                            / S_Cal%dWn) + 1
     S_Cal%Ns_Last  = idnint( Interf_Param%Wn_Usefull_Last(SB)  &
                            / S_Cal%dWn) + 1
     S_Cal%N_Sample = S_Cal%Ns_Last - S_Cal%Ns_First + 1
     S_Cal%Wn_First = S_Cal%dWn * dble(S_Cal%Ns_First-1)
     S_Cal%Wn_Last  = S_Cal%dWn * dble(S_Cal%Ns_Last-1)
     write(*,*) 'S_Cal%N_Sample',S_Cal%N_Sample
     write(*,*) 'S_Cal%Ns_First',S_Cal%Ns_First
     write(*,*) 'S_Cal%Ns_Last ',S_Cal%Ns_Last
     write(*,*) 'S_Cal%dWn     ',S_Cal%dWn
     write(*,*) 'S_Cal%WnMax   ',S_Cal%WnMax
     call alloc_Spectrum( S_Cal )
     call Spectrum_Basis( S_Cal )
!
!    allocation
     allocate( B_CS(S_Cal%N_Sample) )
     allocate( B_BB(S_Cal%N_Sample) )
     allocate( Spc_CS(NsFft+1) )
     allocate( Spc_BB(NsFft+1) )
     allocate( Spc_EW(NsFft+1) )
     allocate( Slope(NsFft+1) )
     allocate( Offset(NsFft+1) )
     allocate( SMod(NsFft+1) )
     allocate( SArg(NsFft+1) )
!
!    Planck functions
     do Ns = S_Cal%Ns_First, S_Cal%Ns_Last
        n = Ns - S_Cal%Ns_First + 1
        if( Interf_Param%T_CS .ne. 0.d+00 ) then
           call plkdirect( Interf_Param%T_CS, S_Cal%Wn(n), B_CS(n) )
        else
           B_CS(n) = 0.d+00
        end if
        call plkdirect( Interf_Param%T_BB, S_Cal%Wn(n), B_BB(n) )
     end do
!
!    Cold Space fft
     call fft_r2c_open( NsFft, Sens, Interf_CS%Real_Part(NsDel+1), &
                   Interf_CS%dOpd, S_Cal%dWn, Spc_CS )
     call cplxtomodarg( Spc_CS, SMod, SArg, NsFft+1 )
!
!    Hot BlackBody fft
     call fft_r2c_open( NsFft, Sens, Interf_BB%Real_Part(NsDel+1), &
                   Interf_CS%dOpd, S_Cal%dWn, Spc_BB )
     call cplxtomodarg( Spc_BB, SMod, SArg, NsFft+1 )
!
!    Earth view fft
     call fft_r2c_open( NsFft, Sens, Interf_EW%Real_Part(NsDel+1), &
                   Interf_CS%dOpd, S_Cal%dWn, Spc_EW )
     call cplxtomodarg( Spc_EW, SMod, SArg, NsFft+1 )
!
!    Calibration coefficients
     Slope(1:S_Cal%N_Sample)  =                                   &
             ( B_BB(1:S_Cal%N_Sample) - B_CS(1:S_Cal%N_Sample) )  &
           / ( Spc_BB(NsFftp+S_Cal%Ns_First:NsFftp+S_Cal%Ns_Last) &
              -Spc_CS(NsFftp+S_Cal%Ns_First:NsFftp+S_Cal%Ns_Last) )
     Offset(1:S_Cal%N_Sample) =                                    &
         ( ( B_BB(1:S_Cal%N_Sample)                                &
           * Spc_CS(NsFftp+S_Cal%Ns_First:NsFftp+S_Cal%Ns_Last) )  &
          -( B_CS(1:S_Cal%N_Sample)                                &
           * Spc_BB(NsFftp+S_Cal%Ns_First:NsFftp+S_Cal%Ns_Last) ) )&
         / ( B_BB(1:S_Cal%N_Sample) - B_CS(1:S_Cal%N_Sample) )
     call cplxtomodarg( Slope,  SMod, SArg, S_Cal%N_Sample )
     call cplxtomodarg( Offset, SMod, SArg, S_Cal%N_Sample )
!
!    Earth view spectrum calibration
     S_Cal%Complex(1:S_Cal%N_Sample) = Slope(1:S_Cal%N_Sample) &
        * ( Spc_EW(NsFftp+S_Cal%Ns_First:NsFftp+S_Cal%Ns_Last) &
           -Offset(1:S_Cal%N_Sample) )
     call cplxtomodarg( S_Cal%Complex, SMod, SArg, S_Cal%N_Sample )
!
!    deallocation
     deallocate( SMod )
     deallocate( SArg )
     deallocate( Slope )
     deallocate( Offset )
     deallocate( Spc_CS )
     deallocate( Spc_BB )
     deallocate( Spc_EW )
     deallocate( B_BB )
     deallocate( B_CS )
     return
   end subroutine radio_cal_bilatere
!
!
   subroutine radio_cal_monomertz( Interf_Param, &
                                   Spectrum_CS,  &
                                   Spectrum_BB,  &
                                   Spectrum_EW,  &
                                   S_Cal         )
   implicit none
     type(type_Interf_Param) , intent(in)             :: Interf_Param
     type(type_Spectrum)     , intent(in)             :: Spectrum_CS
     type(type_Spectrum)     , intent(in)             :: Spectrum_BB
     type(type_Spectrum)     , intent(in)             :: Spectrum_EW
     type(type_Spectrum)     , intent(out)            :: S_Cal
     integer(kind=LONG)                               :: Ns
     real(kind=DOUBLE)    , dimension(:), allocatable :: SMod
     real(kind=DOUBLE)    , dimension(:), allocatable :: SArg
     real(kind=DOUBLE)    , dimension(:), allocatable :: B_CS
     real(kind=DOUBLE)    , dimension(:), allocatable :: B_BB
     complex(kind=DOUBLE) , dimension(:), allocatable :: Slope
     complex(kind=DOUBLE) , dimension(:), allocatable :: Offset
!
!    spectrum header transfert
     call Spectrum_Header_Transfer( Spectrum_EW, S_Cal )
!
!    allocation
     allocate( B_CS(S_Cal%N_Sample) )
     allocate( B_BB(S_Cal%N_Sample) )
     allocate( Slope(S_Cal%N_Sample) )
     allocate( Offset(S_Cal%N_Sample) )
     allocate( SMod(S_Cal%N_Sample) )
     allocate( SArg(S_Cal%N_Sample) )
!
!    Planck functions
     do Ns = 1, S_Cal%N_Sample
        if( Interf_Param%T_CS .ne. 0.d+00 ) then
           call plkdirect( Interf_Param%T_CS, S_Cal%Wn(Ns), B_CS(Ns) )
        else
           B_CS(Ns) = 0.d+00
        end if
        call plkdirect( Interf_Param%T_BB, S_Cal%Wn(Ns), B_BB(Ns) )
     end do
!
!    Calibration coefficients
     Slope(1:S_Cal%N_Sample) =                              &
             (B_BB(1:S_Cal%N_Sample)-B_CS(1:S_Cal%N_Sample))&
           / (Spectrum_BB%Complex(1:S_Cal%N_Sample)         &
             -Spectrum_CS%Complex(1:S_Cal%N_Sample))
     Offset(1:S_Cal%N_Sample) =                                              &
         ( (B_BB(1:S_Cal%N_Sample) * Spectrum_CS%Complex(1:S_Cal%N_Sample))  &
          -(B_CS(1:S_Cal%N_Sample) * Spectrum_BB%Complex(1:S_Cal%N_Sample)) )&
         / ( B_BB(1:S_Cal%N_Sample) - B_CS(1:S_Cal%N_Sample) )
     call cplxtomodarg( Slope,  SMod, SArg, S_Cal%N_Sample )
     call cplxtomodarg( Offset, SMod, SArg, S_Cal%N_Sample )
!
!    Earth view spectrum calibration
     S_Cal%Complex(1:S_Cal%N_Sample) = Slope(1:S_Cal%N_Sample)&
                     * ( Spectrum_EW%Complex(1:S_Cal%N_Sample)&
                        -Offset(1:S_Cal%N_Sample) )
     call cplxtomodarg( S_Cal%Complex, SMod, SArg, S_Cal%N_Sample )
!
!    deallocation
     deallocate( SMod )
     deallocate( SArg )
     deallocate( Slope )
     deallocate( Offset )
     deallocate( B_BB )
     deallocate( B_CS )
     return
   end subroutine radio_cal_monomertz
!
!

!> radio_cal_decim -- Public
!!
!! * Purpose
!!
!!     Radiometric calibration algorithm using aliasing compensation for the calibration 
!!     coefficients computation.
!!
!! * Description
!! 
!!     This subroutine aims to apply a radiometric calibration algorithm in two-side
!!     interferogram configuration. The calibration algorithm respects the same steps than the 
!!     radio_cal_bilatere subroutine but in addition it applies an aliasing compensation before 
!!     the calibration coefficients computation.
!!
!! * Inputs
!!                  
!!     - Interf_Param : type_Interf_Param / type for declaration and allocation of 
!!                      interferogram
!!     - Interf_CS    : type_Interferogram / type for declaration and allocation of 
!!                      interferogram 
!!     - Interf_BB    : type_Interferogram / type for declaration and allocation of 
!!                      interferogram
!!     - Interf_EW    : type_Interferogram / type for declaration and allocation of 
!!                      interferogram
!!     - SB           : spectral band
!!
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - S_Cal : type_Spectrum / type for declaration and allocation of spectrum
!!
!! * References
!!
!!     SPS ATBD
!!

   subroutine radio_cal_decim( Interf_Param, &
                               Decim_Param,  &
                               Interf_CS,    &
                               Interf_BB,    &
                               Interf_EW,    &
                               SB,           &
                               S_Cal         )
   implicit none
     type(type_Interf_Param) , intent(in)             :: Interf_Param
     type(type_Decim_One_fir), intent(in)             :: Decim_Param
     type(type_Interferogram), intent(inout)          :: Interf_CS
     type(type_Interferogram), intent(inout)          :: Interf_BB
     type(type_Interferogram), intent(inout)          :: Interf_EW
     integer(kind=LONG)      , intent(in)             :: SB
     type(type_Spectrum)     , intent(out)            :: S_Cal
     integer(kind=LONG)                               :: Sens
     integer(kind=LONG)                               :: NsFft
     integer(kind=LONG)                               :: NsFftp
     real(kind=DOUBLE)                                :: WnRef
     integer(kind=LONG)                               :: NsWnRef
     integer(kind=LONG)                               :: Nvdi
     integer(kind=LONG)                               :: NsDel
     real(kind=DOUBLE)                                :: dOpd
     real(kind=DOUBLE)                                :: dWn
     real(kind=DOUBLE)    , dimension(:), allocatable :: Wn
     real(kind=DOUBLE)    , dimension(:), allocatable :: SReal
     real(kind=DOUBLE)    , dimension(:), allocatable :: SImag
     real(kind=DOUBLE)    , dimension(:), allocatable :: B_CS
     real(kind=DOUBLE)    , dimension(:), allocatable :: B_BB
     complex(kind=DOUBLE) , dimension(:), allocatable :: Spc_CS
     complex(kind=DOUBLE) , dimension(:), allocatable :: Spc_BB
     complex(kind=DOUBLE) , dimension(:), allocatable :: Spc_EW
     complex(kind=DOUBLE) , dimension(:), allocatable :: S_CS
     complex(kind=DOUBLE) , dimension(:), allocatable :: S_BB
     complex(kind=DOUBLE) , dimension(:), allocatable :: S_EW
     complex(kind=DOUBLE) , dimension(:), allocatable :: S_EW_Cal
     complex(kind=DOUBLE) , dimension(:), allocatable :: Slope
     complex(kind=DOUBLE) , dimension(:), allocatable :: Offset
     integer(kind=LONG)                               :: Ns
     integer(kind=LONG)                               :: ErrCode
!
!    Coherence verification
     if( (Interf_CS%N_Sample /= Interf_BB%N_Sample) .or. &
         (Interf_CS%N_Sample /= Interf_EW%N_Sample) ) then
        write(*,*) 'Interferograms N_Sample Error'
        write(*,*) 'radio_cal_decim_1cs Fatal Error'
        call exit(1)
     end if
     if( (Interf_CS%Type /= Interf_BB%Type) .or. &
         (Interf_CS%Type /= Interf_EW%Type) ) then
        write(*,*) 'Interferograms Type Error'
        write(*,*) 'radio_cal_decim_1cs Fatal Error'
        call exit(1)
     end if
     if( (Interf_CS%dOpd /= Interf_BB%dOpd) .or. &
         (Interf_CS%dOpd /= Interf_EW%dOpd) ) then
        write(*,*) 'Interferograms dOpd Error'
        write(*,*) 'radio_cal_bilatere Fatal Error'
        call exit(1)
     end if
     if( (Interf_CS%OpdMax /= Interf_BB%OpdMax) .or. &
         (Interf_CS%OpdMax /= Interf_EW%OpdMax) ) then
        write(*,*) 'Interferograms OpdMax Error'
        write(*,*) 'radio_cal_bilatere Fatal Error'
        call exit(1)
     end if
!
!    Constant initialisation
     Sens   = -1
     NsFft  = Decim_Param%Ns_Fft
     NsFftp = int(NsFft/2)
     if( Interf_CS%N_Sample-1 < NsFft ) then
        write(*,*) ' NsFft N_Sample ERROR'
        call exit(1)
     end if
     NsDel = int( Interf_CS%N_Sample/2) - NsFftp
     WnRef  = Decim_Param%WnRef
     Nvdi   = Decim_Param%Nvdi
     write(*,*) 'radio_cal_decim_1cs : NsDel,NsFftp,NsFft',NsDel,NsFftp,NsFft
!
!    Scaling factors
     S_Cal%Type = 'C'
     S_Cal%dWn   = 1.d+00 / ( Interf_CS%dOpd * dble(NsFft) )
     S_Cal%WnMax = 1.d+00 / ( 2.d+00 * Interf_CS%dOpd )
!
!    Spectral limits and spectrum allocation
     S_Cal%Ns_First = idnint( Interf_Param%Wn_Usefull_First(SB) &
                             /S_Cal%dWn) + 1
     S_Cal%Ns_Last  = idnint( Interf_Param%Wn_Usefull_Last(SB)  &
                             /S_Cal%dWn) + 1
     S_Cal%N_Sample = S_Cal%Ns_Last - S_Cal%Ns_First + 1
     S_Cal%Wn_First = S_Cal%dWn * dble(S_Cal%Ns_First-1)
     S_Cal%Wn_Last  = S_Cal%dWn * dble(S_Cal%Ns_Last-1)
     write(*,*) 'S_Cal%N_Sample',S_Cal%N_Sample
     write(*,*) 'S_Cal%Ns_First',S_Cal%Ns_First
     write(*,*) 'S_Cal%Ns_Last ',S_Cal%Ns_Last
     write(*,*) 'S_Cal%dWn     ',S_Cal%dWn
     write(*,*) 'S_Cal%WnMax   ',S_Cal%WnMax
     call alloc_Spectrum( S_Cal )
     call Spectrum_Basis( S_Cal )
     dOpd   = Interf_CS%dOpd
     dWn    = S_Cal%dWn
     NsWnRef = idnint(WnRef/S_Cal%dWn) + 1
!
!    local wave numbers basis
     allocate( Wn(NsFft) )
     Wn(1:NsFft) = S_Cal%dWn *                                       &
        ( dble(NsWnRef) + (/ (dble(Ns-int(NsFft/2)-1),Ns=1,NsFft) /) )
     write(*,*) 'spectral band ',Wn(1),Wn(NsFft),S_Cal%dWn
!
!    Allocation
     allocate( B_CS(NsFft),     stat=ErrCode )
     allocate( B_BB(NsFft),     stat=ErrCode )
     allocate( Spc_CS(NsFft+1), stat=ErrCode )
     allocate( Spc_BB(NsFft+1), stat=ErrCode )
     allocate( Spc_EW(NsFft+1), stat=ErrCode )
     allocate( S_CS(NsFft+1),   stat=ErrCode )
     allocate( S_BB(NsFft+1),   stat=ErrCode )
     allocate( S_EW(NsFft+1),   stat=ErrCode )
     allocate( Slope(NsFft+1),  stat=ErrCode )
     allocate( Offset(NsFft+1), stat=ErrCode )
     allocate( SReal(NsFft+1),  stat=ErrCode )
     allocate( SImag(NsFft+1),  stat=ErrCode )
!
!    Planck function computation
     do Ns = 1, NsFft
        if( Interf_Param%T_CS /= 0.d+00 ) then
          call plkdirect( Interf_Param%T_CS, Wn(Ns), B_CS(Ns) )
        else
          B_CS(Ns) = 0.d+00
        end if
        call plkdirect( Interf_Param%T_BB, Wn(Ns), B_BB(Ns) )
     end do
!
!    Cold space
     call cplxtorealimag( Interf_CS%Complex,SReal,SImag,NsFft+1 )
     call fft_c2c_open( NsFft, Sens, Interf_CS%Complex(NsDel+1), &
                        dOpd, dWn, Spc_CS )
     call cplxtorealimag( Spc_CS,SReal,SImag,NsFft+1 )
!
!    Hot BlackBody
     call cplxtorealimag( Interf_BB%Complex,SReal,SImag,NsFft+1 )
     call fft_c2c_open( NsFft, Sens, Interf_BB%Complex(NsDel+1), &
                        dOpd, dWn, Spc_BB )
     call cplxtorealimag( Spc_BB,SReal,SImag,NsFft+1 )
!
!    earth view
     call cplxtorealimag( Interf_EW%Complex,SReal,SImag,NsFft+1 )
     call fft_c2c_open( NsFft, Sens, Interf_EW%Complex(NsDel+1), &
                        dOpd, dWn, Spc_EW )
     call cplxtorealimag( Spc_EW,SReal,SImag,NsFft+1 )
!
!    reverse spectra
     S_CS(1:NsFft)     = dconjg( Spc_CS(NsFft:1:-1) )
     S_BB(1:NsFft)     = dconjg( Spc_BB(NsFft:1:-1) )
     S_EW(1:NsFft)     = dconjg( Spc_EW(NsFft:1:-1) )
!
!    Calibration coefficients
     Slope(1:NsFft)  = (B_BB(1:NsFft) - B_CS(1:NsFft)) &
                     / (S_BB(1:NsFft) - S_CS(1:NsFft))
     Offset(1:NsFft) = ( (B_BB(1:NsFft) * S_CS(1:NsFft))  &
                       - (B_CS(1:NsFft) * S_BB(1:NsFft)) )&
                     / (B_BB(1:NsFft) - B_CS(1:NsFft))
     call cplxtorealimag( Slope,SReal,SImag,NsFft )
     call cplxtorealimag( Offset,SReal,SImag,NsFft )
!
!    EW spectrum calibration
     allocate( S_EW_Cal(NsFft) )
     S_EW_Cal(1:NsFft) = Slope(1:NsFft)  &
          * ( S_EW(1:NsFft) - Offset(1:NsFft) )
     call cplxtorealimag( S_Cal%Complex,SReal,SImag,NsFft )
!
!    usefull band extraction
     NsDel = S_Cal%Ns_First - (idnint(Wn(1)/S_Cal%dWn)+1)
     S_Cal%Complex(1:S_Cal%N_Sample) = S_EW_Cal(NsDel+1:NsDel+S_Cal%N_Sample)
     call cplxtorealimag( S_Cal%Complex,SReal,SImag,S_Cal%N_Sample)
!
     deallocate( Wn )
     deallocate( SReal )
     deallocate( SImag )
     deallocate( B_CS )
     deallocate( B_BB )
     deallocate( Slope )
     deallocate( Offset )
     deallocate( Spc_CS )
     deallocate( Spc_BB )
     deallocate( Spc_EW )
     deallocate( S_CS )
     deallocate( S_BB )
     deallocate( S_EW )
     deallocate( S_EW_Cal )
     return
   end subroutine radio_cal_decim
!
!

!> radio_cal_decim_1cs -- Public
!!
!! * Purpose
!!
!!     Radiometric calibration algorithm using aliasing compensation for the calibration 
!!     coefficients computation.
!!
!! * Description
!! 
!!     This subroutine aims to apply a radiometric calibration algorithm in two-side
!!     interferogram configuration. The calibration algorithm respects the same steps than the 
!!     radio_cal_bilatere subroutine but in addition it applies an aliasing compensation before 
!!     the calibration coefficients computation.
!!
!! * Inputs
!!                  
!!     - Interf_Param : type_Interf_Param / type for declaration and allocation of 
!!                      interferogram
!!     - Interf_CS    : type_Interferogram / type for declaration and allocation of 
!!                      interferogram 
!!     - Interf_BB    : type_Interferogram / type for declaration and allocation of 
!!                      interferogram
!!     - Interf_EW    : type_Interferogram / type for declaration and allocation of 
!!                      interferogram
!!     - SB           : spectral band
!!
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - S_Cal : type_Spectrum / type for declaration and allocation of spectrum
!!
!! * References
!!
!!     SPS ATBD
!!

   subroutine radio_cal_decim_1cs( Interf_Param, &
                                   Decim_Param,  &
                                   Spectral_L1c, &
                                   Apf,          &
                                   Interf_CS,    &
                                   Interf_BB,    &
                                   Interf_EW,    &
                                   SB,           &
                                   S_Cal         )
   implicit none
     type(type_Interf_Param) , intent(in)             :: Interf_Param
     type(type_Decim_One_fir), intent(in)             :: Decim_Param
     type(type_Spectral_L1c) , intent(in)             :: Spectral_L1c
     type(type_Apf)          , intent(in)             :: Apf
     type(type_Interferogram), intent(inout)          :: Interf_CS
     type(type_Interferogram), intent(inout)          :: Interf_BB
     type(type_Interferogram), intent(inout)          :: Interf_EW
     integer(kind=LONG)      , intent(in)             :: SB
     type(type_Spectrum)     , intent(out)            :: S_Cal
     integer(kind=LONG)                               :: Sens
     integer(kind=LONG)                               :: NsFft
     integer(kind=LONG)                               :: NsFftp
     real(kind=DOUBLE)                                :: Opd0
     real(kind=DOUBLE)                                :: WnRef
     integer(kind=LONG)                               :: NsWnRef
     integer(kind=LONG)                               :: Nvdi
     integer(kind=LONG)                               :: NsDel
     real(kind=DOUBLE)                                :: dOpd
     real(kind=DOUBLE)                                :: dWn
     real(kind=DOUBLE)    , dimension(:), allocatable :: Wn
     real(kind=DOUBLE)    , dimension(:), allocatable :: Opd
     real(kind=DOUBLE)    , dimension(:), allocatable :: L1cTF
     real(kind=DOUBLE)    , dimension(:), allocatable :: SReal
     real(kind=DOUBLE)    , dimension(:), allocatable :: SImag
     real(kind=DOUBLE)    , dimension(:), allocatable :: B_CS
     real(kind=DOUBLE)    , dimension(:), allocatable :: B_BB
     complex(kind=DOUBLE) , dimension(:), allocatable :: Spc_CS
     complex(kind=DOUBLE) , dimension(:), allocatable :: Spc_BB
     complex(kind=DOUBLE) , dimension(:), allocatable :: Spc_EW
     complex(kind=DOUBLE) , dimension(:), allocatable :: S_CS
     complex(kind=DOUBLE) , dimension(:), allocatable :: S_BB
     complex(kind=DOUBLE) , dimension(:), allocatable :: S_EW
     complex(kind=DOUBLE) , dimension(:), allocatable :: S_EW_Cal
     complex(kind=DOUBLE) , dimension(:), allocatable :: Slope
     complex(kind=DOUBLE) , dimension(:), allocatable :: Offset
     integer(kind=LONG)                               :: Ns
     integer(kind=LONG)                               :: ErrCode
!
!    Coherence verification
     if( (Interf_CS%N_Sample /= Interf_BB%N_Sample) .or. &
         (Interf_CS%N_Sample /= Interf_EW%N_Sample) ) then
        write(*,*) 'Interferograms N_Sample Error'
        write(*,*) 'radio_cal_decim_1cs Fatal Error'
        call exit(1)
     end if
     if( (Interf_CS%Type /= Interf_BB%Type) .or. &
         (Interf_CS%Type /= Interf_EW%Type) ) then
        write(*,*) 'Interferograms Type Error'
        write(*,*) 'radio_cal_decim_1cs Fatal Error'
        call exit(1)
     end if
     if( (Interf_CS%dOpd /= Interf_BB%dOpd) .or. &
         (Interf_CS%dOpd /= Interf_EW%dOpd) ) then
        write(*,*) 'Interferograms dOpd Error'
        write(*,*) 'radio_cal_bilatere Fatal Error'
        call exit(1)
     end if
     if( (Interf_CS%OpdMax /= Interf_BB%OpdMax) .or. &
         (Interf_CS%OpdMax /= Interf_EW%OpdMax) ) then
        write(*,*) 'Interferograms OpdMax Error'
        write(*,*) 'radio_cal_bilatere Fatal Error'
        call exit(1)
     end if
!
!    Constant initialisation
     Sens   = -1
     NsFft  = Decim_Param%Ns_Fft
     NsFftp = int(NsFft/2)
     if( Interf_CS%N_Sample-1 < NsFft ) then
        write(*,*) ' NsFft N_Sample ERROR'
        call exit(1)
     end if
     NsDel = int( Interf_CS%N_Sample/2) - NsFftp
     WnRef  = Decim_Param%WnRef
     Nvdi   = Decim_Param%Nvdi
     write(*,*) 'radio_cal_decim_1cs : NsDel,NsFftp,NsFft',NsDel,NsFftp,NsFft
!
!    Scaling factors
     S_Cal%Type = 'C'
     S_Cal%dWn   = 1.d+00 / ( Interf_CS%dOpd * dble(NsFft) )
     S_Cal%WnMax = 1.d+00 / ( 2.d+00 * Interf_CS%dOpd )
!
!    Spectral limits and spectrum allocation
     S_Cal%Ns_First = idnint( Interf_Param%Wn_Usefull_First(SB) &
                             /S_Cal%dWn) + 1
     S_Cal%Ns_Last  = idnint( Interf_Param%Wn_Usefull_Last(SB)  &
                             /S_Cal%dWn) + 1
     S_Cal%N_Sample = S_Cal%Ns_Last - S_Cal%Ns_First + 1
     S_Cal%Wn_First = S_Cal%dWn * dble(S_Cal%Ns_First-1)
     S_Cal%Wn_Last  = S_Cal%dWn * dble(S_Cal%Ns_Last-1)
     write(*,*) 'S_Cal%N_Sample',S_Cal%N_Sample
     write(*,*) 'S_Cal%Ns_First',S_Cal%Ns_First
     write(*,*) 'S_Cal%Ns_Last ',S_Cal%Ns_Last
     write(*,*) 'S_Cal%dWn     ',S_Cal%dWn
     write(*,*) 'S_Cal%WnMax   ',S_Cal%WnMax
     call alloc_Spectrum( S_Cal )
     call Spectrum_Basis( S_Cal )
     dOpd   = Interf_CS%dOpd
     dWn    = S_Cal%dWn
     NsWnRef = idnint(WnRef/S_Cal%dWn) + 1
!
!    local wave numbers basis
     allocate( Wn(NsFft) )
     Wn(1:NsFft) = S_Cal%dWn *                                       &
        ( dble(NsWnRef) + (/ (dble(Ns-int(NsFft/2)-1),Ns=1,NsFft) /) )
     write(*,*) 'spectral band ',Wn(1),Wn(NsFft),S_Cal%dWn
!
!    Allocation
     allocate( B_CS(NsFft),     stat=ErrCode )
     allocate( B_BB(NsFft),     stat=ErrCode )
     allocate( Spc_CS(NsFft+1), stat=ErrCode )
     allocate( Spc_BB(NsFft+1), stat=ErrCode )
     allocate( Spc_EW(NsFft+1), stat=ErrCode )
     allocate( S_CS(NsFft+1),   stat=ErrCode )
     allocate( S_BB(NsFft+1),   stat=ErrCode )
     allocate( S_EW(NsFft+1),   stat=ErrCode )
     allocate( Slope(NsFft+1),  stat=ErrCode )
     allocate( Offset(NsFft+1), stat=ErrCode )
     allocate( SReal(NsFft+1),  stat=ErrCode )
     allocate( SImag(NsFft+1),  stat=ErrCode )
!
!    Planck function computation
     do Ns = 1, NsFft
        if( Interf_Param%T_CS /= 0.d+00 ) then
          call plkdirect( Interf_Param%T_CS, Wn(Ns), B_CS(Ns) )
        else
          B_CS(Ns) = 0.d+00
        end if
        call plkdirect( Interf_Param%T_BB, Wn(Ns), B_BB(Ns) )
     end do
!
!    Interferogram apodisation
     select case ( trim(Spectral_L1c%Mode) )
     case ( 'CALL1A', 'CALL1B', 'CALREF', 'CALOPT' )
        write(*,'(a,a)') 'Radio cal : apodisation ', trim(Spectral_L1c%Mode)
        allocate( L1cTF(NsFft+1),    stat=ErrCode )
        do Ns = 1, Apf%NsOpd, 10
           write(*,*) Apf%Opd(Ns), Apf%L1cTF(Ns)
        end do
        Opd0 = Interf_CS%Opd(Interf_CS%N_Sample/2+1)
        allocate( Opd(Interf_CS%N_Sample) )
        Opd(1:Interf_CS%N_Sample) = Interf_CS%Opd(1:Interf_CS%N_Sample)&
                                  - Opd0
        call intspline( Apf%NsOpd, Apf%Opd, Apf%L1cTF,&
                        NsFft+1, Opd(NsDel+1), L1cTF  )
        do Ns = 1, NsFft, 100
           write(*,*) 'L1cTF', Opd(NsDel+Ns), L1cTF(Ns)
        end do
        deallocate( Opd )
        Interf_CS%Complex(NsDel+1:NsDel+NsFft+1) = &
                 Interf_CS%Complex(NsDel+1:NsDel+NsFft+1) * L1cTF(1:NsFft+1)
        Interf_BB%Complex(NsDel+1:NsDel+NsFft+1) = &
                 Interf_BB%Complex(NsDel+1:NsDel+NsFft+1) * L1cTF(1:NsFft+1)
        Interf_EW%Complex(NsDel+1:NsDel+NsFft+1) = &
                 Interf_EW%Complex(NsDel+1:NsDel+NsFft+1) * L1cTF(1:NsFft+1)
        deallocate( L1cTF )
     case default
        write(*,'(a,a)') 'Radio cal : no apodisation ',trim(Spectral_L1c%Mode)
     end select
!
!    Cold space
     call cplxtorealimag( Interf_CS%Complex,SReal,SImag,NsFft+1 )
     call fft_c2c_open( NsFft, Sens, Interf_CS%Complex(NsDel+1), &
                        dOpd, dWn, Spc_CS )
     call cplxtorealimag( Spc_CS,SReal,SImag,NsFft+1 )
!
!    Hot BlackBody
     call cplxtorealimag( Interf_BB%Complex,SReal,SImag,NsFft+1 )
     call fft_c2c_open( NsFft, Sens, Interf_BB%Complex(NsDel+1), &
                        dOpd, dWn, Spc_BB )
     call cplxtorealimag( Spc_BB,SReal,SImag,NsFft+1 )
!
!    earth view
     call cplxtorealimag( Interf_EW%Complex,SReal,SImag,NsFft+1 )
     call fft_c2c_open( NsFft, Sens, Interf_EW%Complex(NsDel+1), &
                        dOpd, dWn, Spc_EW )
     call cplxtorealimag( Spc_EW,SReal,SImag,NsFft+1 )
!
!    reverse spectra
     S_CS(1:NsFft)     = dconjg( Spc_CS(NsFft:1:-1) )
     S_BB(1:NsFft)     = dconjg( Spc_BB(NsFft:1:-1) )
     S_EW(1:NsFft)     = dconjg( Spc_EW(NsFft:1:-1) )
!
!    Calibration coefficients
     Slope(1:NsFft)  = (B_BB(1:NsFft) - B_CS(1:NsFft)) &
                     / (S_BB(1:NsFft) - S_CS(1:NsFft))
     Offset(1:NsFft) = ( (B_BB(1:NsFft) * S_CS(1:NsFft))  &
                       - (B_CS(1:NsFft) * S_BB(1:NsFft)) )&
                     / (B_BB(1:NsFft) - B_CS(1:NsFft))
     call cplxtorealimag( Slope,SReal,SImag,NsFft )
     call cplxtorealimag( Offset,SReal,SImag,NsFft )
!
!    EW spectrum calibration
     allocate( S_EW_Cal(NsFft) )
     S_EW_Cal(1:NsFft) = Slope(1:NsFft)  &
          * ( S_EW(1:NsFft) - Offset(1:NsFft) )
     call cplxtorealimag( S_Cal%Complex,SReal,SImag,NsFft )
!
!    usefull band extraction
     NsDel = S_Cal%Ns_First - (idnint(Wn(1)/S_Cal%dWn)+1)
     S_Cal%Complex(1:S_Cal%N_Sample) = S_EW_Cal(NsDel+1:NsDel+S_Cal%N_Sample)
     call cplxtorealimag( S_Cal%Complex,SReal,SImag,S_Cal%N_Sample)
!
     deallocate( Wn )
     deallocate( SReal )
     deallocate( SImag )
     deallocate( B_CS )
     deallocate( B_BB )
     deallocate( Slope )
     deallocate( Offset )
     deallocate( Spc_CS )
     deallocate( Spc_BB )
     deallocate( Spc_EW )
     deallocate( S_CS )
     deallocate( S_BB )
     deallocate( S_EW )
     deallocate( S_EW_Cal )
     return
   end subroutine radio_cal_decim_1cs
!
!
!> radio_cal_init -- Public
!!
!! * Purpose
!!
!!     Initialisation of the radiometric calibration parameters    
!!
!! * Description
!!
!!     The parameters defining the type_Radio_Cal are initialized 
!!
!! * Inputs 
!!
!!     - file_radio_cal : file with parameters of radiometric calibration 
!!
!! * Outputs
!!
!!     - Radio_Cal : type_Radio_Calibration / type for declaration and allocation of radiometric!!                   calibration
!!
!! * References
!!   SPS ATBD
!!
   subroutine radio_cal_init( File_Param,&
                              Radio_Cal  )
     implicit none
     character(len=*)            ,intent( in)        :: File_Param
     type(type_Radio_Calibration),intent(out)        :: Radio_Cal
     !
     integer(kind=LONG)                              :: iFile
     integer(kind=LONG)                              :: iPos
     integer(kind=LONG)                              :: Ns
     !
     iFile = 10
     !
     iPos = 1
     write(*,'(a)') File_Param(1:len_trim(File_Param))
     open(unit=iFile, file=File_Param, status='old',err=999)
     Radio_Cal%filename = File_Param
     iPos = 2
     read(iFile,*,err=999) Radio_Cal%NbBBRrs
     iPos = 3
     read(iFile,*,err=999) Radio_Cal%NsWn0
     write(*,*) 'Radio_Cal%NbBBRrs :', Radio_Cal%NbBBRrs 
     write(*,*) 'Radio_Cal%NsWn0   :', Radio_Cal%NsWn0 
     call alloc_Radio_Calibration( Radio_Cal )
     read(iFile,*,err=999)        
     iPos = 4
     do Ns = 1, Radio_Cal%NbBBRrs
        iPos = 5 + Ns
        read(iFile,*,err=999) Radio_Cal%TRrs(Ns),  Radio_Cal%RRrs(Ns)
        write(*,*) 'Radio_Cal TRrs RRrs:', Ns, Radio_Cal%TRrs(Ns), &
             Radio_Cal%RRrs(Ns)
     end do
     iPos = 6 + Radio_Cal%NbBBRrs
     read(iFile,*,err=999) 
     do Ns = 1, Radio_Cal%NsWn0
        iPos = 7 + Radio_Cal%NbBBRrs + Ns
        read(iFile,*,err=999) Radio_Cal%Wn0(Ns), &
             Radio_Cal%BB_Emis(Ns)
        write(*,*) 'Radio_Cal BB_Emis:', &
             Radio_Cal%Wn0(Ns),          &
             Radio_Cal%BB_Emis(Ns)
     end do
     close(unit=iFile)
!
     return
 999 write(*,*) 'radio_cal_init Fatal Error', iPos
     call exit(1)
   end subroutine radio_cal_init
!
!
end module radiometric_calibration_module
