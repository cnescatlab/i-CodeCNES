!!#   spectral_shift_module.f90 --
!!# 
!!#            Project: SPS_GENERIC
!!#            Authors: NOVELTIS/B.TOURNIER
!!#               Date: march 2010
!!#            Version: $Revision: 1.10 $
!!#  Last modification: $Date: 2012-02-08 10:19:15 $
!!#
!!#  Language:  F90
!!#  Standards: Noveltis
!!#
!!# --
!!#
!!
!>  spectral shift -- Module
!!
!! * Purpose
!!
!!   Module for Spectral Shift computation
!!
!! * Description
!!      
!!   The objective of this module is to deliver spectra at a fixed absolute spectral grid. 
!!   Moreover it provides the spectral calibration functions to the level 1b processing and
!!   spectral response function to the users. Thus, it is necessary to correct the measured
!!   spectra for instrument, platform and earth motion effects relative to absolute spectral
!!   sample position. 
!!   The absolute spectral position is determined thanks to the spectral calibration functions
!!   which are derived from the actual knowledge of the relevant instrument, platform and
!!   target states. This knowledge is obtained by the comparison between measured spectral
!!   shifts and predicted spectral shifts. Usually, the fitting between measurements and 
!!   predictions, characterize the instrument state and the actual spectral response function.
!!   The spectral calibration method is mainly based on the retrieval of a spectral instrument
!!   model. The spectral calibration algorithm is based on cross-correlation methods between
!!   measured and simulated reference spectra. This cross-corerlation can be realised in Fourier
!!   transform space, in spectrum space or in spectrum derivative space. 
!!
!! * Sub-routines and functions
!!
!! -  shift_ft_init    : initialisation of the spectral shift Fourier transform parameters
!! -  shift_ft         : spectral calibration by spectral shift computation in Fourier space
!! -  shift_sp_init    : initialisation of the spectral shift SP parameters
!! -  shift_sp         : spectral calibration by spectral shift computation in spectrum space
!! -  shift_dr_init    : initialisation of the spectral shift DR parameters.
!! -  shift_dr         : spectral calibration by spectral shift computation in spectrum 
!!                       derivative space
!! -  deriv_spectrum   : derivative coefficients computation
!! -  shift_ana        : spectral calibration by spectral shift computation thanks to analytical
!!                       method
!! -  amplitude_adjust : amplitude adjustment in the spectrum derivative space
!! -  scale_spectrum   : spectrum scale computation
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.7.3
!!

module spectral_shift_module
   use precision_type
   use error_type
   use constantes_type
   use spectrum_type
   use shift_dr_param_type
   use shift_ft_param_type
   use shift_sp_param_type
   use spectral_calib_type
   use fft_module
   use math_module
   use spectrum_module
   use spectrum_oversamp_module
!
   implicit none
!
!
   public :: shift_ft_init,   &
             shift_ft,        &
             shift_sp_init,   &
             shift_sp,        &
             shift_dr_init,   &
             shift_dr,        &
             deriv_spectrum,  &
             deriv_spe_single,&
             shift_ana,       &
             amplitude_adjust,&
             scale_spectrum,  &
             rmbg_spectrum
!
   contains
!
!

!
!
!> shift_ft_init -- Public
!!
!! * Purpose
!!
!!     Initialisation of the spectral shift Fourier transform parameters.
!!
!! * Description
!! 
!!     This subroutine allows to initialise the spectral shift Fourier transform parameters. 
!!     It updates the type dedicated to the spectral shift Fourier transform parameters.
!!     The following steps are required:
!!     - First steps consist in the reading of the spectral shift Fourier transform parameters
!!       file, the definition of the periodic basis, the spacing of the spectroscopic lines at
!!       the window centre, the computation of reference wavenumber of the spectral window.
!!     - Then, computations are done on the transitions : computations of the average spacing
!!       between two spectroscopic transitions, definition of the minimum sample number between
!!       two transitions
!!     - Computations are done on sample numbers: sample number necessary to represent harmonics
!!       is computed, sample number between two transitions is adjusted, and sample number in 
!!       the spectral window is defined.
!!     - The Fourier transform size is determined, and required tables are allocated.
!!     - The spectral position of the periodic samples are determined and by dichotomy, initial 
!!       calibration coefficient are searched.
!!     - The low-resolution spectrum defined on the determined spectral window is oversampled 
!!       by zero padding
!!     - Then, before the Fourier transform of the spectrum defined on the spectral window, 
!!       this one is interpolated by spline onto the periodic basis and spectral window edges
!!       are smoothed
!!     - At last, harmonics modulus and argument are computed and tables are deallocated
!!
!! * Inputs
!!
!!     - shift_param_file : character / input file with definition of spectral shift Fourier 
!!                          transform parameters
!!     - Srf              : type_Srf / type for declaration and allocation of srf
!!     - Spectrum_Ref     : type_Spectrum / type for declaration and allocation of spectrum 
!!     - Reference_Level  : character / flag for identification of reference level 
!!
!! * Inputs/outputs
!!
!!     - shift_ft_param : type_shift_ft_param / type for declaration and allocation of spectral 
!!                        shift Fourier transform parameters
!!     - Spectral_Calib : type_Spectral_Calib / type for declaration and allocation of spectral
!!                        calibration.
!!
!! * Outputs
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.7.3
!!

   subroutine shift_ft_init( shift_param_file,&
                             Srf_Ref,         &
                             Srf,             &
                             Spectrum_Ref,    &
                             Reference_Level, &
                             shift_ft_param,  &
                             Spectral_Calib   )
     implicit none
     character(len=*)          ,intent(in)                :: shift_param_file
     type(type_Srf)            ,intent(in)                :: Srf_Ref
     type(type_Srf)            ,intent(in)                :: Srf
     type(type_Spectrum)       ,intent(in)                :: Spectrum_Ref
     character(len=*)          ,intent(in)                :: Reference_Level
     type(type_shift_ft_param) ,intent(inout)             :: shift_ft_param
     type(type_Spectral_Calib) ,intent(inout)             :: Spectral_Calib
     type(type_Spectrum)                                  :: Spectrum_os
     integer(kind=LONG)                                   :: Ns
     integer(kind=LONG)                                   :: NsDel
     real(kind=DOUBLE)                                    :: WnMean
     real(kind=DOUBLE)                                    :: DWnSRM
     integer(kind=LONG)                                   :: NsHar
     integer(kind=LONG)                                   :: ip
     integer(kind=LONG)                                   :: NsSSRp
     integer(kind=LONG)                                   :: Nc
     integer(kind=LONG)                                   :: Ni
     real(kind=DOUBLE)                                    :: dWn
     real(kind=DOUBLE)                                    :: ddWn
     real(kind=DOUBLE)         ,dimension(:) ,allocatable :: RadSW
     integer(kind=LONG)                                   :: iFile
     integer(kind=LONG)                                   :: iPos
     real(kind=DOUBLE)         ,dimension(:) ,allocatable :: WnSR
     real(kind=DOUBLE)         ,dimension(:) ,allocatable :: PressureShiftSR
     integer(kind=LONG)                                   :: ErrCode
     integer(kind=LONG)                                   :: Sens
     integer(kind=LONG)                                   :: Nh
     integer(kind=LONG)                                   :: NabsHar
     real(kind=DOUBLE)                                    :: DWnSW
     integer(kind=LONG)                                   :: NsFft
     integer(kind=LONG)                                   :: n1
     integer(kind=LONG)                                   :: n2
     real(kind=DOUBLE)                                    :: Pds
     real(kind=DOUBLE)                                    :: WnRef
!
!    reading the parameters file
     iFile = 10
     iPos  = 1
     open(unit=iFile,file=shift_param_file,err=999)
     iPos = iPos + 1
     read(iFile,*,err=999) shift_ft_param%NIteration
     read(iFile,*,err=999) shift_ft_param%NbHar
     read(iFile,*,err=999) shift_ft_param%NHar
     read(iFile,*,err=999) shift_ft_param%ModulCutoff
     read(iFile,*,err=999) shift_ft_param%ConverCutoff
     read(iFile,*,err=999) shift_ft_param%OSFactor
     read(iFile,*,err=999) shift_ft_param%Sampling
     read(iFile,*,err=999) shift_ft_param%SigS
     read(iFile,*,err=999) shift_ft_param%Signe
     read(iFile,*,err=999) shift_ft_param%Margin
     read(iFile,'(a)',err=999) shift_ft_param%Molecule
     read(iFile,*,err=999) shift_ft_param%PressureRef
     read(iFile,*,err=999) shift_ft_param%DWnSWAverage
     read(iFile,*,err=999) shift_ft_param%NbSR
     allocate( WnSR(shift_ft_param%NbSR), stat=ErrCode )
     allocate( PressureShiftSR(shift_ft_param%NbSR), stat=ErrCode )
     do Ns = 1, shift_ft_param%NbSR
        iPos = iPos + 1
        read(iFile,*,err=999) WnSR(Ns), PressureShiftSR(Ns)
        WnSR(Ns) = WnSR(Ns) + PressureShiftSR(Ns)&
                 *shift_ft_param%PressureRef/Cst_P_Ref
     end do
     close(unit=iFile)
     shift_ft_param%filename = shift_param_file
!
!    definition of the periodic basis
!
!    spacing of the spectroscopic lines at the window centre
     shift_ft_param%DWnSWref = &
           ( WnSR(int(shift_ft_param%NbSR/2)+2)        &
            -WnSR(int(shift_ft_param%NbSR/2)) ) / 2.d+00
     write(*,*) 'shift_ft_param%DWnSWref',shift_ft_param%DWnSWref
!
!    Reference Wavenumber of the spectral window
     WnMean = 0.d+00
     do Ns = 1, shift_ft_param%NbSR-1
        WnMean = WnMean &
               + ( (WnSR(Ns+1)+WnSR(Ns))/2.d+00         &
                  - WnSR(int(shift_ft_param%NbSR/2)+1) )&
               / ( WnSR(Ns+1)-WnSR(Ns) )
     end do
     shift_ft_param%WnSWref = &
              WnSR(int(shift_ft_param%NbSR/2)+1)     &
            + ( shift_ft_param%DWnSWref              &
              / dble(shift_ft_param%NbSR-1) * WnMean )
     write(*,*) 'shift_ft_param%WnSWref',shift_ft_param%WnSWref
!
!    average spacing between two spectroscopic transitions
     DWnSRM = 0.d+00
     do Ns = 1, shift_ft_param%NbSR - 1
       DWnSRM = DWnSRM + (WnSR(Ns+1)-WnSR(Ns)) 
     end do
     DWnSRM = DWnSRM / dble(shift_ft_param%NbSR-1)
     write(*,*) 'DWnSRM',DWnSRM
!
!    minimum sample number between two transitions
     shift_ft_param%NsSSR = shift_ft_param%Sampling       &
                          * idnint( DWnSRM / Spectrum_Ref%dWn )
     write(*,*) 'shift_ft_param%NsSSR',shift_ft_param%NsSSR
!
!    sample number necessary to represent harmonics
     NsHar = 2 * (shift_ft_param%NbHar+1) * (shift_ft_param%NbSR-1)
!
!    sample number between two transitions adjustment
     shift_ft_param%NsSSR = int( max( NsHar, shift_ft_param%NsSSR &
                                       * (shift_ft_param%NbSR-1)) &
                                    / (shift_ft_param%NbSR-1) ) + 1 
     write(*,*) 'shift_ft_param%NsSSR',shift_ft_param%NsSSR
     ip = 1
     do while( 2**ip+1 < shift_ft_param%NsSSR )
        ip = ip + 1
     end do
     shift_ft_param%NsSSR = 2**ip
     write(*,*) 'shift_ft_param%NsSSR',shift_ft_param%NsSSR
!
!    Number of samples in the spectral window
     shift_ft_param%NsSW = shift_ft_param%NsSSR * (shift_ft_param%NbSR-1) &
                         + 2*int(shift_ft_param%NsSSR/2)+1
     write(*,*) 'shift_ft_param%NsSW',shift_ft_param%NsSW
!
!    Fourier transform size
     ip = 1 
     do while ( 2**ip + 1 .lt. shift_ft_param%NsSW )
        ip = ip + 1
     end do
     shift_ft_param%NsFft = 2**ip
     write(*,*) 'shift_ft_param%NsFft',shift_ft_param%NsFft
!
!    allocation
     call alloc_shift_ft_param( shift_ft_param )
     shift_ft_param%WnSR(1:shift_ft_param%NbSR) = &
                    WnSR(1:shift_ft_param%NbSR)
     shift_ft_param%PressureShiftSR(1:shift_ft_param%NbSR) = &
                    PressureShiftSR(1:shift_ft_param%NbSR)
     do Ns = 1, shift_ft_param%NbSR
        write(*,*) 'shift_ft_param%WnSR',Ns,shift_ft_param%WnSR(Ns)
     end do
     allocate( RadSW(shift_ft_param%NsFft+1), stat=ErrCode)
     if( ErrCode > 0 ) then
        write(0,*) 'allocation shift_ft_param Error'
        write(0,*) 'shift_ft_init: fatal error'
        call exit(1)
      end if
!
!    Spectral position of the periodic samples
     NsSSRp = int(shift_ft_param%NsSSR/2)
     do Ns = 1, shift_ft_param%NbSR-1
        dWn = shift_ft_param%WnSR(Ns+1) - shift_ft_param%WnSR(Ns)
        ddWn = dWn / dble(shift_ft_param%NsSSR)
        if( Ns .eq. 1 ) then
           do Ni = 1, NsSSRp
              Nc = Ni
              shift_ft_param%WnSW(Nc) = shift_ft_param%WnSR(Ns)&
                                       +(dble(Ni-NsSSRp-1)*ddWn)
           end do
        end if
        do Ni = 1, shift_ft_param%NsSSR
           Nc=Ni+(Ns-1)*shift_ft_param%NsSSR+NsSSRp
           shift_ft_param%WnSW(Nc) = shift_ft_param%WnSR(Ns)&
                                   + (dble(Ni-1)*ddWn)
        end do
     end do
     do Ni = 1, NsSSRp+1
        Nc = Ni + (shift_ft_param%NbSR-1)*shift_ft_param%NsSSR+NsSSRp
        shift_ft_param%WnSW(Nc) = shift_ft_param%WnSR(shift_ft_param%NbSR)&
                                + (dble(Ni-1)*ddWn)
     end do
     shift_ft_param%WnS = shift_ft_param%WnSW(1)
     shift_ft_param%WnE = shift_ft_param%WnSW(shift_ft_param%NsSW)
!
!    search for initial calibration coefficient
     if( Reference_Level == 'L1b' ) then
        WnRef = (shift_ft_param%WnS+shift_ft_param%WnE) / 2.d+00
        n1 = 1
        n2 = Srf_Ref%NsWn0
        call dichotomd( WnRef, Srf_Ref%Wn0, n1, n2 )
        if( n1 == n2 ) then
           Spectral_Calib%CoefCal0 = Srf_Ref%Fcs1a(n1,Srf_Ref%NsPixel+1)
           Spectral_Calib%WnShift_Expected = Srf%WnShift1a(n1,Srf%NsPixel+1)
        else
           Pds = (WnRef - Srf_Ref%Wn0(n1)) / (Srf_Ref%Wn0(n2)-Srf_Ref%Wn0(n1))
           Spectral_Calib%CoefCal0 =                                   &
                         (1-Pds) * Srf_Ref%Fcs1a(n1,Srf_Ref%NsPixel+1) &
                         +  Pds  * Srf_Ref%Fcs1a(n2,Srf_Ref%NsPixel+1)
           Spectral_Calib%WnShift_Expected =                       &
                         (1-Pds) * Srf%WnShift1a(n1,Srf%NsPixel+1) &
                         +  Pds  * Srf%WnShift1a(n2,Srf%NsPixel+1)
        end if
     else
        WnRef = (shift_ft_param%WnS+shift_ft_param%WnE) / 2.d+00
        n1 = 1
        n2 = Srf_Ref%NsWn0
        call dichotomd( WnRef, Srf_Ref%Wn0, n1, n2 )
        if( n1 == n2 ) then
           Spectral_Calib%WnShift_Expected =                    &
                              Srf%WnShift1a(n1,Srf%NsPixel+1)   &
                            - Srf_Ref%WnShift1a(n1,Srf%NsPixel+1)
        else
           Pds = (WnRef - Srf_Ref%Wn0(n1)) / (Srf_Ref%Wn0(n2)-Srf_Ref%Wn0(n1))
           Spectral_Calib%WnShift_Expected =                              &
                         (1-Pds) * ( Srf%WnShift1a(n1,Srf%NsPixel+1)      &
                                   - Srf_Ref%WnShift1a(n1,Srf%NsPixel+1) )&
                         +  Pds  * ( Srf%WnShift1a(n2,Srf%NsPixel+1)      &
                                   - Srf_Ref%WnShift1a(n2,Srf%NsPixel+1)  )
        end if
        Spectral_Calib%CoefCal0 = 0.d+00 
     end if
!
!    spectral window oversampling
     NsFft = 0
     write(*,*) 'shift_ft_param%WnS',shift_ft_param%WnS
     write(*,*) 'shift_ft_param%WnE',shift_ft_param%WnE
     call spectrum_os_zero_padding( shift_ft_param%WnS,     &
                                    shift_ft_param%WnE,     &
                                    NsFft,                  &
                                    shift_ft_param%OSFactor,&
                                    Spectrum_Ref,           &
                                    Spectrum_os             )
!
!    spline interpolation of the spectrum onto the periodic basis
     call intspline( Spectrum_os%N_Sample, &
                     Spectrum_os%Wn,       &
                     Spectrum_os%Real_Part,&
                     shift_ft_param%NsSW,  &
                     shift_ft_param%WnSW,  &
                     shift_ft_param%RdSW   )
!
!    spectral window edge smoothing
     if( shift_ft_param%SigS /= 0 ) then
        DWnSW = ( shift_ft_param%WnSW(shift_ft_param%NsSW) &
                - shift_ft_param%WnSW(1) ) / dble(shift_ft_param%NsSW-1)
        call windowing( shift_ft_param%NsSW, &
                        shift_ft_param%RdSW, &
                        shift_ft_param%SigS, &
                        DWnSW                )
     end if
     do Ns = 1, shift_ft_param%NsSW
        write(*,*) 'shift_ft_param%RdSW',Ns,shift_ft_param%WnSW(Ns),&
                                            shift_ft_param%RdSW(Ns)
     end do
!
!    Fourier transform of the spectral window
     NsDel = int(shift_ft_param%NsFft/2) - int(shift_ft_param%NsSW/2)
     if( NsDel < 0 ) then
        write(*,*) 'NsDel Error',NsDel
        write(*,*) 'shift_ft_init Fatal Error'
        call exit(1)
     end if
     RadSW(1:shift_ft_param%NsFft+1) = 0.d+00
     RadSW(NsDel+1:NsDel+shift_ft_param%NsSW) = &
                   shift_ft_param%RdSW(1:shift_ft_param%NsSW)
     Sens = +1
     call fft_r2c_open( shift_ft_param%NsFft,&
                        Sens, RadSW,         &
                        1.d+00, 1.d+00,      &
                        shift_ft_param%FftSW )
     do Ns = 1, shift_ft_param%NsFft
        write(*,*) 'shift_ft_param%FftSW',Ns,shift_ft_param%FftSW(Ns)
     end do
!
!    Harmonics modulus and argument
     do Nh = 1, shift_ft_param%NbHar
        NabsHar = ( Nh ) &
                * ( shift_ft_param%NsFft / shift_ft_param%NsSSR ) &
                + int( shift_ft_param%NsFft/2 ) + 1
        shift_ft_param%ModulNat(Nh) = &
             dsqrt( dreal( shift_ft_param%FftSW(NabsHar) &
                         * dconjg(shift_ft_param%FftSW(NabsHar)) ) )
        shift_ft_param%PhaseNat(Nh) = &
                 datan2( dimag(shift_ft_param%FftSW(NabsHar)) &
                            * dble(shift_ft_param%Signe)      &
     &                  ,dreal(shift_ft_param%FftSW(NabsHar)) &
                            * dble(shift_ft_param%Signe) )
        write(*,*) 'PhaseNat ModulNat',Nh,NabsHar,&
                                          shift_ft_param%PhaseNat(Nh),&
                                          shift_ft_param%ModulNat(Nh)
     end do
     deallocate( RadSW )
     deallocate( WnSR )
     call dalloc_Spectrum( Spectrum_os )
     write(*,*) 'End shift_ft_init'
!
     return
 999 write(*,*) ' Reading Error',iPos
     write(*,*) ' shift_ft_init Fatal Error'
     call exit(1)
   end subroutine shift_ft_init
!
!

!
!
!> shift_ft -- Public
!!
!! * Purpose
!!
!!     Spectral calibration by spectral shift computation in Fourier space. 
!!
!! * Description
!! 
!!     The objective of this algorithm is to compute the spectral shift in the Fourier space.    
!!     The following steps are required:
!!     - First steps consist in the spectral window oversampling by zero padding.
!!     - The iteration is then initialised before the iteration loop. This one consists 
!!              * first in spectral basis calibration
!!              * then, the spectral window is interpolated on the calibrated basis and 
!!                is smoothed in order to provide a spectrum to the Fourier transform.
!!              * after the spectrum Fourier transform, harmonic abscissa modulus and phase are 
!!                determined, and spectral calibration coefficient is computed
!!     - Spectral shift, quality and convergence flags are computed. 
!!     - At last, before tables deallocation, calibration of the spectral window is done with
!!       first the spectral basis calibration and then the spectral window interpolation on the
!!       calibrated basis and the spectral window edge smoothing
!!
!! * Inputs
!!
!!     - Spectrum : type_Spectrum / type for declaration and allocation of spectrum 
!!
!! * Inputs/outputs
!!
!!     - shift_ft_param : type_shift_ft_param / type for declaration and allocation of spectral 
!!                        shift Fourier transform parameters
!!     - Spectral_Calib : type_Spectral_Calib / type for declaration and allocation of spectral
!!                        calibration
!!
!! * Outputs
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.7.3
!!

   subroutine shift_ft( shift_ft_param,&
                        Spectrum,      &
                        Spectral_Calib )
     implicit none
     type(type_shift_ft_param) ,intent(inout)             :: shift_ft_param
     type(type_Spectrum)       ,intent(in)                :: Spectrum
     type(type_Spectral_Calib) ,intent(inout)             :: Spectral_Calib
     type(type_Spectrum)                                  :: Spectrum_os
     logical                                              :: Iteration
     integer(kind=LONG)                                   :: NsFft
     integer(kind=LONG)                                   :: NsDel
     real(kind=DOUBLE)                                    :: Spectcalcoef
     real(kind=DOUBLE)                                    :: PhasePrevious
     real(kind=DOUBLE)                                    :: Phase
     real(kind=DOUBLE)                                    :: Modul
     integer(kind=LONG)                                   :: Nit
     real(kind=DOUBLE)         ,dimension(:) ,allocatable :: WnSS
     integer(kind=LONG)                                   :: ErrCode
     integer(kind=LONG)                                   :: ioalloc
     real(kind=DOUBLE)                                    :: DWnSW
     real(kind=DOUBLE)         ,dimension(:) ,allocatable :: RadSW
     integer(kind=LONG)                                   :: NabsHar
     integer(kind=LONG)                                   :: Sens
!
!    spectral window oversampling
     NsFft = 0
     write(*,*) 'shift_ft_param%WnS',shift_ft_param%WnS
     write(*,*) 'shift_ft_param%WnE',shift_ft_param%WnE
     call spectrum_os_zero_padding( shift_ft_param%WnS,     &
                                    shift_ft_param%WnE,     &
                                    NsFft,                  &
                                    shift_ft_param%OSFactor,&
                                    Spectrum,               &
                                    Spectrum_os             )
!
     NsDel = int(shift_ft_param%NsFft/2) - int(shift_ft_param%NsSW/2)
     if( NsDel < 0 ) then
        write(*,*) 'NsDel Error',NsDel
        write(*,*) 'shift_ft Fatal Error'
        call exit(1)
     end if
!
!    Iteration initialisation
     Spectcalcoef  = 1.d+00 + Spectral_Calib%CoefCal0
     PhasePrevious = 0.d+00
     Iteration = .true.
     Spectral_Calib%Last_Iteration = shift_ft_param%NIteration
     DWnSW = ( shift_ft_param%WnSW(shift_ft_param%NsSW) &
             - shift_ft_param%WnSW(1) ) / dble(shift_ft_param%NsSW-1)
     ioalloc = 0
     allocate( WnSS(Spectrum_os%N_Sample),    stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( RadSW(shift_ft_param%NsFft+1), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     if( ioalloc /= 0 ) then
        write(*,*) 'shift_ft allocation Error',ioalloc
        call exit(1)
     end if
!
!    Iteration loop
     do Nit = 1, shift_ft_param%NIteration
        if( Iteration ) then
!
!          spectral basis calibration
           WnSS(1:Spectrum_os%N_Sample) = Spectcalcoef &
                * Spectrum_os%Wn(1:Spectrum_os%N_Sample)
!
!          Spectral window interpolation on the calibrated basis
           call intspline( Spectrum_os%N_Sample, &
                           WnSS,                 &
                           Spectrum_os%Real_Part,&
                           shift_ft_param%NsSW,  &
                           shift_ft_param%WnSW,  &
                           shift_ft_param%RdSW   )

!
!          spectral window smoothing
           if( shift_ft_param%SigS /= 0 ) then
              DWnSW = ( shift_ft_param%WnSW(shift_ft_param%NsSW) &
                      - shift_ft_param%WnSW(1) ) / dble(shift_ft_param%NsSW-1)
              call windowing( shift_ft_param%NsSW, &
                              shift_ft_param%RdSW, &
                              shift_ft_param%SigS, &
                              DWnSW                )
           end if
!
!          Fourier transform of the spectral window
           NsDel = int(shift_ft_param%NsFft/2) - int(shift_ft_param%NsSW/2)
           if( NsDel < 0 ) then
              write(*,*) 'No coherence NsFft NsSW',NsDel
              write(*,*) 'shift_ft Fatal Error'
              call exit(1)
           end if
           RadSW(1:shift_ft_param%NsFft+1) = 0.d+00
           RadSW(NsDel+1:NsDel+shift_ft_param%NsSW) = &
                            shift_ft_param%RdSW(1:shift_ft_param%NsSW)
           Sens = +1
           call fft_r2c_open( shift_ft_param%NsFft,&
                              Sens, RadSW,         &
                              1.d+00, 1.d+00,      &
                              shift_ft_param%FftSW )
!
!          Harmonic abscissa modulus and phase
           NabsHar = (shift_ft_param%NHar+1)                       &
                   * (shift_ft_param%NsFft / shift_ft_param%NsSSR) &
                   + int(shift_ft_param%NsFft/2) + 1
           Modul   = dsqrt( dreal(shift_ft_param%FftSW(NabsHar)   &
                          * dconjg(shift_ft_param%FftSW(NabsHar))))
           Phase   = datan2( dimag(shift_ft_param%FftSW(NabsHar)) &
                             *dble(shift_ft_param%Signe)          &
                            ,dreal(shift_ft_param%FftSW(NabsHar)) &
                             *dble(shift_ft_param%Signe))
!
!          spectral calibration coefficient
           Spectcalcoef = Spectcalcoef                                      &
                        * ( 1.d+00 + (Phase                                 &
                           -shift_ft_param%PhaseNat(shift_ft_param%NHar+1)) &
                            / twoPi * shift_ft_param%DWnSWref               &
                            / shift_ft_param%WnSWref                        &
                            / (shift_ft_param%NHar + 1) )
           write(*,*) 'shift_ft',Nit,NabsHar,Phase,Spectcalcoef
           if( dabs(Phase-PhasePrevious) < shift_ft_param%ConverCutoff ) then
              Iteration = .false.
              Spectral_Calib%Last_Iteration = Nit
           end if
           PhasePrevious = Phase
        end if
     end do
!
!    Spectral shift
     Spectral_Calib%WnShift = shift_ft_param%WnSWref * (Spectcalcoef-1.d+00)
     Spectral_Calib%Wn      = shift_ft_param%WnSWref
     Spectral_Calib%CoefCal = Spectral_Calib%WnShift / Spectral_Calib%Wn
!
!    quality index
     Spectral_Calib%WnShiftQual = Modul &
                          / shift_ft_param%ModulNat(shift_ft_param%NHar+1)
     if( Spectral_Calib%WnShiftQual > 1.d+00 ) then
        Spectral_Calib%WnShiftQual = 1.d+00
     end if
!
!    quality flag
     if ( (Spectral_Calib%WnShiftQual <= shift_ft_param%ModulCutoff) &
                                      .or. (Iteration) ) then
        Spectral_Calib%FlagShiftQual = 0
        Spectral_Calib%WnShiftQual   = 0
     else
        Spectral_Calib%FlagShiftQual = 1
     end if
!
!    convergence flag
     if ( Spectral_Calib%Last_Iteration >= shift_ft_param%NIteration ) then
        Spectral_Calib%FlagConver  = 0
        Spectral_Calib%WnShiftQual = 0
     else
        Spectral_Calib%FlagConver = 1
     end if
     write(*,*) 'Spectral_Calib%Wn           ',Spectral_Calib%Wn
     write(*,*) 'Spectral_Calib%WnShift      ',Spectral_Calib%WnShift
     write(*,*) 'Spectral_Calib%CoefCal      ',Spectral_Calib%CoefCal
     write(*,*) 'Spectral_Calib%WnShiftQual  ',Spectral_Calib%WnShiftQual
     write(*,*) 'Spectral_Calib%FlagShiftQual',Spectral_Calib%FlagShiftQual
     write(*,*) 'Spectral_Calib%FlagConver   ',Spectral_Calib%FlagConver
!
!    calibration of the spectral window
!
!    spectral basis calibration
     WnSS(1:Spectrum_os%N_Sample) = Spectcalcoef &
          * Spectrum_os%Wn(1:Spectrum_os%N_Sample)
!
!    Spectral window interpolation on the calibrated basis
     call intspline( Spectrum_os%N_Sample, &
                     WnSS,                 &
                     Spectrum_os%Real_Part,&
                     shift_ft_param%NsSW,  &
                     shift_ft_param%WnSW,  &
                     shift_ft_param%RdSW   )
     
!
!    spectral window edge smoothing
     if( shift_ft_param%SigS /= 0 ) then
        DWnSW = ( shift_ft_param%WnSW(shift_ft_param%NsSW) &
                - shift_ft_param%WnSW(1) ) / dble(shift_ft_param%NsSW-1)
        call windowing( shift_ft_param%NsSW, &
                        shift_ft_param%RdSW, &
                        shift_ft_param%SigS, &
                        DWnSW                )
     end if
!
!    deallocation
     deallocate( RadSW )
     deallocate( WnSS )
     call dalloc_Spectrum( Spectrum_os )
     write(*,*) 'End shift_ft'
!
     return
   end subroutine shift_ft
!
!

!
!
!> shift_sp_init -- Public
!!
!! * Purpose
!!
!!     Initialisation of the spectral shift SP parameters .
!!
!! * Description
!! 
!!     This subroutine allows to initialise the spectral shift SP parameters. 
!!     It updates the type dedicated to the spectral shift SP parameters.
!!     The following steps are required:
!!     - First steps consist in, according to the init parameter value, the 
!!       reading of the spectral shift SP parameters file and by dichotomy, 
!!       initial calibration coefficient search.
!!     - Then, by zero padding, spectra are oversampled
!!
!! * Inputs
!!
!!     -  init             : initialisation flag
!!     -  shift_param_file : character / input file with definition of spectral shift Fourier 
!!                           transform parameters
!!     -  Srf              : type_Srf / type for declaration and allocation of srf
!!     -  Spectrum_Ref     : type_Spectrum / type for declaration and allocation of spectrum 
!!     -  Spectrum         : type_Spectrum / type for declaration and allocation of spectrum 
!!     -  Reference_Level  : character / flag for identification of reference level 
!!
!! * Inputs/outputs
!!
!!     -  Spectrum_os_Ref : type_Spectrum / type for declaration and allocation of spectrum 
!!     -  Spectrum_os     : type_Spectrum / type for declaration and allocation of spectrum 
!!     -  shift_sp_param  : type_shift_sp_param / type for declaration and allocation of spectral 
!!                          shift Fourier transform parameters
!!     -  Spectral_Calib  : type_Spectral_Calib / type for declaration and allocation of spectral
!!                          calibration.
!!
!! * Outputs
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.7.3
!!

   subroutine shift_sp_init( init,            &
                             shift_param_file,&
                             Srf_Ref,         &
                             Srf,             &
                             Spectrum_Ref,    &
                             Spectrum,        &
                             Reference_Level, &
                             Spectrum_os_Ref, &
                             Spectrum_os,     &
                             shift_sp_param,  &
                             Spectral_Calib   )
     implicit none
     integer(kind=LONG)        ,intent(in)                :: init
     character(len=*)          ,intent(in)                :: shift_param_file
     type(type_Srf)            ,intent(in)                :: Srf_Ref
     type(type_Srf)            ,intent(in)                :: Srf
     type(type_Spectrum)       ,intent(in)                :: Spectrum_Ref
     type(type_Spectrum)       ,intent(in)                :: Spectrum
     character(len=*)          ,intent(in)                :: Reference_Level
     type(type_Spectrum)       ,intent(inout)             :: Spectrum_os_Ref
     type(type_Spectrum)       ,intent(inout)             :: Spectrum_os
     type(type_shift_sp_param) ,intent(inout)             :: shift_sp_param
     type(type_Spectral_Calib) ,intent(inout)             :: Spectral_Calib
     type(type_Spectrum)                                  :: Spectrum_Ref_Flat
     type(type_Spectrum)                                  :: Spectrum_Flat
     integer(kind=LONG)                                   :: iFile
     integer(kind=LONG)                                   :: iPos
     integer(kind=LONG)                                   :: NsFft
     integer(kind=LONG)                                   :: n1
     integer(kind=LONG)                                   :: n2
     integer(kind=LONG)                                   :: NsDel
     real(kind=DOUBLE)                                    :: Pds
     real(kind=DOUBLE)                                    :: WnRef
!
!    reading the parameters file
     if( init == 0 ) then
        iFile = 10
        iPos  = 1
        write(*,*) 'shift_param_file',shift_param_file
        open(unit=iFile,file=shift_param_file,err=999)
        iPos = iPos + 1
        read(iFile,*,err=999) shift_sp_param%ndecinit
        iPos = iPos + 1
        read(iFile,*,err=999) shift_sp_param%dalphainit
        iPos = iPos + 1
        read(iFile,*,err=999) shift_sp_param%ndecratio
        iPos = iPos + 1
        read(iFile,*,err=999) shift_sp_param%CoefCal
        iPos = iPos + 1
        read(iFile,*,err=999) shift_sp_param%OSFactor
        iPos = iPos + 1
        read(iFile,*,err=999) shift_sp_param%SigS
        iPos = iPos + 1
        read(iFile,*,err=999) shift_sp_param%WnS
        iPos = iPos + 1
        read(iFile,*,err=999) shift_sp_param%WnE
        iPos = iPos + 1
        read(iFile,*,err=999) shift_sp_param%Margin
        iPos = iPos + 1
        read(iFile,*,err=999) shift_sp_param%Fft_Reduction
        iPos = iPos + 1
        close(unit=iFile)
        shift_sp_param%ndec   = shift_sp_param%ndecinit
        shift_sp_param%dalpha = shift_sp_param%dalphainit
!
!       search for initial calibration coefficient
        if( Reference_Level == 'L1b' ) then
           WnRef = (shift_sp_param%WnS+shift_sp_param%WnE) / 2.d+00
           n1 = 1
           n2 = Srf_Ref%NsWn0
           call dichotomd( WnRef, Srf_Ref%Wn0, n1, n2 )
           if( n1 == n2 ) then
              Spectral_Calib%CoefCal0 = Srf_Ref%Fcs1a(n1,Srf_Ref%NsPixel+1)
              Spectral_Calib%WnShift_Expected = Srf%WnShift1a(n1,Srf%NsPixel+1)
           else
              Pds = (WnRef - Srf_Ref%Wn0(n1)) &
                  / (Srf_Ref%Wn0(n2)-Srf_Ref%Wn0(n1))
              Spectral_Calib%CoefCal0 = &
                            (1-Pds) * Srf_Ref%Fcs1a(n1,Srf_Ref%NsPixel+1) &
                            +  Pds  * Srf_Ref%Fcs1a(n2,Srf_Ref%NsPixel+1)
              Spectral_Calib%WnShift_Expected =                       &
                            (1-Pds) * Srf%WnShift1a(n1,Srf%NsPixel+1) &
                            +  Pds  * Srf%WnShift1a(n2,Srf%NsPixel+1)
           end if
        else
           WnRef = (shift_sp_param%WnS+shift_sp_param%WnE) / 2.d+00
           n1 = 1
           n2 = Srf_Ref%NsWn0
           call dichotomd( WnRef, Srf_Ref%Wn0, n1, n2 )
           if( n1 == n2 ) then
              Spectral_Calib%WnShift_Expected =                    &
                                 Srf%WnShift1a(n1,Srf%NsPixel+1)   &
                               - Srf_Ref%WnShift1a(n1,Srf%NsPixel+1)
           else
              Pds = (WnRef - Srf_Ref%Wn0(n1))       &
                  / (Srf_Ref%Wn0(n2)-Srf_Ref%Wn0(n1))
              Spectral_Calib%WnShift_Expected =                           &
                         (1-Pds) * ( Srf%WnShift1a(n1,Srf%NsPixel+1)      &
                                   - Srf_Ref%WnShift1a(n1,Srf%NsPixel+1) )&
                         +  Pds  * ( Srf%WnShift1a(n2,Srf%NsPixel+1)      &
                                   - Srf_Ref%WnShift1a(n2,Srf%NsPixel+1) )
           end if
           Spectral_Calib%CoefCal0 = 0.d+00 
        end if
!
!       Remove spectrum background
        call Spectrum_Header_Transfer( Spectrum_Ref, Spectrum_Ref_Flat )
        call Spectrum_Header_Transfer( Spectrum, Spectrum_Flat )
        if( shift_sp_param%Fft_Reduction > 1 ) then
           call rmbg_spectrum( shift_sp_param, Spectrum_Ref, &
                               Spectrum_Ref_Flat )
           call rmbg_spectrum( shift_sp_param, Spectrum, Spectrum_Flat )
        else
           Spectrum_Ref_Flat = Spectrum_Ref
           Spectrum_Flat     = Spectrum
        end if
!
!       spectral window oversampling
        NsFft = 0
        call spectrum_os_zero_padding( shift_sp_param%WnS-     &
                                       shift_sp_param%Margin,  &
                                       shift_sp_param%WnE+     &
                                       shift_sp_param%Margin,  &
                                       NsFft,                  &
                                       shift_sp_param%OSFactor,&
                                       Spectrum_Ref_Flat,      &
                                       Spectrum_os_Ref         )
        write (*,*) 'spectrum_os_zero_padding Spectrum_Ref'
        call spectrum_os_zero_padding( shift_sp_param%WnS-     &
                                       shift_sp_param%Margin,  &
                                       shift_sp_param%WnE+     &
                                       shift_sp_param%Margin,  &
                                       NsFft,                  &
                                       shift_sp_param%OSFactor,&
                                       Spectrum_Flat,          &
                                       Spectrum_os             )
        write (*,*) 'spectrum_os_zero_padding Spectrum'
        if( shift_sp_param%SigS /= 0 ) then
           call windowing( Spectrum_os_Ref%N_Sample, &
                           Spectrum_os_Ref%Real_Part,&
                           shift_sp_param%SigS,      &
                           Spectrum_os_Ref%dWn       )
           write (*,*) 'windowing Spectrum_Ref'
           call windowing( Spectrum_os%N_Sample, &
                           Spectrum_os%Real_Part,&
                           shift_sp_param%SigS,  &
                           Spectrum_os%dWn       )
           write (*,*) 'windowing Spectrum'
        end if
        shift_sp_param%WnSWref = (Spectrum_os_Ref%Wn(1)&
                              +Spectrum_os_Ref%Wn(Spectrum_os_Ref%N_Sample)) &
                            / 2.d+00
        shift_sp_param%CoefCal = Spectral_Calib%CoefCal0
        shift_sp_param%nd0 = -idnint( ( 1.d+00 + shift_sp_param%CoefCal ) &
                                               / shift_sp_param%dalpha )  &
                           + int(shift_sp_param%ndec/2)
        NsDel = idnint(shift_sp_param%Margin/Spectrum_os_Ref%dWn)
        write(*,*) 'shift_sp_init NsDel',NsDel
        shift_sp_param%N_Sampler = Spectrum_os_Ref%N_Sample-(2*NsDel)
        shift_sp_param%N_Samples = Spectrum_os%N_Sample
        write(*,*) 'shift_sp_param%N_Sampler',shift_sp_param%N_Sampler
        write(*,*) 'shift_sp_param%N_Samples',shift_sp_param%N_Samples
        write(*,*) 'shift_sp_param%nd0      ',shift_sp_param%nd0
        call alloc_shift_sp_param( shift_sp_param )
        call dalloc_Spectrum ( Spectrum_Ref_Flat )
        call dalloc_Spectrum ( Spectrum_Flat )
     else if( init == 1 ) then
        shift_sp_param%ndec   = shift_sp_param%ndecinit   &
                              * shift_sp_param%ndecratio
        shift_sp_param%dalpha = shift_sp_param%dalphainit &
                              / shift_sp_param%ndecratio
        shift_sp_param%nd0    = -idnint( ( 1.d+00 + shift_sp_param%CoefCal ) &
                                                  / shift_sp_param%dalpha )  &
                              + int(shift_sp_param%ndec/2)
        write(*,*) 'shift_sp_param%N_Sampler',shift_sp_param%N_Sampler
        write(*,*) 'shift_sp_param%N_Samples',shift_sp_param%N_Samples
        write(*,*) 'shift_sp_param%nd0      ',shift_sp_param%nd0
     else
        write(*,*) ' shift_sp_init Error',init
        call exit(1)
     end if
     write(*,*) 'shift_sp_param%N_Sampler',shift_sp_param%N_Sampler
     write(*,*) 'shift_sp_param%N_Samples',shift_sp_param%N_Samples
     write(*,*) 'shift_sp_param%ndec     ',shift_sp_param%ndec
     write(*,*) 'shift_sp_param%nd0      ',shift_sp_param%nd0
     write(*,*) 'shift_sp_param%CoefCal  ',shift_sp_param%CoefCal
     write(*,*) 'shift_sp_param%WnSWref  ',shift_sp_param%WnSWref
     write(*,*) 'shift_sp_param%dalpha   ',shift_sp_param%dalpha
!
     write(*,*) 'End shift_sp_init'
     return
 999 write(*,*) ' Reading Error',iPos
     write(*,*) ' shift_sp_init Fatal Error'
     call exit(1)
   end subroutine shift_sp_init
!
!

!
!
!> shift_sp -- Public
!!
!! * Purpose
!!     
!!     Spectral calibration by spectral shift computation in spectrum space. 
!!     
!! * Description
!!     
!!     The objective of this algorithm is to compute the spectral shift in the spectrum space.    
!!     The following steps are required:
!!     - The first steps consists in both the reference and the shifted spectrum scaling.
!!     - After the correlation coefficient are initialised, they are computed by using 
!        a loop with the following steps:
!!          * computation of the spectral scaling factor
!!          * wave numbers scaling
!!          * spectrum resampling on the scaled basis
!!          * correlation coefficient computation
!!     - Then the position of the maximum corrrelation is researched.
!!     - At last, before tables deallocation, calibration of the spectral window is done with
!!       first the wave numbers scalings, then the spectrum resampling on the scaled basis, at
!!       last the uncalibrated spectrum resampling for viewing purpose
!!
!! * Inputs
!!
!!     - Spectrum_os_Ref : type_Spectrum / type for declaration and allocation of spectrum 
!!     - Spectrum_os     : type_Spectrum / type for declaration and allocation of spectrum 
!!
!! * Inputs/outputs
!!
!!     - shift_sp_param : type_shift_sp_param / type for declaration and allocation of spectral 
!!                        shift SP parameters
!!     - Spectral_Calib : type_Spectral_Calib / type for declaration and allocation of spectral
!!                        calibration
!!
!! * Outputs
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.7.3
!!

   subroutine shift_sp( shift_sp_param, &
                        Spectrum_os_Ref,&
                        Spectrum_os,    &
                        Spectral_Calib  )
     implicit none
     type(type_shift_sp_param) ,intent(inout)             :: shift_sp_param
     type(type_Spectrum)       ,intent(in)                :: Spectrum_os_Ref
     type(type_Spectrum)       ,intent(in)                :: Spectrum_os
     type(type_Spectral_Calib) ,intent(inout)             :: Spectral_Calib
     integer(kind=LONG)                                   :: nd
     integer(kind=LONG)                                   :: NsDel
!
     write(*,*) 'shift_sp *******************'
!
!    Reference spectrum scaling
     NsDel = idnint(shift_sp_param%Margin/Spectrum_os_Ref%dWn)
     if( shift_sp_param%N_Sampler /=               &
         (Spectrum_os_Ref%N_Sample-(2*NsDel)) ) then
        write(*,*) 'shift_sp_param%N_Sampler Error',shift_sp_param%N_Sampler
        write(*,*) 'shift_sp Fatal Error'
        call exit(1)
     end if
     if( shift_sp_param%N_Samples /= Spectrum_os%N_Sample ) then
        write(*,*) 'shift_sp_param%N_Samples Error',shift_sp_param%N_Samples
        write(*,*) 'shift_sp Fatal Error'
        call exit(1)
     end if
     write(*,*) 'shift_sp_param%N_Sampler',shift_sp_param%N_Sampler
     write(*,*) 'shift_sp_param%N_Samples',shift_sp_param%N_Samples
     write(*,*) 'Spectrum_os_Ref%N_Sample',Spectrum_os_Ref%N_Sample
     write(*,*) 'Spectrum_os%N_Sample    ',Spectrum_os%N_Sample
     write(*,*) 'NsDel                   ',NsDel
     write(*,*) 'scale_spectrum yr'
     shift_sp_param%xr(1:shift_sp_param%N_Sampler) =                &
           Spectrum_os_Ref%Wn(NsDel+1:NsDel+shift_sp_param%N_Sampler)
     shift_sp_param%yr(1:shift_sp_param%N_Sampler) =                &
           Spectrum_os_Ref%Real_Part(NsDel+1:NsDel+shift_sp_param%N_Sampler)
!     call scale_spectrum( shift_sp_param%N_Sampler,          &
!                          Spectrum_os_Ref%Real_Part(NsDel+1),&
!                          shift_sp_param%yr                  )
!
!    Shifted spectrum scaling
     write(*,*) 'scale_spectrum ys'
     shift_sp_param%xs(1:shift_sp_param%N_Samples) = &
                   Spectrum_os%Wn(1:shift_sp_param%N_Samples)
     shift_sp_param%ys(1:shift_sp_param%N_Samples) = &
                   Spectrum_os%Real_Part(1:shift_sp_param%N_Samples)
!     call scale_spectrum( shift_sp_param%N_Samples, &
!                          Spectrum_os%Real_Part,&
!                          shift_sp_param%ys     )
!
!    Correlation coefficient intialisation
     shift_sp_param%rc(1:shift_sp_param%ndec) = 0.d+00
!
!    Correlation step loop
     do nd = 1, shift_sp_param%ndec
!
!       Spectral scaling factor
        shift_sp_param%alpha(nd) = dble(nd-shift_sp_param%nd0) &
                                 * shift_sp_param%dalpha
!
!       Wane numbers scaling
        shift_sp_param%xn(1:shift_sp_param%N_Samples) = &
           shift_sp_param%xs(1:shift_sp_param%N_Samples)&
           * shift_sp_param%alpha(nd)
!
!       Spectrum resampling on the scaled basis
        call intspline( shift_sp_param%N_Samples,&
                        shift_sp_param%xn,       &
                        shift_sp_param%ys,       &
                        shift_sp_param%N_Sampler,&
                        shift_sp_param%xr,       &
                        shift_sp_param%yn        )
!
!       Correlation coefficient computation
        call correlation( shift_sp_param%N_Sampler,&
                          shift_sp_param%yr,       &
                          shift_sp_param%yn,       &
                          shift_sp_param%rc(nd)    )
        write(*,*) 'correlation yr yn',nd,shift_sp_param%alpha(nd),&
                                          shift_sp_param%rc(nd)
     end do
!    end Correlation step loop
!
!    search for position of the maximum corrrelation
     write(*,*) 'correlation max'
     call correlmax1D( shift_sp_param%ndec,         &
                       shift_sp_param%alpha,        &
                       shift_sp_param%rc,           &
                       Spectral_Calib%WnShiftQual,  &
                       shift_sp_param%xmax,         &
                       shift_sp_param%nd1,          &
                       shift_sp_param%nd2,          &
                       Spectral_Calib%FlagShiftQual )
     shift_sp_param%CoefCal = shift_sp_param%xmax -1.d+00
     Spectral_Calib%CoefCal = shift_sp_param%CoefCal
     Spectral_Calib%Wn      = shift_sp_param%WnSWref
     Spectral_Calib%WnShift = shift_sp_param%CoefCal &
                            * shift_sp_param%WnSWref
     Spectral_Calib%FlagConver = Spectral_Calib%FlagShiftQual
     write(*,*) 'shift_sp_param%nd0          ',shift_sp_param%nd0
     write(*,*) 'shift_sp_param%nd1          ',shift_sp_param%nd1
     write(*,*) 'shift_sp_param%nd2          ',shift_sp_param%nd2
     write(*,*) 'Spectral_Calib%Wn           ',Spectral_Calib%Wn
     write(*,*) 'Spectral_Calib%WnShift      ',Spectral_Calib%WnShift
     write(*,*) 'Spectral_Calib%CoefCal      ',Spectral_Calib%CoefCal
     write(*,*) 'Spectral_Calib%WnShiftQual  ',Spectral_Calib%WnShiftQual
     write(*,*) 'Spectral_Calib%FlagShiftQual',Spectral_Calib%FlagShiftQual
!
!   calibrated spectral window
!   Wave numbers scaling
    shift_sp_param%xn(1:shift_sp_param%N_Samples) =     &
           shift_sp_param%xs(1:shift_sp_param%N_Samples)&
           * ( 1.d+00 + shift_sp_param%CoefCal )
    
!
!    Spectrum resampling on the scaled basis
     call intspline( shift_sp_param%N_Samples,&
                     shift_sp_param%xn,       &
                     shift_sp_param%ys,       &
                     shift_sp_param%N_Sampler,&
                     shift_sp_param%xr,       &
                     shift_sp_param%yn        )
!
!    uncalibrated spectrum resampling for viewing purpose
     call intspline( shift_sp_param%N_Samples,&
                     shift_sp_param%xs,       &
                     Spectrum_os%Real_Part,   &
                     shift_sp_param%N_Sampler,&
                     shift_sp_param%xr,       &
                     shift_sp_param%ys        )
!!     do Ns = 1, shift_sp_param%N_Sampler
!!        write(*,*) 'ys',Ns,shift_sp_param%xr(Ns),shift_sp_param%ys(Ns)
!!     end do
!
     return
   end subroutine shift_sp
!
!

!
!
!> shift_dr_init -- Public
!!
!! * Purpose
!!
!!     Initialisation of the spectral shift DR parameters.
!!
!! * Description
!! 
!!     This subroutine allows to initialise the spectral shift DR parameters. 
!!     It updates the type dedicated to the spectral shift DR parameters, that 
!!     means in the spectrum derivative space.
!!     The following steps are required:
!!     - First steps consist in the reading of the spectral shift DR parameters 
!!       file and in the spectral window extraction.
!!     - Then, by dichotomy, initial calibration coefficient are searched, and 
!!       at last the derivative coefficients are initialised.
!!
!! * Inputs
!!
!!     -  shift_param_file : character / input file with definition of spectral shift Fourier 
!!                           transform parameters
!!     -  Srf              : type_Srf / type for declaration and allocation of srf
!!     -  Spectrum_Ref     : type_Spectrum / type for declaration and allocation of spectrum 
!!     -  Reference_Level  : character / flag for identification of reference level 
!!
!! * Inputs/outputs
!!
!!     -  shift_dr_param  : type_shift_sp_param / type for declaration and allocation of spectral 
!!                          shift Fourier transform parameters
!!     -  Spectral_Calib  : type_Spectral_Calib / type for declaration and allocation of spectral
!!                          calibration.
!!
!! * Outputs
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.7.3
!!

   subroutine shift_dr_init( shift_param_file,&
                             Srf_Ref,         &
                             Srf,             &
                             Spectrum_Ref,    &
                             Reference_Level, &
                             shift_dr_param,  &
                             Spectral_Calib   )
     implicit none
     character(len=*)          ,intent(in)                :: shift_param_file
     type(type_Srf)            ,intent(in)                :: Srf_Ref
     type(type_Srf)            ,intent(in)                :: Srf
     type(type_Spectrum)       ,intent(in)                :: Spectrum_Ref
     character(len=*)          ,intent(in)                :: Reference_Level
     type(type_shift_dr_param) ,intent(inout)             :: shift_dr_param
     type(type_Spectral_Calib) ,intent(inout)             :: Spectral_Calib
     integer(kind=LONG)                                   :: iFile
     integer(kind=LONG)                                   :: iPos
     integer(kind=LONG)                                   :: Init
     integer(kind=LONG)                                   :: n1
     integer(kind=LONG)                                   :: n2
     real(kind=DOUBLE)                                    :: Pds
     real(kind=DOUBLE)                                    :: WnRef
!
!    reading the parameters file
     iFile = 10
     iPos  = 1
     open(unit=iFile,file=shift_param_file,err=999)
     iPos = iPos + 1
     read(iFile,*,err=999) shift_dr_param%SigS
     iPos = iPos + 1
     read(iFile,*,err=999) shift_dr_param%WnS
     iPos = iPos + 1
     read(iFile,*,err=999) shift_dr_param%WnE
     iPos = iPos + 1
     close(unit=iFile)
     write(*,*) 'shift_dr_param%WnS   ',shift_dr_param%WnS
     write(*,*) 'shift_dr_param%WnE   ',shift_dr_param%WnE
     write(*,*) 'Spectrum_Ref%Wn_First',Spectrum_Ref%Wn_First
     write(*,*) 'Spectrum_Ref%Wn_Last ',Spectrum_Ref%Wn_Last
     write(*,*) 'Spectrum_Ref%dWn     ',Spectrum_Ref%dWn
!
!    Spectral window extraction
     shift_dr_param%m1 = idnint( shift_dr_param%WnS    &
                               / Spectrum_Ref%dWn ) + 1&
                       - Spectrum_Ref%Ns_First + 1
     shift_dr_param%m2 = idnint( shift_dr_param%WnE &
                               / Spectrum_Ref%dWn ) + 1&
                       - Spectrum_Ref%Ns_First + 1
     shift_dr_param%N_Sample = Spectrum_Ref%N_Sample
     write(*,*) 'shift_dr_param%m1      ',shift_dr_param%m1
     write(*,*) 'shift_dr_param%m2      ',shift_dr_param%m2
     write(*,*) 'shift_dr_param%N_Sample',shift_dr_param%N_Sample
     call alloc_shift_dr_param( shift_dr_param )
     shift_dr_param%x(1:shift_dr_param%N_Sample)   = 0.d+00
     shift_dr_param%y1(1:shift_dr_param%N_Sample)  = 0.d+00
     shift_dr_param%y2(1:shift_dr_param%N_Sample)  = 0.d+00
     shift_dr_param%y3(1:shift_dr_param%N_Sample)  = 0.d+00
     shift_dr_param%dy1(1:shift_dr_param%N_Sample) = 0.d+00
     shift_dr_param%dy2(1:shift_dr_param%N_Sample) = 0.d+00
     shift_dr_param%dy3(1:shift_dr_param%N_Sample) = 0.d+00
     shift_dr_param%dWn = Spectrum_Ref%dWn
     shift_dr_param%dx  = Spectrum_Ref%dWn
     shift_dr_param%x(shift_dr_param%m1:shift_dr_param%m2)  =      &
                Spectrum_Ref%Wn(shift_dr_param%m1:shift_dr_param%m2)
     shift_dr_param%WnSWref = ( shift_dr_param%WnS  &
                              + shift_dr_param%WnE )&
                              / 2.d+00
!
!    search for initial calibration coefficient
     if( Reference_Level == 'L1b' ) then
        WnRef = (shift_dr_param%WnS+shift_dr_param%WnE) / 2.d+00
        n1 = 1
        n2 = Srf_Ref%NsWn0
        call dichotomd( WnRef, Srf_Ref%Wn0, n1, n2 )
        if( n1 == n2 ) then
           Spectral_Calib%CoefCal0 = Srf_Ref%Fcs1a(n1,Srf_Ref%NsPixel+1)
           Spectral_Calib%WnShift_Expected = Srf%WnShift1a(n1,Srf%NsPixel+1)
        else
           Pds = (WnRef - Srf_Ref%Wn0(n1)) / (Srf_Ref%Wn0(n2)-Srf_Ref%Wn0(n1))
           Spectral_Calib%CoefCal0 = &
                           (1-Pds) * Srf_Ref%Fcs1a(n1,Srf_Ref%NsPixel+1) &
                           +  Pds  * Srf_Ref%Fcs1a(n2,Srf_Ref%NsPixel+1)
           Spectral_Calib%WnShift_Expected =                       &
                         (1-Pds) * Srf%WnShift1a(n1,Srf%NsPixel+1) &
                         +  Pds  * Srf%WnShift1a(n2,Srf%NsPixel+1)
        end if
     else
        WnRef = (shift_dr_param%WnS+shift_dr_param%WnE) / 2.d+00
        n1 = 1
        n2 = Srf_Ref%NsWn0
        call dichotomd( WnRef, Srf_Ref%Wn0, n1, n2 )
        if( n1 == n2 ) then
           Spectral_Calib%WnShift_Expected =                    &
                              Srf%WnShift1a(n1,Srf%NsPixel+1)   &
                            - Srf_Ref%WnShift1a(n1,Srf%NsPixel+1)
        else
           Pds = (WnRef - Srf_Ref%Wn0(n1)) / (Srf_Ref%Wn0(n2)-Srf_Ref%Wn0(n1))
           Spectral_Calib%WnShift_Expected =                              &
                         (1-Pds) * ( Srf%WnShift1a(n1,Srf%NsPixel+1)      &
                                   - Srf_Ref%WnShift1a(n1,Srf%NsPixel+1) )&
                         +  Pds  * ( Srf%WnShift1a(n2,Srf%NsPixel+1)      &
                                   - Srf_Ref%WnShift1a(n2,Srf%NsPixel+1) )
        end if
        Spectral_Calib%CoefCal0 = 0.d+00 
     end if
!
!    initialisation of the derivative coefficients
     Init = 0
     call deriv_spectrum( Init, shift_dr_param )
!
     return
 999 write(*,*) ' Reading Error',iPos
     write(*,*) ' shift_dr_init Fatal Error'
     call exit(1)
   end subroutine shift_dr_init
!
!

!
!
!> shift_dr -- Public
!!
!! * Purpose
!!     
!!     Spectral calibration by spectral shift computation in spectrum derivative space. 
!!     
!! * Description
!!     
!!     The objective of this algorithm is to compute the spectral shift in the spectrum 
!!     derivative space.    
!!     The following steps are required:
!!     - The first steps consists in the spectral window extraction of both the Spectrum 
!!       y1 and the Reference Spectrum y2. 
!!     - Then, derivatives (dy1 and dy2) are determined, and amplitude is adjusted for the
!!       two spectra (y3 spectrum is so defined).
!!     - Thanks to the analytic method, shift between (y1,y2) and (y1,y3) are computed, 
!!       and spectral calibration parameters are written in the dedicated type for 
!!       declaration and allocation of spectral shift DR parameters  
!!       last the uncalibrated spectrum resampling for viewing purpose
!!
!! * Inputs
!!
!!     - Spectrum_Ref : type_Spectrum / type for declaration and allocation of spectrum 
!!     - Spectrum     : type_Spectrum / type for declaration and allocation of spectrum 
!!
!! * Inputs/outputs
!!
!!     - shift_dr_param : type_shift_dr_param / type for declaration and allocation of spectral 
!!                        shift DR parameters
!!     - Spectral_Calib : type_Spectral_Calib / type for declaration and allocation of spectral
!!                        calibration
!!
!! * Outputs
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.7.3
!!

   subroutine shift_dr( shift_dr_param, &
                        Spectrum_Ref,   &
                        Spectrum,       &
                        Spectral_Calib  )
     implicit none
     type(type_shift_dr_param) ,intent(inout)             :: shift_dr_param
     type(type_Spectrum)       ,intent(in)                :: Spectrum_Ref
     type(type_Spectrum)       ,intent(in)                :: Spectrum
     type(type_Spectral_Calib) ,intent(inout)             :: Spectral_Calib
     integer(kind=LONG)                                   :: Init
     integer(kind=LONG)                                   :: N_Sample
     real(kind=DOUBLE) ,dimension(:) , allocatable        :: x_cal
!
!    Spectral window extraction Spectrum_Ref ==> y1
     shift_dr_param%y1(shift_dr_param%m1:shift_dr_param%m2) = &
                Spectrum_Ref%Real_Part(shift_dr_param%m1:shift_dr_param%m2)
!
!    Spectral window extraction Spectrum ==> y2
     shift_dr_param%y2(shift_dr_param%m1:shift_dr_param%m2) = &
                Spectrum%Real_Part(shift_dr_param%m1:shift_dr_param%m2)
     if( shift_dr_param%SigS /= 0 ) then
        N_Sample = shift_dr_param%m2 - shift_dr_param%m1 + 1
        call windowing( N_Sample,            &
                        shift_dr_param%y1    &
                        (shift_dr_param%m1), &
                        shift_dr_param%SigS, &
                        shift_dr_param%dx    )
        call windowing( N_Sample,            &
                        shift_dr_param%y2    &
                        (shift_dr_param%m1), &
                        shift_dr_param%SigS, &
                        shift_dr_param%dx    )
     end if
!
!    derivatives computation dy1 and dy2
     Init = 1
     call deriv_spectrum( Init, shift_dr_param )
!
!    y2 and dy2 amplitude adjustment ==> y3 dy3
     call amplitude_adjust( shift_dr_param )
!
!    shift computation between (y1,y2) and (y1,y3)
     call shift_ana( shift_dr_param )
     shift_dr_param%eps12 = -shift_dr_param%eps12
     write(*,*) 'shift_dr_param%eps12',shift_dr_param%eps12
     write(*,*) 'shift_dr_param%eps21',shift_dr_param%eps21
     shift_dr_param%eps13 = -shift_dr_param%eps13
     write(*,*) 'shift_dr_param%eps13',shift_dr_param%eps13
     write(*,*) 'shift_dr_param%eps31',shift_dr_param%eps31
!
!    results storage: calculate the average value of epsilon
     Spectral_Calib%Wn = shift_dr_param%WnSWref
     if( (shift_dr_param%FlagError1 /= 0) .and. &
         (shift_dr_param%FlagError2 /= 0) ) then
        Spectral_Calib%CoefCal = ( shift_dr_param%eps12 &
                                 + shift_dr_param%eps21 )/real(2,kind=DOUBLE)
!!                                 + shift_dr_param%eps13 &
!!                                 + shift_dr_param%eps31 )/real(4,kind=DOUBLE)
        Spectral_Calib%WnShift = Spectral_Calib%CoefCal * Spectral_Calib%Wn
        Spectral_Calib%WnShiftQual = real(1,kind=DOUBLE) - dsqrt(                 &
                  ( Spectral_Calib%CoefCal-shift_dr_param%eps12 )**2 &
               +  ( Spectral_Calib%CoefCal-shift_dr_param%eps21 )**2 &
               +  ( Spectral_Calib%CoefCal-shift_dr_param%eps13 )**2 &
               +  ( Spectral_Calib%CoefCal-shift_dr_param%eps31 )**2 )
        Spectral_Calib%FlagShiftQual = 1
        Spectral_Calib%FlagConver = Spectral_Calib%FlagShiftQual
     else
        Spectral_Calib%CoefCal       = real(0,kind=DOUBLE)
        Spectral_Calib%WnShift       = real(0,kind=DOUBLE)
        Spectral_Calib%WnShiftQual   = real(0,kind=DOUBLE)
        Spectral_Calib%FlagShiftQual = 0
        Spectral_Calib%FlagConver    = Spectral_Calib%FlagShiftQual
     end if
     write(*,*) 'Spectral_Calib%Wn',Spectral_Calib%Wn
     write(*,*) 'Spectral_Calib%WnShift',Spectral_Calib%WnShift
     write(*,*) 'Spectral_Calib%CoefCal',Spectral_Calib%CoefCal
     write(*,*) 'Spectral_Calib%WnShiftQual',Spectral_Calib%WnShiftQual
     write(*,*) 'Spectral_Calib%FlagShiftQual',Spectral_Calib%FlagShiftQual
!
!    y2 calibration ==> y3
     allocate( x_cal(shift_dr_param%m2-shift_dr_param%m1+1) )
     x_cal(1:shift_dr_param%m2-shift_dr_param%m1+1) =                   &
                  shift_dr_param%x(shift_dr_param%m1:shift_dr_param%m2) &
                * ( real(1,kind=DOUBLE) + Spectral_Calib%CoefCal )
     call intspline( shift_dr_param%m2-shift_dr_param%m1+1,&
                     x_cal,                                &
                     shift_dr_param%y2(shift_dr_param%m1), &
                     shift_dr_param%m2-shift_dr_param%m1+1,&
                     shift_dr_param%x(shift_dr_param%m1),  &
                     shift_dr_param%y3(shift_dr_param%m1)  )
     deallocate( x_cal )
!
     return
   end subroutine shift_dr
!
!

!
!
!> deriv_spectrum -- Public
!!
!! * Purpose
!!
!!     Derivative coefficients computation.
!!
!! * Description
!! 
!!     In order to match one of the spectra with respect to the other, this subroutine
!!     allows to derive the calibration coefficient between two spectra. 
!!     Coefficients are written in the type dedicated to the declaration and allocation 
!!     of spectral shift DR parameters. 
!!     Computation of the derivative dy of the spectrum y with step T at points Ns*T
!!     for Ns=m1,m1+1,....,m2-1,m2 at equal to 0 outside the interval [m1,m2].
!!     
!!     dy = 1/T sum over Ms  for Ms .ne. Ns of (-1)**(Ms-Ns)*y(Ms)/(Ns-Ms)
!!     The method splits the sum into 4 sums to avoid recalculating factor
!!     (-1)**(Ms-Ns) and is properly handling the sign of (Ns-Ms).
!!     The factor 1/T is not include in the resulting vector dy(Ns)
!!
!! * Inputs
!!
!!     -  Init : initialisation flag
!!
!! * Inputs/outputs
!!
!!     -  shift_dr_param : type_shift_dr_param / type for declaration and allocation of 
!!                         spectral shift DR parameters
!!
!! * Outputs
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.7.3
!!

   subroutine deriv_spectrum( Init, shift_dr_param )
     implicit none
     integer(kind=LONG)        ,intent(in)                :: Init
     type(type_shift_dr_param) ,intent(inout)             :: shift_dr_param
     integer(kind=LONG)                                   :: Ns
     integer(kind=LONG)                                   :: Ms
     real(kind=DOUBLE)                                    :: s1
     real(kind=DOUBLE)                                    :: s2
     real(kind=DOUBLE)                                    :: s3
     real(kind=DOUBLE)                                    :: s4
!
     if( Init == 0 ) then
        shift_dr_param%ginv(1:shift_dr_param%N_Sample) = 1.d+00 &
                  / (/ (dble(Ns),Ns=1,shift_dr_param%N_Sample) /)
     end if
     do Ns = shift_dr_param%m1, shift_dr_param%m2
        s1 = real(0,kind=DOUBLE)
        s2 = real(0,kind=DOUBLE)
        s3 = real(0,kind=DOUBLE)
        s4 = real(0,kind=DOUBLE)
        do Ms = Ns+1, shift_dr_param%m2, 2
           s1 = s1 + shift_dr_param%y1(Ms) * shift_dr_param%ginv(Ms-Ns)
        end do
        do Ms = Ns+2, shift_dr_param%m2, 2
           s2 = s2 + shift_dr_param%y1(Ms) * shift_dr_param%ginv(Ms-Ns)
        end do
        do Ms = Ns-1, shift_dr_param%m1, -2
           s3 = s3 + shift_dr_param%y1(Ms) * shift_dr_param%ginv(Ns-Ms)
        end do
        do Ms = Ns-2, shift_dr_param%m1, -2
           s4 = s4 + shift_dr_param%y1(Ms) * shift_dr_param%ginv(Ns-Ms)
        end do
        shift_dr_param%dy1(Ns) = s1 - s2 - s3 + s4
     end do
     do Ns = shift_dr_param%m1, shift_dr_param%m2
        s1 = real(0,kind=DOUBLE)
        s2 = real(0,kind=DOUBLE)
        s3 = real(0,kind=DOUBLE)
        s4 = real(0,kind=DOUBLE)        
        do Ms = Ns+1, shift_dr_param%m2, 2
           s1 = s1 + shift_dr_param%y2(Ms) * shift_dr_param%ginv(Ms-Ns)
        end do
        do Ms = Ns+2, shift_dr_param%m2, 2
           s2 = s2 + shift_dr_param%y2(Ms) * shift_dr_param%ginv(Ms-Ns)
        end do
        do Ms = Ns-1, shift_dr_param%m1, -2
           s3 = s3 + shift_dr_param%y2(Ms) * shift_dr_param%ginv(Ns-Ms)
        end do
        do Ms = Ns-2, shift_dr_param%m1, -2
           s4 = s4 + shift_dr_param%y2(Ms) * shift_dr_param%ginv(Ns-Ms)
        end do
        shift_dr_param%dy2(Ns) = s1 - s2 - s3 + s4
     end do
!
     return
   end subroutine deriv_spectrum
!
!

   subroutine deriv_spe_single( Init, shift_dr_param )
     implicit none
     integer(kind=LONG)        ,intent(in)                :: Init
     type(type_shift_dr_param) ,intent(inout)             :: shift_dr_param
     integer(kind=LONG)                                   :: Ns
     integer(kind=LONG)                                   :: Ms
     real(kind=DOUBLE)                                    :: s1
     real(kind=DOUBLE)                                    :: s2
     real(kind=DOUBLE)                                    :: s3
     real(kind=DOUBLE)                                    :: s4
!
     if( Init == 0 ) then
        shift_dr_param%ginv(1:shift_dr_param%N_Sample) = 1.d+00 &
                  / (/ (dble(Ns),Ns=1,shift_dr_param%N_Sample) /)
     end if
     do Ns = shift_dr_param%m1, shift_dr_param%m2
        s1 = real(0,kind=DOUBLE)
        s2 = real(0,kind=DOUBLE)
        s3 = real(0,kind=DOUBLE)
        s4 = real(0,kind=DOUBLE)
        do Ms = Ns+1, shift_dr_param%m2, 2
           s1 = s1 + shift_dr_param%y1(Ms) * shift_dr_param%ginv(Ms-Ns)
        end do
        do Ms = Ns+2, shift_dr_param%m2, 2
           s2 = s2 + shift_dr_param%y1(Ms) * shift_dr_param%ginv(Ms-Ns)
        end do
        do Ms = Ns-1, shift_dr_param%m1, -2
           s3 = s3 + shift_dr_param%y1(Ms) * shift_dr_param%ginv(Ns-Ms)
        end do
        do Ms = Ns-2, shift_dr_param%m1, -2
           s4 = s4 + shift_dr_param%y1(Ms) * shift_dr_param%ginv(Ns-Ms)
        end do
        shift_dr_param%dy1(Ns) = s1 - s2 - s3 + s4
     end do
!
     return
   end subroutine deriv_spe_single
!
!

!
!
!> shift_ana -- Public
!!
!! * Purpose
!!     
!!     Spectral calibration by spectral shift computation thanks to analytical method. 
!!     
!! * Description
!!     
!!     The objective of this algorithm is to compute the retrieving calibration coefficient 
!!     between two spectra (or spectral window) with an analytical method that means that can
!!     be fully described by a mathematical expression which is clearly described in the ATBD
!!     document.
!!
!! * Inputs
!!
!! * Inputs/outputs
!!
!!     - shift_dr_param : type_shift_dr_param / type for declaration and allocation of spectral 
!!                        shift DR parameters
!!
!! * Outputs
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.7.3
!!

   subroutine shift_ana( shift_dr_param )
     implicit none
     type(type_shift_dr_param) ,intent(inout)             :: shift_dr_param
     real(kind=DOUBLE)                                    :: y12
     real(kind=DOUBLE)                                    :: y13
!
     ! initialisation
     y12 = real(0, kind=DOUBLE)
     y13 = real(0, kind=DOUBLE)
     !
     y12 = sum( shift_dr_param%dy1(shift_dr_param%m1:shift_dr_param%m2)  &
              * shift_dr_param%dy2(shift_dr_param%m1:shift_dr_param%m2)  &
              * shift_dr_param%x(shift_dr_param%m1:shift_dr_param%m2)**2 )
     if( dabs(y12) > 0 ) then 
        shift_dr_param%FlagError1 = 1
     else
         shift_dr_param%FlagError1 = 0
     end if
     ! compute epsilon(y1,y2)
     if( shift_dr_param%FlagError1 == 1 ) then
        shift_dr_param%eps12 = &
                   sum( ( shift_dr_param%y1(shift_dr_param%m1  &
                                           :shift_dr_param%m2) &
                         -shift_dr_param%y2(shift_dr_param%m1  &
                                           :shift_dr_param%m2))&
                        * shift_dr_param%dy2(shift_dr_param%m1 &
                                            :shift_dr_param%m2)&
                        * shift_dr_param%x(shift_dr_param%m1   &
                                          :shift_dr_param%m2) )&
                   * shift_dr_param%dWn / y12
        shift_dr_param%eps21 = &
                   sum( ( shift_dr_param%y2(shift_dr_param%m1  &
                                           :shift_dr_param%m2) &
                         -shift_dr_param%y1(shift_dr_param%m1  &
                                           :shift_dr_param%m2))&
                        * shift_dr_param%dy1(shift_dr_param%m1 &
                                            :shift_dr_param%m2)&
                        * shift_dr_param%x(shift_dr_param%m1   &
                                          :shift_dr_param%m2) )&
                   * shift_dr_param%dWn / y12
     else
        shift_dr_param%eps12 = real(0, kind=DOUBLE)
        shift_dr_param%eps21 = real(0, kind=DOUBLE)
     end if
!
     y13 = sum( shift_dr_param%dy1(shift_dr_param%m1:shift_dr_param%m2)  &
              * shift_dr_param%dy3(shift_dr_param%m1:shift_dr_param%m2)  &
              * shift_dr_param%x(shift_dr_param%m1:shift_dr_param%m2)**2 )
     if( dabs(y13) > 0 ) then
        shift_dr_param%FlagError2 = 1
     else
        shift_dr_param%FlagError2 = 0
     end if
     if( shift_dr_param%FlagError2 == 1 ) then
        shift_dr_param%eps13 = &
                   sum( ( shift_dr_param%y1(shift_dr_param%m1  &
                                           :shift_dr_param%m2) &
                         -shift_dr_param%y3(shift_dr_param%m1  &
                                           :shift_dr_param%m2))&
                        * shift_dr_param%dy3(shift_dr_param%m1 &
                                            :shift_dr_param%m2)&
                        * shift_dr_param%x(shift_dr_param%m1   &
                                          :shift_dr_param%m2) )&
                   * shift_dr_param%dWn / y13
        shift_dr_param%eps31 = &
                   sum( ( shift_dr_param%y3(shift_dr_param%m1  &
                                           :shift_dr_param%m2) &
                         -shift_dr_param%y1(shift_dr_param%m1  &
                                           :shift_dr_param%m2))&
                        * shift_dr_param%dy1(shift_dr_param%m1 &
                                            :shift_dr_param%m2)&
                        * shift_dr_param%x(shift_dr_param%m1   &
                                          :shift_dr_param%m2) )&
                   * shift_dr_param%dWn / y13
     else
        shift_dr_param%eps13 = real(0, kind=DOUBLE)
        shift_dr_param%eps31 = real(0, kind=DOUBLE)
     end if
!
     return
   end subroutine shift_ana
!
!

!
!
!> amplitude_adjust -- Public
!!
!! * Purpose
!!
!!     Amplitude adjustment in the spectrum derivative space.
!!     Amplitude spectrum rescaling
!!
!! * Description
!! 
!!     In order to match one of the spectra with respect to the other, this subroutine adjusts
!!     the spectrum amplitude in the spectrum derivative space. 
!!     Determination of the coefficients aa and bb and their error fitting at best 
!!     in amplitude the modified spectrum y3 = y2_modified to the spectrum y1 by 
!!     minimising: sum over n of [ aa*y2(n) + bb - y1(n) ]**2
!!
!! * Inputs
!!
!! * Inputs/outputs
!!
!!     -  shift_dr_param : type_shift_dr_param / type for declaration and allocation of 
!!                         spectral shift DR parameters
!!
!! * Outputs
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.7.3
!!

   subroutine amplitude_adjust( shift_dr_param )
     implicit none
     type(type_shift_dr_param) ,intent(inout)             :: shift_dr_param
     real(kind=DOUBLE)                                    :: z1
     real(kind=DOUBLE)                                    :: z2
     real(kind=DOUBLE)                                    :: z12
     real(kind=DOUBLE)                                    :: z11
     real(kind=DOUBLE)                                    :: z22
     real(kind=DOUBLE)                                    :: delta
     real(kind=DOUBLE)                                    :: s1
     real(kind=DOUBLE)                                    :: s2
     real(kind=DOUBLE)                                    :: s3
!
     ! initialisation
     z1  = real(0, kind=DOUBLE)
     z2  = real(0, kind=DOUBLE)
     z12 = real(0, kind=DOUBLE)
     z11 = real(0, kind=DOUBLE)
     z22 = real(0, kind=DOUBLE)
     s1 = real(0, kind=DOUBLE)
     s2 = real(0, kind=DOUBLE)
     s3 = real(0, kind=DOUBLE)
     ! calculates averages 
     ! z1  = <y1>
     ! z2  = <y2>
     ! z12 = <y1*y2>
     ! z11 = <y1*y1>
     ! z22 = <y2*y2>
     z1  = sum( shift_dr_param%y1(shift_dr_param%m1:shift_dr_param%m2) )&
         / dble(shift_dr_param%m2-shift_dr_param%m1+1)
     z2  = sum( shift_dr_param%y2(shift_dr_param%m1:shift_dr_param%m2) )&
         / dble(shift_dr_param%m2-shift_dr_param%m1+1)
     z12 = sum( shift_dr_param%y1(shift_dr_param%m1:shift_dr_param%m2)  &
               *shift_dr_param%y2(shift_dr_param%m1:shift_dr_param%m2) )&
         / dble(shift_dr_param%m2-shift_dr_param%m1+1)
     z11 = sum( shift_dr_param%y1(shift_dr_param%m1:shift_dr_param%m2)  &
               *shift_dr_param%y1(shift_dr_param%m1:shift_dr_param%m2) )&
         / dble(shift_dr_param%m2-shift_dr_param%m1+1)
     z22 = sum( shift_dr_param%y2(shift_dr_param%m1:shift_dr_param%m2)  &
               *shift_dr_param%y2(shift_dr_param%m1:shift_dr_param%m2) )&
         / dble(shift_dr_param%m2-shift_dr_param%m1+1)
     ! solves the system of normal equations
     delta = z22 - z2**2
     shift_dr_param%aa = (z12 - (z1 * z2)) / delta
     shift_dr_param%bb = ((z22 * z1) - (z2 * z12)) / delta
     ! computes errors
     ! on aa
     s1 = delta**2 * (z22 + z2**2 + z11)
     s2 = real(2,kind=DOUBLE) * z2 * z12 - z1 * (z22 + z2**2)
     s2 = s2**2
     s3 = z12 - z1 * z2
     s3 = real(4,kind=DOUBLE) * z22 * s3**2
     shift_dr_param%da = dsqrt( (s1 + s2 + s3) &
                               / dble(shift_dr_param%m2-shift_dr_param%m1+1) )&
                        / delta**2
     ! on bb
     s1 = delta**2 * (z22 * (z22 + z2**2) + z11 * z2**2)
     s2 = real(2,kind=DOUBLE) * z22 * z1 * z2 - z12 * (z22 + z2**2)
     s2 = s2**2
     s3 = z22 * z1 - z2 * z12
     s3 = real(4,kind=DOUBLE) * z22 * s3**2
     shift_dr_param%db = dsqrt( (s1 + s2 + s3) &
                               / dble(shift_dr_param%m2-shift_dr_param%m1+1) )&
                       / delta**2
!
!    rescaled spectrum y2 ==> y3
     shift_dr_param%y3(shift_dr_param%m1:shift_dr_param%m2) = &
              shift_dr_param%aa &
            * shift_dr_param%y2(shift_dr_param%m1:shift_dr_param%m2)&
            + shift_dr_param%bb
!
!    rescaled spectrum dy2 ==> dy3
     shift_dr_param%dy3(shift_dr_param%m1:shift_dr_param%m2) = &
              shift_dr_param%aa &
            * shift_dr_param%dy2(shift_dr_param%m1:shift_dr_param%m2)
!
!    calculates rms between y1 and y3
     shift_dr_param%r13 = dsqrt(                                    &
        sum( ( shift_dr_param%dy3(shift_dr_param%m1:shift_dr_param%m2)      &
             - shift_dr_param%dy1(shift_dr_param%m1:shift_dr_param%m2) )**2)&
         / dble(shift_dr_param%m2-shift_dr_param%m1+1) )
!
     return
   end subroutine amplitude_adjust
!
!

!
!
!> scale_spectrum -- Public
!!
!! * Purpose
!!
!!     Spectrum scale computation
!!
!! * Description
!! 
!!     This subroutine computes the spectrum scale.
!!
!! * Inputs
!!
!!     -  N_Sample : sample number
!!     -  Spect_In : type_Spectrum / type for declaration and allocation of spectrum 
!!
!! * Inputs/outputs
!!
!!     -  Spect_Out : type_Spectrum / type for declaration and allocation of spectrum 
!!
!! * Outputs
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.7.3
!!

   subroutine scale_spectrum( N_Sample, Spect_In, Spect_Out )
     implicit none
     integer(kind=LONG) ,intent(in)                           :: N_Sample
     real(kind=DOUBLE)  ,intent(in)    ,dimension(1:N_Sample) :: Spect_In
     real(kind=DOUBLE)  ,intent(inout) ,dimension(1:N_Sample) :: Spect_Out
     real(kind=DOUBLE)                                        :: Spect_min
     real(kind=DOUBLE)                                        :: Spect_max
     integer(kind=LONG)                                       :: Ns
!
!    Minimum and maximun computation
     Spect_min = +1.d+10
     Spect_max = -1.d+10
     do Ns = 1, N_Sample
        if( Spect_In(Ns) > Spect_max ) then
           Spect_max = Spect_In(Ns)
        end if
     end do
     do Ns = 1, N_Sample
        if( Spect_In(Ns) < Spect_min ) then
           Spect_min = Spect_In(Ns)
        end if
     end do
!
     if( Spect_max - Spect_min /= 0 ) then
        do Ns = 1, N_Sample
           Spect_Out(Ns) = 2.d+00 * ( Spect_In(Ns) - Spect_min ) &
                         / ( Spect_max - Spect_min )             &
                         -1
        end do
      else
         do Ns = 1, N_Sample
            Spect_Out(Ns) = Spect_In(Ns)
         end do
      end if
!
     return
   end subroutine scale_spectrum
!
!
   subroutine rmbg_spectrum( shift_sp_param, Spectrum_In, Spectrum_Out )
     implicit none
     type(type_shift_sp_param) ,intent(in)                 :: shift_sp_param
     type(type_Spectrum)       ,intent(in)                 :: Spectrum_In
     type(type_Spectrum)       ,intent(out)                :: Spectrum_Out
     type(type_Interferogram)                              :: Interf
     integer(kind=LONG)                                    :: N_Sample_Max
     integer(kind=LONG)                                    :: NsFft
     integer(kind=LONG)                                    :: Ns_First
     integer(kind=LONG)                                    :: Ns_Last
     integer(kind=LONG)                                    :: ios
!
!    remove low frequency and keep only high frequency
     if( Spectrum_In%Type /= 'R' ) then
        write(*,*) 'Spectrum Type must be R',Spectrum_In%Type
        call exit(1)
     end if
!
     call Spectrum_Header_Transfer( Spectrum_In, Spectrum_Out )
!
!    Compute FFT size
     N_Sample_Max = 2 * (Spectrum_Out%Ns_Last*1.2)
     call tabule_NsFft( N_Sample_Max,&
                        NsFft, ios   )
     if( ios /= 0 ) then
        write(*,*) 'tabule_NsFft Error',NsFft
        write(*,*) 'spectrum_zero_padding Fatal Error'
        call exit(1)
     end if
     Spectrum_Out%WnMax    = dble(int(NsFft/2)) * Spectrum_Out%dWn
!
!    Interferogram computation
     Interf%Type    = 'R'
     Interf%dOpd = 1.d+00 / (2.d+00*dble(int(NsFft/2))*Spectrum_In%dWn)
     Interf%N_Sample = NsFft + 1
     call alloc_Interferogram( Interf )
     write(*,*) 'sptoif Spectrum, Interf'
     call sptoif( Spectrum_In, Interf )
     write(*,*) 'End sptoif Spectrum, Interf'
!
!    remove High Frequency
     Ns_First = int(NsFft/2) - int(NsFft/shift_sp_param%Fft_Reduction/2)
     Ns_Last  = int(NsFft/2) + int(NsFft/shift_sp_param%Fft_Reduction/2)
     Interf%Real_Part(1:Ns_First-1) = 0.d+00
     Interf%Real_Part(Ns_Last-1:Interf%N_Sample) = 0.d+00
!
!    Low frequency spectrum
     call iftosp( Interf, Spectrum_Out )
!
!    High frequency spectrum
     Spectrum_Out%Real_Part(1:Spectrum_Out%N_Sample) =             &
                    Spectrum_In%Real_Part(1:Spectrum_Out%N_Sample) &
                  - Spectrum_Out%Real_Part(1:Spectrum_Out%N_Sample)
!
!    De allocation
     call dalloc_Interferogram( Interf )
     return
   end subroutine rmbg_spectrum
!
!
end module spectral_shift_module
