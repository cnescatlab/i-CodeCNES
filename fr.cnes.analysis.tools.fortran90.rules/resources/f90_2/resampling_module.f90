!!#  resampling_module.f90 --
!!# 
!!#            Project: SPS_GENERIC
!!#            Authors: NOVELTIS/B.TOURNIER
!!#               Date: october 2009
!!#            Version: $Revision: 1.18 $
!!#  Last modification: $Date: 2011-11-02 14:57:50 $
!!#
!!#  Language:  F90
!!#  Standards: Noveltis
!!#
!!# --
!!#
!!
!>  interferogram resampling -- Module
!!
!! * Purpose
!!
!!   Module for interferogram resampling functions
!!
!! * Description
!!      
!!   This module aims to resample interferograms.
!!
!! * Sub-routines and functions
!!
!! -  interf_resampling_init           : initialisation of interferogram resampling parameters
!! -  interf_triplet_resampling_ref    : interferograms resampling thanks to the subroutine
!!                                       interf_resampling_ref for three targets Cold Space, 
!!                                       BlackBody and Earth-View
!! -  interf_triplet_resampling        : interferograms resampling thanks to the subroutine  
!!                                       interf_resampling for the three targets Cold Space, 
!!                                       BlackBody and Earth-View
!! -  interf_triplet_resampling_forman : optimized resampling of the interferograms thanks to  
!!                                       the interf_resampling_forman subroutine for the three  
!!                                       targets Cold Space, BlackBody and Earth-View
!! -  interf_triplet_resampling_spline : optimized resampling of the interferograms thanks to a 
!!                                       cubic spline interpolation for the three targets Cold 
!!                                       Space, BlackBody and Earth-View
!! -  if_quadruplet_resampling_spline  : optimized resampling of the interferograms thanks to a 
!!                                       cubic spline interpolation for the four targets Cold 
!!                                       Space, BlackBody, Earth-View1 and Earth-View2
!! -  interf_resampling_ref            : interferograms resampling thanks to the use of the 
!!                                       oversampled interferogram
!! -  interf_resampling                : interferograms resampling thanks to the use of cubic 
!!                                       spline interpolation
!! -  interf_resampling_forman         : resampling of the sampled interferograms in forman 
!!                                       configuration
!! -  interf_resampling_spline         : optimized resampling of the sampled interferograms 
!!                                       thanks to a cubic spline interpolation 
!! -  interf_resampling_decim          : resampling of the sampled interferograms thanks to the 
!!                                       decimation subroutine
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.5.6
!!

module resampling_module
   use precision_type
   use error_type
   use interf_param_type
   use resampling_param_type
   use interferogram_type
   use math_module
   use fft_module
   use zpd_type
   use nzpd_module
!
   implicit none
!
!
   public ::                                  &
             interf_triplet_resampling,       &
             interf_resampling_ref,           &
             interf_resampling_fullspline,    &
             interf_resampling_spline,        &
             interf_resampling_lagrange,      &
             interf_resampling_init
!
   contains
!
!
!> interf_triplet_resampling -- Public
!!
!! * Purpose
!!
!!     Interferogram resampling thanks to "interf_resampling" for 3 targets (CS/BB/EW)
!!
!! * Description
!! 
!!     This subroutine applies the interf_resampling subroutine on the three different
!!     targets : the Cold Space (CS), the BlackBody (BB) and one Earth-View (EW).
!!
!! * Inputs
!!                  
!!     - Resampling_Param : type_Resampling_Param / type for declaration and allocation of 
!!                          resampling parameters
!!     - Interf_Param     : type_Interf_Param / type for declaration and allocation of 
!!                          interferogram parameters
!!     - Zpd_CS           : type_Zpd / type for zpd parameters declaration and allocation
!!     - Zpd_BB           : type_Zpd / type for zpd parameters declaration and allocation
!!     - Zpd_EW           : type_Zpd / type for zpd parameters declaration and allocation
!!     - Interf_Samp_Rpd  : type_Interferogram / type for declaration and allocation of 
!!                          interferogram
!!     - Interf_Samp_CS   : type_Interferogram / type for declaration and allocation of 
!!                          interferogram
!!     - Interf_Samp_BB   : type_Interferogram / type for declaration and allocation of 
!!                          interferogram
!!     - Interf_Samp_EW   : type_Interferogram / type for declaration and allocation of 
!!                          interferogram
!!
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Interf_ReSamp_CS : type_Interferogram / type for declaration and allocation of 
!!                          interferogram
!!     - Interf_ReSamp_BB : type_Interferogram / type for declaration and allocation of 
!!                          interferogram
!!     - Interf_ReSamp_EW : type_Interferogram / type for declaration and allocation of 
!!                          interferogram
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.5.6
!!

    subroutine interf_triplet_resampling( Resampling_Param,&
                                          Interf_Param,    &
                                          Zpd_CS,          &
                                          Zpd_BB,          &
                                          Zpd_EW,          &
                                          Interf_Samp_Rpd, &
                                          Interf_Samp_CS,  &
                                          Interf_Samp_BB,  &
                                          Interf_Samp_EW,  &
                                          Interf_ReSamp_CS,&
                                          Interf_ReSamp_BB,&
                                          Interf_ReSamp_EW )
   implicit none
     type(type_Resampling_Param), intent(in)            :: Resampling_Param
     type(type_Interf_Param)    , intent(in)            :: Interf_Param
     type(type_Zpd)             , intent(in)            :: Zpd_CS
     type(type_Zpd)             , intent(in)            :: Zpd_BB
     type(type_Zpd)             , intent(in)            :: Zpd_EW
     type(type_Interferogram)   , intent(in)            :: Interf_Samp_Rpd
     type(type_Interferogram)   , intent(in)            :: Interf_Samp_CS
     type(type_Interferogram)   , intent(in)            :: Interf_Samp_BB
     type(type_Interferogram)   , intent(in)            :: Interf_Samp_EW
     type(type_Interferogram)   , intent(out)           :: Interf_ReSamp_CS
     type(type_Interferogram)   , intent(out)           :: Interf_ReSamp_BB
     type(type_Interferogram)   , intent(out)           :: Interf_ReSamp_EW
!
     select case ( Resampling_Param%Sampling_Mode(1:&
                   len_trim(Resampling_Param%Sampling_Mode)) )
     case ( 'REFERENCE' )
        call interf_resampling_ref( Resampling_Param,&
                                    Interf_Param,    &
                                    Zpd_CS,          &
                                    Interf_Samp_Rpd, &
                                    Interf_Samp_CS,  &
                                    Interf_ReSamp_CS )
!
        call interf_resampling_ref( Resampling_Param,&
                                    Interf_Param,    &
                                    Zpd_BB,          &
                                    Interf_Samp_Rpd, &
                                    Interf_Samp_BB,  &
                                    Interf_ReSamp_BB )
!
        call interf_resampling_ref( Resampling_Param,&
                                    Interf_Param,    &
                                    Zpd_EW,          &
                                    Interf_Samp_Rpd, &
                                    Interf_Samp_EW,  &
                                    Interf_ReSamp_EW )
!
     case ( 'FULLSPLINE' )
        call interf_resampling_fullspline( Resampling_Param,&
                                           Interf_Param,    &
                                           Zpd_CS,          &
                                           Interf_Samp_Rpd, &
                                           Interf_Samp_CS,  &
                                           Interf_ReSamp_CS )
!
        call interf_resampling_fullspline( Resampling_Param,&
                                           Interf_Param,    &
                                           Zpd_BB,          &
                                           Interf_Samp_Rpd, &
                                           Interf_Samp_BB,  &
                                           Interf_ReSamp_BB )
!
        call interf_resampling_fullspline( Resampling_Param,&
                                           Interf_Param,    &
                                           Zpd_EW,          &
                                           Interf_Samp_Rpd, &
                                           Interf_Samp_EW,  &
                                           Interf_ReSamp_EW )
!
     case ( 'SPLINE' )
        call interf_resampling_spline( Resampling_Param,&
                                       Interf_Param,    &
                                       Zpd_CS,          &
                                       Interf_Samp_Rpd, &
                                       Interf_Samp_CS,  &
                                       Interf_ReSamp_CS )
!
        call interf_resampling_spline( Resampling_Param,&
                                       Interf_Param,    &
                                       Zpd_BB,          &
                                       Interf_Samp_Rpd, &
                                       Interf_Samp_BB,  &
                                       Interf_ReSamp_BB )
!
        call interf_resampling_spline( Resampling_Param,&
                                       Interf_Param,    &
                                       Zpd_EW,          &
                                       Interf_Samp_Rpd, &
                                       Interf_Samp_EW,  &
                                       Interf_ReSamp_EW )
!
     case ( 'LAGRANGE' )
        call interf_resampling_lagrange( Resampling_Param,&
                                         Interf_Param,    &
                                         Zpd_CS,          &
                                         Interf_Samp_Rpd, &
                                         Interf_Samp_CS,  &
                                         Interf_ReSamp_CS )
!
        call interf_resampling_lagrange( Resampling_Param,&
                                         Interf_Param,    &
                                         Zpd_BB,          &
                                         Interf_Samp_Rpd, &
                                         Interf_Samp_BB,  &
                                         Interf_ReSamp_BB )
!
        call interf_resampling_lagrange( Resampling_Param,&
                                         Interf_Param,    &
                                         Zpd_EW,          &
                                         Interf_Samp_Rpd, &
                                         Interf_Samp_EW,  &
                                         Interf_ReSamp_EW )
     case default
        write(*,*) 'Re Sampling_Mode Error ',&
                    Resampling_Param%Sampling_Mode
        call exit(1)
     end select
!
     return
   end subroutine interf_triplet_resampling
!
!

!> interf_resampling_ref -- Public
!!
!! * Purpose
!!
!!     Interferograms resampling thanks to the use of the oversampled interferogram
!!
!! * Description
!! 
!!     This subroutine allows the interferograms resampling thanks to the use of the oversampled
!!     interferogram.
!!     More precisely, the interferogram resampling computation requires the following steps:
!!        - The first steps are the determination of the Fourier Transform size and the required 
!!          tables allocation.
!!        - Then, before the spectrum transform thanks to an inverse Fourier Transform, the 
!!          interferogram is transfered in a table with required dimension.
!!        - The obtained spectrum table is filled out with zero.
!!        - a Fourier Transform provides the oversampled interferogram, and abscissa in the over
!!          sampling space are computed, as well as the NZpd of the oversampled interferogram.
!!        - Then field angles compensation is applied.
!!        - At last, the interferogram resampling is done thanks to a cubic spline interpolator 
!!          and resampled interferogram parameters are computed before all tables deallocation.
!!
!! * Inputs
!!                  
!!     - Resampling_Param : type_Resampling_Param / type for declaration and allocation of 
!!                          resampling parameters
!!     - Interf_Param     : type_Interf_Param / type for declaration and allocation of 
!!                          interferogram parameters
!!     - Zpd              : type_Zpd / type for zpd parameters declaration and allocation
!!     - Interf_Samp_Rpd  : type_Interferogram / type for declaration and allocation of 
!!                          interferogram
!!     - Interf_Samp      : type_Interferogram / type for declaration and allocation of 
!!                          interferogram
!!
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Interf_ReSamp : type_Interferogram / type for declaration and allocation of 
!!                       interferogram
!!
!! * References
!! 
!!     NOV-3820-NT-ATBD : 4.5.6
!!

    subroutine interf_resampling_ref( Resampling_Param,&
                                      Interf_Param,    &
                                      Zpd,             &
                                      Interf_Samp_Rpd, &
                                      Interf_Samp,     &
                                      Interf_ReSamp    )
   implicit none
     type(type_Resampling_Param), intent(in)            :: Resampling_Param
     type(type_Interf_Param)    , intent(in)            :: Interf_Param
     type(type_Zpd)             , intent(in)            :: Zpd
     type(type_Interferogram)   , intent(in)            :: Interf_Samp_Rpd
     type(type_Interferogram)   , intent(in)            :: Interf_Samp
     type(type_Interferogram)   , intent(out)           :: Interf_ReSamp
     type(type_Interferogram)                           :: Interf_Samp_OS
     type(type_Interferogram)                           :: Interf_Samp_temp
     type(type_Interferogram)                           :: Interf_Samp_Rpd_temp
     type(type_Interferogram)                           :: Interf_Local
     real(kind=DOUBLE)                                  :: Opd0
     real(kind=DOUBLE)                                  :: Time0
     integer(kind=LONG)                                 :: Ns_Fft
     integer(kind=LONG)                                 :: OSNs_Fft
     real(kind=DOUBLE)                                  :: dWn
     integer(kind=LONG)                                 :: OSNZpd
     integer(kind=LONG)                                 :: ios
     integer(kind=LONG)                                 :: ErrCode
     integer(kind=LONG)                                 :: Sens
     integer(kind=LONG)                                 :: NsDel
     integer(kind=LONG)                                 :: Ns
     integer(kind=LONG)                                 :: NR1
     integer(kind=LONG)                                 :: NR2
     integer(kind=LONG)                                 :: n1
     integer(kind=LONG)                                 :: n2
     integer(kind=LONG)                                 :: N_Sample_first
     integer(kind=LONG)                                 :: N_Sample_last
     real(kind=DOUBLE)   ,dimension(:) ,allocatable     :: Interf
     real(kind=DOUBLE)   ,dimension(:) ,allocatable     :: OSInterf
     complex(kind=DOUBLE),dimension(:) ,allocatable     :: Spectrum
     complex(kind=DOUBLE),dimension(:) ,allocatable     :: OSSpectrum
     real(kind=DOUBLE)   ,dimension(:) ,allocatable     :: Absciss
     real(kind=DOUBLE)   ,dimension(:) ,allocatable     :: OSAbsciss
     real(kind=DOUBLE)   ,dimension(:) ,allocatable     :: Opd_Samp_Local
     real(kind=DOUBLE)   ,dimension(:) ,allocatable     :: Time_Samp_Local
!
     ios = 0
     if( Interf_Samp%Type /= 'R' ) then
        write(*,*) 'Interf_Samp%Type Error',Interf_Samp%Type
        write(*,*) 'interf_resampling_ref Fatal Error'
        call exit(1)
     end if
!
!    measured interferogram transfert into temporary fields
     Interf_Samp_temp%Type     = Interf_Samp%Type
     Interf_Samp_temp%N_Sample = Interf_Samp%N_Sample
     call alloc_Interferogram( Interf_Samp_temp )
     Interf_Samp_temp = Interf_Samp
!
!    metrology interferogram transfert into temporary fields
     Interf_Samp_Rpd_temp%Type     = Interf_Samp_Rpd%Type
     Interf_Samp_Rpd_temp%N_Sample = Interf_Samp_Rpd%N_Sample
     call alloc_Interferogram( Interf_Samp_Rpd_temp )
     Interf_Samp_Rpd_temp = Interf_Samp_Rpd
!
!    Interferogram oversampling
     write(*,*) 'Sampling_Mode',Interf_Param%Sampling_Mode
!    
!    size of the Fourier Transform
     call tabule_NsFft( Interf_Samp_temp%N_Sample-1, &
                        Ns_Fft,                      &
                        ios                          )
     if( ios /= 0 ) then
        write(*,*) 'tabule_NsFft Error'
        write(*,*) 'interf_resampling_ref Fatal Error'
        call exit(1)
     end if
!
!    allocation
     allocate( Interf(Ns_Fft+1), stat=ErrCode )
     ios = ios + ErrCode
     allocate( Spectrum(Ns_Fft+1), stat=ErrCode  )
     ios = ios + ErrCode
     if( ios /= 0 ) then
        write(*,*) 'Allocation Error',ios
        write(*,*) 'interf_resampling_ref Fatal Error'
        call exit(1)
     end if
!
!    interferogram transfer
     NsDel = int(Ns_Fft/2) - int(Interf_Samp_temp%N_Sample/2)
     if( NsDel < 0 ) then
        write(*,*) 'NsDel',NsDel
        write(*,*) 'interf_resampling_ref Fatal Error'
        call exit(1)
     end if
     write(*,*) 'interferogram transfer NsDel',NsDel
     Interf(1:Ns_Fft+1) = 0.d+00
     Interf(NsDel+1:NsDel+Interf_Samp_temp%N_Sample) =              & 
              Interf_Samp_temp%Real_Part(1:Interf_Samp_temp%N_Sample)
!
!    spectrum transform
     dWn = 1.d+00 / ( Interf_Samp_temp%dOpd * dble(Ns_Fft) )
     Sens = -1
     call fft_r2c( Ns_Fft, Sens, Interf, Interf_Samp_temp%dOpd, &
                   dWn, Spectrum                           )
!
!    zero filling
     OSNs_Fft = Ns_Fft * Resampling_Param%OSFactor
     allocate( OSInterf(OSNs_Fft+1), stat=ErrCode )
     ios = ios + ErrCode
     allocate( OSSpectrum(OSNs_Fft+1), stat=ErrCode )
     ios = ios + ErrCode
     if( ios /= 0 ) then
        write(*,*) 'Allocation Error',ios
        write(*,*) 'interf_resampling_ref Fatal Error'
        call exit(1)
     end if
     NsDel = int(OSNs_Fft/2) - int(Ns_Fft/2)
     write(*,*) 'zero filling NsDel',NsDel
     OSSpectrum(1:OSNs_Fft+1) = dcmplx(0.d+00,0.d+00)
     OSSpectrum(NsDel+1:NsDel+Ns_Fft+1) = Spectrum(1:Ns_Fft+1)
!
!    oversampled interferogram transform
     Sens = +1
     call fft_c2r( OSNs_Fft, Sens, OSSpectrum, dWn, &
                   Interf_Samp_temp%dOpd, OSInterf  )
!
     NsDel  = int( OSNs_Fft/2 ) &
            - int( ((Interf_Samp_temp%N_Sample-1)   &
                    *Resampling_Param%OSFactor+1)/2 )
     if( NsDel < 0 ) then
        write(*,*) 'NsDel OSInterf',NsDel
        write(*,*) 'interf_resampling_ref Fatal Error'
        call exit(1)
     end if
     write(*,*) 'interferogram oversampled NsDel',NsDel
     Interf_Samp_OS%N_Sample = OSNs_Fft + 1 - 2*NsDel
     write(*,*) 'interferogram oversampled N_Sample',Interf_Samp_OS%N_Sample
     Interf_Samp_OS%Type = Interf_Samp_temp%Type
     Interf_Samp_OS%dOpd = Interf_Samp_temp%dOpd           &
                           / dble(Resampling_Param%OSFactor)
     Interf_Samp_OS%dTime = Interf_Samp_temp%dTime          &
                            / dble(Resampling_Param%OSFactor)
     call alloc_Interferogram( Interf_Samp_OS )
!
!    compute abscissa in over sampling space
     allocate( Opd_Samp_Local(Interf_Samp_temp%N_Sample) )
     ios = ios + ErrCode
     allocate( Time_Samp_Local(Interf_Samp_temp%N_Sample) )
     ios = ios + ErrCode
     allocate( Absciss(Interf_Samp_temp%N_Sample) )
     ios = ios + ErrCode
     allocate( OSAbsciss(Interf_Samp_OS%N_Sample) )
     ios = ios + ErrCode
     if( ios /= 0 ) then
        write(*,*) 'Allocation Error',ios
        write(*,*) 'interf_resampling_ref Fatal Error'
        call exit(1)
     end if
!
     if( Interf_Param%Rpd_Type == 'RI' ) then
!
!       extraction of the Opd(t) law
        write(*,*) 'Interf_Samp_temp%N_Sample    ',&
                    Interf_Samp_temp%N_Sample
        write(*,*) 'Interf_Samp_Rpd_temp%N_Sample',&
                    Interf_Samp_Rpd_temp%N_Sample
        do while( Interf_Samp_temp%N_Sample*Interf_Param%Rpd_Samp_OSFactor > &
                  Interf_Samp_Rpd_temp%N_Sample )
           Interf_Samp_temp%N_Sample = Interf_Samp_temp%N_Sample-1
        end do
        do while( Interf_Samp_temp%N_Sample*Interf_Param%Rpd_Samp_OSFactor < &
                  Interf_Samp_Rpd_temp%N_Sample )
           Interf_Samp_Rpd_temp%N_Sample = Interf_Samp_Rpd_temp%N_Sample-1
        end do
        write(*,*) 'End Interf_Samp_temp%N_Sample    ',&
                    Interf_Samp_temp%N_Sample
        write(*,*) 'End Interf_Samp_Rpd_temp%N_Sample',&
                    Interf_Samp_Rpd_temp%N_Sample
        Opd_Samp_Local(1:Interf_Samp_temp%N_Sample) =                   &
               Interf_Samp_Rpd_temp%Opd(1:Interf_Samp_Rpd_temp%N_Sample &
                                         :Interf_Param%Rpd_Samp_OSFactor)
        Time_Samp_Local(1:Interf_Samp_temp%N_Sample) =                    &
                Interf_Samp_Rpd_temp%Time(1:Interf_Samp_Rpd_temp%N_Sample &
                                           :Interf_Param%Rpd_Samp_OSFactor)
        Opd0  = Opd_Samp_Local(int(Interf_Samp_temp%N_Sample/2)+1)
        Time0 = Time_Samp_Local(int(Interf_Samp_temp%N_Sample/2)+1)
        Opd_Samp_Local(1:Interf_Samp_temp%N_Sample) =            &
                Opd_Samp_Local(1:Interf_Samp_temp%N_Sample) - Opd0
        Time_Samp_Local(1:Interf_Samp_temp%N_Sample) =             &
                Time_Samp_Local(1:Interf_Samp_temp%N_Sample) - Time0
!
     else if( Interf_Param%Rpd_Type == 'R' ) then
        Opd0  = Interf_Samp_temp%Opd(Zpd%NZpd)
        Time0 = Interf_Samp_temp%Time(Zpd%NZpd)
        Opd_Samp_Local(1:Interf_Samp_temp%N_Sample) =                      &
                    Interf_Samp_temp%Opd(1:Interf_Samp_temp%N_Sample) - Opd0
        Time_Samp_Local(1:Interf_Samp_temp%N_Sample) =                       &
                    Interf_Samp_temp%Time(1:Interf_Samp_temp%N_Sample) - Time0
     else
        write(*,*) 'Interf_Param%Rpd_Type Error',Interf_Param%Rpd_Type
        call exit(1)
     end if
     write(*,*) 'Time_Samp_Local',Time_Samp_Local(1)
     write(*,*) 'Time_Samp_Local',Time_Samp_Local(Interf_Samp_temp%N_Sample)
     write(*,*) 'Opd_Samp_Local ',Opd_Samp_Local(1)
     write(*,*) 'Opd_Samp_Local ',Opd_Samp_Local(Interf_Samp_temp%N_Sample)
     Absciss(1:Interf_Samp_temp%N_Sample) =                      &
               (/ (dble( int(Ns-1)*Resampling_Param%OSFactor+1), &
                                Ns=1,Interf_Samp_temp%N_Sample) /)
     OSAbsciss(1:Interf_Samp_OS%N_Sample) =                &
               (/ (dble(Ns),Ns=1,Interf_Samp_OS%N_Sample) /)
     write(*,*) 'Absciss  ',Absciss(1)
     write(*,*) 'Absciss  ',Absciss(Interf_Samp_temp%N_Sample)
     write(*,*) 'OSAbsciss',OSAbsciss(1)
     write(*,*) 'OSAbsciss',OSAbsciss(Interf_Samp_OS%N_Sample)
     call intspline( Interf_Samp_temp%N_Sample,&
                     Absciss,                  &
                     Opd_Samp_Local,           &
                     Interf_Samp_OS%N_Sample,  &
                     OSAbsciss,                &
                     Interf_Samp_OS%Opd        )
     call intspline( Interf_Samp_temp%N_Sample,&
                     Absciss,                  &
                     Time_Samp_Local,          &
                     Interf_Samp_OS%N_Sample,  &
                     OSAbsciss,                &
                     Interf_Samp_OS%Time       )
!
!    compute NZpd of the oversampled interferogram
     OSNZpd = ( ( Zpd%NZpd-1+int(Ns_Fft/2)-int(Interf_Samp_temp%N_Sample/2) )&
                *Resampling_Param%OSFactor) +1
     OSNZpd = OSNZpd - NsDel
     write(*,*) 'Interf_Samp_temp%N_Sample',Interf_Samp_temp%N_Sample
     write(*,*) 'Zpd%NZpd                  ',Zpd%NZpd
     write(*,*) 'Resampling_Param%OSFactor',Resampling_Param%OSFactor
     write(*,*) 'NsDel                     ',NsDel
     write(*,*) 'OSN_Sample :              ',Interf_Samp_OS%N_Sample
     write(*,*) 'OSNZpd     :              ',OSNZpd
!
!    Field Angles Compensation
     Opd0  = Interf_Samp_OS%Opd(OSNZpd)
     Time0 = Interf_Samp_OS%Time(OSNZpd)
     write(*,*) 'Opd0,Time0 :',Opd0,Time0
     write(*,*) 'Resampling_Param%Field_Compensation : '&
                ,Resampling_Param%Field_Compensation
     write(*,*) 'Interf_Samp%FieldMeanAngle : ',&
                 Interf_Samp%FieldMeanAngle
     if( Resampling_Param%Field_Compensation .eq. 'YES' ) then
        Interf_Samp_OS%Opd(1:Interf_Samp_OS%N_Sample) =       &
      ( Interf_Samp_OS%Opd(1:Interf_Samp_OS%N_Sample) - Opd0 )&
      * dcos(Interf_Samp%FieldMeanAngle)
        Interf_Samp_OS%Time(1:Interf_Samp_OS%N_Sample) =        &
      ( Interf_Samp_OS%Time(1:Interf_Samp_OS%N_Sample) - Time0 )&
      * dcos(Interf_Samp%FieldMeanAngle)
     else if( Resampling_Param%Field_Compensation .eq. 'NO' ) then
        Interf_Samp_OS%Opd(1:Interf_Samp_OS%N_Sample) =      &
      ( Interf_Samp_OS%Opd(1:Interf_Samp_OS%N_Sample) - Opd0 )
        Interf_Samp_OS%Time(1:Interf_Samp_OS%N_Sample) =       &
      ( Interf_Samp_OS%Time(1:Interf_Samp_OS%N_Sample) - Time0 )
     else
        write(*,*) 'Field Compensation parameter Error'
        write(*,*) 'interf_resampling_ref Fatal Error'
        call exit(1)
     end if
     Interf_Samp_OS%Real_Part(1:Interf_Samp_OS%N_Sample) =   &
               OSInterf(NsDel+1:NsDel+Interf_Samp_OS%N_Sample)
     write(*,*) 'Interf_Samp_OS Time(1)       ',Interf_Samp_OS%Time(1)
     write(*,*) 'Interf_Samp_OS Time(N_Sample)',&
                          Interf_Samp_OS%Time(Interf_Samp_OS%N_Sample)
     write(*,*) 'Interf_Samp_OS Opd(1)        ',Interf_Samp_OS%Opd(1)
     write(*,*) 'Interf_Samp_OS Opd(N_Sample) ',&
                          Interf_Samp_OS%Opd(Interf_Samp_OS%N_Sample)
     write(*,*) 'Interf_Samp_OS Time0         ',Time0
     write(*,*) 'Interf_Samp_OS Opd0          ',Opd0
     write(*,*) 'Interf_Samp_OS Real(1)        ',Interf_Samp_OS%Real_Part(1)
     write(*,*) 'Interf_Samp_OS Real(N_Sample) ',&
                          Interf_Samp_OS%Real_Part(Interf_Samp_OS%N_Sample)
!
!    Local resampled interferogram initialisation
     Interf_Local%Type     = Interf_Samp_temp%Type
     Interf_Local%N_Sample = 2 * int( Interf_Samp_Rpd%N_Sample / 2      &
                                    / Interf_Param%Rpd_Samp_OSFactor ) +1
     Interf_Local%dOpd     = Resampling_Param%dOpd
     Interf_Local%dTime    = Interf_Local%dOpd       &
                           / (2.d+00*Interf_Param%VCC)
     Interf_Local%OpdMax   = Interf_Local%dOpd                &
                           * dble(int(Interf_Local%N_Sample/2))
     Interf_Local%TimeMax  = Interf_Local%OpdMax     &
                           / (2.d+00*Interf_Param%VCC)
     call alloc_Interferogram( Interf_Local )
     call Interferogram_Basis( Interf_Local )
     Opd0  = Interf_Local%Opd(int(Interf_Local%N_Sample/2)+1)
     Time0 = Interf_Local%Time(int(Interf_Local%N_Sample/2)+1)
     Interf_Local%Opd(1:Interf_Local%N_Sample) =                 &
                  Interf_Local%Opd(1:Interf_Local%N_Sample) - Opd0
     Interf_Local%Time(1:Interf_Local%N_Sample) =                  &
                  Interf_Local%Time(1:Interf_Local%N_Sample) - Time0
     write(*,*) 'Interf Local N_Sample',Interf_Local%N_Sample
     write(*,*) 'Interf local Time',Interf_Local%Time(1)
     write(*,*) 'Interf local Time',Interf_Local%Time(Interf_Local%N_Sample)
     write(*,*) 'Interf local Opd ',Interf_Local%Opd(1)
     write(*,*) 'Interf local Opd ',Interf_Local%Opd(Interf_Local%N_Sample)
!
!    search for the first availlable sample
     N_Sample_first = 1
     do while( Interf_Local%Time(N_Sample_first) < &
               Interf_Samp_OS%Time(1)              )
        N_Sample_first = N_Sample_first + 1
     end do
     write(*,*) 'N_Sample_first :',N_Sample_first
!
!    search for the last availlable sample
     N_Sample_last = Interf_Local%N_Sample
     do while( Interf_Local%Time(N_Sample_last) >           &
               Interf_Samp_OS%Time(Interf_Samp_OS%N_Sample) )
        N_Sample_last = N_Sample_last - 1
     end do
     write(*,*) 'N_Sample_last :',N_Sample_last
!
!    first usefull sample of the input interferogram
     n1 = N_Sample_first
     n2 = N_Sample_last
     write(*,*) 'n1, n2',n1, n2
     call dichotomd( Interf_Samp_OS%Time(1),          &
                     Interf_Local%Time(n1:n2), n1, n2 )
     NR1 = n2
!
!    last usefull sample of the input interferogram
     n1 = N_Sample_first
     n2 = N_Sample_last
     write(*,*) 'n1, n2',n1, n2
     call dichotomd( Interf_Samp_OS%Time(Interf_Samp_OS%N_Sample),&
                     Interf_Local%Time(n1:n2), n1, n2             )
     NR2 = n1
     write(*,*) 'NR1, NR2',NR1, NR2
!
!    Resampling using a cubic spline interpolator
     call intspline( Interf_Samp_OS%N_Sample,        &
                     Interf_Samp_OS%Time,            &
                     Interf_Samp_OS%Real_Part,       &
                     NR2-NR1+1,                      &
                     Interf_Local%Time(NR1:NR2),     &
                     Interf_Local%Real_Part(NR1:NR2) )
!
!    Resampled interferogram parameters
     Interf_ReSamp%Type     = Interf_Local%Type
     Interf_ReSamp%N_Sample = NR2-NR1+1
     Interf_ReSamp%N_Top    = 0
     Interf_ReSamp%dOpd     = Interf_Local%dOpd
     Interf_ReSamp%dTime    = Interf_ReSamp%dOpd      &
                            / (2.d+00*Interf_Param%VCC)
     Interf_ReSamp%OpdMax   = Interf_ReSamp%dOpd                &
                            * dble(int(Interf_ReSamp%N_Sample/2))
     Interf_ReSamp%TimeMax  = Interf_ReSamp%OpdMax    &
                            / (2.d+00*Interf_Param%VCC)
     if( Resampling_Param%Field_Compensation == 'YES' ) then
        Interf_ReSamp%FieldMeanAngle = 0.0d+00
     else if( Resampling_Param%Field_Compensation == 'NO' ) then
        Interf_ReSamp%FieldMeanAngle = Interf_Samp%FieldMeanAngle
     end if
     call alloc_Interferogram( Interf_ReSamp )
     Interf_ReSamp%Real_Part(1:Interf_ReSamp%N_Sample) =           &
                                     Interf_Local%Real_Part(NR1:NR2)
     Interf_ReSamp%Time(1:Interf_ReSamp%N_Sample) =           &
                                     Interf_Local%Time(NR1:NR2)
     Interf_ReSamp%Opd(1:Interf_ReSamp%N_Sample) =           &
                                     Interf_Local%Opd(NR1:NR2)
     write(*,*) 'Interf_Samp_OS%N_Sample :',&
                 Interf_Samp_OS%N_Sample
     write(*,*) 'Interf_Samp_Rpd%N_Top :',  &
                 Interf_Samp_Rpd%N_Top
     write(*,*) 'Interf_ReSamp%N_Sample :', &
                 Interf_ReSamp%N_Sample
!
!    de allocation
     call dalloc_Interferogram( Interf_Samp_temp )
     call dalloc_Interferogram( Interf_Samp_Rpd_temp )
     call dalloc_Interferogram( Interf_Samp_OS )
     call dalloc_Interferogram( Interf_Local )
     deallocate( Interf )
     deallocate( Spectrum )
     deallocate( OSInterf )
     deallocate( OSSpectrum )
     deallocate( Opd_Samp_Local )
     deallocate( Time_Samp_Local )
     deallocate( Absciss )
     deallocate( OSAbsciss )
     return
   end subroutine interf_resampling_ref
!
!

!> interf_resampling_fullspline -- Public
!!
!! * Purpose
!!
!!     Interferograms resampling thanks to the use of cubic spline interpolation
!!
!! * Description
!! 
!!     This subroutine allows the interferograms resampling thanks to a cubic spline 
!!     interpolation
!!
!! * Inputs
!!                  
!!     - Resampling_Param : type_Resampling_Param / type for declaration and allocation of 
!!                          resampling parameters
!!     - Interf_Param     : type_Interf_Param / type for declaration and allocation of 
!!                          interferogram parameters
!!     - Zpd              : type_Zpd / type for zpd parameters declaration and allocation
!!     - Interf_Samp_Rpd  : type_Interferogram / type for declaration and allocation of 
!!                          interferogram
!!     - Interf_Samp      : type_Interferogram / type for declaration and allocation of 
!!                          interferogram
!!
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Interf_ReSamp : type_Interferogram / type for declaration and allocation of 
!!                       interferogram
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.5.6
!!


    subroutine interf_resampling_fullspline( Resampling_Param,&
                                             Interf_Param,    &
                                             Zpd,             &
                                             Interf_Samp_Rpd, &
                                             Interf_Samp,     &
                                             Interf_ReSamp    )

   implicit none
     type(type_Resampling_Param), intent(in)           :: Resampling_Param
     type(type_Interf_Param)    , intent(in)           :: Interf_Param
     type(type_Zpd)             , intent(in)           :: Zpd
     type(type_Interferogram)   , intent(in)           :: Interf_Samp_Rpd
     type(type_Interferogram)   , intent(in)           :: Interf_Samp
     type(type_Interferogram)   , intent(out)          :: Interf_ReSamp
     type(type_Interferogram)                          :: Interf_Samp_temp
     type(type_Interferogram)                          :: Interf_Samp_Rpd_temp
     type(type_Interferogram)                          :: Interf_Local
     real(kind=DOUBLE)                                 :: Opd0
     real(kind=DOUBLE)                                 :: Time0
     integer(kind=LONG)                                :: NR1
     integer(kind=LONG)                                :: NR2
     integer(kind=LONG)                                :: n1
     integer(kind=LONG)                                :: n2
     integer(kind=LONG)                                :: N_Top_first
     integer(kind=LONG)                                :: N_Top_last
!
     write(*,*) 'interf_resampling full spline'
     if( Interf_Samp%Type /= 'R' ) then
        write(*,*) 'Interf_Samp%Type Error',Interf_Samp%Type
        write(*,*) 'interf_resampling full spline Fatal Error'
        call exit(1)
     end if
!
!    measured interferogram transfert into temporary fields
     Interf_Samp_temp%Type     = Interf_Samp%Type
     Interf_Samp_temp%N_Sample = Interf_Samp%N_Sample
     call alloc_Interferogram( Interf_Samp_temp )
     Interf_Samp_temp = Interf_Samp
!
!    metrology interferogram transfert into temporary fields
     Interf_Samp_Rpd_temp%Type     = Interf_Samp_Rpd%Type
     Interf_Samp_Rpd_temp%N_Sample = Interf_Samp_Rpd%N_Sample
     call alloc_Interferogram( Interf_Samp_Rpd_temp )
     Interf_Samp_Rpd_temp = Interf_Samp_Rpd
!
     write(*,*) 'Interf_Samp_temp%N_Sample    ',Interf_Samp_temp%N_Sample
     write(*,*) 'Interf_Samp_Rpd_temp%N_Sample',Interf_Samp_Rpd_temp%N_Sample
     write(*,*) 'Interf_Samp_Rpd_temp%N_Top   ',Interf_Samp_Rpd_temp%N_Top
!
     if( Interf_Param%Rpd_Type == 'RI' ) then
!
!       extraction of the Opd(t) law
        do while( Interf_Samp_temp%N_Sample*Interf_Param%Rpd_Samp_OSFactor > &
                  Interf_Samp_Rpd_temp%N_Sample )
           Interf_Samp_temp%N_Sample = Interf_Samp_temp%N_Sample-1
        end do
        do while( Interf_Samp_temp%N_Sample*Interf_Param%Rpd_Samp_OSFactor < &
                  Interf_Samp_Rpd_temp%N_Sample )
           Interf_Samp_Rpd_temp%N_Sample = Interf_Samp_Rpd_temp%N_Sample-1
        end do
        write(*,*) 'End Interf_Samp_temp%N_Sample    ',&
                    Interf_Samp_temp%N_Sample
        write(*,*) 'End Interf_Samp_Rpd_temp%N_Sample',&
                    Interf_Samp_Rpd_temp%N_Sample
        Interf_Samp_temp%Opd(1:Interf_Samp_temp%N_Sample) =      &
             Interf_Samp_Rpd%Opd(1:Interf_Samp_Rpd_temp%N_Sample &
                                  :Interf_Param%Rpd_Samp_OSFactor)
        Interf_Samp_temp%Time(1:Interf_Samp_temp%N_Sample) =      &
             Interf_Samp_Rpd%Time(1:Interf_Samp_Rpd_temp%N_Sample &
                                   :Interf_Param%Rpd_Samp_OSFactor)
!
     else if( Interf_Param%Rpd_Type == 'R' ) then
!
!       extraction of the Rpd Top
        Write(*,*) 'Sampling CLOCK/LZRPD nothing to do'
        write(*,*) 'End Interf_Samp_temp%N_Sample    ',&
                    Interf_Samp_temp%N_Sample
        write(*,*) 'End Interf_Samp_Rpd_temp%N_Top',&
                    Interf_Samp_Rpd_temp%N_Top
     else
        write(*,*) 'Interf_Param%Rpd_Type Error',Interf_Param%Rpd_Type
        call exit(1)
     end if
!
!    Field Angles Compensation
     Opd0  = Interf_Samp%Opd(Zpd%NZpd)
     Time0 = Interf_Samp%Time(Zpd%NZpd)
     if( Resampling_Param%Field_Compensation == 'YES' ) then
        write(*,*) 'Field Compensation  YES'
        Interf_Samp_temp%Opd(1:Interf_Samp_temp%N_Sample) =            &
           ( Interf_Samp_temp%Opd(1:Interf_Samp_temp%N_Sample) - Opd0 )&
           * dcos(Interf_Samp_temp%FieldMeanAngle)
        Interf_Samp_temp%Time(1:Interf_Samp_temp%N_Sample) =             &
           ( Interf_Samp_temp%Time(1:Interf_Samp_temp%N_Sample) - Time0 )&
           * dcos(Interf_Samp_temp%FieldMeanAngle)
     else if( Resampling_Param%Field_Compensation == 'NO' ) then
        write(*,*) 'Field Compensation  NO'
        Interf_Samp_temp%Opd(1:Interf_Samp_temp%N_Sample) =           &
           ( Interf_Samp_temp%Opd(1:Interf_Samp_temp%N_Sample) - Opd0 )
        Interf_Samp_temp%Time(1:Interf_Samp_temp%N_Sample) =            &
           ( Interf_Samp_temp%Time(1:Interf_Samp_temp%N_Sample) - Time0 )
     else
        write(*,*) 'Field Compensation parameter Error'
        write(*,*) 'interf_resampling full spline Fatal Error'
        call exit(1)
     end if
     write(*,*) 'Interf_Samp Time(1)       ',Interf_Samp_temp%Time(1)
     write(*,*) 'Interf_Samp Time(N_Sample)',&
                          Interf_Samp_temp%Time(Interf_Samp_temp%N_Sample)
     write(*,*) 'Interf_Samp Opd(1)        ',Interf_Samp_temp%Opd(1)
     write(*,*) 'Interf_Samp Opd(N_Sample) ',&
                          Interf_Samp_temp%Opd(Interf_Samp_temp%N_Sample)
     write(*,*) 'Interf_Samp Time0         ',Time0
     write(*,*) 'Interf_Samp Opd0          ',Opd0
!
!    Local interferogram
!
     Interf_Local%Type     = Interf_Samp%Type
     Interf_Local%N_Sample = 2 * int( Interf_Samp_Rpd_temp%N_Sample / 2 &
                                    / Interf_Param%Rpd_Samp_OSFactor ) +1
     Interf_Local%dOpd     = Resampling_Param%dOpd
     Interf_Local%dTime    = Interf_Local%dOpd       &
                           / (2.d+00*Interf_Param%VCC)
     Interf_Local%OpdMax   = Interf_Local%dOpd          &
                           * int(Interf_Local%N_Sample/2)
     Interf_Local%TimeMax  = Interf_Local%OpdMax     &
                           / (2.d+00*Interf_Param%VCC)
     call alloc_Interferogram( Interf_Local )
     call Interferogram_Basis( Interf_Local )
     Opd0  = Interf_Local%Opd(int(Interf_Local%N_Sample/2)+1)
     Time0 = Interf_Local%Time(int(Interf_Local%N_Sample/2)+1)
     Interf_Local%Opd(1:Interf_Local%N_Sample) =              &
               Interf_Local%Opd(1:Interf_Local%N_Sample) - Opd0
     Interf_Local%Time(1:Interf_Local%N_Sample) =               &
               Interf_Local%Time(1:Interf_Local%N_Sample) - Time0
!
     write(*,*) 'Interf Local N_Sample',Interf_Local%N_Sample
     write(*,*) 'Interf local Time',Interf_Local%Time(1)
     write(*,*) 'Interf local Time',Interf_Local%Time(Interf_Local%N_Sample)
     write(*,*) 'Interf local Opd ',Interf_Local%Opd(1)
     write(*,*) 'Interf local Opd ',Interf_Local%Opd(Interf_Local%N_Sample)
!
!    Resampling using a cubic spline interpolator
     call intspline( Interf_Samp_temp%N_Sample, &
                     Interf_Samp_temp%Time,     &
                     Interf_Samp_temp%Real_Part,&
                     Interf_Local%N_Sample,     &
                     Interf_Local%Time,         &
                     Interf_Local%Real_Part     )
!
!    search for the first availlable sample
     N_Top_first = 1
     do while( Interf_Local%Time(N_Top_first) < &
               Interf_Samp_temp%Time(1)         )
        N_Top_first = N_Top_first + 1
     end do
     write(*,*) 'N_Top_first :',N_Top_first
!
!    search for the last availlable sample
     N_Top_last = Interf_Local%N_Sample
     do while( Interf_Local%Time(N_Top_last) >                  &
               Interf_Samp_temp%Time(Interf_Samp_temp%N_Sample) )
        N_Top_last = N_Top_last - 1
     end do
     write(*,*) 'N_Top_last :',N_Top_last
!
!    first and last sample
     n1 = N_Top_first
     n2 = N_Top_last
     write(*,*) 'n1, n2',n1, n2
     call dichotomd( Interf_Samp_temp%Time(1),        &
                     Interf_Local%Time(n1:n2), n1, n2 )
     NR1 = n2
     n1 = N_Top_first
     n2 = N_Top_last
     write(*,*) 'n1, n2',n1, n2
     call dichotomd( Interf_Samp_temp%Time(Interf_Samp_temp%N_Sample),&
                     Interf_Local%Time(n1:n2), n1, n2                 )
     NR2 = n1
     write(*,*) 'NR1, NR2',NR1, NR2
!
!    Resampled interferogram parameters
     Interf_ReSamp%Type     = Interf_Local%Type
     Interf_ReSamp%N_Sample = NR2-NR1+1
     Interf_ReSamp%N_Top    = 0
     Interf_ReSamp%dOpd     = Interf_Local%dOpd
     Interf_ReSamp%dTime    = Interf_ReSamp%dOpd      &
                            / (2.d+00*Interf_Param%VCC)
     Interf_ReSamp%OpdMax   = Interf_ReSamp%dOpd                 &
                            * dble(int(Interf_ReSamp%N_Sample/2))
     Interf_ReSamp%TimeMax  = Interf_ReSamp%OpdMax    &
                            / (2.d+00*Interf_Param%VCC)
     if( Resampling_Param%Field_Compensation == 'YES' ) then
        Interf_ReSamp%FieldMeanAngle = 0.0d+00
     else if( Resampling_Param%Field_Compensation == 'NO' ) then
        Interf_ReSamp%FieldMeanAngle = Interf_Samp%FieldMeanAngle
     end if
     call alloc_Interferogram( Interf_ReSamp )
     Interf_ReSamp%Real_Part(1:Interf_ReSamp%N_Sample) = &
                           Interf_Local%Real_Part(NR1:NR2)
     Interf_ReSamp%Time(1:Interf_ReSamp%N_Sample) =      &
                                Interf_Local%Time(NR1:NR2)
     Interf_ReSamp%Opd(1:Interf_ReSamp%N_Sample) =       &
                                 Interf_Local%Opd(NR1:NR2)
     write(*,*) 'Interf_Samp_temp%N_Sample :',&
                 Interf_Samp_temp%N_Sample
     write(*,*) 'Interf_Samp_Rpd%N_Top :',    &
                 Interf_Samp_Rpd%N_Top
     write(*,*) 'Interf_ReSamp%N_Sample :',   &
                 Interf_ReSamp%N_Sample
!
     call dalloc_Interferogram( Interf_Local )
     call dalloc_Interferogram( Interf_Samp_temp )
     call dalloc_Interferogram( Interf_Samp_Rpd_temp )
!
     return
   end subroutine interf_resampling_fullspline
!
!
!> interf_resampling_spline -- Public
!!
!! * Purpose
!!
!!     Optimized resampling of the sampled interferograms thanks to a cubic spline interpolation 
!!
!! * Description
!! 
!!     The sampled interferograms resampling done within this subroutine is based on a cubic 
!!     spline interpolation ; it is optimized in order to be used in an operational mode. 
!!      - The input is an interferogram sampled by a clock, 
!!      - then the resampling is done on the top RPD temporal basis which is the basis of the 
!!        zero of the RPD laser, which gives OPD sampling on the interferometric axis. 
!!      - The time and OPD basis are centred on the NZPD of the sampled interferogram and on 
!!        the value (Ntop/2 +1) of the RPD tops. 
!!      - After the cubic spline interpolation, the NZPD of the interferogram resampled is 
!!        calculated and the resampled interferogram is centered.
!!
!! * Inputs
!!                  
!!     - Resampling_Param : type_Resampling_Param / type for declaration and allocation of 
!!                          resampling parameters
!!     - Interf_Param     : type_Interf_Param / type for declaration and allocation of 
!!                          interferogram parameters
!!     - Zpd              : type_Zpd / type for zpd parameters declaration and allocation
!!     - Interf_Samp_Rpd  : type_Interferogram / type for declaration and allocation of 
!!                          interferogram
!!     - Interf_Samp      : type_Interferogram / type for declaration and allocation of 
!!                          interferogram
!!
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Interf_ReSamp : type_Interferogram / type for declaration and allocation of 
!!                       interferogram
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.5.6
!!

    subroutine interf_resampling_spline( Resampling_Param,&
                                         Interf_Param,    &
                                         Zpd,             &
                                         Interf_Samp_Rpd, &
                                         Interf_Samp,     &
                                         Interf_ReSamp    )
   implicit none
     type(type_Resampling_Param), intent(in)           :: Resampling_Param
     type(type_Interf_Param)    , intent(in)           :: Interf_Param
     type(type_Zpd)             , intent(in)           :: Zpd
     type(type_Interferogram)   , intent(in)           :: Interf_Samp_Rpd
     type(type_Interferogram)   , intent(in)           :: Interf_Samp
     type(type_Interferogram)   , intent(out)          :: Interf_ReSamp
     type(type_Interferogram)                          :: Interf_Samp_temp
     type(type_Interferogram)                          :: Interf_Samp_Rpd_temp
     type(type_Interferogram)                          :: Interf_Local
     real(kind=DOUBLE)                                 :: Opd0
     real(kind=DOUBLE)                                 :: Time0
     integer(kind=LONG)                                :: dN1
     integer(kind=LONG)                                :: dN2
     integer(kind=LONG)                                :: n1
     integer(kind=LONG)                                :: n2
     integer(kind=LONG)                                :: Ns1
     integer(kind=LONG)                                :: Ns2
     integer(kind=LONG)                                :: NR1
     integer(kind=LONG)                                :: NR2
     integer(kind=LONG)                                :: NRs
     integer(kind=LONG)                                :: NReSample
     integer(kind=LONG)                                :: Ns
     integer(kind=LONG)                                :: N_Sample_first
     integer(kind=LONG)                                :: N_Sample_last
!
     write(*,*) 'resampling_spline'
     if( Interf_Samp%Type /= 'R' ) then
        write(*,*) 'Interf_Samp%Type Error',Interf_Samp%Type
        write(*,*) 'interf_resampling_spline Fatal Error'
        call exit(1)
     end if
!
!    measured interferogram transfert into temporary fields
     Interf_Samp_temp%Type     = Interf_Samp%Type
     Interf_Samp_temp%N_Sample = Interf_Samp%N_Sample
     call alloc_Interferogram( Interf_Samp_temp )
     Interf_Samp_temp = Interf_Samp
!
!    metrology interferogram transfert into temporary fields
     Interf_Samp_Rpd_temp%Type     = Interf_Samp_Rpd%Type
     Interf_Samp_Rpd_temp%N_Sample = Interf_Samp_Rpd%N_Sample
     call alloc_Interferogram( Interf_Samp_Rpd_temp )
     Interf_Samp_Rpd_temp = Interf_Samp_Rpd
!
     write(*,*) 'Interf_Samp_temp%N_Sample    ',Interf_Samp_temp%N_Sample
     write(*,*) 'Interf_Samp_Rpd_temp%N_Sample',Interf_Samp_Rpd_temp%N_Sample
     write(*,*) 'Interf_Samp_Rpd_temp%N_Top   ',Interf_Samp_Rpd_temp%N_Top
!
     if( Interf_Param%Rpd_Type == 'RI' ) then
!
!       extraction of the Opd(t) law
        do while( Interf_Samp_temp%N_Sample*Interf_Param%Rpd_Samp_OSFactor > &
                  Interf_Samp_Rpd_temp%N_Sample )
           Interf_Samp_temp%N_Sample = Interf_Samp_temp%N_Sample-1
        end do
        do while( Interf_Samp_temp%N_Sample*Interf_Param%Rpd_Samp_OSFactor < &
                  Interf_Samp_Rpd_temp%N_Sample )
           Interf_Samp_Rpd_temp%N_Sample = Interf_Samp_Rpd_temp%N_Sample-1
        end do
        write(*,*) 'End Interf_Samp_temp%N_Sample    ',&
                    Interf_Samp_temp%N_Sample
        write(*,*) 'End Interf_Samp_Rpd_temp%N_Sample',&
                    Interf_Samp_Rpd_temp%N_Sample
        Interf_Samp_temp%Opd(1:Interf_Samp_temp%N_Sample) =      &
             Interf_Samp_Rpd%Opd(1:Interf_Samp_Rpd_temp%N_Sample &
                                  :Interf_Param%Rpd_Samp_OSFactor)
        Interf_Samp_temp%Time(1:Interf_Samp_temp%N_Sample) =      &
             Interf_Samp_Rpd%Time(1:Interf_Samp_Rpd_temp%N_Sample &
                                   :Interf_Param%Rpd_Samp_OSFactor)
!
     else if( Interf_Param%Rpd_Type == 'R' ) then
!
!       extraction of the Rpd Top
        Write(*,*) 'Sampling CLOCK/LZRPD nothing to do'
        write(*,*) 'End Interf_Samp_temp%N_Sample    ',&
                    Interf_Samp_temp%N_Sample
        write(*,*) 'End Interf_Samp_Rpd_temp%N_Top',&
                    Interf_Samp_Rpd_temp%N_Top
     else
        write(*,*) 'Interf_Param%Rpd_Type Error',Interf_Param%Rpd_Type
        call exit(1)
     end if
!
!    Field Angles Compensation
     Opd0  = Interf_Samp%Opd(Zpd%NZpd)
     Time0 = Interf_Samp%Time(Zpd%NZpd)
     if( Resampling_Param%Field_Compensation == 'YES' ) then
        write(*,*) 'Field Compensation  YES'
        Interf_Samp_temp%Opd(1:Interf_Samp_temp%N_Sample) =            &
           ( Interf_Samp_temp%Opd(1:Interf_Samp_temp%N_Sample) - Opd0 )&
           * dcos(Interf_Samp_temp%FieldMeanAngle)
        Interf_Samp_temp%Time(1:Interf_Samp_temp%N_Sample) =             &
           ( Interf_Samp_temp%Time(1:Interf_Samp_temp%N_Sample) - Time0 )&
           * dcos(Interf_Samp_temp%FieldMeanAngle)
     else if( Resampling_Param%Field_Compensation == 'NO' ) then
        write(*,*) 'Field Compensation  NO'
        Interf_Samp_temp%Opd(1:Interf_Samp_temp%N_Sample) =           &
           ( Interf_Samp_temp%Opd(1:Interf_Samp_temp%N_Sample) - Opd0 )
        Interf_Samp_temp%Time(1:Interf_Samp_temp%N_Sample) =            &
           ( Interf_Samp_temp%Time(1:Interf_Samp_temp%N_Sample) - Time0 )
     else
        write(*,*) 'Field Compensation parameter Error'
        write(*,*) 'interf_resampling_spline Fatal Error'
        call exit(1)
     end if
     write(*,*) 'Interf_Samp Time(1)       ',Interf_Samp_temp%Time(1)
     write(*,*) 'Interf_Samp Time(N_Sample)',&
                          Interf_Samp_temp%Time(Interf_Samp_temp%N_Sample)
     write(*,*) 'Interf_Samp Opd(1)        ',Interf_Samp_temp%Opd(1)
     write(*,*) 'Interf_Samp Opd(N_Sample) ',&
                          Interf_Samp_temp%Opd(Interf_Samp_temp%N_Sample)
     write(*,*) 'Interf_Samp Time0         ',Time0
     write(*,*) 'Interf_Samp Opd0          ',Opd0
!
!    Local interferogram
!
     Interf_Local%Type     = Interf_Samp%Type
     Interf_Local%N_Sample = 2 * int( Interf_Samp_Rpd%N_Sample / 2      &
                                    / Interf_Param%Rpd_Samp_OSFactor ) +1
     Interf_Local%dOpd     = Resampling_Param%dOpd
     Interf_Local%dTime    = Interf_Local%dOpd       &
                           / (2.d+00*Interf_Param%VCC)
     Interf_Local%OpdMax   = Interf_Local%dOpd          &
                           * int(Interf_Local%N_Sample/2)
     Interf_Local%TimeMax  = Interf_Local%OpdMax     &
                           / (2.d+00*Interf_Param%VCC)
     call alloc_Interferogram( Interf_Local )
     call Interferogram_Basis( Interf_Local )
     Opd0  = Interf_Local%Opd(int(Interf_Local%N_Sample/2)+1)
     Time0 = Interf_Local%Time(int(Interf_Local%N_Sample/2)+1)
     Interf_Local%Opd(1:Interf_Local%N_Sample) =                 &
                  Interf_Local%Opd(1:Interf_Local%N_Sample) - Opd0
     Interf_Local%Time(1:Interf_Local%N_Sample) =                  &
                  Interf_Local%Time(1:Interf_Local%N_Sample) - Time0
!
     write(*,*) 'Interf Local N_Sample',Interf_Local%N_Sample
     write(*,*) 'Interf local Time',Interf_Local%Time(1)
     write(*,*) 'Interf local Time',Interf_Local%Time(Interf_Local%N_Sample)
     write(*,*) 'Interf local Opd ',Interf_Local%Opd(1)
     write(*,*) 'Interf local Opd ',Interf_Local%Opd(Interf_Local%N_Sample)
!
!    Cubic spline interpolation
!
!    initialisation
     dN1 = int(Resampling_Param%N_Sample/2) - 1
     dN2 = Resampling_Param%N_Sample - (dN1+1)
     NReSample = 0
     write(*,*) 'dN1, dN2',dN1, dN2
!
!    search for the first availlable sample
     N_Sample_first = 1
     do while( Interf_Local%Time(N_Sample_first) < &
               Interf_Samp_temp%Time(1)            )
        N_Sample_first = N_Sample_first + 1
     end do
     write(*,*) 'N_Sample_first :',N_Sample_first
!
!    search for the last availlable sample
     N_Sample_last = Interf_Local%N_Sample
     do while( Interf_Local%Time(N_Sample_last) >               &
               Interf_Samp_temp%Time(Interf_Samp_temp%N_Sample) )
        N_Sample_last = N_Sample_last - 1
     end do
     write(*,*) 'N_Sample_last :',N_Sample_last
!
!    first and last sample
     n1 = N_Sample_first
     n2 = N_Sample_last
     write(*,*) 'n1, n2',n1, n2
     call dichotomd( Interf_Samp_temp%Time(1),        &
                     Interf_Local%Time(n1:n2), n1, n2 )
     NR1 = n2
     n1 = N_Sample_first
     n2 = N_Sample_last
     write(*,*) 'n1, n2',n1, n2
     call dichotomd( Interf_Samp_temp%Time(Interf_Samp_temp%N_Sample),&
                     Interf_Local%Time(n1:n2), n1, n2                 )
     NR2 = n1
     write(*,*) 'NR1, NR2',NR1, NR2
     do NRs = NR1, NR2
!
!       equivalent "measured" sample
        n1 = 1
        n2 = Interf_Samp_temp%N_Sample
        call dichotomd( Interf_Local%Time(NRs),       &
                        Interf_Samp_temp%Time, n1, n2 )
        Ns = n1
        Ns1 = Ns-dN1
        Ns2 = Ns+dN2
!
!       case of extremity
        if( Ns1 <= 0 ) then
           Ns1 = 1 
           Ns2 = Ns1 + Resampling_Param%N_Sample - 1
        end if
        if( Ns2 > Interf_Samp_temp%N_Sample ) then
           Ns2 = Interf_Samp_temp%N_Sample
           Ns1 = Ns2 - Resampling_Param%N_Sample + 1
        end if

        NReSample = NReSample + 1
        call intspline( Resampling_Param%N_Sample, &
                        Interf_Samp_temp%Time(Ns1),       &
                        Interf_Samp_temp%Real_Part(Ns1),  &
                        1,                                &
                        Interf_Local%Time(NRs),           &
                        Interf_Local%Real_Part(NRs)       )
     end do
!
!    Resampled interferogram parameters
     Interf_ReSamp%Type     = Interf_Local%Type
     Interf_ReSamp%N_Sample = NR2-NR1+1
     Interf_ReSamp%N_Top    = 0
     Interf_ReSamp%dOpd     = Interf_Local%dOpd
     Interf_ReSamp%dTime    = Interf_ReSamp%dOpd      &
                            / (2.d+00*Interf_Param%VCC)
     Interf_ReSamp%OpdMax   = Interf_ReSamp%dOpd                 &
                            * dble(int(Interf_ReSamp%N_Sample/2))
     Interf_ReSamp%TimeMax  = Interf_ReSamp%OpdMax    &
                            / (2.d+00*Interf_Param%VCC)
     if( Resampling_Param%Field_Compensation == 'YES' ) then
        Interf_ReSamp%FieldMeanAngle = 0.0d+00
     else if( Resampling_Param%Field_Compensation == 'NO' ) then
        Interf_ReSamp%FieldMeanAngle = Interf_Samp%FieldMeanAngle
     end if
     call alloc_Interferogram( Interf_ReSamp )
     Interf_ReSamp%Real_Part(1:Interf_ReSamp%N_Sample) = &
                           Interf_Local%Real_Part(NR1:NR2)
     Interf_ReSamp%Time(1:Interf_ReSamp%N_Sample) =      &
                                Interf_Local%Time(NR1:NR2)
     Interf_ReSamp%Opd(1:Interf_ReSamp%N_Sample) =       &
                                 Interf_Local%Opd(NR1:NR2)
     write(*,*) 'Interf_Samp_temp%N_Sample :',&
                 Interf_Samp_temp%N_Sample
     write(*,*) 'Interf_Samp_Rpd%N_Top :',    &
                 Interf_Samp_Rpd%N_Top
     write(*,*) 'Interf_ReSamp%N_Sample :',   &
                 Interf_ReSamp%N_Sample
!
     call dalloc_Interferogram( Interf_Local )
     call dalloc_Interferogram( Interf_Samp_temp )
     call dalloc_Interferogram( Interf_Samp_Rpd_temp )
!
     return
   end subroutine interf_resampling_spline
!
!
   subroutine interf_resampling_lagrange( Resampling_Param,&
                                          Interf_Param,    &
                                          Zpd,             &
                                          Interf_Samp_Rpd, &
                                          Interf_Samp,     &
                                          Interf_ReSamp    )
   implicit none
     type(type_Resampling_Param), intent(in)            :: Resampling_Param
     type(type_Interf_Param)    , intent(in)            :: Interf_Param
     type(type_Zpd)             , intent(in)            :: Zpd
     type(type_Interferogram)   , intent(in)            :: Interf_Samp_Rpd
     type(type_Interferogram)   , intent(in)            :: Interf_Samp
     type(type_Interferogram)   , intent(out)           :: Interf_ReSamp
     type(type_Interferogram)                           :: Interf_Samp_temp
     type(type_Interferogram)                           :: Interf_Samp_Rpd_temp
     type(type_Interferogram)                           :: Interf_Local
     real(kind=DOUBLE)                                  :: Opd0
     real(kind=DOUBLE)                                  :: Time0
     integer(kind=LONG)                                 :: ErrCode
     integer(kind=LONG)                                 :: N_Lagr1
     integer(kind=LONG)                                 :: N_Lagr2
     integer(kind=LONG)                                 :: N_Lagr
     integer(kind=LONG)                                 :: N
     integer(kind=LONG)                                 :: NReSample
     integer(kind=LONG)                                 :: N_Samp_first
     integer(kind=LONG)                                 :: N_Samp_last
     integer(kind=LONG)                                 :: n1
     integer(kind=LONG)                                 :: n2
     integer(kind=LONG)                                 :: NR1
     integer(kind=LONG)                                 :: NR2
     integer(kind=LONG)                                 :: NRs
     integer(kind=LONG)                                 :: Ns
     integer(kind=LONG)                                 :: Ns1
     integer(kind=LONG)                                 :: Ns2
     real(kind=DOUBLE)     ,dimension(:),allocatable    :: Lagr
!
     write(*,*) 'resampling_lagrange'
     if( Interf_Samp%Type /= 'R' ) then
        write(*,*) 'Interf_Samp%Type Error',Interf_Samp%Type
        write(*,*) 'interf_resampling_lagrange Fatal Error'
        call exit(1)
     end if
     if( Interf_Samp_Rpd%Type /= 'RI' ) then
        write(*,*) 'Interf_Samp_Rpd%Type Error',Interf_Samp_Rpd%Type
        write(*,*) 'interf_resampling_lagrange Fatal Error'
        call exit(1)
     end if
!    measured interferogram transfert into temporary fields
     Interf_Samp_temp%Type     = Interf_Samp%Type
     Interf_Samp_temp%N_Sample = Interf_Samp%N_Sample
     Interf_Samp_temp%N_Top    = Interf_Samp%N_Top
     call alloc_Interferogram( Interf_Samp_temp )
     Interf_Samp_temp = Interf_Samp
!
!    metrology interferogram transfert into temporary fields
     Interf_Samp_Rpd_temp%Type     = Interf_Samp_Rpd%Type
     Interf_Samp_Rpd_temp%N_Sample = Interf_Samp_Rpd%N_Sample
     Interf_Samp_Rpd_temp%N_Top    = Interf_Samp_Rpd%N_Top
     call alloc_Interferogram( Interf_Samp_Rpd_temp )
     Interf_Samp_Rpd_temp = Interf_Samp_Rpd
!
     write(*,*) 'Interf_Samp_temp%N_Sample    ',Interf_Samp_temp%N_Sample
     write(*,*) 'Interf_Samp_Rpd_temp%N_Sample',Interf_Samp_Rpd_temp%N_Sample
     write(*,*) 'Interf_Samp_Rpd_temp%N_Top   ',Interf_Samp_Rpd_temp%N_Top
!
     if( Interf_Param%Rpd_Type == 'RI' ) then
!
!       extraction of the Opd(t) law
        write(*,'(a,i10)') 'initial Interf_Samp_temp%N_Sample    ',&
                                    Interf_Samp_temp%N_Sample
        write(*,'(a,i10)') 'initial Interf_Samp_Rpd_temp%N_Sample',&
                                    Interf_Samp_Rpd_temp%N_Sample
        do while( Interf_Samp_temp%N_Sample*Interf_Param%Rpd_Samp_OSFactor > &
                  Interf_Samp_Rpd_temp%N_Sample )
           Interf_Samp_temp%N_Sample = Interf_Samp_temp%N_Sample-1
        end do
        write(*,'(a,i10)') 'initial Interf_Samp_temp%N_Sample    ',&
                                    Interf_Samp_temp%N_Sample
        write(*,'(a,i10)') 'initial Interf_Samp_Rpd_temp%N_Sample',&
                                    Interf_Samp_Rpd_temp%N_Sample
        do while( Interf_Samp_temp%N_Sample*Interf_Param%Rpd_Samp_OSFactor < &
                  Interf_Samp_Rpd_temp%N_Sample )
           Interf_Samp_Rpd_temp%N_Sample = Interf_Samp_Rpd_temp%N_Sample-1
        end do
        write(*,'(a,i10)') 'final Interf_Samp_temp%N_Sample      ',&
                                  Interf_Samp_temp%N_Sample
        write(*,'(a,i10)') 'final Interf_Samp_Rpd_temp%N_Sample  ',&
                                  Interf_Samp_Rpd_temp%N_Sample
!
        Interf_Samp_temp%Opd(1:Interf_Samp_temp%N_Sample) =      &
             Interf_Samp_Rpd%Opd(1:Interf_Samp_Rpd_temp%N_Sample &
                                  :Interf_Param%Rpd_Samp_OSFactor)
        ! Rpd_Type=RI do not use NZPD value
        Opd0  = Interf_Samp_temp%Opd(int(Interf_Samp_temp%N_Sample/2)+1)
        Time0 = Interf_Samp_temp%Time(int(Interf_Samp_temp%N_Sample/2)+1)
!
     else if( Interf_Param%Rpd_Type == 'R' ) then
!
!       extraction of the Rpd Top
        Opd0  = Interf_Samp%Opd(Zpd%NZpd)
        Time0 = Interf_Samp%Time(Zpd%NZpd)
        Write(*,*) 'Sampling CLOCK/LZRPD nothing to do'
        write(*,*) 'End Interf_Samp_temp%N_Sample    ',&
                    Interf_Samp_temp%N_Sample
        write(*,*) 'End Interf_Samp_Rpd_temp%N_Top',&
                    Interf_Samp_Rpd_temp%N_Top
     else
        write(*,*) 'Interf_Param%Rpd_Type Error',Interf_Param%Rpd_Type
        call exit(1)
     end if
!
!    Field Angles Compensation
     if( Resampling_Param%Field_Compensation == 'YES' ) then
        write(*,*) 'Field Compensation  YES'
        Interf_Samp_temp%Opd(1:Interf_Samp%N_Sample) =            &
           ( Interf_Samp_temp%Opd(1:Interf_Samp%N_Sample) - Opd0 )&
           * dcos(Interf_Samp_temp%FieldMeanAngle)
        Interf_Samp_temp%Time(1:Interf_Samp%N_Sample) =             &
           ( Interf_Samp_temp%Time(1:Interf_Samp%N_Sample) - Time0 )&
           * dcos(Interf_Samp_temp%FieldMeanAngle)
     else if( Resampling_Param%Field_Compensation == 'NO' ) then
        write(*,*) 'Field Compensation  NO'
        Interf_Samp_temp%Opd(1:Interf_Samp%N_Sample) =           &
           ( Interf_Samp_temp%Opd(1:Interf_Samp%N_Sample) - Opd0 )
        Interf_Samp_temp%Time(1:Interf_Samp%N_Sample) =            &
           ( Interf_Samp_temp%Time(1:Interf_Samp%N_Sample) - Time0 )
     else
        write(*,*) 'Field Compensation parameter Error'
        write(*,*) 'interf_resampling_spline Fatal Error'
        call exit(1)
     end if
     write(*,*) 'Interf_Samp'
     write(*,*) 'Time(1)       ',Interf_Samp_temp%Time(1)
     write(*,*) 'Time(N_Sample)',Interf_Samp_temp%Time(Interf_Samp%N_Sample)
     write(*,*) 'Opd(1)        ',Interf_Samp_temp%Opd(1)
     write(*,*) 'Opd(N_Sample) ',Interf_Samp_temp%Opd(Interf_Samp%N_Sample)
     write(*,*) 'Time0         ',Time0
     write(*,*) 'Opd0          ',Opd0
!
!    Lagrange interpolation initialisation
     N_Lagr1 = -int(Resampling_Param%N_Sample/2)
     N_Lagr2 =  int(Resampling_Param%N_Sample/2)
     NReSample = 0
     write(*,*) 'N_Lagr1 N_Lagr2', N_Lagr1, N_Lagr2
!
!    temporary interpolated interferogram (theoretical OPD basis)
     Interf_Local%Type     = Interf_Samp%Type
     Interf_Local%N_Sample = 2 * int( Interf_Samp_Rpd%N_Sample / 2      &
                                    / Interf_Param%Rpd_Samp_OSFactor ) +1
     Interf_Local%dOpd     = Resampling_Param%dOpd
     Interf_Local%dTime    = Interf_Local%dOpd       &
                           / (2.d+00*Interf_Param%VCC)
     Interf_Local%OpdMax   = Interf_Local%dOpd          &
                           * int(Interf_Local%N_Sample/2)
     Interf_Local%TimeMax  = Interf_Local%OpdMax     &
                           / (2.d+00*Interf_Param%VCC)
     call alloc_Interferogram( Interf_Local )
     call Interferogram_Basis( Interf_Local )
     Opd0  = Interf_Local%Opd(int(Interf_Local%N_Sample/2)+1)
     Time0 = Interf_Local%Time(int(Interf_Local%N_Sample/2)+1)
     Interf_Local%Opd(1:Interf_Local%N_Sample) =                 &
                  Interf_Local%Opd(1:Interf_Local%N_Sample) - Opd0
     Interf_Local%Time(1:Interf_Local%N_Sample) =                  &
                  Interf_Local%Time(1:Interf_Local%N_Sample) - Time0
!
!    search for the first availlable sample
     N_Samp_first = 1
     do while( Interf_Local%Opd(N_Samp_first)      &
              +dble(2*N_Lagr1)*Interf_Local%dOpd < &
               Interf_Samp_temp%Opd(1)             )
        N_Samp_first = N_Samp_first + 1
     end do
     write(*,*) 'N_Samp_first :',N_Samp_first
!
!    search for the last availlable sample
     N_Samp_last = Interf_Local%N_Sample
     do while( Interf_Local%Opd(N_Samp_last)                   &
              +dble(2*N_Lagr2)*Interf_Local%dOpd  >            &
               Interf_Samp_temp%Opd(Interf_Samp_temp%N_Sample) )
        N_Samp_last = N_Samp_last - 1
     end do
     write(*,*) 'N_Samp_last :',N_Samp_last
!
!    first usefull sample of the input interferogram
     n1 = N_Samp_first
     n2 = N_Samp_last
     write(*,*) 'n1, n2',n1, n2
     call dichotomd( Interf_Samp_temp%Opd(1),        &
                     Interf_Local%Opd(n1:n2), n1, n2 )
     NR1 = n2
!
!    last usefull sample of the input interferogram
     n1 = N_Samp_first
     n2 = N_Samp_last
     write(*,*) 'n1, n2',n1, n2
     call dichotomd( Interf_Samp_temp%Opd(Interf_Samp_temp%N_Sample),&
                     Interf_Local%Opd(n1:n2), n1, n2                 )
     NR2 = n1
     write(*,*) 'NR1, NR2',NR1, NR2
     allocate( Lagr(N_Lagr1:N_Lagr2), stat=ErrCode)
!
     do NRs = NR1, NR2
!
!       Measured sample (Ns) closest to ideal sample (NRs)
        n1 = 1
        n2 = Interf_Samp_temp%N_Sample
        call dichotomd( Interf_Local%Opd(NRs),       &
                        Interf_Samp_temp%Opd, n1, n2 )
        Ns = n1
!
!       Lagrange limits
        Ns1 = Ns+N_Lagr1
        Ns2 = Ns+N_Lagr2
!
!       Lagrange interpolator
        do N_Lagr = N_Lagr1, N_Lagr2
           Lagr(N_Lagr) = 1.d+00
           do N = N_Lagr1, N_Lagr2
              if( N /= N_Lagr ) then
                 Lagr(N_Lagr) = Lagr(N_Lagr)         &
                  * ( Interf_Local%Opd(NRs)          &
                     -Interf_Samp_temp%Opd(Ns+N) )   &
                  / ( Interf_Samp_temp%Opd(Ns+N_Lagr)&
                     -Interf_Samp_temp%Opd(Ns+N) )
              end if
           end do
        end do
!
!       Interpolated sample
        Interf_Local%Real_Part(NRs) = sum(Interf_Samp_temp%Real_Part(Ns1:Ns2)&
                                         * Lagr(N_Lagr1:N_Lagr2))
        NReSample = NReSample + 1
     end do
     deallocate( Lagr )
!
!    Resampled interferogram parameters
     Interf_ReSamp%Type     = Interf_Local%Type
     Interf_ReSamp%N_Sample = NR2-NR1+1
     Interf_ReSamp%N_Top    = 0
     Interf_ReSamp%dOpd     = Interf_Local%dOpd
     Interf_ReSamp%dTime    = Interf_ReSamp%dOpd      &
                            / (2.d+00*Interf_Param%VCC)
     Interf_ReSamp%OpdMax   = Interf_ReSamp%dOpd                &
                            * dble(int(Interf_ReSamp%N_Sample/2))
     Interf_ReSamp%TimeMax  = Interf_ReSamp%OpdMax    &
                            / (2.d+00*Interf_Param%VCC)
     if( Resampling_Param%Field_Compensation == 'YES' ) then
        Interf_ReSamp%FieldMeanAngle = 0.0d+00
     else if( Resampling_Param%Field_Compensation == 'NO' ) then
        Interf_ReSamp%FieldMeanAngle = Interf_Samp%FieldMeanAngle
     end if
     call alloc_Interferogram( Interf_ReSamp )
     Interf_ReSamp%Real_Part(1:Interf_ReSamp%N_Sample) = &
                           Interf_Local%Real_Part(NR1:NR2)
     Interf_ReSamp%Time(1:Interf_ReSamp%N_Sample) =      &
                                Interf_Local%Time(NR1:NR2)
     Interf_ReSamp%Opd(1:Interf_ReSamp%N_Sample) =       &
                                 Interf_Local%Opd(NR1:NR2)
     write(*,*) 'Interf_Samp_temp%N_Sample :',&
                 Interf_Samp_temp%N_Sample
     write(*,*) 'Interf_Samp_Rpd%N_Top :',    &
                 Interf_Samp_Rpd%N_Top
     write(*,*) 'Interf_ReSamp%N_Sample :',   &
                 Interf_ReSamp%N_Sample
!
     call dalloc_Interferogram( Interf_Local )
     call dalloc_Interferogram( Interf_Samp_temp )
     call dalloc_Interferogram( Interf_Samp_Rpd_temp )
!
     return
   end subroutine interf_resampling_lagrange
!
!
!> interf_resampling_init -- Public
!!
!! * Purpose
!!
!!     Initialisation of interferogram resampling parameters
!!
!! * Description
!! 
!!     This subroutine allows to update the type dedicated to the interferogram resampling 
!!     parameters
!!
!! * Inputs
!!                  
!!     - Files_ReSamp_param : character / input file with definition of interferogram resampling
!!                            parameters
!!     - Interf_Param       : type_Interf_Param / type for declaration and allocation of 
!!                            interferogram
!!
!! * Inputs/outputs
!!
!!     - Resampling_Param : type_Resampling_Param / type for resampling parameters declaration and 
!!                          allocation.
!!
!! * Outputs
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.5.6
!!

   subroutine interf_resampling_init( Files_ReSamp_param, &
                                      Resampling_Param   )
   implicit none
     character(len=500)          ,intent(in)             :: Files_ReSamp_param
     type(type_Resampling_Param) ,intent(inout)          :: Resampling_Param
     integer(kind=LONG)                                  :: iFile
     integer(kind=LONG)                                  :: iPos
!
     iFile = 10
     iPos = 1
     write(*,'(a)') Files_ReSamp_param(1:len_trim(Files_ReSamp_param))
     open(iFile, file=Files_ReSamp_param, status='old', err=999)
     iPos = 2
     Resampling_Param%filename = Files_ReSamp_param
     read(iFile,'(a)',err=999) Resampling_Param%Sampling_Mode
     write(*,'(a,a)') 'Resampling_Param%Sampling_Mode',&
                       Resampling_Param%Sampling_Mode
     iPos = 3
     read(iFile,'(a)',err=999) Resampling_Param%Field_Compensation
     write(*,'(a,a)') 'Resampling_Param%Field_Compensation',&
                       Resampling_Param%Field_Compensation
     iPos = 4
     read(iFile,*,err=999) Resampling_Param%OSFactor
     write(*,*) 'Resampling_Param%OSFactor',Resampling_Param%OSFactor
     iPos = 5
     read(iFile,*,err=999) Resampling_Param%N_Sample
     write(*,*) 'Resampling_Param%N_Sample',&
                 Resampling_Param%N_Sample
     iPos = 8
     read(iFile,*,err=999) Resampling_Param%dOpd
     write(*,*) 'Resampling_Param%dOpd',&
                 Resampling_Param%dOpd
     iPos = 9
     read(iFile,*,err=999) Resampling_Param%Ns_Fft
     write(*,*) 'Resampling_Param%Ns_Fft',Resampling_Param%Ns_Fft
     iPos = 10
     read(iFile,*,err=999) Resampling_Param%Ns_Margin
     write(*,*) 'Resampling_Param%Ns_Margin',Resampling_Param%Ns_Margin
     iPos = 11
     close(iFile)
     Resampling_Param%OpdMax = dble(Resampling_Param%Ns_Fft) &
                             * Resampling_Param%dOpd / 2.d+00
     Resampling_Param%FieldMeanAngle = 0.d+00
     Resampling_Param%dWn = 1.d+00 / ( 2.d+00 * Resampling_Param%OpdMax )
     write(*,*) 'Resampling_Param%OpdMax',Resampling_Param%OpdMax
     write(*,*) 'Resampling_Param%dWn   ',Resampling_Param%dWn
!
     return
 999 write(*,*) 'interf_resampling_init Fatal error',iPos
    call exit(1)
   end subroutine interf_resampling_init
!
!
end module resampling_module
