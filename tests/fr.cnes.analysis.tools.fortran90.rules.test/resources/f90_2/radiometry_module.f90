!!# radiometry_module.f90 --
!!#
!!#           Project: SPS_GENERIC
!!#           Authors: NOVELTIS/B.TOURNIER
!!#              Date: october 2009
!!#           Version: $Revision: 1.9 $
!!# Last modification: $Date: 2011-06-22 10:06:26 $
!!#
!!# Language:  F90
!!# Standards: Noveltis
!!#
!!# --
!!#
!! 
!> radiometry_module -- Module
!!
!! * Purpose
!!
!!     Module for Radiometric performance computations
!!
!! * Description
!!      
!!     This module allows to compute radiometric performances. It computes radiometric 
!!     differences between a reference spectrum and a perturbed one. 
!!     The algorithm converts the two spectra in Brightness Temperature (BT) by using 
!!     the inverse Planck function, then the difference between reference and perturbed 
!!     BT are analysed in term of statistics. The module provides statistics on whole 
!!     spectral domain or by spectral classes for real and imaginary parts of the 
!!     difference. 
!!
!! * Sub-routines and functions
!!
!!     - analyse_radiometry : Computation of radiometric performance
!!
!! * References
!!
!!------------------------------------------------------------------------------------
! 21.09.12 (JD) radiometric_noise_calc added
! 28.09.12 (JD) radiometric_noise_correlation added
module radiometry_module
   use precision_type
   use error_type
   use constantes_type
   use spectrum_type
   use performance_type
   use diagonal_noise_type
   use matrix_noise_type
   use radiometry_error_type
   use radiometry_stats_type
   use math_module
   use statistique_module
   use plancklib_module
   use performance_module
!
   implicit none
!
!
   public :: analyse_radiometry           &
            ,radiometric_noise_calc       &
            ,radiometric_noise_correlation&
            ,error_spectrum               &
            ,error_radiometry             &
            ,stats_radiometry             &
            ,diagonal_noise_spectrum      &
            ,correlation_noise_matrix
!
!
   contains
!
!

!!
!!
!> analyse_radiometry -- Public
!!
!! * Purpose
!!
!!     Computation of radiometric performance.   
!!
!! * Description
!!
!!     This performance estimation subroutine derives radiometric performances by comparison 
!!     between one spectrum and a reference one. 
!!     It allows statistical analysis for the whole spectral domain and per spectral classes,
!!     both on the real or the imaginary spectrum parts of the differences. 
!!     
!! * Inputs 
!!
!!     - file_plot       : character / name of writing file with statistical results
!!     - File_Wn_Class   : character / name of file describing spectral classes
!!     - Spectrum        : type_Spectrum / type for declaration and allocation of spectrum 
!!     - Spectrum_ref    : type_Spectrum / type for declaration and allocation of spectrum 
!!
!! * Inputs/outputs
!!
!! * Outputs
!!
!! * References
!!

   subroutine analyse_radiometry( file_plot,    &
                                  File_Wn_Class,&
                                  Spectrum,     &
                                  Spectrum_ref  )
   implicit none
     character(len=*)        ,intent(in)                     :: file_plot
     character(len=*)        ,intent(in)                     :: File_Wn_Class
     type(type_Spectrum)     ,intent(in)                     :: Spectrum
     type(type_Spectrum)     ,intent(in)                     :: Spectrum_ref
     integer(kind=LONG)                                      :: iFile
     integer(kind=LONG)                                      :: Ns
     integer(kind=LONG)                                      :: n
     real(kind=DOUBLE)                                       :: TB0
     real(kind=DOUBLE)                                       :: TB
     real(kind=DOUBLE)                                       :: diff
     real(kind=DOUBLE)                                       :: SCal_R
     real(kind=DOUBLE)                                       :: SCal0_R
     real(kind=DOUBLE)                                       :: SCal_I
     real(kind=DOUBLE)                                       :: SCal0_I
     real(kind=DOUBLE)                                       :: Nedt_Avg
     real(kind=DOUBLE)                                       :: Nedt_Std
     real(kind=DOUBLE)                                       :: Nedt_Min
     real(kind=DOUBLE)                                       :: Nedt_Max
     integer(kind=LONG)                                      :: NsNedt_Min
     integer(kind=LONG)                                      :: NsNedt_Max
     real(kind=DOUBLE)                                       :: Nedl_Avg
     real(kind=DOUBLE)                                       :: Nedl_Std
     real(kind=DOUBLE)                                       :: Nedl_Min
     real(kind=DOUBLE)                                       :: Nedl_Max
     integer(kind=LONG)                                      :: NsNedl_Min
     integer(kind=LONG)                                      :: NsNedl_Max
     real(kind=DOUBLE)     ,dimension(:) ,allocatable        :: Nedl_R
     real(kind=DOUBLE)     ,dimension(:) ,allocatable        :: Nedl_I
     real(kind=DOUBLE)     ,dimension(:) ,allocatable        :: Nedt_R
     real(kind=DOUBLE)     ,dimension(:) ,allocatable        :: Nedt_I
     integer(kind=LONG)                                      :: Nb_Wn_Class
     real(kind=DOUBLE)     ,dimension(:) ,allocatable        :: Wn_Class
     integer(kind=LONG)                                      :: N_Class
     integer(kind=LONG)                                      :: NsStart
     integer(kind=LONG)                                      :: NsStop
     real(kind=DOUBLE)                                       :: PlankT0
     real(kind=DOUBLE)                                       :: derPlankT0
!
!    coherence control
     if( (Spectrum%Ns_First /= Spectrum_ref%Ns_First) .or.  &
         (Spectrum%Ns_Last  /= Spectrum_ref%Ns_Last )  ) then
        write(*,*) 'Spectral coherence Error',              &
                   Spectrum%Ns_First,Spectrum_ref%Ns_First, &
                   Spectrum%Ns_Last,Spectrum_ref%Ns_Last
        write(*,*) 'analyse_radiometry Fatal Error'
        call exit(1)
     end if
     if( Spectrum%Type /= Spectrum_ref%Type ) then
        write(*,*) 'Spectrum Type Error'
        call exit(1)
     end if
     allocate( Nedt_R(Spectrum%N_Sample) )
     allocate( Nedt_I(Spectrum%N_Sample) )
     allocate( Nedl_R(Spectrum%N_Sample) )
     allocate( Nedl_I(Spectrum%N_Sample) )
!
     iFile = 10
!
!    reading Wn_Class parameters
     open(iFile, file=File_Wn_Class, status='old',err=999)
     read(iFile,*,err=999) Nb_Wn_Class
     allocate( Wn_Class(Nb_Wn_Class+1) )
     do Ns = 1, Nb_Wn_Class+1
        read(iFile,*,err=999) Wn_Class(Ns)
     end do
     close(iFile)

     write(*,fmt='(a)') file_plot(1:len_trim(file_plot))
     open(iFile,file=file_plot,err=999)
     write(iFile,fmt='(a)',err=999) &
      '#Wn; S_Real; S_Imag; Nedt_R; Nedt_I; TB-TB0; TB0; TB; Nedl_R; Nedl_I '
     do Ns = Spectrum%Ns_First, Spectrum%Ns_Last
        n = Ns - Spectrum%Ns_First + 1
        if( Spectrum%Type == 'C' ) then
           SCal_R = dreal(Spectrum%Complex(n))
           SCal_I = dimag(Spectrum%Complex(n))
           SCal0_R = dreal(Spectrum_ref%Complex(n))
           SCal0_I = dimag(Spectrum_ref%Complex(n))
        else if( Spectrum%Type == 'RI' ) then
           SCal_R = Spectrum%Real_Part(n)
           SCal_I = Spectrum%Imag_Part(n)
           SCal0_R = Spectrum_ref%Real_Part(n)
           SCal0_I = Spectrum_ref%Imag_Part(n)
        else if( Spectrum%Type == 'R' ) then
           SCal_R = Spectrum%Real_Part(n)
           SCal_I = 0.d+00
           SCal0_R = Spectrum_ref%Real_Part(n)
           SCal0_I = 0.d+00
        else
           write(*,*) 'Spectrum Type Error must be C RI R'
           call exit(1)
        end if
        call plkderive( Cst_T_Ref, Spectrum%Wn(n),  PlankT0, derPlankT0 )
        call plkinverse( TB0, Spectrum%Wn(n), SCal0_R )
        call plkinverse( TB,  Spectrum%Wn(n), SCal_R  )
        diff      = SCal_R - SCal0_R
        Nedl_R(n) = diff
        Nedt_R(n) = diff/derPlankT0
        diff      = SCal_I - SCal0_I
        Nedl_I(n) = diff
        Nedt_I(n) = diff/derPlankT0
        write(iFile,9999,err=999)               &
                          Spectrum%Wn(n)/100. , &
                          SCal0_R*100.,         &
                          SCal0_I*100.,         &
                          Nedt_R(n),            &
                          Nedt_I(n),            &
                          TB-TB0,               &
                          TB0,                  &
                          TB,                   &
                          Nedl_R(n)*100.,       &
                          Nedl_I(n)*100.
     end do
     call statistics( Spectrum%N_Sample,&
                      Nedt_R,           &
!
                      Nedt_Avg,         &
                      Nedt_Std,         &
                      Nedt_Min,         &
                      Nedt_Max,         &
                      NsNedt_Min,       &
                      NsNedt_Max        )
     write(iFile,'(a16,4e12.4,2i6)',err=999) '# Nedt Real Part' &
         ,Nedt_Avg,Nedt_Std,Nedt_Min,Nedt_Max              &
         ,NsNedt_Min,NsNedt_Max
     call statistics( Spectrum%N_Sample,&
                      Nedt_I,           &
!
                      Nedt_Avg,         &
                      Nedt_Std,         &
                      Nedt_Min,         &
                      Nedt_Max,         &
                      NsNedt_Min,       &
                      NsNedt_Max        )
     write(iFile,'(a16,4e12.4,2i6)',err=999) '# Nedt Imag Part' &
         ,Nedt_Avg,Nedt_Std,Nedt_Min,Nedt_Max              &
         ,NsNedt_Min,NsNedt_Max
     call statistics( Spectrum%N_Sample,&
                      Nedl_R,           &
!
                      Nedl_Avg,         &
                      Nedl_Std,         &
                      Nedl_Min,         &
                      Nedl_Max,         &
                      NsNedl_Min,       &
                      NsNedl_Max        )
     write(iFile,'(a16,4e12.4,2i6)',err=999) '# Nedl Real Part' &
         ,Nedl_Avg*100.,Nedl_Std*100.,Nedl_Min*100.,Nedl_Max*100.&
         ,NsNedl_Min,NsNedl_Max
     call statistics( Spectrum%N_Sample,&
                      Nedl_I,           &
!
                      Nedl_Avg,         &
                      Nedl_Std,         &
                      Nedl_Min,         &
                      Nedl_Max,         &
                      NsNedl_Min,       &
                      NsNedl_Max        )
     write(iFile,'(a16,4e12.4,2i6)',err=999) '# Nedl Imag Part' &
         ,Nedl_Avg*100.,Nedl_Std*100.,Nedl_Min*100.,Nedl_Max*100.&
         ,NsNedl_Min,NsNedl_Max
!
!    statistical analysis per spectral classes
     NsStart = 1
     do N_Class = 1, Nb_Wn_Class
        do while( Spectrum%Wn(NsStart) < Wn_Class(N_Class) .and. &
                  NsStart <  Spectrum%N_Sample )
           NsStart = NsStart + 1
        end do
        NsStop = NsStart
        do while( Spectrum%Wn(NsStop) < Wn_Class(N_Class+1) .and. &
                  NsStop <  Spectrum%N_Sample )
           NsStop = NsStop + 1
        end do
        if( NsStart /= NsStop ) then
           call statistics( NsStop-NsStart+1,&
                            Nedt_R(NsStart), &
!
                            Nedt_Avg,        &
                            Nedt_Std,        &
                            Nedt_Min,        &
                            Nedt_Max,        &
                            NsNedt_Min,      &
                            NsNedt_Max       )
           write(iFile,'(a16,f10.3,4e12.4,2i6)',err=999) '# Nedt Real Part', &
                (Wn_Class(N_Class)+Wn_Class(N_Class+1))/200.d+00, &
                Nedt_Avg,Nedt_Std,Nedt_Min,Nedt_Max,              &
                NsNedt_Min,NsNedt_Max
        end if
     end do
     NsStart = 1
     do N_Class = 1, Nb_Wn_Class
        do while( Spectrum%Wn(NsStart) < Wn_Class(N_Class) .and. &
                  NsStart <  Spectrum%N_Sample )
           NsStart = NsStart + 1
        end do
        NsStop = NsStart
        do while( Spectrum%Wn(NsStop) < Wn_Class(N_Class+1) .and. &
                  NsStop <  Spectrum%N_Sample )
           NsStop = NsStop + 1
        end do
        if( NsStart /= NsStop ) then
           call statistics( NsStop-NsStart+1,&
                            Nedt_I(NsStart), &
!
                            Nedt_Avg,        &
                            Nedt_Std,        &
                            Nedt_Min,        &
                            Nedt_Max,        &
                            NsNedt_Min,      &
                            NsNedt_Max       )
           write(*,'(a30,3i6,f10.0)')   'N_Class,NsStart,NsStop,Wn',&
                        N_Class,NsStart,NsStop,           &
                      (Wn_Class(N_Class)+Wn_Class(N_Class+1))/200.d+00
           write(iFile,'(a16,f10.3,4e12.4,2i6)',err=999) '# Nedt Imag Part', &
                (Wn_Class(N_Class)+Wn_Class(N_Class+1))/200.d+00, &
                Nedt_Avg,Nedt_Std,Nedt_Min,Nedt_Max,              &
                NsNedt_Min,NsNedt_Max
        end if
     end do
!
!    statistical analysis per spectral classes
     NsStart = 1
     do N_Class = 1, Nb_Wn_Class
        do while( Spectrum%Wn(NsStart) < Wn_Class(N_Class) .and. &
                  NsStart <  Spectrum%N_Sample )
           NsStart = NsStart + 1
        end do
        NsStop = NsStart
        do while( Spectrum%Wn(NsStop) < Wn_Class(N_Class+1) .and. &
                  NsStop <  Spectrum%N_Sample )
           NsStop = NsStop + 1
        end do
        if( NsStart /= NsStop ) then
           call statistics( NsStop-NsStart+1,&
                            Nedl_R(NsStart), &
!
                            Nedl_Avg,        &
                            Nedl_Std,        &
                            Nedl_Min,        &
                            Nedl_Max,        &
                            NsNedl_Min,      &
                            NsNedl_Max       )
           write(iFile,'(a16,f10.3,4e12.4,2i6)',err=999) '# Nedl Real Part', &
                (Wn_Class(N_Class)+Wn_Class(N_Class+1))/200.d+00, &
                Nedl_Avg*100.,Nedl_Std*100.,Nedl_Min*100.,Nedl_Max*100.,&
                NsNedl_Min,NsNedl_Max
        end if
     end do
     NsStart = 1
     do N_Class = 1, Nb_Wn_Class
        do while( Spectrum%Wn(NsStart) < Wn_Class(N_Class) .and. &
                  NsStart <  Spectrum%N_Sample )
           NsStart = NsStart + 1
        end do
        NsStop = NsStart
        do while( Spectrum%Wn(NsStop) < Wn_Class(N_Class+1) .and. &
                  NsStop <  Spectrum%N_Sample )
           NsStop = NsStop + 1
        end do
        if( NsStart /= NsStop ) then
           call statistics( NsStop-NsStart+1,&
                            Nedl_I(NsStart), &
!
                            Nedl_Avg,        &
                            Nedl_Std,        &
                            Nedl_Min,        &
                            Nedl_Max,        &
                            NsNedl_Min,      &
                            NsNedl_Max       )
           write(*,'(a30,3i6,f10.0)')   'N_Class,NsStart,NsStop,Wn',&
                        N_Class,NsStart,NsStop,           &
                      (Wn_Class(N_Class)+Wn_Class(N_Class+1))/200.d+00
           write(iFile,'(a16,f10.3,4e12.4,2i6)',err=999) '# Nedl Imag Part', &
                (Wn_Class(N_Class)+Wn_Class(N_Class+1))/200.d+00, &
                Nedl_Avg*100.,Nedl_Std*100.,Nedl_Min*100.,Nedl_Max*100.,&
                NsNedl_Min,NsNedl_Max
        end if
     end do
     close(iFile)
!
     deallocate( Nedl_R )
     deallocate( Nedl_I )
     deallocate( Nedt_R )
     deallocate( Nedt_I )
     deallocate( Wn_Class )
     return
999  write(*,*) 'write file plot Error'
     write(*,*) 'analyse_radiometry Fatal Error'
     call exit(1)
9999 format (1h ,30e21.12)
   end subroutine analyse_radiometry
!!
!!
!> radiometric_noise_calc -- Public
!!
!! * Purpose
!!
!!     Radiometric noise calculation.   
!!
!! * Description
!!
!!     The radiometric noise spectrum is calculated by using a list of spectra. 
!!     
!! * Inputs 
!!
!!     - Nlist           : number of spectra into the list
!!     - Nsample         : number of sample of the spectrum
!!     - Spectrum        : type spectrum measured spectra Spectrum(Nlist)
!!     - Date            : time of the measurement of the spectra Date(Nlist)
!!     - deg_polynom     : degree-order of the polynomial to fit the temporal drift
!!     - SlideFilter     : half width of the moving average window
!!
!! * Inputs/outputs
!!
!! * Outputs
!!     
!!     - NoiseRadSpect       : spectrum of noise in radiances - NoiseRadSpect(Nsample)
!!     - NoiseNedTSpect      : spectrum of noise in NEdT at Cst_T_Ref - NoiseNedTSpect(Nsample)
!!     - DriftT              : temporal drift in NEdT at Cst_T_Ref - DriftT(Nsample)
!!     - MeanSpectrum        : mean spectrum in radiances - MeanSpectrum(Nsample)
!!     - SmoothNoiseRadSpect : smoothed spectrum of noise in radiances - SmoothNoiseRadSpect(Nsample)
!!     - SmoothNoiseNedTSpect: smoothed spectrum of noise in NEdT at Cst_T_Ref - SmoothNoiseNedTSpect(Nsample)
!!     - SmoothDriftT        : smoothed temporal drift in NEdT at Cst_T_Ref - SmoothDriftT(Nsample)
!!     - SmoothMeanSpectrum  : smoothed mean spectrum in radiances - SmoothMeanSpectrum(Nsample)
!!     - ErrCode             : return error code
!!
!! * References
!!
!!   SPS reference document
!
   subroutine radiometric_noise_calc &
        ( Nlist,                     &
        Nsample,                     &
        Spectrum,                    &
        Date,                        &
        deg_polynom,                 &
        SlideFilter,                 &
        NoiseRadSpect,               &
        NoiseNedTSpect,              &
        DriftT,                      &
        MeanSpectrum,                &
        SmoothNoiseRadSpect,         &
        SmoothNoiseNedTSpect,        &
        SmoothDriftT,                &
        SmoothMeanSpectrum,          &
        ErrCode )
     
     use precision_type
     use constantes_type
     use math_module
     use plancklib_module

     integer(kind=LONG),                    intent(in) :: Nlist
     integer(kind=LONG),                    intent(in) :: Nsample
     type(type_Spectrum), dimension(Nlist), intent(in) :: Spectrum
     real(kind=DOUBLE),   dimension(Nlist), intent(in) :: Date
     integer(kind=LONG),                    intent(in) :: deg_polynom
     integer(kind=LONG),                    intent(in) :: SlideFilter
     real(kind=DOUBLE), dimension(Nsample), intent(out):: NoiseRadSpect 
     real(kind=DOUBLE), dimension(Nsample), intent(out):: NoiseNedTSpect
     real(kind=DOUBLE), dimension(Nsample), intent(out):: DriftT
     real(kind=DOUBLE), dimension(Nsample), intent(out):: MeanSpectrum
     real(kind=DOUBLE), dimension(Nsample), intent(out):: SmoothNoiseRadSpect
     real(kind=DOUBLE), dimension(Nsample), intent(out):: SmoothNoiseNedTSpect
     real(kind=DOUBLE), dimension(Nsample), intent(out):: SmoothDriftT
     real(kind=DOUBLE), dimension(Nsample), intent(out):: SmoothMeanSpectrum
     integer(kind=LONG),                    intent(out):: ErrCode      
     integer(kind=LONG)  :: List
     integer(kind=LONG)  :: Ns
     real(kind=DOUBLE)   :: rmsfit
     real(kind=DOUBLE)   :: sumResidual
     real(kind=DOUBLE)   :: SpectResidual
     real(kind=DOUBLE)   :: PlankT0
     real(kind=DOUBLE)   :: derPlankT0
     real(kind=DOUBLE), dimension(:), allocatable :: Weight 
     real(kind=DOUBLE), dimension(:), allocatable :: Spectrum_tmp  
     real(kind=DOUBLE), dimension(:), allocatable :: Spect 
     !
     write (*,*) 'radiometric_noise_calc'
     NoiseRadSpect(:) = 0_DOUBLE
     NoiseNedTSpect(:) = 0_DOUBLE
     DriftT(:) = 0_DOUBLE 
     MeanSpectrum(:) = 0_DOUBLE
     SmoothNoiseRadSpect(:) = 0_DOUBLE
     SmoothNoiseNedTSpect(:) = 0_DOUBLE
     SmoothDriftT(:) = 0_DOUBLE
     SmoothMeanSpectrum(:) = 0_DOUBLE
     ErrCode = 0
     ! weight initialisation 
     allocate ( Weight(Nlist), stat=ErrCode )
     if( ErrCode /= 0 ) then
        write(0,*) 'allocation Weight Error'
        return
     end if
     Weight(:) = 1_DOUBLE
     ! loop on the spectrum samples
     do Ns = 1, Spectrum(1)%N_Sample  
        ! temporal drift calculation
        ! by fitting the spectrum with a degree-order polynomial 
        allocate ( Spectrum_tmp(Nlist), stat=ErrCode )
        allocate ( Spect(Nlist), stat=ErrCode )
        if( ErrCode /= 0 ) then
           write(0,*) 'allocation Spectrum Error'
           return
        end if
        do List = 1, Nlist
           Spectrum_tmp(List) = Spectrum(List)%Real_Part(Ns)
        end do
        rmsfit=0_DOUBLE
        call intpolynome( deg_polynom,Nlist,Date,Spectrum_tmp,Weight, &
             Nlist,Date,Spect,MeanSpectrum(Ns),rmsfit,ErrCode )
        if( ErrCode /= 0 ) then
           write(0,*) 'Warnning temporal drift PolyLeastsqr fit abort'
           call exit(1)
        end if
        deallocate (Spectrum_tmp)
        !  write(*,*) ' rms', rmsfit 
        sumResidual= 0_DOUBLE
        do List = 1, Nlist
           SpectResidual  = Spectrum(List)%Real_Part(Ns) - Spect(List)
           sumResidual = sumResidual + SpectResidual**2
        end do
        ! spectrum of noise
        NoiseRadSpect(Ns) = dsqrt( sumResidual/dble(Nlist) )
        ! conversion in NEdt a T0
        call plkderive( Cst_T_Ref,Spectrum(1)%Wn(Ns),PlankT0,derPlankT0 )
        NoiseNedTSpect(Ns) = NoiseRadSpect(Ns) / derPlankT0
        ! temporal drift during the sequence (1:Nlist)
        DriftT(Ns) = Spect(Nlist)-Spect(1)
        DriftT(Ns) = DriftT(Ns) / derPlankT0
        deallocate( Spect )
     end do
     ! smoothed spectrum
     call moving_average (NoiseRadSpect, Spectrum(1)%N_Sample,SlideFilter, &
          SmoothNoiseRadSpect)
     call moving_average (NoiseNedTSpect, Spectrum(1)%N_Sample,SlideFilter, &
          SmoothNoiseNedTSpect)
     call moving_average (DriftT, Spectrum(1)%N_Sample,SlideFilter, &
          SmoothDriftT)
     call moving_average (MeanSpectrum, Spectrum(1)%N_Sample,SlideFilter, &
          SmoothMeanSpectrum)

     return

   end subroutine radiometric_noise_calc
!!
!!
!> radiometric_noise_correlation -- Public
!!
!! * Purpose
!!
!!     Radiometric noise correlation matrix calculation.   
!!
!! * Description
!!
!!     The radiometric noise correlation matrix between spectral samples is calculated.
!!     The spectral correlation noise is also calculated. 
!!     
!! * Inputs 
!!
!!     - Nlist           : number of spectra into the list
!!     - Nsample         : number of sample of the spectrum
!!     - Spectrum        : type spectrum measured spectra Spectrum(Nlist)
!!     - Noise           : spectrum of the radiometric noise Noise(Nsample)
!!
!! * Inputs/outputs
!!
!! * Outputs
!!     
!!     - r              : correlation matrix of the radiometric noise - r(Nlist(Nlist+1))
!!     - CorrNoiseSpect : spectrum of the correlated noise - CorrNoiseSpect(Nsample)
!!     - ErrCode        : return error code
!!
!! * References
!!
!!   SPS reference document
!
   subroutine radiometric_noise_correlation &
        ( Nlist,                     &
        Nsample,                     &
        Spectrum,                    &
        Noise,                       &
        r,                           &
        CorrNoiseSpect,              &
        ErrCode )
     
     use precision_type
     use constantes_type
     use math_module
     use plancklib_module

     integer(kind=LONG),                    intent(in) :: Nlist
     integer(kind=LONG),                    intent(in) :: Nsample
     type(type_Spectrum), dimension(Nlist), intent(in) :: Spectrum
     real(kind=DOUBLE), dimension(Nsample), intent(in) :: Noise
     real(kind=DOUBLE), dimension(Nsample*(Nsample+1)/2), intent(out):: r
     real(kind=DOUBLE), dimension(Nsample*(Nsample+1)/2), intent(out):: CorrNoiseSpect
     integer(kind=LONG),                    intent(out):: ErrCode      
     integer(kind=LONG)  :: List
     integer(kind=LONG)  :: Ns,Nss,index
     real(kind=DOUBLE)   :: MeanNoise
     real(kind=DOUBLE), dimension(:), allocatable :: Spectrum_tmp
     real(kind=DOUBLE), dimension(:,:), allocatable :: Cnoise 
     real(kind=DOUBLE), dimension(:), allocatable :: CnoiseDiag
     real(kind=DOUBLE), dimension(:), allocatable :: CnoiseCross
     !
     r(:) = 0_DOUBLE
     CorrNoiseSpect(:) = 0_DOUBLE
     ! loop on the spectrum samples
     allocate ( Cnoise(Nsample,Nlist), stat=ErrCode )
     if( ErrCode /= 0 ) then
        write(0,*) 'allocation Cnoise Error'
        return
     end if
     do Ns = 1, Spectrum(1)%N_Sample  
        ! calculate centred noise
        allocate ( Spectrum_tmp(Nlist), stat=ErrCode )
        if( ErrCode /= 0 ) then
           write(0,*) 'allocation Spectrum Error'
           return
        end if
        do List = 1, Nlist
           Spectrum_tmp(List) = Spectrum(List)%Real_Part(Ns)
        end do
        call average(Nlist,Spectrum_tmp,MeanNoise)
        deallocate (Spectrum_tmp)
        do List = 1, Nlist
          Cnoise(Ns,List)  = Spectrum(List)%Real_Part(Ns) - MeanNoise
        end do
     end do
     ! correlation matrix calculation
     ! diagonal products  
     allocate ( CnoiseDiag(Spectrum(1)%N_Sample), stat=ErrCode )
     if( ErrCode /= 0 ) then
        write(0,*) 'allocation Cdiag Error'
        return
     end if
     CnoiseDiag(:) = 0_DOUBLE
     do List = 1, Nlist
        do Ns = 1, Spectrum(1)%N_Sample
           CnoiseDiag(Ns) = CnoiseDiag(Ns) + Cnoise(Ns,List)**2
        end do
     end do
     ! cross products  
     allocate ( CnoiseCross(Spectrum(1)%N_Sample*(Spectrum(1)%N_Sample+1)/2), stat=ErrCode )
     if( ErrCode /= 0 ) then
        write(0,*) 'allocation Cdiag Error'
        return
     end if
     CnoiseCross(:) = 0_DOUBLE
     do List = 1, Nlist
        index = 0
        do Ns = 1, Spectrum(1)%N_Sample
           do Nss = 1, Ns
              index = index + 1
              CnoiseCross(index) = CnoiseCross(index) + Cnoise(Ns,List)*Cnoise(Nss,List)
           end do
        end do
     end do
     deallocate( Cnoise ) 
     ! correlation matrix (vector) and spectrally correlated noise     
     do List = 1, Nlist
        index = 0
        do Ns = 1, Spectrum(1)%N_Sample
           do Nss = 1, Ns
              index = index + 1
              if ( CnoiseDiag(Ns) > 0 .and. CnoiseCross(index) > 0 ) then
                 r(index) = CnoiseCross(index) / dsqrt( CnoiseDiag(Ns) * CnoiseDiag(Nss))
              else
                 r(index) = 0_DOUBLE 
              end if
              CorrNoiseSpect(index) = Noise(Ns)*r(index)
           end do
        end do
     end do
     deallocate( CnoiseDiag )
     deallocate( CnoiseCross )

     return

   end subroutine radiometric_noise_correlation
!
!
   subroutine error_spectrum(  Performance_Param,&
                               Nb_per,           &
                               Spectrum_per,     &
                               Nb_nom,           &
                               Spectrum_nom,     &
                               File_Wn_Class,    &
                               Spectrum_Error,   &
                               Spectrum_Stats    )
   type(type_Performance)   ,intent(in)                  :: Performance_Param
   integer(kind=LONG)       ,intent(in)                     :: Nb_per
   type(type_Spectrum)      ,intent(inout),dimension(Nb_per):: Spectrum_per
   integer(kind=LONG)       ,intent(in)                     :: Nb_nom
   type(type_Spectrum)      ,intent(inout),dimension(Nb_nom):: Spectrum_nom
   character(len=*)         ,intent(in)                     :: File_Wn_Class
   type(type_Radiometry_Error),intent(out),dimension(:),allocatable &
                                                         :: Spectrum_Error
   type(type_Radiometry_Stats),intent(out)               :: Spectrum_Stats
!
   type(type_Spectrum)                                   :: Spectrum_per_avg
   type(type_Spectrum)                                   :: Spectrum_nom_avg
   integer(kind=LONG)                                    :: ErrCode
   integer(kind=LONG)                                    :: Nb
   integer(kind=LONG)                                    :: iPos
   integer(kind=LONG)                                    :: ioalloc
!
   ErrCode = 0
   ioalloc = 0
   iPos = 1
   allocate( Spectrum_Error(Nb_Per), stat=ErrCode )
   if( ErrCode /= 0 ) ioalloc = ioalloc +1
   if( ioalloc /= 0 ) then
      write(*,*) 'Allocation Error',ioalloc
      go to 999
   end if
!
   if( trim(Performance_Param%Comparison) == 'Collect_Collect' ) then
      if( Nb_per /= Nb_nom ) then
         write(*,*) 'Collections size must be equal :',Nb_per, Nb_nom
         go to 999
      end if
      do Nb = 1, Nb_per
!
!        compute the statistics per spectrum
         call error_radiometry( File_Wn_Class,     &
                                Spectrum_per(Nb),  &
                                Spectrum_nom(Nb),  &
                                Spectrum_Error(Nb) )
         call dalloc_Spectrum( Spectrum_per(Nb) )
         call dalloc_Spectrum( Spectrum_nom(Nb) )
      end do
!
!     compute the statistics over the collection
      call stats_radiometry( Nb_per,        &
                             Spectrum_Error,&
                             Spectrum_Stats )
   else if( trim(Performance_Param%Comparison) == 'Collect_One' ) then
      do Nb = 1, Nb_per
!
!        compute the statistics per spectrum
         call error_radiometry( File_Wn_Class,     &
                                Spectrum_per(Nb),  &
                                Spectrum_nom(1),   &
                                Spectrum_Error(Nb) )
         call dalloc_Spectrum( Spectrum_per(Nb) )
      end do
      call dalloc_Spectrum( Spectrum_nom(1) )
!
!     compute the statistics over the collection
      call stats_radiometry( Nb_per,        &
                             Spectrum_Error,&
                             Spectrum_Stats )
   else if( trim(Performance_Param%Comparison) == 'Collect_Average' ) then
!
!     Compute the average of the nominal collection
      call average_spectrum( Nb_nom,          &
                             Spectrum_nom,    &
                             Spectrum_nom_avg )
      do Nb = 1, Nb_per
!
!        compute the statistics per spectrum
         call error_radiometry( File_Wn_Class,     &
                                Spectrum_per(Nb),  &
                                Spectrum_nom_avg,  &
                                Spectrum_Error(Nb) )
         call dalloc_Spectrum( Spectrum_per(Nb) )
      end do
      call dalloc_Spectrum( Spectrum_nom_avg )
!
!     compute the statistics over the collection
      call stats_radiometry( Nb_per,        &
                             Spectrum_Error,&
                             Spectrum_Stats )
!
   else if( trim(Performance_Param%Comparison) == 'Collect_None' ) then
      do Nb = 1, Nb_per
!
!        set Spectrum_nom to 0
         if( Nb == 1 ) then
            call dalloc_Spectrum( Spectrum_nom(1) )
            call Spectrum_Header_Transfer( Spectrum_per(1), Spectrum_nom(1) )
         end if
!
!        compute the statistics per spectrum
         call error_radiometry( File_Wn_Class,     &
                                Spectrum_per(Nb),  &
                                Spectrum_nom(1),   &
                                Spectrum_Error(Nb) )
         call dalloc_Spectrum( Spectrum_per(Nb) )
      end do
      call dalloc_Spectrum( Spectrum_nom(1) )
!
!     compute the statistics over the collection
      call stats_radiometry( Nb_per,        &
                             Spectrum_Error,&
                             Spectrum_Stats )
!
   else if( trim(Performance_Param%Comparison) == 'Average_Average' ) then
!
!     compute average of the nominal collection
      call average_spectrum( Nb_nom,          &
                             Spectrum_nom,    &
                             Spectrum_nom_avg )
!
!     compute average of the perturb collection
      call average_spectrum( Nb_per,          &
                             Spectrum_per,    &
                             Spectrum_per_avg )
!
!     compute the statistics for averaged spectrum
      call error_radiometry( File_Wn_Class,    &
                             Spectrum_per_avg, &
                             Spectrum_nom_avg, &
                             Spectrum_Error(1) )
      call dalloc_Spectrum( Spectrum_per_avg )
      call dalloc_Spectrum( Spectrum_nom_avg )
!
   else if( trim(Performance_Param%Comparison) == 'One_One' ) then
!
!     compute the statistics per spectrum
      call error_radiometry( File_Wn_Class,    &
                             Spectrum_per(1),  &
                             Spectrum_nom(1),  &
                             Spectrum_Error(1) )
      call dalloc_Spectrum( Spectrum_per(1) )
      call dalloc_Spectrum( Spectrum_nom(1) )
!
   else if( trim(Performance_Param%Comparison) == 'One_None' ) then
!
!     set Spectrum_nom to 0
      call dalloc_Spectrum( Spectrum_nom(1) )
      call Spectrum_Header_Transfer( Spectrum_per(1), Spectrum_nom(1) )
!
!     compute the statistics per spectrum
      call error_radiometry( File_Wn_Class,    &
                             Spectrum_per(1),  &
                             Spectrum_nom(1),  &
                             Spectrum_Error(1) )
      call dalloc_Spectrum( Spectrum_per(1) )
      call dalloc_Spectrum( Spectrum_nom(1) )
   else
      write(*,*) 'Wrong Comparison Mode : ',Performance_Param%Comparison
      go to 999
   end if
   
   return
999 write(*,*) 'spectrum_error Fatal error',iPos
   call exit(1)
!
   end subroutine error_spectrum
!
!
   subroutine error_radiometry( File_Wn_Class, &
                                Spectrum,      &
                                Spectrum_ref,  &
                                Spectrum_Error )
   implicit none
     character(len=*)           ,intent(in)                  :: File_Wn_Class
     type(type_Spectrum)        ,intent(in)                  :: Spectrum
     type(type_Spectrum)        ,intent(in)                  :: Spectrum_ref
     type(type_Radiometry_Error),intent(out)                 :: Spectrum_Error
!
     integer(kind=LONG)                                      :: iFile
     integer(kind=LONG)                                      :: Ns
     integer(kind=LONG)                                      :: n
     integer(kind=LONG)                                      :: Nb_Wn_Class
     real(kind=DOUBLE)     ,dimension(:) ,allocatable        :: Wn_Class
     integer(kind=LONG)                                      :: N_Class
     integer(kind=LONG)                                      :: NsStart
     integer(kind=LONG)                                      :: NsStop
     real(kind=DOUBLE)                                       :: PlankT0
     real(kind=DOUBLE)                                       :: derPlankT0
!
!    coherence control
     if( (Spectrum%Ns_First /= Spectrum_ref%Ns_First) .or.  &
         (Spectrum%Ns_Last  /= Spectrum_ref%Ns_Last )  ) then
        write(*,*) 'Spectral coherence Error',              &
                   Spectrum%Ns_First,Spectrum_ref%Ns_First, &
                   Spectrum%Ns_Last,Spectrum_ref%Ns_Last
        write(*,*) 'radiometry_error Fatal Error'
        go to 999
     end if
     if( Spectrum%Type /= Spectrum_ref%Type ) then
        write(*,*) 'Spectrum Type Error must be the same'
        go to 999
     end if
!
     iFile = 10
!
!    reading Wn_Class parameters
     open(iFile, file=File_Wn_Class, status='old',err=999)
     read(iFile,*,err=999) Nb_Wn_Class
     allocate( Wn_Class(Nb_Wn_Class+1) )
     do Ns = 1, Nb_Wn_Class+1
        read(iFile,*,err=999) Wn_Class(Ns)
     end do
     close(iFile)
!
!    Spectrum header transfert
     if( Spectrum%Type == 'C' ) then
        Spectrum_Error%Type = 'RI'
     else if( Spectrum%Type == 'RI' .or. &
              Spectrum%Type == 'R'  .or. &
              Spectrum%Type == 'I' ) then
!       Nothing to do
        Spectrum_Error%Type = Spectrum%Type
     else
         write(*,*) 'Spectrum Type Error must be C RI R I'
         go to 999
     end if
     Spectrum_Error%N_Sample = Spectrum%N_Sample
     Spectrum_Error%Ns_First = Spectrum%Ns_First
     Spectrum_Error%Ns_Last  = Spectrum%Ns_Last
     Spectrum_Error%Wn_First = Spectrum%Wn_First
     Spectrum_Error%Wn_Last  = Spectrum%Wn_Last
     Spectrum_Error%WnMax    = Spectrum%WnMax
     Spectrum_Error%dWn      = Spectrum%dWn
     Spectrum_Error%Nb_Wn_Class = Nb_Wn_Class
     call alloc_Radiometry_Error ( Spectrum_Error )
!
!    wavenumber basis 
     Spectrum_Error%Wn(1:Spectrum_Error%N_Sample) = &
          Spectrum%Wn(1:Spectrum%N_Sample)
     Spectrum_Error%Wn_Class(1:Nb_Wn_Class) = Wn_Class(1:Nb_Wn_Class)
!
!    compute spectrum errors
     do Ns = Spectrum%Ns_First, Spectrum%Ns_Last
        n = Ns - Spectrum%Ns_First + 1
        if( Spectrum%Type == 'C' ) then
           Spectrum_Error%S_R(n)  = dreal(Spectrum%Complex(n))
           Spectrum_Error%S_I(n)  = dimag(Spectrum%Complex(n))
           Spectrum_Error%S0_R(n) = dreal(Spectrum_ref%Complex(n))
           Spectrum_Error%S0_I(n) = dimag(Spectrum_ref%Complex(n))
        else if( Spectrum%Type == 'RI' ) then
           Spectrum_Error%S_R(n)  = Spectrum%Real_Part(n)
           Spectrum_Error%S_I(n)  = Spectrum%Imag_Part(n)
           Spectrum_Error%S0_R(n) = Spectrum_ref%Real_Part(n)
           Spectrum_Error%S0_I(n) = Spectrum_ref%Imag_Part(n)
        else if( Spectrum%Type == 'R' ) then
           Spectrum_Error%S_R(n)  = Spectrum%Real_Part(n)
           Spectrum_Error%S0_R(n) = Spectrum_ref%Real_Part(n)
        else if( Spectrum%Type == 'I' ) then
           Spectrum_Error%S_I(n)  = Spectrum%Imag_Part(n)
           Spectrum_Error%S0_I(n) = Spectrum_ref%Imag_Part(n)
        else
           write(*,*) 'Spectrum Type Error must be C RI R I'
           call exit(1)
        end if
        call plkderive( Cst_T_Ref, Spectrum%Wn(n), PlankT0, derPlankT0 )
        if( trim(Spectrum_Error%Type) == 'RI' .or. &
            trim(Spectrum_Error%Type) == 'R' ) then
           call plkinverse( Spectrum_Error%TB0(n), &
                            Spectrum%Wn(n),        &
                            Spectrum_Error%S0_R(n) )
           call plkinverse( Spectrum_Error%TB(n),  &
                            Spectrum%Wn(n),        &
                            Spectrum_Error%S_R(n)  )
!
           Spectrum_Error%Nedl_R(n) = Spectrum_Error%S_R(n)&
                                    - Spectrum_Error%S0_R(n)
           Spectrum_Error%Nedt_R(n) = Spectrum_Error%Nedl_R(n)/derPlankT0
        end if
        if( trim(Spectrum_Error%Type) == 'RI' .or. &
            trim(Spectrum_Error%Type) == 'I' ) then
!
           Spectrum_Error%Nedl_I(n) = Spectrum_Error%S_I(n)&
                                    - Spectrum_Error%S0_I(n)
           Spectrum_Error%Nedt_I(n) = Spectrum_Error%Nedl_I(n)/derPlankT0
        end if
     end do
!
!    Real Part
     if( trim(Spectrum_Error%Type) == 'RI' .or. &
         trim(Spectrum_Error%Type) == 'R' ) then
!
!       Nedl global spectrum statistics
        call statistics( Spectrum%N_Sample,          &
                         Spectrum_Error%Nedl_R,      &
!
                         Spectrum_Error%Nedl_R_Avg,  &
                         Spectrum_Error%Nedl_R_Std,  &
                         Spectrum_Error%Nedl_R_Min,  &
                         Spectrum_Error%Nedl_R_Max,  &
                         Spectrum_Error%Nedl_R_NsMin,&
                         Spectrum_Error%Nedl_R_NsMax )
!
!       Nedt global spectrum statistics
        call statistics( Spectrum%N_Sample,          &
                         Spectrum_Error%Nedt_R,      &
!
                         Spectrum_Error%Nedt_R_Avg,  &
                         Spectrum_Error%Nedt_R_Std,  &
                         Spectrum_Error%Nedt_R_Min,  &
                         Spectrum_Error%Nedt_R_Max,  &
                         Spectrum_Error%Nedt_R_NsMin,&
                         Spectrum_Error%Nedt_R_NsMax )
!
!       Nedl statistical analysis per spectral classes
        NsStart = 1
        do N_Class = 1, Nb_Wn_Class
           do while( Spectrum%Wn(NsStart) < Wn_Class(N_Class) .and. &
                     NsStart <  Spectrum%N_Sample )
              NsStart = NsStart + 1
           end do
           NsStop = NsStart
           do while( Spectrum%Wn(NsStop) < Wn_Class(N_Class+1) .and. &
                     NsStop <  Spectrum%N_Sample )
              NsStop = NsStop + 1
           end do
           if( NsStart /= NsStop ) then
              call statistics( NsStop-NsStart+1,                          &
                               Spectrum_Error%Nedl_R(NsStart),            &
!
                               Spectrum_Error%Nedl_R_Class_Avg(N_Class),  &
                               Spectrum_Error%Nedl_R_Class_Std(N_Class),  &
                               Spectrum_Error%Nedl_R_Class_Min(N_Class),  &
                               Spectrum_Error%Nedl_R_Class_Max(N_Class),  &
                               Spectrum_Error%Nedl_R_Class_NsMin(N_Class),&
                               Spectrum_Error%Nedl_R_Class_NsMax(N_Class) )
           end if
        end do
!
!       Nedt statistical analysis per spectral classes
        NsStart = 1
        do N_Class = 1, Nb_Wn_Class
           do while( Spectrum%Wn(NsStart) < Wn_Class(N_Class) .and. &
                     NsStart <  Spectrum%N_Sample )
              NsStart = NsStart + 1
           end do
           NsStop = NsStart
           do while( Spectrum%Wn(NsStop) < Wn_Class(N_Class+1) .and. &
                     NsStop <  Spectrum%N_Sample )
              NsStop = NsStop + 1
           end do
           if( NsStart /= NsStop ) then
              call statistics( NsStop-NsStart+1,                          &
                               Spectrum_Error%Nedt_R(NsStart),            &
!
                               Spectrum_Error%Nedt_R_Class_Avg(N_Class),  &
                               Spectrum_Error%Nedt_R_Class_Std(N_Class),  &
                               Spectrum_Error%Nedt_R_Class_Min(N_Class),  &
                               Spectrum_Error%Nedt_R_Class_Max(N_Class),  &
                               Spectrum_Error%Nedt_R_Class_NsMin(N_Class),&
                               Spectrum_Error%Nedt_R_Class_NsMax(N_Class) )
           end if
        end do
     end if
!
!    Imaginary Part
     if( trim(Spectrum_Error%Type) == 'RI' .or. &
         trim(Spectrum_Error%Type) == 'I' ) then
!
!       Nedl global spectrum statistics
        call statistics( Spectrum%N_Sample,          &
                         Spectrum_Error%Nedl_I,      &
!   
                         Spectrum_Error%Nedl_I_Avg,  &
                         Spectrum_Error%Nedl_I_Std,  &
                         Spectrum_Error%Nedl_I_Min,  &
                         Spectrum_Error%Nedl_I_Max,  &
                         Spectrum_Error%Nedl_I_NsMin,&
                         Spectrum_Error%Nedl_I_NsMax )
!
!       Nedt global spectrum statistics
        call statistics( Spectrum%N_Sample,          &
                         Spectrum_Error%Nedt_I,      &
!   
                         Spectrum_Error%Nedt_I_Avg,  &
                         Spectrum_Error%Nedt_I_Std,  &
                         Spectrum_Error%Nedt_I_Min,  &
                         Spectrum_Error%Nedt_I_Max,  &
                         Spectrum_Error%Nedt_I_NsMin,&
                         Spectrum_Error%Nedt_I_NsMax )
!
!       Nedl statistical analysis per spectral classes
        NsStart = 1
        do N_Class = 1, Nb_Wn_Class
           do while( Spectrum%Wn(NsStart) < Wn_Class(N_Class) .and. &
                     NsStart <  Spectrum%N_Sample )
              NsStart = NsStart + 1
           end do
           NsStop = NsStart
           do while( Spectrum%Wn(NsStop) < Wn_Class(N_Class+1) .and. &
                     NsStop <  Spectrum%N_Sample )
              NsStop = NsStop + 1
           end do
           if( NsStart /= NsStop ) then
              call statistics( NsStop-NsStart+1,                          &
                               Spectrum_Error%Nedl_I(NsStart),            &
!   
                               Spectrum_Error%Nedl_I_Class_Avg(N_Class),  &
                               Spectrum_Error%Nedl_I_Class_Std(N_Class),  &
                               Spectrum_Error%Nedl_I_Class_Min(N_Class),  &
                               Spectrum_Error%Nedl_I_Class_Max(N_Class),  &
                               Spectrum_Error%Nedl_I_Class_NsMin(N_Class),&
                               Spectrum_Error%Nedl_I_Class_NsMax(N_Class) )
           end if
        end do
!
!       Nedt statistical analysis per spectral classes
        NsStart = 1
        do N_Class = 1, Nb_Wn_Class
           do while( Spectrum%Wn(NsStart) < Wn_Class(N_Class) .and. &
                     NsStart <  Spectrum%N_Sample )
              NsStart = NsStart + 1
           end do
           NsStop = NsStart
           do while( Spectrum%Wn(NsStop) < Wn_Class(N_Class+1) .and. &
                     NsStop <  Spectrum%N_Sample )
             NsStop = NsStop + 1
           end do
           if( NsStart /= NsStop ) then
              call statistics( NsStop-NsStart+1,                          &
                               Spectrum_Error%Nedt_I(NsStart),            &
!
                               Spectrum_Error%Nedt_I_Class_Avg(N_Class),  &
                               Spectrum_Error%Nedt_I_Class_Std(N_Class),  &
                               Spectrum_Error%Nedt_I_Class_Min(N_Class),  &
                               Spectrum_Error%Nedt_I_Class_Max(N_Class),  &
                               Spectrum_Error%Nedt_I_Class_NsMin(N_Class),&
                               Spectrum_Error%Nedt_I_Class_NsMax(N_Class) )
           end if
        end do
     end if
!
   return
999 write(*,*) 'radiometry_error Fatal Error'
    call exit(1)
!
   end subroutine error_radiometry
!
!
   subroutine stats_radiometry( Nb_Spectrum,   &
                                Spectrum_Error,&
                                Spectrum_Stats )
   implicit none
   integer(kind=LONG)       ,intent(in)                  :: Nb_Spectrum
   type(type_Radiometry_Error),intent(in),dimension(Nb_Spectrum) &
                                                         :: Spectrum_Error
   type(type_Radiometry_Stats),intent(out)               :: Spectrum_Stats
!
   integer(kind=LONG)                                    :: Ns
   integer(kind=LONG)                                    :: Nb
   real(kind=DOUBLE) ,dimension(:),allocatable           :: X
!
!    header transfert
     Spectrum_Stats%Type     = Spectrum_Error(1)%Type
     Spectrum_Stats%N_Sample = Spectrum_Error(1)%N_Sample
     Spectrum_Stats%Ns_First = Spectrum_Error(1)%Ns_First
     Spectrum_Stats%Ns_Last  = Spectrum_Error(1)%Ns_Last
     Spectrum_Stats%Wn_First = Spectrum_Error(1)%Wn_First
     Spectrum_Stats%Wn_Last  = Spectrum_Error(1)%Wn_Last
     Spectrum_Stats%WnMax    = Spectrum_Error(1)%WnMax
     Spectrum_Stats%dWn      = Spectrum_Error(1)%dWn
     Spectrum_Stats%Nb_Wn_Class = Spectrum_Error(1)%Nb_Wn_Class
     call alloc_Radiometry_Stats ( Spectrum_Stats )
     Spectrum_Stats%Wn(1:Spectrum_Stats%N_Sample) = &
                    Spectrum_Error(1)%Wn(1:Spectrum_Stats%N_Sample)
     Spectrum_Stats%Wn_Class(1:Spectrum_Stats%Nb_Wn_Class) =            &
                    Spectrum_Error(1)%Wn_Class(1:Spectrum_Stats%Nb_Wn_Class)
!
!    loop on the spectral samples
     allocate( X(Nb_Spectrum) )
     do Ns = 1, Spectrum_Stats%N_Sample
!
!       Real Part
        if( trim(Spectrum_Stats%Type) == 'RI' .or. &
            trim(Spectrum_Stats%Type) == 'R' ) then
!
!          Nedl real part
           do Nb = 1, Nb_Spectrum
              X(Nb) = Spectrum_Error(Nb)%Nedl_R(Ns)
           end do
           call statistics( Nb_Spectrum,                    &
                            X,                              &
                            Spectrum_Stats%Nedl_R_Avg(Ns),  &
                            Spectrum_Stats%Nedl_R_Std(Ns),  &
                            Spectrum_Stats%Nedl_R_Min(Ns),  &
                            Spectrum_Stats%Nedl_R_Max(Ns),  &
                            Spectrum_Stats%Nedl_R_NsMin(Ns),&
                            Spectrum_Stats%Nedl_R_NsMax(Ns) )
!
!          Nedt real part
           do Nb = 1, Nb_Spectrum
              X(Nb) = Spectrum_Error(Nb)%Nedt_R(Ns)
           end do
           call statistics( Nb_Spectrum,                    &
                            X,                              &
                            Spectrum_Stats%Nedt_R_Avg(Ns),  &
                            Spectrum_Stats%Nedt_R_Std(Ns),  &
                            Spectrum_Stats%Nedt_R_Min(Ns),  &
                            Spectrum_Stats%Nedt_R_Max(Ns),  &
                            Spectrum_Stats%Nedt_R_NsMin(Ns),&
                            Spectrum_Stats%Nedt_R_NsMax(Ns) )
        end if
!
!       Imaginary Part
        if( trim(Spectrum_Stats%Type) == 'RI' .or. &
            trim(Spectrum_Stats%Type) == 'I' ) then
!
!          Nedl imaginary part
           do Nb = 1, Nb_Spectrum
              X(Nb) = Spectrum_Error(Nb)%Nedl_I(Ns)
           end do
           call statistics( Nb_Spectrum,                    &
                            X,                              &
                            Spectrum_Stats%Nedl_I_Avg(Ns),  &
                            Spectrum_Stats%Nedl_I_Std(Ns),  &
                            Spectrum_Stats%Nedl_I_Min(Ns),  &
                            Spectrum_Stats%Nedl_I_Max(Ns),  &
                            Spectrum_Stats%Nedl_I_NsMin(Ns),&
                            Spectrum_Stats%Nedl_I_NsMax(Ns) )
!
!          Nedt imaginary part
           do Nb = 1, Nb_Spectrum
              X(Nb) = Spectrum_Error(Nb)%Nedt_I(Ns)
           end do
           call statistics( Nb_Spectrum,                    &
                            X,                              &
                            Spectrum_Stats%Nedt_I_Avg(Ns),  &
                            Spectrum_Stats%Nedt_I_Std(Ns),  &
                            Spectrum_Stats%Nedt_I_Min(Ns),  &
                            Spectrum_Stats%Nedt_I_Max(Ns),  &
                            Spectrum_Stats%Nedt_I_NsMin(Ns),&
                            Spectrum_Stats%Nedt_I_NsMax(Ns) )
        end if
     end do ! End spectral samples loop
!
!    loop on the spectral classes
     do Ns = 1, Spectrum_Stats%Nb_Wn_Class
!
!       Real Part
        if( trim(Spectrum_Stats%Type) == 'RI' .or. &
            trim(Spectrum_Stats%Type) == 'R' ) then
!
!          Avg Nedl real part
           do Nb = 1, Nb_Spectrum
              X(Nb) = Spectrum_Error(Nb)%Nedl_R_Class_Avg(Ns)
           end do
           call statistics( Nb_Spectrum,                              &
                            X,                                        &
                            Spectrum_Stats%Nedl_R_Class_Avg_Avg(Ns),  &
                            Spectrum_Stats%Nedl_R_Class_Avg_Std(Ns),  &
                            Spectrum_Stats%Nedl_R_Class_Avg_Min(Ns),  &
                            Spectrum_Stats%Nedl_R_Class_Avg_Max(Ns),  &
                            Spectrum_Stats%Nedl_R_Class_Avg_NsMin(Ns),&
                            Spectrum_Stats%Nedl_R_Class_Avg_NsMax(Ns) )
!
!          Std Nedl real part
           do Nb = 1, Nb_Spectrum
              X(Nb) = Spectrum_Error(Nb)%Nedl_R_Class_Std(Ns)
           end do
           call statistics( Nb_Spectrum,                              &
                            X,                                        &
                            Spectrum_Stats%Nedl_R_Class_Std_Avg(Ns),  &
                            Spectrum_Stats%Nedl_R_Class_Std_Std(Ns),  &
                            Spectrum_Stats%Nedl_R_Class_Std_Min(Ns),  &
                            Spectrum_Stats%Nedl_R_Class_Std_Max(Ns),  &
                            Spectrum_Stats%Nedl_R_Class_Std_NsMin(Ns),&
                            Spectrum_Stats%Nedl_R_Class_Std_NsMax(Ns) )
!
!          Min Nedl real part
           do Nb = 1, Nb_Spectrum
              X(Nb) = Spectrum_Error(Nb)%Nedl_R_Class_Min(Ns)
           end do
           call statistics( Nb_Spectrum,                              &
                            X,                                        &
                            Spectrum_Stats%Nedl_R_Class_Min_Avg(Ns),  &
                            Spectrum_Stats%Nedl_R_Class_Min_Std(Ns),  &
                            Spectrum_Stats%Nedl_R_Class_Min_Min(Ns),  &
                            Spectrum_Stats%Nedl_R_Class_Min_Max(Ns),  &
                            Spectrum_Stats%Nedl_R_Class_Min_NsMin(Ns),&
                            Spectrum_Stats%Nedl_R_Class_Min_NsMax(Ns) )
!
!          Max Nedl real part
           do Nb = 1, Nb_Spectrum
              X(Nb) = Spectrum_Error(Nb)%Nedl_R_Class_Max(Ns)
           end do
           call statistics( Nb_Spectrum,                              &
                            X,                                        &
                            Spectrum_Stats%Nedl_R_Class_Max_Avg(Ns),  &
                            Spectrum_Stats%Nedl_R_Class_Max_Std(Ns),  &
                            Spectrum_Stats%Nedl_R_Class_Max_Min(Ns),  &
                            Spectrum_Stats%Nedl_R_Class_Max_Max(Ns),  &
                            Spectrum_Stats%Nedl_R_Class_Max_NsMin(Ns),&
                            Spectrum_Stats%Nedl_R_Class_Max_NsMax(Ns) )
!
!          NsMin Nedl real part
           do Nb = 1, Nb_Spectrum
              X(Nb) = Spectrum_Error(Nb)%Nedl_R_Class_NsMin(Ns)
           end do
           call statistics( Nb_Spectrum,                                &
                            X,                                          &
                            Spectrum_Stats%Nedl_R_Class_NsMin_Avg(Ns),  &
                            Spectrum_Stats%Nedl_R_Class_NsMin_Std(Ns),  &
                            Spectrum_Stats%Nedl_R_Class_NsMin_Min(Ns),  &
                            Spectrum_Stats%Nedl_R_Class_NsMin_Max(Ns),  &
                            Spectrum_Stats%Nedl_R_Class_NsMin_NsMin(Ns),&
                            Spectrum_Stats%Nedl_R_Class_NsMin_NsMax(Ns) )
!
!          NsMax Nedl real part
           do Nb = 1, Nb_Spectrum
              X(Nb) = Spectrum_Error(Nb)%Nedl_R_Class_NsMax(Ns)
           end do
           call statistics( Nb_Spectrum,                                &
                            X,                                          &
                            Spectrum_Stats%Nedl_R_Class_NsMax_Avg(Ns),  &
                            Spectrum_Stats%Nedl_R_Class_NsMax_Std(Ns),  &
                            Spectrum_Stats%Nedl_R_Class_NsMax_Min(Ns),  &
                            Spectrum_Stats%Nedl_R_Class_NsMax_Max(Ns),  &
                            Spectrum_Stats%Nedl_R_Class_NsMax_NsMin(Ns),&
                            Spectrum_Stats%Nedl_R_Class_NsMax_NsMax(Ns) )
!
!          Avg Nedt real part
           do Nb = 1, Nb_Spectrum
              X(Nb) = Spectrum_Error(Nb)%Nedt_R_Class_Avg(Ns)
           end do
           call statistics( Nb_Spectrum,                              &
                            X,                                        &
                            Spectrum_Stats%Nedt_R_Class_Avg_Avg(Ns),  &
                            Spectrum_Stats%Nedt_R_Class_Avg_Std(Ns),  &
                            Spectrum_Stats%Nedt_R_Class_Avg_Min(Ns),  &
                            Spectrum_Stats%Nedt_R_Class_Avg_Max(Ns),  &
                            Spectrum_Stats%Nedt_R_Class_Avg_NsMin(Ns),&
                            Spectrum_Stats%Nedt_R_Class_Avg_NsMax(Ns) )
!
!          Std Nedt real part
           do Nb = 1, Nb_Spectrum
              X(Nb) = Spectrum_Error(Nb)%Nedt_R_Class_Std(Ns)
           end do
           call statistics( Nb_Spectrum,                              &
                            X,                                        &
                            Spectrum_Stats%Nedt_R_Class_Std_Avg(Ns),  &
                            Spectrum_Stats%Nedt_R_Class_Std_Std(Ns),  &
                            Spectrum_Stats%Nedt_R_Class_Std_Min(Ns),  &
                            Spectrum_Stats%Nedt_R_Class_Std_Max(Ns),  &
                            Spectrum_Stats%Nedt_R_Class_Std_NsMin(Ns),&
                            Spectrum_Stats%Nedt_R_Class_Std_NsMax(Ns) )
!
!          Min Nedt real part
           do Nb = 1, Nb_Spectrum
              X(Nb) = Spectrum_Error(Nb)%Nedt_R_Class_Min(Ns)
           end do
           call statistics( Nb_Spectrum,                              &
                            X,                                        &
                            Spectrum_Stats%Nedt_R_Class_Min_Avg(Ns),  &
                            Spectrum_Stats%Nedt_R_Class_Min_Std(Ns),  &
                            Spectrum_Stats%Nedt_R_Class_Min_Min(Ns),  &
                            Spectrum_Stats%Nedt_R_Class_Min_Max(Ns),  &
                            Spectrum_Stats%Nedt_R_Class_Min_NsMin(Ns),&
                            Spectrum_Stats%Nedt_R_Class_Min_NsMax(Ns) )
!
!          Max Nedt real part
           do Nb = 1, Nb_Spectrum
              X(Nb) = Spectrum_Error(Nb)%Nedt_R_Class_Max(Ns)
           end do
           call statistics( Nb_Spectrum,                              &
                            X,                                        &
                            Spectrum_Stats%Nedt_R_Class_Max_Avg(Ns),  &
                            Spectrum_Stats%Nedt_R_Class_Max_Std(Ns),  &
                            Spectrum_Stats%Nedt_R_Class_Max_Min(Ns),  &
                            Spectrum_Stats%Nedt_R_Class_Max_Max(Ns),  &
                            Spectrum_Stats%Nedt_R_Class_Max_NsMin(Ns),&
                            Spectrum_Stats%Nedt_R_Class_Max_NsMax(Ns) )
!
!          NsMin Nedt real part
           do Nb = 1, Nb_Spectrum
              X(Nb) = Spectrum_Error(Nb)%Nedt_R_Class_NsMin(Ns)
           end do
           call statistics( Nb_Spectrum,                                &
                            X,                                          &
                            Spectrum_Stats%Nedt_R_Class_NsMin_Avg(Ns),  &
                            Spectrum_Stats%Nedt_R_Class_NsMin_Std(Ns),  &
                            Spectrum_Stats%Nedt_R_Class_NsMin_Min(Ns),  &
                            Spectrum_Stats%Nedt_R_Class_NsMin_Max(Ns),  &
                            Spectrum_Stats%Nedt_R_Class_NsMin_NsMin(Ns),&
                            Spectrum_Stats%Nedt_R_Class_NsMin_NsMax(Ns) )
!
!          NsMax Nedt real part
           do Nb = 1, Nb_Spectrum
              X(Nb) = Spectrum_Error(Nb)%Nedt_R_Class_NsMax(Ns)
           end do
           call statistics( Nb_Spectrum,                                &
                            X,                                          &
                            Spectrum_Stats%Nedt_R_Class_NsMax_Avg(Ns),  &
                            Spectrum_Stats%Nedt_R_Class_NsMax_Std(Ns),  &
                            Spectrum_Stats%Nedt_R_Class_NsMax_Min(Ns),  &
                            Spectrum_Stats%Nedt_R_Class_NsMax_Max(Ns),  &
                            Spectrum_Stats%Nedt_R_Class_NsMax_NsMin(Ns),&
                            Spectrum_Stats%Nedt_R_Class_NsMax_NsMax(Ns) )
        end if
!
!       Imaginary Part
        if( trim(Spectrum_Stats%Type) == 'RI' .or. &
            trim(Spectrum_Stats%Type) == 'I' ) then
!
!          Avg Nedl imaginary part
           do Nb = 1, Nb_Spectrum
              X(Nb) = Spectrum_Error(Nb)%Nedl_I_Class_Avg(Ns)
           end do
           call statistics( Nb_Spectrum,                              &
                            X,                                        &
                            Spectrum_Stats%Nedl_I_Class_Avg_Avg(Ns),  &
                            Spectrum_Stats%Nedl_I_Class_Avg_Std(Ns),  &
                            Spectrum_Stats%Nedl_I_Class_Avg_Min(Ns),  &
                            Spectrum_Stats%Nedl_I_Class_Avg_Max(Ns),  &
                            Spectrum_Stats%Nedl_I_Class_Avg_NsMin(Ns),&
                            Spectrum_Stats%Nedl_I_Class_Avg_NsMax(Ns) )
!
!          Std Nedl imaginary part
           do Nb = 1, Nb_Spectrum
              X(Nb) = Spectrum_Error(Nb)%Nedl_I_Class_Std(Ns)
           end do
           call statistics( Nb_Spectrum,                              &
                            X,                                        &
                            Spectrum_Stats%Nedl_I_Class_Std_Avg(Ns),  &
                            Spectrum_Stats%Nedl_I_Class_Std_Std(Ns),  &
                            Spectrum_Stats%Nedl_I_Class_Std_Min(Ns),  &
                            Spectrum_Stats%Nedl_I_Class_Std_Max(Ns),  &
                            Spectrum_Stats%Nedl_I_Class_Std_NsMin(Ns),&
                            Spectrum_Stats%Nedl_I_Class_Std_NsMax(Ns) )
!
!          Min Nedl imaginary part
           do Nb = 1, Nb_Spectrum
              X(Nb) = Spectrum_Error(Nb)%Nedl_I_Class_Min(Ns)
           end do
           call statistics( Nb_Spectrum,                              &
                            X,                                        &
                            Spectrum_Stats%Nedl_I_Class_Min_Avg(Ns),  &
                            Spectrum_Stats%Nedl_I_Class_Min_Std(Ns),  &
                            Spectrum_Stats%Nedl_I_Class_Min_Min(Ns),  &
                            Spectrum_Stats%Nedl_I_Class_Min_Max(Ns),  &
                            Spectrum_Stats%Nedl_I_Class_Min_NsMin(Ns),&
                            Spectrum_Stats%Nedl_I_Class_Min_NsMax(Ns) )
!
!          Max Nedl imaginary part
           do Nb = 1, Nb_Spectrum
              X(Nb) = Spectrum_Error(Nb)%Nedl_I_Class_Max(Ns)
           end do
           call statistics( Nb_Spectrum,                              &
                            X,                                        &
                            Spectrum_Stats%Nedl_I_Class_Max_Avg(Ns),  &
                            Spectrum_Stats%Nedl_I_Class_Max_Std(Ns),  &
                            Spectrum_Stats%Nedl_I_Class_Max_Min(Ns),  &
                            Spectrum_Stats%Nedl_I_Class_Max_Max(Ns),  &
                            Spectrum_Stats%Nedl_I_Class_Max_NsMin(Ns),&
                            Spectrum_Stats%Nedl_I_Class_Max_NsMax(Ns) )
!
!          NsMin Nedl imaginary part
           do Nb = 1, Nb_Spectrum
              X(Nb) = Spectrum_Error(Nb)%Nedl_I_Class_NsMin(Ns)
           end do
           call statistics( Nb_Spectrum,                                &
                            X,                                          &
                            Spectrum_Stats%Nedl_I_Class_NsMin_Avg(Ns),  &
                            Spectrum_Stats%Nedl_I_Class_NsMin_Std(Ns),  &
                            Spectrum_Stats%Nedl_I_Class_NsMin_Min(Ns),  &
                            Spectrum_Stats%Nedl_I_Class_NsMin_Max(Ns),  &
                            Spectrum_Stats%Nedl_I_Class_NsMin_NsMin(Ns),&
                            Spectrum_Stats%Nedl_I_Class_NsMin_NsMax(Ns) )
!
!          NsMax Nedl imaginary part
           do Nb = 1, Nb_Spectrum
              X(Nb) = Spectrum_Error(Nb)%Nedl_I_Class_NsMax(Ns)
           end do
           call statistics( Nb_Spectrum,                                &
                            X,                                          &
                            Spectrum_Stats%Nedl_I_Class_NsMax_Avg(Ns),  &
                            Spectrum_Stats%Nedl_I_Class_NsMax_Std(Ns),  &
                            Spectrum_Stats%Nedl_I_Class_NsMax_Min(Ns),  &
                            Spectrum_Stats%Nedl_I_Class_NsMax_Max(Ns),  &
                            Spectrum_Stats%Nedl_I_Class_NsMax_NsMin(Ns),&
                            Spectrum_Stats%Nedl_I_Class_NsMax_NsMax(Ns) )
!
!          Avg Nedt imaginary part
           do Nb = 1, Nb_Spectrum
              X(Nb) = Spectrum_Error(Nb)%Nedt_I_Class_Avg(Ns)
           end do
           call statistics( Nb_Spectrum,                              &
                            X,                                        &
                            Spectrum_Stats%Nedt_I_Class_Avg_Avg(Ns),  &
                            Spectrum_Stats%Nedt_I_Class_Avg_Std(Ns),  &
                            Spectrum_Stats%Nedt_I_Class_Avg_Min(Ns),  &
                            Spectrum_Stats%Nedt_I_Class_Avg_Max(Ns),  &
                            Spectrum_Stats%Nedt_I_Class_Avg_NsMin(Ns),&
                            Spectrum_Stats%Nedt_I_Class_Avg_NsMax(Ns) )
!
!          Std Nedt imaginary part
           do Nb = 1, Nb_Spectrum
              X(Nb) = Spectrum_Error(Nb)%Nedt_I_Class_Std(Ns)
           end do
           call statistics( Nb_Spectrum,                              &
                            X,                                        &
                            Spectrum_Stats%Nedt_I_Class_Std_Avg(Ns),  &
                            Spectrum_Stats%Nedt_I_Class_Std_Std(Ns),  &
                            Spectrum_Stats%Nedt_I_Class_Std_Min(Ns),  &
                            Spectrum_Stats%Nedt_I_Class_Std_Max(Ns),  &
                            Spectrum_Stats%Nedt_I_Class_Std_NsMin(Ns),&
                            Spectrum_Stats%Nedt_I_Class_Std_NsMax(Ns) )
!
!          Min Nedt imaginary part
           do Nb = 1, Nb_Spectrum
              X(Nb) = Spectrum_Error(Nb)%Nedt_I_Class_Min(Ns)
           end do
           call statistics( Nb_Spectrum,                              &
                            X,                                        &
                            Spectrum_Stats%Nedt_I_Class_Min_Avg(Ns),  &
                            Spectrum_Stats%Nedt_I_Class_Min_Std(Ns),  &
                            Spectrum_Stats%Nedt_I_Class_Min_Min(Ns),  &
                            Spectrum_Stats%Nedt_I_Class_Min_Max(Ns),  &
                            Spectrum_Stats%Nedt_I_Class_Min_NsMin(Ns),&
                            Spectrum_Stats%Nedt_I_Class_Min_NsMax(Ns) )
!
!          Max Nedt imaginary part
           do Nb = 1, Nb_Spectrum
              X(Nb) = Spectrum_Error(Nb)%Nedt_I_Class_Max(Ns)
           end do
           call statistics( Nb_Spectrum,                              &
                            X,                                        &
                            Spectrum_Stats%Nedt_I_Class_Max_Avg(Ns),  &
                            Spectrum_Stats%Nedt_I_Class_Max_Std(Ns),  &
                            Spectrum_Stats%Nedt_I_Class_Max_Min(Ns),  &
                            Spectrum_Stats%Nedt_I_Class_Max_Max(Ns),  &
                            Spectrum_Stats%Nedt_I_Class_Max_NsMin(Ns),&
                            Spectrum_Stats%Nedt_I_Class_Max_NsMax(Ns) )
!
!          NsMin Nedt imaginary part
           do Nb = 1, Nb_Spectrum
              X(Nb) = Spectrum_Error(Nb)%Nedt_I_Class_NsMin(Ns)
           end do
           call statistics( Nb_Spectrum,                                &
                            X,                                          &
                            Spectrum_Stats%Nedt_I_Class_NsMin_Avg(Ns),  &
                            Spectrum_Stats%Nedt_I_Class_NsMin_Std(Ns),  &
                            Spectrum_Stats%Nedt_I_Class_NsMin_Min(Ns),  &
                            Spectrum_Stats%Nedt_I_Class_NsMin_Max(Ns),  &
                            Spectrum_Stats%Nedt_I_Class_NsMin_NsMin(Ns),&
                            Spectrum_Stats%Nedt_I_Class_NsMin_NsMax(Ns) )
!
!          NsMax Nedt imaginary part
           do Nb = 1, Nb_Spectrum
              X(Nb) = Spectrum_Error(Nb)%Nedt_I_Class_NsMax(Ns)
           end do
           call statistics( Nb_Spectrum,                                &
                            X,                                          &
                            Spectrum_Stats%Nedt_I_Class_NsMax_Avg(Ns),  &
                            Spectrum_Stats%Nedt_I_Class_NsMax_Std(Ns),  &
                            Spectrum_Stats%Nedt_I_Class_NsMax_Min(Ns),  &
                            Spectrum_Stats%Nedt_I_Class_NsMax_Max(Ns),  &
                            Spectrum_Stats%Nedt_I_Class_NsMax_NsMin(Ns),&
                            Spectrum_Stats%Nedt_I_Class_NsMax_NsMax(Ns) )
        end if
     end do ! End spectral classes loop
   return
   end subroutine stats_radiometry
!
!
   subroutine diagonal_noise_spectrum( Performance_Param,&
                                       Nb_Spectrum,      &
                                       Spectrum,         &
                                       Diagonal_Noise    )
   implicit None
     type(type_Performance)   ,intent(in)                     :: Performance_Param
     integer(kind=LONG)       ,intent(in)                     :: Nb_Spectrum
     type(type_Spectrum),intent(inout),dimension(Nb_Spectrum) :: Spectrum
     type(type_Diagonal_Noise),intent(out)                    :: Diagonal_Noise
!
     integer(kind=LONG)                                       :: iPos
     integer(kind=LONG)                                       :: ErrCode
     integer(kind=LONG)                                       :: Nb
     integer(kind=LONG)                                       :: Ns
     real(kind=DOUBLE)           ,dimension(:),allocatable    :: X
     real(kind=DOUBLE)           ,dimension(:),allocatable    :: Y
     real(kind=DOUBLE)           ,dimension(:),allocatable    :: Z
     real(kind=DOUBLE)           ,dimension(:),allocatable    :: W
     real(kind=DOUBLE)                                        :: Z_Avg
     real(kind=DOUBLE)                                        :: PlankT0
     real(kind=DOUBLE)                                        :: derPlankT0
     real(kind=DOUBLE)                                        :: rmsfit
!
     iPos = 0
     if( Nb_Spectrum < Performance_Param%Col_Size_Min ) then
        write(*,*) 'Not enough spectra in the collection', Nb_Spectrum
        go to 999
     end if
     do Nb = 1, Nb_Spectrum
        iPos = Nb
        if( Spectrum(Nb)%N_Sample /= Spectrum(1)%N_Sample ) then
           write(*,*) 'Wrong Spectrum sample number 1 Nb ', Nb, &
                       Spectrum(1)%N_Sample, Spectrum(Nb)%N_Sample
           go to 999
        end if
!
!       spectrum header transfert to diagonal noise
        if( Nb == 1 ) then
           if( Spectrum(Nb)%Type == 'C' ) then
              Diagonal_Noise%Type     = 'RI'
           else if( Spectrum(Nb)%Type == 'R' ) then
              Diagonal_Noise%Type     = 'R'
           else if( Spectrum(Nb)%Type == 'I' ) then
              Diagonal_Noise%Type     = 'I'
           else
              write(*,*) 'Spectrum Type Error must be C RI R'
              go to 999
           end if
           Diagonal_Noise%N_Sample = Spectrum(1)%N_Sample
           Diagonal_Noise%Ns_First = Spectrum(1)%Ns_First
           Diagonal_Noise%Ns_Last  = Spectrum(1)%Ns_Last
           Diagonal_Noise%Wn_First = Spectrum(1)%Wn_First
           Diagonal_Noise%Wn_Last  = Spectrum(1)%Wn_Last
           Diagonal_Noise%WnMax    = Spectrum(1)%WnMax
           Diagonal_Noise%dWn      = Spectrum(1)%dWn
           call alloc_Diagonal_Noise( Diagonal_Noise )
           Diagonal_Noise%Wn(1:Spectrum(1)%N_Sample) = &
                               Spectrum(1)%Wn(1:Spectrum(1)%N_Sample)
        end if
!
!       transform spectrum type to RI
        if( Spectrum(Nb)%Type == 'C' ) then
           Spectrum(Nb)%Type = 'RI'
           allocate( Spectrum(Nb)%Real_Part(Spectrum(Nb)%N_Sample) )
           allocate( Spectrum(Nb)%Imag_Part(Spectrum(Nb)%N_Sample) )
           Spectrum(Nb)%Real_Part(1:Spectrum(Nb)%N_Sample) = &
              dreal( Spectrum(Nb)%Complex(1:Spectrum(Nb)%N_Sample))
           Spectrum(Nb)%Imag_Part(1:Spectrum(Nb)%N_Sample) = &
              dimag( Spectrum(Nb)%Complex(1:Spectrum(Nb)%N_Sample))
           deallocate( Spectrum(Nb)%Complex )
        else if( Spectrum(Nb)%Type == 'R' .or. &
                 Spectrum(Nb)%Type == 'I' .or. &
                 Spectrum(Nb)%Type == 'RI' ) then
!          Nothing to do
        else
           write(*,*) 'Spectrum Type Error must be C RI R'
           go to 999
        end if
     end do
!
!    central date
     Diagonal_Noise%Date     = Spectrum(int(Nb_Spectrum/2))%Date
!
!    loop on the spectral samples
     allocate( X( Nb_Spectrum ) )
     allocate( Y( Nb_Spectrum ) )
     allocate( Z( Nb_Spectrum ) )
!
!    Weight set to 1
     allocate( W( Nb_Spectrum ) )
     W(1:Nb_Spectrum) = 1_DOUBLE
!
!    Date transfert
!!!!     X(1:Nb_Spectrum) = Spectrum(1:Nb_Spectrum)%Date  
     X(1:Nb_Spectrum) = (/ (dble(Ns-1),Ns=1,Nb_Spectrum) /)
!
!    Noise real part
     if( trim(Diagonal_Noise%Type) == 'RI' .or. &
         trim(Diagonal_Noise%Type) == 'R' ) then
        do Ns = 1, Spectrum(1)%N_Sample
!
!          spectrum samples transfert
           do Nb = 1, Nb_Spectrum
              Y(Nb) = Spectrum(Nb)%Real_Part(Ns)
           end do
!
!          polynomial fit
           rmsfit = 0_DOUBLE
           call intpolynome( Performance_Param%deg_polynom,&
                             Nb_Spectrum, X, Y, W,         &
                             Nb_Spectrum, X, Z, Z_Avg,     &
                             rmsfit, ErrCode               )
           if( ErrCode /= 0 ) then
              write(0,*) 'Warnning temporal drift real part PolyLeastsqr fit abort'
              go to 999
           end if
!
!          Average spectrum
           Diagonal_Noise%Spectrum_Avg_R(Ns) = Z_Avg
!
!          Noise as the residual fit (Y-Z)
           Diagonal_Noise%NoiseNedl_R(Ns) = &
                       dsqrt( sum( (Y(1:Nb_Spectrum)-Z(1:Nb_Spectrum))**2 )&
                              / real(Nb_Spectrum-1,DOUBLE) )
!
!          Nedt Conversion
           call plkderive( Cst_T_Ref,Spectrum(1)%Wn(Ns),PlankT0,derPlankT0 )
           Diagonal_Noise%NoiseNedT_R(Ns) = Diagonal_Noise%NoiseNedl_R(Ns) &
                                          / derPlankT0
!
!          temporal drift during the sequence in NedT at T0
           Diagonal_Noise%DriftT_R(Ns) = (Z(Nb_Spectrum)-Z(1))  / derPlankT0
!
!          noise spectrum corrected for the temporal drift
           do Nb = 1, Nb_Spectrum
              Spectrum(Nb)%Real_Part(Ns) = Y(Nb)-Z(Nb)
           end do
        end do
!
!       smooth spectrum
        call moving_average( Diagonal_Noise%NoiseNedl_R,     &
                             Spectrum(1)%N_Sample,           &
                             Performance_Param%SlideFilter,  &
                             Diagonal_Noise%SmoothNoiseNedl_R )
        call moving_average( Diagonal_Noise%NoiseNedt_R,      &
                             Spectrum(1)%N_Sample,            &
                             Performance_Param%SlideFilter,   &
                             Diagonal_Noise%SmoothNoiseNedt_R )
        call moving_average( Diagonal_Noise%DriftT_R,      &
                             Spectrum(1)%N_Sample,         &
                             Performance_Param%SlideFilter,&
                             Diagonal_Noise%SmoothDriftT_R )
        call moving_average( Diagonal_Noise%Spectrum_Avg_R,      &
                             Spectrum(1)%N_Sample,               &
                             Performance_Param%SlideFilter,      &
                             Diagonal_Noise%SmoothSpectrum_Avg_R )
     end if
!
!    Noise imaginary part
     if( trim(Diagonal_Noise%Type) == 'RI' .or. &
         trim(Diagonal_Noise%Type) == 'I' ) then
        do Ns = 1, Spectrum(1)%N_Sample
!
!          spectrum samples transfert
           do Nb = 1, Nb_Spectrum
              Y(Nb) = Spectrum(Nb)%Imag_Part(Ns)
           end do
!
!          polynomial fit
           rmsfit=0_DOUBLE
           call intpolynome( Performance_Param%deg_polynom,&
                             Nb_Spectrum, X, Y, W,         &
                             Nb_Spectrum, X, Z, Z_Avg,     &
                             rmsfit, ErrCode               )
           if( ErrCode /= 0 ) then
              write(0,*) 'Warnning temporal drift imag part PolyLeastsqr fit abort'
              go to 999
           end if
!
!          Average spectrum
           Diagonal_Noise%Spectrum_Avg_I(Ns) = Z_Avg
!
!          Noise as the residual fit (Y-Z)
           Diagonal_Noise%NoiseNedl_I(Ns) = &
                       dsqrt( sum( (Y(1:Nb_Spectrum)-Z(1:Nb_Spectrum))**2 )&
                              / real(Nb_Spectrum-1,DOUBLE) )
!
!          Nedt Conversion
           call plkderive( Cst_T_Ref,Spectrum(1)%Wn(Ns),PlankT0,derPlankT0 )
           Diagonal_Noise%NoiseNedT_I(Ns) = Diagonal_Noise%NoiseNedl_I(Ns) &
                                          / derPlankT0
!
!          temporal drift during the sequence in NedT at T0
           Diagonal_Noise%DriftT_I(Ns) = (Z(Nb_Spectrum)-Z(1)) / derPlankT0
!
!          noise spectrum corrected for the temporal drift
           do Nb = 1, Nb_Spectrum
              Spectrum(Nb)%Imag_Part(Ns) = Y(Nb)-Z(Nb)
           end do
        end do
!
!       smooth spectra
        call moving_average( Diagonal_Noise%NoiseNedl_I,     &
                             Spectrum(1)%N_Sample,           &
                             Performance_Param%SlideFilter,  &
                             Diagonal_Noise%SmoothNoiseNedl_I )
        call moving_average( Diagonal_Noise%NoiseNedt_I,      &
                             Spectrum(1)%N_Sample,            &
                             Performance_Param%SlideFilter,   &
                             Diagonal_Noise%SmoothNoiseNedt_I )
        call moving_average( Diagonal_Noise%DriftT_I,      &
                             Spectrum(1)%N_Sample,         &
                             Performance_Param%SlideFilter,&
                             Diagonal_Noise%SmoothDriftT_I )
        call moving_average( Diagonal_Noise%Spectrum_Avg_I,      &
                             Spectrum(1)%N_Sample,               &
                             Performance_Param%SlideFilter,      &
                             Diagonal_Noise%SmoothSpectrum_Avg_I )
     end if
!
!    deallocation
     deallocate( X )
     deallocate( Y )
     deallocate( Z )
     deallocate( W )
!
     return
999  write(*,*) 'diagonal_noise_spectrum Fatal Error',iPos
   end subroutine diagonal_noise_spectrum
!
!
   subroutine correlation_noise_matrix( Performance_Param,&
                                        Nb_Spectrum,      &
                                        Spectrum,         &
                                        Diagonal_Noise,   &
                                        Matrix_Noise      )
   implicit None
     type(type_Performance)                    ,intent(in)   :: Performance_Param
     integer(kind=LONG)                        ,intent(in)   :: Nb_Spectrum
     type(type_Spectrum),dimension(Nb_Spectrum),intent(inout):: Spectrum
     type(type_Diagonal_Noise)                 ,intent(in)   :: Diagonal_Noise
     type(type_Matrix_Noise)                   ,intent(out)  :: Matrix_Noise
!
     integer(kind=LONG)                                      :: iPos
     integer(kind=LONG)                                      :: Nb
     integer(kind=LONG)                                      :: Ns
     integer(kind=LONG)                                      :: Nss
     integer(kind=LONG)                                      :: index
     real(kind=DOUBLE)        ,dimension(:),allocatable      :: X
     real(kind=DOUBLE)        ,dimension(:,:),allocatable    :: Y_R
     real(kind=DOUBLE)        ,dimension(:,:),allocatable    :: Y_I
     real(kind=DOUBLE)        ,dimension(:),allocatable      :: Z
     real(kind=DOUBLE)                                       :: X_Avg
!
     iPos = 0
     if( Nb_Spectrum < Performance_Param%Col_Size_Min ) then
        write(*,*) 'Not enough spectra in the collection', Nb_Spectrum
        go to 999
     end if
     do Nb = 1, Nb_Spectrum
        iPos = Nb
        if( Spectrum(Nb)%N_Sample /= Spectrum(1)%N_Sample ) then
           write(*,*) 'Wrong Spectrum sample number 1 Nb ', Nb, &
                       Spectrum(1)%N_Sample, Spectrum(Nb)%N_Sample
           go to 999
        end if
!
!       spectrum header transfert to correlation matrix noise
        if( Nb == 1 ) then
           if( Spectrum(Nb)%Type == 'C' ) then
              Matrix_Noise%Type     = 'RI'
           else if( Spectrum(Nb)%Type == 'R' ) then
              Matrix_Noise%Type     = 'R'
           else if( Spectrum(Nb)%Type == 'I' ) then
              Matrix_Noise%Type     = 'I'
           else
              write(*,*) 'Spectrum Type Error must be C RI R'
              go to 999
           end if
!
!          type coherence check
           if( Matrix_Noise%Type /= Diagonal_Noise%Type ) then
              write(*,*) 'Matrix and Diagonal Type must be the same',&
                          Matrix_Noise%Type, Diagonal_Noise%Type
              go to 999
           end if
           Matrix_Noise%N_Sample = Spectrum(1)%N_Sample
           Matrix_Noise%Ns_First = Spectrum(1)%Ns_First
           Matrix_Noise%Ns_Last  = Spectrum(1)%Ns_Last
           Matrix_Noise%Wn_First = Spectrum(1)%Wn_First
           Matrix_Noise%Wn_Last  = Spectrum(1)%Wn_Last
           Matrix_Noise%WnMax    = Spectrum(1)%WnMax
           Matrix_Noise%dWn      = Spectrum(1)%dWn
           call alloc_Matrix_Noise( Matrix_Noise )
           Matrix_Noise%Wn(1:Spectrum(1)%N_Sample) = &
                               Spectrum(1)%Wn(1:Spectrum(1)%N_Sample)
        end if
!
!       transform spectrum type to RI
        if( Spectrum(Nb)%Type == 'C' ) then
           Spectrum(Nb)%Type = 'RI'
           allocate( Spectrum(Nb)%Real_Part(Spectrum(Nb)%N_Sample) )
           allocate( Spectrum(Nb)%Imag_Part(Spectrum(Nb)%N_Sample) )
           Spectrum(Nb)%Real_Part(1:Spectrum(Nb)%N_Sample) = &
              dreal( Spectrum(Nb)%Complex(1:Spectrum(Nb)%N_Sample))
           Spectrum(Nb)%Imag_Part(1:Spectrum(Nb)%N_Sample) = &
              dimag( Spectrum(Nb)%Complex(1:Spectrum(Nb)%N_Sample))
           deallocate( Spectrum(Nb)%Complex )
        else if( Spectrum(Nb)%Type == 'R' .or. &
                 Spectrum(Nb)%Type == 'I' .or. &
                 Spectrum(Nb)%Type == 'RI' ) then
!          Nothing to do
        else
           write(*,*) 'Spectrum Type Error must be C RI R'
           go to 999
        end if
     end do
!
!    central date
     Matrix_Noise%Date     = Spectrum(int(Nb_Spectrum/2))%Date
!
!    loop on the spectral samples
     allocate( X(Nb_Spectrum) )
!
!    compute the centered signal real part
     if( trim(Matrix_Noise%Type) == 'RI' .or. &
         trim(Matrix_Noise%Type) == 'R' ) then
        allocate( Y_R(Spectrum(1)%N_Sample, Nb_Spectrum) )
        do Ns = 1, Spectrum(1)%N_Sample
           do Nb = 1, Nb_Spectrum
              X(Nb) = Spectrum(Nb)%Real_Part(Ns)
           end do
           call average( Nb_Spectrum, X, X_Avg )
!
!          centred signal
           Y_R(Ns,1:Nb_Spectrum) = X(1:Nb_Spectrum) - X_Avg
        end do
     end if
!
!    compute the centered signal imaginary part
     if( trim(Matrix_Noise%Type) == 'RI' .or. &
         trim(Matrix_Noise%Type) == 'I' ) then
         allocate( Y_I( Spectrum(1)%N_Sample, Nb_Spectrum ) )
        do Ns = 1, Spectrum(1)%N_Sample
           do Nb = 1, Nb_Spectrum
              X(Nb) = Spectrum(Nb)%Imag_Part(Ns)
           end do
           call average( Nb_Spectrum, X, X_Avg )
!
!          centred signal
           Y_I(Ns,1:Nb_Spectrum) = X(1:Nb_Spectrum) - X_Avg
        end do
     end if
     deallocate( X )
!
!    partial deallocation
     do Nb = 2, Nb_Spectrum
        call dalloc_Spectrum( Spectrum(Nb) )
     end do
!
!    real part
     if( trim(Matrix_Noise%Type) == 'RI' .or. &
         trim(Matrix_Noise%Type) == 'R' ) then
!
!       Compute the Variance term
        allocate( Z(Spectrum(1)%N_Sample) )
        Z(1:Spectrum(1)%N_Sample) = &
            sum( Y_R(1:Spectrum(1)%N_Sample,1:Nb_Spectrum)**2 )
!
        if( trim(Matrix_Noise%Matrix_Type) == 'CovarCorrel' ) then
!
!         Compute the covariance (vector)
          index = 0
          do Ns = 1, Spectrum(1)%N_Sample
            do Nss = 1, Ns
              index = index + 1
              Matrix_Noise%Covariance_R(index) =                            &
                          sum( Y_R(Ns,1:Nb_Spectrum)*Y_R(Nss,1:Nb_Spectrum) )
              if( Matrix_Noise%Covariance_R(index) < 0_DOUBLE ) then
                 Matrix_Noise%Covariance_R(index) = 0_DOUBLE
              end if
            end do
          end do
!
!         Compute the correlation (vector)
          index = 0
          do Ns = 1, Spectrum(1)%N_Sample
            do Nss = 1, Ns
              index = index + 1
              if ( Z(Ns) > 0_DOUBLE .and.                        &
                   Matrix_Noise%Covariance_R(index) > 0_DOUBLE ) then
                 Matrix_Noise%Correlation_R(index) = &
                            Matrix_Noise%Covariance_R(index) &
                          / dsqrt( Z(Ns)*Z(Nss) )
              else
                 Matrix_Noise%Correlation_R(index) = 0_DOUBLE
              end if
            end do
          end do
!
        else if( trim(Matrix_Noise%Matrix_Type) == 'Covariance'  ) then
!
!         Compute the covariance (vector)
          index = 0
          do Ns = 1, Spectrum(1)%N_Sample
            do Nss = 1, Ns
              index = index + 1
              Matrix_Noise%Covariance_R(index) =                            &
                          sum( Y_R(Ns,1:Nb_Spectrum)*Y_R(Nss,1:Nb_Spectrum) )
              if( Matrix_Noise%Covariance_R(index) < 0_DOUBLE ) then
                 Matrix_Noise%Covariance_R(index) = 0_DOUBLE
              end if
            end do
          end do
!
        else if( trim(Matrix_Noise%Matrix_Type) == 'Correlation'  ) then
!
!         Compute the correlation (vector)
          index = 0
          do Ns = 1, Spectrum(1)%N_Sample
            do Nss = 1, Ns
              index = index + 1
              if ( Z(Ns) > 0_DOUBLE ) then
                 Matrix_Noise%Correlation_R(index) = &
                          sum( Y_R(Ns,1:Nb_Spectrum)*Y_R(Nss,1:Nb_Spectrum) ) &
                          / dsqrt( Z(Ns)*Z(Nss) )
                 if( Matrix_Noise%Correlation_R(index) < 0_DOUBLE ) then
                    Matrix_Noise%Correlation_R(index) = 0_DOUBLE
                 end if
              else
                 Matrix_Noise%Correlation_R(index) = 0_DOUBLE
              end if
            end do
          end do
        else
           write(*,*) 'Matrix_Type Error ',Matrix_Noise%Matrix_Type
           go to 999
        end if
        deallocate( Y_R )
        deallocate( Z)
     end if
!
!    Diagonal term imaginary part
     if( trim(Matrix_Noise%Type) == 'RI' .or. &
         trim(Matrix_Noise%Type) == 'I' ) then
!
        allocate( Z(Spectrum(1)%N_Sample) )
        Z(1:Spectrum(1)%N_Sample) = &
            sum( Y_I(1:Spectrum(1)%N_Sample,1:Nb_Spectrum)**2 )
!
        if( trim(Matrix_Noise%Matrix_Type) == 'CovarCorrel' ) then
!
!         Compute the covariance (vector)
          index = 0
          do Ns = 1, Spectrum(1)%N_Sample
            do Nss = 1, Ns
              index = index + 1
              Matrix_Noise%Covariance_I(index) =                            &
                          sum( Y_I(Ns,1:Nb_Spectrum)*Y_I(Nss,1:Nb_Spectrum) )
              if( Matrix_Noise%Covariance_I(index) < 0_DOUBLE ) then
                 Matrix_Noise%Covariance_I(index) = 0_DOUBLE
              end if
            end do
          end do
!
!         Compute the correlation (vector)
          index = 0
          do Ns = 1, Spectrum(1)%N_Sample
            do Nss = 1, Ns
              index = index + 1
              if ( Z(Ns) > 0_DOUBLE .and.                        &
                   Matrix_Noise%Covariance_I(index) > 0_DOUBLE ) then
                 Matrix_Noise%Correlation_I(index) = &
                            Matrix_Noise%Covariance_I(index) &
                          / dsqrt( Z(Ns)*Z(Nss) )
              else
                 Matrix_Noise%Correlation_I(index) = 0_DOUBLE
              end if
            end do
          end do
!
        else if( trim(Matrix_Noise%Matrix_Type) == 'Covariance'  ) then
!
!         Compute the covariance (vector)
          index = 0
          do Ns = 1, Spectrum(1)%N_Sample
            do Nss = 1, Ns
              index = index + 1
              Matrix_Noise%Covariance_I(index) =                            &
                          sum( Y_I(Ns,1:Nb_Spectrum)*Y_I(Nss,1:Nb_Spectrum) )
              if( Matrix_Noise%Covariance_I(index) < 0_DOUBLE ) then
                 Matrix_Noise%Covariance_I(index) = 0_DOUBLE
              end if
            end do
          end do
!
        else if( trim(Matrix_Noise%Matrix_Type) == 'Correlation'  ) then
!
!         Compute the correlation (vector)
          index = 0
          do Ns = 1, Spectrum(1)%N_Sample
            do Nss = 1, Ns
              index = index + 1
              if ( Z(Ns) > 0_DOUBLE ) then
                 Matrix_Noise%Correlation_I(index) = &
                          sum( Y_I(Ns,1:Nb_Spectrum)*Y_I(Nss,1:Nb_Spectrum) ) &
                          / dsqrt( Z(Ns)*Z(Nss) )
                 if( Matrix_Noise%Correlation_I(index) < 0_DOUBLE ) then
                    Matrix_Noise%Correlation_I(index) = 0_DOUBLE
                 end if
              else
                 Matrix_Noise%Correlation_I(index) = 0_DOUBLE
              end if
            end do
          end do
        else
           write(*,*) 'Matrix_Type Error ',Matrix_Noise%Matrix_Type
           go to 999
        end if
        deallocate( Y_I )
        deallocate( Z)
     end if
!
     return
999  write(*,*) 'correlation_noise_matrix Fatal Error',iPos
   end subroutine correlation_noise_matrix
!
!
end module radiometry_module
