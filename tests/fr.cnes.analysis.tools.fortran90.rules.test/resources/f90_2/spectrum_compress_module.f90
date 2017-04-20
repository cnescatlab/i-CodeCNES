!!#  spectrum_compress_module.f90 --
!!# 
!!#            Project: SPS_GENERIC
!!#            Authors: NOVELTIS/B.TOURNIER
!!#               Date: october 2011
!!#            Version: $Revision: 1.3 $
!!#  Last modification: $Date: 2011-11-02 16:26:36 $
!!#
!!#  Language:  F90
!!#  Standards: Noveltis
!!#
!!# --
!!#
!!
!> spectrum compression -- Module
!!
!! * Purpose
!!
!!    Module for spectrum compression.
!
!
module spectrum_compress_module
   use precision_type
   use error_type
   use constantes_type
   use interf_param_type
   use interferogram_type
   use spectral_l1c_mb_type
   use spectrum_type
   use math_module
   use fft_module
   use spectrum_module
!
   implicit none
!
!
   public :: spectrum_to_interf,      &
             interf_to_spectrum,      &
             interf_resamp_lagrange,  &
             interf_resamp_spline,    &
             norm_interf
!
   contains
!
!
   subroutine spectrum_to_interf( Spectral_hr, &
                                  SB,          &
                                  Spectrum_hr, &
                                  Interf_hr    )
   implicit none
     type(type_Spectral_L1c_mb),intent(in)                 :: Spectral_hr
     integer(kind=LONG)        ,intent(in)                 :: SB
     type(type_Spectrum)       ,intent(inout)              :: Spectrum_hr
     type(type_Interferogram)  ,intent(out)                :: Interf_hr
     integer(kind=LONG)                                    :: N_Sample
     integer(kind=LONG)                                    :: NsFft
     real(kind=DOUBLE)                                     :: Opd_Pivot
     real(kind=DOUBLE)                                     :: Time_Pivot
     integer(kind=LONG)                                    :: ErrCode
!
     write(*,'(a20,i10)')   'Spectrum_hr%N_Sample',Spectrum_hr%N_Sample
     write(*,'(a20,i10)')   'Spectrum_hr%Ns_First',Spectrum_hr%Ns_First
     write(*,'(a20,i10)')   'Spectrum_hr%Ns_Last ',Spectrum_hr%Ns_Last
     write(*,'(a20,f10.2)') 'Spectrum_hr%Wn_First',Spectrum_hr%Wn_First
     write(*,'(a20,f10.2)') 'Spectrum_hr%Wn_Last ',Spectrum_hr%Wn_Last
     write(*,'(a20,f10.2)') 'Spectrum_hr%WnMax   ',Spectrum_hr%WnMax
     write(*,'(a20,f10.2)') 'Spectrum_hr%dWn     ',Spectrum_hr%dWn
!
!    spectrum hr edge smoothing
     if( Spectral_hr%SigS(SB) /= 0 ) then
        if( Spectrum_hr%Type == 'R' ) then
           call windowing( Spectrum_hr%N_Sample, &
                           Spectrum_hr%Real_Part,&
                           Spectral_hr%SigS(SB), &
                           Spectrum_hr%dWn       )
        else if( Spectrum_hr%Type == 'I' ) then
           call windowing( Spectrum_hr%N_Sample, &
                           Spectrum_hr%Imag_Part,&
                           Spectral_hr%SigS(SB), &
                           Spectrum_hr%dWn       )
        else if( Spectrum_hr%Type == 'C' ) then
           call windowingcplx( Spectrum_hr%N_Sample,&
                               Spectrum_hr%Complex, &
                               Spectral_hr%SigS(SB),&
                               Spectrum_hr%dWn      )
        else if( Spectrum_hr%Type == 'RI' ) then
           call windowing( Spectrum_hr%N_Sample, &
                           Spectrum_hr%Real_Part,&
                           Spectral_hr%SigS(SB), &
                           Spectrum_hr%dWn       )
           call windowing( Spectrum_hr%N_Sample, &
                           Spectrum_hr%Imag_Part,&
                           Spectral_hr%SigS(SB), &
                           Spectrum_hr%dWn       )
        else
           write(*,*) 'Unexpected Spectrum_hr%Type : ',Spectrum_hr%Type
           write(*,*) 'spectrum_to_interf Fatal Error'
           call exit(1)
        end if
     end if
!
!    FFT size computation
     N_Sample  = 2 * idnint( Spectrum_hr%WnMax/Spectrum_hr%dWn )
!
     call tabule_NsFft( N_Sample, NsFft, ErrCode )
     if( ErrCode /= 0 ) then
        write(*,*) 'tabule_NsFft Error : ',N_Sample,NsFft
        write(*,*) 'spectrum_ti_interf error'
        call exit(1)
     end if
!
!    Real hr interferogram parametrisation
     Interf_hr%Type     = 'R'
     Interf_hr%N_Sample = NsFft + 1
     Interf_hr%N_Top    = 0
     Interf_hr%OpdMax   = 1.d+00 / ( 2.d+00*Spectrum_hr%dWn )
     Interf_hr%dOpd     = Interf_hr%OpdMax &
                        / dble(int(Interf_hr%N_Sample/2))
     Interf_hr%dTime    = Interf_hr%dOpd
     Interf_hr%TimeMax  = Interf_hr%OpdMax
     call alloc_Interferogram( Interf_hr )
     call Interferogram_Basis( Interf_hr )
     Opd_Pivot = Interf_hr%Opd(int(Interf_hr%N_Sample/2)+1)
     Interf_hr%Opd(1:Interf_hr%N_Sample) = &
                 Interf_hr%Opd(1:Interf_hr%N_Sample) - Opd_Pivot
     Time_Pivot = Interf_hr%Time(int(Interf_hr%N_Sample/2)+1)
     Interf_hr%Time(1:Interf_hr%N_Sample) = &
                 Interf_hr%Time(1:Interf_hr%N_Sample) - Opd_Pivot
     write(*,'(a20,i10)')   'Interf_hr%N_Sample',Interf_hr%N_Sample
     write(*,'(a20,f10.7)') 'Interf_hr%OpdMax  ',Interf_hr%OpdMax
     write(*,'(a20,f10.7)') 'Interf_hr%dOpd    ',Interf_hr%dOpd
     write(*,'(a20,f10.7)') 'Interf_hr%Opd(-x) ',Interf_hr%Opd(1)
     write(*,'(a20,f10.7)') 'Interf_hr%Opd(0)  ',&
                             Interf_hr%Opd(Interf_hr%N_Sample/2+1)
     write(*,'(a20,f10.7)') 'Interf_hr%Opd(+x) ',&
                             Interf_hr%Opd(Interf_hr%N_Sample)
!
!    Fourier transform of the high resolution spectrum
     call sptoif( Spectrum_hr, Interf_hr )
!
     return
!
   end subroutine spectrum_to_interf
!
!

!
!
   subroutine interf_to_spectrum( Spectral_lr, &
                                  Interf_Param,&
                                  N_Lagrange,  &
                                  SB, Noise,   &
                                  File_Noise,  &
                                  Interf_hr,   &
                                  Interf_lr,   &
                                  Spectrum_lr  )
   implicit none
     type(type_Spectral_L1c_mb),intent(in)                 :: Spectral_lr
     type(type_Interf_Param)   ,intent(in)                 :: Interf_Param
     integer(kind=LONG)        ,intent(in)                 :: N_Lagrange
     integer(kind=LONG)        ,intent(in)                 :: SB
     character(len=*)          ,intent(in)                 :: Noise
     character(len=*)          ,intent(in)                 :: File_Noise
     type(type_Interferogram)  ,intent(in)                 :: Interf_hr
     type(type_Interferogram)  ,intent(out)                :: Interf_lr
     type(type_Spectrum)       ,intent(out)                :: Spectrum_lr
     type(type_Spectrum)                                   :: Spectrum_Noise
     integer(kind=LONG)                                    :: OSFactor
     integer(kind=LONG)                                    :: N_Sample
     integer(kind=LONG)                                    :: NsFft
     real(kind=DOUBLE)                                     :: Opd_Pivot
     real(kind=DOUBLE)                                     :: Time_Pivot
     integer(kind=LONG)                                    :: ErrCode
!
!    spectrum parametrisation
     Spectrum_lr%Type     = 'R'
     Spectrum_lr%Wn_First = Spectral_lr%WnS(SB)
     Spectrum_lr%Wn_Last  = Spectral_lr%WnE(SB)
     Spectrum_lr%dWn      = Spectral_lr%dWn(SB)
     Spectrum_lr%Ns_First = idnint( Spectrum_lr%Wn_First&
                                  / Spectrum_lr%dWn ) + 1
     Spectrum_lr%Ns_Last  = idnint( Spectrum_lr%Wn_Last &
                                  / Spectrum_lr%dWn ) + 1
     Spectrum_lr%Wn_First = dble(Spectrum_lr%Ns_First-1)&
                          * Spectrum_lr%dWn
     Spectrum_lr%Wn_Last  = dble(Spectrum_lr%Ns_Last-1) &
                          * Spectrum_lr%dWn
     Spectrum_lr%WnMax    = 1_DOUBLE / ( 2_DOUBLE * Interf_hr%dOpd )
     Spectrum_lr%N_Sample = Spectrum_lr%Ns_Last - Spectrum_lr%Ns_First + 1
     call alloc_Spectrum( Spectrum_lr )
     call Spectrum_Basis( Spectrum_lr )
!
     write(*,'(a20,i10)')   'Spectrum_lr%N_Sample',Spectrum_lr%N_Sample
     write(*,'(a20,i10)')   'Spectrum_lr%Ns_First',Spectrum_lr%Ns_First
     write(*,'(a20,i10)')   'Spectrum_lr%Ns_Last ',Spectrum_lr%Ns_Last
     write(*,'(a20,f10.2)') 'Spectrum_lr%Wn_First',Spectrum_lr%Wn_First
     write(*,'(a20,f10.2)') 'Spectrum_lr%Wn_Last ',Spectrum_lr%Wn_Last
     write(*,'(a20,f10.2)') 'Spectrum_lr%WnMax   ',Spectrum_lr%WnMax
     write(*,'(a20,f10.2)') 'Spectrum_lr%dWn     ',Spectrum_lr%dWn
!
!    FFT size computation
     N_Sample  = 2 * idnint( Spectrum_lr%WnMax/Spectrum_lr%dWn )
!
     ErrCode = 0
     call tabule_NsFft( N_Sample, NsFft, ErrCode )
     if( ErrCode /= 0 ) then
        write(*,*) 'tabule_NsFft Error : ',N_Sample,NsFft
        write(*,*) 'interf_to_spectrum error'
        call exit(1)
     end if
     NsFft = NsFft * Interf_Param%OSFactor
!
     Interf_lr%Type     = Interf_hr%Type
     Interf_lr%OpdMax   = 1.d+00 / ( 2.d+00 * Spectrum_lr%dWn )
     Interf_lr%N_Sample = NsFft + 1
     Interf_lr%N_Top    = 0
     Interf_lr%dOpd     = 2.d+00 * Interf_lr%OpdMax / dble(NsFft)
     Interf_lr%dTime    = Interf_lr%dOpd
     Interf_lr%TimeMax  = Interf_lr%OpdMax
     call alloc_Interferogram( Interf_lr )
     call Interferogram_Basis( Interf_lr )
     Opd_Pivot = Interf_lr%Opd(int(Interf_lr%N_Sample/2)+1)
     Interf_lr%Opd(1:Interf_lr%N_Sample) = &
                 Interf_lr%Opd(1:Interf_lr%N_Sample) - Opd_Pivot
     Time_Pivot = Interf_lr%Time(int(Interf_lr%N_Sample/2)+1)
     Interf_lr%Time(1:Interf_lr%N_Sample) = &
                 Interf_lr%Time(1:Interf_lr%N_Sample) - Time_Pivot
     write(*,'(a20,i10)')   'Interf_lr%N_Sample',Interf_lr%N_Sample
     write(*,'(a20,f10.7)') 'Interf_lr%OpdMax  ',Interf_lr%OpdMax
     write(*,'(a20,f10.7)') 'Interf_lr%dOpd    ',Interf_lr%dOpd
     write(*,'(a20,f10.7)') 'Interf_lr%Opd(-x) ',Interf_lr%Opd(1)
     write(*,'(a20,f10.7)') 'Interf_lr%Opd(0)  ',&
                             Interf_lr%Opd(Interf_lr%N_Sample/2+1)
     write(*,'(a20,f10.7)') 'Interf_lr%Opd(+x) ',&
                             Interf_lr%Opd(Interf_lr%N_Sample)
!
!    Interferogram resampling
     if( N_Lagrange /= 0 ) then
!
!       Lagrange interpolation
        call interf_resamp_lagrange( Interf_hr, N_Lagrange, Interf_lr )
     else
!
!       Cubic spline interpolation
        call interf_resamp_spline( Interf_hr,  Interf_lr )
     end if
!
!    spectrum computation
     call iftosp( Interf_lr, Spectrum_lr )
!
!    add spectrum noise
     if( Noise == 'YES' ) then
        OSFactor = 1
        call spectrum_noise_init( File_Noise, Spectrum_lr, Spectrum_Noise )
        call add_spectrum_noise( Spectrum_Noise, OSFactor, Spectrum_lr )
        call dalloc_Spectrum( Spectrum_Noise )
     end if
!
!
     return
!
   end subroutine interf_to_spectrum

!
!
   subroutine interf_resamp_lagrange( Interf_lr, N_Lagrange, Interf )
   implicit none
     type(type_Interferogram) ,intent(in)                 :: Interf_lr
     integer(kind=LONG)       ,intent(in)                 :: N_Lagrange
     type(type_Interferogram) ,intent(inout)              :: Interf
     integer(kind=LONG)                                   :: n1
     integer(kind=LONG)                                   :: n2
     integer(kind=LONG)                                   :: NRs
     integer(kind=LONG)                                   :: Ns1
     integer(kind=LONG)                                   :: Ns2
     real(kind=DOUBLE)        ,dimension(:),allocatable   :: Lagr
     integer(kind=LONG)                                   :: N_Lagr1
     integer(kind=LONG)                                   :: N_Lagr2
     integer(kind=LONG)                                   :: N_Lagr
     integer(kind=LONG)                                   :: N
!
!    Lagrange interpolation initialisation
     N_Lagr1 = -int(N_Lagrange/2)
     N_Lagr2 =  int(N_Lagrange/2)
     write(*,*) 'N_Lagr1 N_Lagr2', N_Lagr1, N_Lagr2
     allocate( Lagr(N_Lagr1:N_Lagr2) )
!
!    verification of the desired opd limits
     n1 = 1
     n2 = Interf_lr%N_Sample
     call dichotomd( Interf%Opd(1),        &
                     Interf_lr%Opd, n1, n2 )
     Ns1 = n1+N_Lagr1
     n1 = 1
     n2 = Interf_lr%N_Sample
     call dichotomd( Interf%Opd(Interf%N_Sample),&
                     Interf_lr%Opd, n1, n2       )
     Ns2 = n1+N_Lagr2
     if( (Ns1 < 1) .or. (Ns2 > Interf_lr%N_Sample) ) then
        write(*,*) 'Interpolator limits ERROR',Ns1,Ns2
        call exit(1)
     end if
!
!    interferogram resampling
     do NRs = 1, Interf%N_Sample
!
!       Stored sample (Ns) closest to desired sample (NRs)
        n1 = 1
        n2 = Interf_lr%N_Sample
        call dichotomd( Interf%Opd(NRs),      &
                        Interf_lr%Opd, n1, n2 )
!
!       Lagrange limits
        Ns1 = n1+N_Lagr1
        Ns2 = n1+N_Lagr2
!
!       Lagrange interpolator
        do N_Lagr = N_Lagr1, N_Lagr2
           Lagr(N_Lagr) = 1.d+00
           do N = N_Lagr1, N_Lagr2
              if( N /= N_Lagr ) then
                 Lagr(N_Lagr) = Lagr(N_Lagr)  &
                  * ( Interf%Opd(NRs)         &
                     -Interf_lr%Opd(n1+N) )   &
                  / ( Interf_lr%Opd(n1+N_Lagr)&
                     -Interf_lr%Opd(n1+N) )
              end if
           end do
        end do
!
!       Interpolated sample
        Interf%Real_Part(NRs) = sum(Interf_lr%Real_Part(Ns1:Ns2)&
                                        * Lagr(N_Lagr1:N_Lagr2))&
                              / sum(Lagr(N_Lagr1:N_Lagr2))
     end do
     
!    Opd sampling ratio for radiance normalisation
     call norm_interf( Interf_lr, Interf )

     deallocate( Lagr )
!
     return
!
   end subroutine interf_resamp_lagrange
!
!
   subroutine interf_resamp_spline( Interf_lr, Interf )
   implicit none
     type(type_Interferogram) ,intent(in)                 :: Interf_lr
     type(type_Interferogram) ,intent(inout)              :: Interf
!
!    Interpolation limits verification
     if( (Interf%Opd(1) < Interf_lr%Opd(1)) .or. &
         (Interf_lr%Opd(Interf_lr%N_Sample) <    &
          Interf%Opd(Interf%N_Sample)) ) then
        write(*,*) 'Interpolation limits Error'
        write(*,*) 'Interf-in  ',Interf_lr%Opd(1),&
                                 Interf_lr%Opd(Interf_lr%N_Sample)
        write(*,*) 'Interf-out ',Interf%Opd(1),&
                                 Interf%Opd(Interf%N_Sample)
        write(*,*) 'interf_resamp_spline Fatal Error'
        call exit(1)
     end if
!
!    interferogram resampling
     call intspline_open( Interf_lr%N_Sample, &
                          Interf_lr%Opd,      &
                          Interf_lr%Real_Part,&
                          Interf%N_Sample,    &
                          Interf%Opd,         &
                          Interf%Real_Part    )
     
!    Opd sampling ratio for radiance normalisation
     call norm_interf( Interf_lr, Interf )
!
     return
!
   end subroutine interf_resamp_spline
!
!
   subroutine norm_interf( Interf_in, Interf_out )
     implicit none
     type(type_Interferogram) ,intent(in)      :: Interf_in    
     type(type_Interferogram) ,intent(inout)   :: Interf_out
     real(kind=DOUBLE)                         :: Ratio

     Ratio = Interf_in%dOpd / Interf_out%dOpd
     Interf_out%Real_Part(1:Interf_out%N_Sample) = &
          Interf_out%Real_Part(1:Interf_out%N_Sample) * Ratio

     return
   end subroutine norm_interf
!
!
end module spectrum_compress_module
