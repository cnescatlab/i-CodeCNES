! convolution_module.f90 --!
!           Project: SPS_GENERIC
!           Authors: NOVELTIS/B.TOURNIER
!              Date: october 2009
!           Version: $Revision: 1.16 $
! Last modification: $Date: 2012-02-08 10:19:14 $
!
! Convolution -- Module
!
! * Purpose
!   Module for Fourier Transform
!
module convolution_module
   use precision_type
   use error_type
   use constantes_type
   use srf_param_type
   use general_type
   use spectrum_type
   use interferogram_type
   use saf_type
   use srf_type
   use math_module
   use fft_module
!
   implicit none
!
!
   public ::                      &
             calc_gauss,          &
             calc_gauss_tronc,    &
             calc_sinc,           &
             calc_hamming,        &
             calc_blackman_harris,&
             cnv_sp_fct,          &
             cnv_ft_fct,          &
             cnv_sp,              &
             cnv_ft
             
!
!
   contains
!
!
  subroutine calc_gauss( Cnv_Fct )
  implicit none
      type(type_Cnv_Fct), intent(inout)                         :: Cnv_Fct
      real(kind=DOUBLE)                                         :: dln2
      real(kind=DOUBLE)                                         :: coef
      real(kind=DOUBLE)                                         :: coeff
      real(kind=DOUBLE)                                         :: Norm
      real(kind=DOUBLE)                                         :: sigma_Opd
!
!    Parameters which have to be defined outside of the subroutine
!    Cnv_Fct%OpdMax   : maximum optical path
!    Cnv_Fct%Ns_Opd   : number of samples descibing the optical path
!    Cnv_Fct%N_Sample : number of samples describing the spectral domain
!    Cnv_Fct%FullSDWn : full width of the SRF spectral domain
!    Cnv_Fct%FWHM     : full width at half maximum
!    Cnv_Fct%WnShift  : spectral shift to be applied
!
     Cnv_Fct%Opd_effective = Cnv_Fct%OpdMax
     Cnv_Fct%dOpd = 2.d+00 * Cnv_Fct%OpdMax / dble(Cnv_Fct%Ns_Opd-1)
     Cnv_Fct%dWn  = Cnv_Fct%FullSDWn / dble(Cnv_Fct%N_Sample-1)
     dln2         = log(2.d+00)
     call alloc_Cnv_Fct( Cnv_Fct )
     call Cnv_Fct_Basis( Cnv_Fct )
!    constants computation
     coef      = dln2/(Cnv_Fct%FWHM/2.d+00)**2
     coeff     = dsqrt(dln2/Pi)/(Cnv_Fct%FWHM/2.d+00)*Cnv_Fct%dWn
!    Gaussian computation
     Cnv_Fct%Fct(1:Cnv_Fct%N_Sample) = coeff               &
             * dexp(-coef*( Cnv_Fct%Wn(1:Cnv_Fct%N_Sample) &
                           -Cnv_Fct%WnShift )**2)
!    Surface normalisation
     Norm                            = sum( Cnv_Fct%Fct(1:Cnv_Fct%N_Sample) )
     Cnv_Fct%Fct(1:Cnv_Fct%N_Sample) = Cnv_Fct%Fct(1:Cnv_Fct%N_Sample) / Norm
!
!    Gaussian computation in the interferogram space
     sigma_Opd = dln2 / ( Pi * Cnv_Fct%FWHM / 2.d+00 )
     Cnv_Fct%Fct_TF(1:Cnv_Fct%Ns_Opd) =                            &
                     dexp( -dln2 * ( Cnv_Fct%Opd(1:Cnv_Fct%Ns_Opd) &
                                   / sigma_Opd )**2 )
     return
  end subroutine calc_gauss
!
!
  subroutine calc_gauss_tronc( NsFft, Cnv_Fct )
  implicit none
     integer(kind=LONG) ,intent(in)                            :: NsFft
     type(type_Cnv_Fct) ,intent(inout)                         :: Cnv_Fct
     integer(kind=LONG)                                        :: OSFactor
     integer(kind=LONG)                                        :: OSNsFft
     real(kind=DOUBLE)                                         :: dOpd
     real(kind=DOUBLE)                                         :: sigma_Opd
     integer(kind=LONG)                                        :: Ns0
     integer(kind=LONG)                                        :: Ns
     integer(kind=LONG)                                        :: NsDel
     integer(kind=LONG)                                        :: allocerror
     integer(kind=LONG)                                        :: ErrCode
     real(kind=DOUBLE)  ,dimension(:) ,allocatable             :: Opd
     real(kind=DOUBLE)  ,dimension(:) ,allocatable             :: GTF
     real(kind=DOUBLE)  ,dimension(:) ,allocatable             :: Isrf
     integer(kind=LONG)                                        :: Sens
     integer(kind=LONG)                                        :: Nsfirst
     integer(kind=LONG)                                        :: Nslast
     real(kind=DOUBLE)                                         :: Norm
!
!    Oversampling
     OSFactor = 16
!
!    constants
     Cnv_Fct%dWn    = 1.d+00 / ( 2.d+00 * Cnv_Fct%OpdMax * dble(OSFactor) )
     Cnv_Fct%N_Sample = 2*idnint(Cnv_Fct%FullSDWn/2/Cnv_Fct%dWn) + 1
     write(*,*) 'Cnv_Fct%FWHM    ',Cnv_Fct%FWHM
     write(*,*) 'Cnv_Fct%FullSDWn',Cnv_Fct%FullSDWn
     write(*,*) 'Cnv_Fct%Ns_Opd  ',Cnv_Fct%Ns_Opd
     write(*,*) 'Cnv_Fct%OpdMax  ',Cnv_Fct%OpdMax
     write(*,*) 'Cnv_Fct%Opd_eff ',Cnv_Fct%Opd_effective
     write(*,*) 'Cnv_Fct%dOpd    ',Cnv_Fct%dOpd
     write(*,*) 'Cnv_Fct%dWn     ',Cnv_Fct%dWn
     write(*,*) 'Cnv_Fct%N_Sample',Cnv_Fct%N_Sample
     OSNsFft   = NsFft * OSFactor
     dOpd      = Cnv_Fct%OpdMax / dble( int(NsFft/2) )
     sigma_Opd = log(2.d+00) / ( Pi * Cnv_Fct%FWHM / 2.d+00 )
     Ns0 = int(NsFft/2)+1
     NsDel = int(OSNsFft/2) - int(NsFft/2)
     write(*,*) 'OSNsFft  ',OSNsFft
     write(*,*) 'dOpd     ',dOpd
     write(*,*) 'sigma_Opd',sigma_Opd
     write(*,*) 'Ns0      ',Ns0
     write(*,*) 'NsDel    ',NsDel
!
!    Cnv_Fct type allocation
     call alloc_Cnv_Fct( Cnv_Fct )
     call Cnv_Fct_Basis( Cnv_Fct )
!
!    allocation
     allocerror = 0
     allocate(Opd(OSNsFft+1),stat=ErrCode )
     if (ErrCode > 0) allocerror = allocerror + 1
     allocate(GTF(OSNsFft+1),stat=ErrCode )
     if (ErrCode > 0) allocerror = allocerror + 1
     allocate(Isrf(OSNsFft+1),stat=ErrCode )
     if (ErrCode > 0) allocerror = allocerror + 1
!
     if (allocerror > 0) then
        write(0,*) 'allocation srf_L1c_Gauss Error'
        write(0,*) 'srf_L1c_Gauss : fatal error'
        call exit(1)
     end if
!
!    GTF computation
     GTF(1:OSNsFft+1) = 0.d+00
     Opd(1:NsFft+1)   = (/ (dble(Ns-Ns0),Ns=1,NsFft+1) /) * dOpd
     GTF(NsDel+1:NsDel+NsFft+1) = dexp( -log(2.d+00) * ( Opd(1:NsFft+1) &
                                                       / sigma_Opd )**2 )
!
!    Troncated Gaussian computation
     Sens = -1
     call fft_r2r_open( OSNsFft, Sens, GTF, dOpd, Cnv_Fct%dWn, Isrf )
!
!    Usefull srf extraction
     Ns0 = int(OSNsFft/2)+1
     Nsfirst = Ns0 - int( Cnv_Fct%N_Sample/2 )
     Nslast  = Ns0 + int( Cnv_Fct%N_Sample/2 )
     Cnv_Fct%Fct(1:Cnv_Fct%N_Sample) = Isrf(Nsfirst:Nslast)
     
!    Surface normalisation
     Norm                            = sum( Cnv_Fct%Fct(1:Cnv_Fct%N_Sample) )
     Cnv_Fct%Fct(1:Cnv_Fct%N_Sample) = Cnv_Fct%Fct(1:Cnv_Fct%N_Sample) / Norm
!
!    FWHM of the truncated Gaussian
     call fwhm_real( Cnv_Fct%N_Sample, Cnv_Fct%Fct, Cnv_Fct%dWn, Cnv_Fct%FWHM )
!
!    Gaussian computation in the interferogram space
     Cnv_Fct%Fct_TF(1:Cnv_Fct%Ns_Opd) =                            &
              dexp( -log(2.d+00) * ( Cnv_Fct%Opd(1:Cnv_Fct%Ns_Opd) &
                                   / sigma_Opd )**2 )
!
!    deallocation
     deallocate( Opd )
     deallocate( GTF )
     deallocate( Isrf )
     return
  end subroutine calc_gauss_tronc
!
!
  subroutine calc_coscar( NsFft, OpdFraction, Cnv_Fct )
  implicit none
     integer(kind=LONG) ,intent(in)                            :: NsFft
     real(kind=DOUBLE)  ,intent(in)                            :: OpdFraction
     type(type_Cnv_Fct) ,intent(inout)                         :: Cnv_Fct
     integer(kind=LONG)                                        :: OSFactor
     integer(kind=LONG)                                        :: OSNsFft
     real(kind=DOUBLE)                                         :: dOpd
     integer(kind=LONG)                                        :: Ns0
     integer(kind=LONG)                                        :: Ns
     integer(kind=LONG)                                        :: NsDel
     integer(kind=LONG)                                        :: allocerror
     integer(kind=LONG)                                        :: ErrCode
     real(kind=DOUBLE)  ,dimension(:) ,allocatable             :: Opd
     real(kind=DOUBLE)  ,dimension(:) ,allocatable             :: GTF
     real(kind=DOUBLE)  ,dimension(:) ,allocatable             :: Isrf
     integer(kind=LONG)                                        :: Sens
     integer(kind=LONG)                                        :: Nsfirst
     integer(kind=LONG)                                        :: Nslast
     real(kind=DOUBLE)                                         :: Norm
     real(kind=DOUBLE)                                         :: sigma_Opd
     real(kind=DOUBLE)                                         :: Arg
!
!    Parameters which have to be defined outside of the subroutine
!    Cnv_Fct%OpdMax   : maximum optical path
!    Cnv_Fct%Ns_Opd   : number of samples descibing the optical path
!    Cnv_Fct%FullSDWn : full width of the SRF spectral domain
!    Cnv_Fct%WnShift  : spectral shift to be applied
!    NsFft            : nominal fft size for the interferogram
!
!    Oversampling
     OSFactor = 16
!
!    constants
     Cnv_Fct%dWn    = 1.d+00 / ( 2.d+00 * Cnv_Fct%OpdMax * dble(OSFactor) )
     Cnv_Fct%N_Sample = 2*idnint(Cnv_Fct%FullSDWn/2/Cnv_Fct%dWn) + 1
     write(*,*) 'Cnv_Fct%FWHM    ',Cnv_Fct%FWHM
     write(*,*) 'Cnv_Fct%FullSDWn',Cnv_Fct%FullSDWn
     write(*,*) 'Cnv_Fct%Ns_Opd  ',Cnv_Fct%Ns_Opd
     write(*,*) 'Cnv_Fct%OpdMax  ',Cnv_Fct%OpdMax
     write(*,*) 'Cnv_Fct%Opd_eff ',Cnv_Fct%Opd_effective
     write(*,*) 'Cnv_Fct%dOpd    ',Cnv_Fct%dOpd
     write(*,*) 'Cnv_Fct%dWn     ',Cnv_Fct%dWn
     write(*,*) 'Cnv_Fct%N_Sample',Cnv_Fct%N_Sample
     OSNsFft   = NsFft * OSFactor
     dOpd      = Cnv_Fct%OpdMax / dble( int(NsFft/2) )
     sigma_Opd = log(2.d+00) / ( Pi * Cnv_Fct%FWHM / 2.d+00 )
     Ns0 = int(NsFft/2)+1
     NsDel = int(OSNsFft/2) - int(NsFft/2)
     write(*,*) 'OSNsFft  ',OSNsFft
     write(*,*) 'dOpd     ',dOpd
     write(*,*) 'sigma_Opd',sigma_Opd
     write(*,*) 'Ns0      ',Ns0
     write(*,*) 'NsDel    ',NsDel
!
!    Cnv_Fct type allocation
     call alloc_Cnv_Fct( Cnv_Fct )
     call Cnv_Fct_Basis( Cnv_Fct )
!
!    allocation
     allocerror = 0
     allocate(Opd(OSNsFft+1),stat=ErrCode )
     if (ErrCode > 0) allocerror = allocerror + 1
     allocate(GTF(OSNsFft+1),stat=ErrCode )
     if (ErrCode > 0) allocerror = allocerror + 1
     allocate(Isrf(OSNsFft+1),stat=ErrCode )
     if (ErrCode > 0) allocerror = allocerror + 1
!
     if (allocerror > 0) then
        write(0,*) 'allocation srf_L1c_Gauss Error'
        write(0,*) 'srf_L1c_Gauss : fatal error'
        call exit(1)
     end if
!
!    GTF computation
     GTF(1:OSNsFft+1) = 0.d+00
     Opd(1:NsFft+1)   = (/ (dble(Ns-Ns0),Ns=1,NsFft+1) /) * dOpd
!
!    Square cosine computation 
     do Ns = 1, NsFft+1  
        if( dabs(Opd(Ns)) .le. OpdFraction*Cnv_Fct%Opd_effective ) then
           GTF(NsDel+Ns) = 1_DOUBLE
        else if( dabs(Opd(Ns)) .le. Cnv_Fct%Opd_effective ) then
           Arg = Pi*( (dabs(Opd(Ns))-OpdFraction*Cnv_Fct%Opd_effective) &
                    / ((Cnv_Fct%Opd_effective*(1.d+00-OpdFraction))) )**2
           GTF(NsDel+Ns) = 0.5d+00 * ( 1 + dcos( Arg ) ) * 1_DOUBLE
        else
           GTF(NsDel+Ns) = 0.d+00
        end if
     end do
!
!    Spectrum space computation
     Sens = -1
     call fft_r2r_open( OSNsFft, Sens, GTF, dOpd, Cnv_Fct%dWn, Isrf )
!
!    Usefull srf extraction
     Ns0 = int(OSNsFft/2)+1
     Nsfirst = Ns0 - int( Cnv_Fct%N_Sample/2 )
     Nslast  = Ns0 + int( Cnv_Fct%N_Sample/2 )
     Cnv_Fct%Fct(1:Cnv_Fct%N_Sample) = Isrf(Nsfirst:Nslast)
     
!    Surface normalisation
     Norm                            = sum( Cnv_Fct%Fct(1:Cnv_Fct%N_Sample) )
     Cnv_Fct%Fct(1:Cnv_Fct%N_Sample) = Cnv_Fct%Fct(1:Cnv_Fct%N_Sample) / Norm
!
!    FWHM of the function
     call fwhm_real( Cnv_Fct%N_Sample, Cnv_Fct%Fct, Cnv_Fct%dWn, Cnv_Fct%FWHM )
!
!    Computation in the interferogram space
     do Ns = 1, Cnv_Fct%Ns_Opd
        if( dabs(Cnv_Fct%Opd(Ns)) .le. OpdFraction*Cnv_Fct%Opd_effective ) then
           Cnv_Fct%Fct_TF(Ns) = 1_DOUBLE
        else if( dabs(Cnv_Fct%Opd(Ns)) .le. Cnv_Fct%Opd_effective ) then
           Arg = Pi*( (dabs(Cnv_Fct%Opd(Ns))-OpdFraction*Cnv_Fct%Opd_effective) &
                    / ((Cnv_Fct%Opd_effective*(1.d+00-OpdFraction))) )**2
           Cnv_Fct%Fct_TF(Ns) = 0.5d+00 * ( 1 + dcos( Arg ) ) * 1_DOUBLE
        else
           Cnv_Fct%Fct_TF(Ns) = 0.d+00
        end if
      end do
!
!    deallocation
     deallocate( Opd )
     deallocate( GTF )
     deallocate( Isrf )
     return
  end subroutine calc_coscar
!
!
  subroutine calc_sinc( NsFft, Cnv_Fct )
  implicit none
     integer(kind=LONG) ,intent(in)                            :: NsFft
     type(type_Cnv_Fct), intent(inout)                         :: Cnv_Fct
     integer(kind=LONG)                                        :: OSFactor
     integer(kind=LONG)                                        :: OSNsFft
     real(kind=DOUBLE)                                         :: dOpd
     integer(kind=LONG)                                        :: Ns0
     integer(kind=LONG)                                        :: Ns
     integer(kind=LONG)                                        :: NsDel
     integer(kind=LONG)                                        :: allocerror
     integer(kind=LONG)                                        :: ErrCode
     real(kind=DOUBLE)  ,dimension(:) ,allocatable             :: Opd
     real(kind=DOUBLE)  ,dimension(:) ,allocatable             :: GTF
     real(kind=DOUBLE)  ,dimension(:) ,allocatable             :: Isrf
     integer(kind=LONG)                                        :: Sens
     integer(kind=LONG)                                        :: Nsfirst
     integer(kind=LONG)                                        :: Nslast
     real(kind=DOUBLE)                                         :: Norm
!
!    Parameters which have to be defined outside of the subroutine
!    Cnv_Fct%OpdMax   : maximum optical path
!    Cnv_Fct%Ns_Opd   : number of samples descibing the optical path
!    Cnv_Fct%FullSDWn : full width of the SRF spectral domain
!    Cnv_Fct%WnShift  : spectral shift to be applied
!    NsFft            : nominal fft size for the interferogram
!
!    Oversampling
     OSFactor = 16
!
!    constants
     Cnv_Fct%FWHM     = 1.20671 / ( 2.d+00*Cnv_Fct%OpdMax )
     Cnv_Fct%dWn      = 1.d+00 / ( 2.d+00 * Cnv_Fct%OpdMax * dble(OSFactor) )
     Cnv_Fct%N_Sample = 2*idnint(Cnv_Fct%FullSDWn/2/Cnv_Fct%dWn) + 1
     OSNsFft          = NsFft * OSFactor
     dOpd             = Cnv_Fct%OpdMax / dble( int(NsFft/2) )
     Ns0              = int(NsFft/2)+1
     NsDel            = int(OSNsFft/2) - int(NsFft/2)
!
!    allocation
     allocerror = 0
     allocate(Opd(OSNsFft+1),stat=ErrCode )
     if (ErrCode > 0) allocerror = allocerror + 1
     allocate(GTF(OSNsFft+1),stat=ErrCode )
     if (ErrCode > 0) allocerror = allocerror + 1
     allocate(Isrf(OSNsFft+1),stat=ErrCode )
     if (ErrCode > 0) allocerror = allocerror + 1
!
     if (allocerror > 0) then
        write(0,*) 'allocation srf_L1c_Gauss Error'
        write(0,*) 'srf_L1c_Gauss : fatal error'
        call exit(1)
     end if
!
!    Cnv_Fct type allocation
     call alloc_Cnv_Fct( Cnv_Fct )
     call Cnv_Fct_Basis( Cnv_Fct )
!
!    Gate TF computation
     GTF(1:OSNsFft+1) = 0.d+00
     Opd(1:NsFft+1)   = (/ (dble(Ns-Ns0),Ns=1,NsFft+1) /) * dOpd
     GTF(NsDel+1:NsDel+NsFft+1) = 1.d+00
!
!    Gate computation in the spectrum space (sinc)
     Sens = -1
     call fft_r2r_open( OSNsFft, Sens, GTF, dOpd, Cnv_Fct%dWn, Isrf )
!
!    Usefull srf extraction
     Ns0 = int(OSNsFft/2)+1
     Nsfirst = Ns0 - int( Cnv_Fct%N_Sample/2 )
     Nslast  = Ns0 + int( Cnv_Fct%N_Sample/2 )
     Cnv_Fct%Fct(1:Cnv_Fct%N_Sample) = Isrf(Nsfirst:Nslast)
     
!    Surface normalisation
     Norm                            = sum( Cnv_Fct%Fct(1:Cnv_Fct%N_Sample) )
     Cnv_Fct%Fct(1:Cnv_Fct%N_Sample) = Cnv_Fct%Fct(1:Cnv_Fct%N_Sample) / Norm
!
!    Sinc computation in the interferogram space
     Cnv_Fct%Fct_TF(1:Cnv_Fct%Ns_Opd) = 1.d+00
!
!    deallocation
     deallocate( Opd )
     deallocate( GTF )
     deallocate( Isrf )
     return
  end subroutine calc_sinc
!
!
  subroutine calc_triangl( NsFft, Cnv_Fct )
  implicit none
     integer(kind=LONG) ,intent(in)                            :: NsFft
     type(type_Cnv_Fct), intent(inout)                         :: Cnv_Fct
     integer(kind=LONG)                                        :: OSFactor
     integer(kind=LONG)                                        :: OSNsFft
     real(kind=DOUBLE)                                         :: dOpd
     integer(kind=LONG)                                        :: Ns0
     integer(kind=LONG)                                        :: Ns1
     integer(kind=LONG)                                        :: Ns2
     integer(kind=LONG)                                        :: Ns
     integer(kind=LONG)                                        :: NsDel
     integer(kind=LONG)                                        :: NsFftp
     integer(kind=LONG)                                        :: allocerror
     integer(kind=LONG)                                        :: ErrCode
     real(kind=DOUBLE)  ,dimension(:) ,allocatable             :: Opd
     real(kind=DOUBLE)  ,dimension(:) ,allocatable             :: GTF
     real(kind=DOUBLE)  ,dimension(:) ,allocatable             :: Isrf
     integer(kind=LONG)                                        :: Sens
     integer(kind=LONG)                                        :: Nsfirst
     integer(kind=LONG)                                        :: Nslast
     real(kind=DOUBLE)                                         :: Norm
!
!    Parameters which have to be defined outside of the subroutine
!    Cnv_Fct%OpdMax   : maximum optical path
!    Cnv_Fct%Ns_Opd   : number of samples descibing the optical path
!    Cnv_Fct%FullSDWn : full width of the SRF spectral domain
!    Cnv_Fct%WnShift  : spectral shift to be applied
!    NsFft            : nominal fft size for the interferogram
!
!    Oversampling
     OSFactor = 16
!
!    constants
     Cnv_Fct%FWHM     = 0_DOUBLE
     Cnv_Fct%dWn      = 1.d+00 / ( 2.d+00 * Cnv_Fct%OpdMax * dble(OSFactor) )
     Cnv_Fct%N_Sample = 2*idnint(Cnv_Fct%FullSDWn/2/Cnv_Fct%dWn) + 1
     OSNsFft          = NsFft * OSFactor
     dOpd             = Cnv_Fct%OpdMax / dble( int(NsFft/2) )
     Ns0              = int(NsFft/2)+1
     NsDel            = int(OSNsFft/2) - int(NsFft/2)
!
!    allocation
     allocerror = 0
     allocate(Opd(OSNsFft+1),stat=ErrCode )
     if (ErrCode > 0) allocerror = allocerror + 1
     allocate(GTF(OSNsFft+1),stat=ErrCode )
     if (ErrCode > 0) allocerror = allocerror + 1
     allocate(Isrf(OSNsFft+1),stat=ErrCode )
     if (ErrCode > 0) allocerror = allocerror + 1
!
     if (allocerror > 0) then
        write(0,*) 'allocation srf_L1c_Gauss Error'
        write(0,*) 'srf_L1c_Gauss : fatal error'
        call exit(1)
     end if
!
!    Cnv_Fct type allocation
     call alloc_Cnv_Fct( Cnv_Fct )
     call Cnv_Fct_Basis( Cnv_Fct )
!
!    Gate TF computation
     GTF(1:OSNsFft+1) = 0.d+00
     Opd(1:NsFft+1)   = (/ (dble(Ns-Ns0),Ns=1,NsFft+1) /) * dOpd
!
!    triangular function computation
     Ns1 = 1
     Ns2 = NsFft+1
     NsFftp = int(NsFft/2)
     call dichotomd( -Cnv_Fct%Opd_effective, Opd(Ns1:Ns2), Ns1, Ns2 )
     do Ns = 1, NsFftp+1
        if( dabs(Opd(Ns)) .le. Cnv_Fct%Opd_effective ) then
           GTF(NsDel+Ns) = dabs(Opd(Ns)-Opd(Ns1)) / Cnv_Fct%Opd_effective
           GTF(NsFft+2-(NsDel+Ns)) = GTF(NsDel+Ns)
        else
           GTF(NsDel+Ns) = 0.d+00
        end if
     end do
!
!    Gate computation in the spectrum space (sinc)
     Sens = -1
     call fft_r2r_open( OSNsFft, Sens, GTF, dOpd, Cnv_Fct%dWn, Isrf )
!
!    Usefull srf extraction
     Ns0 = int(OSNsFft/2)+1
     Nsfirst = Ns0 - int( Cnv_Fct%N_Sample/2 )
     Nslast  = Ns0 + int( Cnv_Fct%N_Sample/2 )
     Cnv_Fct%Fct(1:Cnv_Fct%N_Sample) = Isrf(Nsfirst:Nslast)
     
!    Surface normalisation
     Norm                            = sum( Cnv_Fct%Fct(1:Cnv_Fct%N_Sample) )
     Cnv_Fct%Fct(1:Cnv_Fct%N_Sample) = Cnv_Fct%Fct(1:Cnv_Fct%N_Sample) / Norm
!
!    FWHM of the function
     call fwhm_real( Cnv_Fct%N_Sample, Cnv_Fct%Fct, Cnv_Fct%dWn, Cnv_Fct%FWHM )
!
!    Triangl omputation of the function for the Apf sampling
     do Ns = 1, int(Cnv_Fct%Ns_Opd/2)+1
        if( dabs(Cnv_Fct%Opd(Ns)) .le. Cnv_Fct%Opd_effective ) then
           Cnv_Fct%Fct_TF(Ns) = dabs(Cnv_Fct%Opd(Ns)-Cnv_Fct%Opd(1)) &
                          / Cnv_Fct%Opd_effective
           Cnv_Fct%Fct_TF(Cnv_Fct%Ns_Opd+1-Ns) = Cnv_Fct%Fct_TF(Ns)
        else
           Cnv_Fct%Fct_TF(Ns) = 0.d+00
        end if
      end do
!
!    deallocation
     deallocate( Opd )
     deallocate( GTF )
     deallocate( Isrf )
     return
  end subroutine calc_triangl

!
!
  subroutine calc_hamming( NsFft, Cnv_Fct )
  implicit none
     integer(kind=LONG) ,intent(in)                            :: NsFft
     type(type_Cnv_Fct), intent(inout)                         :: Cnv_Fct
     integer(kind=LONG)                                        :: OSFactor
     real(kind=DOUBLE)                                         :: H_Optim
     integer(kind=LONG)                                        :: OSNsFft
     real(kind=DOUBLE)                                         :: dOpd
     integer(kind=LONG)                                        :: Ns0
     integer(kind=LONG)                                        :: Ns
     integer(kind=LONG)                                        :: NsDel
     integer(kind=LONG)                                        :: allocerror
     integer(kind=LONG)                                        :: ErrCode
     real(kind=DOUBLE)  ,dimension(:) ,allocatable             :: Opd
     real(kind=DOUBLE)  ,dimension(:) ,allocatable             :: HTF
     real(kind=DOUBLE)  ,dimension(:) ,allocatable             :: Isrf
     integer(kind=LONG)                                        :: Sens
     integer(kind=LONG)                                        :: Nsfirst
     integer(kind=LONG)                                        :: Nslast
     real(kind=DOUBLE)                                         :: Norm
!
!    Parameters which have to be defined outside of the subroutine
!    Cnv_Fct%OpdMax   : maximum optical path
!    Cnv_Fct%Ns_Opd   : number of samples descibing the optical path
!    Cnv_Fct%FullSDWn : full width of the SRF spectral domain
!    Cnv_Fct%WnShift  : spectral shift to be applied
!    NsFft            : nominal fft size for the interferogram
!
!    Oversampling
     OSFactor = 16
!
!    Optimal Hamming coefficient
     H_Optim = 0.23d+00
!
!    constants
     Cnv_Fct%dWn      = 1.d+00 / ( 2.d+00 * Cnv_Fct%OpdMax * dble(OSFactor) )
     Cnv_Fct%N_Sample = 2*idnint(Cnv_Fct%FullSDWn/2/Cnv_Fct%dWn) + 1
     OSNsFft          = NsFft * OSFactor
     dOpd             = Cnv_Fct%OpdMax / dble( int(NsFft/2) )
     Ns0              = int(NsFft/2)+1
     NsDel            = int(OSNsFft/2) - int(NsFft/2)
!
!    allocation
     allocerror = 0
     allocate(Opd(OSNsFft+1),stat=ErrCode )
     if (ErrCode > 0) allocerror = allocerror + 1
     allocate(HTF(OSNsFft+1),stat=ErrCode )
     if (ErrCode > 0) allocerror = allocerror + 1
     allocate(Isrf(OSNsFft+1),stat=ErrCode )
     if (ErrCode > 0) allocerror = allocerror + 1
!
     if (allocerror > 0) then
        write(0,*) 'allocation srf_L1c_Gauss Error'
        write(0,*) 'srf_L1c_Gauss : fatal error'
        call exit(1)
     end if
!
!    Cnv_Fct type allocation
     call alloc_Cnv_Fct( Cnv_Fct )
     call Cnv_Fct_Basis( Cnv_Fct )
!
!    Hamming TF computation
     HTF(1:OSNsFft+1) = 0.d+00
     Opd(1:NsFft+1)   = (/ (dble(Ns-Ns0),Ns=1,NsFft+1) /) * dOpd
     HTF(NsDel+1:NsDel+NsFft+1) = (1.d+00-2.d+00*H_Optim) &
                                + (2.d+00*H_Optim)*dcos(Pi*Opd(1:NsFft+1)&
                                                          /Cnv_Fct%OpdMax)
!
!    Hamming computation in the spectrum space
     Sens = -1
     call fft_r2r_open( OSNsFft, Sens, HTF, dOpd, Cnv_Fct%dWn, Isrf )
!
!    Usefull srf extraction
     Ns0 = int(OSNsFft/2)+1
     Nsfirst = Ns0 - int( Cnv_Fct%N_Sample/2 )
     Nslast  = Ns0 + int( Cnv_Fct%N_Sample/2 )
     Cnv_Fct%Fct(1:Cnv_Fct%N_Sample) = Isrf(Nsfirst:Nslast)
     
!    Surface normalisation
     Norm                            = sum( Cnv_Fct%Fct(1:Cnv_Fct%N_Sample) )
     Cnv_Fct%Fct(1:Cnv_Fct%N_Sample) = Cnv_Fct%Fct(1:Cnv_Fct%N_Sample) / Norm
!
!    FWHM
     call fwhm_real( Cnv_Fct%N_Sample, Cnv_Fct%Fct, Cnv_Fct%dWn, Cnv_Fct%FWHM )
!
!    Hamming computation in the interferogram space
     Cnv_Fct%Fct_TF(1:Cnv_Fct%Ns_Opd) = (1.d+00-2.d+00*H_Optim)           &
                  + (2.d+00*H_Optim)*dcos(Pi*Cnv_Fct%Opd(1:Cnv_Fct%Ns_Opd)&
                                            /Cnv_Fct%OpdMax)
!
!    deallocation
     deallocate( Opd )
     deallocate( HTF )
     deallocate( Isrf )
     return
  end subroutine calc_hamming
!
!
  subroutine calc_blackman_harris( NsFft, BH_term, Cnv_Fct )
  implicit none
     integer(kind=LONG) ,intent(in)                            :: NsFft
     integer(kind=LONG) ,intent(in)                            :: BH_term
     type(type_Cnv_Fct), intent(inout)                         :: Cnv_Fct
     integer(kind=LONG)                                        :: OSFactor
     real(kind=DOUBLE)                                         :: A0
     real(kind=DOUBLE)                                         :: A1
     real(kind=DOUBLE)                                         :: A2
     real(kind=DOUBLE)                                         :: A3
     integer(kind=LONG)                                        :: OSNsFft
     real(kind=DOUBLE)                                         :: dOpd
     integer(kind=LONG)                                        :: Ns0
     integer(kind=LONG)                                        :: Ns
     integer(kind=LONG)                                        :: NsDel
     integer(kind=LONG)                                        :: allocerror
     integer(kind=LONG)                                        :: ErrCode
     real(kind=DOUBLE)  ,dimension(:) ,allocatable             :: Opd
     real(kind=DOUBLE)  ,dimension(:) ,allocatable             :: BHTF
     real(kind=DOUBLE)  ,dimension(:) ,allocatable             :: Isrf
     integer(kind=LONG)                                        :: Sens
     integer(kind=LONG)                                        :: Nsfirst
     integer(kind=LONG)                                        :: Nslast
     real(kind=DOUBLE)                                         :: Norm
!
!    Parameters which have to be defined outside of the subroutine
!    Cnv_Fct%OpdMax   : maximum optical path
!    Cnv_Fct%Ns_Opd   : number of samples descibing the optical path
!    Cnv_Fct%FullSDWn : full width of the SRF spectral domain
!    Cnv_Fct%WnShift  : spectral shift to be applied
!    NsFft            : nominal fft size for the interferogram
!    BH_term          : number of Blackman-Harris term (must be 3 or 4)
!
!    Oversampling
     OSFactor = 16
!
!    Blackman-Harris coefficients
     if( BH_term == 3 ) then
        A0 = 0.42323
        A1 = 0.49755
        A2 = 0.07922
        A3 = 0.d+00
     else if( BH_term == 4 ) then
        A0 = 0.35875
        A1 = 0.48829
        A2 = 0.14128
        A3 = 0.01168
     else
        write(*,*) 'Wrong Blaclman-Harris term number : ',BH_term
        write(*,*) '******* must be 3 or 4 *************'
        call exit(1)
     end if
!
!    constants
     Cnv_Fct%dWn      = 1.d+00 / ( 2.d+00 * Cnv_Fct%OpdMax * dble(OSFactor) )
     Cnv_Fct%N_Sample = 2*idnint(Cnv_Fct%FullSDWn/2/Cnv_Fct%dWn) + 1
     OSNsFft          = NsFft * OSFactor
     dOpd             = Cnv_Fct%OpdMax / dble( int(NsFft/2) )
     Ns0              = int(NsFft/2)+1
     NsDel            = int(OSNsFft/2) - int(NsFft/2)
!
!    allocation
     allocerror = 0
     allocate(Opd(OSNsFft+1),stat=ErrCode )
     if (ErrCode > 0) allocerror = allocerror + 1
     allocate(BHTF(OSNsFft+1),stat=ErrCode )
     if (ErrCode > 0) allocerror = allocerror + 1
     allocate(Isrf(OSNsFft+1),stat=ErrCode )
     if (ErrCode > 0) allocerror = allocerror + 1
!
     if (allocerror > 0) then
        write(0,*) 'allocation srf_L1c_Gauss Error'
        write(0,*) 'srf_L1c_Gauss : fatal error'
        call exit(1)
     end if
!
!    Cnv_Fct type allocation
     call alloc_Cnv_Fct( Cnv_Fct )
     call Cnv_Fct_Basis( Cnv_Fct )
!
!    Blackman-Harris TF computation
     BHTF(1:OSNsFft+1) = 0.d+00
     Opd(1:NsFft+1)   = (/ (dble(Ns-Ns0),Ns=1,NsFft+1) /) * dOpd
     BHTF(NsDel+1:NsDel+NsFft+1) =                                      &
                       A0                                               &
                    + (A1*dcos(       Pi*Opd(1:NsFft+1)/Cnv_Fct%OpdMax))&
                    + (A2*dcos(2.d+00*Pi*Opd(1:NsFft+1)/Cnv_Fct%OpdMax))&
                    + (A3*dcos(3.d+00*Pi*Opd(1:NsFft+1)/Cnv_Fct%OpdMax))
!
!    Blackman-Harris computation in the spectrum space
     Sens = -1
     call fft_r2r_open( OSNsFft, Sens, BHTF, dOpd, Cnv_Fct%dWn, Isrf )
!
!    Usefull srf extraction
     Ns0 = int(OSNsFft/2)+1
     Nsfirst = Ns0 - int( Cnv_Fct%N_Sample/2 )
     Nslast  = Ns0 + int( Cnv_Fct%N_Sample/2 )
     Cnv_Fct%Fct(1:Cnv_Fct%N_Sample) = Isrf(Nsfirst:Nslast)
     
!    Surface normalisation
     Norm                            = sum( Cnv_Fct%Fct(1:Cnv_Fct%N_Sample) )
     Cnv_Fct%Fct(1:Cnv_Fct%N_Sample) = Cnv_Fct%Fct(1:Cnv_Fct%N_Sample) / Norm
!
!    FWHM
     call fwhm_real( Cnv_Fct%N_Sample, Cnv_Fct%Fct, Cnv_Fct%dWn, Cnv_Fct%FWHM )
!
!    Blackman-Harris computation in the interferogram space
     Cnv_Fct%Fct_TF(1:Cnv_Fct%Ns_Opd) =                                       &
              A0                                                              &
           + (A1*dcos(       Pi*Cnv_Fct%Opd(1:Cnv_Fct%Ns_Opd)/Cnv_Fct%OpdMax))&
           + (A2*dcos(2.d+00*Pi*Cnv_Fct%Opd(1:Cnv_Fct%Ns_Opd)/Cnv_Fct%OpdMax))&
           + (A3*dcos(3.d+00*Pi*Cnv_Fct%Opd(1:Cnv_Fct%Ns_Opd)/Cnv_Fct%OpdMax))
!
!    deallocation
     deallocate( Opd )
     deallocate( BHTF )
     deallocate( Isrf )
     return
  end subroutine calc_blackman_harris
!
!
   subroutine cnv_sp_fct( Spectrum_hr,  &
                          Cnv_Fct,      &
                          Spectrum      )
   implicit none
     type(type_Spectrum),intent(in)                      :: Spectrum_hr
     type(type_Cnv_Fct) ,intent(in)                      :: Cnv_Fct
     type(type_Spectrum),intent(inout)                   :: Spectrum
     integer(kind=LONG)                                  :: Ns
     integer(kind=LONG)                                  :: Nc
     integer(kind=LONG)                                  :: n
     integer(kind=LONG)                                  :: m
     integer(kind=LONG)                                  :: m0
     real(kind=DOUBLE)  ,dimension(:) ,allocatable       :: Spectre
     real(kind=DOUBLE)  ,dimension(:) ,allocatable       :: Srf_Wn_temp
     integer(kind=LONG)                                  :: Srf_HR_Ns
     real(kind=DOUBLE)  ,dimension(:) ,allocatable       :: Srf_HR_Wn
     real(kind=DOUBLE)  ,dimension(:) ,allocatable       :: Srf_HR_Wn_temp
     real(kind=DOUBLE)  ,dimension(:) ,allocatable       :: Srf_HR_Srf
     real(kind=DOUBLE)                                   :: Norm
     integer(kind=LONG)                                  :: ErrCode
     integer(kind=LONG)                                  :: iostatus
!
     write(*,*) 'Cnv_Fct%FullSDWn       ',Cnv_Fct%FullSDWn
     write(*,*) 'Cnv_Fct%N_Sample       ',Cnv_Fct%N_Sample
     write(*,*) 'Cnv_Fct%dWn            ',Cnv_Fct%dWn
     write(*,*) 'Cnv_Fct%FWHM           ',Cnv_Fct%FWHM
     write(*,*) 'Cnv_Fct%WnShift        ',Cnv_Fct%WnShift
     write(*,*) 'Cnv_Fct%OpdMax         ',Cnv_Fct%OpdMax
     write(*,*) 'Cnv_Fct%Ns_Opd         ',Cnv_Fct%Ns_Opd
     write(*,*) 'Cnv_Fct%dOpd           ',Cnv_Fct%dOpd
     write(*,*) 'Cnv_Fct%Opd_effective  ',Cnv_Fct%Opd_effective
     Srf_HR_Ns = idnint( Cnv_Fct%FullSDWn / Spectrum_hr%dWn ) +1
     write(*,*) 'Spectrum_hr%dWn',Spectrum_hr%dWn
     write(*,*) 'Srf_HR_Ns',Srf_HR_Ns
     iostatus = 0
     allocate( Spectre(Spectrum%N_Sample),    stat=ErrCode )
     if (ErrCode > 0) iostatus = iostatus+1
     allocate( Srf_Wn_temp(Cnv_Fct%N_Sample), stat=ErrCode )
     if (ErrCode > 0) iostatus = iostatus+1
     allocate( Srf_HR_Wn(Srf_HR_Ns),          stat=ErrCode )
     if (ErrCode > 0) iostatus = iostatus+1
     allocate( Srf_HR_Wn_temp(Srf_HR_Ns),     stat=ErrCode )
     if (ErrCode > 0) iostatus = iostatus+1
     allocate( Srf_HR_Srf(Srf_HR_Ns),         stat=ErrCode )
     if (ErrCode > 0) iostatus = iostatus+1
!
     if (iostatus > 0) then
        write(0,*) 'allocation Error',iostatus
        write(0,*) 'cnv_sp_fct : fatal error'
        call exit(1)
     end if
!
!    High Resolution SRF basis
     Srf_HR_Wn(1:Srf_HR_Ns) = Spectrum_hr%dWn                       &
                 * (/ (dble(Ns-int(Srf_HR_Ns/2)-1),Ns=1,Srf_HR_Ns) /)
     write(*,*) 'Srf_HR_Wn',Srf_HR_Wn(1)
     write(*,*) 'Srf_HR_Wn',Srf_HR_Wn(Srf_HR_Ns/2+1)
     write(*,*) 'Srf_HR_Wn',Srf_HR_Wn(Srf_HR_Ns)
!
!    Convolution samples loop
     do Ns = Spectrum%Ns_First, Spectrum%Ns_Last
        n = Ns - Spectrum%Ns_First + 1
        m0 = idnint( (Spectrum%Wn(n) - Spectrum_hr%Wn_First) &
                    / Spectrum_hr%dWn ) + 1
        m = m0 - int(Srf_HR_Ns/2)
        if( m0 <= 0 .or. m0 > Spectrum_hr%N_Sample ) then
           write(*,'(a,2i10,2f15.3)') 'm0 Error',       &
                 n,m0,Spectrum_hr%Wn_First,Spectrum%Wn(n)
           call exit(1)
        end if
!
!       Base transport Srf_Wn
        Srf_Wn_temp(1:Cnv_Fct%N_Sample) = Cnv_Fct%Wn(1:Cnv_Fct%N_Sample) &
                                        + Spectrum%Wn(n)
!
!       Base transport Srf_HR_Wn
        Srf_HR_Wn_temp(1:Srf_HR_Ns) = Srf_HR_Wn(1:Srf_HR_Ns) &
                                    + Spectrum_hr%Wn(m0)
!
!       SRF interpolation in the HR space
        call intspline( Cnv_Fct%N_Sample,&
                        Srf_Wn_temp,     &
                        Cnv_Fct%Fct,     &
                        Srf_HR_Ns,       &
                        Srf_HR_Wn_temp,  &
                        Srf_HR_Srf       )
!
!       Srf Normalisation
        Norm = sum( Srf_HR_Srf ) * Spectrum_hr%dWn
        Srf_HR_Srf(1:Srf_HR_Ns) = Srf_HR_Srf(1:Srf_HR_Ns) / Norm
        if( (n < 10) .or. (n > Spectrum%N_Sample-10) ) then
           if( n == 1 ) write(*,*) 'Cnv_Fct loop'
           write(*,*) 'n,Ns,m0,m',n,Ns,m0,m
           write(*,*) 'Srf_Wn_temp   ',Srf_Wn_temp(1)
           write(*,*) 'Srf_Wn_temp   ',Srf_Wn_temp(Cnv_Fct%N_Sample/2+1)
           write(*,*) 'Srf_Wn_temp   ',Srf_Wn_temp(Cnv_Fct%N_Sample)
           write(*,*) 'Srf_HR_Wn_temp',Srf_HR_Wn_temp(1)
           write(*,*) 'Srf_HR_Wn_temp',Srf_HR_Wn_temp(Srf_HR_Ns/2+1)
           write(*,*) 'Srf_HR_Wn_temp',Srf_HR_Wn_temp(Srf_HR_Ns)
           write(*,*) 'Srf_HR_Srf    ',Srf_HR_Srf(1)
           write(*,*) 'Srf_HR_Srf    ',Srf_HR_Srf(Srf_HR_Ns/2+1)
           write(*,*) 'Srf_HR_Srf    ',Srf_HR_Srf(Srf_HR_Ns)
           write(*,*) 'Norm          ',Norm
        end if
!
!       convolution sample n
        Spectre(n) = 0.d+00
        do Nc = 1, Srf_HR_Ns
           if( (m > 0) .and. (m <= Spectrum_hr%N_Sample) ) then
              Spectre(n) = Spectre(n)                      &
                           + ( Spectrum_hr%Real_Part(m)    &
                              * Srf_HR_Srf(Nc) )           &
                           * Spectrum_hr%dWn
           else
              write(*,*) 'Out of domain m,Nc,n',m,Nc,n
           end if
           m = m + 1
        end do
     end do ! end convolution samples loop
!
     if( Spectrum%Type == 'C' ) then
        Spectrum%Complex(1:Spectrum%N_Sample) = &
                       dcmplx(Spectre(1:Spectrum%N_Sample),0.d+00)
     else if( Spectrum%Type == 'R' ) then
        Spectrum%Real_Part(1:Spectrum%N_Sample) = &
                       Spectre(1:Spectrum%N_Sample)
     else
        write(*,*) 'Spectrum Type Error must be C or R'
        write(*,*) 'cnv_sp_fct Fatal Error'
        call exit(1)
     end if
     deallocate ( Spectre )
     deallocate ( Srf_Wn_temp )
     deallocate ( Srf_HR_Wn )
     deallocate ( Srf_HR_Wn_temp )
     deallocate ( Srf_HR_Srf )
!
     return
   end subroutine cnv_sp_fct
!
!
   subroutine cnv_ft_fct( Spectrum_hr,&
                          Cnv_Fct,    &
                          L1NsFft,    &
                          Spectrum    )
   implicit none
     type(type_Spectrum),intent(in)                      :: Spectrum_hr
     type(type_Cnv_Fct) ,intent(in)                      :: Cnv_Fct
     integer(kind=LONG) ,intent(in)                      :: L1NsFft
     type(type_Spectrum),intent(inout)                   :: Spectrum
     integer(kind=LONG)                                  :: NsFft
     integer(kind=LONG)                                  :: NsDel
     integer(kind=LONG)                                  :: N_Sample
     type(type_Interferogram)                            :: Interf_hr
     type(type_Interferogram)                            :: Interf_lr
     real(kind=DOUBLE)       ,dimension(:) ,allocatable  :: Fct_TF
     integer(kind=LONG)                                  :: ErrCode
     real(kind=DOUBLE)                                   :: Opd_Pivot
     real(kind=DOUBLE)                                   :: dWn_Ratio
     integer(kind=LONG)                                  :: Ns_Erase
     integer(kind=LONG)                                  :: n1
     integer(kind=LONG)                                  :: n2
!
!    FFT size computation
     dWn_Ratio = idnint(Spectrum%dWn/Spectrum_hr%dWn)
     N_Sample  = L1NsFft * dWn_Ratio
     call tabule_NsFft( N_Sample, NsFft, ios )
     write(*,*) 'dWn Ratio              ',dWn_Ratio
     write(*,*) 'cnv_ft N_Sample L1NsFft',N_Sample,L1NsFft
     write(*,*) 'cnv_ft_fct NsFft       ',NsFft
     write(*,*) 'Cnv_Fct%FullSDWn       ',Cnv_Fct%FullSDWn
     write(*,*) 'Cnv_Fct%N_Sample       ',Cnv_Fct%N_Sample
     write(*,*) 'Cnv_Fct%dWn            ',Cnv_Fct%dWn
     write(*,*) 'Cnv_Fct%FWHM           ',Cnv_Fct%FWHM
     write(*,*) 'Cnv_Fct%WnShift        ',Cnv_Fct%WnShift
     write(*,*) 'Cnv_Fct%OpdMax         ',Cnv_Fct%OpdMax
     write(*,*) 'Cnv_Fct%Ns_Opd         ',Cnv_Fct%Ns_Opd
     write(*,*) 'Cnv_Fct%dOpd           ',Cnv_Fct%dOpd
     write(*,*) 'Cnv_Fct%Opd_effective  ',Cnv_Fct%Opd_effective
!
!    Real hr interferogram parametrisation
     Interf_hr%Type     = 'R'
     Interf_hr%N_Sample = NsFft + 1
     Interf_hr%OpdMax   = 1.d+00 / ( 2.d+00*Spectrum_hr%dWn )
     Interf_hr%dOpd     = Interf_hr%OpdMax &
                        / dble(int(Interf_hr%N_Sample/2))
     Interf_hr%dTime    = Interf_hr%dOpd
     call alloc_Interferogram( Interf_hr )
     call Interferogram_Basis( Interf_hr )
     Opd_Pivot = Interf_hr%Opd(int(Interf_hr%N_Sample/2)+1)
     Interf_hr%Opd(1:Interf_hr%N_Sample) = &
                 Interf_hr%Opd(1:Interf_hr%N_Sample) - Opd_Pivot
!
!    Fourier transform of the high resolution spectrum
     call sptoif( Spectrum_hr, Interf_hr )
!
!    Real interferogram parametrisation
     Interf_lr%Type     = 'R'
     Interf_lr%N_Sample = L1NsFft + 1
     Interf_lr%OpdMax   = 1.d+00 / ( 2.d+00*Spectrum%dWn )
     Interf_lr%dOpd     = Interf_lr%OpdMax              &
                        / dble(int(Interf_lr%N_Sample/2))
     Interf_lr%dTime    = Interf_lr%dOpd
     call alloc_Interferogram( Interf_lr )
     call Interferogram_Basis( Interf_lr )
     Opd_Pivot = Interf_lr%Opd(int(Interf_lr%N_Sample/2)+1)
     Interf_lr%Opd(1:Interf_lr%N_Sample) =                     &
                 Interf_lr%Opd(1:Interf_lr%N_Sample) - Opd_Pivot
     allocate( Fct_TF(Interf_lr%N_Sample), stat=ErrCode )
     if( ErrCode /= 0 ) then
        write(*,*) 'Allocation Error'
        write(*,*) 'cnv_ft_fct Fatal Error'
        call exit(1)
     end if
!
!    extraction of the lr interferogram
     NsDel = int(NsFft/2) - int(L1NsFft/2)
     Interf_lr%Real_Part(1:Interf_lr%N_Sample) =                       &
                   Interf_hr%Real_Part(NsDel+1:NsDel+Interf_lr%N_Sample)
!
!    Convolution function interpolation on the lr basis
     write(*,*) 'Cnv_Fct%Opd       ',Cnv_Fct%Opd(1)
     write(*,*) 'Cnv_Fct%Opd       ',Cnv_Fct%Opd(Cnv_Fct%Ns_Opd/2+1)
     write(*,*) 'Cnv_Fct%Opd       ',Cnv_Fct%Opd(Cnv_Fct%Ns_Opd)
     write(*,*) 'Cnv_Fct%Fct_TF    ',Cnv_Fct%Fct_TF(1)
     write(*,*) 'Cnv_Fct%Fct_TF    ',Cnv_Fct%Fct_TF(Cnv_Fct%Ns_Opd/2+1)
     write(*,*) 'Cnv_Fct%Fct_TF    ',Cnv_Fct%Fct_TF(Cnv_Fct%Ns_Opd)
     write(*,*) 'Interf_lr%N_Sample',Interf_lr%N_Sample
     write(*,*) 'Interf_lr%dOpd    ',Interf_lr%dOpd
     write(*,*) 'Interf_lr%OpdMax  ',Interf_lr%OpdMax
     write(*,*) 'Interf_lr Opd     ',Interf_lr%Opd(1)
     write(*,*) 'Interf_lr Opd     ',Interf_lr%Opd(Interf_lr%N_Sample/2+1)
     write(*,*) 'Interf_lr Opd     ',Interf_lr%Opd(Interf_lr%N_Sample)
     call intspline_open( Cnv_Fct%Ns_Opd,    &
                          Cnv_Fct%Opd,       &
                          Cnv_Fct%Fct_TF,    &
                          Interf_lr%N_Sample,&
                          Interf_lr%Opd,     &
                          Fct_TF             )
     write(*,*) 'Fct_TF       ',Fct_TF(1)
     write(*,*) 'Fct_TF       ',Fct_TF(Interf_lr%N_Sample/2+1)
     write(*,*) 'Fct_TF       ',Fct_TF(Interf_lr%N_Sample)
!
!    Effective Opd introduction
     n1 = 1
     n2 = Interf_lr%N_Sample
     call dichotomd( Cnv_Fct%Opd_effective, Interf_lr%Opd(n1), n1, n2 )
     if( n1 == Interf_lr%N_Sample ) then
        Ns_Erase = 0
     else
        Ns_Erase = Interf_lr%N_Sample - n1
     end if
     write(*,*) 'cnv_ft_fct Ns_Erase :',Ns_Erase
!
!    reduction to the effective OPD
     if( Ns_Erase /= 0 ) then
        Fct_TF(1:Ns_Erase) = 0_DOUBLE
        Fct_TF(Interf_lr%N_Sample-Ns_Erase+1:Interf_lr%N_Sample) = &
                                                            0_DOUBLE
     end if
!
!    Convolution in the Fourier space
     Interf_lr%Real_Part(1:Interf_lr%N_Sample) =               &
                     Interf_lr%Real_Part(1:Interf_lr%N_Sample) &
                   * Fct_TF(1:Interf_lr%N_Sample)
!
     write(*,*) 'Interf_lr      ',Interf_lr%Real_Part(1)
     write(*,*) 'Interf_lr      ',Interf_lr%Real_Part(2)
     write(*,*) 'Interf_lr      ',Interf_lr%Real_Part(Interf_lr%N_Sample/2)
     write(*,*) 'Interf_lr      ',Interf_lr%Real_Part(Interf_lr%N_Sample/2+1)
     write(*,*) 'Interf_lr      ',Interf_lr%Real_Part(Interf_lr%N_Sample/2+2)
     write(*,*) 'Interf_lr      ',Interf_lr%Real_Part(Interf_lr%N_Sample-1)
     write(*,*) 'Interf_lr      ',Interf_lr%Real_Part(Interf_lr%N_Sample)
!
!    Fourier transform of the convolved interferogram
     call iftosp_open( Interf_lr, Spectrum )
!
     call dalloc_Interferogram( Interf_hr )
     call dalloc_Interferogram( Interf_lr )
     deallocate( Fct_TF )
!
     return
   end subroutine cnv_ft_fct
!
!
   subroutine cnv_sp( Spectrum_hr, &
                      Srf,         &
                      Level,       &
                      PN,          &
                      Spectrum     )
   implicit none
     type(type_Spectrum),intent(in)                      :: Spectrum_hr
     type(type_Srf)     ,intent(in)                      :: Srf
     character(len=*)   ,intent(in)                      :: Level
     integer(kind=LONG) ,intent(in)                      :: PN
     type(type_Spectrum),intent(inout)                   :: Spectrum
     integer(kind=LONG)                                  :: Ns
     real(kind=DOUBLE)  ,dimension(:) ,allocatable       :: Spectre
     real(kind=DOUBLE)  ,dimension(:) ,allocatable       :: Srf_Wn_temp
     integer(kind=LONG)                                  :: Srf_HR_Ns
     real(kind=DOUBLE)  ,dimension(:) ,allocatable       :: Srf_HR_Wn
     real(kind=DOUBLE)  ,dimension(:) ,allocatable       :: Srf_HR_Wn_temp
     real(kind=DOUBLE)  ,dimension(:) ,allocatable       :: Srf_HR_Srf
     real(kind=DOUBLE)                                   :: Norm
     real(kind=DOUBLE)                                   :: alpha
     real(kind=DOUBLE)                                   :: beta
     integer(kind=LONG)                                  :: iostatus
     integer(kind=LONG)                                  :: ErrCode
     integer(kind=LONG)                                  :: n
     integer(kind=LONG)                                  :: n1
     integer(kind=LONG)                                  :: n2
     integer(kind=LONG)                                  :: m
     integer(kind=LONG)                                  :: m0
     integer(kind=LONG)                                  :: Nc
     integer(kind=LONG)                                  :: Srf_NsWn0
     integer(kind=LONG)                                  :: Srf_NsSDWn
     real(kind=DOUBLE)                                   :: Srf_SDWnMax
     real(kind=DOUBLE)  ,dimension(:)   ,allocatable     :: Srf_Wn0
     real(kind=DOUBLE)  ,dimension(:)   ,allocatable     :: Srf_SDWn
     real(kind=DOUBLE)  ,dimension(:,:) ,allocatable     :: Srf_L1
     real(kind=DOUBLE)  ,dimension(:)   ,allocatable     :: Srf_L1_temp
     real(kind=DOUBLE)  ,dimension(:)   ,allocatable     :: Srf_WnShift
     real(kind=DOUBLE)                                   :: Srf_WnShift_temp
!
!    Spectral parameter transfert
     if( Level(1:len_trim(Level)) == 'L1a' ) then
        Srf_NsWn0   = Srf%NsWn0
        Srf_SDWnMax = Srf%SDWnMax1a
        Srf_NsSDWn  = Srf%NsSDWn1a
        allocate(Srf_SDWn(Srf_NsSDWn),             stat=ErrCode )
        allocate(Srf_L1(1:Srf_NsSDWn,1:Srf_NsWn0), stat=ErrCode )
        allocate(Srf_WnShift(1:Srf_NsWn0),         stat=ErrCode )
        Srf_SDWn(1:Srf_NsSDWn) = Srf%SDWn1a(1:Srf%NsSDWn1a)
        Srf_L1(1:Srf_NsSDWn,1:Srf_NsWn0) = &
                                 Srf%L1a(1:Srf_NsSDWn,1:Srf_NsWn0,PN)
        Srf_WnShift(1:Srf_NsWn0) = &
                                 Srf%WnShift1a(1:Srf_NsWn0,PN)
     else if( Level(1:len_trim(Level)) == 'L1b' ) then
        Srf_NsWn0   = Srf%NsWn0
        Srf_SDWnMax = Srf%SDWnMax1b
        Srf_NsSDWn  = Srf%NsSDWn1b
        allocate(Srf_SDWn(Srf_NsSDWn),             stat=ErrCode )
        allocate(Srf_L1(1:Srf_NsSDWn,1:Srf_NsWn0), stat=ErrCode )
        allocate(Srf_WnShift(1:Srf_NsWn0),         stat=ErrCode )
        Srf_SDWn(1:Srf_NsSDWn) = Srf%SDWn1b(1:Srf%NsSDWn1b)
        Srf_L1(1:Srf_NsSDWn,1:Srf_NsWn0) = &
                                 Srf%L1b(1:Srf_NsSDWn,1:Srf_NsWn0,PN)
        Srf_WnShift(1:Srf_NsWn0) = &
                                 Srf%WnShift1b(1:Srf_NsWn0,PN)
     else if( Level(1:len_trim(Level)) == 'L1c' ) then
        Srf_NsWn0   = Srf%NsWn0
        Srf_SDWnMax = Srf%SDWnMax1c
        Srf_NsSDWn  = Srf%NsSDWn1c
        allocate(Srf_SDWn(Srf_NsSDWn),             stat=ErrCode )
        allocate(Srf_L1(1:Srf_NsSDWn,1:Srf_NsWn0), stat=ErrCode )
        allocate(Srf_WnShift(1:Srf_NsWn0),         stat=ErrCode )
        Srf_SDWn(1:Srf_NsSDWn) = Srf%SDWn1c(1:Srf%NsSDWn1c)
        Srf_L1(1:Srf_NsSDWn,1:Srf_NsWn0) = &
                                 Srf%L1c(1:Srf_NsSDWn,1:Srf_NsWn0,PN)
        Srf_WnShift(1:Srf_NsWn0) = &
                                 Srf%WnShift1c(1:Srf_NsWn0,PN)
     else if( Level(1:len_trim(Level)) == 'L1d' ) then
        Srf_NsWn0   = Srf%NsWn0
        Srf_SDWnMax = Srf%SDWnMax1c
        Srf_NsSDWn  = Srf%NsSDWn1c
        allocate(Srf_SDWn(Srf_NsSDWn),             stat=ErrCode )
        allocate(Srf_L1(1:Srf_NsSDWn,1:Srf_NsWn0), stat=ErrCode )
        allocate(Srf_WnShift(1:Srf_NsWn0),         stat=ErrCode )
        Srf_SDWn(1:Srf_NsSDWn) = Srf%SDWn1c(1:Srf%NsSDWn1c)
        Srf_L1(1:Srf_NsSDWn,1:Srf_NsWn0) = &
                                 Srf%L1c(1:Srf_NsSDWn,1:Srf_NsWn0,PN)
        Srf_WnShift(1:Srf_NsWn0) = &
                                 Srf%WnShift1c(1:Srf_NsWn0,PN)
     else
        write(*,*) 'Convolution level Error :',Level
        write(*,*) 'Cnv_Sp Fatal Error'
        call exit(1)
     end if
!
     Srf_HR_Ns = idnint( 2.d+00 * Srf_SDWnMax / Spectrum_hr%dWn ) +1
     allocate(Srf_Wn0(Srf_NsWn0),           stat=ErrCode )
     Srf_Wn0(1:Srf_NsWn0) = Srf%Wn0(1:Srf_NsWn0)
     allocate( Srf_Wn_temp(Srf_NsSDWn),     stat=ErrCode )
     write(*,*) 'Spectrum_hr%dWn',Spectrum_hr%dWn
     write(*,*) 'Srf_SDWnMax    ',Srf_SDWnMax
     write(*,*) 'Srf_NsSDWn     ',Srf_NsSDWn
     write(*,*) 'Srf_HR_Ns      ',Srf_HR_Ns
!
     iostatus = 0
     allocate( Spectre(Spectrum%N_Sample),    stat=ErrCode )
     if (ErrCode > 0) iostatus = iostatus+1
     allocate( Srf_L1_temp(Srf_NsSDWn),       stat=ErrCode )
     if (ErrCode > 0) iostatus = iostatus+1
     allocate( Srf_Wn_temp(Srf_NsSDWn),       stat=ErrCode )
     if (ErrCode > 0) iostatus = iostatus+1
     allocate( Srf_HR_Wn(Srf_HR_Ns),          stat=ErrCode )
     if (ErrCode > 0) iostatus = iostatus+1
     allocate( Srf_HR_Wn_temp(Srf_HR_Ns),     stat=ErrCode )
     if (ErrCode > 0) iostatus = iostatus+1
     allocate( Srf_HR_Srf(Srf_HR_Ns),         stat=ErrCode )
     if (ErrCode > 0) iostatus = iostatus+1
!
     if (ErrCode > 0) then
        write(0,*) 'allocation Error',iostatus
        write(0,*) 'cnv_sp : fatal error'
        call exit(1)
     end if
!
!    High Resolution SRF basis
     Srf_HR_Wn(1:Srf_HR_Ns) = Spectrum_hr%dWn                       &
                 * (/ (dble(Ns-int(Srf_HR_Ns/2)-1),Ns=1,Srf_HR_Ns) /)
!
!    Convolution samples loop
     do Ns = Spectrum%Ns_First, Spectrum%Ns_Last
        n = Ns - Spectrum%Ns_First + 1
        m0 = idnint( (Spectrum%Wn(n) - Spectrum_hr%Wn_First) &
                    / Spectrum_hr%dWn ) + 1
        m = m0 - int(Srf_HR_Ns/2)
        if( m0 <= 0 .or. m0 > Spectrum_hr%N_Sample ) then
           write(*,'(a,2i10,2f15.3)') 'm0 Error',&
                 n,m0,Spectrum_hr%Wn_First,Spectrum%Wn(n)
           call exit(1)
        end if
!
!       Base transport high resolution Srf_HR_Wn
        Srf_HR_Wn_temp(1:Srf_HR_Ns) = Srf_HR_Wn(1:Srf_HR_Ns) &
                                    + Spectrum_hr%Wn(m0)
!
!       Interpolation between two SRF
        n1 = 1
        n2 = Srf_NsWn0
        call dichotomd(Spectrum%Wn(n),Srf_Wn0(n1),n1,n2) 
        if( n1 == n2 ) then
           beta  = 0.d+00
           alpha = 1.d+00
           Srf_L1_temp(1:Srf_NsSDWn) = Srf_L1(1:Srf_NsSDWn,n1)
           Srf_WnShift_temp = Srf_WnShift(n1)
        else
           beta  = (Spectrum%Wn(n)-Srf_Wn0(n1))/(Srf_Wn0(n2)-Srf_Wn0(n1))
           alpha = 1.d+00-beta
           Srf_L1_temp(1:Srf_NsSDWn) =           &
                 alpha * Srf_L1(1:Srf_NsSDWn,n1) &
               + beta  * Srf_L1(1:Srf_NsSDWn,n2)
           Srf_WnShift_temp =            &
                 alpha * Srf_WnShift(n1) &
               + beta  * Srf_WnShift(n2)
        end if
!
!       Base transport tabulated Srf_Wn
        if( Level(1:len_trim(Level)) == 'L1a' ) then
           Srf_Wn_temp(1:Srf_NsSDWn) = Srf_SDWn(1:Srf_NsSDWn) &
                                     + Spectrum%Wn(n) + Srf_WnShift_temp
        else
           Srf_Wn_temp(1:Srf_NsSDWn) = Srf_SDWn(1:Srf_NsSDWn) &
                                     + Spectrum%Wn(n)
        end if
!
!       SRF interpolation in the HR space
        call intspline( Srf_NsSDWn,    &
                        Srf_Wn_temp,   &
                        Srf_L1_temp,   &
                        Srf_HR_Ns,     &
                        Srf_HR_Wn_temp,&
                        Srf_HR_Srf     )
!
!       Srf Normalisation
        Norm = sum( Srf_HR_Srf(1:Srf_HR_Ns) ) * Spectrum_hr%dWn
        if( Norm /= 0.d+00 ) then
           Srf_HR_Srf(1:Srf_HR_Ns) = Srf_HR_Srf(1:Srf_HR_Ns) / Norm
        end if
!
!       convolution sample n
        Spectre(n) = 0.d+00
        do Nc = 1, Srf_HR_Ns
           if( (m > 0) .and. (m <= Spectrum_hr%N_Sample) ) then
              Spectre(n) = Spectre(n)                    &
                         + ( Spectrum_hr%Real_Part(m)    &
                           * Srf_HR_Srf(Nc) )            &
                         * Spectrum_hr%dWn
           else
              write(*,*) 'Out of domain m,Nc,n',m,Nc,n
           end if
           m = m + 1
        end do
     end do ! end convolution samples loop
!
     if( Spectrum%Type == 'C' ) then
        Spectrum%Complex(1:Spectrum%N_Sample) =                  &
                       dcmplx(Spectre(1:Spectrum%N_Sample),0.d+00)
     else if( Spectrum%Type == 'R' ) then
        Spectrum%Real_Part(1:Spectrum%N_Sample) = &
                       Spectre(1:Spectrum%N_Sample)
     else
        write(*,*) 'Spectrum Type Error must be C or R'
        write(*,*) 'cnv_sp Fatal Error'
        call exit(1)
     end if
!
     deallocate ( Spectre )
     deallocate ( Srf_L1_temp )
     deallocate ( Srf_Wn_temp )
     deallocate ( Srf_HR_Wn )
     deallocate ( Srf_HR_Wn_temp )
     deallocate ( Srf_HR_Srf )
     deallocate ( Srf_SDWn )
     deallocate ( Srf_L1 )
     deallocate ( Srf_WnShift )
!
     return
   end subroutine cnv_sp
!
!
   subroutine cnv_ft( Spectrum_hr, &
                      Saf,         &
                      Srf,         &
                      Calib_Option,&
                      Level,       &
                      L1NsFft,     &
                      SUB_PN,      &
                      Spectrum     )
   implicit none
     type(type_Spectrum)     ,intent(in)                 :: Spectrum_hr
     type(type_Saf)          ,intent(in)                 :: Saf
     type(type_Srf)          ,intent(in)                 :: Srf
     integer(kind=LONG)      ,intent(in)                 :: Calib_Option
     character(len=*)        ,intent(in)                 :: Level
     integer(kind=LONG)      ,intent(in)                 :: L1NsFft
     integer(kind=LONG)      ,intent(in)                 :: SUB_PN
     type(type_Spectrum)     ,intent(inout)              :: Spectrum
     type(type_Interferogram)                            :: Interf_hr
     type(type_Interferogram)                            :: Interf_lr
     type(type_Interferogram)                            :: Interf_Saf
     type(type_Spectrum)                                 :: Spectrum_1
     type(type_Spectrum)                                 :: Spectrum_2
     integer(kind=LONG)                                  :: Ns_First
     integer(kind=LONG)                                  :: Ns_Last
     integer(kind=LONG)                                  :: N_Sample
     integer(kind=LONG)                                  :: NsFft
     integer(kind=LONG)                                  :: NsDel
     integer(kind=LONG)                                  :: n1
     integer(kind=LONG)                                  :: n2
     integer(kind=LONG)                                  :: NSafStart
     integer(kind=LONG)                                  :: NSafEnd
     real(kind=DOUBLE)                                   :: Opd_Pivot
     integer(kind=LONG)                                  :: NSaf
     real(kind=DOUBLE)                                   :: WnSafStart
     real(kind=DOUBLE)                                   :: WnSafEnd
     real(kind=DOUBLE)       ,dimension(:) ,allocatable  :: Saf_Mod
     real(kind=DOUBLE)       ,dimension(:) ,allocatable  :: Saf_Arg
     real(kind=DOUBLE)       ,dimension(:) ,allocatable  :: Saf_Arg_Cal
     complex(kind=DOUBLE)    ,dimension(:) ,allocatable  :: Saf_cplx
     complex(kind=DOUBLE)    ,dimension(:) ,allocatable  :: Spectre
     integer(kind=LONG)                                  :: Ns
     integer(kind=LONG)                                  :: NsStart
     integer(kind=LONG)                                  :: NsEnd
     integer(kind=LONG)                                  :: n
     real(kind=DOUBLE)                                   :: alpha
     real(kind=DOUBLE)                                   :: beta
     real(kind=DOUBLE)                                   :: dWn_Ratio
     integer(kind=LONG)                                  :: ios
!
!    FFT size computation
     dWn_Ratio = idnint(Spectrum%dWn/Spectrum_hr%dWn)
     N_Sample  = L1NsFft * dWn_Ratio
     call tabule_NsFft( N_Sample, NsFft, ios )
     write(*,*) 'dWn Ratio           ',dWn_Ratio
     write(*,*) 'cnv_ft L1NsFft      ',L1NsFft
     write(*,*) 'cnv_ft NsFft        ',NsFft
!
!    Real interferogram parametrisation
     Interf_hr%Type     = 'R'
     Interf_hr%N_Sample = NsFft + 1
     Interf_hr%OpdMax   = 1.d+00 / ( 2.d+00*Spectrum_hr%dWn )
     Interf_hr%dOpd     = Interf_hr%OpdMax &
                        / dble(int(NsFft/2))
     Interf_hr%dTime    = Interf_hr%dOpd
     call alloc_Interferogram( Interf_hr )
     call Interferogram_Basis( Interf_hr )
     Opd_Pivot = Interf_hr%Opd(int(Interf_hr%N_Sample/2)+1)
     write(*,*) 'Opd_Pivot hr           ',Opd_Pivot
     Interf_hr%Opd(1:Interf_hr%N_Sample) =                    &
                Interf_hr%Opd(1:Interf_hr%N_Sample) - Opd_Pivot
     write(*,*) 'Interf_hr%Type         ',Interf_hr%Type
     write(*,*) 'Interf_hr%N_Sample     ',Interf_hr%N_Sample
     write(*,*) 'Interf_hr%OpdMax       ',Interf_hr%OpdMax
     write(*,*) 'Interf_hr%dOpd         ',Interf_hr%dOpd
     write(*,*) 'Interf_hr%Opd(1)       ',Interf_hr%Opd(1)
     write(*,*) 'Interf_hr%Opd(N_Sample)',Interf_hr%Opd(Interf_hr%N_Sample)
!
!    Fourier transform of the high resolution spectrum
     call sptoif( Spectrum_hr, Interf_hr )
!
!    Interferogram resampling
     Interf_lr%Type     = 'R'
     Interf_lr%N_Sample = L1NsFft + 1
     Interf_lr%OpdMax   = 1.d+00 / ( 2.d+00*Spectrum%dWn )
     Interf_lr%dOpd     = Interf_lr%OpdMax              &
                        / dble(int(Interf_lr%N_Sample/2))
     Interf_lr%dTime = Interf_lr%dOpd
     call alloc_Interferogram( Interf_lr )
     call Interferogram_Basis( Interf_lr )
     Opd_Pivot = Interf_lr%Opd(int(Interf_lr%N_Sample/2)+1)
     write(*,*) 'Opd_Pivot lr           ',Opd_Pivot
     Interf_lr%Opd(1:Interf_lr%N_Sample) = &
                Interf_lr%Opd(1:Interf_lr%N_Sample) - Opd_Pivot
     write(*,*) 'Interf_lr%Type         ',Interf_lr%Type
     write(*,*) 'Interf_lr%N_Sample     ',Interf_lr%N_Sample
     write(*,*) 'Interf_lr%OpdMax       ',Interf_lr%OpdMax
     write(*,*) 'Interf_lr%dOpd         ',Interf_lr%dOpd
     write(*,*) 'Interf_lr%Opd(1)       ',Interf_lr%Opd(1)
     write(*,*) 'Interf_lr%Opd(N_Sample)',Interf_lr%Opd(Interf_lr%N_Sample)
!
!    extraction of the lr interferogram
     NsDel = int(NsFft/2) - int(L1NsFft/2)
     write(*,*) 'NsDel                  ',NsDel
     write(*,*) 'Interf_hr%Opd(NsDel+1) ',Interf_hr%Opd(NsDel+1)
     write(*,*) 'Interf_hr%Opd(NsDel+N) ',&
                        Interf_hr%Opd(NsDel+Interf_lr%N_Sample)
     Interf_lr%Real_Part(1:Interf_lr%N_Sample) =                       &
                   Interf_hr%Real_Part(NsDel+1:NsDel+Interf_lr%N_Sample)
!
!    complex interferogram parametrisation
     Interf_Saf%Type     = 'C'
     Interf_Saf%N_Sample = L1NsFft + 1
     Interf_Saf%OpdMax   = 1.d+00 / ( 2.d+00*Spectrum%dWn )
     Interf_Saf%dOpd     = Interf_Saf%OpdMax              &
                         / dble(int(Interf_Saf%N_Sample/2))
     Interf_Saf%dTime    = Interf_Saf%dOpd
     call alloc_Interferogram( Interf_Saf )
     call Interferogram_Basis( Interf_Saf )
     Opd_Pivot = Interf_Saf%Opd(int(Interf_Saf%N_Sample/2)+1)
     write(*,*) 'Opd_Pivot Interf_Saf',Opd_Pivot
     Interf_Saf%Opd(1:Interf_Saf%N_Sample) = &
                Interf_Saf%Opd(1:Interf_Saf%N_Sample) - Opd_Pivot
     write(*,*) 'Interf_Saf%Type         ',Interf_Saf%Type
     write(*,*) 'Interf_Saf%N_Sample     ',Interf_Saf%N_Sample
     write(*,*) 'Interf_Saf%OpdMax       ',Interf_Saf%OpdMax
     write(*,*) 'Interf_Saf%dOpd         ',Interf_Saf%dOpd
     write(*,*) 'Interf_Saf%Opd(1)       ',Interf_Saf%Opd(1)
     write(*,*) 'Interf_Saf%Opd(N_Sample)',Interf_Saf%Opd(Interf_Saf%N_Sample)
!
!    limits in the spectrum space
     Ns_First = Spectrum%Ns_First + int(Interf_Saf%N_Sample/2)
     Ns_Last  = Spectrum%Ns_Last  + int(Interf_Saf%N_Sample/2)
!
!    convolution limits
     n1 = 1
     n2 = Saf%NsWn0
     call dichotomd( Spectrum%Wn_First, Saf%Wn0, n1, n2 ) 
     NSafStart = n1
     n1 = 1
     n2 = Saf%NsWn0
     call dichotomd( Spectrum%Wn_Last, Saf%Wn0, n1, n2 )
     NSafEnd = n2
     allocate( Saf_Arg_Cal(Saf%NsOpd) )
     allocate( Saf_Mod(Interf_Saf%N_Sample) )
     allocate( Saf_Arg(Interf_Saf%N_Sample) )
     allocate( Saf_cplx(Interf_Saf%N_Sample) )
!
!    Spectrum parametrisation
     Spectrum_1%N_Sample = Spectrum%N_Sample
     Spectrum_2%N_Sample = Spectrum%N_Sample
     Spectrum_1%Type     = 'C'
     Spectrum_2%Type     = 'C'
     Spectrum_1%dWn      = Spectrum%dWn
     Spectrum_2%dWn      = Spectrum%dWn
     Spectrum_1%Ns_First = Spectrum%Ns_First
     Spectrum_2%Ns_First = Spectrum%Ns_First
     Spectrum_1%Ns_Last  = Spectrum%Ns_Last
     Spectrum_2%Ns_Last  = Spectrum%Ns_Last
     call alloc_Spectrum( Spectrum_1 )
     call alloc_Spectrum( Spectrum_2 )
     allocate( Spectre(Spectrum%N_Sample) )
     write(*,*) 'Opd 1       ',Saf%Opd(1),Interf_Saf%Opd(1)
     write(*,*) 'Opd N_Sample',Saf%Opd(Saf%NsOpd),&
                               Interf_Saf%Opd(Interf_Saf%N_Sample)
!
!    tabulated self apodisation loop
     WnSafEnd = Saf%Wn0( NSafStart )
     do NSaf = NSafStart, NSafEnd
!
        WnSafStart = WnSafEnd
        WnSafEnd   = Saf%Wn0( NSaf )
!
!       spectral calibration
        if( Level == 'L1b' ) then
           if( Calib_Option <= 1 ) then
!
!             use of the SRF barycentre
              write(*,*) 'spectral calibration barycentre'
              Saf_Arg_Cal(1:Saf%NsOpd) =                        &
                             Saf%Arg(1:Saf%NsOpd,NSaf,SUB_PN)   &
                           - ( twoPi*Saf%Opd(1:Saf%NsOpd)       &
                                    *Srf%WnShift1a(NSaf,SUB_PN) )
           else 
!
!             use of the SAF argument slope at ZPD
              write(*,*) 'spectral calibration slope :',Calib_Option
              Saf_Arg_Cal(1:Saf%NsOpd) = Saf%Arg(1:Saf%NsOpd,NSaf,SUB_PN)
              call extract_linear_argument( Saf%NsOpd,             &
                                            Saf%Opd,               &
                                            Calib_Option,          &
                                            Saf%Mod(1,NSaf,SUB_PN),&
                                            Saf_Arg_Cal            )
           end if
        else if ( Level == 'L1a' ) then 
           write(*,*) 'no spectral calibration applied'
           Saf_Arg_Cal(1:Saf%NsOpd) = Saf%Arg(1:Saf%NsOpd,NSaf,SUB_PN)
        else
           write(0,*) 'wrong level :', Level
           call exit(1)
        end if
!   
!       interpolation in the computation discretisation
        call intspline_open( Saf%NsOpd             &
                            ,Saf%Opd               &
                            ,Saf%Mod(1,NSaf,SUB_PN)&
                            ,Interf_Saf%N_Sample   &
                            ,Interf_Saf%Opd        &
                            ,Saf_Mod               )
        call intspline_open( Saf%NsOpd             &
                            ,Saf%Opd               &
                            ,Saf_Arg_Cal           &
                            ,Interf_Saf%N_Sample   &
                            ,Interf_Saf%Opd        &
                            ,Saf_Arg               )
        call modargtocplx( Saf_cplx, Saf_Mod, Saf_Arg,&
                           Interf_Saf%N_Sample        )
!
        Interf_Saf%Complex(1:Interf_Saf%N_Sample) =        &
                  dconjg(Saf_cplx(1:Interf_Saf%N_Sample))  &
                * Interf_lr%Real_Part(1:Interf_Saf%N_Sample)
        write(*,*) 'Saf_Arg   ',Saf_Arg(1)
        write(*,*) 'Saf_Arg   ',Saf_Arg(Interf_Saf%N_Sample/2+1)
        write(*,*) 'Saf_Arg   ',Saf_Arg(Interf_Saf%N_Sample)
        write(*,*) 'Saf_cplx  ',dconjg(Saf_cplx(1))
        write(*,*) 'Saf_cplx  ',dconjg(Saf_cplx(Interf_Saf%N_Sample/2+1))
        write(*,*) 'Saf_cplx  ',dconjg(Saf_cplx(Interf_Saf%N_Sample))
        write(*,*) 'Interf_lr ',Interf_lr%Real_Part(1)
        write(*,*) 'Interf_lr ',Interf_lr%Real_Part(Interf_Saf%N_Sample/2+1)
        write(*,*) 'Interf_lr ',Interf_lr%Real_Part(Interf_Saf%N_Sample)
        write(*,*) 'Interf_Saf',Interf_Saf%Complex(1)
        write(*,*) 'Interf_Saf',Interf_Saf%Complex(Interf_Saf%N_Sample/2+1)
        write(*,*) 'Interf_Saf',Interf_Saf%Complex(Interf_Saf%N_Sample)
!
!       interferogram conversion into the spectrum space
        call iftosp_open( Interf_Saf, Spectrum_2 )
!
!       interpolation between WnSafStart and WnSafEnd
        if( NSaf .ne. NSafStart ) then
           NsStart = Ns_First
           NsEnd   = Ns_Last
           if( WnSafStart .gt. Spectrum%Wn_First ) &
              NsStart = Ns_First &
                      + idnint( (WnSafStart-Spectrum%Wn_First)/Spectrum%dWn )
           if( WnSafEnd .lt. Spectrum%Wn_Last )    &
              NsEnd   = Ns_First &
                      + idnint( (WnSafEnd  -Spectrum%Wn_First)/Spectrum%dWn )
           do Ns = NsStart, NsEnd
              n = Ns - Ns_First + 1
              beta  = (Spectrum%Wn(n)-WnSafStart)/(WnSafEnd-WnSafStart)
              alpha = 1.d+00-beta
              Spectre(n) = ( alpha*Spectrum_1%Complex(n) &
                            + beta*Spectrum_2%Complex(n) )
           end do
        end if
!
!       transfert sp2 into sp1
        Spectrum_1%Complex(1:Spectrum%N_Sample) = &
                        Spectrum_2%Complex(1:Spectrum%N_Sample) 
     end do ! end self apodisation loop
!
     if( Spectrum%Type == 'C' ) then
        Spectrum%Complex(1:Spectrum%N_Sample) = &
                       Spectre(1:Spectrum%N_Sample)
     else if( Spectrum%Type == 'R' ) then
        Spectrum%Real_Part(1:Spectrum%N_Sample) = &
                       dreal(Spectre(1:Spectrum%N_Sample))
     else
        write(*,*) 'Spectrum Type Error must be C or R'
        write(*,*) 'cnv_ft Fatal Error'
        call exit(1)
     end if
!
!    de allocation
     deallocate( Saf_cplx )
     deallocate( Saf_Mod )
     deallocate( Saf_Arg )
     deallocate( Saf_Arg_Cal )
     deallocate( Spectre )
     call dalloc_Interferogram( Interf_Saf )
     call dalloc_Interferogram( Interf_lr )
     call dalloc_Interferogram( Interf_hr )
     call dalloc_Spectrum( Spectrum_1 )
     call dalloc_Spectrum( Spectrum_2 )
     return
   end subroutine cnv_ft
!
!
end module convolution_module
