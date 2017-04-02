! derivation_module.f90 --
!
!           Project: SPS_GENERIC
!           Authors: NOVELTIS/B.TOURNIER
!              Date: may 2011
!           Version: $Revision: 1.1 $
! Last modification: $Date: 2011-06-22 10:06:26 $
!
! (c) Noveltis
!
! --------


! derivation -- Module
!
! * Purpose
!   This module contains parameters, sub-routines and functions for classic
!   derivation tools.
module derivation_module

  use precision_type
  use constantes_type
  use interferogram_type
  use spectrum_type
  use saf_type
  use srf_type
  use srf_moments_type
  use fft_module
  use math_module

  implicit none

  public ::                 &
       derivationh2,        &
       derivationh6,        &
       derivation_sp_ft,    &
       derivation_saf_ft
!
   contains
!
!
   subroutine derivationh2( n, dx, y, dysdx )
   implicit none
   integer(kind=LONG),intent(in)                :: n
   real(kind=DOUBLE) ,intent(in)                :: dx
   real(kind=DOUBLE),dimension(n),intent(in)    :: y
   real(kind=DOUBLE),dimension(n),intent(inout) :: dysdx
   integer(kind=LONG)             :: i

     i = 1
     dysdx(i) = (y(i+1)-y(i))/ (dx)
     do i = 2, n-1
       dysdx(i) = (y(i+1)-y(i-1))/ (2.d+00*dx)
     enddo
     i = n
     dysdx(i) = (y(i)-y(i-1))/ (dx)
     
     return
   end subroutine derivationh2
!
!
   subroutine derivationh6( n, dx, y, dysdx )
   implicit none
   integer(kind=LONG),intent(in)                :: n
   real(kind=DOUBLE) ,intent(in)                :: dx
   real(kind=DOUBLE),dimension(n),intent(in)    :: y
   real(kind=DOUBLE),dimension(n),intent(inout) :: dysdx
   integer(kind=LONG)             :: i
   real(kind=DOUBLE)              :: norm
   real(kind=DOUBLE),dimension(7) :: x6g,x5g,x4g,x3c,x4d,x5d,x6d

!precision = erreur*h**6*f(7) : 1/7, 1/42, 1/105, 1/140, 1/105, 1/42, 1/7
   data x6g/10d0,-72d0,225d0,-400d0,450d0,-360d0,147d0/
   data x5g/-2d0,15d0,-50d0,100d0,-150d0,77d0,10d0/
   data x4g/1d0,-8d0,30d0,-80d0,35d0,24d0,-2d0/
   data x3c/-1d0,9d0,-45d0,0d0,45d0,-9d0,1d0/
   data x4d/2d0,-24d0,-35d0,80d0,-30d0,8d0,-1d0/
   data x5d/-10d0,-77d0,150d0,-100d0,50d0,-15d0,2d0/
   data x6d/-147d0,360d0,-450d0,400d0,-225d0,72d0,-10d0/
   data norm/60d0/

!    i = 1
     dysdx(1) = x6d(1)*y(1)+x6d(2)*y(2)+x6d(3)*y(3)+x6d(4)*y(4) &
               +x6d(5)*y(5)+x6d(6)*y(6)+x6d(7)*y(7)

!    i = 2
     dysdx(2) = x5d(1)*y(1)+x5d(2)*y(2)+x5d(3)*y(3)+x5d(4)*y(4) &
               +x5d(5)*y(5)+x5d(6)*y(6)+x5d(7)*y(7)

!    i = 3
     dysdx(3) = x4d(1)*y(1)+x4d(2)*y(2)+x4d(3)*y(3)+x4d(4)*y(4) &
               +x4d(5)*y(5)+x4d(6)*y(6)+x4d(7)*y(7)

!    i >=4 et <=n-3
     do i = 4, n-3
        dysdx(i) = x3c(1)*y(i-3)+x3c(2)*y(i-2)+x3c(3)*y(i-1)+x3c(4)*y(i) &
                  +x3c(5)*y(i+1)+x3c(6)*y(i+2)+x3c(7)*y(i+3)
     end do

!    i = n-2
     dysdx(n-2) = x4g(1)*y(n-6)+x4g(2)*y(n-5)+x4g(3)*y(n-4)+x4g(4)*y(n-3) &
                 +x4g(5)*y(n-2)+x4g(6)*y(n-1)+x4g(7)*y(n)

!    i = n-1
     dysdx(n-1) = x5g(1)*y(n-6)+x5g(2)*y(n-5)+x5g(3)*y(n-4)+x5g(4)*y(n-3) &
                 +x5g(5)*y(n-2)+x5g(6)*y(n-1)+x5g(7)*y(n)

!    i = n 
     dysdx(n) = x6g(1)*y(n-6)+x6g(2)*y(n-5)+x6g(3)*y(n-4)+x6g(4)*y(n-3) &
               +x6g(5)*y(n-2)+x6g(6)*y(n-1)+x6g(7)*y(n)

     do i = 1, n
       dysdx(i) = dysdx(i)/(norm*dx)
     enddo
     
     return
   end subroutine derivationh6
!
!
   subroutine derivation_sp_ft( N_Derive, Nth, SigI, Level, &
                                Spectrum_Hr, Spectrum_Ref,  &
                                Interf, Spectrum_Derive )
   implicit none
     integer(kind=LONG)        ,intent(in)                :: N_Derive
     integer(kind=LONG)        ,intent(in)                :: Nth
     integer(kind=LONG)        ,intent(in)                :: SigI
     character(len=*)          ,intent(in)                :: Level
     type(type_Spectrum)       ,intent(in)                :: Spectrum_Hr
     type(type_Spectrum)       ,intent(in)                :: Spectrum_Ref
     type(type_Interferogram)  ,intent(inout)             :: Interf
     type(type_Spectrum)       ,intent(inout)             :: Spectrum_Derive
     complex(kind=DOUBLE) ,dimension(:),allocatable       :: I_cplx
     complex(kind=DOUBLE) ,dimension(:),allocatable       :: S_cplx
     integer(kind=LONG)                                   :: ios
     integer(kind=LONG)                                   :: NsFft
     integer(kind=LONG)                                   :: NsFftp
     integer(kind=LONG)                                   :: NsDel
     integer(kind=LONG)                                   :: N_Sample
     integer(kind=LONG)                                   :: ErrCode
     integer(kind=LONG)                                   :: Sens
     real(kind=DOUBLE)                                    :: Opd0
     real(kind=DOUBLE)                                    :: OpdMax
     real(kind=DOUBLE)    ,dimension(:),allocatable       :: Fct
     real(kind=DOUBLE)    ,dimension(:),allocatable       :: Part_Real
     real(kind=DOUBLE)    ,dimension(:),allocatable       :: Part_Imag
!
!    Spectrum header transfert
     call Spectrum_Header_Transfer( Spectrum_Ref, Spectrum_Derive )
!
!    Interferogram computation
     if( Nth == 0 ) then
!
!       FFT size computation
        N_Sample = ( 2 * idnint( Spectrum_Hr%WnMax / Spectrum_Hr%dWn ) ) + 1
        call tabule_NsFft( N_Sample, NsFft, ios )
        if( ios /= 0 ) then
           write(*,*) 'tabule_NsFFT error'
           write(*,*) 'derivation_ft Error'
           call exit(1)
        end if
        NsFftp = int(NsFft/2)
        Interf%N_Sample = NsFft + 1
        Interf%Type     = 'R'
        Interf%OpdMax   = 1.d+00 / ( 2.d+00*Spectrum_Hr%dWn )
        Interf%dOpd     = 1.d+00 / ( 2.d+00*Spectrum_Hr%dWn*dble(NsFftp) )
        call alloc_Interferogram( Interf )
        call Interferogram_Basis( Interf )
        Opd0 = Interf%Opd(NsFftp+1)
        Interf%Opd(1:Interf%N_Sample) = Interf%Opd(1:Interf%N_Sample) - Opd0
        call sptoif( Spectrum_Hr, Interf )
!
!       Opd reduction
        OpdMax = 1.d+00 / (2.d+00*Spectrum_Ref%dWn)
        N_Sample = 2*idnint(OpdMax/Interf%dOpd)
        call tabule_NsFft( N_Sample, NsFft, ios )
        if( NsFft /= N_Sample ) then
           write(*,*) 'NsFft Error'
           call exit(1)
        end if
        NsFftp = int(NsFft/2)
        NsDel = int(Interf%N_Sample/2) - int(N_Sample/2)
        write(*,*) 'OpdMax   ',OpdMax
        write(*,*) 'N_Sample',N_Sample
        write(*,*) 'NsDel   ',NsDel
        if( NsDel < 0 .and.N_Sample > NsDel ) then
           write(*,*) 'Opd Reduction Error'
           call exit(1)
        end if
        N_Sample = N_Sample + 1
        Interf%Real_Part(1:N_Sample) = Interf%Real_Part(NsDel+1:NsDel+N_Sample)
        Interf%Opd(1:N_Sample) = Interf%Opd(NsDel+1:NsDel+N_Sample)
        Interf%N_Sample = N_Sample
        Interf%OpdMax   = OpdMax
        if( SigI /= 0 ) then
           call windowing( Interf%N_Sample, &
                           Interf%Real_Part,&
                           SigI,            &
                           Interf%dOpd      )
        end if
!
!       transfert function application
        allocate( Fct(Interf%N_Sample) )
        if( (Level == 'L1a') .or. (Level == 'L1b') ) then
!          pure sinc
           Fct(1:Interf%N_Sample) = 1.d+00
        else if( (Level == 'L1c') ) then
!          pure Gauss
!           call intspline( Saf%NsOpd,       &
!                           Saf%Opd,         &
!                           Srf%L1cTF,       &
!                           Interf%N_Sample, &
!                           Interf%Opd,      &
!                           Fct              )
           Fct(1:Interf%N_Sample) = 1.d+00
        else
           write(*,*) 'Level Error in derivation'
           call exit(1)
        end if
        Interf%Real_Part(1:Interf%N_Sample) =  Fct(1:Interf%N_Sample) &
                               *  Interf%Real_Part(1:Interf%N_Sample)
        call iftosp( Interf, Spectrum_Derive )
        Interf%Type     = 'C'
        allocate( Interf%Complex(Interf%N_Sample) )
        Interf%Complex(1:Interf%N_Sample) = &
                  dcmplx( Interf%Real_Part(1:Interf%N_Sample), 0.d+00 )
        deallocate( Interf%Real_Part )
        allocate( Part_Real(NsFft+1))
        allocate( Part_Imag(NsFft+1))
        call cplxtorealimag( Interf%Complex, Part_Real, Part_Imag, NsFft+1 )
        deallocate( Part_Real )
        deallocate( Part_Imag )
        deallocate( Fct )
     else
!
!       spectrum derivation
        NsFft = Interf%N_Sample - 1
        NsFftp = int(NsFft/2)
        allocate( Part_Real(Interf%N_Sample))
        allocate( Part_Imag(Interf%N_Sample))
        allocate( I_cplx(Interf%N_Sample), stat=ErrCode )
        allocate( S_cplx(Interf%N_Sample), stat=ErrCode )
        I_cplx(1:Interf%N_Sample)= Interf%Opd(1:Interf%N_Sample)     &
                                 * Interf%Complex(1:Interf%N_Sample) &
                                 * twoPi * dcmplx( 0.d+00, -1.d+00 )
        call cplxtorealimag( I_cplx, Part_Real, Part_Imag, Interf%N_Sample )
        Sens = -1
        call fft_c2c_open( NsFft, Sens, I_cplx, Interf%dOpd, &
                           Spectrum_Ref%dWn , S_cplx )
        call cplxtorealimag( S_cplx, Part_Real, Part_Imag, Interf%N_Sample )
        Spectrum_Derive%Real_Part(1:Spectrum_Derive%N_Sample) = &
                     dreal( S_cplx(NsFftp+Spectrum_Ref%Ns_First &
                                  :NsFftp+Spectrum_Ref%Ns_Last) )
        Interf%Complex(1:Interf%N_Sample) = I_cplx(1:Interf%N_Sample)
        call cplxtorealimag( Interf%Complex, Part_Real, Part_Imag, &
                             Interf%N_Sample )
        if( SigI /= 0 ) then
           call windowingcplx( Interf%N_Sample, &
                               Interf%Complex,  &
                               SigI,            &
                               Interf%dOpd      )
        end if
        deallocate( Part_Real )
        deallocate( Part_Imag )
        deallocate( I_cplx )
        deallocate( S_cplx )
     end if
     if( Nth == N_Derive ) then
       call dalloc_Interferogram( Interf )
     end if
     return
   end subroutine derivation_sp_ft
!
!
   subroutine derivation_saf_ft( Nth, NF, SigS, Level,   &
                                 NsFft1a, OSFactor, Calib_Option,  &
                                 Saf, Srf, Saf_Derive, Srf_Moments )
   implicit none
     integer(kind=LONG)        ,intent(in)                :: Nth
     integer(kind=LONG)        ,intent(in)                :: NF
     integer(kind=LONG)        ,intent(in)                :: SigS
     character(len=*)          ,intent(in)                :: Level
     integer(kind=LONG)        ,intent(in)                :: NsFft1a
     integer(kind=LONG)        ,intent(in)                :: OSFactor
     integer(kind=LONG)        ,intent(in)                :: Calib_Option
     type(type_Saf)            ,intent(inout)             :: Saf
     type(type_Srf)            ,intent(in)                :: Srf
     type(type_Srf_Moments)    ,intent(inout)             :: Srf_Moments
     type(type_Spectrum)       ,intent(inout)             :: Saf_Derive
!
     complex(kind=DOUBLE) ,dimension(:),allocatable       :: Srf_cplx
     complex(kind=DOUBLE) ,dimension(:),allocatable       :: Saf_cplx
     integer(kind=LONG)                                   :: NsFft
     integer(kind=LONG)                                   :: NsDel
     integer(kind=LONG)                                   :: ErrCode
     integer(kind=LONG)                                   :: Sens
     integer(kind=LONG)                                   :: Ns
     integer(kind=LONG)                                   :: Ms
     integer(kind=LONG)                                   :: OSNsOpd
     integer(kind=LONG)                                   :: NZpd
     real(kind=DOUBLE)                                    :: dWn
     real(kind=DOUBLE)                                    :: OSdOpd
     real(kind=DOUBLE)    ,dimension(:),allocatable       :: OSOpd
     real(kind=DOUBLE)    ,dimension(:),allocatable       :: OSMod
     real(kind=DOUBLE)    ,dimension(:),allocatable       :: OSArg
     complex(kind=DOUBLE) ,dimension(:),allocatable       :: OSCplx
     complex(kind=DOUBLE) ,dimension(:),allocatable       :: Saf_0pad
     real(kind=DOUBLE)    ,dimension(:),allocatable       :: Part_Real
     real(kind=DOUBLE)    ,dimension(:),allocatable       :: Part_Imag
!
!    Sas selection
     OSNsOpd = NsFft1a + 1
     OSdOpd  = 2.d+00 * Saf%OpdMax / dble(NsFft1a)
     NZpd = int(OSNsOpd/2)+1
     allocate( OSOpd(OSNsOpd),  stat=ErrCode)
     allocate( OSMod(OSNsOpd),  stat=ErrCode)
     allocate( OSArg(OSNsOpd),  stat=ErrCode)
     allocate( OSCplx(OSNsOpd),  stat=ErrCode)
     OSOpd(1:OSNsOpd) = (/ (dble(Ns-NZpd),Ns=1,OSNsOpd) /) * OSdOpd
!
     NsFft = OSFactor * NsFft1a
     dWn = Saf_Derive%dWn * OSFactor
     dWn = Saf_Derive%dWn 
     if( Saf_Derive%N_Sample /= NsFft+1 ) then
         write(*,*) ' derivation_saf_ft NsFft Error'
         call exit(1)
     end if
     NsDel = int(NsFft/2) - int(NsFft1a/2)
     allocate( Saf_0pad(NsFft+1) )
     allocate( Saf_cplx(NsFft+1) )
     allocate( Srf_cplx(NsFft+1) )
     allocate( Part_Real(NsFft+1))
     allocate( Part_Imag(NsFft+1))
     if( Nth == 0 ) then
           if(     Level == 'L1a' ) then
              call intspline( Saf%NsOpd,               &
                              Saf%Opd,                 &
                              Saf%Mod(1:Saf%NsOpd,NF,  &
                                        Saf%NsPixel+1),&
                              OSNsOpd,                 &
                              OSOpd,                   &
                              OSMod                    )
              call intspline( Saf%NsOpd,               &
                              Saf%Opd,                 &
                              Saf%Arg(1:Saf%NsOpd,NF,  &
                                        Saf%NsPixel+1),&
                              OSNsOpd,                 &
                              OSOpd,                   &
                              OSArg                    )
           else if( Level == 'L1b' ) then
!
!             extract linear argument
              call extract_linear_argument(                       &
                            Saf%NsOpd,Saf%Opd,Calib_Option,       &
                            Saf%Mod(1:Saf%NsOpd,NF,Saf%NsPixel+1),&
                            Saf%Arg(1:Saf%NsOpd,NF,Saf%NsPixel+1) )
              call intspline( Saf%NsOpd,               &
                              Saf%Opd,                 &
                              Saf%Mod(1:Saf%NsOpd,NF,  &
                                        Saf%NsPixel+1),&
                              OSNsOpd,                 &
                              OSOpd,                   &
                              OSMod                    )
              call intspline( Saf%NsOpd,               &
                              Saf%Opd,                 &
                              Saf%Arg(1:Saf%NsOpd,NF,  &
                                        Saf%NsPixel+1),&
                              OSNsOpd,                 &
                              OSOpd,                   &
                              OSArg                    )
           else if( Level == 'L1c' ) then
              call intspline( Saf%NsOpd,              &
                              Saf%Opd,                &
                              Srf%L1cTF(1:Saf%NsOpd), &
                              OSNsOpd,                &
                              OSOpd,                  &
                              OSMod                   )
              OSArg(1:OSNsOpd) = 0.d+00
           else
              write(*,*) 'Level Error'
              call exit(1)
           end if
           call modargtocplx( OSCplx, &
                              OSMod,  &
                              OSArg,  &
                              OSNsOpd )
!
!          SAF zero padding
           Saf_0pad(1:NsFft+1) = dcmplx( 0.d+00, 0.d+00 )
           Saf_0pad(NsDel+1:NsDel+NsFft1a+1) = OSCplx(1:OSNsOpd)
           Sens = -1
           call fft_c2c_open( NsFft, Sens, Saf_0pad,&
                              OSdOpd, dWn, Srf_cplx )
!
           call cplxtorealimag( Srf_cplx, Part_Real, Part_Imag, NsFft+1 )
           if( SigS /= 0 ) then
              call windowingcplx( NsFft,         &
                                  Srf_cplx,      &
                                  SigS,          &
                                  Saf_Derive%dWn )
           end if
           Ms = int(NsFft/2) + 1
           Srf_Moments%Derive0(NF,Nth) = Saf_0pad(Ms)
           Srf_Moments%Moments(NF,Nth) = dcmplx( dcos(Nth*Pi/2.d+00), &
                                                 dsin(Nth*Pi/2.d+00) )&
                                       * Srf_Moments%Derive0(NF,Nth)  &
                                       * 1.d+00 / (twoPi**Nth)
!!!           Srf_Moments%Moments(NF,Nth) = Srf_Moments%Derive0(NF,Nth)
!
           Saf_Derive%Complex(1:Saf_Derive%N_Sample) = Srf_cplx(1:NsFft+1)
     else
!
!          saf derivation
           Srf_cplx(1:NsFft+1) = Saf_Derive%Wn(1:NsFft+1)        &
                               * Saf_Derive%Complex(1:NsFft+1)   &
                               * twoPi * dcmplx( 0.d+00, 1.d+00 )&
                               / dble(NsFft)
           call cplxtorealimag( Srf_cplx, Part_Real, Part_Imag, NsFft+1 )
           Sens = +1
           call fft_c2c_open( NsFft, Sens, Srf_cplx,&
                              dWn, OSdOpd, Saf_cplx )
!
           call cplxtorealimag( Saf_cplx, Part_Real, Part_Imag, NsFft+1 )
           Ms = int(NsFft/2) + 1
           Srf_Moments%Derive0(NF,Nth) = Saf_cplx(Ms)
           Srf_Moments%Moments(NF,Nth) = dcmplx( dcos(Nth*Pi/2.d+00), &
                                                -dsin(Nth*Pi/2.d+00) )&
                                       * Srf_Moments%Derive0(NF,Nth)  &
                                       * 1.d+00 / (twoPi**Nth)
!!!           Srf_Moments%Moments(NF,Nth) = Srf_Moments%Derive0(NF,Nth)
!!!           Sens = -1
!!!           call fft_c2c_open( NsFft, Sens, Saf_cplx,&
!!!                              OSdOpd, dWn, Srf_cplx )
           Saf_Derive%Complex(1:Saf_Derive%N_Sample) = Srf_cplx(1:NsFft+1)
           if( SigS /= 0 ) then
              call windowingcplx( Saf_Derive%N_Sample,&
                                  Saf_Derive%Complex, &
                                  SigS,               &
                                  Saf_Derive%dWn      )
           end if
     end if
     deallocate( Saf_cplx )
     deallocate( Srf_cplx )
     deallocate( Saf_0pad )
     deallocate( OSOpd )
     deallocate( OSMod )
     deallocate( OSArg )
     deallocate( Part_Real )
     deallocate( Part_Imag )
     return
   end subroutine derivation_saf_ft
!
!
end module derivation_module
