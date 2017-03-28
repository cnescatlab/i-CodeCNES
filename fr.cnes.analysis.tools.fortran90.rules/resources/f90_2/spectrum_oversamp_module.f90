!!#   spectrum_oversamp_module.f90 --
!!# 
!!#            Project: SPS_GENERIC
!!#            Authors: NOVELTIS/B.TOURNIER
!!#               Date: march 2010
!!#            Version: $Revision: 1.7 $
!!#  Last modification: $Date: 2012-02-08 10:19:15 $
!!#
!!#  Language:  F90
!!#  Standards: Noveltis
!!#
!!# --
!!#
!!
!>  spectrum oversampling -- Module
!!
!! * Purpose
!!
!!   Module for Spectrum Oversampling.
!!
!! * Description
!!      
!!   The objective of this module is to propose different methods that allow oversampling
!!   a low-resolution spectrum: oversampling can be done by zero padding, by dephasing or 
!!   by spline interpolation.
!!
!! * Sub-routines and functions
!!
!! -  spectrum_os_zero_padding : low-resolution spectrum oversampling by zero padding.
!! -  spectrum_os_dephasing    : low-resolution spectrum oversampling by dephasing.
!! -  spectrum_os_spline       : low-resolution spectrum oversampling by spline interpolation.
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.8.12
!!

module spectrum_oversamp_module
   use precision_type
   use error_type
   use constantes_type
   use spectrum_type
   use interferogram_type
   use fft_module
   use math_module
!
   implicit none
!
!
   public :: spectrum_os_zero_padding, &
             spectrum_os_dephasing,    &
             spectrum_os_spline
!
   contains
!
!

!
!
!> spectrum_os_zero_padding -- Public
!!
!! * Purpose
!!
!!     Low-resolution spectrum oversampling by zero padding
!!
!! * Description
!! 
!!     This subroutine allows to oversample low-resolution spectrum by zero padding.
!!     The method consists in oversampling spectrum by inverse Fourier transform of an 
!!     interferogram (direct Fourier Transform of the low resolution spectrum) extended 
!!     with zeros (Zero padding). 
!!     Fisrt, the algorithm starts with the computation of the direct Fourier transform of 
!!     the low resolution spectrum (sampling of Delta_nu) to get the interferogram with NsFFT
!!     samples. The oversampled interferogram of a factor OSFactor has (NsFFT*OSFactor)
!!     samples. The oversampled interferogram is filled with zeros. At last the inverse Fourier 
!!     transform of the oversampled interferogram gives the oversampled spectrum.
!!
!! * Inputs
!!
!!     - WnS      : first wavenumber
!!     - WnE      : last wavenumber
!!     - OSFactor : oversampling factor
!!     - Spectrum : type_Spectrum / type for declaration and allocation of spectrum 
!! 
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Spectrum_os : type_Spectrum / type for declaration and allocation of spectrum
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.8.12.1
!!


   subroutine spectrum_os_zero_padding( WnS, WnE,   &
                                        NsFft_in,   &
                                        OSFactor,   &
                                        Spectrum,   &
                                        Spectrum_os )
     implicit none
     real(kind=DOUBLE)       ,intent(in)                 :: WnS
     real(kind=DOUBLE)       ,intent(in)                 :: WnE
     integer(kind=LONG)      ,intent(in)                 :: NsFft_in
     integer(kind=LONG)      ,intent(in)                 :: OSFactor
     type(type_Spectrum)     ,intent(in)                 :: Spectrum
     type(type_Spectrum)     ,intent(out)                :: Spectrum_os
     type(type_Spectrum)                                 :: Spectrum_Local
     integer(kind=LONG)                                  :: NsFft
     integer(kind=LONG)                                  :: OSNsFft
     integer(kind=LONG)                                  :: ios
     type(type_Interferogram)                            :: Interf
     type(type_Interferogram)                            :: OSInterf
     integer(kind=LONG)                                  :: NsDel
     integer(kind=LONG)                                  :: N_Sample_Max
!
     write(*,*) 'spectrum_os_zero_padding *********************'
!
!    Low Resolution spectrum definition
     Spectrum_Local%Type     = Spectrum%Type
     Spectrum_Local%dWn      = Spectrum%dWn
     Spectrum_Local%Ns_First = idnint( WnS / Spectrum_Local%dWn ) + 1
     Spectrum_Local%Ns_Last  = idnint( WnE / Spectrum_Local%dWn ) + 1
     Spectrum_Local%Wn_First = dble( Spectrum_Local%Ns_First-1) &
                             * Spectrum_Local%dWn
     Spectrum_Local%Wn_Last  = dble( Spectrum_Local%Ns_Last-1)  &
                             * Spectrum_Local%dWn
     Spectrum_Local%N_Sample = Spectrum_Local%Ns_Last &
                             - Spectrum_Local%Ns_First + 1
     call alloc_Spectrum( Spectrum_Local )
     call Spectrum_Basis( Spectrum_Local )
     NsDel = Spectrum_Local%Ns_First - Spectrum%Ns_First
     if( NsDel < 0 .or.                                    &
         NsDel+Spectrum_Local%N_Sample > Spectrum%N_Sample ) then
        write(*,*) 'Incompatible spectral limits',NsDel
        write(*,*) 'spectrum_os_zero_padding Fatal Error'
        call exit(1)
     end if
     write(*,*) 'NsDel',NsDel
     Spectrum_Local%Real_Part(1:Spectrum_Local%N_Sample) =            &
              Spectrum%Real_Part(NsDel+1:NsDel+Spectrum_Local%N_Sample)
!
     if( NsFft_in == 0 ) then
!
!       Compute FFT size
        N_Sample_Max = 2 * (Spectrum_Local%Ns_Last*1.2)
        call tabule_NsFft( N_Sample_Max,&
                           NsFft, ios   )
        if( ios /= 0 ) then
           write(*,*) 'tabule_NsFft Error',NsFft
           write(*,*) 'spectrum_zero_padding Fatal Error'
           call exit(1)
        end if
     else
        NsFft = NsFft_in
        write(*,*) ' OverSampling NsFft ',NsFft
     end if
     Spectrum_Local%WnMax    = dble(int(NsFft/2)) * Spectrum_Local%dWn
     write(*,*) 'Spectrum_Local%N_Sample',Spectrum_Local%N_Sample
!
!    Interferogram computation
     Interf%Type    = 'R'
     Interf%dOpd = 1.d+00 / (2.d+00*dble(int(NsFft/2))*Spectrum%dWn)
     Interf%N_Sample = NsFft + 1
     call alloc_Interferogram( Interf )
     write(*,*) 'sptoif Spectrum, Interf'
     call sptoif( Spectrum_Local, Interf )
     write(*,*) 'End sptoif Spectrum, Interf'
!!
!    Zero padded interferogram
     OSNsFft = NsFft * OSFactor
     OSInterf%Type = 'R'
     OSInterf%dOpd = Interf%dOpd
     OSInterf%N_Sample = OSNsFft + 1
     call alloc_Interferogram( OSInterf )
     NsDel = int(OSNsFft/2) - int(NsFft/2)
     if( NsDel < 0 .or.                 &
         NsDel+NsFft+1 > OSNsFft+1 ) then
        write(*,*) 'Incompatible FFT size',NsDel
        write(*,*) 'spectrum_os_zero_padding Fatal Error'
        call exit(1)
     end if
     write(*,*) 'NsDel',NsDel
     OSInterf%Real_Part(1:OSNsFft+1) = 0.d+00
!
!    open domain (last sample set to 0)
     OSInterf%Real_Part(NsDel+1:NsDel+NsFft) = &
                     Interf%Real_Part(1:NsFft)
!
!    High Resolution spectrum definition
     Spectrum_os%Type     = Spectrum%Type
     Spectrum_os%dWn      = Spectrum%dWn / dble(OSFactor)
     Spectrum_os%Ns_First = idnint( WnS / Spectrum_os%dWn ) + 1
     Spectrum_os%Ns_Last  = idnint( WnE / Spectrum_os%dWn ) + 1
     Spectrum_os%Wn_First = dble(Spectrum_os%Ns_First-1) * Spectrum_os%dWn
     Spectrum_os%Wn_Last  = dble(Spectrum_os%Ns_Last-1)  * Spectrum_os%dWn
     Spectrum_os%N_Sample = Spectrum_os%Ns_Last - Spectrum_os%Ns_First + 1
     Spectrum_os%WnMax    = dble(int(NsFft/2)) * Spectrum%dWn
     call alloc_Spectrum( Spectrum_os )
     call Spectrum_Basis( Spectrum_os )
!
!    Oversampled spectrum
     write(*,*) 'iftosp OSInterf Spectrum_os'
     call iftosp_open( OSInterf, Spectrum_os )
!
!    Deallocation
     call dalloc_Interferogram( Interf )
     call dalloc_Interferogram( OSInterf )
     call dalloc_Spectrum( Spectrum_Local )
!
     return
   end subroutine spectrum_os_zero_padding
!
!

!
!
!> spectrum_os_dephasing -- Public
!!
!! * Purpose
!!
!!     Low-resolution spectrum oversampling by dephasing
!!
!! * Description
!! 
!!     This subroutine allows to oversample low-resolution spectrum by dephasing.
!!     The method consists in the computation of intermediate samples, obtained by successive
!!     applications of the inverse Fourier Transform to the product of direct Fourier Transform
!!     of the low-resolution spectrum and ad-hoc dephasing functions. 
!!     To oversample by a factor OSFactor the spectrum Spectrum, that has original sampling of
!!     Delta_nu, the algorithm starts by calculating the direct Fourier transform of the
!!     spectrum. The resulting interferogram is then dephased in function of the OSFactor.
!!     Then the inverse Fourier transform of the resulting interferogram is calculated. The 
!!     resulting spectrum is then oversampled by a factor OSFactor.
!!
!! * Inputs
!!
!!     - WnS      : first wavenumber
!!     - WnE      : last wavenumber
!!     - OSFactor : oversampling factor
!!     - Spectrum : type_Spectrum / type for declaration and allocation of spectrum 
!! 
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Spectrum_os : type_Spectrum / type for declaration and allocation of spectrum
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.8.12.2
!!

   subroutine spectrum_os_dephasing( WnS, WnE,   &
                                     NsFft_in,   &
                                     OSFactor,   &
                                     Spectrum,   &
                                     Spectrum_os )
     implicit none
     real(kind=DOUBLE)       ,intent(in)                 :: WnS
     real(kind=DOUBLE)       ,intent(in)                 :: WnE
     integer(kind=LONG)      ,intent(inout)              :: NsFft_in
     integer(kind=LONG)      ,intent(in)                 :: OSFactor
     type(type_Spectrum)     ,intent(in)                 :: Spectrum
     type(type_Spectrum)     ,intent(out)                :: Spectrum_os
     integer(kind=LONG)                                  :: ios
     integer(kind=LONG)                                  :: NsDel
     integer(kind=LONG)                                  :: Ns
     integer(kind=LONG)                                  :: NsFft
     type(type_Interferogram)                            :: Interf
     type(type_Interferogram)                            :: Interf_dphase
     type(type_Spectrum)                                 :: Spectrum_Local
     type(type_Spectrum)                                 :: Spectrum_temp
     real(kind=DOUBLE)       ,dimension(:) ,allocatable  :: phase
     integer(kind=LONG)                                  :: ErrCode
     integer(kind=LONG)                                  :: N_Sample_Max
     real(kind=DOUBLE)                                   :: Time0
     real(kind=DOUBLE)                                   :: Opd0
!
!    Low Resolution spectrum definition
     Spectrum_Local%Type     = Spectrum%Type
     Spectrum_Local%dWn      = Spectrum%dWn
     Spectrum_Local%Ns_First = idnint( WnS / Spectrum_Local%dWn ) + 1
     Spectrum_Local%Ns_Last  = idnint( WnE / Spectrum_Local%dWn ) + 1
     Spectrum_Local%Wn_First = dble(Spectrum_Local%Ns_First-1) &
                             * Spectrum_Local%dWn
     Spectrum_Local%Wn_Last  = dble(Spectrum_Local%Ns_Last-1)  &
                             * Spectrum_Local%dWn
     Spectrum_Local%N_Sample = Spectrum_Local%Ns_Last    &
                             - Spectrum_Local%Ns_First + 1
     call alloc_Spectrum( Spectrum_Local )
     call Spectrum_Basis( Spectrum_Local )
     NsDel = Spectrum_Local%Ns_First - Spectrum%Ns_First
     if( NsDel < 0 .or.                                    &
         NsDel+Spectrum_Local%N_Sample > Spectrum%N_Sample ) then
        write(*,*) 'Incompatible spectral limits',NsDel
        write(*,*) 'spectrum_os_dephasing Fatal Error'
        call exit(1)
     end if
     write(*,*) 'NsDel',NsDel
     Spectrum_Local%Real_Part(1:Spectrum_Local%N_Sample) =            &
              Spectrum%Real_Part(NsDel+1:NsDel+Spectrum_Local%N_Sample)
!
     if( NsFft_in == 0 ) then
!
!       Compute FFT size
        N_Sample_Max = 2 * (Spectrum_Local%Ns_Last*1.2)
        call tabule_NsFft( N_Sample_Max,&
                           NsFft, ios   )
        if( ios /= 0 ) then
           write(*,*) 'spectrum_os_dephasing Fatal Error'
           call exit(1)
        end if
     else
        NsFft = NsFft_in
        write(*,*) ' OverSampling NsFft ',NsFft
     end if
     Spectrum_Local%WnMax    = dble(int(NsFft/2)) * Spectrum%dWn
     write(*,*) 'Spectrum_Local%N_Sample',Spectrum_Local%N_Sample
     write(*,*) 'Spectrum_Local%WnMax   ',Spectrum_Local%WnMax
!
!    Interferogram computation
     Interf%Type     = 'R'
     Interf%dOpd     = 1.d+00 / (2.d+00*Spectrum_Local%WnMax)
     Interf%dTime    = 1.d+00 
     Interf%N_Sample = NsFft + 1
     call alloc_Interferogram( Interf )
     call Interferogram_Basis( Interf )
     Time0 = Interf%Time(int(Interf%N_Sample/2)+1)
     Opd0  = Interf%Opd(int(Interf%N_Sample/2)+1)
     Interf%Time(1:Interf%N_Sample) = &
              Interf%Time(1:Interf%N_Sample) - Time0
     Interf%Opd(1:Interf%N_Sample) = &
              Interf%Opd(1:Interf%N_Sample) - Opd0
     write(*,*) 'Interf%dOpd         ',Interf%dOpd
     write(*,*) 'Interf%Opd(1)       ',Interf%Opd(1)
     write(*,*) 'Interf%Opd(N_Sample)',Interf%Opd(Interf%N_Sample)
     call sptoif( Spectrum_Local, Interf )
!
!    High Resolution spectrum definition
     Spectrum_os%Type     = Spectrum%Type
     Spectrum_os%dWn      = Spectrum%dWn / dble(OSFactor)
     Spectrum_os%Ns_First = idnint( WnS / Spectrum_os%dWn ) + 1
     Spectrum_os%Ns_Last  = idnint( WnE / Spectrum_os%dWn ) + 1
     Spectrum_os%Wn_First = dble( Spectrum_os%Ns_First-1) * Spectrum_os%dWn
     Spectrum_os%Wn_Last  = dble( Spectrum_os%Ns_Last-1)  * Spectrum_os%dWn
     Spectrum_os%N_Sample = Spectrum_os%Ns_Last - Spectrum_os%Ns_First + 1
     Spectrum_os%WnMax    = dble(int(NsFft/2)) * Spectrum%dWn
     call alloc_Spectrum( Spectrum_os )
     call Spectrum_Basis( Spectrum_os )
!
     Interf_dphase%Type     = 'C'
     Interf_dphase%dOpd     = Interf%dOpd
     Interf_dphase%N_Sample = Interf%N_Sample
     call alloc_Interferogram( Interf_dphase )
     allocate( phase(Interf%N_Sample), stat=ErrCode )
     if( ErrCode /= 0 ) then
        write(*,*) 'spectrum_dephasing allocation Error'
        call exit(1)
     end if
!
!    temporary spectrum definition
     Spectrum_temp%Type     = Spectrum_Local%Type
     Spectrum_temp%dWn      = Spectrum_Local%dWn
     Spectrum_temp%N_Sample = Spectrum_Local%N_Sample
     Spectrum_temp%Ns_First = Spectrum_Local%Ns_First
     Spectrum_temp%Ns_Last  = Spectrum_Local%Ns_Last
     call alloc_Spectrum( Spectrum_temp )
     call Spectrum_Basis( Spectrum_temp )
     do Ns = 1, OSFactor
        phase(1:Interf%N_Sample) = twoPi * Spectrum%dWn        &
                                 * dble(Ns-1)/dble(OSFactor)   &
                                 * Interf%Opd(1:Interf%N_Sample)
        Interf_dphase%Complex(1:Interf_dphase%N_Sample) = &
                  Interf%Real_Part(1:Interf%N_Sample)     &
                * dcmplx( dcos(phase(1:Interf%N_Sample)), &
                         -dsin(phase(1:Interf%N_Sample))  )
        call iftosp( Interf_dphase, Spectrum_temp )
!
!       spectrum samples transfert
        if( Ns == 1 ) then
           Spectrum_os%Real_Part(Ns:Spectrum_os%N_Sample:OSFactor) = &
                     Spectrum_temp%Real_Part(1:Spectrum_temp%N_Sample)
        else
           Spectrum_os%Real_Part(Ns:Spectrum_os%N_Sample:OSFactor) =   &
                     Spectrum_temp%Real_Part(1:Spectrum_temp%N_Sample-1)
        end if
        write(*,*) 'Ns :',Ns
        write(*,'(3f15.10)') &
                      Spectrum_temp%Real_Part(1:3)
        if( Ns == OSFactor ) then
        write(*,'(31f15.10)') &
                      Spectrum_os%Real_Part(1:31)
        end if
     end do
!
!    Deallocation
     deallocate( phase )
     call dalloc_Spectrum( Spectrum_Local )
     call dalloc_Spectrum( Spectrum_temp )
     call dalloc_Interferogram( Interf_dphase )
     call dalloc_Interferogram( Interf )
!
     return
   end subroutine spectrum_os_dephasing
!
!

!
!
!> spectrum_os_spline -- Public
!!
!! * Purpose
!!
!!     Low-resolution spectrum oversampling by spline interpolation
!!
!! * Description
!! 
!!     This subroutine allows to oversample low-resolution spectrum by spline interpolation.
!!     To oversample by a factor OSFactor the spectrum Spectrum, that has original sampling 
!!     of Delta_nu, the algorithm starts by calculating the oversampled basis separated of 
!!     (Delta_nu/OSFactor). Then a spline interpolation gives the oversampled spectrum.
!!
!! * Inputs
!!
!!     - WnS      : first wavenumber
!!     - WnE      : last wavenumber
!!     - OSFactor : oversampling factor
!!     - Spectrum : type_Spectrum / type for declaration and allocation of spectrum 
!! 
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Spectrum_os : type_Spectrum / type for declaration and allocation of spectrum
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.8.12.3
!!

   subroutine spectrum_os_spline( WnS, WnE,   &
                                  OSFactor,   &
                                  Spectrum,   &
                                  Spectrum_os )
     implicit none
     real(kind=DOUBLE)       ,intent(in)                 :: WnS
     real(kind=DOUBLE)       ,intent(in)                 :: WnE
     integer(kind=LONG)      ,intent(in)                 :: OSFactor
     type(type_Spectrum)     ,intent(in)                 :: Spectrum
     type(type_Spectrum)     ,intent(out)                :: Spectrum_os
     real(kind=DOUBLE)       ,dimension(:) ,allocatable  :: Real_Part
     real(kind=DOUBLE)       ,dimension(:) ,allocatable  :: Imag_Part
     real(kind=DOUBLE)       ,dimension(:) ,allocatable  :: Real_Part_hr
     real(kind=DOUBLE)       ,dimension(:) ,allocatable  :: Imag_Part_hr
     integer(kind=LONG)                                  :: ErrCode
     integer(kind=LONG)                                  :: ioalloc
!
!    High Resolution spectrum definition
     Spectrum_os%Type     = Spectrum%Type
     Spectrum_os%dWn      = Spectrum%dWn / dble(OSFactor)
     Spectrum_os%Ns_First = idnint( WnS / Spectrum_os%dWn ) + 1
     Spectrum_os%Ns_Last  = idnint( WnE / Spectrum_os%dWn ) + 1
     Spectrum_os%Wn_First = dble( Spectrum_os%Ns_First-1) * Spectrum_os%dWn
     Spectrum_os%Wn_Last  = dble( Spectrum_os%Ns_Last-1)  * Spectrum_os%dWn
     Spectrum_os%N_Sample = Spectrum_os%Ns_Last - Spectrum_os%Ns_First + 1
     Spectrum_os%WnMax    = Spectrum%WnMax
     call alloc_Spectrum( Spectrum_os )
     call Spectrum_Basis( Spectrum_os )
!
!    spline interpolation
     if(      Spectrum%Type == 'C'  ) then
        ioalloc = 0
        allocate( Real_Part(Spectrum%N_Sample), stat=ErrCode )
        if( ErrCode /= 0 ) ioalloc = ioalloc + 1
        allocate( Imag_Part(Spectrum%N_Sample), stat=ErrCode )
        if( ErrCode /= 0 ) ioalloc = ioalloc + 1
        allocate( Real_Part_hr(Spectrum_os%N_Sample), stat=ErrCode )
        if( ErrCode /= 0 ) ioalloc = ioalloc + 1
        allocate( Imag_Part_hr(Spectrum_os%N_Sample), stat=ErrCode )
        if( ErrCode /= 0 ) ioalloc = ioalloc + 1
        if( ioalloc /= 0 ) then
           write(*,*) 'spectrum_os_spline allocation Error'
           call exit(1)
        end if
        Real_Part(1:Spectrum%N_Sample) = &
            dreal( Spectrum%Complex(1:Spectrum%N_Sample) )
        Imag_Part(1:Spectrum%N_Sample) = &
            dimag( Spectrum%Complex(1:Spectrum%N_Sample) )
        call intspline( Spectrum%N_Sample,    &
                        Spectrum%Wn,          &
                        Real_Part,            &
                        Spectrum_os%N_Sample, &
                        Spectrum_os%Wn,       &
                        Real_Part_hr          )
        call intspline( Spectrum%N_Sample,    &
                        Spectrum%Wn,          &
                        Imag_Part,            &
                        Spectrum_os%N_Sample, &
                        Spectrum_os%Wn,       &
                        Imag_Part_hr          )
        Spectrum_os%Complex(1:Spectrum_os%N_Sample) =      &
              dcmplx( Real_Part_hr(1:Spectrum_os%N_Sample),&
                      Imag_Part_hr(1:Spectrum_os%N_Sample) )
        deallocate( Real_Part )
        deallocate( Imag_Part )
        deallocate( Real_Part_hr )
        deallocate( Imag_Part_hr )
     else if( Spectrum%Type == 'RI' ) then
        call intspline( Spectrum%N_Sample,    &
                        Spectrum%Wn,          &
                        Spectrum%Real_Part,   &
                        Spectrum_os%N_Sample, &
                        Spectrum_os%Wn,       &
                        Spectrum_os%Real_Part )
        call intspline( Spectrum%N_Sample,    &
                        Spectrum%Wn,          &
                        Spectrum%Imag_Part,   &
                        Spectrum_os%N_Sample, &
                        Spectrum_os%Wn,       &
                        Spectrum_os%Imag_Part )
     else if( Spectrum%Type == 'MA' ) then
        call intspline( Spectrum%N_Sample,    &
                        Spectrum%Wn,          &
                        Spectrum%Modulus,     &
                        Spectrum_os%N_Sample, &
                        Spectrum_os%Wn,       &
                        Spectrum_os%Modulus   )
        call intspline( Spectrum%N_Sample,    &
                        Spectrum%Wn,          &
                        Spectrum%Argument,    &
                        Spectrum_os%N_Sample, &
                        Spectrum_os%Wn,       &
                        Spectrum_os%Argument  )
     else if( Spectrum%Type == 'R'  ) then
        call intspline( Spectrum%N_Sample,    &
                        Spectrum%Wn,          &
                        Spectrum%Real_Part,   &
                        Spectrum_os%N_Sample, &
                        Spectrum_os%Wn,       &
                        Spectrum_os%Real_Part )
     else if( Spectrum%Type == 'I'  ) then
        call intspline( Spectrum%N_Sample,    &
                        Spectrum%Wn,          &
                        Spectrum%Imag_Part,   &
                        Spectrum_os%N_Sample, &
                        Spectrum_os%Wn,       &
                        Spectrum_os%Imag_Part )
     else if( Spectrum%Type == 'M'  ) then
        call intspline( Spectrum%N_Sample,    &
                        Spectrum%Wn,          &
                        Spectrum%Modulus,     &
                        Spectrum_os%N_Sample, &
                        Spectrum_os%Wn,       &
                        Spectrum_os%Modulus   )
     else if( Spectrum%Type == 'A'  ) then
        call intspline( Spectrum%N_Sample,    &
                        Spectrum%Wn,          &
                        Spectrum%Argument,    &
                        Spectrum_os%N_Sample, &
                        Spectrum_os%Wn,       &
                        Spectrum_os%Argument  )
     else
        write(*,*) 'Spectrum%Type Error',Spectrum%Type
        write(*,*) 'spectrum_os_spline Fatal Error'
        call exit(1)
     end if
!
     return
   end subroutine spectrum_os_spline
!
!
end module spectrum_oversamp_module
