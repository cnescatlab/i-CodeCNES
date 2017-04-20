!!#  iasitocris_module.f90 --
!!# 
!!#            Project: SPS_GENERIC
!!#            Authors: NOVELTIS/B.TOURNIER
!!#               Date: september 2012
!!#            Version: $Revision: $
!!#  Last modification: $Date: $
!!#
!!#  Language:  F90
!!#  Standards: Noveltis
!!#
!!# --
!!#
!!
!> spectrum conversion IASI to CRIS -- Module
!!
!! * Purpose
!!
!!    Module for spectrum conversion IASI to CRIS
!
!
module iasitocris_module
   use precision_type
   use error_type
   use constantes_type
   use general_type
   use interferogram_type
   use spectrum_type
   use math_module
   use fft_module
   use interf_oversamp_module
   use spectrum_oversamp_module
   use spectrum_compress_module
!
   implicit none
!
!
   public :: iasitocris,      &
             iasitocris_sic,  &
             readsic_cris_asc,&
             readsic_iasi_asc,&
             writesic_cris_asc

!
   contains
!
!
   subroutine iasitocris( OSFactor, N_Lagrange,        &
                          NsFft_iasi, NsFft_cris,      &
                          Cnv_Fct_iasi, Cnv_Fct_cris,  &
                          Spectrum_iasi, Spectrum_cris )
!
     integer(kind=LONG)       ,intent(in)               :: OSFactor
     integer(kind=LONG)       ,intent(in)               :: N_Lagrange
     integer(kind=LONG)       ,intent(in)               :: NsFft_iasi
     integer(kind=LONG)       ,intent(in)               :: NsFft_cris
     type(type_Cnv_Fct)       ,intent(in)               :: Cnv_Fct_iasi
     type(type_Cnv_Fct)       ,intent(in)               :: Cnv_Fct_cris
     type(type_Spectrum)      ,intent(in)               :: Spectrum_iasi
     type(type_Spectrum)      ,intent(inout)            :: Spectrum_cris
     type(type_Interferogram)                           :: Interf_iasi
     type(type_Interferogram)                           :: Interf_cris
     type(type_Interferogram)                           :: Interf_os
     real(kind=DOUBLE)        ,dimension(:),allocatable :: Fct_TF
     real(kind=DOUBLE)                                  :: Opd0
!
!    CRIS Interferogram initialisation
     Interf_cris%Type           = 'R'
     Interf_cris%N_Sample       = NsFft_cris + 1
     Interf_cris%N_Top          = 0
     Interf_cris%OpdMax         = 1_DOUBLE / ( 2_DOUBLE * Spectrum_cris%dWn )
     Interf_cris%TimeMax        = Interf_cris%OpdMax
     Interf_cris%dOpd           = 2_DOUBLE * Interf_cris%OpdMax &
                                / dble( Interf_cris%N_Sample-1 )
     Interf_cris%dTime          = Interf_cris%dOpd
     Interf_cris%FieldMeanAngle = 0_DOUBLE
!
     call alloc_Interferogram( Interf_cris )
     call Interferogram_Basis( Interf_cris )
     Opd0 = Interf_cris%Opd(int(Interf_cris%N_Sample/2)+1)
     Interf_cris%Opd(1:Interf_cris%N_Sample) =                      &
                       Interf_cris%Opd(1:Interf_cris%N_Sample) - Opd0
!
!    IASI Interferogram initialisation
     Interf_iasi%Type           = 'R'
     Interf_iasi%N_Sample       = NsFft_iasi + 1
     Interf_iasi%N_Top          = 0
     Interf_iasi%OpdMax         = 1_DOUBLE / ( 2_DOUBLE * Spectrum_iasi%dWn )
     Interf_iasi%TimeMax        = Interf_iasi%OpdMax
     Interf_iasi%dOpd           = 2_DOUBLE * Interf_iasi%OpdMax &
                                / dble( Interf_iasi%N_Sample-1 )
     Interf_iasi%dTime          = Interf_iasi%dOpd
     Interf_iasi%FieldMeanAngle = 0_DOUBLE
!
     call alloc_Interferogram( Interf_iasi )
     call Interferogram_Basis( Interf_iasi )
     Opd0 = Interf_iasi%Opd(int(Interf_iasi%N_Sample/2)+1)
     Interf_iasi%Opd(1:Interf_iasi%N_Sample) =                      &
                       Interf_iasi%Opd(1:Interf_iasi%N_Sample) - Opd0
!
!    IASI Interferogram computation
     call sptoif( Spectrum_iasi, Interf_iasi )
!
!    IASI apodisation function Resampling
     allocate( Fct_TF(Interf_iasi%N_Sample) )
!
     call intspline_open( Cnv_Fct_iasi%Ns_Opd, &
                          Cnv_Fct_iasi%Opd,    &
                          Cnv_Fct_iasi%Fct_TF, &
                          Interf_iasi%N_Sample,&
                          Interf_iasi%Opd,     &
                          Fct_TF )
!
!    Removing IASI apodisation function
     Interf_iasi%Real_Part(1:Interf_iasi%N_Sample) =                &
                       Interf_iasi%Real_Part(1:Interf_iasi%N_Sample)&
                     / Fct_TF(1:Interf_iasi%N_Sample)
!
     deallocate( Fct_TF )
!
!    CRIS apodisation function Resampling
     allocate( Fct_TF(Interf_iasi%N_Sample) )
!
     call intspline_open( Cnv_Fct_cris%Ns_Opd, &
                          Cnv_Fct_cris%Opd,    &
                          Cnv_Fct_cris%Fct_TF, &
                          Interf_iasi%N_Sample,&
                          Interf_iasi%Opd,     &
                          Fct_TF )
!
!    Application CRIS apodisation function
     Interf_iasi%Real_Part(1:Interf_iasi%N_Sample) =                &
                       Interf_iasi%Real_Part(1:Interf_iasi%N_Sample)&
                     * Fct_TF(1:Interf_iasi%N_Sample)
!
     deallocate( Fct_TF )
!
     if( OSFactor > 1 ) then
!
!       CRIS Interferogram oversampling
        call interf_os_zero_padding( OSFactor,   &
                                     Interf_iasi,&
                                     Interf_os   )
        Opd0 = Interf_os%Opd(int(Interf_os%N_Sample/2)+1)
        Interf_os%Opd(1:Interf_os%N_Sample) =                    &
                        Interf_os%Opd(1:Interf_os%N_Sample) - Opd0
!
!       CRIS Interferogram Re-sampling
        if( N_Lagrange /= 0 ) then
           call interf_resamp_lagrange( Interf_os, N_Lagrange, Interf_cris )
        else
           call interf_resamp_spline( Interf_os, Interf_cris )
        end if
        call dalloc_Interferogram( Interf_os )
        call dalloc_Interferogram( Interf_iasi )
!
     else
!
!       CRIS Interferogram Re-sampling
        if( N_Lagrange /= 0 ) then
           call interf_resamp_lagrange( Interf_iasi, N_Lagrange, Interf_cris )
        else
           call interf_resamp_spline( Interf_iasi, Interf_cris )
        end if
        call dalloc_Interferogram( Interf_iasi )
!
     end if
!
!    CRIS Spectrum computation
     call iftosp_open( Interf_cris, Spectrum_cris )
!
!    deallocation
     call dalloc_Interferogram( Interf_cris )
!
     return
   end subroutine iasitocris
!
!
   subroutine iasitocris_sic( NsFft_iasi, NsFft_cris,      &
                              Cnv_Fct_iasi, Cnv_Fct_cris,  &
                              Spectrum_iasi, Spectrum_cris )
!
     integer(kind=LONG)       ,intent(in)               :: NsFft_iasi
     integer(kind=LONG)       ,intent(in)               :: NsFft_cris
     type(type_Cnv_Fct)       ,intent(in)               :: Cnv_Fct_iasi
     type(type_Cnv_Fct)       ,intent(in)               :: Cnv_Fct_cris
     type(type_Spectrum)      ,intent(in)               :: Spectrum_iasi
     type(type_Spectrum)      ,intent(inout)            :: Spectrum_cris
     type(type_Interferogram)                           :: Interf_iasi
     type(type_Interferogram)                           :: Interf_cris
     real(kind=DOUBLE)        ,dimension(:),allocatable :: Fct_TF
     real(kind=DOUBLE)                                  :: Opd0
!
!    CRIS Interferogram initialisation
     Interf_cris%Type           = 'R'
     Interf_cris%N_Sample       = NsFft_cris + 1
     Interf_cris%N_Top          = 0
     Interf_cris%OpdMax         = 1_DOUBLE / ( 2_DOUBLE * Spectrum_cris%dWn )
     Interf_cris%TimeMax        = Interf_cris%OpdMax
     Interf_cris%dOpd           = 2_DOUBLE * Interf_cris%OpdMax &
                                / dble( Interf_cris%N_Sample-1 )
     Interf_cris%dTime          = Interf_cris%dOpd
     Interf_cris%FieldMeanAngle = 0_DOUBLE
!
     call alloc_Interferogram( Interf_cris )
     call Interferogram_Basis( Interf_cris )
!
!    OPD centerring
     Opd0 = Interf_cris%Opd(int(Interf_cris%N_Sample/2)+1)
     Interf_cris%Opd(1:Interf_cris%N_Sample) =                      &
                       Interf_cris%Opd(1:Interf_cris%N_Sample) - Opd0
!
!    IASI Interferogram initialisation
     Interf_iasi%Type           = 'R'
     Interf_iasi%N_Sample       = NsFft_iasi + 1
     Interf_iasi%N_Top          = 0
     Interf_iasi%OpdMax         = 1_DOUBLE / ( 2_DOUBLE * Spectrum_iasi%dWn )
     Interf_iasi%TimeMax        = Interf_iasi%OpdMax
     Interf_iasi%dOpd           = 2_DOUBLE * Interf_iasi%OpdMax &
                                / dble( Interf_iasi%N_Sample-1 )
     Interf_iasi%dTime          = Interf_iasi%dOpd
     Interf_iasi%FieldMeanAngle = 0_DOUBLE
!
     call alloc_Interferogram( Interf_iasi )
     call Interferogram_Basis( Interf_iasi )
!
!    OPD centerring
     Opd0 = Interf_iasi%Opd(int(Interf_iasi%N_Sample/2)+1)
     Interf_iasi%Opd(1:Interf_iasi%N_Sample) =                      &
                       Interf_iasi%Opd(1:Interf_iasi%N_Sample) - Opd0
!
!    IASI Interferogram computation
     call sptoif( Spectrum_iasi, Interf_iasi )
!
!    IASI apodisation function resampling on interferogram basis
     allocate( Fct_TF(Interf_iasi%N_Sample) )
!
     call intspline_open( Cnv_Fct_iasi%Ns_Opd, &
                          Cnv_Fct_iasi%Opd,    &
                          Cnv_Fct_iasi%Fct_TF, &
                          Interf_iasi%N_Sample,&
                          Interf_iasi%Opd,     &
                          Fct_TF )
!
!    IASI apodisation function removing
     Interf_iasi%Real_Part(1:Interf_iasi%N_Sample) =                &
                       Interf_iasi%Real_Part(1:Interf_iasi%N_Sample)&
                     / Fct_TF(1:Interf_iasi%N_Sample)
!
     deallocate( Fct_TF )
!
!    CRIS apodisation function resampling on interferogram basis
     allocate( Fct_TF(Interf_iasi%N_Sample) )
!
     call intspline_open( Cnv_Fct_cris%Ns_Opd, &
                          Cnv_Fct_cris%Opd,    &
                          Cnv_Fct_cris%Fct_TF, &
                          Interf_iasi%N_Sample,&
                          Interf_iasi%Opd,     &
                          Fct_TF )
!
!    CRIS apodisation function application
     Interf_iasi%Real_Part(1:Interf_iasi%N_Sample) =                &
                       Interf_iasi%Real_Part(1:Interf_iasi%N_Sample)&
                     * Fct_TF(1:Interf_iasi%N_Sample)
!
     deallocate( Fct_TF )
!
!    CRIS Interferogram Re-sampling on CRIS FFT basis
     call interf_resamp_spline( Interf_iasi, Interf_cris )
     call dalloc_Interferogram( Interf_iasi )
!
!    CRIS Spectrum computation
     call iftosp_open( Interf_cris, Spectrum_cris )
!
!    deallocation
     call dalloc_Interferogram( Interf_cris )
!
     return
   end subroutine iasitocris_sic
!
!
   subroutine readsic_cris_asc( File_Spectrum_cris, &
                                SB, Spectrum_cris,  &
                                NbPix, ErrCode      )
     character(len=*)        ,intent(in)               :: File_Spectrum_cris
     integer(kind=LONG)      ,intent(in)               :: SB
     type(type_Spectrum)     ,intent(inout)            :: Spectrum_cris
     integer(kind=LONG)      ,intent(inout)            :: NbPix
     integer(kind=LONG)      ,intent(inout)            :: ErrCode
     integer(kind=LONG)                                :: iFile
     integer(kind=LONG)                                :: iPos
     integer(kind=LONG)                                :: Ns
     real(kind=DOUBLE)                                 :: Dummy
!
!    
     iFile = 10
     iPos  = 1
     open(iFile, file=File_Spectrum_cris, err=999)
     iPos  = 2
     read(iFile,*,err=999)
     read(iFile,*,err=999)
     read(iFile,*,err=999)
     read(iFile,*,err=999)
     iPos  = 3
     if( SB == 1 ) then
        do Ns = 1, Spectrum_cris%N_Sample
           read(iFile,*,err=999) Spectrum_cris%Wn(Ns), &
                                 Dummy,                &
                                 Spectrum_cris%Real_Part(Ns),&
                                 Dummy,                &
                                 Dummy,                &
                                 Dummy,                &
                                 NbPix
           if( Spectrum_cris%Real_Part(Ns) <= 0.0d+00 ) ErrCode = 1
        end do
     else if( SB == 2 .or. SB == 3 ) then
        Ns = 1
        read(iFile,*,err=999) Spectrum_cris%Wn(Ns), &
                              Dummy,                &
                              Spectrum_cris%Real_Part(Ns),&
                              Dummy,                &
                              Dummy,                &
                              Dummy,                &
                              NbPix
        if( Spectrum_cris%Real_Part(Ns) <= 0.0d+00 ) ErrCode = 1
        do while ( Spectrum_cris%Wn(Ns) .lt. Spectrum_cris%Wn_First/100.d+0 )
           read(iFile,*,err=999) Spectrum_cris%Wn(Ns), &
                                 Dummy,                &
                                 Spectrum_cris%Real_Part(Ns),&
                                 Dummy,                &
                                 Dummy,                &
                                 Dummy,                &
                                 NbPix
           if( Spectrum_cris%Real_Part(Ns) <= 0.0d+00 ) ErrCode = 1
        end do
        do Ns = 2, Spectrum_cris%N_Sample
           read(iFile,*,err=999) Spectrum_cris%Wn(Ns), &
                                 Dummy,                &
                                 Spectrum_cris%Real_Part(Ns),&
                                 Dummy,                &
                                 Dummy,                &
                                 Dummy,                &
                                 NbPix
           if( Spectrum_cris%Real_Part(Ns) <= 0.0d+00 ) ErrCode = 1
        end do
     else
        write(*,*) 'Spectral Band Error',SB
        go to 999
     end if
     close( iFile )
!
     Spectrum_cris%filename = File_Spectrum_cris
!
!    Units conversion cm-1 ==> m-1
     Spectrum_cris%Wn(1:Spectrum_cris%N_Sample) = &
            Spectrum_cris%Wn(1:Spectrum_cris%N_Sample)*100.d+00
!
!    Units conversion mW/m2/sr/cm-1 ==> W/m2/sr/m-1
     Spectrum_cris%Real_Part(1:Spectrum_cris%N_Sample) = &
            Spectrum_cris%Real_Part(1:Spectrum_cris%N_Sample)&
                                           /1000.d+00/100.d+00
!     write(0,*) 'Wn ',Spectrum_cris%Wn(1),&
!                      Spectrum_cris%Wn(Spectrum_cris%N_Sample)
     return
 999 write(*,*) 'readsic_cris_asc fatal error ',iPos
     call exit(1)
   end subroutine readsic_cris_asc
!
!
   subroutine readsic_iasi_asc( File_Spectrum_iasi,&
                                SB, Spectrum_iasi, &
                                NbPix, ErrCode     )
     character(len=*)        ,intent(in)               :: File_Spectrum_iasi
     integer(kind=LONG)      ,intent(in)               :: SB
     type(type_Spectrum)     ,intent(inout)            :: Spectrum_iasi
     integer(kind=LONG)      ,intent(inout)            :: NbPix
     integer(kind=LONG)      ,intent(inout)            :: ErrCode
     integer(kind=LONG)                                :: iFile
     integer(kind=LONG)                                :: iPos
     integer(kind=LONG)                                :: Ns
     real(kind=DOUBLE)                                 :: Dummy
!
!    
     ErrCode = 0
     iFile = 10
     iPos  = 1
     open(iFile, file=File_Spectrum_iasi, err=999)
     iPos  = 2
     read(iFile,*,err=999)
     read(iFile,*,err=999)
     read(iFile,*,err=999)
     read(iFile,*,err=999)
     iPos  = 3
     if( SB == 1 ) then
        do Ns = 1, Spectrum_iasi%N_Sample
           read(iFile,*,err=999) Spectrum_iasi%Wn(Ns), &
                                 Dummy,                &
                                 Spectrum_iasi%Real_Part(Ns),&
                                 Dummy,                &
                                 Dummy,                &
                                 Dummy,                &
                                 NbPix
           if( Spectrum_iasi%Real_Part(Ns) <= 0.0d+00 ) ErrCode = 1
        end do
     else if( SB == 2 .or. SB == 3 ) then
        Ns = 1
        read(iFile,*,err=999) Spectrum_iasi%Wn(Ns), &
                              Dummy,                &
                              Spectrum_iasi%Real_Part(Ns),&
                              Dummy,                &
                              Dummy,                &
                              Dummy,                &
                              NbPix
        if( Spectrum_iasi%Real_Part(Ns) <= 0.0d+00 ) ErrCode = 1
        do while ( Spectrum_iasi%Wn(Ns) .lt. Spectrum_iasi%Wn_First/100.d+0 )
           read(iFile,*,err=999) Spectrum_iasi%Wn(Ns), &
                                 Dummy,                &
                                 Spectrum_iasi%Real_Part(Ns),&
                                 Dummy,                &
                                 Dummy,                &
                                 Dummy,                &
                                 NbPix
           if( Spectrum_iasi%Real_Part(Ns) <= 0.0d+00 ) ErrCode = 1
        end do
        do Ns = 2, Spectrum_iasi%N_Sample
           read(iFile,*,err=999) Spectrum_iasi%Wn(Ns), &
                                 Dummy,                &
                                 Spectrum_iasi%Real_Part(Ns),&
                                 Dummy,                &
                                 Dummy,                &
                                 Dummy,                &
                                 NbPix
           if( Spectrum_iasi%Real_Part(Ns) <= 0.0d+00 ) ErrCode = 1
        end do
     else
        write(*,*) 'Spectral Band Error',SB
        go to 999
     end if
     close( iFile )
!
     Spectrum_iasi%filename = File_Spectrum_iasi
!
!    Units conversion cm-1 ==> m-1
     Spectrum_iasi%Wn(1:Spectrum_iasi%N_Sample) = &
            Spectrum_iasi%Wn(1:Spectrum_iasi%N_Sample)*100.d+00
!
!    Units conversion mW/m2/sr/cm-1 ==> W/m2/sr/m-1
     Spectrum_iasi%Real_Part(1:Spectrum_iasi%N_Sample) = &
            Spectrum_iasi%Real_Part(1:Spectrum_iasi%N_Sample)&
                                           /1000.d+00/100.d+00
!     write(0,*) 'Wn ',Spectrum_iasi%Wn(1),&
!                      Spectrum_iasi%Wn(Spectrum_iasi%N_Sample)
     return
 999 write(*,*) 'readsic_iasi_asc fatal error ',iPos
     call exit(1)
   end subroutine readsic_iasi_asc
!
!
   subroutine writesic_cris_asc( Nband, SB,         &
                                 File_Spectrum_cris,&
                                 Spectrum_cris,     &
                                 NbPix              )
!
     integer(kind=LONG)      ,intent(in)               :: Nband
     integer(kind=LONG)      ,intent(in)               :: SB
     character(len=*)        ,intent(in)               :: File_Spectrum_cris
     type(type_Spectrum)     ,intent(in)               :: Spectrum_cris
     integer(kind=LONG)      ,intent(in)               :: NbPix
     integer(kind=LONG)                                :: iFile
     integer(kind=LONG)                                :: iPos
     integer(kind=LONG)                                :: Ns
     real(kind=DOUBLE)                                 :: Dummy
!
!    Wave number conversion m-1 => cm-1
!    Radiance conversion W/(m2.str.m-1) => mW/(m2.str.cm-1)
     iFile = 10
     Dummy = 0.d+00
     if( SB == 1 ) then
        iPos  = 1
        open(iFile, file=File_Spectrum_cris, err=999)
        iPos  = 2
        write(iFile,*,err=999)
        write(iFile,*,err=999)
        write(iFile,*,err=999)
        write(iFile,*,err=999)
     end if
     iPos  = 2 + SB
     do Ns = 1, Spectrum_cris%N_Sample
        write(iFile,*,err=999) Spectrum_cris%Wn(Ns)/100., &
                               Dummy,                     &
                               Spectrum_cris%Real_Part(Ns)*1.d+5,&
                               Dummy,                     &
                               Dummy,                     &
                               Dummy,                     &
                               NbPix
     end do
     if( SB == Nband ) then
        close( iFile )
     end if
     return
 999 write(*,*) 'writesic_cris_asc fatal error ',iPos
     close( iFile )
     call exit(1)
   end subroutine writesic_cris_asc
!
!
end module iasitocris_module
