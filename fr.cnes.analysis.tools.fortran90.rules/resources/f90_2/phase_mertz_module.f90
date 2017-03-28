!!#   phase_mertz_module.f90 --
!!# 
!!#            Project: SPS_GENERIC
!!#            Authors: NOVELTIS/B.TOURNIER
!!#               Date: may 2011
!!#            Version: $Revision: 1.2 $
!!#  Last modification: $Date: 2011-11-02 14:57:50 $
!!#
!!#  Language:  F90
!!#  Standards: Noveltis
!!#
!!# --
!!#
!!
!>  interferogram Mertz phase correction -- Module
!!
!! * Purpose
!!
!!   Module for Mertz phase correction
!!
!! * Description
!!      

module phase_mertz_module
   use precision_type
   use error_type
   use constantes_type
   use phase_mertz_type
   use interf_param_type
   use resampling_param_type
   use interferogram_type
   use spectrum_type
   use math_module
   use fft_module
!
   implicit none
!
!
   public :: phase_mertz_init, &
             phase_mertz_calc, &
             spectrum_mertz,   &
             phase_mertz_correction
!
   contains
!
!  

!> phase_mertz_init -- Public
!!
!! * Purpose
!!
!!     Initialisation of the type defining the interferogram phase_mertz parameters.
!!
!! * Description
!! 
!
!
   subroutine phase_mertz_init( File_Phase_Mertz_Param, Phase_Mertz )
   implicit none
     character(len=*)        , intent(in)           :: File_Phase_Mertz_Param
     type(type_Phase_Mertz)  , intent(inout)        :: Phase_Mertz
     integer(kind=LONG)                             :: iFile
     integer(kind=LONG)                             :: iPos
!
     iFile = 10
     iPos  = 1
     write(*,'(a)') File_Phase_Mertz_Param(1:len_trim(File_Phase_Mertz_Param))
     open(unit=iFile, file=File_Phase_Mertz_Param, status='old', err=999)
     iPos  = 2
     Phase_Mertz%filename = File_Phase_Mertz_Param
     read(iFile,'(a)',err=999) Phase_Mertz%Mode_Traitement
     read(iFile,'(a)',err=999) Phase_Mertz%Filter
     iPos  = 3
     read(iFile,*,err=999) Phase_Mertz%Deg_Polynom
     read(iFile,*,err=999) Phase_Mertz%Ratio_bilatere
     read(iFile,*,err=999) Phase_Mertz%NsFft
     read(iFile,*,err=999) Phase_Mertz%SigI
     read(iFile,*,err=999) Phase_Mertz%Wn_First
     read(iFile,*,err=999) Phase_Mertz%Wn_Last
     write(*,*) 'Phase_Mertz%Filter        ',Phase_Mertz%Filter
     write(*,*) 'Phase_Mertz%Deg_Polynom   ',Phase_Mertz%Deg_Polynom
     write(*,*) 'Phase_Mertz%NsFft         ',Phase_Mertz%NsFft
     write(*,*) 'Phase_Mertz%SigI          ',Phase_Mertz%SigI
     write(*,*) 'Phase_Mertz%Ratio_bilatere',Phase_Mertz%Ratio_bilatere
     write(*,*) 'Phase_Mertz%Wn_First      ',Phase_Mertz%Wn_First
     write(*,*) 'Phase_Mertz%Wn_Last       ',Phase_Mertz%Wn_Last
     call alloc_Phase_Mertz( Phase_Mertz )
     close(unit=iFile)
     return
 999 write(*,*) 'phase_Mertz_init Fatal error',iPos
     call exit(1)
   end subroutine phase_mertz_init
!
!

!> phase_mertz_calc -- Public
!!
!! * Purpose
!!
!!     Phase_mertz computation - phase correction of the spectrum
!!
!! * Description
!! 
   subroutine phase_mertz_calc( Interf_Target,&
                                Interf,       &
                                Phase_Mertz   )
   implicit none
     character(len=2)        , intent(in)                    :: Interf_Target
     type(type_Interferogram), intent(in)                    :: Interf
     type(type_Phase_Mertz)  , intent(inout)                 :: Phase_Mertz
     integer(kind=LONG)                                      :: Sens
     integer(kind=LONG)                                      :: NsFftp
     integer(kind=LONG)                                      :: NsDel
     integer(kind=LONG)                                      :: Ns
     real(kind=DOUBLE)                                       :: dWn_Norm
     complex(kind=DOUBLE)    , dimension(:), allocatable     :: Sp_Reduced
     real(kind=DOUBLE)       , dimension(:), allocatable     :: If_Reduced
     real(kind=DOUBLE)       , dimension(:), allocatable     :: Opd_Reduced
     real(kind=DOUBLE)       , dimension(:), allocatable     :: SMod
     real(kind=DOUBLE)       , dimension(:), allocatable     :: SArg
     character(len=6)                                        :: Edge
!
!    allocation
     allocate( Opd_Reduced(Phase_Mertz%NsFft+1) )
     allocate( If_Reduced(Phase_Mertz%NsFft+1) )
     allocate( Sp_Reduced(Phase_Mertz%NsFft+1) )
     allocate( SMod(Phase_Mertz%NsFft+1) )
     allocate( SArg(Phase_Mertz%NsFft+1) )
!
!    Windowing Interferogramme bilatere SigI
!
!    Half Size NsFftp
     NsFftp  = int( Phase_Mertz%NsFft/2 )
!
!    Scaling factors
     Phase_Mertz%dOpd = Interf%dOpd
     Phase_Mertz%dWn  = 1.d+00 / ( Phase_Mertz%dOpd * Phase_Mertz%NsFft )
     dWn_Norm    = 1.d+00 / ( Interf%dOpd * dble(Interf%N_Sample-1) )
!
!    Reduced spectra band limits
     Phase_Mertz%Ns_First = &
               idnint(Phase_Mertz%Wn_First/Phase_Mertz%dWn) + 1
     Phase_Mertz%Ns_Last  = &
               idnint(Phase_Mertz%Wn_Last/Phase_Mertz%dWn) + 1
     Phase_Mertz%N_Sample = Phase_Mertz%Ns_Last    &
                          - Phase_Mertz%Ns_First + 1
     Phase_Mertz%Wn_First = &
               dble( Phase_Mertz%Ns_First-1 ) * Phase_Mertz%dWn
     Phase_Mertz%Wn_Last  = &
               dble( Phase_Mertz%Ns_Last-1 )  * Phase_Mertz%dWn
     write(*,*) 'Phase_Mertz%Ns_First '&
                ,Phase_Mertz%Ns_First
     write(*,*) 'Phase_Mertz%Ns_Last  '&
                ,Phase_Mertz%Ns_Last
     write(*,*) 'Phase_Mertz%N_Sample '&
                ,Phase_Mertz%N_Sample
!
     NsDel = int(Interf%N_Sample/2) - NsFftp
     if( NsDel < 0 ) then
        write(*,*) 'First sample Error',NsDel
        write(*,*) 'phase_mertz_calc Fatal Error'
        call exit(1)
     end if
     if( int(Interf%N_Sample/2) + NsFftp > Interf%N_Sample ) then
        write(*,*) 'Last sample Error',Interf%N_Sample
        write(*,*) 'phase_mertz_calc Fatal Error'
        call exit(1)
     end if
!
!    Reduced interferogram extraction
     If_Reduced(1:Phase_Mertz%NsFft+1) =                       &
             Interf%Real_Part(NsDel+1:NsDel+Phase_Mertz%NsFft+1)
     Opd_Reduced(1:Phase_Mertz%NsFft+1) =                &
             Interf%Opd(NsDel+1:NsDel+Phase_Mertz%NsFft+1)
!
!    Windowing (SigI) Interferogram extremity
     if( Phase_Mertz%SigI /= 0 ) then
        call windowing( Phase_Mertz%NsFft+1,&
                        If_Reduced,         &
                        Phase_Mertz%SigI,   &
                        Phase_Mertz%dOpd    )
     end if
!
!    Numerical apodisation central part
     Edge = 'LEFT'
     if(      Phase_Mertz%Filter == 'COSINE' ) then
        write(*,*) 'Mertz Filter  ',Phase_Mertz%Filter
        call smoothcosine( Phase_Mertz%NsFft+1, Edge,&
                           Opd_Reduced,              &
                           If_Reduced                )
     else if( Phase_Mertz%Filter == 'ERF' ) then
        write(*,*) 'Mertz Filter  ',Phase_Mertz%Filter
        call smootherf( Phase_Mertz%NsFft+1, Edge,&
                        Interf%dOpd,              &
                        Opd_Reduced,              &
                        If_Reduced                )
     else if( Phase_Mertz%Filter == 'LINEAR' ) then
        write(*,*) 'Mertz Filter  ',Phase_Mertz%Filter
        call smoothlinear( Phase_Mertz%NsFft+1, Edge,&
                           Opd_Reduced,              &
                           If_Reduced                )
     else if( Phase_Mertz%Filter == 'NONE' ) then
        write(*,*) 'Mertz Filter  ',Phase_Mertz%Filter
     else
        write(*,*) 'spectrum_mertz Filter Error',Phase_Mertz%Filter
        call exit(1)
     end if
!
     Sens = -1
     call fft_r2c_open( Phase_Mertz%NsFft, Sens, If_Reduced,   &
                        Phase_Mertz%dOpd, dWn_Norm, Sp_Reduced )
!
!    Module and phase extration
     call cplxtomodarg( Sp_Reduced, SMod, SArg, Phase_Mertz%NsFft+1 )
     Phase_Mertz%Sp_Mod(1:Phase_Mertz%N_Sample) =    &
               SMod(NsFftp+1+(Phase_Mertz%Ns_First-1)&
                   :NsFftp+1+(Phase_Mertz%Ns_Last-1) )
     Phase_Mertz%Sp_Arg(1:Phase_Mertz%N_Sample) =    &
               SArg(NsFftp+1+(Phase_Mertz%Ns_First-1)&
                   :NsFftp+1+(Phase_Mertz%Ns_Last-1) )
!
     if( (Interf_Target == 'CS') .and.                 &
         (Phase_Mertz%Mode_Traitement == 'MESUREIASI') ) then
        Phase_Mertz%Sp_Arg(1:Phase_Mertz%N_Sample) = &
        Phase_Mertz%Sp_Arg(1:Phase_Mertz%N_Sample) + Pi
     end if
!
!    Phase regularisation
     call phasemono( Phase_Mertz%Sp_Arg, Phase_Mertz%N_Sample )
     Phase_Mertz%Wn(1:Phase_Mertz%N_Sample) =         &
             Phase_Mertz%dWn                          &
             * (/ (dble(Ns-1),Ns=Phase_Mertz%Ns_First,&
                                Phase_Mertz%Ns_Last) /)
     do Ns = 1, Phase_Mertz%N_Sample
        write(*,'(a,f10.2,2e12.4)') 'Phase_Mertz ',&
                        Phase_Mertz%Wn(Ns),        &
                        Phase_Mertz%Sp_Mod(Ns),    &
                        Phase_Mertz%Sp_Arg(Ns)
     end do
!
!    deallocation
     deallocate( SMod )
     deallocate( SArg )
     deallocate( Opd_Reduced )
     deallocate( If_Reduced )
     deallocate( Sp_Reduced )
!
     return
   end subroutine phase_mertz_calc
!
!
   subroutine spectrum_mertz( Interf_Param,&
                              Resamp_Param,&
                              SB,          &
                              Interf,      &
                              Phase_Mertz, &
                              Spectrum     )
   implicit none
     type(type_Interf_Param)    ,intent(in)                 :: Interf_Param
     type(type_Resampling_Param),intent(in)                 :: Resamp_Param
     integer(kind=LONG)         ,intent(in)                 :: SB
     type(type_Interferogram)   ,intent(in)                 :: Interf
     type(type_Phase_Mertz)     ,intent(in)                 :: Phase_Mertz
     type(type_Spectrum)        ,intent(inout)              :: Spectrum
     integer(kind=LONG)                                     :: NsDel
     integer(kind=LONG)                                     :: NR1
     integer(kind=LONG)                                     :: NR2
     type(type_Interferogram)                               :: Interf_Local
     character(len=6)                                       :: Edge
!
!    Scaling factors
     if( Interf%N_Sample-1 < Resamp_Param%Ns_Fft ) then
        write(*,*) ' spectrum_mertz NsFft N_Sample ERROR'
        call exit(1)
     end if
     Spectrum%Type = 'C'
     Spectrum%dWn   = 1.d+00 / ( Interf%dOpd * dble(Resamp_Param%Ns_Fft) )
     Spectrum%WnMax = 1.d+00 / ( 2.d+00 * Interf%dOpd )
!
!    Spectral limits and spectrum allocation
     Spectrum%Ns_First = idnint( Interf_Param%Wn_Usefull_First(SB) &
                               / Spectrum%dWn ) + 1
     Spectrum%Ns_Last  = idnint( Interf_Param%Wn_Usefull_Last(SB)  &
                               / Spectrum%dWn ) + 1
     Spectrum%N_Sample = Spectrum%Ns_Last - Spectrum%Ns_First + 1
     Spectrum%Wn_First = Spectrum%dWn * dble(Spectrum%Ns_First-1)
     Spectrum%Wn_Last  = Spectrum%dWn * dble(Spectrum%Ns_Last-1)
     write(*,*) 'Spectrum%N_Sample',Spectrum%N_Sample
     write(*,*) 'Spectrum%Ns_First',Spectrum%Ns_First
     write(*,*) 'Spectrum%Ns_Last ',Spectrum%Ns_Last
     write(*,*) 'Spectrum%dWn     ',Spectrum%dWn
     write(*,*) 'Spectrum%WnMax   ',Spectrum%WnMax
     call alloc_Spectrum( Spectrum )
     call Spectrum_Basis( Spectrum )
!
!    Interferogram extraction
     call Interf_Header_Transfer( Interf, Interf_Local )
     if( Interf_Local%Type /= 'R' ) then
        write(*,*) 'spectrum_mertz Interf Type Error ',Interf_Local%Type
        call exit(1)
     end if
     NsDel = int(Interf%N_Sample/2) - int(Resamp_Param%Ns_Fft/2)
     if( NsDel < 0 ) then
        write(*,*) ' spectrum_mertz NsDel Error',NsDel
        call exit(1)
     end if
     Interf_Local%N_Sample = Resamp_Param%Ns_Fft+1
     Interf_Local%Opd(1:Interf_Local%N_Sample) =         &
           Interf%Opd(NsDel+1:NsDel+Interf_Local%N_Sample)
     Interf_Local%Time(1:Interf_Local%N_Sample) =         &
           Interf%Time(NsDel+1:NsDel+Interf_Local%N_Sample)
     Interf_Local%Real_Part(1:Interf_Local%N_Sample) =         &
           Interf%Real_Part(NsDel+1:NsDel+Interf_Local%N_Sample)
     write(*,*) 'Interf_Local%Opd(1)              ',&
                 Interf_Local%Opd(1)
     write(*,*) 'Interf_Local%Opd(Interf%N_Sample)',&
                 Interf_Local%Opd(Interf%N_Sample)
!
!    Interferogram Mertz filtering
     NR1 = int(Interf_Local%N_Sample/2)+1 - int(Phase_Mertz%NsFft/2)
     NR2 = int(Interf_Local%N_Sample/2)+1 + int(Phase_Mertz%NsFft/2)
     write(*,*) 'Interf_Local N_Sample  ',Interf_Local%N_Sample
     write(*,*) 'Phase_Mertz%NsFft',Phase_Mertz%NsFft
     write(*,*) 'NR1,NR2          ',NR1,NR2
     if( (NR1 <= 0) .or. (NR2 > Interf_Local%N_Sample) ) then
        write(*,*) 'spectrum_mertz Reduced Interferogram limits Error '&
                                                                ,NR1,NR2
        call exit(1)
     end if
!
!    Erase left part
     Interf_Local%Real_Part(1:NR1-1) = 0.d+00
!
!    Numerical apodisation central part
     Edge = 'LEFT'
     if(      Phase_Mertz%Filter == 'COSINE' ) then
        write(*,*) 'Mertz Filter  ',Phase_Mertz%Filter
        call smoothcosine( Phase_Mertz%NsFft+1, Edge,      &
                           Interf_Local%Opd(NR1:NR2),      &
                           Interf_Local%Real_Part(NR1:NR2) )
     else if( Phase_Mertz%Filter == 'ERF' ) then
        write(*,*) 'Mertz Filter  ',Phase_Mertz%Filter
        call smootherf( Phase_Mertz%NsFft+1, Edge,      &
                        Interf_Local%dOpd,              &
                        Interf_Local%Opd(NR1:NR2),      &
                        Interf_Local%Real_Part(NR1:NR2) )
     else if( Phase_Mertz%Filter == 'LINEAR' ) then
        write(*,*) 'Mertz Filter  ',Phase_Mertz%Filter
        call smoothlinear( Phase_Mertz%NsFft+1, Edge,      &
                           Interf_Local%Opd(NR1:NR2),      &
                           Interf_Local%Real_Part(NR1:NR2) )
     else if( Phase_Mertz%Filter == 'NONE' ) then
        write(*,*) 'Mertz Filter  ',Phase_Mertz%Filter
     else
        write(*,*) 'spectrum_mertz Filter Error',Phase_Mertz%Filter
        call exit(1)
     end if
     Interf_Local%Real_Part(NR1:NR2) = 2.d+00                        &
                                     * Interf_Local%Real_Part(NR1:NR2)
!
!    Doubling right part
     Interf_Local%Real_Part(NR2+1:Interf_Local%N_Sample) = 2.d+00       &
                    * Interf_Local%Real_Part(NR2+1:Interf_Local%N_Sample)
!
!    smoothing right edge
!     call windowing( Interf_Local%N_Sample,  &
!                     Interf_Local%Real_Part, &
!                     Phase_Mertz%SigI,       &
!                     Interf_Local%dOpd       )
!
!    Fourier transform
     call iftosp_open( Interf_Local, Spectrum )
!
     call dalloc_Interferogram( Interf_Local )
     return
   end subroutine spectrum_mertz
!
!
   subroutine phase_mertz_correction( Phase_Mertz,&
                                      Spectrum    )
   implicit none
     type(type_Phase_Mertz)  , intent(in)                    :: Phase_Mertz
     type(type_Spectrum)     , intent(inout)                 :: Spectrum
     real(kind=DOUBLE)                                       :: rms
     real(kind=DOUBLE)       , dimension(:), allocatable     :: SArg
     real(kind=DOUBLE)                                       :: SArgMean
     real(kind=DOUBLE)       , dimension(:), allocatable     :: Mod
     real(kind=DOUBLE)       , dimension(:), allocatable     :: Arg
     real(kind=DOUBLE)       , dimension(:), allocatable     :: Part_Real
     real(kind=DOUBLE)       , dimension(:), allocatable     :: Part_Imag
     integer(kind=LONG)                                      :: ErrorCode
!
     allocate( Part_Real(Spectrum%N_Sample) )
     allocate( Part_Imag(Spectrum%N_Sample) )
     allocate( SArg(Spectrum%N_Sample) )
     allocate( Mod(Spectrum%N_Sample) )
     allocate( Arg(Spectrum%N_Sample) )
!
!    Reduced spectrum phase fitting and spectrum phase correction
     if( Phase_Mertz%Deg_Polynom == -1 ) then
!
!       linear interpolation
        call inttabdble( Phase_Mertz%Sp_Arg,  &
                         Phase_Mertz%Wn,      &
                         Phase_Mertz%N_Sample,&
                         SArg,                &
                         Spectrum%Wn,         &
                         Spectrum%N_Sample    )
        write(*,*) 'phase_mertz linear interpolation'
     else if( Phase_Mertz%Deg_Polynom == 0 ) then
!
!       cubic spline interpolation
        call intspline( Phase_Mertz%N_Sample,&
                        Phase_Mertz%Wn,      &
                        Phase_Mertz%Sp_Arg,  &
                        Spectrum%N_Sample,   &
                        Spectrum%Wn,         &
                        SArg                 )
        write(*,*) 'phase_mertz spline interpolation'
     else if( Phase_Mertz%Deg_Polynom == 1 ) then
!
!       Linear fitting
        call intlinear( Phase_Mertz%N_Sample,&
                        Phase_Mertz%Wn,      &
                        Phase_Mertz%Sp_Arg,  &
                        Phase_Mertz%Sp_Mod,  &
                        Spectrum%N_Sample,   &
                        Spectrum%Wn,         &
                        SArg,                &
                        rms                  )
        write(*,*) 'phase_mertz linear fitting',rms
     else if( Phase_Mertz%Deg_Polynom > 1 ) then
!
!       Polynomial fitting
        call intpolynome( Phase_Mertz%Deg_Polynom,&
                          Phase_Mertz%N_Sample,   &
                          Phase_Mertz%Wn,         &
                          Phase_Mertz%Sp_Arg,     &
                          Phase_Mertz%Sp_Mod,     &
                          Spectrum%N_Sample,      &
                          Spectrum%Wn,            &
                          SArg,                   &
                          SArgMean,               &
                          rms,                    &
                          ErrorCode )
        write(*,*) 'phase_mertz polynomial fitting',rms
     else
        write(*,*) 'Phase Mertz Fiiting Error ',Phase_Mertz%Deg_Polynom
        call exit(1)
     end if
!
!    spectrum phase correction
     call cplxtomodarg( Spectrum%Complex, Mod, Arg, Spectrum%N_Sample )
     Spectrum%Complex(1:Spectrum%N_Sample) =              &
                  Spectrum%Complex(1:Spectrum%N_Sample)   &
                * dcmplx( dcos(SArg(1:Spectrum%N_Sample)) &
                        ,-dsin(SArg(1:Spectrum%N_Sample)) )
     call cplxtomodarg( Spectrum%Complex, Mod, Arg, Spectrum%N_Sample )
!
!    real part extraction
     call cplxtorealimag( Spectrum%Complex, Part_Real, Part_Imag,&
                          Spectrum%N_Sample )
     Part_Imag(1:Spectrum%N_Sample) = 0.d+00
     call realimagtocplx( Spectrum%Complex, Part_Real, Part_Imag,&
                          Spectrum%N_Sample )
     call cplxtomodarg( Spectrum%Complex, Mod, Arg, Spectrum%N_Sample )
     deallocate( SArg )
     deallocate( Mod )
     deallocate( Arg )
     deallocate( Part_Real )
     deallocate( Part_Imag )
!
     return
   end subroutine phase_mertz_correction
!
!
end module phase_mertz_module
