!!#   forman_module.f90 --
!!# 
!!#            Project: SPS_GENERIC
!!#            Authors: NOVELTIS/B.TOURNIER
!!#               Date: october 2009
!!#            Version: $Revision: 1.11 $
!!#  Last modification: $Date: 2010-11-17 09:56:31 $
!!#
!!#  Language:  F90
!!#  Standards: Noveltis
!!#
!!# --
!!#
!!
!>  interferogram forman -- Module
!!
!! * Purpose
!!
!!   Module for interferogram forman computation
!!
!! * Description
!!      
!!   The objective of the Forman module is to compute the symmetric interferogram centered on 
!!   the Zero Path Difference, as the measured interferogram will never have a sample at the 
!!   exact ZPD. This correction is done thanks to the knowledge of the complex argument of the 
!!   spectrum, the phase. The phase correction is obtained after convolution of the single-side 
!!   interferogram by the Forman kernel.
!!
!! * Sub-routines and functions
!!
!! -  forman_init   : initialisation of the type defining the interferogram forman parameters.
!! -  forman_kernel : Forman kernel computation - phase correction of the interferogram
!! -  forman_cnv    : convolution of the single-side interferogram by the Forman kernel.
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.5.8
!!

module forman_module
   use precision_type
   use error_type
   use constantes_type
   use forman_type
   use decim_one_fir_type
   use zpd_type
   use interferogram_type
   use math_module
   use fft_module
   use bilatere_module
!
   implicit none
!
!
   public :: forman_init,        &
             forman_kernel,      &
             forman_kernel_decim,&
             forman_kernel_cplx, &
             forman_cnv,         &
             forman_cnv_cplx
!
   contains
!
!  

!> forman_init -- Public
!!
!! * Purpose
!!
!!     Initialisation of the type defining the interferogram forman parameters.
!!
!! * Description
!! 
!!     This subroutine allows to update the type dedicated to the interferogram resampling 
!!     parameters
!!
!! * Inputs
!!                  
!!     - File_Forman_Param : character / input file with definition of interferogram Forman
!!                           parameters
!!
!! * Inputs/outputs
!!
!!     - Forman : type_Forman / type for Forman parameters declaration and allocation.
!!
!! * Outputs
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.5.8
!!

   subroutine forman_init( File_Forman_Param, Forman )
   implicit none
     character(len=*)        , intent(in)                 :: File_Forman_Param
     type(type_Forman)       , intent(inout)              :: Forman
     integer(kind=LONG)                                   :: iFile
     integer(kind=LONG)                                   :: iPos
!
     iFile = 10
     iPos  = 1
     write(*,'(a)') File_Forman_Param(1:len_trim(File_Forman_Param))
     open(unit=iFile, file=File_Forman_Param, status='old', err=999)
     iPos  = 2
     Forman%filename = File_Forman_Param
     read(iFile,'(a)',err=999) Forman%Mode_Traitement
     iPos  = 3
     read(iFile,'(a)',err=999) Forman%Type
     read(iFile,*,err=999) Forman%Ratio_bilatere
     read(iFile,*,err=999) Forman%Ns_Fft
     read(iFile,*,err=999) Forman%SigK
     read(iFile,*,err=999) Forman%SigS
     read(iFile,*,err=999) Forman%SigI
     read(iFile,*,err=999) Forman%Wn_Usefull_First
     read(iFile,*,err=999) Forman%Wn_Usefull_Last
     read(iFile,*,err=999) Forman%Wn_Ref
     write(*,*) 'Forman%Type          ',Forman%Type
     write(*,*) 'Forman%Ns_Fft        ',Forman%Ns_Fft
     write(*,*) 'Forman%SigK          ',Forman%SigK
     write(*,*) 'Forman%SigS          ',Forman%SigS
     write(*,*) 'Forman%SigI          ',Forman%SigI
     write(*,*) 'Forman%Ratio_bilatere',Forman%Ratio_bilatere
     write(*,*) 'Forman%Wn_First      ',Forman%Wn_Usefull_First
     write(*,*) 'Forman%Wn_Last       ',Forman%Wn_Usefull_Last
     write(*,*) 'Forman%Wn_Last       ',Forman%Wn_Ref
     call alloc_Forman( Forman )
     close(unit=iFile)
     return
 999 write(*,*) 'forman_init Fatal error',iPos
     call exit(1)
   end subroutine forman_init
!
!

!> forman_kernel -- Public
!!
!! * Purpose
!!
!!     Forman kernel computation - phase correction of the interferogram
!!
!! * Description
!! 
!!     This subroutine allows to compute the phase correction of the single-side interferogram, 
!!     close to the central fringe.
!!     The following steps are required:
!!       - First, the extremities of the reduced interferogram can be smoothed by applying a  
!!         windowing function. 
!!       - Then the complex reduced spectrum is obtained by inverse Fourier transform of the 
!!         reduced interferogram and the complex reduced spectrum is converted in modulus and 
!!         argument, the modulus is set at 1. The phase is modified to be monotonous. 
!!       - A complex windowing function can be applied on the edge of the spectrum. 
!!       - The real Forman kernel is the direct Fourier Transform of the conjugate symmetrised 
!!         reduced complex spectrum. 
!!       - The Forman kernel edge can be smoothed by using the windowing function. 
!!       - Then, it is normalised. 
!!     The Forman kernel contains only the phase correction of the interferogram.
!!
!! * Inputs
!!                  
!!     - Interf_Target : flag for interferogram target (CS/BB/EW)
!!     - SB            : spectral band
!!     - Ns_Fft        : Fourier Transform samples number
!!     - Interf        : type_Interferogram / type for declaration and allocation of 
!!                       interferogram
!!     - Zpd           : type_Zpd / type for zpd parameters declaration and allocation
!!
!! * Inputs/outputs
!!
!!     - Forman : type_Forman / type for Forman parameters declaration and allocation.
!!
!! * Outputs
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.5.8.1
!!

   subroutine forman_kernel( Interf_Target,&
                             Ns_Fft,       &
                             Interf,       &
                             Zpd,          &
                             Forman        )
   implicit none
     character(len=2)        , intent(in)                    :: Interf_Target
     integer(kind=LONG)      , intent(in)                    :: Ns_Fft
     type(type_Interferogram), intent(in)                    :: Interf
     type(type_Zpd)          , intent(in)                    :: Zpd
     type(type_Forman)       , intent(inout)                 :: Forman
     integer(kind=LONG)                                      :: Sens
     integer(kind=LONG)                                      :: Ns_Fftp
     integer(kind=LONG)                                      :: NsDel
     integer(kind=LONG)                                      :: Ns
     real(kind=DOUBLE)                                       :: dWn_Norm
     real(kind=DOUBLE)                                       :: NormR
     real(kind=DOUBLE)       , dimension(:), allocatable     :: If_Reduced
     real(kind=DOUBLE)       , dimension(:), allocatable     :: SMod
     real(kind=DOUBLE)       , dimension(:), allocatable     :: SArg
!
!    Windowing Interferogramme bilatere SigI
!    Windowing Kernel SigK
!    Windowing Reduced spectrum SigS
!
!    coherence control
     if( Forman%Type /= 'R' ) then
        write(*,*) 'Wrong Forman Type must be R :',Forman%Type
        call exit(1)
     end if
!
!    Kernel Half Size Ns_Fftp
     Ns_Fftp  = int( Forman%Ns_Fft/2 )
!
!    Scaling factors
     Forman%dOpd = Interf%dOpd
     Forman%dWn  = 1.d+00 / ( Forman%dOpd * Forman%Ns_Fft )
     dWn_Norm    = 1.d+00 / ( Interf%dOpd * dble(Ns_Fft) )
!
!    Reduced spectra band limites
     Forman%Ns_Usefull_First = &
               idnint(Forman%Wn_Usefull_First/Forman%dWn) + 1
     Forman%Ns_Usefull_Last  = &
               idnint(Forman%Wn_Usefull_Last/Forman%dWn) + 1
     Forman%N_Usefull_Sample = Forman%Ns_Usefull_Last &
                             - Forman%Ns_Usefull_First + 1
     Forman%Wn_Usefull_First =                              &
               dble( Forman%Ns_Usefull_First-1 ) * Forman%dWn
     Forman%Wn_Usefull_Last  =                              &
               dble( Forman%Ns_Usefull_Last-1 )  * Forman%dWn
     write(*,*) 'Forman%Ns_Usefull_First ',Forman%Ns_Usefull_First
     write(*,*) 'Forman%Ns_Usefull_Last  ',Forman%Ns_Usefull_Last
     write(*,*) 'Forman%N_Usefull_Sample ',Forman%N_Usefull_Sample
!
     NsDel = Zpd%NZpd - (Ns_Fftp+1)
     if( NsDel < 0 ) then
        write(*,*) 'First sample Error',NsDel
        write(*,*) 'forman_kernel Fatal Error'
        call exit(1)
     end if
     if( Zpd%NZpd + Ns_Fftp > Interf%N_Sample ) then
        write(*,*) 'Last sample Error',Interf%N_Sample
        write(*,*) 'forman_kernel Fatal Error'
        call exit(1)
     end if
!
!    Reduced interferogram extraction
     allocate( If_Reduced(Forman%Ns_Fft+1) )
     If_Reduced(1:Forman%Ns_Fft+1) =                       &
             Interf%Real_Part(NsDel+1:NsDel+Forman%Ns_Fft+1)
!
!    Windowing (SigI) Interferogram extremity
     if( Forman%SigI /= 0 ) then
        call windowing( Forman%Ns_Fft+1,&
                        If_Reduced,     &
                        Forman%SigI,    &
                        Forman%dOpd     )
     end if
!
     Sens = -1
     call fft_r2c_open( Forman%Ns_Fft, Sens, If_Reduced,         &
                        Forman%dOpd, dWn_Norm, Forman%Sp_Reduced )
!!!     call fft_r2c( Forman%Ns_Fft, Sens, If_Reduced,         &
!!!                   Forman%dOpd, dWn_Norm, Forman%Sp_Reduced )
     Forman%Sp_Usefull(1:Forman%N_Usefull_Sample) =                     &
            Forman%Sp_Reduced( Ns_Fftp + 1 + (Forman%Ns_Usefull_First-1)&
                              :Ns_Fftp + 1 + (Forman%Ns_Usefull_Last-1) )
!
!    conversion and normalisation
     allocate( SMod(Forman%N_Usefull_Sample) )
     allocate( SArg(Forman%N_Usefull_Sample) )
     call cplxtomodarg( Forman%Sp_Usefull, SMod, SArg,&
                        Forman%N_Usefull_Sample       )
     SMod(1:Forman%N_Usefull_Sample) = 1.d+00
     if( (Interf_Target == 'CS') .and.                 &
         (Forman%Mode_Traitement == 'MESUREIASI') ) then
        SArg(1:Forman%N_Usefull_Sample) = &
        SArg(1:Forman%N_Usefull_Sample) + Pi
     end if
!
!    Phase regularisation
     call phasemono( SArg, Forman%N_Usefull_Sample )
     call modargtocplx( Forman%Sp_Usefull, SMod, SArg,&
                        Forman%N_Usefull_Sample       )
     Forman%Wn_Usefull(1:Forman%N_Usefull_Sample) = Forman%dWn &
                   * (/ (dble(Ns-1),Ns=Forman%Ns_Usefull_First,&
                                      Forman%Ns_Usefull_Last) /)
!
!    Windowing (SigS) usefull spectrum extremity
     if( Forman%SigS /= 0 ) then
        call windowingcplx( Forman%N_Usefull_Sample,&
                            Forman%Sp_Usefull,      &
                            Forman%SigS,            &
                            Forman%dWn              )
     end if      
!
!    usefull spectrum symetrised
     Forman%Sp_Reduced(1:Forman%Ns_Fft+1) = dcmplx(0.d+00,0.d+00)
     Forman%Sp_Reduced( Ns_Fftp + 1 + (Forman%Ns_Usefull_First-1)    &
                       :Ns_Fftp + 1 + (Forman%Ns_Usefull_Last-1) ) = &
                          Forman%Sp_Usefull(1:Forman%N_Usefull_Sample)
!                     dconjg(Forman%Sp_Usefull(1:Forman%N_Usefull_Sample))
     Forman%Sp_Reduced( Ns_Fftp + 1 - (Forman%Ns_Usefull_First-1)       &
                       :Ns_Fftp + 1 - (Forman%Ns_Usefull_Last-1):-1 ) = &
                     dconjg(Forman%Sp_Usefull(1:Forman%N_Usefull_Sample))
!                          Forman%Sp_Usefull(1:Forman%N_Usefull_Sample)
!
!    Kernel computation
     Sens = +1
     call fft_c2r( Forman%Ns_Fft, Sens, Forman%Sp_Reduced,&
                   dWn_Norm, Forman%dOpd, Forman%Kernel_R )
!
!    Windowing (SigK) Kernel extremity
     if( Forman%SigK /= 0 ) then
        call windowing( Forman%Ns_Fft+1, &
                        Forman%Kernel_R, &
                        Forman%SigK,     &
                        Forman%dOpd      )
     end if
!
!    Kernel normalisation
     NormR = dabs( sum( Forman%Kernel_R(1:Forman%Ns_Fft+1) ) )
     write(*,*) 'Forman kernel NormR :',NormR
     Forman%Kernel_R(1:Forman%Ns_Fft+1) =            &
                  Forman%Kernel_R(1:Forman%Ns_Fft+1) &
                / NormR
     NormR = dabs( sum( Forman%Kernel_R(1:Forman%Ns_Fft+1) ) )
     write(*,*) 'Forman kernel NormR :',NormR
!
     deallocate( SMod )
     deallocate( SArg )
     deallocate( If_Reduced )
!
     return
   end subroutine forman_kernel
!
!

   subroutine forman_kernel_decim( Decim_One_fir,&
                                   Forman,       &
                                   Forman_Decim  )
   implicit none
     type(type_Decim_One_fir),intent(inout)                 :: Decim_One_fir
     type(type_Forman)       ,intent(in)                    :: Forman
     type(type_Forman)       ,intent(inout)                 :: Forman_Decim
     integer(kind=LONG)                                     :: NZpd
     integer(kind=LONG)                                     :: N_Sample_0
     integer(kind=LONG)                                     :: N_Start
     integer(kind=LONG)                                     :: N_Stop
     integer(kind=LONG)                                     :: N_Filt
     real(kind=DOUBLE)       ,dimension(:),allocatable      :: Phase
     complex(kind=DOUBLE)    ,dimension(:),allocatable      :: Kernel_dePhase
     integer(kind=LONG)                                     :: Ns
     integer(kind=LONG)                                     :: Ms
     real(kind=DOUBLE)       ,dimension(:),allocatable      :: I_Mod
     real(kind=DOUBLE)       ,dimension(:),allocatable      :: I_Arg
!
!    coherence control
     if( Forman%Type /= 'R' ) then
        write(*,*) 'Wrong Forman Type must be R :',Forman%Type
        call exit(1)
     end if
     if( Forman_Decim%Type /= 'C' ) then
        write(*,*) 'Wrong Forman Type must be C :',Forman_Decim%Type
        call exit(1)
     end if
!
     NZpd = int(Forman%Ns_Fft/2) + Decim_One_fir%Nc_Fir_1
!
!    compute decimation parameters
     Decim_One_fir%OpdMax = Forman%dOpd                 &
                         * Decim_One_fir%Decim_Factor_1 &
                         * dble(Decim_One_fir%Ns_Fft)   &
                         / 2.d+00
     Decim_One_fir%dWn = 1.d+00 / ( 2.d+00 * Decim_One_fir%OpdMax )
     Decim_One_fir%N_Shift = idnint(Decim_One_fir%WnRef / Decim_One_fir%dWn)
     Decim_One_fir%N_Sample_1 = Decim_One_fir%Ns_Decim + 1
     write(*,*) 'Decim_One_fir%OpdMax    ',Decim_One_fir%OpdMax
     write(*,*) 'Decim_One_fir%dWn       ',Decim_One_fir%dWn
     write(*,*) 'Decim_One_fir%N_Shift   ',Decim_One_fir%N_Shift
     write(*,*) 'Decim_One_fir%N_Sample_1',Decim_One_fir%N_Sample_1
!
!    Usefull Kernel Samples
     N_Sample_0 = ( Forman_Decim%Ns_Fft       &
                  * Decim_One_fir%Decim_Factor_1 ) 
!
!    First and last kept Kernel Samples
     N_Start = NZpd - int(N_Sample_0/2)
     N_Stop  = NZpd + int(N_Sample_0/2)
     N_Filt  = int(Decim_One_fir%N_Fir_1/2)
     write(*,*) 'NZpd            ',NZpd
     write(*,*) 'N_Start         ',N_Start
     write(*,*) 'N_Stop          ',N_Stop
!
!    compute the phase function for spectral band centering
     allocate( Phase(Forman%Ns_Fft+Decim_One_fir%N_Fir_1) )
     Phase(1:Forman%Ns_Fft+Decim_One_fir%N_Fir_1) =           &
             twoPi * dble(Decim_One_fir%N_Shift)              &
                   / dble(Decim_One_fir%Ns_Fft)               &
                   / dble(Decim_One_fir%Decim_Factor_1)       &
            *(/ (dble(Ns-NZpd),Ns=1,Forman%Ns_Fft+Decim_One_fir%N_Fir_1) /)
!
!    Kernel transfert
     allocate( Kernel_dePhase(Forman%Ns_Fft+Decim_One_fir%N_Fir_1) )
     Kernel_dePhase(:) = dcmplx(0.d+00,0.d+00)
     Kernel_dePhase(N_Filt+1:N_Filt+Forman%Ns_Fft+1) =   &
         dcmplx(Forman%Kernel_R(1:Forman%Ns_Fft+1),0.d+00)
!
!    Replace samples out of usefull domain
     Kernel_dePhase(N_Start-N_Filt:N_Start-1) =        &
                  Kernel_dePhase(N_Stop-N_Filt:N_Stop-1)
     Kernel_dePhase(N_Stop:N_Stop+N_Filt) =            &
                  Kernel_dePhase(N_Start:N_Start+N_Filt)
!
!    Apply the phase function
     Kernel_dePhase(1:Forman%Ns_Fft+Decim_One_fir%N_Fir_1) =                 &
                Kernel_dePhase(1:Forman%Ns_Fft+Decim_One_fir%N_Fir_1)        &
                * dcmplx( dcos(Phase(1:Forman%Ns_Fft+Decim_One_fir%N_Fir_1)),&
                          dsin(Phase(1:Forman%Ns_Fft+Decim_One_fir%N_Fir_1)) )
     allocate( I_Mod(Forman%Ns_Fft+Decim_One_fir%N_Fir_1) )
     allocate( I_Arg(Forman%Ns_Fft+Decim_One_fir%N_Fir_1) )
     call cplxtomodarg( Kernel_dePhase, I_Mod, I_Arg,     &
                        Forman%Ns_Fft+Decim_One_fir%N_Fir_1 )
     deallocate( I_Mod )
     deallocate( I_Arg )
     deallocate( Phase )
!
!    Decimation step
     do Ns = 1, Forman_Decim%Ns_Fft+1
        Ms = NZpd &
           - ( int(Forman_Decim%Ns_Fft/2)+1 - Ns ) &
             * Decim_One_fir%Decim_Factor_1
        if( (Ns == 1) .or. (Ns == Forman_Decim%Ns_Fft+1) ) then
           write(*,*) 'Ms ',Ms
        end if
        Forman_Decim%Kernel_C(Ns) = sum(                &
           Decim_One_fir%Fir_1(1:Decim_One_fir%N_Fir_1)   &
         * Kernel_dePhase(Ms-(Decim_One_fir%Nc_Fir_1-1)  &
                         :Ms+(Decim_One_fir%Nc_Fir_1-1)) )
     end do
     allocate( I_Mod(Forman_Decim%Ns_Fft+1) )
     allocate( I_Arg(Forman_Decim%Ns_Fft+1) )
     call cplxtomodarg( Forman_Decim%Kernel_C, I_Mod, I_Arg, &
                        Forman_Decim%Ns_Fft+1 )
     deallocate( I_Mod )
     deallocate( I_Arg )
     deallocate( Kernel_dePhase )
!
     return
   end subroutine forman_kernel_decim
!
!
   subroutine forman_kernel_cplx( Interf_Target,&
                                  Ns_Fft,       &
                                  Interf,       &
                                  Zpd,          &
                                  Forman        )
   implicit none
     character(len=2)        ,intent(in)                     :: Interf_Target
     integer(kind=LONG)      ,intent(in)                     :: Ns_Fft
     type(type_Interferogram),intent(in)                     :: Interf
     type(type_Zpd)          ,intent(in)                     :: Zpd
     type(type_Forman)       ,intent(inout)                  :: Forman
     integer(kind=LONG)                                      :: Sens
     integer(kind=LONG)                                      :: Ns_Fftp
     integer(kind=LONG)                                      :: NsDel
     integer(kind=LONG)                                      :: NsWn_Ref
     integer(kind=LONG)                                      :: NsWn_First
     integer(kind=LONG)                                      :: Ns
     real(kind=DOUBLE)                                       :: dWn_Norm
     real(kind=DOUBLE)                                       :: NormR
     real(kind=DOUBLE)                                       :: Arg0
     real(kind=DOUBLE)                                       :: Wn_First
     complex(kind=DOUBLE)    , dimension(:), allocatable     :: If_Reduced_cplx
     real(kind=DOUBLE)       , dimension(:), allocatable     :: SMod
     real(kind=DOUBLE)       , dimension(:), allocatable     :: SArg
!
!    Windowing Interferogramme bilatere SigI
!    Windowing Kernel SigK
!    Windowing Reduced spectrum SigS
!
!    coherence control
     if( Forman%Type /= 'C' ) then
        write(*,*) 'Wrong Forman Type must be C :',Forman%Type
        call exit(1)
     end if
!
!    Kernel Half Size Ns_Fftp
     Ns_Fftp  = int( Forman%Ns_Fft/2 )
!
!    Scaling factors
     Forman%dOpd = Interf%dOpd
     Forman%dWn  = 1.d+00 / ( Forman%dOpd * Forman%Ns_Fft )
     dWn_Norm    = 1.d+00 / ( Interf%dOpd * dble(Ns_Fft) )
!
!    Reduced spectra band limites
     Forman%Ns_Usefull_First = &
               idnint(Forman%Wn_Usefull_First/Forman%dWn) + 1
     Forman%Ns_Usefull_Last  = &
               idnint(Forman%Wn_Usefull_Last/Forman%dWn) + 1
     Forman%N_Usefull_Sample = Forman%Ns_Usefull_Last &
                             - Forman%Ns_Usefull_First + 1
     Forman%Wn_Usefull_First =                              &
               dble( Forman%Ns_Usefull_First-1 ) * Forman%dWn
     Forman%Wn_Usefull_Last  =                              &
               dble( Forman%Ns_Usefull_Last-1 )  * Forman%dWn
     write(*,*) 'Forman%Ns_Usefull_First ',Forman%Ns_Usefull_First
     write(*,*) 'Forman%Ns_Usefull_Last  ',Forman%Ns_Usefull_Last
     write(*,*) 'Forman%N_Usefull_Sample ',Forman%N_Usefull_Sample
!
     NsDel = Zpd%NZpd - (Ns_Fftp+1)
     if( NsDel < 0 ) then
        write(*,*) 'First sample Error',NsDel
        write(*,*) 'forman_kernel Fatal Error'
        call exit(1)
     end if
     if( Zpd%NZpd + Ns_Fftp > Interf%N_Sample ) then
        write(*,*) 'Last sample Error',Interf%N_Sample
        write(*,*) 'forman_kernel Fatal Error'
        call exit(1)
     end if
!
!    Reduced interferogram extraction
     allocate( If_Reduced_cplx(Forman%Ns_Fft+1) )
     If_Reduced_cplx(1:Forman%Ns_Fft+1) =                &
             Interf%Complex(NsDel+1:NsDel+Forman%Ns_Fft+1)
!
!    Windowing (SigI) Interferogram extremity
     if( Forman%SigI /= 0 ) then
        call windowingcplx( Forman%Ns_Fft+1,&
                            If_Reduced_cplx,&
                            Forman%SigI,    &
                            Forman%dOpd     )
     end if
!
     Sens = -1
     call fft_c2c_open( Forman%Ns_Fft, Sens, If_Reduced_cplx,    &
                           Forman%dOpd, dWn_Norm, Forman%Sp_Reduced )
!
!    reverse spectra
     Forman%Sp_Reduced(1:Forman%Ns_Fft) = &
              dconjg( Forman%Sp_Reduced(Forman%Ns_Fft:1:-1) )
     Forman%Sp_Reduced(1:Forman%Ns_Fft) = &
              Forman%Sp_Reduced(Forman%Ns_Fft:1:-1)
     NsWn_Ref   = idnint( Forman%Wn_Ref/Forman%dWn) + 1
     Wn_First   = Forman%dWn * ( NsWn_Ref-int(Forman%Ns_Fft/2) )
     NsWn_First = idnint(Wn_First/Forman%dWn)+1
     NsDel      = Forman%Ns_Usefull_First - NsWn_First
     Forman%Sp_Usefull(1:Forman%N_Usefull_Sample) =                  &
             Forman%Sp_Reduced( NsDel+1:NsDel+Forman%N_Usefull_Sample)
!
!    conversion and normalisation
     allocate( SMod(Forman%N_Usefull_Sample) )
     allocate( SArg(Forman%N_Usefull_Sample) )
     call cplxtomodarg( Forman%Sp_Usefull, SMod, SArg,&
                        Forman%N_Usefull_Sample       )
     SMod(1:Forman%N_Usefull_Sample) = 1.d+00
     if( (Interf_Target == 'CS') .and.                 &
         (Forman%Mode_Traitement == 'MESUREIASI') ) then
        SArg(1:Forman%N_Usefull_Sample) = &
        SArg(1:Forman%N_Usefull_Sample) + Pi
     end if
!
!    Phase regularisation
     call phasemono( SArg, Forman%N_Usefull_Sample )
     Arg0 = SArg( NsWn_Ref-Forman%Ns_Usefull_First+1)
     SArg(1:Forman%N_Usefull_Sample) = SArg(1:Forman%N_Usefull_Sample) - Arg0
     call modargtocplx( Forman%Sp_Usefull, SMod, SArg,&
                        Forman%N_Usefull_Sample       )
     Forman%Wn_Usefull(1:Forman%N_Usefull_Sample) = Forman%dWn &
                   * (/ (dble(Ns-1),Ns=Forman%Ns_Usefull_First,&
                                      Forman%Ns_Usefull_Last) /)
!
!    Windowing (SigS) usefull spectrum extremity
     if( Forman%SigS /= 0 ) then
        call windowingcplx( Forman%N_Usefull_Sample,&
                            Forman%Sp_Usefull,      &
                            Forman%SigS,            &
                            Forman%dWn              )
     end if      
!
!    usefull spectrum symetrised
     Forman%Sp_Reduced(1:Forman%Ns_Fft+1) = dcmplx(0.d+00,0.d+00)
     Forman%Sp_Reduced(NsDel+1:NsDel+Forman%N_Usefull_Sample) =      &
             dconjg( Forman%Sp_Usefull(Forman%N_Usefull_Sample:1:-1) )
!
!    Kernel computation
     Sens = +1
     allocate( Forman%Kernel_R(Forman%Ns_Fft+1) )
     call fft_c2c_open( Forman%Ns_Fft, Sens, Forman%Sp_Reduced,&
                        dWn_Norm, Forman%dOpd, Forman%Kernel_C )
!
!    Windowing (SigK) Kernel extremity
     if( Forman%SigK /= 0 ) then
        call windowingcplx( Forman%Ns_Fft+1,&
                            Forman%Kernel_C,&
                            Forman%SigK,    &
                            Forman%dOpd     )
     end if
!
!    Kernel normalisation
     deallocate( SMod )
     deallocate( SArg )
     allocate( SMod(Forman%Ns_Fft+1) )
     allocate( SArg(Forman%Ns_Fft+1) )
     call cplxtomodarg( Forman%Kernel_C, SMod, SArg, Forman%Ns_Fft+1 )
     NormR = sum( SMod(1:Forman%Ns_Fft+1) )
     SMod(1:Forman%Ns_Fft+1) =  SMod(1:Forman%Ns_Fft+1) / NormR
     call modargtocplx( Forman%Kernel_C, SMod, SArg, Forman%Ns_Fft+1 )
!
     deallocate( SMod )
     deallocate( SArg )
     deallocate( If_Reduced_cplx )
!
     return
   end subroutine forman_kernel_cplx
!
!

!> forman_cnv -- Public
!!
!! * Purpose
!!
!!     Convolution of the single-side interferogram by the Forman kernel.
!!
!! * Description
!! 
!!     This subroutine allows the convolution of the single-side interferogram by the 
!!     Forman kernel.
!!     The pivot sample is defined as the absolute maximum of the result of the interferogram
!!     convolution by the kernel.
!!     The half convoluted interferogram is symmetrised around the pivot sample.

!!
!! * Inputs
!!                  
!!     - Ns_Fft           : FFT size
!!     - Interf           : type_Interferogram / type for declaration and allocation of 
!!                          interferogram 
!!     - Forman           : type_Forman / type for Forman parameters declaration and allocation
!!
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Interf_Out       : type_Interferogram / type for declaration and allocation of 
!!                          interferogram
!! 
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.5.8.2
!!

   subroutine forman_cnv( Interf_Target,&
                          Ns_Fft,       &
                          Interf,       &
                          Forman,       &
                          Interf_Out    )
   implicit none
     character(len=2)            ,intent(in)              :: Interf_Target
     integer(kind=LONG)          ,intent(in)              :: Ns_Fft
     type(type_Interferogram)    ,intent(in)              :: Interf
     type(type_Forman)           ,intent(inout)           :: Forman
     type(type_Interferogram)    ,intent(out)             :: Interf_Out
     integer(kind=LONG)                                   :: Ns_Fftp
     integer(kind=LONG)                                   :: Forman_Ns_Fftp
     integer(kind=LONG)                                   :: Ns
     integer(kind=LONG)                                   :: Ns1
     integer(kind=LONG)                                   :: Ns2
     integer(kind=LONG)                                   :: Ns3
     integer(kind=LONG)                                   :: n
     integer(kind=LONG)                                   :: m0
     integer(kind=LONG)                                   :: m
     integer(kind=LONG)      ,dimension(:) ,allocatable   :: NZpd
     real(kind=DOUBLE)       ,dimension(:) ,allocatable   :: Interf_Cnv
     real(kind=DOUBLE)                                    :: Opd0
     real(kind=DOUBLE)                                    :: Time0
     real(kind=DOUBLE)                                    :: Interf_Avg
!
!    coherence control
     if( Forman%Type /= 'R' ) then
        write(*,*) 'Wrong Forman Type must be R :',Forman%Type
        call exit(1)
     end if
!
!
     Forman_Ns_Fftp = int(Forman%Ns_Fft/2)
!
!    First interferogram sample to be convolved
     Ns1 = Forman_Ns_Fftp+1
!
!    Last interferogram sample to be convolved
     Ns2 = Interf%N_Sample - Forman_Ns_Fftp
!
!    Interferogram convolution by the kernel
     allocate( Interf_Cnv(Ns2-Ns1+1) )
     Interf_Cnv(1:Ns2-Ns1+1) = 0.d+00
     do Ns = Ns1, Ns2
        n = Ns - Ns1 + 1
        m0 = Ns-Forman_Ns_Fftp-1
        do m = 1, Forman%Ns_Fft+1
           Interf_Cnv(n) = Interf_Cnv(n)                        &
                         + ( Interf%Real_Part(m0+m)             &
                           * Forman%Kernel_R(Forman%Ns_Fft+2-m) )
        end do
     end do
!
!    Search for the pivot sample
     if( Interf_Target == 'BB' ) then
        allocate( NZpd(1) )
        NZpd = maxloc( dabs(Interf_Cnv(1:Ns2-Ns1+1)) )
        Forman%NZpd = NZpd(1)
        deallocate( NZpd )
     end if
!
     write(*,*) 'Symmetry check - NZpd ',Forman%NZpd
     write(*,*) 'Interf_Cnv(NZpd)      ',Interf_Cnv(Forman%NZpd)
     do Ns=1,50
       write(*,*) '-/+',Ns,'samples :',Interf_Cnv(Forman%NZpd-Ns),&
                                       Interf_Cnv(Forman%NZpd+Ns)
     enddo
!
!    FFT final
     Ns_Fftp = int(Ns_Fft/2)
!
!    convolution size
     Ns3 = Ns2-Ns1+1
!
!    last usefull convoluted sample
     Ns2 = Forman%NZpd + Ns_Fftp
     if( (Ns2 > Ns3) ) then
        write(*,*) 'Not Enough Samples',Ns2, Ns3
        write(*,*) 'forman_cnv Fatal Error'
        call exit(1)
     end if
!
!    Interferogram transfer
     Interf_Out%N_Sample = Ns_Fft+1
     Interf_Out%Type     = Interf%Type
     write(*,*) 'Ns2                ',Ns2
     write(*,*) 'Interf_Out%N_Sample',Interf_Out%N_Sample
     write(*,*) 'Interf_Out%Type :  ',Interf_Out%Type
     call alloc_Interferogram( Interf_Out )
     Interf_Out%dOpd    = Interf%dOpd
     Interf_Out%dTime   = Interf%dTime
     Interf_Out%OpdMax  = Interf_Out%dOpd * dble(int(Interf_Out%N_Sample/2))
     Interf_Out%TimeMax = Interf_Out%dTime * dble(int(Interf_Out%N_Sample/2))
     call Interferogram_Basis( Interf_Out )
     Opd0  = Interf_Out%Opd(int(Interf_Out%N_Sample/2)+1)
     Time0 = Interf_Out%Time(int(Interf_Out%N_Sample/2)+1)
     Interf_Out%Opd(1:Interf_Out%N_Sample) = &
                Interf_Out%Opd(1:Interf_Out%N_Sample) - Opd0
     Interf_Out%Time(1:Interf_Out%N_Sample) = &
                Interf_Out%Time(1:Interf_Out%N_Sample) - Time0
!
!
!    symetrisation of the half interferogram
     Interf_Out%Real_Part(Ns_Fftp+1:Ns_Fft+1) = &
                          Interf_Cnv(Forman%NZpd:Ns2)
     Interf_Out%Real_Part(1:Ns_Fftp) = &
            Interf_Cnv(Ns2:Forman%NZpd+1:-1)
!
!    Interferogram centering
     Interf_Avg = sum(Interf_Out%Real_Part(1:Interf_Out%N_Sample))&
                / dble(Interf_Out%N_Sample)
     Interf_Out%Real_Part(1:Interf_Out%N_Sample) =             &
                    Interf_Out%Real_Part(1:Interf_Out%N_Sample)&
                  - Interf_Avg
!
     deallocate( Interf_Cnv )
     return
   end subroutine forman_cnv
!
!
   subroutine forman_cnv_cplx( Interf_Target,&
                               Ns_Fft,       &
                               Interf,       &
                               Forman,       &
                               Interf_Out    )
   implicit none
     character(len=2)            ,intent(in)              :: Interf_Target
     integer(kind=LONG)          ,intent(in)              :: Ns_Fft
     type(type_Interferogram)    ,intent(in)              :: Interf
     type(type_Forman)           ,intent(inout)           :: Forman
     type(type_Interferogram)    ,intent(out)             :: Interf_Out
     integer(kind=LONG)                                   :: Ns_Fftp
     integer(kind=LONG)                                   :: Forman_Ns_Fftp
     integer(kind=LONG)                                   :: Ns
     integer(kind=LONG)                                   :: Ns1
     integer(kind=LONG)                                   :: Ns2
     integer(kind=LONG)                                   :: Ns3
     integer(kind=LONG)                                   :: n
     integer(kind=LONG)                                   :: m0
     integer(kind=LONG)                                   :: m
     integer(kind=LONG)      ,dimension(:) ,allocatable   :: NZpd
     complex(kind=DOUBLE)    ,dimension(:) ,allocatable   :: Interf_Temp
     complex(kind=DOUBLE)    ,dimension(:) ,allocatable   :: Interf_Cnv_cplx
     real(kind=DOUBLE)       ,dimension(:) ,allocatable   :: I_Mod
     real(kind=DOUBLE)       ,dimension(:) ,allocatable   :: I_Arg
     real(kind=DOUBLE)                                    :: Opd0
     real(kind=DOUBLE)                                    :: Time0
!
!    coherence control
     if( Forman%Type /= 'C' ) then
        write(*,*) 'Wrong Forman Type must be C :',Forman%Type
        call exit(1)
     end if
!
!
     Forman_Ns_Fftp = int(Forman%Ns_Fft/2)
!
!    First interferogram sample to be convolved
     Ns1 = Forman_Ns_Fftp+1
!
!    Last interferogram sample to be convolved
     Ns2 = Interf%N_Sample
!!     Ns2 = Interf%N_Sample - Forman_Ns_Fftp
!
!    Decimated Interferogram zero padding
     
     allocate ( Interf_Temp(Interf%N_Sample+Forman_Ns_Fftp+1) )
     Interf_Temp(:)     = 0_DOUBLE
     Interf_Temp(1:Ns2) = Interf%Complex(1:Ns2)
     write(*,*) 'Forman_Ns_Fftp  ',Forman_Ns_Fftp
     write(*,*) 'Interf%N_Sample ',Interf%N_Sample
     write(*,*) 'Ns1             ',Ns1
     write(*,*) 'Ns2             ',Ns2
!
!    Interferogram convolution by the kernel
     allocate( Interf_Cnv_cplx(Ns2-Ns1+1) )
     Interf_Cnv_cplx(1:Ns2-Ns1+1) = 0.d+00
     do Ns = Ns1, Ns2
        n = Ns - Ns1 + 1
        m0 = Ns-Forman_Ns_Fftp-1
        do m = 1, Forman%Ns_Fft+1
           Interf_Cnv_cplx(n) = Interf_Cnv_cplx(n)              &
                              + ( Interf_Temp(m0+m)             &
                           * Forman%Kernel_C(Forman%Ns_Fft+2-m) )
        end do
     end do
     deallocate( Interf_Temp )
!
!    Search for the pivot sample
     if( Interf_Target == 'BB' ) then
        allocate( NZpd(1) )
        allocate( I_Mod(Ns2-Ns1+1) )
        allocate( I_Arg(Ns2-Ns1+1) )
        call cplxtomodarg( Interf_Cnv_cplx, I_Mod, I_Arg, Ns2-Ns1+1 )
        NZpd = maxloc( I_Mod(1:Forman%Ns_Fft+1) )
        Forman%NZpd = NZpd(1)
        deallocate( I_Mod )
        deallocate( I_Arg )
        deallocate( NZpd )
     end if
!
     write(*,*) 'Symmetry check - NZpd ',Forman%NZpd
     write(*,*) 'Interf_Cnv(NZpd)      ',Interf_Cnv_cplx(Forman%NZpd)
     do Ns=1,50
       write(*,*) '-/+',Ns,'samples :',Interf_Cnv_cplx(Forman%NZpd-Ns),&
                                       Interf_Cnv_cplx(Forman%NZpd+Ns)
     enddo
!
!    FFT final
     Ns_Fftp = int(Ns_Fft/2)
!
!    convolution size
     Ns3 = Ns2-Ns1+1
!
!    last usefull convoluted sample
     Ns2 = Forman%NZpd + Ns_Fftp
     if( (Ns2 > Ns3) ) then
        write(*,*) 'Not Enough Samples',Ns2, Ns3
        write(*,*) 'forman_cnv Fatal Error'
!!!!        call exit(1)
     end if
!
!    Interferogram transfer
     Interf_Out%N_Sample = Ns_Fft+1
     Interf_Out%Type     = Interf%Type
     write(*,*) 'Ns2                ',Ns2
     write(*,*) 'Interf_Out%N_Sample',Interf_Out%N_Sample
     write(*,*) 'Interf_Out%Type :  ',Interf_Out%Type
     call alloc_Interferogram( Interf_Out )
     Interf_Out%dOpd    = Interf%dOpd
     Interf_Out%dTime   = Interf%dTime
     Interf_Out%OpdMax  = Interf_Out%dOpd * dble(int(Interf_Out%N_Sample/2))
     Interf_Out%TimeMax = Interf_Out%dTime * dble(int(Interf_Out%N_Sample/2))
     call Interferogram_Basis( Interf_Out )
     Opd0  = Interf_Out%Opd(int(Interf_Out%N_Sample/2)+1)
     Time0 = Interf_Out%Time(int(Interf_Out%N_Sample/2)+1)
     Interf_Out%Opd(1:Interf_Out%N_Sample) =               &
                Interf_Out%Opd(1:Interf_Out%N_Sample) - Opd0
     Interf_Out%Time(1:Interf_Out%N_Sample) =                &
                Interf_Out%Time(1:Interf_Out%N_Sample) - Time0
!
!
!    symetrisation of the half interferogram
     Interf_Out%Complex(Ns_Fftp+1:Ns_Fft+1) = &
                   Interf_Cnv_cplx(Forman%NZpd:Ns2)
     Interf_Out%Complex(1:Ns_Fftp) =                &
            dconjg(Interf_Cnv_cplx(Ns2:Forman%NZpd+1:-1))
!
     allocate( I_Mod(Ns_Fft+1) )
     allocate( I_Arg(Ns_Fft+1) )
     call cplxtomodarg( Interf_Out%Complex, I_Mod, I_Arg, Ns_Fft+1 )
     deallocate( I_Mod )
     deallocate( I_Arg )
     deallocate( Interf_Cnv_cplx )
     return
   end subroutine forman_cnv_cplx
!
!
end module forman_module
