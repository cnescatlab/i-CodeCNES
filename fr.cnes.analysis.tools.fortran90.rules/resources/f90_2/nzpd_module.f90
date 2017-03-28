!!#   nzpd_module.f90 --
!!# 
!!#            Project: SPS_GENERIC
!!#            Authors: NOVELTIS/B.TOURNIER
!!#               Date: october 2009
!!#            Version: $Revision: 1.15 $
!!#  Last modification: $Date: 2011-04-12 08:32:31 $
!!#
!!#  Language:  F90
!!#  Standards: Noveltis
!!#
!!# --
!!#
!!
!>  interferogram nzpd -- Module
!!
!! * Purpose
!!
!!   Module for interferogram nzpd computation
!!
!! * Description
!!      
!!   The objective of this module is to determine the sample number corresponding to Zero
!!   Path Difference (position of central fringe in the interferogram); the input is the 
!!   interferogram corrected for non-linearity, and the output is the NZPD of the investigated
!!   interferogram;
!!   The radiometric calibration equation requires the NZPD knowledge of the interferogram 
!!   triplet CS, BB, EW. Thus the NZPD detection has to be done for these three interferograms. 
!!   The detection of the ZPD position can be done by hardware in the instrument or by software.
!!   In the SPS three software methods (barycentre, Conne's and IASI-like) are implemented.
!!
!! * Sub-routines and functions
!!
!! -  nzpd_init        : initialisation of the type defining the ZPD parameters.
!! -  nzpd_barycentre  : NZPD computation thanks to the interferogram barycentre computation.
!! -  nzpd_connes      : NZPD computation thanks to the Connes' method.
!! -  nzpd             : NZPD software retrieval - NZPD method.
!! -  srd_argument     : computation of the spectrum reduced argument.
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.5.3
!!

module nzpd_module
   use precision_type
   use error_type
   use constantes_type
   use math_module
   use fft_module
   use zpd_type
   use interf_param_type
   use interferogram_type
!
   implicit none
!
!
   public :: nzpd_init,       &
             nzpd_barycentre, &
             nzpd_connes,     &
             nzpd,            &
             srd_argument
!
   contains

!
!
!> nzpd_init -- Public
!!
!! * Purpose
!!
!!     Initialisation of the type defining the ZPD parameters.
!!
!! * Description
!! 
!!     This subroutine allows to update the type dedicated to the ZPD parameters.
!!
!! * Inputs
!!
!!     - File_Zpd_param : character / input file with definition of ZPD computation parameters
!!     - Interf_Param   : type_Interf_Param / type for declaration and allocation of 
!!                        interferogram parameters
!!     - SB             : spectral band
!! 
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Zpd : type_Zpd / type for declaration and allocation of ZPD
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.5.3
!!

   subroutine nzpd_init( File_Zpd_param, &
                         Interf_Param,   &
                         SB,             &
                         Zpd             )
   implicit none
     character(len=500)     ,intent(in)                     :: File_Zpd_param
     type(type_Interf_Param),intent(in)                     :: Interf_Param
     integer(kind=LONG)     ,intent(in)                     :: SB
     type(type_Zpd)         ,intent(out)                    :: Zpd
     integer(kind=LONG)                                     :: iFile
     integer(kind=LONG)                                     :: iPos
!
     iFile = 10
     iPos  = 1
     open(unit=iFile, file=File_Zpd_param, status='old', err=999)
     iPos  = 2
     read(iFile,*,err=999)  Zpd%WidthCF
     iPos  = 3
     read(iFile,*,err=999)  Zpd%Ns_Fft
     iPos  = 4
     read(iFile,*,err=999)  Zpd%Round_Offset
     iPos  = 5
     read(iFile,*,err=999)  Zpd%NstepSrdFT
     iPos  = 6
     read(iFile,*,err=999)  Zpd%OffsetCF
     iPos  = 7
     read(iFile,*,err=999)  Zpd%Qual_Index_Cutoff
     iPos  = 8
     read(iFile,*,err=999)  Zpd%Wn_First
     iPos  = 9
     read(iFile,*,err=999)  Zpd%Wn_Last
     iPos  = 10
     close(unit=iFile)
     if( Zpd%Wn_First < Interf_Param%Wn_Usefull_First(SB) ) then
        write(*,*) 'Wn_First Error',Zpd%Wn_First
        go to 999
     end if
     if( Zpd%Wn_Last  > Interf_Param%Wn_Usefull_Last(SB) ) then
        write(*,*) 'Wn_Last Error',Zpd%Wn_Last
        go to 999
     end if
     Zpd%NZpd = 0
     Zpd%NZpd_Connes = 0
     Zpd%NZpd_Barycentre = 0
!
     return
!
999  write(*,*) 'parameters reading error',iPos
     write(*,*) 'nzpd_init Fatal Error'
     call exit(1)
   end subroutine nzpd_init
!
!

!
!
!> nzpd_barycentre -- Public
!!
!! * Purpose
!!
!!     NZPD computation thanks to the interferogram barycentre computation.
!!
!! * Description
!! 
!!     This subroutine allows to retrieve the NZPD by the interferogram barycentre computation.
!!     The method uses the real part of the interferograms and computes a first approximation 
!!     of the NZPD. The first approximation of the NZPD is the number of the sample of the 
!!     maximum value (positive or negative) of the interferogram. Then around this first value
!!     of the NZPD a finer value is found by using a quadratic barycentre formula.
!!
!! * Inputs
!!
!!     - Interf        : type_Interferogram / type for declaration and allocation of 
!!                       interferogram
!!
!! * Inputs/outputs
!!
!!     - Zpd : type_Zpd / type for declaration and allocation of ZPD
!!
!! * Outputs
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.5.3
!!

   subroutine nzpd_barycentre( Zpd,   &
                               Interf )
   implicit none
     type(type_Zpd)          , intent(inout)             :: Zpd
     type(type_Interferogram), intent(in)                :: Interf
     real(kind=DOUBLE)                                   :: MeanInterf
     integer(kind=LONG)                                  :: NsPivot
     integer(kind=LONG)                                  :: Ns
     integer(kind=LONG)                                  :: NsfirstNzpdCF
     integer(kind=LONG)                                  :: NslastNzpdCF
     integer(kind=LONG)                                  :: NZpdApprox
     real(kind=DOUBLE)                                   :: DiffMax
     real(kind=DOUBLE)                                   :: Diff
     real(kind=DOUBLE)                                   :: CogSup
     real(kind=DOUBLE)                                   :: CogSupNorm
     real(kind=DOUBLE)                                   :: CogInf
     real(kind=DOUBLE)                                   :: CogInfNorm
     real(kind=DOUBLE)                                   :: Cog
!
!    interferogram average
     NsPivot    = int(Interf%N_Sample/2)+1
     NsfirstNzpdCF = NsPivot - Zpd%WidthCF
     NslastNzpdCF  = NsPivot + Zpd%WidthCF
     MeanInterf = sum( Interf%Real_Part( NsfirstNzpdCF  &
                                       : NslastNzpdCF) )&
                / dble(2*Zpd%WidthCF+1)
     write(*,*) 'MeanInterf',MeanInterf
!
!    position of maximum difference with average
     DiffMax    = -1.d+22
     NZpdApprox = 0
     do Ns = NsfirstNzpdCF, NslastNzpdCF
        Diff = dabs(Interf%Real_Part(Ns)-MeanInterf)
        if( Diff .gt. DiffMax ) then
           DiffMax = Diff
           NZpdApprox = Ns
        end if
     end do
     write(*,*) 'NZpdApprox',NZpdApprox
     if( NZpdApprox == 0 ) then
        write(*,*) 'NZpdApprox Error'
        write(*,*) 'nzpd_barycentre Fatal Error'
        call exit(1)
     end if
!
!    Domain limits for barycentre computation
     NsfirstNzpdCF = NZpdApprox - Zpd%WidthCF
     NslastNzpdCF  = NZpdApprox + Zpd%WidthCF
     if( (NsfirstNzpdCF < 1) .or. (NslastNzpdCF > Interf%N_Sample) ) then
        write(*,*) 'NZpdCF Error',NsfirstNzpdCF,NslastNzpdCF,NZpdApprox
        write(*,*) 'nzpd_barycentre Fatal Error'
        call exit(1)
     end if
     MeanInterf = sum( Interf%Real_Part( NsfirstNzpdCF  &
                                       : NslastNzpdCF) )&
                / dble(2*Zpd%WidthCF+1)
     write(*,*) 'MeanInterf',MeanInterf
!
!    barycentre computation
     CogSup     = 0.d0
     CogInf     = 0.d0
     CogSupNorm = 0.d0
     CogInfNorm = 0.d0
     do Ns = NsfirstNzpdCF, NslastNzpdCF
        if( (Interf%Real_Part(Ns)-MeanInterf) >= 0.d+00 ) then
          CogSup     = CogSup + dble(Ns) * (Interf%Real_Part(Ns)-MeanInterf)**2
          CogSupNorm = CogSupNorm +        (Interf%Real_Part(Ns)-MeanInterf)**2
        else
          CogInf     = CogInf + dble(Ns) * (Interf%Real_Part(Ns)-MeanInterf)**2
          CogInfNorm = CogInfNorm +        (Interf%Real_Part(Ns)-MeanInterf)**2
        end if
     end do
     if( CogSupNorm == 0 ) then
        write(*,*) 'CogSupNorm Error'
        write(*,*) 'nzpd_barycentre Fatal Error'
        call exit(1)
     end if
     if( CogInfNorm == 0 ) then
        write(*,*) 'CogInfNorm Error'
        write(*,*) 'nzpd_barycentre Fatal Error'
        call exit(1)
     end if
!
     Cog  = ( (CogSup/CogSupNorm) + (CogInf/CogInfNorm) ) / 2.d+00
     Zpd%NZpd_Barycentre   = idnint( Cog )
     Zpd%Offset_Barycentre = Cog - Zpd%NZpd_Barycentre
     write(*,*) 'Barycentre           ',Cog
     write(*,*) 'Zpd%NZpd_Barycentre  ',Zpd%NZpd_Barycentre
     write(*,*) 'Zpd%Offset_Barycentre',Zpd%Offset_Barycentre
     Zpd%NZpd          = Zpd%NZpd_Barycentre
     Zpd%NZpd_Connes   = Zpd%NZpd_Barycentre
     Zpd%Offset_Connes = Zpd%Offset_Barycentre
!
     return
   end subroutine nzpd_barycentre
!
!

!
!
!> nzpd_connes -- Public
!!
!! * Purpose
!!
!!     NZPD computation thanks to the Connes' method.
!!
!! * Description
!! 
!!     This subroutine allows to retrieve the NZPD thanks to the Connes' method.
!!     This method works with the reduced raw spectra obtained by inverse Fourier transform of 
!!     the interferograms.  When the NZPD has a bad value, the phase of the raw spectrum varies
!!     linearly with the wave number. 
!!     The Connes's method consists in retrieving the slope of this phase. The slope gives the
!!     real ZPD sample number with respect to the NZPD_barycentre. 
!!     The values of the phase of the raw spectrum samples are adjusted linearly because the
!!     phase is not strictly linear (instrument noise, optical non linearity). The NZPD is the
!!     number of sample closer than the ZPD. If the ZPD is between two samples, the NZPD will 
!!     be retrieved with an error of 1 sample according to the measurement noise. To solve 
!!     these cases, an offset of 0.5 can be added to the NZPD_barycentre value.
!!     So from an algorithm point of view, the following steps are required:
!!     - First, reduced spectra limits are defined and all required tables are allocated;
!!     - Then, the interferogram centring on the barycentre is checked.
!!     - Thanks to an inverse Fourier transform, the complex reduced spectrum is computed and 
!!       is then converted in modulus and argument. The argument is defined as continuous.
!!     - The non-linear phase is extracted and the linear argument is adjusted thanks to 
!!       linear mean square.
!!     - The Zpd position is defined thanks to the Connes' method.
!!     - At last, the corresponding complex reduced spectrum is computed by an inverse Fourier 
!!       transform and is extracted and converted in modulus and argument which is defined as 
!!       continuous.
!!
!! * Inputs
!!
!!     - Interf_Param : type_Interf_Param / type for declaration and allocation of 
!!                      interferogram parameters
!!     - Interf       : type_Interferogram / type for declaration and allocation of 
!!                      interferogram
!!
!! * Inputs/outputs
!!
!!     - Zpd : type_Zpd / type for declaration and allocation of ZPD
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.5.3
!!

   subroutine nzpd_connes( Interf_Param, &
                           Zpd,          &
                           Interf        )
   implicit none
     !$PRAGMA C(crvlinearleastsqr)
     type(type_Interf_Param) , intent(in)                    :: Interf_Param
     integer(kind=LONG)                                      :: crvlinearleastsqr
     type(type_Zpd)          , intent(inout)                 :: Zpd
     type(type_Interferogram), intent(in)                    :: Interf
     integer(kind=LONG)                                      :: Ns_Fftp
     integer(kind=LONG)                                      :: Sens
     complex(kind=DOUBLE)    ,dimension(:) ,allocatable      :: C_Temp
     real(kind=DOUBLE)       ,dimension(:) ,allocatable      :: Phase
     integer(kind=LONG)                                      :: ires
     real(kind=DOUBLE)                                       :: A0
     real(kind=DOUBLE)                                       :: A1
     real(kind=DOUBLE)                                       :: rms
     real(kind=DOUBLE)                                       :: RZpd
     integer(kind=LONG)                                      :: Ns
!
!    Reduced spectra limits
     Zpd%dWn      = 1.d+00 / ( Zpd%Ns_Fft * Interf%dOpd )
     Zpd%Ns_First = idnint( Zpd%Wn_First / Zpd%dWn ) + 1
     Zpd%Ns_Last  = idnint( Zpd%Wn_Last  / Zpd%dWn ) + 1
     Zpd%N_Sample = Zpd%Ns_Last - Zpd%Ns_First + 1
     call alloc_Zpd( Zpd )
     Zpd%Wn(1:Zpd%N_Sample) = Zpd%dWn                           &
                * (/ (dble(Ns-1),Ns=Zpd%Ns_First,Zpd%Ns_Last)  /)
     call srd_argument( Interf_Param, Zpd )
     write(*,*) 'Zpd%Wn_First',Zpd%Wn_First
     write(*,*) 'Zpd%Wn_Last ',Zpd%Wn_Last
     write(*,*) 'Zpd%dWn     ',Zpd%dWn
     write(*,*) 'Zpd%Ns_First',Zpd%Ns_First
     write(*,*) 'Zpd%Ns_Last ',Zpd%Ns_Last
     write(*,*) 'Zpd%N_Sample',Zpd%N_Sample
     write(*,*) 'Interf%dOpd ',Interf%dOpd
     write(*,*) 'Zpd%Ns_Fft  ',Zpd%Ns_Fft
!
!    allocation
     allocate( C_Temp(Zpd%Ns_Fft+1) )
     allocate( Phase(Zpd%N_Sample) )
!
     call nzpd_barycentre( Zpd,   &
                           Interf )
!
!    Centring interferogram on the barycentre
     Ns_Fftp = int(Zpd%Ns_Fft/2)
     if( (Zpd%NZpd_Barycentre-Ns_Fftp < 1) .or.          &
         (Zpd%NZpd_Barycentre+Ns_Fftp > Interf%N_Sample) ) then
        write(*,*) 'Error in Interferogram Center',Zpd%NZpd_Barycentre
        call exit(1)
     end if
!
!    Complex reduced spectrum
     Sens = -1
     call fft_r2c( Zpd%Ns_Fft,                                   &
                   Sens,                                         &
                   Interf%Real_Part(Zpd%NZpd_Barycentre-Ns_Fftp),&
                   Interf%dOpd,                                  &
                   Zpd%dWn,                                      &
                   C_Temp )
!
!    extraction and convertion in modulus and argument
     Zpd%Sp_Reduced_Connes(1:Zpd%N_Sample) = &
                C_Temp(Ns_Fftp+Zpd%Ns_First:Ns_Fftp+Zpd%Ns_Last)
     call cplxtomodarg( Zpd%Sp_Reduced_Connes, Zpd%Mod_Connes, &
                        Zpd%Arg_Connes, Zpd%N_Sample )
!
!    argument continuous determination
     call phasemono( Zpd%Arg_Connes, Zpd%N_Sample )
!
!    Non linear Phase extraction
     Phase(1:Zpd%N_Sample) = Zpd%Arg_Connes(1:Zpd%N_Sample) &
                           - Zpd%Phase_Model(1:Zpd%N_Sample)
!
!    argument linear adjust
     ires = crvlinearleastsqr    &
       (%VAL(Zpd%N_Sample), Zpd%Wn, Phase, Zpd%Mod_Connes, A0, A1, rms) 
     write(*,*) 'A0, A1, rms',A0, A1, rms
     if (ires /= 0) then
        write(*,*) ' Linear ajustement error'
        write(*,*) ' nzpd_connes Fatal Error'
        call exit(1)
     end if
!
!    Zpd position
     RZpd = Zpd%NZpd_Barycentre + (A1/(twoPi)) / ( Interf%dOpd )
     Zpd%NZpd_Connes   = int( RZpd + 0.5 + Zpd%Round_Offset )
     Zpd%Offset_Connes = RZpd - Zpd%NZpd_Connes
     Zpd%Qual_Index_Connes = rms
     write(*,*) 'Zpd%NZpd_Connes      ',Zpd%NZpd_Connes
     write(*,*) 'Zpd%Offset_Connes    ',Zpd%Offset_Connes
     write(*,*) 'Zpd%Qual_Index_Connes',Zpd%Qual_Index_Connes
     write(*,*) 'RZpd                 ',RZpd
     write(*,*) 'Zpd%Round_Offset     ',Zpd%Round_Offset
     Zpd%NZpd = Zpd%NZpd_Connes
!
!    Corresponding Complex reduced spectrum
     Sens = -1
     call fft_r2c( Zpd%Ns_Fft,                               &
                   Sens,                                     &
                   Interf%Real_Part(Zpd%NZpd_Connes-Ns_Fftp),&
                   Interf%dOpd,                              &
                   Zpd%dWn,                                  &
                   C_Temp )
!
!    extraction and convertion in modulus and argument
     Zpd%Sp_Reduced_Connes(1:Zpd%N_Sample) = &
                C_Temp(Ns_Fftp+Zpd%Ns_First:Ns_Fftp+Zpd%Ns_Last)
     call cplxtomodarg( Zpd%Sp_Reduced_Connes, Zpd%Mod_Connes, &
                        Zpd%Arg_Connes, Zpd%N_Sample )
!
!    argument continuous determination
     call phasemono( Zpd%Arg_Connes, Zpd%N_Sample )
!     write(*,*) 'Ns, Zpd%Wn(Ns), Zpd%Mod_Connes(Ns), Zpd%Arg_Connes(Ns)'
!     do Ns = 1, Zpd%N_Sample
!        write(*,*) Ns, Zpd%Wn(Ns), Zpd%Mod_Connes(Ns), Zpd%Arg_Connes(Ns)
!     end do
!
     Zpd%NZpd = Zpd%NZpd_Connes
!
     deallocate( C_Temp )
     deallocate( Phase )
     return
   end subroutine nzpd_connes
!
!

!
!
!> nzpd -- Public
!!
!! * Purpose
!!
!!     NZPD software retrieval - NZPD method
!!
!! * Description
!! 
!!     This algorithm computes a first approximation of the NZPD by using the Barycentre 
!!     method on interferograms. Then a set inverse Fourier transform of the reduced 
!!     interferogram centred on a sample shifted around the approximated NOZPD is calculated.
!!     For each reduced spectrum the algorithm calculates the distance between the complex 
!!     spectra samples and the calibration line.
!!     The NZPD is the sample which gives the minimum of the distance in the complex spectra 
!!     space. This algorithm runs whatever the target is.
!!     The algorithm calculates a quality index of the NZPD retrieval. The quality index is 
!!     the ratio between the minimal distance and the distance of the two adjacent NZPD samples.
!!     The reduced spectrum is calculated for the retrieved NZPD value.
!!     The Fourier transform algorithm applied to the interferogram centred at the NZPD sample 
!!     position gives the complex spectrum corresponding to the measured interferogram.
!!     The following steps are required:     
!!     - First of all, all required tables are allocated.
!!     - Then, the interferogram centring on the barycentre is checked.
!!     - The calibration line is normalised and the background extraction is done.
!!     - Then, after the complex reduced spectrum has been computed thanks to an inverse Fourier 
!!       transform, the distance to the calibration line is determined as well as the position 
!!       of the minimal distance: the position of the NZPD is then defined as the sample which 
!!       gives this minimum of distance.
!!     - At last, before the corresponding complex reduced spectrum is computed by an inverse 
!!       Fourier transform and is extracted and converted in modulus and argument which is 
!!       defined as continuous, the quality index and the quality flag associated to the NZPD 
!!       are defined..
!!     - the last step consists in the deallocation of all required tables. 
!!
!! * Inputs
!!
!!     - Interf : type_Interferogram / type for declaration and allocation of interferogram
!! 
!! * Inputs/outputs
!!
!!     - Zpd    : type_Zpd / type for declaration and allocation of ZPD
!!     - Zpd_CS : type_Zpd / type for declaration and allocation of ZPD
!!     - Zpd_BB : type_Zpd / type for declaration and allocation of ZPD
!!
!! * Outputs
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.5.3
!!

   subroutine nzpd( Zpd_CS, &
                    Zpd_BB, &
                    Zpd,    &
                    Interf )
   implicit none
     type(type_Zpd)          , intent(inout)                 :: Zpd_CS
     type(type_Zpd)          , intent(inout)                 :: Zpd_BB
     type(type_Zpd)          , intent(inout)                 :: Zpd
     type(type_Interferogram), intent(in)                    :: Interf
     integer(kind=LONG)                                      :: Ns_Fftp
     integer(kind=LONG)                                      :: Sens
     complex(kind=DOUBLE)    ,dimension(:) ,allocatable      :: C_Temp
     real(kind=DOUBLE)       ,dimension(:) ,allocatable      :: Norm
     complex(kind=DOUBLE)    ,dimension(:) ,allocatable      :: Nord
     complex(kind=DOUBLE)    ,dimension(:) ,allocatable      :: BackGround
     integer(kind=LONG)                                      :: Ns
     integer(kind=LONG)                                      :: Nstep
     integer(kind=LONG)                                      :: Center
     integer(kind=LONG)      ,dimension(:) ,allocatable      :: NstepMin
!
!    allocation
     allocate( C_Temp(Zpd%Ns_Fft+1) )
     allocate( Norm(Zpd%N_Sample) )
     allocate( Nord(Zpd%N_Sample) )
     allocate( BackGround(Zpd%N_Sample) )
     allocate( NstepMin(1) )
!
     call nzpd_barycentre( Zpd,   &
                           Interf )
!
!    Centering interferogram on the barycentre
     Ns_Fftp = int(Zpd%Ns_Fft/2)
     if( (Zpd%NZpd_Barycentre-Ns_Fftp <= 1) .or.          &
         (Zpd%NZpd_Barycentre+Ns_Fftp >= Interf%N_Sample) ) then
        write(*,*) 'Error in Interferogram Center',Zpd%NZpd_Barycentre
        call exit(1)
     end if
!
!    calibration line normalisation
     Norm(1:Zpd%N_Sample) = &
         dreal( ( Zpd_BB%Sp_Reduced_Connes(1:Zpd%N_Sample)  &
                - Zpd_CS%Sp_Reduced_Connes(1:Zpd%N_Sample) )&
                * dconjg( Zpd_BB%Sp_Reduced_Connes(1:Zpd%N_Sample)   &
                        - Zpd_CS%Sp_Reduced_Connes(1:Zpd%N_Sample) ) )
     Nord(1:Zpd%N_Sample) = &
         dconjg( Zpd_BB%Sp_Reduced_Connes(1:Zpd%N_Sample) &
               - Zpd_CS%Sp_Reduced_Connes(1:Zpd%N_Sample) )
!
!    BackGround extraction
     BackGround(1:Zpd%N_Sample) = Zpd_CS%Sp_Reduced_Connes(1:Zpd%N_Sample)
!
!    pivot point loop
     do Nstep = 1, 2*Zpd%NstepSrdFT+1
        Center = Zpd%NZpd_Barycentre    &
               + Nstep-Zpd%NstepSrdFT-1 &
               + Zpd%OffsetCF
!
!       Complex reduced spectrum
        Sens = -1
        call fft_r2c( Zpd%Ns_Fft,                      &
                      Sens,                            &
                      Interf%Real_Part(Center-Ns_Fftp),&
                      Interf%dOpd,                     &
                      Zpd%dWn,                         &
                      C_Temp                           )
!
!       extraction
        Zpd%Sp_Reduced(1:Zpd%N_Sample) = &
                C_Temp(Ns_Fftp+Zpd%Ns_First:Ns_Fftp+Zpd%Ns_Last)
!
!       distance to the calibration line
        Zpd%Dist(Nstep) = 0.d+00
        do Ns = 1, Zpd%N_Sample
           Zpd%Dist(Nstep) = Zpd%Dist(Nstep) &
                           + dabs( dimag( ( Zpd%Sp_Reduced(Ns)    &
                                           -BackGround(Ns) )      &
                                          * Nord(Ns) ) ) / Norm(Ns)
        end do
        write(*,*) 'Zpd%Dist(Nstep)',Nstep,Zpd%Dist(Nstep)
     end do
!
!    position of the minimal distance
     NstepMin = minloc( Zpd%Dist( 1:2*Zpd%NstepSrdFT+1 ) )
     write(*,*) 'NstepMin',NstepMin(1)
!
!    position of the NZPD
     Zpd%NZpd = Zpd%NZpd_Barycentre &
              + NstepMin(1)-Zpd%NstepSrdFT-1 &
              + Zpd%OffsetCF
!
!    quality index
     if( (NstepMin(1) <= 1) .or. &
         (NstepMin(1) >= 2*Zpd%NstepSrdFT+1) ) then
        Zpd%Qual_Index = 2*Zpd%Qual_Index_Cutoff
     else
        Zpd%Qual_Index = Zpd%Dist(NstepMin(1))**2 &
                       / (Zpd%Dist(NstepMin(1)-1)*Zpd%Dist(NstepMin(1)+1))
     end if
!
!    quality flag
     if( Zpd%Qual_Index > Zpd%Qual_Index_Cutoff ) then
        Zpd%Flag_Qual = 0
     else
        Zpd%Flag_Qual = 1
     end if
!
!    corresponding reduced spectrum
     Sens = -1
     call fft_r2c( Zpd%Ns_Fft,                        &
                   Sens,                              &
                   Interf%Real_Part(Zpd%NZpd-Ns_Fftp),&
                   Interf%dOpd,                       &
                   Zpd%dWn,                           &
                   C_Temp                             )
!
!    extraction and convertion in modulus and argument
     Zpd%Sp_Reduced(1:Zpd%N_Sample) = &
                C_Temp(Ns_Fftp+Zpd%Ns_First:Ns_Fftp+Zpd%Ns_Last)
     call cplxtomodarg( Zpd%Sp_Reduced, Zpd%Mod, Zpd%Arg, Zpd%N_Sample )
!
!    argument continuous determination
     call phasemono( Zpd%Arg, Zpd%N_Sample )
!     write(*,*) 'Ns, Zpd%Wn(Ns), Zpd%Mod(Ns), Zpd%Arg(Ns)'
!     do Ns = 1, Zpd%N_Sample
!        write(*,*) Ns, Zpd%Wn(Ns), Zpd%Mod(Ns), Zpd%Arg(Ns)
!     end do
     write(*,*) 'Zpd%NZpd      ',Zpd%NZpd
     write(*,*) 'Zpd%Qual_Index',Zpd%Qual_Index
     write(*,*) 'Zpd%Flag_Qual ',Zpd%Flag_Qual
!
     deallocate( C_Temp )
     deallocate( NstepMin )
     deallocate( Norm )
     deallocate( Nord )
     deallocate( BackGround )
     return
   end subroutine nzpd
!
!

!
!
!> srd_argument -- Public
!!
!! * Purpose
!!
!!     Computation of the spectrum reduced argument
!!
!! * Description
!! 
!!     This algorithm computes the spectrum reduced argument.
!!
!! * Inputs
!!
!!     - Interf_Param   : type_Interf_Param / type for declaration and allocation of 
!!                        interferogram parameters
!! 
!! * Inputs/outputs
!!
!!     - Zpd    : type_Zpd / type for declaration and allocation of ZPD
!!
!! * Outputs
!!
!! * References
!!
!!     NOV-3820-NT-ATBD 
!!

   subroutine srd_argument( Interf_Param, Zpd )
   implicit none
     type(type_Interf_Param) , intent(in)                    :: Interf_Param
     type(type_Zpd)          , intent(inout)                 :: Zpd
     integer(kind=LONG)                                      :: Ns
!
     if( Interf_Param%Phase_Poly_Deg > 1 ) then
        do Ns = 1, Zpd%N_Sample
           call polyappli( 1,                             &
                           Zpd%Wn(Ns),                    &
                           Interf_Param%Phase_Poly_Coeff, &
                           Interf_Param%Phase_Poly_Deg,   &
                           Zpd%Phase_Model(Ns)            )
        end do
     else if( Interf_Param%Phase_Poly_Coeff(0) /= 0 ) then
        do Ns = 1, Zpd%N_Sample
           Zpd%Phase_Model(Ns) = Interf_Param%Phase_Poly_Coeff(0)       &
                               * dsin( twoPi * Zpd%Wn(Ns)               &
                                     / Interf_Param%Phase_Poly_Coeff(1) )
        end do
     else
        Zpd%Phase_Model(1:Zpd%N_Sample) = 0.0+00
     end if
!
     return
   end subroutine srd_argument
!
!
end module nzpd_module
