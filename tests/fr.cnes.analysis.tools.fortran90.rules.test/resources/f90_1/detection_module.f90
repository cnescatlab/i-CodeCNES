!!#  detection_module.f90 -- 
!!# 
!!#            Project: SPS_GENERIC
!!#            Authors: NOVELTIS/B.TOURNIER
!!#               Date: august 2010
!!#            Version: $Revision: 1.4 $
!!#  Last modification: $Date: 2011-11-02 14:57:50 $
!!#
!!#  Language:  F90
!!#  Standards: Noveltis
!!#
!!# --
!!#
!!
!> detection_module -- Module
!!
!! * Purpose
!!
!!   Module for interferogram detection
!!
!! * Description
!!      
!!   The module simulates algorithms for interferogram detection.
!!   A photosensitive detector sensor transforms electromagnetic energy into electrical
!!   signal. A photon or quantum detector is based on the photoelectric effect. The radiation
!!   is absorbed within a device by direct interaction with electrons, which are excited to a
!!   higher energy level. Under the effect of an electric field these carriers move and produce
!!   measurable electric current. 
!!   The interferogram measured by the instrument is simulated as the sum of the three terms, 
!!   three currents detected in Ampere:
!!        - the first term is the number of electron by unit of time multiplied by the electron 
!!          charge expressed in coulomb (integral of the spectrum); 
!!        - the second term takes into account the interferogram value by using a scale factor
!!          to convert the interferogram into current; 
!!        - the last term is the dark current, that means the current that flows through 
!!          photosensitive detector when no photon arrives on the detector. 
!!
!! * Sub-routines and functions
!!
!! -  detection_init   : initialisation of the type defining the interferogram 
!!                       detection parameters 
!! -  interf_detection : computation in voltage of the interferogram arriving on the detector
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.4.2.5
!!

module detection_module
   use precision_type
   use error_type
   use constantes_type
   use detection_type
   use spectrum_type
   use interf_param_type
   use interferogram_type
   use math_module
!
   implicit none
!
!
   public :: detection_init, &
             interf_detection
!
!
   contains
!
!

!> detection_init -- Public
!!
!! * Purpose
!!
!!     Initialisation of the type defining the interferogram detection parameters 
!!
!! * Description
!!
!!     The detection_init subroutine allows to update the type defining the interferogram  
!!     detection parameters thanks to the read of the File_Param file; 
!!
!! * Inputs
!!
!!     - File_Param : character / input file with definition of interferogram 
!!                    detection parameters
!!
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Detection : type_Detection / type for interferogram detection parameters 
!!                   declaration and allocation.
!!
!! * References
!!

   subroutine detection_init( File_Param,&
                              Detection  )
   implicit none
     character(len=*)        ,intent(in)                 :: File_Param
     type(type_Detection)    ,intent(out)                :: Detection
!
     integer(kind=LONG)                                  :: iFile
     integer(kind=LONG)                                  :: iPos
     integer(kind=LONG)                                  :: Ns
!
     iFile = 10
!
     iPos = 1
     write(*,'(a)') File_Param(1:len_trim(File_Param))
     open(unit=iFile, file=File_Param, status='old',err=999)
     Detection%filename = File_Param
     iPos = 2
     read(iFile,*,err=999) Detection%NsWn0
     write(*,*) 'Detection%NsWn0 :',Detection%NsWn0
     call alloc_Detection( Detection )
     iPos = 3
     read(iFile,*,err=999) 
     do Ns = 1, Detection%NsWn0
        iPos = 3 + Ns
        read(iFile,*,err=999) Detection%Wn0(Ns),            &
                              Detection%Transfert_Mod(Ns),  &
                              Detection%Transfert_Dir(Ns),  &
                              Detection%Detector_Response(Ns)
        write(*,*) 'Detection :',                 &
                    Detection%Wn0(Ns),            &
                    Detection%Transfert_Mod(Ns),  &
                    Detection%Transfert_Dir(Ns),  &
                    Detection%Detector_Response(Ns)
     end do
     iPos = 4 + Detection%NsWn0
     read(iFile,*,err=999) Detection%V_Min
     write(*,*) 'Detection%V_Min       :',Detection%V_Min
     read(iFile,*,err=999) Detection%V_Max
     write(*,*) 'Detection%V_Max       :',Detection%V_Max
     read(iFile,*,err=999) Detection%Dark_Current
     write(*,*) 'Detection%Dark_Current:',Detection%Dark_Current
     iPos = 5 + Detection%NsWn0
     read(iFile,*,err=999) Detection%Rcr
     write(*,*) 'Detection%Rcr         :',Detection%Rcr
     iPos = 6 + Detection%NsWn0
     read(iFile,*,err=999) Detection%Vdc
     write(*,*) 'Detection%Vdc         :',Detection%Vdc
     iPos = 7 + Detection%NsWn0
     read(iFile,*,err=999) Detection%Noise
     write(*,*) 'Detection%Noise       :',Detection%Noise
     iPos = 8 + Detection%NsWn0
     read(iFile,*,err=999) Detection%Photon_Noise
     write(*,*) 'Detection%Photon_Noise:',Detection%Photon_Noise
     iPos = 8 + Detection%NsWn0
     read(iFile,*,err=999) Detection%H_Sat
     write(*,*) 'Detection%H_Sat       :',Detection%H_Sat
     iPos = 10 + Detection%NsWn0
     read(iFile,*,err=999) Detection%Pupill_Diam
     write(*,*) 'Detection%Pupill_Diam :',Detection%Pupill_Diam
     iPos = 11 + Detection%NsWn0
     read(iFile,*,err=999) Detection%Pixel_Diam
     write(*,*) 'Detection%Pixel_Diam  :',Detection%Pixel_Diam
     iPos = 12 + Detection%NsWn0
     read(iFile,*,err=999) Detection%SigS
     write(*,*) 'Detection%SigS        :',Detection%SigS
     iPos = 13 + Detection%NsWn0
     close(unit=iFile)
!
     return
 999 write(*,*) 'detection_init Fatal Error',iPos
     call exit(1)
   end subroutine detection_init
!
!

!> interf_detection -- Public
!!
!! * Purpose
!!
!!     Computation in voltage of the interferogram arriving on the detector. 
!!
!! * Description
!!
!!     The interf_detection subroutine allows to compute in voltage the interferogram
!!     arriving on the detector. The computation requires the following steps:  
!!      -  The interferogram, expressed in Amperes, is simulated as the sum of three 
!!         currents detected in Ampere:
!!                   - the number of electron by unit of time multiplied by the electron 
!!                     charge in coulomb (integral of the spectrum),
!!                   - the interferogram value by using a scale factor to convert the 
!!                     interferogram into current,
!!                   - the dark current that flows through photosensitive detector when 
!!                     no photon arrives on the detector. 
!!      - The second step consists in detection chain noise introduction: the detection  
!!        noise is added in Ampere as a Gaussian random noise.
!!      - At last, the current is converted in voltage (Volts), takin into account a 
!!        resistance of negative feedback and a offset voltage  
!!
!! * Inputs
!!
!!     - Detection    : type_Detection / type for interferogram detection parameters 
!!                      declaration and allocation.
!!     - Interf_Param : type_Interf_Param / type for declaration and allocation of 
!!                      interferogram parameters
!!     - Spectrum     : type_Spectrum / type for declaration and allocation of spectrum 
!!
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Interf : type_Interferogram / type for declaration and allocation of 
!!                interferogram
!!
!! * References
!!

   subroutine interf_detection( Interf_Target,&
                                Detection,    &
                                Interf_Param, &
                                Spectrum_bg,  &
                                Spectrum,     &
                                Interf        )
   implicit none
     character(len=*)        ,intent(in)                 :: Interf_Target
     type(type_Detection)    ,intent(in)                 :: Detection
     type(type_Interf_Param) ,intent(in)                 :: Interf_Param
     type(type_Spectrum)     ,intent(in)                 :: Spectrum_bg
     type(type_Spectrum)     ,intent(in)                 :: Spectrum
     type(type_Interferogram),intent(inout)              :: Interf
!
     real(kind=DOUBLE)                                   :: Spectrum_Sum
     real(kind=DOUBLE)                                   :: Spectrum_bg_Sum
     real(kind=DOUBLE)                                   :: Scale_Factor
     real(kind=DOUBLE)                                   :: Brs
     real(kind=DOUBLE)                                   :: Br
     real(kind=DOUBLE)                                   :: Br2sum
     real(kind=DOUBLE)                                   :: Brsig
     integer(kind=LONG)                                  :: Ns
     integer(kind=LONG)                                  :: Ks
     integer(kind=LONG)                                  :: Kk
     real(kind=DOUBLE)                                   :: Photon_Noise
!
!    Interferogram modulated baseline : Amperes
     Spectrum_Sum = sum( dsqrt(                                     &
              dreal( Spectrum%Complex(1:Spectrum%N_Sample)          &
                   * dconjg(Spectrum%Complex(1:Spectrum%N_Sample)) )&
                                                                 ) )&
                * Spectrum%dWn
!
!    Interferogram direct baseline contribution : Amperes
     Spectrum_bg_Sum = sum( dsqrt(                                        &
              dreal( Spectrum_bg%Complex(1:Spectrum_bg%N_Sample)          &
                   * dconjg(Spectrum_bg%Complex(1:Spectrum_bg%N_Sample)) )&
                                                                       ) )&
                * Spectrum_bg%dWn
     Scale_Factor = Interf_Param%VCC * Interf%dTime
!
     write(*,*) 'Dark Current',Detection%Dark_Current * Detection%Rcr
     write(*,*) 'Background  ',Spectrum_bg_Sum * Detection%Rcr
     write(*,*) 'BaseLine    ',Spectrum_Sum * Detection%Rcr
     write(*,*) 'Interf Min  ',minval(Interf%Real_Part(1:Interf%N_Sample))&
                               * Detection%Rcr * Scale_Factor
     write(*,*) 'Interf Max  ',maxval(Interf%Real_Part(1:Interf%N_Sample))&
                               * Detection%Rcr * Scale_Factor
!
!    Complete interferogram : Amperes
     Interf%Real_Part(1:Interf%N_Sample) =                          &
              ( Interf%Real_Part(1:Interf%N_Sample) * Scale_Factor )&
              + Spectrum_Sum                                        &
              + Spectrum_bg_Sum                                     &
              + Detection%Dark_Current
!
!    Photon noise introduction
     if( Interf_Target == 'EW' ) then
        if( Detection%Photon_Noise > 0.d+00 ) then
           write(*,*) ' Detection : Photon Noise Applied'
           Photon_Noise = dsqrt( 2.d+00 &
                * dsqrt( Spectrum_Sum*Spectrum_Sum &
                + Spectrum_bg_Sum*Spectrum_bg_Sum )&
                * Cst_chel/ Interf%dTime )
           Br2sum = 0.d+00
           do Ns = 1, Interf%N_Sample
              Ks=idnint(mod(Interf%Real_Part(Ns)*1.d+08,128.d+00))
              do Kk = 1, Ks
                 Br = prnd()
              end do
              Br     = Photon_Noise*grnd(0)
              Br2sum = Br2sum + Br**2
              Interf%Real_Part(Ns) = Interf%Real_Part(Ns) + Br
           end do
           Brsig = dsqrt( Br2sum/dble(Interf%N_Sample-1) )
           write(*,*) 'Detection : Std Applied Photon Noise ',&
                       Brsig,' Amperes'
           Brsig = Brsig * Detection%Rcr
           write(*,*) 'Detection : Std Applied Photon Noise ',&
                       Brsig,' Volts'
        else
           write(*,*) 'Detection : No Photon Noise Applied'
        end if
!
!       Detection noise introduction
        if( Detection%Noise > 0.d+00 ) then
           Brs = Detection%Noise            &
               / dsqrt( Interf%dTime )
           write(*,*) ' Detection : Applied Noise',Brs,' Amperes'
           Br2sum = 0.d+00
           do Ns = 1, Interf%N_Sample
              Ks=idnint(mod(Interf%Real_Part(Ns)*1.d+08,128.d+00))
              do Kk = 1, Ks
                 Br = prnd()
              end do
              Br     = Brs*grnd(0)
              Br2sum = Br2sum + Br**2
              Interf%Real_Part(Ns) = Interf%Real_Part(Ns) + Br
           end do
           Brsig = dsqrt( Br2sum/dble(Interf%N_Sample-1) )
           write(*,*) ' Detection : Std Applied Noise ',Brsig,' Amperes'
           Brsig = Brsig * Detection%Rcr
           write(*,*) ' Detection : Std Applied Noise ',Brsig,' Volts'
        else
           write(*,*) ' Detection : No Noise Applied'
        end if
     end if
!
!    Volts conversion
     Interf%Real_Part(1:Interf%N_Sample) =                         &
            ( Interf%Real_Part(1:Interf%N_Sample) * Detection%Rcr )&
            + Detection%Vdc
!
     return
   end subroutine interf_detection
!
!
end module detection_module
