!!#   spike_detection_module.f90 --
!!# 
!!#            Project: SPS_GENERIC
!!#            Authors: NOVELTIS/B.TOURNIER
!!#               Date: march 2011
!!#            Version: $Revision: 1.3 $
!!#  Last modification: $Date: 2011-11-02 14:57:50 $
!!#
!!#  Language:  F90
!!#  Standards: Noveltis
!!#
!!# --
!!#
!!
!>  spike detection -- Module
!!
!! * Purpose
!!
!!   Module for Spike detection.
!!
!! * Description
!!      
!!   The objective of this module is to initialise and detect spike
!!
!! * Sub-routines and functions
!!
!! -  spike_detection_init      : spike detection initialisation.
!! -  spike_detection           : spike detection
!! -  interf_triplet_spike      : spike detection on CS, BB and EW targets
!! -  interf_quadruplet_spike   : spike detection on CS, BB, EW1 and EW2 targets
!! * References
!!
!!     SPS ATBD
!!


module spike_detection_module
   use precision_type
   use error_type
   use spike_detection_type
   use interferogram_type
   use zpd_type
!
   implicit none
!
!
   public ::                          &
             interf_triplet_spike,    &
             spike_detection,         &
             spike_detection_init
!
   contains
!
!

!
!
!>  interf_triplet_spike -- Public
!!
!! * Purpose
!!
!!     spike detection on triplet target
!!
!! * Description
!!    
!!     spike detection is applied on CS BB and EW targets.
!!
!! * Inputs 
!!
!!     - Zpd_CS    : type ZPD type for declaration and allocation of ZPD of CS interferogram
!!     - Zpd_BB    : type ZPD type for declaration and allocation of ZPD of BB interferogram
!!     - Zpd_EW    : type ZPD type for declaration and allocation of ZPD of EW interferogram
!!     - Interf_CS : type Interferogram / type for declaration and allocation of CS interferogram
!!     - Interf_BB : type Interferogram / type for declaration and allocation of BB interferogram
!!     - Interf_EW : type Interferogram / type for declaration and allocation of EW interferogram
!!
!! * Inputs-Outputs
!!
!!     - Spike_Detect : type_Spike_Detection / type for declaration and allocation of spike detection
!!     - Interf_Filt_CS : type Interferogram / type for declaration and allocation of CS filtered interferogram
!!     - Interf_Filt_BB : type Interferogram / type for declaration and allocation of BB filtered interferogram
!!     - Interf_Filt_EW : type Interferogram / type for declaration and allocation of EW filtered interferogram
!!
!! * References
!!   SPS ATBD
!!
   subroutine interf_triplet_spike( Spike_Detect, &
                                   Zpd_CS,          &
                                   Zpd_BB,          &
                                   Zpd_EW,          &
                                   Interf_CS,       &
                                   Interf_BB,       &
                                   Interf_EW,       &
                                   Interf_Filt_CS,  &
                                   Interf_Filt_BB,  &
                                   Interf_Filt_EW   )
  implicit none
    type(type_Spike_Detection),intent(inout)             :: Spike_Detect
    type(type_Zpd)            ,intent(in)                :: Zpd_CS
    type(type_Zpd)            ,intent(in)                :: Zpd_BB
    type(type_Zpd)            ,intent(in)                :: Zpd_EW
    type(type_Interferogram)  ,intent(in)                :: Interf_CS
    type(type_Interferogram)  ,intent(in)                :: Interf_BB
    type(type_Interferogram)  ,intent(in)                :: Interf_EW
    type(type_Interferogram)  ,intent(inout)             :: Interf_Filt_CS
    type(type_Interferogram)  ,intent(inout)             :: Interf_Filt_BB
    type(type_Interferogram)  ,intent(inout)             :: Interf_Filt_EW
!
!   Cold Space spike detection
    call spike_detection( Spike_Detect,&
                          Zpd_CS,         &
                          Interf_CS,      &
                          Interf_Filt_CS  )
!
!   Black Body spike detection
    call spike_detection( Spike_Detect,&
                          Zpd_BB,         &
                          Interf_BB,      &
                          Interf_Filt_BB  )
!
!   Earth View spike detection
    call spike_detection( Spike_Detect,&
                          Zpd_EW,         &
                          Interf_EW,      &
                          Interf_Filt_EW  )
!
    return
  end subroutine interf_triplet_spike
!
!
!> spike_detection -- Public
!!
!! * Purpose
!!
!!     spike detection
!!
!! * Description
!!    
!!    Spikes produce signal outside the bandwith of interest. This subroutine convolves   
!!    the interferogram by a high-pass filter that removes the useful signal.
!!    The amplitude of the remaining signal for each sample is compared to a given threshold
!!    and if the amplitude is greater than this threshold, a spike is detected and the
!!    interferogram is flagged. 
!!
!! * Inputs 
!!
!!     - Zpd    : type ZPD type for declaration and allocation of ZPD
!!     - Interf : type Interferogram / type for declaration and allocation of interferogram
!!
!! * Inputs-Outputs
!!
!!     - Spike_Detect : type_Spike_Detection / type for declaration and allocation of spike detection
!!     - Interf_Filtered : type Interferogram / type for declaration and allocation of filtered interferogram
!!
!! * References
!!   SPS ATBD
!!
   subroutine spike_detection( Spike_Detect, &
                               Zpd,             &
                               Interf,          &
                               Interf_Filtered  )
     implicit none
     type(type_Spike_Detection),intent(inout)           :: Spike_Detect
     type(type_Zpd)            ,intent(in)              :: Zpd
     type(type_Interferogram)  ,intent(in)              :: Interf
     type(type_Interferogram)  ,intent(inout)           :: Interf_Filtered
     integer(kind=LONG)                                 :: Ns
!
     if( Interf%Type /= 'R' ) then
        write(*,*) 'Interf%Type Error',Interf%Type
        call exit(1)
     end if
     Interf_Filtered%N_Sample = Interf%N_Sample
     Interf_Filtered%Type     = Interf%Type
     call alloc_Interferogram( Interf_Filtered )
     Interf_Filtered = Interf
     Interf_Filtered%Real_Part(1:Interf_Filtered%N_Sample) = 0.d+00
!
!    Counters initialisation
     Spike_Detect%Count_CF  = 0
     Spike_Detect%Count_CFO = 0
!
!    Central fringe location
     Spike_Detect%Ns_Width_CF = 2 *idnint( Spike_Detect%Opd_Width_CF &
                                            / Interf%dOpd ) + 1
     Spike_Detect%Ns_First_CF = Zpd%NZpd - Spike_Detect%Ns_Width_CF/2
     Spike_Detect%Ns_Last_CF  = Zpd%NZpd + Spike_Detect%Ns_Width_CF/2
     if( Spike_Detect%Ns_First_CF < 1+Spike_Detect%NTaps_CF/2 ) then
        write(*,*) 'Spike_Detect%Ns_First_CF Error', &
                    Spike_Detect%Ns_First_CF
        call exit(1)
     end if
     if( Spike_Detect%Ns_Last_CF > &
                         Interf%N_Sample-Spike_Detect%NTaps_CF/2 ) then
        write(*,*) 'Spike_Detect%Ns_Last_CF Error', &
                    Spike_Detect%Ns_Last_CF
        call exit(1)
     end if
!
!    Central fringe filtering
     do Ns = Spike_Detect%Ns_First_CF, Spike_Detect%Ns_Last_CF
        Interf_Filtered%Real_Part(Ns) =                             &
          sum( Interf%Real_Part( Ns-Spike_Detect%NTaps_CF/2      &
                                :Ns+Spike_Detect%NTaps_CF/2)     &
             * Spike_Detect%Taps_CF(-Spike_Detect%NTaps_CF/2  &
                                       :Spike_Detect%NTaps_CF/2) )
        if( dabs(Interf_Filtered%Real_Part(Ns)) > &
                        Spike_Detect%Cutoff_CF ) then
           Spike_Detect%Count_CF = Spike_Detect%Count_CF + 1
           write(*,'(a,i10,f10.5)') 'Spike in CF',&
                                     Ns,Interf_Filtered%Real_Part(Ns)
        end if
     end do
!
!    Before Central fringe filtering
     do Ns = 1+Spike_Detect%NTaps_CFO/2, Spike_Detect%Ns_First_CF-1
        Interf_Filtered%Real_Part(Ns) =                               &
          sum( Interf%Real_Part( Ns-Spike_Detect%NTaps_CFO/2       &
                                :Ns+Spike_Detect%NTaps_CFO/2)      &
             * Spike_Detect%Taps_CFO(-Spike_Detect%NTaps_CFO/2  &
                                        :Spike_Detect%NTaps_CFO/2) )
        if( dabs(Interf_Filtered%Real_Part(Ns)) > &
                       Spike_Detect%Cutoff_CFO ) then
           Spike_Detect%Count_CFO = Spike_Detect%Count_CFO + 1
           write(*,'(a,i10,f10.5)') 'Spike Left CF',&
                                     Ns,Interf_Filtered%Real_Part(Ns)
        end if
     end do
!
!    After Central fringe filtering
     do Ns = Spike_Detect%Ns_Last_CF+1, &
             Interf%N_Sample-Spike_Detect%NTaps_CFO/2
        Interf_Filtered%Real_Part(Ns) =                               &
          sum( Interf%Real_Part( Ns-Spike_Detect%NTaps_CFO/2       &
                                :Ns+Spike_Detect%NTaps_CFO/2)      &
             * Spike_Detect%Taps_CFO(-Spike_Detect%NTaps_CFO/2  &
                                        :Spike_Detect%NTaps_CFO/2) )
        if( dabs(Interf_Filtered%Real_Part(Ns)) > &
                       Spike_Detect%Cutoff_CFO ) then
           Spike_Detect%Count_CFO = Spike_Detect%Count_CFO + 1
           write(*,'(a,i10,f10.5)') 'Spike right CF',&
                                     Ns,Interf_Filtered%Real_Part(Ns)
        end if
     end do
!
!    Flag setting
     if( (Spike_Detect%Count_CF  /= 0) .or. &
         (Spike_Detect%Count_CFO /= 0) ) then
        Spike_Detect%Flag_Spike = 0
        write(*,'(a,i10)') 'Spike_Detect%Count_CF  ', &
                            Spike_Detect%Count_CF
        write(*,'(a,i10)') 'Spike_Detect%Count_CFO ', &
                            Spike_Detect%Count_CFO
     else
        Spike_Detect%Flag_Spike = 1
        write(*,'(a)') 'No Spike detected'
     end if
!
     return
   end subroutine spike_detection
!
!
!> spike_detection_init -- Public
!!
!! * Purpose
!!
!!     Initialisation of the spike detection filter     
!!
!! * Description
!!
!!     In function of the path which must be "MES" or "RPD", the parameters
!!     defining the type_Saf are initialized 
!!
!! * Inputs 
!!
!!     - file_spike_detection : file with parameters of spike detection 
!!
!! * Outputs
!!
!!     - Spike_Detection : type_Spike_Detection / type for declaration and allocation of spike detection
!!
!! * References
!!   SPS ATBD
!!
   subroutine spike_detection_init( file_spike_detection, &
                                    Spike_Detect       )
     implicit none
     character(len=*)          ,intent(in)             :: file_spike_detection
     type(type_Spike_Detection),intent(inout)          :: Spike_Detect
     integer(kind=LONG)                                :: iFile
     integer(kind=LONG)                                :: iPos
     integer(kind=LONG)                                :: NTaps
!
     iFile = 10
     iPos = 1
     write(*,'(a)') 'file_spike_detection',&
                     file_spike_detection(1:len_trim(file_spike_detection))
     open(unit=iFile, file=file_spike_detection, status='old', err=999)
     iPos = 2
     read(iFile,*,err=999)
     read(iFile,*,err=999) Spike_Detect%NTaps_CF
     read(iFile,*,err=999) Spike_Detect%NTaps_CFO
     read(iFile,*,err=999) Spike_Detect%Opd_Width_CF
     call alloc_Spike_Detection( Spike_Detect )
     do NTaps = -Spike_Detect%NTaps_CF/2, Spike_Detect%NTaps_CF/2
        read(iFile,*,err=999) Spike_Detect%Taps_CF(NTaps)
     end do
     do NTaps = -Spike_Detect%NTaps_CFO/2, Spike_Detect%NTaps_CFO/2
        read(iFile,*,err=999) Spike_Detect%Taps_CFO(NTaps)
     end do
     read(iFile,*,err=999) Spike_Detect%Cutoff_CF
     read(iFile,*,err=999) Spike_Detect%Cutoff_CFO
     close(unit=iFile)
     return
 999 write(*,*) 'spike_detection_init Reading Error',iPos
     call exit(1)
   end subroutine spike_detection_init
!
!
end module spike_detection_module
