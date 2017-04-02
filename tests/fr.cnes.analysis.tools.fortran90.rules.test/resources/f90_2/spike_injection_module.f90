!!#   spike_injection_module.f90 --
!!# 
!!#            Project: SPS_GENERIC
!!#            Authors: NOVELTIS/B.TOURNIER
!!#               Date: march 2011
!!#            Version: $Revision: 1.2 $
!!#  Last modification: $Date: 2011-11-02 14:57:50 $
!!#
!!#  Language:  F90
!!#  Standards: Noveltis
!!#
!!# --
!!#
!!
!>  spike_injection -- Module
!!
!! * Purpose
!!
!!   Module for spike injection.
!!
!! * Description
!!      
!!   The objective of this module is to initialise and apply spike injection type.
!!
!! * Sub-routines and functions
!!
!! -  spike_injection_init      : spike initialisation.
!! -  spike_injection           : spike injection.
!!
!! * References
!!
!!     SPS ATBD
!!


module spike_injection_module
   use precision_type
   use error_type
   use amplification_type
   use spike_injection_type
   use interferogram_type
   use math_module
!
   implicit none
!
!
   public :: spike_injection_init,      &
             spike_injection
!
   contains
!
!

!
!
!> spike_injection_init -- Public
!!
!! * Purpose
!!
!!     Initialisation of the spike injection filter
!!
!! * Description
!!    
!!   The subroutine reads spike injection parametres  
!!
!! * Inputs 
!!
!!     - file_spike_injection : filename with spike injection parametres
!!
!! * Outputs
!!
!!     - Spike_Inject : type Spike_Injection/ type for declaration and allocation of spike injection
!!
!! * References
!!   SPS ATBD
!!
   subroutine spike_injection_init( file_spike_injection, &
                                    Spike_Inject       )
     implicit none
     character(len=*)          ,intent(in)             :: file_spike_injection
     type(type_Spike_Injection),intent(inout)          :: Spike_Inject
     integer(kind=LONG)                                :: iFile
     integer(kind=LONG)                                :: iPos
     integer(kind=LONG)                                :: NSpike
!
     iFile = 10
     iPos = 1
     write(*,'(a)') 'file_spike_injection',&
                     file_spike_injection(1:len_trim(file_spike_injection))
     open(unit=iFile,file=file_spike_injection,status='old',err=999)
     iPos = 2
     read(iFile,*,err=999) 
     read(iFile,*,err=999) Spike_Inject%NSpike
     if( Spike_Inject%NSpike /= 0 ) then
        write(*,*) 'Spike injection'
        call alloc_Spike_Injection( Spike_Inject )
        do NSpike = 1, Spike_Inject%NSpike
           read(iFile,*,err=999) Spike_Inject%Amplitude(NSpike)
        end do
        do NSpike = 1, Spike_Inject%NSpike
           read(iFile,*,err=999) Spike_Inject%Duration(NSpike)
        end do
     else
        write(*,*) 'No spike injection'
     end if
     close(unit=iFile)
     return
 999 write(*,*) 'spike_injection_init Reading Error',iPos
     call exit(1)
   end subroutine spike_injection_init
!
!
!> spike_injection -- Public
!!
!! * Purpose
!!
!!     Simulates spike into interferogram
!!
!! * Description
!!    
!!    The subroutine simulates spikes defined by their Duration and Amplitude. The first sample Ns_First 
!!    of the interferogram affected by spike is calculated by using a Gaussian random function. The spike
!!    is injected as the ratio Amplitude of the amplication V_Max weighted by a exponentail negative quadratic 
!!    term on the spike width.
!!
!! * Inputs 
!!
!!     - Spike_Inject : type Spike_Injection/ type for declaration and allocation of spike injection
!!     - Amplification : type Amplification/ type for declaration and allocation of interferogram amplification
!!
!! * Inputs-Outputs
!!
!!     - Interf : type Interferogram/ type for declaration and allocation of interferogram  
!!
!! * References
!!   SPS ATBD
!!   
   subroutine spike_injection( Spike_Inject, &
                               Amplification,&
                               Interf        )
     implicit none
     type(type_Spike_Injection),intent(in)              :: Spike_Inject
     type(type_Amplification)  ,intent(in)              :: Amplification
     type(type_Interferogram)  ,intent(inout)           :: Interf
     integer(kind=LONG)                                 :: NSpike
     integer(kind=LONG)                                 :: Ns_First
     integer(kind=LONG)                                 :: Ns_Last
     integer(kind=LONG)                                 :: Ns
     real(kind=DOUBLE)  ,dimension(:),allocatable       :: Pattern
     real(kind=DOUBLE)  ,dimension(:),allocatable       :: Spike
!
     if( Spike_Inject%NSpike /= 0 ) then
        write(*,*) 'Spike injection'
        if( Interf%Type /= 'R' ) then
           write(*,*) 'Interf%Type Error',Interf%Type
           call exit(1)
        end if
!
!       Spike loop
        do NSpike = 1, Spike_Inject%NSpike
!
!          spike start call exit(1) position
           Ns_First = idnint( 2.d+00 * Interf%TimeMax * prnd() &
                            / Interf%dTime ) + 1
           if( Ns_First < 1 ) Ns_First = 1
           Ns_Last  = Ns_First &
                    + idnint( Spike_Inject%Duration(NSpike)/Interf%dTime )
           if( Ns_Last > Interf%N_Sample ) Ns_Last = Interf%N_Sample
           allocate( Pattern(Ns_Last-Ns_First+1) )
           allocate( Spike(Ns_Last-Ns_First+1) )
           Pattern(1:Ns_Last-Ns_First+1) = Spike_Inject%Amplitude(NSpike) &
                  * (/ (dexp(-dble(Ns-Ns_Last)**2),Ns=Ns_Last,Ns_First,-1) /)
           Spike(1:Ns_Last-Ns_First+1) = Amplification%V_Max &
                                       * Pattern(1:Ns_Last-Ns_First+1)
           Interf%Real_Part(Ns_First:Ns_Last) = &
           Interf%Real_Part(Ns_First:Ns_Last) + &
                      Spike(1:Ns_Last-Ns_First+1)
           write(*,*) 'Interf%TimeMax ',Interf%TimeMax
           write(*,*) 'Interf%dTime   ',Interf%dTime
           write(*,*) 'Interf%N_Sample',Interf%N_Sample
           write(*,*) 'First Last Duration Amplitude',   &
                       Ns_First, Ns_Last,                &
                       Spike_Inject%Duration(NSpike), &
                       Spike_Inject%Amplitude(NSpike)
           write(*,*) 'Spike ',Spike(1:Ns_Last-Ns_First+1)
        end do ! end spike loop
        deallocate( Pattern )
        deallocate( Spike )
     else
        write(*,*) 'No spike injection'
     end if
!
     return
   end subroutine spike_injection
!
!
end module spike_injection_module
