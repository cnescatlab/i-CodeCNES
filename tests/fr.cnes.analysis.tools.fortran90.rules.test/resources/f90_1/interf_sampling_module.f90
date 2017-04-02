!!#  interf_sampling_module.f90 --
!!# 
!!#            Project: SPS_GENERIC
!!#            Authors: NOVELTIS/B.TOURNIER
!!#               Date: october 2009
!!#            Version: $Revision: 1.16 $
!!#  Last modification: $Date: 2012-02-08 10:19:14 $
!!#
!!#  Language:  F90
!!#  Standards: Noveltis
!!#
!!# --
!!#
!!
!> interf_sampling_module -- Module
!!
!! * Purpose
!!
!!   Module for interferogram sampling.
!!
!! * Description
!!      
!!   This module samples RPD and target interferograms. 
!!   The target continuous interferogram is sampled by a clock or a laser RPD: the temporal 
!!   basis includes the sampling jitter and delay. The detection chain noise is also applied 
!!
!! * Sub-routines and functions
!!
!! -  sampling_clock          : interferogram sampling by a clock
!! -  sampling_rpd_mes        : interferogram sampling by a laser RPD
!! -  signal_integration_time : integration of the signal at constant time 
!! -  signal_integration_opd  : integration of the signal at constant opd 
!! -  opd_samp                : conversion of the time scale into OPD scale
!! -  rpd_zero_crossing       : determination of a temporal sequence thanks to zero crossing 
!!                              of the laser RPD interferogram.
!! -  add_sampling_jitter     : addition of a sampling jitter 
!! -  add_interf_samp_noise   : addition of a Gaussian random radiometric noise to the sampled 
!!                              interferograma sampling jitter 
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.4.2
!!


module interf_sampling_module
   use precision_type
   use error_type
   use constantes_type
   use interf_param_type
   use interferogram_type
   use plancklib_module
   use math_module
!
   implicit none
!
!
   public ::                                 &
             sampling_clock,                 &
             opd_samp,                       &
             sampling_lzrpd,                 &
             rpd_zero_crossing,              &
             signal_integration_none,        &
             signal_integration_time,        &
             signal_integration_opd,         &
             signal_integration_differential,&
             recombine_differential,         &
             add_sampling_jitter,            &
             add_interf_samp_noise
!
   contains
!
!

!> sampling_clock -- Public
!!
!! * Purpose
!!
!!     Interferogram sampling by a clock.
!!
!! * Description
!!
!!     The sampled interferogram can be calculated with integration (by trapezes), in this case 
!!     the signal can be integrated at constant time or at constant OPD.
!!     More precisely, the interferogram computation requires the following steps:
!!       - Before the sampling basis computation, the first steps consist in the sampling 
!!         parameters selection, the time sampling start definition, and the sample number 
!!         computation.
!!       - Then the sampling basis is initialized and a sampling jitter is computed; 
!!       - Whatever the calibration or earth view targets, the interferogram sampling basis is 
!!         computed by integration at constant time, or by integration at OPD constant or at 
!!         last, by interferogram sampling without integration.
!!       - At last, a noise is added at the interferogram sampling basis
!!
!! * Inputs
!!
!!     - Path         : measured interferogram (RPD or MES)
!!     - Interf_Param : type_Interf_Param / type for declaration and allocation of interferogram 
!!                      parameters
!!     - Interf_cont : type_Interferogram / type for declaration and allocation of interferogram
!!
!! * Inputs/outputs
!!
!!     - Interf_Samp : type_Interferogram / type for declaration and allocation of interferogram
!!
!! * Outputs
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.4.2.5.1
!!

   subroutine sampling_clock( Path,        &
                              Interf_Param,&
                              Interf_Cont, &
                              Interf_Samp  )
   implicit none
     character(LEN=3)        , intent(in)                :: Path
     type(type_Interf_Param) , intent(in)                :: Interf_Param
     type(type_Interferogram), intent(in)                :: Interf_Cont
     type(type_Interferogram), intent(out)               :: Interf_Samp
     real(kind=DOUBLE)                                   :: T_Start
     real(kind=DOUBLE)                                   :: T_Stop
     integer(kind=LONG)                                  :: NSample
     integer(kind=LONG)                                  :: Ns
     real(kind=DOUBLE)                                   :: Time
     real(kind=DOUBLE)                                   :: Delay
     real(kind=DOUBLE)                                   :: Samp_Jitter
     real(kind=DOUBLE)                                   :: dT_Samp
     real(kind=DOUBLE)                                   :: Noise_Std
!
!    select sampling params
     if(      Path == 'RPD' ) then
        dT_Samp     = Interf_Param%dT_Rpd_Samp
        Delay       = Interf_Param%Rpd_Delay
        Samp_Jitter = Interf_Param%Rpd_Samp_Jitter
        Noise_Std   = Interf_Param%Noise_Rpd_Std
        Interf_Samp%Type = Interf_Param%Rpd_Type
     else if( Path == 'MES' ) then
        dT_Samp     = Interf_Param%dT_Mes_Samp
        Delay       = Interf_Param%Mes_Delay
        Samp_Jitter = Interf_Param%Mes_Samp_Jitter
        Noise_Std   = 0.0d+00
        Interf_Samp%Type = Interf_Param%Type
     else
        write(*,*) 'error PATH : must be RPD or MES',Path
        write(*,*) 'sampling_clock Fatal Error'
        call exit(1)
     end if
     write(*,*) 'sampling_clock'
     write(*,*) 'Path             ',Path
     write(*,*) 'dT_Samp          ',dT_Samp
     write(*,*) 'Delay            ',Delay
     write(*,*) 'Samp_Jitter      ',Samp_Jitter
     write(*,*) 'Noise_Std        ',Noise_Std
     write(*,*) 'Interf_Samp%Type ',Interf_Samp%Type
!
!    time sampling start
     T_Start = Interf_Cont%Time(1)&
             + 2*Interf_Param%dT_Mes_Samp&
             + Delay
     T_Stop  = Interf_Cont%Time(Interf_Cont%N_Sample)&
             - 2*Interf_Param%dT_Mes_Samp&
             + Delay
!
!    Sample number computation
     NSample = 1
     Time = T_Start
     write(*,*) 'Interf_Cont%N_Sample',Interf_Cont%N_Sample
     write(*,*) 'Interf_Cont%Time(1) ',Interf_Cont%Time(1)
     write(*,*) 'Interf_Cont%Time(Interf_Cont%N_Sample)'&
                ,Interf_Cont%Time(Interf_Cont%N_Sample)
     do while( Time <  T_Stop )
       NSample = NSample + 1
       Time = Time + dT_Samp
     end do
     if( NSample >= Interf_Cont%N_Sample ) then
        write(*,*) 'error NSample',Interf_Cont%N_Sample,NSample
        write(*,*) 'sampling_clock Fatal Error'
        call exit(1)
     end if
!
!    sampling basis computation
     Interf_Samp%N_Sample = NSample
     Interf_Samp%dTime    = dT_Samp
     Interf_Samp%TimeMax  = Interf_Samp%dTime               &
                          * dble(int(Interf_Samp%N_Sample/2))
     Interf_Samp%dOpd     = dT_Samp * 2.d+00 * Interf_Param%VCC
     Interf_Samp%OpdMax   = Interf_Samp%dOpd                &
                          * dble(int(Interf_Samp%N_Sample/2))
     Interf_Samp%FieldMeanAngle = Interf_Cont%FieldMeanAngle
     call alloc_Interferogram( Interf_Samp )
     Interf_Samp%Time(1:Interf_Samp%N_Sample) =                        &
              T_Start + ( (/ (dble(Ns-1),Ns=1,Interf_Samp%N_Sample) /) &
                          * dT_Samp )
     Interf_Samp%Opd(1:Interf_Samp%N_Sample) =                 &
                      Interf_Samp%Time(1:Interf_Samp%N_Sample) &
                      * (2.d+00 * Interf_Param%VCC)
     Interf_Samp%N_Top = 0
     Interf_Samp%Time_Top(1:Interf_Samp%N_Sample) = 0.d+00
     Interf_Samp%Opd_Top(1:Interf_Samp%N_Sample)  = 0.d+00
     write(*,*) 'Interf_Samp%N_Sample',Interf_Samp%N_Sample
     write(*,*) 'Interf_Samp%Time(1) ',Interf_Samp%Time(1)
     write(*,*) 'Interf_Samp%Time(Interf_Samp%N_Sample)'&
                ,Interf_Samp%Time(Interf_Samp%N_Sample)
!
!    sampling jitter computation
     call add_sampling_jitter( Samp_Jitter,&
                               Interf_Samp )
     if( Path == 'RPD' ) then
!
!       interferogram sampling without integration
        write(*,*) ' signal RPD no integration'
        call signal_integration_none( Interf_Cont, &
                                      Interf_Samp  )
     else if( (Path == 'MES') ) then
        select case ( Interf_Param%Integration_Mode               &
                      (1:len_trim(Interf_Param%Integration_Mode)) )
        case ( 'Time_Cst' )
!
!          Integration at constant time
           write(*,*) ' signal integration constant time'
           call signal_integration_time( Interf_Param,&
                                         Interf_Cont, &
                                         Interf_Samp  )
        case ( 'Opd_Cst' )
!
!          integration at OPD constant
           write(*,*) ' signal integration constant opd'
           call signal_integration_opd( Interf_Param,&
                                        Interf_Cont, &
                                        Interf_Samp  )
        case ( 'Differe' )
!
!          differential integration at OPD constant
           write(*,*) ' signal integration differential'
           call signal_integration_differential( Interf_Param,&
                                                 Interf_Cont, &
                                                 Interf_Samp  )
           write(*,*) ' signal integration differential'
        case ( 'None' )
!
!          interferogram sampling without integration
           write(*,*) ' signal no integration'
           call signal_integration_none( Interf_Cont, &
                                         Interf_Samp  )
        case default
           write(*,*) 'Integration Mode Error : ',&
                       Interf_Param%Integration_Mode
           call exit(1)
        end select
     else
        write(*,*) 'Path Error must be RPD or MES : ',Path
        call exit(1)
     end if
!
!    interferogram noise introduction
     if( Noise_Std /= 0.d+00 ) then
        call add_interf_samp_noise( Noise_Std,  &
                                    Interf_Samp )
     end if
     return
!
   end subroutine sampling_clock
!
!

!> opd_samp -- Public
!!
!! * Purpose
!!
!!     Conversion of the time scale into OPD scale
!!
!! * Description
!!
!!     The OPD sampling computation requires the following steps:
!!      - First of all, after required tables allocation, phase computation and regularisation, 
!!        the phase is determined.
!!      - Then, the interferogram fringe are defined and relative OPD are computed
!!
!! * Inputs
!!
!!     - Interf_Param : type_Interf_Param / type for declaration and allocation of 
!!                      interferogram parameters
!!
!! * Inputs/outputs
!!
!!     - Interf_Samp_Rpd  : type_Interferogram / type for declaration and allocation 
!!                          of interferogram
!!
!! * Outputs
!!
!! * References
!!

   subroutine opd_samp( Interf_Param,   &
                        Interf_Samp_Rpd )
   implicit none
     type(type_Interf_Param)  ,intent(in)                :: Interf_Param
     type(type_Interferogram) ,intent(inout)             :: Interf_Samp_Rpd
     real(kind=DOUBLE)        ,dimension(:) ,allocatable :: Phase
     real(kind=DOUBLE)                                   :: Phase0
     integer(kind=LONG)                                  :: Ns
     integer(kind=LONG)                                  :: ErrCode
!
     write(*,*) 'opd_samp'
     if( Interf_Samp_Rpd%type /= 'RI' ) then
        write(*,*) ' Interf_Samp_Rpd%type must be RI',Interf_Samp_Rpd%type
        call exit(1)
     end if
!
!    phase computation
     allocate( Phase(Interf_Samp_Rpd%N_Sample) , stat=ErrCode )
!
     if (ErrCode > 0) then
        write(0,*) 'allocation Phase Error'
        write(0,*) 'opd_samp: fatal error'
        call exit(1)
     end if
!
     Phase(1:Interf_Samp_Rpd%N_Sample) =                                  &
            datan2( Interf_Samp_Rpd%Imag_Part(1:Interf_Samp_Rpd%N_Sample),&
                    Interf_Samp_Rpd%Real_Part(1:Interf_Samp_Rpd%N_Sample) )
     call phasemono( Phase, Interf_Samp_Rpd%N_Sample )
     Phase0 = Phase(1)
     Phase(1:Interf_Samp_Rpd%N_Sample) = Phase(1:Interf_Samp_Rpd%N_Sample)&
                                       - Phase0
     write(*,*) 'Interf_Samp_Rpd%N_Sample   ',Interf_Samp_Rpd%N_Sample
     write(*,*) 'Interf_Param%LambdaLaserRpd',Interf_Param%LambdaLaserRpd
!
!    Fringe counting and OPD computation
     Interf_Samp_Rpd%N_Top = 0
     do Ns = 1,Interf_Samp_Rpd%N_Sample-1
        if( (Phase(Ns) <= Pi*dble(Interf_Samp_Rpd%N_Top)) .and. &
            (Pi*dble(Interf_Samp_Rpd%N_Top) < Phase(Ns+1)) ) then
           Interf_Samp_Rpd%N_Top = Interf_Samp_Rpd%N_Top + 1
           Interf_Samp_Rpd%Time_Top(Interf_Samp_Rpd%N_Top) =          &
                  Interf_Samp_Rpd%Time(Ns)                            &
                  + ( ( Pi*dble(Interf_Samp_Rpd%N_Top-1) - Phase(Ns) )&
                      * ( Interf_Samp_Rpd%Time(Ns+1)                  &
                         -Interf_Samp_Rpd%Time(Ns) )                  &
                      / ( Phase(Ns+1)-Phase(Ns) ) )
           Interf_Samp_Rpd%Opd_Top(Interf_Samp_Rpd%N_Top) =        &
                   Interf_Samp_Rpd%Time_Top(Interf_Samp_Rpd%N_Top) &
                   * 2.d+00 * Interf_Param%VCC
           if( (Ns > Interf_Samp_Rpd%N_Sample-50) .or. &
               (Ns < 50)   ) then
              write(*,'(a,i8,6f9.6,2e14.6)') 'Rpd Top', &
                         Interf_Samp_Rpd%N_Top,   &
                         Interf_Samp_Rpd%Time(Ns),&
                         Interf_Samp_Rpd%Time_Top(Interf_Samp_Rpd%N_Top),&
                         Interf_Samp_Rpd%Time(Ns+1),&
                         Interf_Samp_Rpd%Opd(Ns),   &
                         Interf_Samp_Rpd%Opd_Top(Interf_Samp_Rpd%N_Top),&
                         Interf_Samp_Rpd%Opd(Ns+1), &
                         Phase(Ns),Phase(Ns+1)
           end if
        end if
        Interf_Samp_Rpd%Opd(Ns) = Interf_Param%LambdaLaserRpd / 2.d+00 &
                                * ( Phase(Ns) / Pi )
     end do
     Ns = Interf_Samp_Rpd%N_Sample
     Interf_Samp_Rpd%Opd(Ns) = Interf_Param%LambdaLaserRpd / 2.d+00 &
                             * ( Phase(Ns) / Pi )
     deallocate( Phase )
     write(*,*) 'Interf_Samp_Rpd%N_Top          ',&
                 Interf_Samp_Rpd%N_Top
     write(*,*) 'Interf_Samp_Rpd%Time_Top(1)    ',&
                 Interf_Samp_Rpd%Time_Top(1)
     write(*,*) 'Interf_Samp_Rpd%Time_Top(N_Top)',&
                 Interf_Samp_Rpd%Time_Top(Interf_Samp_Rpd%N_Top)
     write(*,*) 'Interf_Samp_Rpd%Opd_Top(1)     ',&
                 Interf_Samp_Rpd%Opd_Top(1)
     write(*,*) 'Interf_Samp_Rpd%Opd_Top(N_Top) ',&
                 Interf_Samp_Rpd%Opd_Top(Interf_Samp_Rpd%N_Top)
     return
!
   end subroutine opd_samp
!
!

!> sampling_lzrpd -- Public
!!
!! * Purpose
!!
!!     Interferogram sampling by a laser RPD
!!
!! * Description
!!
!!     The target continuous interferogram is sampled by a temporal sequence defined by the 
!!     zero crossing of the laser RPD interferogram. The sampled interferogram is calculated 
!!     with or without integration. In the case of integration (by trapezes), the duration is 
!!     at constant time or at constant OPD.
!!     
!!     More precisely, the interferogram computation requires the following steps:
!!       - The first steps consists in the transfer of the RPD top basis towards the measured
!!         interferogram.
!!       - Then the interferogram sampling basis is computed by integration at constant time,
!!         or by integration at OPD constant or at last, by interferogram sampling without 
!!         integration.
!!
!! * Inputs
!!
!!     - Interf_Param : type_Interf_Param / type for declaration and allocation of interferogram
!!                      parameters
!!     - Interf_Cont : type_Interferogram / type for declaration and allocation of interferogram
!!
!! * Inputs/outputs
!!
!!     - Interf_Samp_Rpd : type_Interferogram / type for declaration and allocation of 
!!                         interferogram
!!
!! * Outputs
!!
!!     - Interf_Samp : type_Interferogram / type for declaration and allocation of interferogram
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.4.2.5.2
!!

   subroutine sampling_lzrpd( Interf_Param,   &
                              Interf_Samp_Rpd,&
                              Interf_Cont,    &
                              Interf_Samp     )
   implicit none
     type(type_Interf_Param) , intent(in)                :: Interf_Param
     type(type_Interferogram), intent(inout)             :: Interf_Samp_Rpd
     type(type_Interferogram), intent(in)                :: Interf_Cont
     type(type_Interferogram), intent(out)               :: Interf_Samp
     integer(kind=LONG)                                  :: Top_Start
     integer(kind=LONG)                                  :: Top_Stop
     real(kind=DOUBLE)                                   :: dT_Integ
!
!    Limits verification
     dT_Integ = Interf_Param%LambdaLaserRpd / 2.d+00 &
              / (2.d+00*Interf_Param%VCC)
     Top_Start = 1
     do while( ( Interf_Samp_Rpd%Time_Top(Top_Start)-(dT_Integ/2.d+00) ) < &
               Interf_Cont%Time(1) )
        Top_Start = Top_Start + 1
     end do
     Top_Stop  = Interf_Samp_Rpd%N_Top
     do while( ( Interf_Samp_Rpd%Time_Top(Top_Stop)+(dT_Integ/2.d+00) ) > &
               Interf_Cont%Time(Interf_Cont%N_Sample) )
        Top_Stop = Top_Stop - 1
     end do
     write(*,*) 'Top_Start,Top_Stop ',Top_Start,Top_Stop
!
!    Top basis transfert
     Interf_Samp%N_Sample = Top_Stop - Top_Start + 1
     Interf_Samp%N_Top    = Top_Stop - Top_Start + 1
     Interf_Samp%Type     = 'R'
     Interf_Samp%dOpd     = Interf_Param%LambdaLaserRpd / 2.d+00
     Interf_Samp%dTime    = Interf_Samp%dOpd        &
                          / (2.d+00*Interf_Param%VCC)
     Interf_Samp%OpdMax   = Interf_Samp%dOpd                &
                          * dble(int(Interf_Samp%N_Sample)/2)
     Interf_Samp%TimeMax  = Interf_Samp%dTime               &
                          * dble(int(Interf_Samp%N_Sample)/2)
     Interf_Samp%FieldMeanAngle = Interf_Cont%FieldMeanAngle
     call alloc_Interferogram( Interf_Samp )
     Interf_Samp%Time(1:Interf_Samp%N_Sample) =              &
                 Interf_Samp_Rpd%Time_Top(Top_Start:Top_Stop)&
               + Interf_Param%Mes_Delay
     Interf_Samp%Opd(1:Interf_Samp%N_Sample) =              &
                  Interf_Samp_Rpd%Opd_Top(Top_Start:Top_Stop)
     Interf_Samp%Time_Top(1:Interf_Samp%N_Sample) =          &
                 Interf_Samp_Rpd%Time_Top(Top_Start:Top_Stop)&
               + Interf_Param%Mes_Delay
     Interf_Samp%Opd_Top(1:Interf_Samp%N_Sample) =          &
                  Interf_Samp_Rpd%Opd_Top(Top_Start:Top_Stop)
!
     write(*,*) 'Interf_Samp_Rpd%Time_Top(Top_Start) ', &
                 Interf_Samp_Rpd%Time_Top(Top_Start)
     write(*,*) 'Interf_Samp_Rpd%Time_Top(Top_Stop)  ', &
                 Interf_Samp_Rpd%Time_Top(Top_Stop)
     write(*,*) 'Interf_Samp%Time(1)                 ', &
                 Interf_Samp%Time(1)
     write(*,*) 'Interf_Samp%Time(N_Sample)          ', &
                 Interf_Samp%Time(Interf_Samp%N_Sample)
!
!    Interferogram sampling
     select case ( Interf_Param%Integration_Mode               &
                   (1:len_trim(Interf_Param%Integration_Mode)) )
     case ( 'Time_Cst' )
!
!       Integration at constant time
        write(*,*) ' signal integration constant time'
        call signal_integration_time( Interf_Param,&
                                      Interf_Cont, &
                                      Interf_Samp  )
     case ( 'Opd_Cst' )
!
!       integration at constant OPD
        write(*,*) ' signal integration constant opd'
        call signal_integration_opd( Interf_Param,&
                                     Interf_Cont, &
                                     Interf_Samp  )
     case ( 'None' )
!
!       integration none
        write(*,*) ' signal no integration'
        call signal_integration_none( Interf_Cont, &
                                      Interf_Samp  )
     case default
        write(*,*) ' Error Integration Mode',Interf_Param%Integration_Mode
        write(*,*) 'sampling_lzrpd Fatal Error'
        call exit(1)
     end select
     return
!
   end subroutine sampling_lzrpd
!
!

!> rpd_zero_crossing -- Public
!!
!! * Purpose
!!
!!     Determination of a temporal sequence thanks to zero crossing of the laser RPD 
!!     interferogram.
!!
!! * Description
!!
!!     The measured interferogram is sampled thanks to interferogram linear interpolation
!!     at each tops RPD; The tops RPD are defined as the zero crossing of the laser RPD 
!!     interferogram.
!!
!! * Inputs
!!
!!     - Interf_Param : type_Interf_Param / type for declaration and allocation of 
!!                      interferogram parameters
!!
!! * Inputs/outputs
!!
!!     - Interf_Samp_Rpd  : type_Interferogram / type for declaration and allocation of 
!!                          interferogram
!!
!! * Outputs
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.4.2
!!

   subroutine rpd_zero_crossing( Interf_Param,   &
                                 Interf_Samp_Rpd )
   implicit none
     type(type_Interf_Param) , intent(in)                :: Interf_Param
     type(type_Interferogram), intent(inout)             :: Interf_Samp_Rpd
     integer(kind=LONG)                                  :: Ns
     real(kind=DOUBLE)       , dimension(:), allocatable :: X
     real(kind=DOUBLE)                                   :: XAvg
     real(kind=DOUBLE)                                   :: XStd
     real(kind=DOUBLE)                                   :: XMin
     real(kind=DOUBLE)                                   :: XMax
     integer(kind=LONG)                                  :: NsMin
     integer(kind=LONG)                                  :: NsMax
!
     write(*,*) 'rpd_zero_crossing'
!
!    TOPs RPD computation at each zero crossing (linear interpolation)
     Interf_Samp_Rpd%N_Top = 0
     do Ns = 1, Interf_Samp_Rpd%N_Sample-1
        if( (Interf_Samp_Rpd%Real_Part(Ns) <= 0.d+00) .and. &
            (0.d+00 < Interf_Samp_Rpd%Real_Part(Ns+1)) ) then
           Interf_Samp_Rpd%N_Top = Interf_Samp_Rpd%N_Top +1
           Interf_Samp_Rpd%Time_Top(Interf_Samp_Rpd%N_Top) = &
                        Interf_Samp_Rpd%Time(Ns)             &
                       - ( Interf_Samp_Rpd%Real_Part(Ns)     &
                         * ( Interf_Samp_Rpd%Time(Ns+1)      &
                            -Interf_Samp_Rpd%Time(Ns) )      &
                         / ( Interf_Samp_Rpd%Real_Part(Ns+1) &
                            -Interf_Samp_Rpd%Real_Part(Ns) ) )
           Interf_Samp_Rpd%Opd_Top(Interf_Samp_Rpd%N_Top) =  &
                           dble(Interf_Samp_Rpd%N_Top-1)     &
                           *Interf_Param%LambdaLaserRpd/2.d+00
        end if
        if( (Interf_Samp_Rpd%Real_Part(Ns) >= 0.d+00) .and. &
            (0.d+00 > Interf_Samp_Rpd%Real_Part(Ns+1)) ) then
           Interf_Samp_Rpd%N_Top = Interf_Samp_Rpd%N_Top +1
           Interf_Samp_Rpd%Time_Top(Interf_Samp_Rpd%N_Top) = &
                        Interf_Samp_Rpd%Time(Ns)             &
                       - ( Interf_Samp_Rpd%Real_Part(Ns)     &
                         * ( Interf_Samp_Rpd%Time(Ns+1)      &
                            -Interf_Samp_Rpd%Time(Ns) )      &
                         / ( Interf_Samp_Rpd%Real_Part(Ns+1) &
                            -Interf_Samp_Rpd%Real_Part(Ns) ) )
           Interf_Samp_Rpd%Opd_Top(Interf_Samp_Rpd%N_Top) =  &
                           dble(Interf_Samp_Rpd%N_Top-1)     &
                           *Interf_Param%LambdaLaserRpd/2.d+00
        end if
     end do
     write(*,*) 'Interf_Samp_Rpd%N_Top          ',&
                 Interf_Samp_Rpd%N_Top
     write(*,*) 'Interf_Samp_Rpd%Time_Top(1)    ',&
                 Interf_Samp_Rpd%Time_Top(1)
     write(*,*) 'Interf_Samp_Rpd%Time_Top(N_Top)',&
                 Interf_Samp_Rpd%Time_Top(Interf_Samp_Rpd%N_Top)
     write(*,*) 'Interf_Samp_Rpd%Opd_Top(1)     ',&
                 Interf_Samp_Rpd%Opd_Top(1)
     write(*,*) 'Interf_Samp_Rpd%Opd_Top(N_Top) ',&
                 Interf_Samp_Rpd%Opd_Top(Interf_Samp_Rpd%N_Top)
!
!    statistical analysis of the Time_Top
     allocate( X(Interf_Samp_Rpd%N_Top) )
     do Ns = 1, Interf_Samp_Rpd%N_Top-1
        X(Ns) = Interf_Samp_Rpd%Time_Top(Ns+1)-Interf_Samp_Rpd%Time_Top(Ns)
     end do
     call statistics( Interf_Samp_Rpd%N_Top, &
                      X,          &
                      XAvg,       &
                      XStd,       &
                      XMin,       &
                      XMax,       &
                      NsMin,      &
                      NsMax       )
     write(*,'(60a)') &
            'Time_Top Variation : XAvg, XStd, XMin, XMax, NsMin, NsMax'
     write(*,'(4f20.9,2i10)') &
                               XAvg, XStd, XMin, XMax, NsMin, NsMax
     deallocate( X )
     return
!
   end subroutine rpd_zero_crossing
!
!
   subroutine signal_integration_none( Interf_Cont, &
                                       Interf_Samp  )
   implicit none
     type(type_Interferogram), intent(in)                :: Interf_Cont
     type(type_Interferogram), intent(inout)             :: Interf_Samp
!
     call intspline( Interf_Cont%N_Sample,  &
                     Interf_Cont%Time,      &
                     Interf_Cont%Real_Part, &
                     Interf_Samp%N_Sample,  &
                     Interf_Samp%Time,      &
                     Interf_Samp%Real_Part  )
!
     write(*,*) 'signal_integration_none Min',&
                 minval(Interf_Samp%Real_Part(1:Interf_Samp%N_Sample))
     write(*,*) 'signal_integration_none Max',&
                 maxval(Interf_Samp%Real_Part(1:Interf_Samp%N_Sample))
!
     if( Interf_Cont%Type == 'RI' ) then
        call intspline( Interf_Cont%N_Sample,  &
                        Interf_Cont%Time,      &
                        Interf_Cont%Imag_Part, &
                        Interf_Samp%N_Sample,  &
                        Interf_Samp%Time,      &
                        Interf_Samp%Imag_Part  )
!
        write(*,*) 'signal_integration_none Min',&
                    minval(Interf_Samp%Imag_Part(1:Interf_Samp%N_Sample))
        write(*,*) 'signal_integration_none Max',&
                    maxval(Interf_Samp%Imag_Part(1:Interf_Samp%N_Sample))
!
     end if
!
     return
!
   end subroutine signal_integration_none
!
!

!> sampling_integration_time -- Public
!!
!! * Purpose
!!
!!     Integration of the signal at constant time 
!!
!! * Description
!!
!!     The signal is computed by integration by trapezes at constant time
!!
!! * Inputs
!!
!!     - Interf_Param : type_Interf_Param / type for declaration and allocation of 
!!                      interferogram parameters
!!     - Interf_Cont  : type_Interferogram / type for declaration and allocation of 
!!                      interferogram
!!
!! * Inputs/outputs
!!
!!     - Interf_Samp  : type_Interferogram / type for declaration and allocation of 
!!                      interferogram
!!
!! * Outputs
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.8.9
!!

   subroutine signal_integration_time( Interf_Param,&
                                       Interf_Cont, &
                                       Interf_Samp  )
   implicit none
     type(type_Interf_Param) , intent(in)                :: Interf_Param
     type(type_Interferogram), intent(in)                :: Interf_Cont
     type(type_Interferogram), intent(inout)             :: Interf_Samp
     integer(kind=LONG)                                  :: Ns
     integer(kind=LONG)                                  :: n
     integer(kind=LONG)                                  :: n1
     integer(kind=LONG)                                  :: n2
     integer(kind=LONG)                                  :: n3
     integer(kind=LONG)                                  :: n4
     real(kind=DOUBLE)                                   :: dT_Integ_Jitter
     real(kind=DOUBLE)                                   :: T_Integ_Start
     real(kind=DOUBLE)                                   :: T_Integ_Stop
!
     write(*,*) 'Interf_Param%dT_Integ',Interf_Param%dT_Integ
     write(*,*) 'Interf_Param%dT_Integ_Jitter',Interf_Param%dT_Integ_Jitter
!
!    limits verification
     dT_Integ_Jitter = Interf_Param%dT_Integ         &
                     + ( 5*Interf_Param%dT_Integ_Jitter )
     T_Integ_Start = Interf_Samp%Time(1) &
                   - dT_Integ_Jitter/2.d+00
     T_Integ_Stop  = Interf_Samp%Time(Interf_Samp%N_Sample) &
                   + dT_Integ_Jitter/2.d+00
     if( T_Integ_Start < Interf_Cont%Time(1) ) then
        write(*,*) 'T_Integ_Start Error',&
                    T_Integ_Start,Interf_Cont%Time(1)
        write(*,*) 'signal_integration_time Fatal Error'
        call exit(1)
     end if
     if( T_Integ_Stop > Interf_Cont%Time(Interf_Cont%N_Sample) ) then
        write(*,*) 'T_Integ_Stop Error',&
                    T_Integ_Stop,Interf_Cont%Time(Interf_Cont%N_Sample)
        write(*,*) 'signal_integration_time Fatal Error'
        call exit(1)
     end if
!
!    sample loop
     do Ns = 1, Interf_Samp%N_Sample
        dT_Integ_Jitter = Interf_Param%dT_Integ                 &
                        + (Interf_Param%dT_Integ_Jitter * prnd())
        T_Integ_Start = Interf_Samp%Time(Ns) - dT_Integ_Jitter/2.d+00
        T_Integ_Stop  = Interf_Samp%Time(Ns) + dT_Integ_Jitter/2.d+00
        Interf_Samp%Real_Part(Ns) = 0.d+00
        n1 = 1
        n2 = Interf_Cont%N_Sample
        call dichotomd( T_Integ_Start,Interf_Cont%Time(n1),n1,n2 )
        n3 = 1
        n4 = Interf_Cont%N_Sample
        call dichotomd( T_Integ_Stop,Interf_Cont%Time(n3),n3,n4 )
!        if( n1 == n2 .or. n2 == n3 .or. n3 == n4 .or. n2+1 == n3 ) then
!           write(*,*) 'n1 n2 n3 n4 ',n1,n2,n3,n4
!        end if
        Interf_Samp%Real_Part(Ns) =                              &
              Interf_Samp%Real_Part(Ns)                          &
              + ( ( ( Interf_Cont%Real_Part(n1)                  &
                      +( Interf_Cont%Real_Part(n1+1)             &
                        -Interf_Cont%Real_Part(n1) )             &
                      /( Interf_Cont%Time(n1+1)                  &
                        -Interf_Cont%Time(n1) )                  &
                      *( T_Integ_Start-Interf_Cont%Time(n1) ) ) )&
                  + Interf_Cont%Real_Part(n2) )                  &
               * ( Interf_Cont%Time(n2)-T_Integ_Start ) / 2.d+00
        if( n2+1 <= n3 ) then
          do n = n2+1, n3
              Interf_Samp%Real_Part(Ns) =               &
                         Interf_Samp%Real_Part(Ns)      &
                         + ( Interf_Cont%Real_Part(n-1) &
                            +Interf_Cont%Real_Part(n) ) &
                         * Interf_Cont%dTime  / 2.d+00
          end do
        end if
        Interf_Samp%Real_Part(Ns) =                            &
             Interf_Samp%Real_Part(Ns)                         &
             + ( ( ( Interf_Cont%Real_Part(n3)                 &
                     +( Interf_Cont%Real_Part(n4)              &
                       -Interf_Cont%Real_Part(n4-1) )          &
                     /( Interf_Cont%Time(n4)                   &
                       -Interf_Cont%Time(n4-1) )               &
                     *( T_Integ_Stop-Interf_Cont%Time(n3) ) ) )&
                 + Interf_Cont%Real_Part(n3) )                 &
              * ( T_Integ_Stop-Interf_Cont%Time(n3) ) / 2.d+00
!
        Interf_Samp%Real_Part(Ns) =                 &
                          Interf_Samp%Real_Part(Ns) &
                        / Interf_Param%dT_Integ
     end do
     write(*,*) 'signal_integration_time Min',&
                 minval(Interf_Samp%Real_Part(1:Interf_Samp%N_Sample))
     write(*,*) 'signal_integration_time Max',&
                 maxval(Interf_Samp%Real_Part(1:Interf_Samp%N_Sample))
     return
!
   end subroutine signal_integration_time
!
!

!> sampling_integration_opd -- Public
!!
!! * Purpose
!!
!!     Integration of the signal at constant OPD 
!!
!! * Description
!!
!!     The signal is computed by integration by trapezes at constant OPD
!!
!! * Inputs
!!
!!     - Interf_Param : type_Interf_Param / type for declaration and allocation of 
!!                      interferogram parameters
!!     - Interf_Cont  : type_Interferogram / type for declaration and allocation of 
!!                      interferogram
!!
!! * Inputs/outputs
!!
!!     - Interf_Samp  : type_Interferogram / type for declaration and allocation of 
!!                      interferogram
!!
!! * Outputs
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.8.9
!!

   subroutine signal_integration_opd( Interf_Param,&
                                      Interf_Cont, &
                                      Interf_Samp  )
   implicit none
     type(type_Interf_Param) , intent(in)                :: Interf_Param
     type(type_Interferogram), intent(in)                :: Interf_Cont
     type(type_Interferogram), intent(inout)             :: Interf_Samp
     integer(kind=LONG)                                  :: Ns
     integer(kind=LONG)                                  :: n
     integer(kind=LONG)                                  :: n1
     integer(kind=LONG)                                  :: n2
     integer(kind=LONG)                                  :: n3
     integer(kind=LONG)                                  :: n4
     real(kind=DOUBLE)                                   :: dT_Integ
     real(kind=DOUBLE)                                   :: T_Integ_Start
     real(kind=DOUBLE)                                   :: T_Integ_Stop
!
     write(*,*) 'Interf_Param%dOpd_Integ',Interf_Param%dOpd_Integ
!
!    limits verification
     dT_Integ = Interf_Param%dOpd_Integ * ( Interf_Samp%Opd(2)  &
                                           -Interf_Samp%Opd(1) )&
                   / (2.d+00*Interf_Param%VCC)
     T_Integ_Start = Interf_Samp%Time(1) &
                   - dT_Integ/2.d+00
     write(*,*) 'dT_Integ Start',dT_Integ,T_Integ_Start
     dT_Integ = Interf_Param%dOpd_Integ &
              * ( Interf_Samp%Opd(Interf_Samp%N_Sample)    &
                 -Interf_Samp%Opd(Interf_Samp%N_Sample-1) )&
                   / (2.d+00*Interf_Param%VCC)
     T_Integ_Stop  = Interf_Samp%Time(Interf_Samp%N_Sample) &
                   + dT_Integ/2.d+00
     write(*,*) 'dT_Integ Stop ',dT_Integ,T_Integ_Stop
     if( T_Integ_Start < Interf_Cont%Time(1) ) then
        write(*,*) 'T_Integ_Start Error',&
                    T_Integ_Start,Interf_Cont%Time(1)
        write(*,*) 'signal_integration_opd Fatal Error'
        call exit(1)
     end if
     if( T_Integ_Stop > Interf_Cont%Time(Interf_Cont%N_Sample) ) then
        write(*,*) 'T_Integ_Stop Error',&
                    T_Integ_Stop,Interf_Cont%Time(Interf_Cont%N_Sample)
        write(*,*) 'signal_integration_opd Fatal Error'
        call exit(1)
     end if
!
!    sampling loop
     do Ns = 1, Interf_Samp%N_Sample
        if( Ns .eq. 1 ) then
          dT_Integ = Interf_Param%dOpd_Integ * ( Interf_Samp%Opd(Ns+1)  &
                                                -Interf_Samp%Opd(Ns) )  &
                   / (2.d+00*Interf_Param%VCC)
        else if( Ns .eq. Interf_Samp%N_Sample ) then
          dT_Integ = Interf_Param%dOpd_Integ * ( Interf_Samp%Opd(Ns)    &
                                                -Interf_Samp%Opd(Ns-1) )&
                   / (2.d+00*Interf_Param%VCC)
        else
          dT_Integ = Interf_Param%dOpd_Integ * ( Interf_Samp%Opd(Ns+1)  &
                                       -Interf_Samp%Opd(Ns-1)) / 2.d+00 &
                   / (2.d+00*Interf_Param%VCC)
        end if
        T_Integ_Start = Interf_Samp%Time(Ns) - dT_Integ/2.d+00
        T_Integ_Stop  = Interf_Samp%Time(Ns) + dT_Integ/2.d+00
        Interf_Samp%Real_Part(Ns) = 0.d+00
        n1 = 1
        n2 = Interf_Cont%N_Sample
        call dichotomd( T_Integ_Start,Interf_Cont%Time(n1),n1,n2 )
        n3 = 1
        n4 = Interf_Cont%N_Sample
        call dichotomd( T_Integ_Stop,Interf_Cont%Time(n3),n3,n4 )
        if( n1 == n2 ) then
          Interf_Samp%Real_Part(Ns) =                &
                     Interf_Samp%Real_Part(Ns)       &
                     + ( Interf_Cont%Real_Part(n1)   &
                        +Interf_Cont%Real_Part(n1+1))&
                     * Interf_Cont%dTime  / 2.d+00
          else
          Interf_Samp%Real_Part(Ns) =                          &
                Interf_Samp%Real_Part(Ns)                      &
                + ( ( Interf_Cont%Real_Part(n1)                &
                      +( Interf_Cont%Real_Part(n2)             &
                        -Interf_Cont%Real_Part(n1) )           &
                      /( Interf_Cont%Time(n2)                  &
                        -Interf_Cont%Time(n1) )                &
                      *( T_Integ_Start-Interf_Cont%Time(n1) ) )&
                    + Interf_Cont%Real_Part(n2) )              &
                 * ( Interf_Cont%Time(n2)-T_Integ_Start ) / 2.d+00
        end if
        do n = n2+1, n3
          Interf_Samp%Real_Part(Ns) =                &
                     Interf_Samp%Real_Part(Ns)       &
                     + ( Interf_Cont%Real_Part(n)    &
                        +Interf_Cont%Real_Part(n-1) )&
                     * Interf_Cont%dTime / 2.d+00
        end do
        if( n3 /= n4 ) then
          Interf_Samp%Real_Part(Ns) =                         &
                Interf_Samp%Real_Part(Ns)                     &
                + ( ( Interf_Cont%Real_Part(n3)               &
                      +( Interf_Cont%Real_Part(n4)            &
                        -Interf_Cont%Real_Part(n3) )          &
                      /( Interf_Cont%Time(n4)                 &
                        -Interf_Cont%Time(n3) )               &
                      *( T_Integ_Stop-Interf_Cont%Time(n3) ) )&
                    + Interf_Cont%Real_Part(n3) )             &
                 * ( T_Integ_Stop-Interf_Cont%Time(n3) ) / 2.d+00
        end if
        Interf_Samp%Real_Part(Ns) =                  &
                           Interf_Samp%Real_Part(Ns) &
                         / Interf_Param%dT_Integ
     end do
     write(*,*) 'signal_integration_opd Min',&
                 minval(Interf_Samp%Real_Part(1:Interf_Samp%N_Sample))
     write(*,*) 'signal_integration_opd Max',&
                 maxval(Interf_Samp%Real_Part(1:Interf_Samp%N_Sample))
     return
!
   end subroutine signal_integration_opd
!
!
   subroutine signal_integration_differential( Interf_Param,&
                                               Interf_Cont, &
                                               Interf_Samp  )
   implicit none
     type(type_Interf_Param) , intent(in)                :: Interf_Param
     type(type_Interferogram), intent(in)                :: Interf_Cont
     type(type_Interferogram), intent(inout)             :: Interf_Samp
     integer(kind=LONG)                                  :: Ns
     integer(kind=LONG)                                  :: n
     integer(kind=LONG)                                  :: n1
     integer(kind=LONG)                                  :: n2
     integer(kind=LONG)                                  :: n3
     integer(kind=LONG)                                  :: n4
     integer(kind=LONG)                                  :: n5
     integer(kind=LONG)                                  :: n6
     real(kind=DOUBLE)                                   :: dT_Integ_Jitter
     real(kind=DOUBLE)                                   :: T_Integ_Start
     real(kind=DOUBLE)                                   :: T_Integ_Half
     real(kind=DOUBLE)                                   :: T_Integ_Stop
     real(kind=DOUBLE)                                   :: Interf_Samp_Left
     real(kind=DOUBLE)                                   :: Interf_Samp_Right
!
     write(*,*) 'Interf_Param%dT_Integ',Interf_Param%dT_Integ
     write(*,*) 'Interf_Param%dT_Integ_Jitter',Interf_Param%dT_Integ_Jitter
!
!    limits verification
     dT_Integ_Jitter = Interf_Param%dT_Integ         &
                     + ( 5*Interf_Param%dT_Integ_Jitter )
     T_Integ_Start = Interf_Samp%Time(1) &
                   - dT_Integ_Jitter/2.d+00
     T_Integ_Stop  = Interf_Samp%Time(Interf_Samp%N_Sample) &
                   + dT_Integ_Jitter/2.d+00
     if( T_Integ_Start < Interf_Cont%Time(1) ) then
        write(*,*) 'T_Integ_Start Error',&
                    T_Integ_Start,Interf_Cont%Time(1)
        write(*,*) 'signal_integration_differential Fatal Error'
        call exit(1)
     end if
     if( T_Integ_Stop > Interf_Cont%Time(Interf_Cont%N_Sample) ) then
        write(*,*) 'T_Integ_Stop Error',&
                    T_Integ_Stop,Interf_Cont%Time(Interf_Cont%N_Sample)
        write(*,*) 'signal_integration_differential Fatal Error'
        call exit(1)
     end if
!
!    sample loop
     do Ns = 1, Interf_Samp%N_Sample
        dT_Integ_Jitter = Interf_Param%dT_Integ                 &
                        + (Interf_Param%dT_Integ_Jitter * prnd())
        T_Integ_Start = Interf_Samp%Time(Ns) - dT_Integ_Jitter/2.d+00
        T_Integ_Half  = Interf_Samp%Time(Ns)  &
                        + (Interf_Param%dT_Integ_Jitter * prnd())
        dT_Integ_Jitter = Interf_Param%dT_Integ                 &
                        + (Interf_Param%dT_Integ_Jitter * prnd())
        T_Integ_Stop  = Interf_Samp%Time(Ns) + dT_Integ_Jitter/2.d+00
        Interf_Samp_Left  = 0.d+00
        Interf_Samp_Right = 0.d+00
        n1 = 1
        n2 = Interf_Cont%N_Sample
        call dichotomd( T_Integ_Start,Interf_Cont%Time(n1),n1,n2 )
        n3 = 1
        n4 = Interf_Cont%N_Sample
        call dichotomd( T_Integ_Half,Interf_Cont%Time(n3),n3,n4 )
        n5 = 1
        n6 = Interf_Cont%N_Sample
        call dichotomd( T_Integ_Stop,Interf_Cont%Time(n5),n5,n6 )
        if( n1 == n2 ) then
          Interf_Samp_Left = Interf_Samp_Left        &
                     + ( Interf_Cont%Real_Part(n1)   &
                        +Interf_Cont%Real_Part(n1+1))&
                     * Interf_Cont%dTime  / 4.d+00
        else
          Interf_Samp_Left = Interf_Samp_Left                  &
                + ( ( Interf_Cont%Real_Part(n1)                &
                      +( Interf_Cont%Real_Part(n2)             &
                        -Interf_Cont%Real_Part(n1) )           &
                      /( Interf_Cont%Time(n2)                  &
                        -Interf_Cont%Time(n1) )                &
                      *( T_Integ_Start-Interf_Cont%Time(n1) ) )&
                    + Interf_Cont%Real_Part(n2) )              &
                 * ( Interf_Cont%Time(n2)-T_Integ_Start ) / 4.d+00
        end if
        do n = n2+1, n3
          Interf_Samp_Left = Interf_Samp_Left        &
                     + ( Interf_Cont%Real_Part(n)    &
                        +Interf_Cont%Real_Part(n-1) )&
                     * Interf_Cont%dTime  / 4.d+00
        end do
        if( n3 /= n4 ) then
          Interf_Samp_Left = Interf_Samp_Left                 &
                + ( ( Interf_Cont%Real_Part(n3)               &
                      +( Interf_Cont%Real_Part(n4)            &
                        -Interf_Cont%Real_Part(n3) )          &
                      /( Interf_Cont%Time(n4)                 &
                        -Interf_Cont%Time(n3) )               &
                      *( T_Integ_Half-Interf_Cont%Time(n3) ) )&
                    + Interf_Cont%Real_Part(n3) )             &
                 * ( T_Integ_Half-Interf_Cont%Time(n3) ) / 4.d+00
        else
          Interf_Samp_Right = Interf_Samp_Right      &
                     + ( Interf_Cont%Real_Part(n3)   &
                        +Interf_Cont%Real_Part(n3+1))&
                     * Interf_Cont%dTime  / 4.d+00
        end if
        do n = n4+1, n5
          Interf_Samp_Right = Interf_Samp_Right      &
                     + ( Interf_Cont%Real_Part(n)    &
                        +Interf_Cont%Real_Part(n-1) )&
                     * Interf_Cont%dTime  / 4.d+00
        end do
        if( n5 /= n6 ) then
          Interf_Samp_Right = Interf_Samp_Right               &
                + ( ( Interf_Cont%Real_Part(n5)               &
                      +( Interf_Cont%Real_Part(n6)            &
                        -Interf_Cont%Real_Part(n5) )          &
                      /( Interf_Cont%Time(n6)                 &
                        -Interf_Cont%Time(n5) )               &
                      *( T_Integ_Stop-Interf_Cont%Time(n5) ) )&
                    + Interf_Cont%Real_Part(n5) )             &
                 * ( T_Integ_Stop-Interf_Cont%Time(n5) ) / 4.d+00
        end if
        if( Ns == 1 ) then
          Interf_Samp%Real_Part(Ns) = Interf_Samp_Left
        else
          Interf_Samp%Real_Part(Ns) = Interf_Samp_Right - Interf_Samp_Left
        end if
     end do
     write(*,*) 'signal_integration_differential Min',&
                 minval(Interf_Samp%Real_Part(2:Interf_Samp%N_Sample))
     write(*,*) 'signal_integration_differential Max',&
                 maxval(Interf_Samp%Real_Part(2:Interf_Samp%N_Sample))
     return
!
   end subroutine signal_integration_differential
!
!
   subroutine recombine_differential( Interf_Param,   &
                                      Interf_Diff,    &
                                      Interf_Samp     )
   implicit none
     type(type_Interf_Param) , intent(in)                :: Interf_Param
     type(type_Interferogram), intent(inout)             :: Interf_Diff
     type(type_Interferogram), intent(out)               :: Interf_Samp
     integer(kind=LONG)                                  :: Ns
!
     Interf_Samp%N_Sample = int(Interf_Diff%N_Sample/2)
     Interf_Samp%dTime    = Interf_Diff%dTime * 2.d+00
     Interf_Samp%TimeMax  = Interf_Samp%dTime               &
                          * dble(int(Interf_Samp%N_Sample/2))
     Interf_Samp%dOpd     = Interf_Samp%dTime * 2.d+00 * Interf_Param%VCC
     Interf_Samp%OpdMax   = Interf_Samp%dOpd                &
                          * dble(int(Interf_Samp%N_Sample/2))
     Interf_Samp%FieldMeanAngle = Interf_Diff%FieldMeanAngle
     call alloc_Interferogram( Interf_Samp )
     Interf_Samp%Time(1:Interf_Samp%N_Sample) =           &
                 Interf_Diff%Time(1:Interf_Diff%N_Sample:2)
     Interf_Samp%Opd(1:Interf_Samp%N_Sample) =            &
                 Interf_Diff%Opd(1:Interf_Diff%N_Sample:2)
     Interf_Samp%N_Top = Interf_Diff%N_Top
     Interf_Samp%Time_Top(1:Interf_Samp%N_Sample) = &
                        Interf_Diff%Time_Top(1:Interf_Diff%N_Top)
     Interf_Samp%Opd_Top(1:Interf_Samp%N_Sample)  = &
                        Interf_Diff%Opd_Top(1:Interf_Diff%N_Top)
    do Ns = 1, Interf_Diff%N_Sample
       if( Ns == 1 ) then
          Interf_Diff%Real_Part(Ns) = Interf_Diff%Real_Part(Ns)
       else
          Interf_Diff%Real_Part(Ns) = Interf_Diff%Real_Part(Ns) &
                                    + Interf_Diff%Real_Part(Ns-1)
       end if
    end do
    Interf_Samp%Real_Part(1:Interf_Samp%N_Sample) =         &
          Interf_Diff%Real_Part(1:Interf_Diff%N_Sample-1:2) &
        + Interf_Diff%Real_Part(2:Interf_Diff%N_Sample:2)
     
     return
!
   end subroutine recombine_differential
!
!

!> add_sampling_jitter -- Public
!!
!! * Purpose
!!
!!     Addition of a sampling jitter 
!!
!! * Description
!!
!!     Interferogram sampling can be affected by a jitter. The sampled temporal basis is 
!!     then simulated as a Gaussian random function and includes the sampling jitter and delay.
!!
!! * Inputs
!!
!!     - Delay       : delay value 
!!     - Samp_Jitter : sampling jitter amplitude value
!!
!! * Inputs/outputs
!!
!!     - Interf_Samp : type_Interferogram / type for declaration and allocation of interferogram
!!
!! * Outputs
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.8.5
!!

   subroutine add_sampling_jitter( Samp_Jitter,&
                                   Interf_Samp )
   implicit none
     real(kind=DOUBLE)       , intent(in)                :: Samp_Jitter
     type(type_Interferogram), intent(inout)             :: Interf_Samp
     integer(kind=LONG)                                  :: Ns
     integer(kind=LONG)                                  :: ks
     integer(kind=LONG)                                  :: kk
     real(kind=DOUBLE)                                   :: br
     real(kind=DOUBLE)                                   :: Jitter
     real(kind=DOUBLE)                                   :: rmsjitter
!
     if( Samp_Jitter /= 0.0d+00 ) then
!
!       sampling jitter computation
        rmsjitter = 0.0d+00
        do Ns = 1, Interf_Samp%N_Sample
           ks = idnint(mod(abs(Interf_Samp%Real_Part(Ns))*1.d+08,128.d+00))
           do kk = 1, ks
              br = prnd()
           end do
           Jitter = Samp_Jitter * grnd(0)
           Interf_Samp%Time(Ns) = Interf_Samp%Time(Ns) &
                                       + Jitter
           rmsjitter = rmsjitter + (Jitter)**2
        end do
        write(*,*) 'Sampling jitter applied Jitter Rms', &
                    Samp_Jitter, rmsjitter
     else
        write(*,*) 'No sampling jitter applied'
     end if
!
     return
!
   end subroutine add_sampling_jitter
!
!

!> add_interf_samp_noise -- Public
!!
!! * Purpose
!!
!!     Addition of a Gaussian random radiometric noise to the sampled interferogram 
!!     sampling jitter 
!!
!! * Description
!!
!!     A Gaussian random radiometric noise can be added to the sampled interferogram. 
!!     The noise which is added with the add_interf_samp_noise subroutine is defined as 
!!     a fraction of the maximum interferogram signal. 
!!
!! * Inputs
!!
!!     - Noise_Std   : the one sigma noise to be applied to RPD, CS, BB or EW interferograms 
!!                     per band
!!
!! * Inputs/outputs
!!
!!     - Interf_Samp : type_Interferogram / type for declaration and allocation of interferogram
!!
!! * Outputs
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.8.6
!!

   subroutine add_interf_samp_noise( Noise_Std,  &
                                     Interf_Samp )
   implicit none
     real(kind=DOUBLE)       , intent(in)                :: Noise_Std
     type(type_Interferogram), intent(inout)             :: Interf_Samp
     real(kind=DOUBLE)                                   :: rmsbr
     real(kind=DOUBLE)                                   :: Noise
     real(kind=DOUBLE)                                   :: Interf_Max
     integer(kind=LONG)                                  :: n
     integer(kind=LONG)                                  :: ks
     integer(kind=LONG)                                  :: kk
     real(kind=DOUBLE)                                   :: br
!
     if( Noise_Std /= 0.0d+00 ) then
        Interf_Max = maxval( Interf_Samp%Real_Part &
                                     (1:Interf_Samp%N_Sample) )
        rmsbr = 0.d+00
        Noise = Noise_Std * Interf_Max
        do n = 1, Interf_Samp%N_Sample
           ks = idnint(mod(abs(Interf_Samp%Real_Part(n))*1.d+08,128.d+00))
           do kk = 1, ks
             br = prnd()
           end do
           br = Noise*grnd(0)
           Interf_Samp%Real_Part(n) = &
                          Interf_Samp%Real_Part(n) + br
           rmsbr = rmsbr + (br)**2
         end do
         rmsbr = dsqrt( rmsbr/dble(Interf_Samp%N_Sample))
         write(*,*) 'rmsbr Noise_Std',rmsbr,Noise_Std
      else
         write(*,*) 'No noise applied'
      end if
     return
!
   end subroutine add_interf_samp_noise
!
!
end module interf_sampling_module
