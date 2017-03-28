!!#  decimation_module.f90 --
!!# 
!!#            Project: SPS_GENERIC
!!#            Authors: NOVELTIS/B.TOURNIER
!!#               Date: october 2009
!!#            Version: $Revision: 1.6 $
!!#  Last modification: $Date: 2011-04-26 09:23:17 $
!!#
!!#  Language:  F90
!!#  Standards: Noveltis
!!#
!!# --
!!#
!!
!> interferogram decimation -- Module
!!
!! * Purpose
!!
!!   Module for interferogram decimation functions
!!
!! * Description
!!      
!!   The interferogram decimation module goal is to decrease the sampling rate of the 
!!   interferograms. The module uses a Finite Impulse Response (FIR) and keeps one point 
!!   of Npts (the decimation factor).
!!
!! * Sub-routines and functions
!!
!! -  decim_index_ref     : definition of decimation indexation parameters
!! -  modified_fir_filter : FIR filter resampling
!! -  sec_fir_decimation  : interferogram resampling
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.5.6
!!

module decimation_module
   use precision_type
   use error_type
   use constantes_type
   use zpd_type
   use decim_one_fir_type
   use interf_param_type
   use interferogram_type
   use math_module
!
   implicit none
!
!
   public ::                              &
             decim_one_fir_init,          &
             decimation_one_fir,          &
             interf_triplet_decim_one_fir
!
   contains
!
!

!
!
   subroutine decim_one_fir_init( file_decim, Decim_One_fir )
   implicit none
     character(len=500)      ,intent(in)               :: file_decim
     type(type_Decim_One_fir),intent(inout)            :: Decim_One_fir
     integer(kind=LONG)                                :: Ns
     integer(kind=LONG)                                :: iFile
     integer(kind=LONG)                                :: iPos
     character(len=500)                                :: ffir_1
     real(kind=DOUBLE)                                 :: Norm_FIR1
!
     iFile = 10
     iPos = 1
     open(iFile,file=file_decim, status='old', err=999)
     Decim_One_fir%filename = file_decim
     iPos = 2
     read(iFile,*,err=999) Decim_One_fir%Ns_Decim
     read(iFile,*,err=999) Decim_One_fir%Ns_Fft
     read(iFile,*,err=999) Decim_One_fir%Decim_Factor_1
     read(iFile,*,err=999) Decim_One_fir%N_Fir_1
     read(iFile,*,err=999) Decim_One_fir%Nc_Fir_1
     read(iFile,*,err=999) Decim_One_fir%WnRef
     read(iFile,*,err=999) Decim_One_fir%Nvdi
     write(*,*) 'Decim_One_fir%Ns_Decim      ',Decim_One_fir%Ns_Decim
     write(*,*) 'Decim_One_fir%Ns_Fft        ',Decim_One_fir%Ns_Fft
     write(*,*) 'Decim_One_fir%Decim_Factor_1',Decim_One_fir%Decim_Factor_1
     write(*,*) 'Decim_One_fir%N_Fir_1       ',Decim_One_fir%N_Fir_1
     write(*,*) 'Decim_One_fir%Nc_Fir_1      ',Decim_One_fir%Nc_Fir_1
     write(*,*) 'Decim_One_fir%WnRef         ',Decim_One_fir%WnRef
     write(*,*) 'Decim_One_fir%Nvdi          ',Decim_One_fir%Nvdi
     iPos = 3
     read(iFile,'(a)',err=999) ffir_1
     close(iFile)
     call alloc_Decim_One_fir( Decim_One_fir )
     iPos = 4
     open(iFile,file=ffir_1, status='old', err=999)
     iPos = 5
     do Ns = 1, Decim_One_fir%N_Fir_1
        read(iFile,*,err=999) Decim_One_fir%Fir_1(Ns)
     end do
     close(iFile)
     write(*,*) 'Norm FIR1',sum(Decim_One_fir%Fir_1(1:Decim_One_fir%N_Fir_1))
     Norm_FIR1 = sum(Decim_One_fir%Fir_1(1:Decim_One_fir%N_Fir_1))
     Decim_One_fir%Fir_1(1:Decim_One_fir%N_Fir_1) = &
         Decim_One_fir%Fir_1(1:Decim_One_fir%N_Fir_1) / Norm_FIR1
     write(*,*) 'Norm FIR1',sum(Decim_One_fir%Fir_1(1:Decim_One_fir%N_Fir_1))
!
     return
999  write(*,*) 'Decimation read parameters Error',iPos
     call exit(1)
   end subroutine decim_one_fir_init
!
!
   subroutine decimation_one_fir( Decim_One_fir,&
                                  Interf_Param, &
                                  Interf_ReSamp,&
                                  Interf_Decim  )
   implicit none
     type(type_Decim_One_fir),intent(inout)             :: Decim_One_fir
     type(type_Interf_Param) ,intent(in)                :: Interf_Param
     type(type_Interferogram),intent(in)                :: Interf_ReSamp
     type(type_Interferogram),intent(inout)             :: Interf_Decim
     real(kind=DOUBLE)       ,dimension(:),allocatable  :: Phase
     complex(kind=DOUBLE)    ,dimension(:),allocatable  :: Interf_dePhase
     integer(kind=LONG)                                 :: Ns
     integer(kind=LONG)                                 :: Ms
     integer(kind=LONG)                                 :: NZpd
     real(kind=DOUBLE)       ,dimension(:),allocatable  :: Part_Real
     real(kind=DOUBLE)       ,dimension(:),allocatable  :: Part_Imag
     integer(kind=LONG)                                 :: N_Sample_0
     integer(kind=LONG)                                 :: N_Start
     integer(kind=LONG)                                 :: N_Stop
     integer(kind=LONG)                                 :: N_Filt
!
     NZpd = int(Interf_ReSamp%N_Sample/2) + 1
!
!    compute decimation parameters
     Decim_One_fir%OpdMax = Interf_ReSamp%dOpd          &
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
!    Usefull ReSampled interferogram Samples
     N_Sample_0 = ( Decim_One_fir%Ns_Fft         &
                  * Decim_One_fir%Decim_Factor_1 ) 
!
!    First and last kept ReSampled interferogram Samples
     N_Start = NZpd - int(N_Sample_0/2)
     N_Stop  = NZpd + int(N_Sample_0/2)
     N_Filt  = int(Decim_One_fir%N_Fir_1/2)
     write(*,*) 'NZpd            ',NZpd
     write(*,*) 'N_Start         ',N_Start
     write(*,*) 'N_Stop          ',N_Stop
!
!    compute the phase function for spectral band centering
     allocate( Phase(Interf_ReSamp%N_Sample) )
     Phase(1:Interf_ReSamp%N_Sample) =                              &
             twoPi * dble(Decim_One_fir%N_Shift)                    &
                   / dble(Decim_One_fir%Ns_Fft)                     &
                   / dble(Decim_One_fir%Decim_Factor_1)             &
                   *(/ (dble(Ns-NZpd),Ns=1,Interf_ReSamp%N_Sample) /)
!
!    Interferogram transfert
     allocate( Interf_dePhase(Interf_ReSamp%N_Sample) )
     Interf_dePhase(1:Interf_ReSamp%N_Sample) =                    &
                  Interf_ReSamp%Real_Part(1:Interf_ReSamp%N_Sample)
!
!    Replace samples out of usefull domain
     Interf_dePhase(N_Start-N_Filt:N_Start-1) =        &
                  Interf_dePhase(N_Stop-N_Filt:N_Stop-1)
     Interf_dePhase(N_Stop:N_Stop+N_Filt) =            &
                  Interf_dePhase(N_Start:N_Start+N_Filt)
!
!    Apply the phase function
     Interf_dePhase(1:Interf_ReSamp%N_Sample) =                 &
                Interf_dePhase(1:Interf_ReSamp%N_Sample)        &
                * dcmplx( dcos(Phase(1:Interf_ReSamp%N_Sample)),&
                          dsin(Phase(1:Interf_ReSamp%N_Sample)) )
     allocate( Part_Real(Interf_ReSamp%N_Sample) )
     allocate( Part_Imag(Interf_ReSamp%N_Sample) )
     call cplxtorealimag( Interf_dePhase, Part_Real, Part_Imag, &
                          Interf_ReSamp%N_Sample )
     deallocate( Part_Real )
     deallocate( Part_Imag )
     deallocate( Phase )
!
!    Decimation step
     Interf_Decim%Type     = 'C'
     Interf_Decim%N_Sample = Decim_One_fir%Ns_Decim + 1
     Interf_Decim%dOpd     = Interf_ReSamp%dOpd &
                           * Decim_One_fir%Decim_Factor_1
     Interf_Decim%dTime    = Interf_Decim%dOpd / (2.d+00*Interf_Param%VCC)
     Interf_Decim%OpdMax   = Interf_Decim%dOpd &
                           * dble(int(Interf_Decim%N_Sample/2))
     Interf_Decim%TimeMax  = Interf_Decim%OpdMax / (2.d+00*Interf_Param%VCC)
     call alloc_Interferogram( Interf_Decim )
     call Interferogram_Basis( Interf_Decim )
     do Ns = 1, Decim_One_fir%N_Sample_1
        Ms = NZpd &
           - ( int(Decim_One_fir%N_Sample_1/2)+1 - Ns ) &
             * Decim_One_fir%Decim_Factor_1
        if( (Ns == 1) .or. (Ns == Decim_One_fir%N_Sample_1) ) then
           write(*,*) 'Ms ',Ms
        end if
        Interf_Decim%Complex(Ns) = sum(                  &
           Decim_One_fir%Fir_1(1:Decim_One_fir%N_Fir_1)  &
         * Interf_dePhase(Ms-(Decim_One_fir%Nc_Fir_1-1)  &
                         :Ms+(Decim_One_fir%Nc_Fir_1-1)) )
     end do
     deallocate( Interf_dePhase )
     allocate( Part_Real(Decim_One_fir%N_Sample_1) )
     allocate( Part_Imag(Decim_One_fir%N_Sample_1) )
     call cplxtorealimag( Interf_Decim%Complex, Part_Real, Part_Imag, &
                          Interf_Decim%N_Sample )
     deallocate( Part_Real )
     deallocate( Part_Imag )
!
     return
   end subroutine decimation_one_fir
!
!
   subroutine interf_triplet_decim_one_fir( Decim_One_fir ,  &
                                            Interf_Param,    &
                                            Interf_ReSamp_CS,&
                                            Interf_ReSamp_BB,&
                                            Interf_ReSamp_EW,&
                                            Interf_Decim_CS, &
                                            Interf_Decim_BB, &
                                            Interf_Decim_EW  )
   implicit none
     type(type_Decim_One_fir),intent(inout)             :: Decim_One_fir
     type(type_Interf_Param) ,intent(in)                :: Interf_Param
     type(type_Interferogram),intent(in)                :: Interf_ReSamp_CS
     type(type_Interferogram),intent(in)                :: Interf_ReSamp_BB
     type(type_Interferogram),intent(in)                :: Interf_ReSamp_EW
     type(type_Interferogram),intent(inout)             :: Interf_Decim_CS
     type(type_Interferogram),intent(inout)             :: Interf_Decim_BB
     type(type_Interferogram),intent(inout)             :: Interf_Decim_EW
!
!    Decimation Interferogram Cold Space
     call decimation_one_fir( Decim_One_fir,   &
                              Interf_Param,    &
                              Interf_ReSamp_CS,&
                              Interf_Decim_CS  )
!
!    Decimation Interferogram Black Body
     call decimation_one_fir( Decim_One_fir,   &
                              Interf_Param,    &
                              Interf_ReSamp_BB,&
                              Interf_Decim_BB  )
!
!    Decimation Interferogram Earth View
     call decimation_one_fir( Decim_One_fir,   &
                              Interf_Param,    &
                              Interf_ReSamp_EW,&
                              Interf_Decim_EW  )
     return
   end subroutine interf_triplet_decim_one_fir
!
!
end module decimation_module
