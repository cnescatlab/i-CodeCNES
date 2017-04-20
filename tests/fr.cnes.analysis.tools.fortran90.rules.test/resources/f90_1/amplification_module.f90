!!#  amplification_module.f90 -- 
!!# 
!!#            Project: SPS_GENERIC
!!#            Authors: NOVELTIS/B.TOURNIER
!!#               Date: november 2010
!!#            Version: $Revision: 1.3 $
!!#  Last modification: $Date: 2011-11-02 14:57:50 $
!!#
!!#  Language:  F90
!!#  Standards: Noveltis
!!#
!!# --
!!#
!!
!> amplification_module -- Module
!!
!! * Purpose
!!
!!   Module for interferogram amplification
!!
!! * Description
!!      
!!   This module simulates algorithms for an amplifier system which increases the amplitude
!!   of the interferogram signal.
!!
!! * Sub-routines and functions
!!
!! -  amplification_init   : initialisation of the type defining the amplification parameters
!! -  interf_amplification : interferogram amplitude amplification
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.4.2.8
!!

module amplification_module
   use precision_type
   use error_type
   use constantes_type
   use detection_type
   use amplification_type
   use interferogram_type
!
   implicit none
!
!
   public :: amplification_init, &
             interf_amplification
!
!
   contains
!
!

!> amplification_init -- Public
!!
!! * Purpose
!!
!!     Initialisation of the type defining the amplification parameters 
!!
!! * Description
!!
!!     The amplification_init subroutine allows to update the type defining the amplification  
!!     parameters thanks to the read of the File_Param file; 
!!
!! * Inputs
!!
!!     - File_Param : character / input file with definition of amplification parameters
!!
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Amplification : type_Amplification / type for declaration and allocation of  
!!                       amplification parameters 
!!
!! * References
!!

   subroutine amplification_init( File_Param,   &
                                  Amplification )
   implicit none
     character(len=*)        ,intent(in)                 :: File_Param
     type(type_Amplification),intent(out)                :: Amplification
!
     integer(kind=LONG)                                  :: iFile
     integer(kind=LONG)                                  :: iPos
!
     iFile = 10
!
     iPos = 1
     write(*,'(a)') File_Param(1:len_trim(File_Param))
     open(unit=iFile, file=File_Param, status='old',err=999)
     Amplification%filename = File_Param
     iPos = 2
     read(iFile,*,err=999) Amplification%V_Min
     iPos = 3
     read(iFile,*,err=999) Amplification%V_Max
     iPos = 4
     close(unit=iFile)
     write(*,*) 'Amplification%V_Min',Amplification%V_Min
     write(*,*) 'Amplification%V_Max',Amplification%V_Max
!
     return
 999 write(*,*) 'amplification_init Fatal Error',iPos
     call exit(1)
   end subroutine amplification_init
!
!

!> interf_amplification -- Public
!!
!! * Purpose
!!
!!     Interferogram amplitude amplification.
!!
!! * Description
!!
!!     The interf_amplification subroutine allows to increase the signal amplitude of the
!!     interferogram. The relationship of the input to the output of an amplifier is called 
!!     the transfer function; the amplitude of the transfer function is called the gain. 
!!     The signal is amplified between Vmin and Vmax with Gain=Vmax-Vmin. The output signal 
!!     is then computed as Voutput=Vinput*Gain+Vmin.
!!
!! * Inputs
!!
!!     - Amplification : type_Amplification / type for amplification parameters 
!!                       declaration and allocation.
!!
!! * Inputs/outputs
!!
!!     - Interf : type_Interferogram / type for declaration and allocation of interferogram
!!
!! * Outputs
!!
!! * References
!!

   subroutine interf_amplification( Detection,    &
                                    Amplification,&
                                    Interf        )
   implicit none
     type(type_Detection)    ,intent(in)                 :: Detection
     type(type_Amplification),intent(in)                 :: Amplification
     type(type_Interferogram),intent(inout)              :: Interf
!
     real(kind=DOUBLE)                                   :: Gain
!
!    amplification
     Gain = ( Amplification%V_Max - Amplification%V_Min )&
          / ( Detection%V_Max - Detection%V_Min )
     write(*,*) 'interf_amplification Gain',Gain
     write(*,*) 'Amplification%V_Min      ',Amplification%V_Min
!
     Interf%Real_Part(1:Interf%N_Sample) = Amplification%V_Min&
              + ( Gain * ( Interf%Real_Part(1:Interf%N_Sample)&
                         - Detection%V_Min ) )
     write(*,*) 'interf_amplification Min',&
                 minval(Interf%Real_Part(1:Interf%N_Sample))
     write(*,*) 'interf_amplification Max',&
                 maxval(Interf%Real_Part(1:Interf%N_Sample))
!
     return
   end subroutine interf_amplification
!
!
end module amplification_module
