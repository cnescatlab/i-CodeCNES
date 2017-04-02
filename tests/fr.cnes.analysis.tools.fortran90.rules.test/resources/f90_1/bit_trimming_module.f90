!!#  bit_trimming_module.f90 --
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
!> interferogram bit_trimming -- Module
!!
!! * Purpose
!!
!!    Module for interferogram bit_trimming
!!
!! * Description
!!      
!!   This module allows the bit_trimming of interferograms
!!
!! * Sub-routines and functions
!!
!! -  bit_trimming_short : bit_trimming of interferograms discretisation function
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 
!!

module bit_trimming_module
   use precision_type
   use error_type
   use constantes_type
   use bit_trimming_type
   use math_module
   use interferogram_type
!
   implicit none
!
!
   public ::                                  &
             interf_triplet_bit_trimming,     &
             interf_triplet_un_bit_trimming,  &
             bit_trimming_short,              &
             bit_trimming_dummy,              &
             un_bit_trimming_dummy,           &
             bit_trimming_init
!
   contains
!
!
   subroutine interf_triplet_bit_trimming( Bit_Trimming,  &
                                           Interf_CS,     &
                                           Interf_BB,     &
                                           Interf_EW,     &
                                           Interf_Trim_CS,&
                                           Interf_Trim_BB,&
                                           Interf_Trim_EW )
   implicit none
     type(type_Bit_Trimming)    ,intent(in)              :: Bit_Trimming
     type(type_Interferogram)   ,intent(in)              :: Interf_CS
     type(type_Interferogram)   ,intent(in)              :: Interf_BB
     type(type_Interferogram)   ,intent(in)              :: Interf_EW
     type(type_Interferogram)   ,intent(inout)           :: Interf_Trim_CS
     type(type_Interferogram)   ,intent(inout)           :: Interf_Trim_BB
     type(type_Interferogram)   ,intent(inout)           :: Interf_Trim_EW
!
     if( Bit_Trimming%Nb_Bits /= 0 ) then
!
!       CS bit trimming
        call bit_trimming_short( Bit_Trimming,  &
                                 Interf_CS,     &
                                 Interf_Trim_CS )
!
!       BB bit trimming
        call bit_trimming_short( Bit_Trimming,  &
                                 Interf_BB,     &
                                 Interf_Trim_BB )
!
!       EW bit trimming
        call bit_trimming_short( Bit_Trimming,  &
                                 Interf_EW,     &
                                 Interf_Trim_EW )
     else
!
!       CS bit trimming
        call bit_trimming_dummy( Bit_Trimming,  &
                                 Interf_CS,     &
                                 Interf_Trim_CS )
!
!       BB bit trimming
        call bit_trimming_dummy( Bit_Trimming,  &
                                 Interf_BB,     &
                                 Interf_Trim_BB )
!
!       EW bit trimming
        call bit_trimming_dummy( Bit_Trimming,  &
                                 Interf_EW,     &
                                 Interf_Trim_EW )
     end if
     return
   end subroutine interf_triplet_bit_trimming
!
!
   subroutine interf_triplet_un_bit_trimming( Bit_Trimming,  &
                                              Interf_Trim_CS,&
                                              Interf_Trim_BB,&
                                              Interf_Trim_EW,&
                                              Interf_CS,     &
                                              Interf_BB,     &
                                              Interf_EW      )
   implicit none
     type(type_Bit_Trimming)    ,intent(in)              :: Bit_Trimming
     type(type_Interferogram)   ,intent(in)              :: Interf_Trim_CS
     type(type_Interferogram)   ,intent(in)              :: Interf_Trim_BB
     type(type_Interferogram)   ,intent(in)              :: Interf_Trim_EW
     type(type_Interferogram)   ,intent(inout)           :: Interf_CS
     type(type_Interferogram)   ,intent(inout)           :: Interf_BB
     type(type_Interferogram)   ,intent(inout)           :: Interf_EW
!
     if( Bit_Trimming%Nb_Bits == 0 ) then
!
!       CS un bit trimming
        call un_bit_trimming_dummy( Bit_Trimming,  &
                                    Interf_Trim_CS,&
                                    Interf_CS      )
!
!       BB un bit trimming
        call un_bit_trimming_dummy( Bit_Trimming,  &
                                    Interf_Trim_BB,&
                                    Interf_BB      )
!
!       EW un bit trimming
        call un_bit_trimming_dummy( Bit_Trimming,  &
                                    Interf_Trim_EW,&
                                    Interf_EW      )
     else
        write(*,*) ' Wrong Un Bit trimming parametrisation'
        call exit(1)
     end if
     return
   end subroutine interf_triplet_un_bit_trimming
!
!

!> bit_trimming_short -- Public
!!
!! * Purpose
!!
!!     Bit_trimming of interferograms
!!
!! * Description
!! 
!!
!! * Inputs
!!
!!     - Bit_Trimming : type_Bit_Trimming)
!!     - Interf       : type_Interferogram / type for declaration and allocation of interferogram
!! 
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Interf_Trim : type_Interferogram / type for declaration and allocation of interferogram
!!
!! * References
!!
!!     SPS ATBD
!!
!
!
   subroutine bit_trimming_short( Bit_Trimming,&
                                  Interf,      &
                                  Interf_Trim  )
   implicit none
     type(type_Bit_Trimming)    ,intent(in)              :: Bit_Trimming
     type(type_Interferogram)   ,intent(in)              :: Interf
     type(type_Interferogram)   ,intent(inout)           :: Interf_Trim
     integer(kind=LONG)                                  :: Cod_Min
     integer(kind=LONG)                                  :: Cod_Max
     real(kind=DOUBLE)                                   :: Gain
!
!    coherence control
     if( Bit_Trimming%N_Sample /= 1 ) then
       write(*,*) ' Bit_Trimming%N_Sample Fatal Error'
       call exit(1)
     end if
!
!    Header transfert
     call Interf_Header_Transfer( Interf, Interf_Trim )
!
!    Interferogram bit trimming
     if( Bit_Trimming%Nb_Bits == 64 ) then
!
!       64 bits case
        if( Interf_Trim%Type == 'R' ) then
           Interf_Trim%Real_Part(1:Interf_Trim%N_Sample) = &
                         Interf%Real_Part(1:Interf%N_Sample)
        else if( Interf_Trim%Type == 'C' ) then
           Interf_Trim%Complex(1:Interf_Trim%N_Sample) =  &
                          Interf%Complex(1:Interf%N_Sample)
        else
           write(*,*) 'Interferogram Type ERROR'
           write(*,*) 'interf_bit_trimming Fatal Error'
           call exit(1)
        end if
     else
!
!       Cod initialisation
        Cod_Min = -2**(Bit_Trimming%Nb_Bits-1)
        Cod_Max =  2**(Bit_Trimming%Nb_Bits-1) - 1
        Gain =  dble( Cod_Max - Cod_Min )&
             / ( Bit_Trimming%V_Max(1)-Bit_Trimming%V_Min(1) )
        if( Interf_Trim%Type == 'R' ) then
           if( maxval(Interf%Real_Part(1:Interf%N_Sample)) >= &
              Bit_Trimming%V_Max(1) ) then
              write(*,*) 'WARNING !!!!! Bit_Trimming OVERFLOW'
           end if
           if( minval(Interf%Real_Part(1:Interf%N_Sample)) <= &
              Bit_Trimming%V_Min(1) ) then
              write(*,*) 'WARNING !!!!! Bit_Trimming UNDERFLOW'
           end if
           Interf_Trim%Real_Part(1:Interf_Trim%N_Sample) = &
                       Cod_Min + int( Gain *               &
                     ( Interf%Real_Part(1:Interf%N_Sample) &
                      -Bit_Trimming%V_Min(1) ) )
        else if( Interf_Trim%Type == 'C' ) then
           if( (maxval(dreal(Interf%Complex(1:Interf%N_Sample))) >= &
                       Bit_Trimming%V_Max(1)) .or.                  &
               (maxval(dimag(Interf%Complex(1:Interf%N_Sample))) >= &
                       Bit_Trimming%V_Max(1)) )  then
              write(*,*) 'WARNING !!!!! Bit_Trimming OVERFLOW'
           end if
           if( (minval(dreal(Interf%Complex(1:Interf%N_Sample))) <= &
                       Bit_Trimming%V_Min(1)) .or.                  &
               (minval(dimag(Interf%Complex(1:Interf%N_Sample))) <= &
                       Bit_Trimming%V_Min(1)) ) then
              write(*,*) 'WARNING !!!!! Bit_Trimming UNDERFLOW'
           end if
           Interf_Trim%Complex(1:Interf_Trim%N_Sample) = dcmplx( &
                      ( Cod_Min + int( Gain *                    &
                      ( dreal(Interf%Complex(1:Interf%N_Sample)) &
                       -Bit_Trimming%V_Min(1) ) ) )              &
                    , ( Cod_Min + int( Gain *                    &
                      ( dimag(Interf%Complex(1:Interf%N_Sample)) &
                       -Bit_Trimming%V_Min(1) ) ) ) )
        else
           write(*,*) 'Interferogram Type ERROR'
           write(*,*) 'interf_bit_trimming Fatal Error'
           call exit(1)
        end if
     end if
!
     return
   end subroutine bit_trimming_short
!
!
   subroutine bit_trimming_dummy( Bit_Trimming,&
                                  Interf,      &
                                  Interf_Trim  )
   implicit none
     type(type_Bit_Trimming)    ,intent(in)              :: Bit_Trimming
     type(type_Interferogram)   ,intent(in)              :: Interf
     type(type_Interferogram)   ,intent(inout)           :: Interf_Trim
!
     call Interf_Transfer( Interf, Interf_Trim )
!
     return
   end subroutine bit_trimming_dummy
!
!
   subroutine un_bit_trimming_dummy( Bit_Trimming,&
                                     Interf_Trim, &
                                     Interf       )
   implicit none
     type(type_Bit_Trimming)    ,intent(in)              :: Bit_Trimming
     type(type_Interferogram)   ,intent(in)              :: Interf_Trim
     type(type_Interferogram)   ,intent(inout)           :: Interf
!
     call Interf_Transfer( Interf_Trim, Interf )
!
     return
   end subroutine un_bit_trimming_dummy
!
!
   subroutine bit_trimming_init( File_Bit_Trimming, &
                                 Bit_Trimming       )
   implicit none
     character(len=*)           ,intent(in)          :: File_Bit_Trimming
     type(type_Bit_Trimming)    ,intent(inout)       :: Bit_Trimming
     integer(kind=LONG)                              :: iFile
     integer(kind=LONG)                              :: iPos
     integer(kind=LONG)                              :: Ns
!
     iFile = 10
     iPos = 1
     open(unit=iFile, file=File_Bit_Trimming, status='old', err=999)
     iPos = 2
     read(iFile,*,err=999) Bit_Trimming%Nb_Bits
     read(iFile,*,err=999) Bit_Trimming%N_Sample
     call alloc_Bit_Trimming( Bit_Trimming )
     iPos = 3
     do Ns = 1, Bit_Trimming%N_Sample
        read(iFile,*,err=999) Bit_Trimming%V_Min(Ns), Bit_Trimming%V_Max(Ns)
     end do
     close(unit=iFile)
     return
999  write(*,*) 'File_Bit_Trimming reading Error',iPos
     call exit(1)
   end subroutine bit_trimming_init
!
!
end module bit_trimming_module
