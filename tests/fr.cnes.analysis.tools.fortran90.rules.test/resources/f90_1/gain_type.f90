!!* gain_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: november 2010
!!*           Version: $Revision: 1.4 $
!!* Last modification: $$Date: 2011-11-02 14:57:53 $
!!
!> gain_type -- Module
!!
!! * Purpose
!!
!!   Module for type detectors gain declaration and allocation.
!!
!! * Description
!!      
!!     This module defines the detectors gain type and allows its allocation and declaration. 
!!
!! * Sub-routines and functions
!!
!!     * type_Gain   : type for declaration and allocation of spectral l1c parameters
!!     * alloc_Gain  : type_Gain allocation
!!     * dalloc_Gain : type_Gain deallocation
!!     * readgain    : Gain reading routine
!!     * writegain   : Gain writing routine
!!
!! * References
!!

module gain_type
   use precision_type
   use error_type
   use constantes_type
   use modinouttools
!
   implicit none
!
!
   public :: type_Gain   &
            ,alloc_Gain  &
            ,dalloc_Gain &
            ,readgain    &
            ,writegain   
!
   type :: type_Gain
     character(len=500)                               :: filename !< Detector gain file name
     character(len=3)                                 :: Variance_Choice !< Variance choice ('MIN','MAX','AVG')
     integer(kind=LONG)                               :: NbCol !< colums number
     integer(kind=LONG)                               :: NbLin !< lines number
     real(kind=DOUBLE)                                :: Step !< not used 
     real(kind=DOUBLE)                                :: Sensitivity_Avg !< not used 
     real(kind=DOUBLE) ,dimension(:,:)   ,allocatable :: PRNU !< photo response non uniformity - PRNU(1:NbCol,1:NbLin)
     integer(kind=LONG)                               :: NbCol_PN !< Pixel columns number
     integer(kind=LONG)                               :: NbLin_PN !< Pixel lines number
     integer(kind=LONG)                               :: ColStart_PN !< Pixel first column 
     integer(kind=LONG)                               :: ColEnd_PN !< Pixel last column 
     integer(kind=LONG)                               :: LinStart_PN !< Pixel first line 
     integer(kind=LONG)                               :: LinEnd_PN!< Pixel last line 
     real(kind=DOUBLE)                                :: Gain_Avg_PN !< Pixel gain average
     real(kind=DOUBLE)                                :: Gain_Std_PN !< Pixel gain standart deviation
     real(kind=DOUBLE)                                :: Gain_Min_PN !< Pixel gain minimum
     real(kind=DOUBLE)                                :: Gain_Max_PN !< Pixel gain maximum
     real(kind=DOUBLE) ,dimension(:,:)   ,allocatable :: Gain_PN !< Pixel gain - Gain_PN(1:NbCol_PN,1:NbLin_PN)
     integer(kind=LONG)                               :: Nb_SUB_PN !< Sub-Pixel number
     integer(kind=LONG)                               :: NbCol_SUB_PN !< Sub-Pixel columns number
     integer(kind=LONG)                               :: NbLin_SUB_PN !< Sub-Pixel lines number
     integer(kind=LONG),dimension(:)     ,allocatable :: ColStart_SUB_PN !< Sub-Pixel first column -  ColStart_SUB_PN(1:Nb_SUB_PN)
     integer(kind=LONG),dimension(:)     ,allocatable :: ColEnd_SUB_PN !< Sub-Pixel last column - ColEnd_SUB_PN(1:Nb_SUB_PN) 
     integer(kind=LONG),dimension(:)     ,allocatable :: LinStart_SUB_PN !< Sub-Pixel first line - LinStart_SUB_PN(1:Nb_SUB_PN)
     integer(kind=LONG),dimension(:)     ,allocatable :: LinEnd_SUB_PN !< Sub-Pixel last line - LinEnd_SUB_PN(1:Nb_SUB_PN)
     real(kind=DOUBLE) ,dimension(:)     ,allocatable :: Gain_Avg_SUB_PN !< Sub-Pixel Statistical Average - Gain_Avg_SUB_PN(1:Nb_SUB_PN)
     real(kind=DOUBLE) ,dimension(:)     ,allocatable :: Gain_Std_SUB_PN !< Sub-Pixel Statistical Standart Deviation - Gain_Std_SUB_PN(1:Nb_SUB_PN)
     real(kind=DOUBLE) ,dimension(:)     ,allocatable :: Gain_Min_SUB_PN !< Sub-Pixel Statistical Minimum - Gain_Min_SUB_PN(1:Nb_SUB_PN)
     real(kind=DOUBLE) ,dimension(:)     ,allocatable :: Gain_Max_SUB_PN !< Sub-Pixel Statistical Maximum - Gain_Max_SUB_PN(1:Nb_SUB_PN)
     real(kind=DOUBLE) ,dimension(:,:,:) ,allocatable :: Gain_SUB_PN !< Sub-Pixel gain - Gain_SUB_PN(1:NbCol_SUB_PN,1:NbLin_SUB_PN,1:Nb_SUB_PN)
   end type type_Gain
!
   contains
!
!
   subroutine alloc_Gain( Gain )
   implicit none
     type(type_Gain)    , intent(inout)      :: Gain
     integer(kind=LONG)                      :: ErrCode
     integer(kind=LONG)                      :: ioalloc
!
     ioalloc = 0
     allocate( Gain%PRNU(Gain%NbCol,Gain%NbLin),          stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Gain%Gain_PN(Gain%NbCol_PN,Gain%NbLin_PN), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Gain%ColStart_SUB_PN(Gain%Nb_SUB_PN),      stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Gain%ColEnd_SUB_PN(Gain%Nb_SUB_PN),        stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Gain%LinStart_SUB_PN(Gain%Nb_SUB_PN),      stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Gain%LinEnd_SUB_PN(Gain%Nb_SUB_PN),        stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Gain%Gain_Avg_SUB_PN(Gain%Nb_SUB_PN),      stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Gain%Gain_Std_SUB_PN(Gain%Nb_SUB_PN),      stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Gain%Gain_Min_SUB_PN(Gain%Nb_SUB_PN),      stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Gain%Gain_Max_SUB_PN(Gain%Nb_SUB_PN),      stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Gain%Gain_SUB_PN(Gain%NbCol_SUB_PN,Gain%NbLin_SUB_PN,   &
                                     Gain%Nb_SUB_PN),     stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
!
     if( ioalloc /= 0) then
        write(0,*) 'allocation Gain Error',ioalloc
        write(0,*) 'Gain: fatal error'
        call exit(1)
     end if
     return
   end subroutine alloc_Gain
!
!
   subroutine dalloc_Gain( Gain )
   implicit none
     type(type_Gain), intent(inout)      :: Gain
!
     deallocate( Gain%PRNU )
     deallocate( Gain%Gain_PN )
     deallocate( Gain%Gain_SUB_PN )
     deallocate( Gain%ColStart_SUB_PN )
     deallocate( Gain%ColEnd_SUB_PN )
     deallocate( Gain%LinStart_SUB_PN )
     deallocate( Gain%LinEnd_SUB_PN )
     deallocate( Gain%Gain_Avg_SUB_PN )
     deallocate( Gain%Gain_Std_SUB_PN )
     deallocate( Gain%Gain_Min_SUB_PN )
     deallocate( Gain%Gain_Max_SUB_PN )
!
   end subroutine dalloc_Gain
!
!
   subroutine readgain( Gain,iostatus )
   implicit none
     type(type_Gain)    , intent(inout) :: Gain
!
     integer(kind=LONG), intent(out)   :: iostatus
     integer(kind=DOUBLE)              :: ifile
     integer(kind=LONG)                :: offset
     integer(kind=LONG)                :: Type
     integer(kind=LONG)                :: Size
!
     iostatus = 0
     call open_file_r(Gain%filename                    &
                      (1:len_trim(Gain%filename))      &
                      // char(0), ifile)
     if ( ifile .eq. 0 ) then
        iostatus = 1
     else
        offset = 0
        Type = ch1Type
        Size = 1*3
        call read_field( ifile, Gain%Variance_Choice,         &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Type = i4Type
        Size = 4
        call read_field( ifile, Gain%NbCol,                   &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Gain%NbLin,                   &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Gain%NbCol_PN,                &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Gain%NbLin_PN,                &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Gain%ColStart_PN,             &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Gain%ColEnd_PN,               &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Gain%LinStart_PN,             &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Gain%LinEnd_PN,               &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Gain%Nb_SUB_PN,               &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Gain%NbCol_SUB_PN,            &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Gain%NbLin_SUB_PN,            &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Type = r8Type
        Size = 8
        call read_field( ifile, Gain%Step,                    &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Gain%Sensitivity_Avg,         &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call alloc_Gain( Gain )
        Type = i4Type
        Size = 4*Gain%Nb_SUB_PN
        call read_field( ifile, Gain%ColStart_SUB_PN,         &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Gain%ColEnd_SUB_PN,           &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Gain%LinStart_SUB_PN,         &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Gain%LinEnd_SUB_PN,           &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Type = r8Type
        Size = 8*Gain%NbCol*Gain%NbLin
        call read_field( ifile, Gain%PRNU,                    &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Gain%NbCol_PN*Gain%NbLin_PN
        call read_field( ifile, Gain%Gain_PN,                 &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8
        call read_field( ifile, Gain%Gain_Avg_PN,             &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Gain%Gain_Std_PN,             &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Gain%Gain_Min_PN,             &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Gain%Gain_Max_PN,             &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Gain%NbCol_SUB_PN*Gain%NbLin_SUB_PN*Gain%Nb_SUB_PN
        call read_field( ifile, Gain%Gain_SUB_PN,             &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Gain%Nb_SUB_PN
        call read_field( ifile, Gain%Gain_Avg_SUB_PN,         &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Gain%Gain_Std_SUB_PN,         &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Gain%Gain_Min_SUB_PN,         &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Gain%Gain_Max_SUB_PN,         &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call close_file(Gain%filename              &
                        (1:len_trim(Gain%filename))&
                        // char(0), ifile)
     end if
     return
  end subroutine readgain
!
!
  subroutine writegain( Gain,iostatus )
  implicit none
     type(type_Gain)    , intent(in)  :: Gain
!
     integer(kind=LONG), intent(out) :: iostatus
     integer(kind=DOUBLE)            :: ifile
     integer(kind=LONG)              :: offset
     integer(kind=LONG)              :: Type
     integer(kind=LONG)              :: Size
!
     iostatus = 0
     call open_file_w(Gain%filename                    &
                      (1:len_trim(Gain%filename))      &
                       // char(0), ifile)
     if ( ifile .eq. 0 ) then
        iostatus = 1
     else
        offset = 0
        Type = ch1Type
        Size = 1*3
        call write_field( ifile, Gain%Variance_Choice,        &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Type = i4Type
        Size = 4
        call write_field( ifile, Gain%NbCol,                  &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Gain%NbLin,                  &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Gain%NbCol_PN,               &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Gain%NbLin_PN,               &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Gain%ColStart_PN,            &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Gain%ColEnd_PN,              &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Gain%LinStart_PN,            &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Gain%LinEnd_PN,              &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Gain%Nb_SUB_PN,              &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Gain%NbCol_SUB_PN,           &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Gain%NbLin_SUB_PN,           &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Type = r8Type
        Size = 8
        call write_field( ifile, Gain%Step,                   &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Gain%Sensitivity_Avg,        &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Type = i4Type
        Size = 4*Gain%Nb_SUB_PN
        call write_field( ifile, Gain%ColStart_SUB_PN,        &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Gain%ColEnd_SUB_PN,          &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Gain%LinStart_SUB_PN,        &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Gain%LinEnd_SUB_PN,          &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Type = r8Type
        Size = 8*Gain%NbCol*Gain%NbLin
        call write_field( ifile, Gain%PRNU,                   &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Gain%NbCol_PN*Gain%NbLin_PN
        call write_field( ifile, Gain%Gain_PN,                &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8
        call write_field( ifile, Gain%Gain_Avg_PN,            &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Gain%Gain_Std_PN,            &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Gain%Gain_Min_PN,            &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Gain%Gain_Max_PN,            &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Gain%NbCol_SUB_PN*Gain%NbLin_SUB_PN*Gain%Nb_SUB_PN
        call write_field( ifile, Gain%Gain_SUB_PN,            &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Gain%Nb_SUB_PN
        call write_field( ifile, Gain%Gain_Avg_SUB_PN,        &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Gain%Gain_Std_SUB_PN,        &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Gain%Gain_Min_SUB_PN,        &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Gain%Gain_Max_SUB_PN,        &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call close_file(Gain%filename              &
                        (1:len_trim(Gain%filename))&
                        // char(0), ifile)
     end if
     return
  end subroutine writegain
!
!
end module gain_type
