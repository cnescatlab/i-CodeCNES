! decim_one_fir_type.f90 --
!
!           Project: SPS_GENERIC
!           Authors: NOVELTIS/B.TOURNIER
!              Date: october 2009
!           Version: $Revision: 1.1 $
! Last modification: $Date: 2011-04-12 08:32:32 $
!
! type_decim_one_fir -- Module
!
! * Purpose
!   Module for type decimation parameters declaration and allocation.
!
module decim_one_fir_type
   use precision_type
   use error_type
!
   implicit none
!
!
   public :: type_Decim_One_fir,  &
             alloc_Decim_One_fir, &
             dalloc_Decim_One_fir
!
   type :: type_Decim_One_fir
     character(len=500)                               :: filename
     integer(kind=LONG)                               :: Ns_Decim
     integer(kind=LONG)                               :: Ns_Fft
     integer(kind=LONG)                               :: Decim_Factor_1
     integer(kind=LONG)                               :: N_Fir_1
     integer(kind=LONG)                               :: Nc_Fir_1
     integer(kind=LONG)                               :: N_Sample_1
     real(kind=DOUBLE)   ,dimension(:)  ,allocatable  :: Fir_1
     real(kind=DOUBLE)                                :: WnRef
     integer(kind=LONG)                               :: Nvdi
     real(kind=DOUBLE)                                :: dWn
     real(kind=DOUBLE)                                :: OpdMax
     integer(kind=LONG)                               :: N_Shift
   end type type_Decim_One_fir
!
   contains
!
!
   subroutine alloc_Decim_One_fir( Decim_One_fir )
   implicit none
     type(type_Decim_One_fir), intent(inout)          :: Decim_One_fir
     integer(kind=LONG)                               :: ErrCode
     integer(kind=LONG)                               :: ioalloc
!
     ioalloc = 0
     allocate( Decim_One_fir%Fir_1(Decim_One_fir%N_Fir_1),  stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     if( ioalloc /= 0 ) then
        write(0,*) 'allocation Decim_One_fir Error'
        write(0,*) 'Decim_One_fir: fatal error'
        call exit(1)
     end if
     return
   end subroutine alloc_Decim_One_fir
!
!
   subroutine dalloc_Decim_One_fir( Decim_One_fir )
   implicit none
     type(type_Decim_One_fir), intent(inout)              :: Decim_One_fir
!
     deallocate( Decim_One_fir%Fir_1 )
!
     return
   end subroutine dalloc_Decim_One_fir
!
!
end module decim_one_fir_type
