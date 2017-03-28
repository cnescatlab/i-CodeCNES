!!* performance_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: october 2013
!!
!> performance_type -- Module
!!
!! * Purpose
!!
!!   Module for performance type declaration and allocation.

module performance_type
   use precision_type
   use error_type
   use modinouttools
!
   implicit none
!
!
   public :: type_Performance   &
            ,alloc_Performance  &
            ,dalloc_Performance

!
   type :: type_Performance
     character(len=500)   ,dimension(:),allocatable:: filename
     integer(kind=LONG)                            :: Nband
     integer(kind=LONG)   ,dimension(:),allocatable:: SB_List
     character(len=100)                            :: Object_Type
     character(len=100)                            :: Analysis_Type
     character(len=3)                              :: Reference
     character(len=100)                            :: Comparison
     integer(kind=LONG)                            :: deg_polynom
     integer(kind=LONG)                            :: SlideFilter
     integer(kind=LONG)                            :: Col_Size_Min
     character(len=100)                            :: Matrix_Type
   end type type_Performance
!
!
   contains
!
!
   subroutine alloc_Performance( Performance )
   type(type_Performance)                        :: Performance
   integer(kind=LONG)                            :: ErrCode
   integer(kind=LONG)                            :: ioalloc
!
   ioalloc = 0
   allocate( Performance%filename(Performance%Nband) ,stat=ErrCode )
   if( ErrCode /= 0 ) ioalloc = ioalloc + 1
   allocate( Performance%SB_List(Performance%Nband)  ,stat=ErrCode )
   if( ErrCode /= 0 ) ioalloc = ioalloc + 1
   if( ioalloc /= 0 ) then
      write(*,*) 'alloc_Performance Error'
      call exit(1)
   end if
!
   return
   end subroutine alloc_Performance
!
!
   subroutine dalloc_Performance( Performance )
   type(type_Performance)                        :: Performance
!
   deallocate( Performance%filename )
   deallocate( Performance%SB_List )
!
   return
   end subroutine dalloc_Performance
!
!
end module performance_type
