! precision_type.f90 --
!
!           Project: SPS_GENERIC
!           Authors: NOVELTIS/B.TOURNIER
!              Date: october 2009
!           Version: $Revision: 1.1 $
! Last modification: $Date: 2010-01-11 09:43:06 $
!
! precision -- Module
!
! * Purpose
!   Module for data type precision definitions.
!
!
module precision_type
!
! Integer type precisions.
  integer, parameter :: ONE        = 1
  integer, parameter :: SHORT      = 2
  integer, parameter :: LONG       = 4

! Real types precisions.
  integer, parameter :: SIMPLE     = 4
  integer, parameter :: DOUBLE     = 8

! Complex types precisions.
  integer, parameter :: SIMPLECPLX = 4
  integer, parameter :: DOUBLECPLX = 8

end module precision_type
