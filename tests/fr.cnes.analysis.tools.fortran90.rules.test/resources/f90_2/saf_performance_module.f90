!!# saf_performance_module.f90 --
!!#
!!#           Project: SPS_GENERIC
!!#           Authors: NOVELTIS/B.TOURNIER
!!#              Date: october 2013
!!#
!!# Language:  F90
!!# Standards: Noveltis
!!#
!!# --
!!#
!! 
!> saf_performance_module -- Module
!!
!! * Purpose
!!
!!     Module for Saf performance computations
!!
!! * Description

module saf_performance_module
   use precision_type
   use error_type
   use constantes_type
   use saf_type
!
   implicit none
!
!
   public :: collection_saf          &
!
!
   contains
!
!
   subroutine collection_saf( Nb_Saf,  &
                              File_Saf,&
                              Saf      )
   integer(kind=LONG)      ,intent(in)                          :: Nb_Saf
   character(len=*)        ,intent(in),dimension(Nb_Spectrum)   :: File_Saf
   type(type_Saf)          ,intent(out),dimension(:),allocatable:: Saf
!
   integer(kind=LONG)                                           :: ErrCode
   integer(kind=LONG)                                           :: Nb
   integer(kind=LONG)                                           :: iPos
!
   allocate( Saf(Nb_Saf) )
!
!  lopp on the saf collection
   do Nb = 1, Nb_Saf
      iPos = Nb
      Saf(Nb)%filename = File_Saf(Nb)
      call readsaf( Saf(Nb), ErrCode )
      if( ErrCode /= 0 ) then
         write(*,*) 'Saferogram Reading Error',ErrCode
         write(*,'(a)') File_Saf(Nb)(1:len_trim(File_Saf(Nb)))
         go to 999
      end if
   end do
!
   return
999 write(*,*) 'collection_saf Fatal error',iPos
   call exit(1)
!
   end subroutine collection_saf
!
!
end module saf_performance_module
