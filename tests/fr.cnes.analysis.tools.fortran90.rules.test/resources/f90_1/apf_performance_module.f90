!!# apf_performance_module.f90 --
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
!> apf_performance_module -- Module
!!
!! * Purpose
!!
!!     Module for Apf performance computations
!!
!! * Description

module apf_performance_module
   use precision_type
   use error_type
   use constantes_type
   use apf_type
!
   implicit none
!
!
   public :: collection_apf          &
!
!
   contains
!
!
   subroutine collection_apf( Nb_Apf,  &
                              File_Apf,&
                              Apf      )
   integer(kind=LONG)      ,intent(in)                          :: Nb_Apf
   character(len=*)        ,intent(in),dimension(Nb_Spectrum)   :: File_Apf
   type(type_Apf)          ,intent(out),dimension(:),allocatable:: Apf
!
   integer(kind=LONG)                                           :: ErrCode
   integer(kind=LONG)                                           :: Nb
   integer(kind=LONG)                                           :: iPos
!
   allocate( Apf(Nb_Apf) )
!
!  lopp on the apf collection
   do Nb = 1, Nb_Apf
      iPos = Nb
      Apf(Nb)%filename = File_Apf(Nb)
      call readapf( Apf(Nb), ErrCode )
      if( ErrCode /= 0 ) then
         write(*,*) 'Apferogram Reading Error',ErrCode
         write(*,'(a)') File_Apf(Nb)(1:len_trim(File_Apf(Nb)))
         go to 999
      end if
   end do
!
   return
999 write(*,*) 'collection_apf Fatal error',iPos
   call exit(1)
!
   end subroutine collection_apf
!
!
end module apf_performance_module
