!!# interf_performance_module.f90 --
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
!> interf_performance_module -- Module
!!
!! * Purpose
!!
!!     Module for Interferogram performance computations
!!
!! * Description

module interf_performance_module
   use precision_type
   use error_type
   use constantes_type
   use interf_perfo_stats_type
   use interferogram_type
   use zpd_type
   use nzpd_module
   use math_module
   use statistique_module
   use fft_module
!
   implicit none
!
!
   public :: collection_interferogram    &
            ,collection_interferogram_nc &
            ,interferogram_stats
!
!
   contains
!
!
   subroutine collection_interferogram( Nb_Interf,  &
                                        File_Interf,&
                                        Interf      )
   integer(kind=LONG)      ,intent(in)                          :: Nb_Interf
   character(len=*)        ,intent(in),dimension(Nb_Interf)     :: File_Interf
   type(type_Interferogram),intent(out),dimension(:),allocatable:: Interf
!
   integer(kind=LONG)                                           :: ErrCode
   integer(kind=LONG)                                           :: Nb
   integer(kind=LONG)                                           :: iPos
!
   allocate( Interf(Nb_Interf) )
!
!  lopp on the interferogram collection
   do Nb = 1, Nb_Interf
      iPos = Nb
      Interf(Nb)%filename = File_Interf(Nb)
      call readinterferogram( Interf(Nb), ErrCode )
      if( ErrCode /= 0 ) then
         write(*,*) 'Interferogram Reading Error',ErrCode
         write(*,'(a)') File_Interf(Nb)(1:len_trim(File_Interf(Nb)))
         go to 999
      end if
!
!     Interferogram type conversion type R forced
      if( Interf(Nb)%Type == 'C' ) then
         allocate( Interf(Nb)%Real_Part(Interf(Nb)%N_Sample) )
         Interf(Nb)%Real_Part(1:Interf(Nb)%N_Sample)  = &
             dreal( Interf(Nb)%Complex(1:Interf(Nb)%N_Sample) )
         deallocate( Interf(Nb)%Complex )
      else if( Interf(Nb)%Type == 'RI' ) then
         deallocate( Interf(Nb)%Imag_Part )
      else if( Interf(Nb)%Type == 'R'  ) then
!        nothing to do
      else
         write(*,*) ' Wrong Interferogram Type must be C RI R'
         go to 999
      end if
      Interf(Nb)%Type = 'R'
   end do
!
   return
999 write(*,*) 'collection_interferogram Fatal error',iPos
   call exit(1)
!
   end subroutine collection_interferogram
!
!
   subroutine collection_interferogram_nc( Nb_Interf,  &
                                           File_Interf,&
                                           Interf      )
   integer(kind=LONG)      ,intent(in)                          :: Nb_Interf
   character(len=*)        ,intent(in),dimension(Nb_Interf)     :: File_Interf
   type(type_Interferogram),intent(out),dimension(:),allocatable:: Interf
!
   integer(kind=LONG)                                           :: ErrCode
   integer(kind=LONG)                                           :: Nb
   integer(kind=LONG)                                           :: iPos
!
   allocate( Interf(Nb_Interf) )
!
!  lopp on the interferogram collection
   do Nb = 1, Nb_Interf
      iPos = Nb
      Interf(Nb)%filename = File_Interf(Nb)
      call readinterferogram_netcdf( Interf(Nb), ErrCode )
      if( ErrCode /= 0 ) then
         write(*,*) 'Interferogram Reading Error',ErrCode
         write(*,'(a)') File_Interf(Nb)(1:len_trim(File_Interf(Nb)))
         go to 999
      end if
!
!     Interferogram type conversion type R forced
      if( Interf(Nb)%Type == 'C' ) then
         allocate( Interf(Nb)%Real_Part(Interf(Nb)%N_Sample) )
         Interf(Nb)%Real_Part(1:Interf(Nb)%N_Sample)  = &
             dreal( Interf(Nb)%Complex(1:Interf(Nb)%N_Sample) )
         deallocate( Interf(Nb)%Complex )
      else if( Interf(Nb)%Type == 'RI' ) then
         deallocate( Interf(Nb)%Imag_Part )
      else if( Interf(Nb)%Type == 'R'  ) then
!        nothing to do
      else
         write(*,*) ' Wrong Interferogram Type must be C RI R'
         go to 999
      end if
      Interf(Nb)%Type = 'R'
   end do
!
   return
999 write(*,*) 'collection_interferogram_nc Fatal error',iPos
   call exit(1)
!
   end subroutine collection_interferogram_nc
!
!
   subroutine interferogram_stats( Nb_Interf,         &
                                   Interf,            &
                                   Interf_Perfo_Stats )
   integer(kind=LONG)      ,intent(in)                          :: Nb_Interf
   type(type_Interferogram),intent(inout),dimension(Nb_Interf)  :: Interf
   type(type_Interf_Perfo_Stats),intent(out)                    :: Interf_Perfo_Stats
   type(type_Zpd)                                               :: Zpd
   integer(kind=LONG)                                           :: iPos
   integer(kind=LONG)                                           :: Nb
   integer(kind=LONG)                                           :: Ns
   integer(kind=LONG)      ,dimension(:),allocatable            :: NZpd
   integer(kind=LONG)                                           :: NZpdMin
   integer(kind=LONG)                                           :: NSampleMin
   integer(kind=LONG)                                           :: N_Sample_Half
   integer(kind=LONG)                                           :: N_Sample_Min
   real(kind=DOUBLE)       ,dimension(:),allocatable            :: Base_Line
   real(kind=DOUBLE)       ,dimension(:),allocatable            :: X
!
   iPos = 1
!
!  allocation
   allocate( NZpd(Nb_Interf) )
   allocate( Base_Line(Nb_Interf) )
   do Nb = 1, Nb_Interf
      call nzpd_barycentre( Zpd, Interf(Nb) )
      NZpd(Nb) = Zpd%Nzpd
   end do
!
!  Min Position
   NZpdMin = minval( NZpd(1:Nb_Interf) )
   N_Sample_Half = NZpdMin-1
   N_Sample_Min = 2*N_Sample_Half + 1
   write(*,*) 'NZpd Min Position', NZpdMin
!
!  check the collection consistency
   NSampleMin = minval( Interf(1:Nb_Interf)%N_Sample )
   if( NSampleMin < N_Sample_Min ) then
      write(*,*) ' Interferogram N_Sample too small', NSampleMin
      go to 999
   end if
!
!  Centering on the NZPD
   do Nb = 1, Nb_Interf
      Interf(Nb)%Real_Part(1:N_Sample_Min) = &
                  Interf(Nb)%Real_Part(NZpd(Nb)-N_Sample_Half:Nzpd(Nb)+N_Sample_Half)
      Interf(Nb)%N_Sample = N_Sample_Min
   end do
!
!  centering on the baseline
   do Nb = 1, Nb_Interf
      Base_Line(Nb) = ( sum( Interf(Nb)%Real_Part(1:N_Sample_Half-100) )&
                        / real(N_Sample_Half-100,DOUBLE) )              &
                    + ( sum( Interf(Nb)%Real_Part(N_Sample_Half+101:    &
                                            (2*N_Sample_Half)+1) )      &
                        / real(N_Sample_Half-100,DOUBLE) )
      Interf(Nb)%Real_Part(1:Interf(Nb)%N_Sample) = &
                Interf(Nb)%Real_Part(1:Interf(Nb)%N_Sample)  - Base_Line(Nb)
   end do
!
!  Interferogram statistics
   Interf_Perfo_Stats%N_Sample = Interf(1)%N_Sample
   Interf_Perfo_Stats%dTime    = Interf(1)%dTime
   Interf_Perfo_Stats%dOpd     = Interf(1)%dOpd
   call alloc_Interf_Perfo_Stats( Interf_Perfo_Stats )
   Interf_Perfo_Stats%Time(1:Interf_Perfo_Stats%N_Sample) = &
                         Interf(1)%Time(1:Interf(1)%N_Sample)
   Interf_Perfo_Stats%Opd(1:Interf_Perfo_Stats%N_Sample) = &
                         Interf(1)%Opd(1:Interf(1)%N_Sample)
   allocate( X(Interf_Perfo_Stats%N_Sample) )
   do Ns = 1, Interf(1)%N_Sample
      do Nb = 1, Nb_Interf
         X(Nb) = Interf(Nb)%Real_Part(Ns)
      end do
      call statistics( Nb_Interf,                   &
                       X,                           &
                       Interf_Perfo_Stats%Avg(Ns),  &
                       Interf_Perfo_Stats%Std(Ns),  &
                       Interf_Perfo_Stats%Min(Ns),  &
                       Interf_Perfo_Stats%Max(Ns),  &
                       Interf_Perfo_Stats%NsMin(Ns),&
                       Interf_Perfo_Stats%NsMax(Ns) )
   end do
!
   deallocate( X )
   deallocate( NZpd )
   deallocate( Base_Line )
   return
999 write(*,*) 'interferogram_stats Fatal error',iPos
   call exit(1)
!
   end subroutine interferogram_stats
!
!
end module interf_performance_module
