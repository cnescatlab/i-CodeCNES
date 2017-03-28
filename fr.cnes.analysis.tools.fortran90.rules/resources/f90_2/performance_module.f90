!!# performance_module.f90 --
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
!> performance_module -- Module
!!
!! * Purpose
!!
!!     Module for Radiometric performance computations
!!
!! * Description
!!
!! * References
!
module performance_module
   use precision_type
   use error_type
   use spectrum_type
   use performance_type
!
   implicit none
!
!
   public :: performance_init            &
            ,collection_spectrum         &
            ,diff_collection_spectrum    &
            ,collection_spectrum_nc      &
            ,diff_collection_spectrum_nc &
            ,average_spectrum         
!
!
   contains
!
!
   subroutine performance_init( File_Perfo_Param, Performance_Param )
!
   implicit none
  character(len=*)       ,intent(in)                         :: File_Perfo_Param
   type(type_Performance),intent(out)                        :: Performance_Param
   integer(kind=LONG)                                        :: iFile
   integer(kind=LONG)                                        :: iPos
   integer(kind=LONG)                                        :: SB
!
   iFile = 10
   iPos  = 1
   write(*,'(a,a)') 'File_Perfo_Param',trim(File_Perfo_Param)
   open(iFile, file=File_Perfo_Param, status='old', err=999)
   iPos  = 2
   read(iFile,*,err=999) Performance_Param%Nband
   write(*,*) 'Nband  ',Performance_Param%Nband
   call alloc_Performance( Performance_Param )
   iPos  = 3
   read(iFile,*,err=999) (Performance_Param%SB_List(SB),&
                            SB=1,Performance_Param%Nband)
   write(*,*) 'SB_List(SB)',(Performance_Param%SB_List(SB),&
                            SB=1,Performance_Param%Nband)
   iPos  = 4
   read(iFile,*,err=999) Performance_Param%Object_Type
   write(*,*) 'Object_Type  ',Performance_Param%Object_Type
   iPos  = 5
   read(iFile,*,err=999) Performance_Param%Reference
   write(*,*) 'Reference  ',Performance_Param%Reference
   iPos  = 6
   read(iFile,*,err=999) Performance_Param%Analysis_Type
   write(*,*) 'Analysis_Type'  ,Performance_Param%Analysis_Type
   iPos  = 7
   read(iFile,*,err=999) Performance_Param%Comparison
   write(*,*) 'Comparison  ',Performance_Param%Comparison
   iPos  = 8
   read(iFile,*,err=999) Performance_Param%Matrix_Type
   write(*,*) 'Matrix_Type  ',Performance_Param%Matrix_Type
   iPos  = 9
   read(iFile,*,err=999) Performance_Param%deg_polynom
   write(*,*) 'deg_polynom  ',Performance_Param%deg_polynom
   iPos  = 10
   read(iFile,*,err=999) Performance_Param%SlideFilter
   write(*,*) 'SlideFilter  ',Performance_Param%SlideFilter
   iPos  = 11
   read(iFile,*,err=999) Performance_Param%Col_Size_Min
   write(*,*) 'Col_Size_Min  ',Performance_Param%Col_Size_Min
   do SB = 1, Performance_Param%Nband
      iPos  = 11 + SB
      read(iFile,'(a)',err=999) Performance_Param%filename(SB)
   write(*,*) 'filename(SB)  ',trim(Performance_Param%filename(SB))
   end do
   close(iFile)
!
   return
999 write(*,*) 'performance_init Fatal error',iPos
    call exit(1)
   end subroutine performance_init
!
!
   subroutine collection_spectrum( Nb_Spectrum,  &
                                   File_Spectrum,&
                                   Spectrum      )
   integer(kind=LONG)  ,intent(in)                          :: Nb_Spectrum
   character(len=*)    ,intent(in),dimension(Nb_Spectrum)   :: File_Spectrum
   type(type_Spectrum) ,intent(out),dimension(:),allocatable:: Spectrum
!
   integer(kind=LONG)                                       :: ErrCode
   integer(kind=LONG)                                       :: Nb
   integer(kind=LONG)                                       :: iPos
!
   allocate( Spectrum(Nb_Spectrum) )
!
!  lopp on the spectrum collection
   do Nb = 1, Nb_Spectrum
      iPos = Nb
      Spectrum(Nb)%filename = File_Spectrum(Nb)
      call readspectrum( Spectrum(Nb), ErrCode )
      if( ErrCode /= 0 ) then
         write(*,*) 'Spectrum Reading Error',ErrCode
         write(*,'(a)') File_Spectrum(Nb)(1:len_trim(File_Spectrum(Nb)))
         go to 999
      end if
   end do
!
   return
999 write(*,*) 'collection_spectrum Fatal error',iPos
   call exit(1)
!
   end subroutine collection_spectrum
!
!
   subroutine diff_collection_spectrum( Nb_Spectrum,      &
                                        File_Spectrum,    &
                                        Nb_Spectrum_ref,  &
                                        File_Spectrum_ref,&
                                        Spectrum          )
   integer(kind=LONG)  ,intent(in)                          :: Nb_Spectrum
   character(len=*)    ,intent(in),dimension(Nb_Spectrum)   :: File_Spectrum
   integer(kind=LONG)  ,intent(in)                          :: Nb_Spectrum_ref
   character(len=*)    ,intent(in),dimension(Nb_Spectrum)   :: File_Spectrum_ref
   type(type_Spectrum) ,intent(out),dimension(:),allocatable:: Spectrum
   type(type_Spectrum)                                      :: Spectrum_ref
   integer(kind=LONG)                                       :: ErrCode
   integer(kind=LONG)                                       :: Nb
   integer(kind=LONG)                                       :: iPos
!
   allocate( Spectrum(Nb_Spectrum) )
   if( Nb_Spectrum_ref == 1 ) then
      Spectrum_ref%filename = File_Spectrum_ref(1)
      call readspectrum( Spectrum_ref, ErrCode )
      if( ErrCode /= 0 ) then
         write(*,*) 'Spectrum ref Reading Error',ErrCode
         write(*,'(a)') File_Spectrum_ref(1)(1:len_trim(File_Spectrum_ref(1)))
         go to 999
      end if
   else
      if( Nb_Spectrum_ref /= Nb_Spectrum ) then
         write(*,*) ' Nb_Spectrum_ref must be equal to Nb_Spectrum'
         go to 999
      end if
   end if
!
!  lopp on the spectrum collection
   do Nb = 1, Nb_Spectrum
      Spectrum(Nb)%filename = File_Spectrum(Nb)
      call readspectrum( Spectrum(Nb), ErrCode )
      if( ErrCode /= 0 ) then
         write(*,*) 'Spectrum Reading Error',ErrCode
         write(*,'(a)') File_Spectrum(Nb)(1:len_trim(File_Spectrum(Nb)))
         go to 999
      end if
      if( Nb_Spectrum_ref /= 1 ) then
         Spectrum_ref%filename = File_Spectrum_ref(Nb)
         call readspectrum( Spectrum_ref, ErrCode )
         if( ErrCode /= 0 ) then
            write(*,*) 'Spectrum ref Reading Error',ErrCode
            write(*,'(a)') File_Spectrum_ref(Nb)(1:len_trim(File_Spectrum_ref(Nb)))
            go to 999
         end if
      end if
      if( Spectrum(Nb)%Type == 'C' ) then
            Spectrum(Nb)%Complex(1:Spectrum(Nb)%N_Sample) =     &
                   Spectrum(Nb)%Complex(1:Spectrum(Nb)%N_Sample)&
                 - Spectrum_ref%Complex(1:Spectrum_ref%N_Sample)
      else if( Spectrum(Nb)%Type == 'RI' ) then
            Spectrum(Nb)%Real_Part(1:Spectrum(Nb)%N_Sample) =     &
                   Spectrum(Nb)%Real_Part(1:Spectrum(Nb)%N_Sample)&
                 - Spectrum_ref%Real_Part(1:Spectrum_ref%N_Sample)
            Spectrum(Nb)%Imag_Part(1:Spectrum(Nb)%N_Sample) =     &
                   Spectrum(Nb)%Imag_Part(1:Spectrum(Nb)%N_Sample)&
                 - Spectrum_ref%Imag_Part(1:Spectrum_ref%N_Sample)
      else if( Spectrum(Nb)%Type == 'MA' ) then
            Spectrum(Nb)%Modulus(1:Spectrum(Nb)%N_Sample) =     &
                   Spectrum(Nb)%Modulus(1:Spectrum(Nb)%N_Sample)&
                 - Spectrum_ref%Modulus(1:Spectrum_ref%N_Sample)
            Spectrum(Nb)%Argument(1:Spectrum(Nb)%N_Sample) =     &
                   Spectrum(Nb)%Argument(1:Spectrum(Nb)%N_Sample)&
                 - Spectrum_ref%Argument(1:Spectrum_ref%N_Sample)
      else if( Spectrum(Nb)%Type == 'R'  ) then
            Spectrum(Nb)%Real_Part(1:Spectrum(Nb)%N_Sample) =     &
                   Spectrum(Nb)%Real_Part(1:Spectrum(Nb)%N_Sample)&
                 - Spectrum_ref%Real_Part(1:Spectrum_ref%N_Sample)
      else if( Spectrum(Nb)%Type == 'I'  ) then
            Spectrum(Nb)%Imag_Part(1:Spectrum(Nb)%N_Sample) =     &
                   Spectrum(Nb)%Imag_Part(1:Spectrum(Nb)%N_Sample)&
                 - Spectrum_ref%Imag_Part(1:Spectrum_ref%N_Sample)
      else if( Spectrum(Nb)%Type == 'M'  ) then
            Spectrum(Nb)%Modulus(1:Spectrum(Nb)%N_Sample) =     &
                   Spectrum(Nb)%Modulus(1:Spectrum(Nb)%N_Sample)&
                 - Spectrum_ref%Modulus(1:Spectrum_ref%N_Sample)
      else if( Spectrum(Nb)%Type == 'A'  ) then
            Spectrum(Nb)%Argument(1:Spectrum(Nb)%N_Sample) =     &
                   Spectrum(Nb)%Argument(1:Spectrum(Nb)%N_Sample)&
                 - Spectrum_ref%Argument(1:Spectrum_ref%N_Sample)
      else
            write(*,*) 'Spectrum(Nb)%Type Error'
            write(*,*) 'spectrum_difference Fatal Error'
            go to 999
      end if
      if( Nb_Spectrum_ref /= 1 ) then
         call dalloc_Spectrum( Spectrum_ref )
      end if
   end do
   if( Nb_Spectrum_ref == 1 ) then
      call dalloc_Spectrum( Spectrum_ref )
   end if
!
   return
999 write(*,*) 'diff_collection_spectrum Fatal error',iPos
   call exit(1)
!
   end subroutine diff_collection_spectrum
!
!
   subroutine collection_spectrum_nc( Nb_Spectrum,  &
                                      File_Spectrum,&
                                      Spectrum      )
   integer(kind=LONG)  ,intent(in)                          :: Nb_Spectrum
   character(len=*)    ,intent(in),dimension(Nb_Spectrum)   :: File_Spectrum
   type(type_Spectrum) ,intent(out),dimension(:),allocatable:: Spectrum
!
   integer(kind=LONG)                                       :: ErrCode
   integer(kind=LONG)                                       :: Nb
   integer(kind=LONG)                                       :: iPos
!
   allocate( Spectrum(Nb_Spectrum) )
!
!  lopp on the spectrum collection
   do Nb = 1, Nb_Spectrum
      iPos = Nb
      Spectrum(Nb)%filename = File_Spectrum(Nb)
      call readspectrum_netcdf( Spectrum(Nb), ErrCode )
      if( ErrCode /= 0 ) then
         write(*,*) 'Spectrum Reading Error',ErrCode
         write(*,'(a)') File_Spectrum(Nb)(1:len_trim(File_Spectrum(Nb)))
         go to 999
      end if
   end do
!
   return
999 write(*,*) 'collection_spectrum Fatal error',iPos
   call exit(1)
!
   end subroutine collection_spectrum_nc
!
!
   subroutine diff_collection_spectrum_nc( Nb_Spectrum,      &
                                           File_Spectrum,    &
                                           Nb_Spectrum_ref,  &
                                           File_Spectrum_ref,&
                                           Spectrum          )
   integer(kind=LONG)  ,intent(in)                          :: Nb_Spectrum
   character(len=*)    ,intent(in),dimension(Nb_Spectrum)   :: File_Spectrum
   integer(kind=LONG)  ,intent(in)                          :: Nb_Spectrum_ref
   character(len=*)    ,intent(in),dimension(Nb_Spectrum)   :: File_Spectrum_ref
   type(type_Spectrum) ,intent(out),dimension(:),allocatable:: Spectrum
   type(type_Spectrum)                                      :: Spectrum_ref
   integer(kind=LONG)                                       :: ErrCode
   integer(kind=LONG)                                       :: Nb
   integer(kind=LONG)                                       :: iPos
!
   allocate( Spectrum(Nb_Spectrum) )
   if( Nb_Spectrum_ref == 1 ) then
      Spectrum_ref%filename = File_Spectrum_ref(1)
      call readspectrum_netcdf( Spectrum_ref, ErrCode )
      if( ErrCode /= 0 ) then
         write(*,*) 'Spectrum ref Reading Error',ErrCode
         write(*,'(a)') File_Spectrum_ref(1)(1:len_trim(File_Spectrum_ref(1)))
         go to 999
      end if
   else
      if( Nb_Spectrum_ref /= Nb_Spectrum ) then
         write(*,*) ' Nb_Spectrum_ref must be equal to Nb_Spectrum'
         go to 999
      end if
   end if
!
!  lopp on the spectrum collection
   do Nb = 1, Nb_Spectrum
      Spectrum(Nb)%filename = File_Spectrum(Nb)
      call readspectrum_netcdf( Spectrum(Nb), ErrCode )
      if( ErrCode /= 0 ) then
         write(*,*) 'Spectrum Reading Error',ErrCode
         write(*,'(a)') File_Spectrum(Nb)(1:len_trim(File_Spectrum(Nb)))
         go to 999
      end if
      if( Nb_Spectrum_ref /= 1 ) then
         Spectrum_ref%filename = File_Spectrum_ref(Nb)
         call readspectrum_netcdf( Spectrum_ref, ErrCode )
         if( ErrCode /= 0 ) then
            write(*,*) 'Spectrum ref Reading Error',ErrCode
            write(*,'(a)') File_Spectrum_ref(Nb)(1:len_trim(File_Spectrum_ref(Nb)))
            go to 999
         end if
      end if
      if( Spectrum(Nb)%Type == 'C' ) then
            Spectrum(Nb)%Complex(1:Spectrum(Nb)%N_Sample) =     &
                   Spectrum(Nb)%Complex(1:Spectrum(Nb)%N_Sample)&
                 - Spectrum_ref%Complex(1:Spectrum_ref%N_Sample)
      else if( Spectrum(Nb)%Type == 'RI' ) then
            Spectrum(Nb)%Real_Part(1:Spectrum(Nb)%N_Sample) =     &
                   Spectrum(Nb)%Real_Part(1:Spectrum(Nb)%N_Sample)&
                 - Spectrum_ref%Real_Part(1:Spectrum_ref%N_Sample)
            Spectrum(Nb)%Imag_Part(1:Spectrum(Nb)%N_Sample) =     &
                   Spectrum(Nb)%Imag_Part(1:Spectrum(Nb)%N_Sample)&
                 - Spectrum_ref%Imag_Part(1:Spectrum_ref%N_Sample)
      else if( Spectrum(Nb)%Type == 'MA' ) then
            Spectrum(Nb)%Modulus(1:Spectrum(Nb)%N_Sample) =     &
                   Spectrum(Nb)%Modulus(1:Spectrum(Nb)%N_Sample)&
                 - Spectrum_ref%Modulus(1:Spectrum_ref%N_Sample)
            Spectrum(Nb)%Argument(1:Spectrum(Nb)%N_Sample) =     &
                   Spectrum(Nb)%Argument(1:Spectrum(Nb)%N_Sample)&
                 - Spectrum_ref%Argument(1:Spectrum_ref%N_Sample)
      else if( Spectrum(Nb)%Type == 'R'  ) then
            Spectrum(Nb)%Real_Part(1:Spectrum(Nb)%N_Sample) =     &
                   Spectrum(Nb)%Real_Part(1:Spectrum(Nb)%N_Sample)&
                 - Spectrum_ref%Real_Part(1:Spectrum_ref%N_Sample)
      else if( Spectrum(Nb)%Type == 'I'  ) then
            Spectrum(Nb)%Imag_Part(1:Spectrum(Nb)%N_Sample) =     &
                   Spectrum(Nb)%Imag_Part(1:Spectrum(Nb)%N_Sample)&
                 - Spectrum_ref%Imag_Part(1:Spectrum_ref%N_Sample)
      else if( Spectrum(Nb)%Type == 'M'  ) then
            Spectrum(Nb)%Modulus(1:Spectrum(Nb)%N_Sample) =     &
                   Spectrum(Nb)%Modulus(1:Spectrum(Nb)%N_Sample)&
                 - Spectrum_ref%Modulus(1:Spectrum_ref%N_Sample)
      else if( Spectrum(Nb)%Type == 'A'  ) then
            Spectrum(Nb)%Argument(1:Spectrum(Nb)%N_Sample) =     &
                   Spectrum(Nb)%Argument(1:Spectrum(Nb)%N_Sample)&
                 - Spectrum_ref%Argument(1:Spectrum_ref%N_Sample)
      else
            write(*,*) 'Spectrum(Nb)%Type Error'
            write(*,*) 'spectrum_difference Fatal Error'
            go to 999
      end if
      if( Nb_Spectrum_ref /= 1 ) then
         call dalloc_Spectrum( Spectrum_ref )
      end if
   end do
   if( Nb_Spectrum_ref == 1 ) then
      call dalloc_Spectrum( Spectrum_ref )
   end if
!
   return
999 write(*,*) 'diff_collection_spectrum_nc Fatal error',iPos
   call exit(1)
!
   end subroutine diff_collection_spectrum_nc
!
!
   subroutine average_spectrum( Nb_Spectrum,     &
                                Spectrum,        &
                                Spectrum_average )
   integer(kind=LONG)  ,intent(in)                          :: Nb_Spectrum
   type(type_Spectrum) ,intent(inout),dimension(Nb_Spectrum):: Spectrum
   type(type_Spectrum) ,intent(out)                         :: Spectrum_average
   integer(kind=LONG)                                       :: Nb
   integer(kind=LONG)                                       :: iPos
!
   do Nb = 1, Nb_Spectrum
      if( Nb == 1 ) then
         call spectrum_transfert( Spectrum(1), Spectrum_average )
      else
         if( Spectrum_average%Type == 'C' ) then
            Spectrum_average%Complex(1:Spectrum_average%N_Sample) =     &
                   Spectrum_average%Complex(1:Spectrum_average%N_Sample)&
                 + (Spectrum(Nb)%Complex(1:Spectrum(Nb)%N_Sample)       &
                   /real(Nb_Spectrum,DOUBLE))
         else if( Spectrum_average%Type == 'RI' ) then
            Spectrum_average%Real_Part(1:Spectrum_average%N_Sample) =     &
                   Spectrum_average%Real_Part(1:Spectrum_average%N_Sample)&
                 + (Spectrum(Nb)%Real_Part(1:Spectrum(Nb)%N_Sample)       &
                   /real(Nb_Spectrum,DOUBLE))
            Spectrum_average%Imag_Part(1:Spectrum_average%N_Sample) =     &
                   Spectrum_average%Imag_Part(1:Spectrum_average%N_Sample)&
                 + (Spectrum(Nb)%Imag_Part(1:Spectrum(Nb)%N_Sample)       &
                   /real(Nb_Spectrum,DOUBLE))
         else if( Spectrum_average%Type == 'MA' ) then
            Spectrum_average%Modulus(1:Spectrum_average%N_Sample) =     &
                   Spectrum_average%Modulus(1:Spectrum_average%N_Sample)&
                 + (Spectrum(Nb)%Modulus(1:Spectrum(Nb)%N_Sample)       &
                   /real(Nb_Spectrum,DOUBLE))
            Spectrum_average%Argument(1:Spectrum_average%N_Sample) =     &
                   Spectrum_average%Argument(1:Spectrum_average%N_Sample)&
                 + (Spectrum(Nb)%Argument(1:Spectrum(Nb)%N_Sample)       &
                   /real(Nb_Spectrum,DOUBLE))
         else if( Spectrum_average%Type == 'R'  ) then
            Spectrum_average%Real_Part(1:Spectrum_average%N_Sample) =     &
                   Spectrum_average%Real_Part(1:Spectrum_average%N_Sample)&
                 + (Spectrum(Nb)%Real_Part(1:Spectrum(Nb)%N_Sample)       &
                   /real(Nb_Spectrum,DOUBLE))
         else if( Spectrum_average%Type == 'I'  ) then
            Spectrum_average%Imag_Part(1:Spectrum_average%N_Sample) =     &
                   Spectrum_average%Imag_Part(1:Spectrum_average%N_Sample)&
                 + (Spectrum(Nb)%Imag_Part(1:Spectrum(Nb)%N_Sample)       &
                   /real(Nb_Spectrum,DOUBLE))
         else if( Spectrum_average%Type == 'M'  ) then
            Spectrum_average%Modulus(1:Spectrum_average%N_Sample) =     &
                   Spectrum_average%Modulus(1:Spectrum_average%N_Sample)&
                 + (Spectrum(Nb)%Modulus(1:Spectrum(Nb)%N_Sample)       &
                   /real(Nb_Spectrum,DOUBLE))
         else if( Spectrum_average%Type == 'A'  ) then
            Spectrum_average%Argument(1:Spectrum_average%N_Sample) =     &
                   Spectrum_average%Argument(1:Spectrum_average%N_Sample)&
                 + (Spectrum(Nb)%Argument(1:Spectrum(Nb)%N_Sample)       &
                   /real(Nb_Spectrum,DOUBLE))
         else
            write(*,*) 'Spectrum(Nb)%Type Error'
            write(*,*) 'spectrum_transfert Fatal Error'
            go to 999
         end if
      end if
      call dalloc_Spectrum( Spectrum(Nb) )
   end do
!
   return
999 write(*,*) 'average_spectrum Fatal error',iPos
   call exit(1)
!
   end subroutine average_spectrum
!
!
end module performance_module
