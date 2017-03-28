!!* interferogram_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: october 2009
!!*           Version: $Revision: 1.6 $
!!* Last modification: $Date: 2011-06-22 10:06:27 $
!!
!> type_interferogram -- Module
!!
!! * Purpose
!!
!!   Module for type interferogram declaration and allocation.
!!
!! * Description
!!      
!!     This module defines the interferogram type and allows its allocation and declaration. 
!!
!! * Sub-routines and functions
!!
!!     * type_Interferogram          : type for declaration and allocation of Interferogram
!!     * type_Interferogram_subfov   : type for declaration and allocation of Interferogram by sub-field of view
!!     * type_Interferogram_sb       : type for declaration and allocation of Interferogram by spectral band
!!     * type_Interferogram_for      : type for declaration and allocation of Interferogram by field of regard
!!     * type_Interferogram_line     : type for declaration and allocation of Interferogram by line
!!     * type_Interferogram_full     : type for declaration and allocation of full Interferogram
!!     * type_Interferogram_full_index: type for declaration and allocation of Interferogram indexes
!!     * alloc_Interferogram    : type_Interferogram allocation
!!     * dalloc_Interferogram   : type_Interferogram deallocation
!!     * alloc_Interferogram_subfov : type_Interferogram_subfov allocation
!!     * dalloc_Interferogram_subfov: type_Interferogram_subfov deallocation
!!     * alloc_Interferogram_sb     : type_Interferogram_sb  allocation
!!     * dalloc_Interferogram_sb    : type_Interferogram_sb  deallocation
!!     * alloc_Interferogram_fov    : type_Interferogram_fov allocation
!!     * dalloc_Interferogram_fov   : type_Interferogram_fov deallocation
!!     * alloc_Interferogram_for    : type_Interferogram_for allocation
!!     * dalloc_Interferogram_for   : type_Interferogram_for deallocation
!!     * alloc_Interferogram_line   : type_Interferogram_line allocation
!!     * dalloc_Interferogram_line  : type_Interferogram_line deallocation
!!     * Interferogram_Basis    : computes interferogram base in Time and Opd
!!     * Interf_Header_Transfer : defines interferogram header
!!     * readinterferogram      : type_Interferogram reading routine
!!     * writeinterferogram     : type_Interferogram writing routine
!!     * writeinterferogram_netcdf : type_Interferogram writing routine (netCDF format)
!!     * write_interferogram_data  : type_Interferogram writing routine (netCDF format)
!!     * init_interferogram_data   : creation and initialization of the netcdf file
!!     * readinterferogram_netcdf  : type_Interferogram reading routine (netCDF format)
!!     * read_interferogram_data   : type_Interferogram reading routine (netCDF format)
!!     * get_interferogram_data    : get global data from the netcdf file
!!     * check_indices_Interferogram_full : checks indices of the compounds type to write into output file
!!
!! * References
!!
!! modified 19.07.2013 (JD) netCDF-4 interfaces


module interferogram_type
   use precision_type
   use error_type
   use modinouttools
   use spect_interf_index_type
   use modionetcdf
   use netcdf
!
   implicit none
!
!
   public :: type_Interferogram   &
            ,type_Interferogram_subfov &
            ,type_Interferogram_sb     &
            ,type_Interferogram_fov    &
            ,type_Interferogram_for    &
            ,type_Interferogram_line   &
            ,type_Interferogram_full   &
            ,type_Interferogram_full_index &
            ,alloc_Interferogram   &
            ,dalloc_Interferogram  &
            ,alloc_Interferogram_subfov  &
            ,dalloc_Interferogram_subfov &
            ,alloc_Interferogram_sb  &
            ,dalloc_Interferogram_sb &
            ,alloc_Interferogram_fov  &
            ,dalloc_Interferogram_fov &
            ,alloc_Interferogram_for  &
            ,dalloc_Interferogram_for &
            ,alloc_Interferogram_line  &
            ,dalloc_Interferogram_line &
            ,dalloc_Interferogram_full &
            ,alloc_Interferogram_full_index &
            ,dalloc_Interferogram_full_index &
            ,Interferogram_Basis   &
            ,Interf_Header_Transfer&
            ,readinterferogram     &
            ,writeinterferogram    &
            ,writeinterferogram_netcdf &
            ,readinterferogram_netcdf &
            ,check_indices_Interferogram_full
!
   type :: type_Interferogram
     type(type_spect_interf_index)                    :: TopLevelIn ! < interferogram top level indexes
     character(len=2)                                 :: Type !< interferogram type
     character(len=500)                               :: filename !< interferogram file name
     integer(kind=LONG)                               :: N_Sample !< samples number 
     integer(kind=LONG)                               :: N_Top !< Rpd samples number 
     real(kind=DOUBLE)                                :: FieldMeanAngle !< field mean angle
     real(kind=DOUBLE)                                :: TimeMax !< Time maximum (s)
     real(kind=DOUBLE)                                :: dTime !< Time sampling (s)
     real(kind=DOUBLE)                                :: OpdMax !< OPD maximum (m)
     real(kind=DOUBLE)                                :: dOpd !< OPD sampling (m)
     real(kind=DOUBLE)    , dimension(:), allocatable :: Time !< Time base - Time(1:N_Sample) (s)
     real(kind=DOUBLE)    , dimension(:), allocatable :: Time_Top !< Time base provided by Rpd laser (s) - Time_Top(1:N_Sample)
     real(kind=DOUBLE)    , dimension(:), allocatable :: Opd !< Optical Path Difference base (m) - Opd(1:N_Sample)
     real(kind=DOUBLE)    , dimension(:), allocatable :: Opd_Top !< OPD base provided by Rpd laser (m) - Opd_Top(1:N_Sample)
     complex(kind=DOUBLE) , dimension(:), allocatable :: Complex !< complex interferogram - Complex(1:N_Sample)
     real(kind=DOUBLE)    , dimension(:), allocatable :: Real_Part !< interferogram real part - Real_Part(1:N_Sample)
     real(kind=DOUBLE)    , dimension(:), allocatable :: Imag_Part !< interferogram imaginary part -  Imag_Part(1:N_Sample)
     real(kind=DOUBLE)    , dimension(:), allocatable :: Modulus !< interferogram modulus part - Modulus(1:N_Sample)
     real(kind=DOUBLE)    , dimension(:), allocatable :: Argument !< interferogram argument part - Argument(1:N_Sample)
   end type type_Interferogram
!
   type :: type_Interferogram_subfov
     integer(kind=LONG) :: NsubFoV ! < sub-field of view number
     integer(kind=LONG),dimension(:),allocatable :: SubFov !< sub fov
     type(type_Interferogram),dimension(:),allocatable :: Interferogram !< type Interferogram
  end type type_Interferogram_subfov
!
!
   type :: type_Interferogram_sb
     integer(kind=LONG) :: Nband ! < number of spectral band
     integer(kind=LONG),dimension(:),allocatable :: Band    !< spectral band
     type(type_Interferogram_subfov),dimension(:),allocatable :: Interferogram_subfov ! < type Interferogram_subfov
   end type type_Interferogram_sb
!
!
   type :: type_Interferogram_fov
     integer(kind=LONG) :: NcolFov ! < number of column of the FoV matrix
     integer(kind=LONG) :: NlinFov ! < number of line of the FoV matrix
     integer(kind=LONG),dimension(:),allocatable  :: FovCol ! < FoV column
     integer(kind=LONG),dimension(:),allocatable  :: FovLin ! < FoV line
     type(type_Interferogram_sb),dimension(:,:),allocatable :: Interferogram_sb ! < type Interferogram_sb
   end type type_Interferogram_fov
!
!
   type :: type_Interferogram_for
     integer(kind=LONG) :: NFor ! < field of view number
     integer(kind=LONG),dimension(:),allocatable  :: For ! < field of regard
     type(type_Interferogram_fov),dimension(:),allocatable :: Interferogram_fov ! < type Interferogram_fov
   end type type_Interferogram_for
!
!
   type :: type_Interferogram_line
     integer(kind=LONG) :: Nline ! < number of line of the product
     integer(kind=LONG),dimension(:),allocatable      :: Line ! < line
     type(type_Interferogram_for),dimension(:),allocatable :: Interferogram_for ! < type Interferogram_for
   end type type_Interferogram_line
!
   type :: type_Interferogram_full
     type(type_header)       :: header_Interferogram ! < type header
     character(len=2)        :: Type !< Interferogram type
     character(len=2)        :: Target !< target type
     type(type_Interferogram_line):: Interferogram_line ! < type Interferogram_line
   end type type_Interferogram_full
!
   type :: type_Interferogram_full_index
     integer(kind=LONG), dimension(:), allocatable :: indices_line
     integer(kind=LONG), dimension(:), allocatable :: indices_for
     integer(kind=LONG), dimension(:), allocatable :: indices_fovcol
     integer(kind=LONG), dimension(:), allocatable :: indices_fovlin
     integer(kind=LONG), dimension(:), allocatable :: indices_band
     integer(kind=LONG), dimension(:), allocatable :: indices_subfov
     integer(kind=LONG), dimension(2) :: index_line   !< start end of index line
     integer(kind=LONG), dimension(2) :: index_for    !< start end of index for
     integer(kind=LONG), dimension(2) :: index_fovcol !< start end of index fovcol
     integer(kind=LONG), dimension(2) :: index_fovlin !< start end of index fovlin
     integer(kind=LONG), dimension(2) :: index_band   !< start end of index band
     integer(kind=LONG), dimension(2) :: index_subfov !< start end of index subfov
     integer(kind=LONG), dimension(2) :: index_Interferogram !< start end of index Interferogram
   end type type_Interferogram_full_index
!
   contains
!
!
   subroutine alloc_Interferogram( Interferogram )
     implicit none
     type(type_Interferogram), intent(inout) :: Interferogram
     integer(kind=LONG)                      :: ErrCode
!
     allocate( Interferogram%Time(Interferogram%N_Sample),         stat=ErrCode )
     allocate( Interferogram%Time_Top(Interferogram%N_Sample),     stat=ErrCode )
     allocate( Interferogram%Opd(Interferogram%N_Sample),          stat=ErrCode )
     allocate( Interferogram%Opd_Top(Interferogram%N_Sample),      stat=ErrCode )
     if( Interferogram%Type(1:len_trim(Interferogram%Type))      == 'C'  ) then
        allocate( Interferogram%Complex(Interferogram%N_Sample),   stat=ErrCode )
     else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'RI' ) then
        allocate( Interferogram%Real_Part(Interferogram%N_Sample),   stat=ErrCode )
        allocate( Interferogram%Imag_Part(Interferogram%N_Sample),   stat=ErrCode )
     else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'MA' ) then
        allocate( Interferogram%Modulus(Interferogram%N_Sample),   stat=ErrCode )
        allocate( Interferogram%Argument(Interferogram%N_Sample),   stat=ErrCode )
     else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'R'  ) then
        allocate( Interferogram%Real_Part(Interferogram%N_Sample),   stat=ErrCode )
     else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'I'  ) then
        allocate( Interferogram%Imag_Part(Interferogram%N_Sample),   stat=ErrCode )
     else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'M'  ) then
        allocate( Interferogram%Modulus(Interferogram%N_Sample),   stat=ErrCode )
     else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'A'  ) then
        allocate( Interferogram%Argument(Interferogram%N_Sample),   stat=ErrCode )
     else
        write(*,*) 'Interferogram Type Error',Interferogram%Type
        write(*,*) 'alloc_Interferogram Fatal Error'
        call exit(1)
     end if
!
     if (ErrCode > 0) then
        write(0,*) 'allocation Interferogram Error'
        write(0,*) 'Interferogram: fatal error'
        call Err_outputErrorMessage(0)
        call exit(1)
     end if
     return
   end subroutine alloc_Interferogram
!
!
   subroutine dalloc_Interferogram( Interferogram )
   implicit none
     type(type_Interferogram), intent(inout) :: Interferogram
!
     deallocate( Interferogram%Time )
     deallocate( Interferogram%Time_Top )
     deallocate( Interferogram%Opd )
     deallocate( Interferogram%Opd_Top )
     if( Interferogram%Type(1:len_trim(Interferogram%Type))      == 'C'  ) then
        deallocate( Interferogram%Complex )
     else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'RI' ) then
        deallocate( Interferogram%Real_Part )
        deallocate( Interferogram%Imag_Part )
     else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'MA' ) then
        deallocate( Interferogram%Modulus )
        deallocate( Interferogram%Argument )
     else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'R'  ) then
        deallocate( Interferogram%Real_Part )
     else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'I'  ) then
        deallocate( Interferogram%Imag_Part )
     else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'M'  ) then
        deallocate( Interferogram%Modulus )
     else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'A'  ) then
        deallocate( Interferogram%Argument )
     else
        write(*,*) 'Interferogram Type Error',Interferogram%Type
        write(*,*) 'dalloc_Interferogram Fatal Error'
        call exit(1)
     end if
!
     return
   end subroutine dalloc_Interferogram
!
!
   subroutine alloc_Interferogram_subfov( Interferogram_subfov )
   implicit none
     type(type_Interferogram_subfov), intent(inout)     :: Interferogram_subfov
     integer(kind=LONG)                            :: ErrCode
!
     allocate( Interferogram_subfov%Interferogram( Interferogram_subfov%NsubFoV ),  stat=ErrCode )
     allocate( Interferogram_subfov%SubFov( Interferogram_subfov%NsubFoV ),  stat=ErrCode )
     if( ErrCode > 0 ) then
        write(*,*) 'allocation Interferogram_subfov Error',ErrCode
        write(*,*) 'type_Interferogram_subfov: fatal error'
        call exit(1)
     end if
! defaut initialisation
     Interferogram_subfov%SubFov(:) = 0
     return
   end subroutine alloc_Interferogram_subfov
!
!
   subroutine dalloc_Interferogram_subfov( Interferogram_subfov )
   implicit none
     type(type_Interferogram_subfov), intent(inout) :: Interferogram_subfov
!
     deallocate( Interferogram_subfov%Interferogram )
     deallocate( Interferogram_subfov%SubFov)
     return
   end subroutine dalloc_Interferogram_subfov
!
!
   subroutine alloc_Interferogram_sb( Interferogram_sb )
   implicit none
     type(type_Interferogram_sb), intent(inout)     :: Interferogram_sb
     integer(kind=LONG)                        :: ErrCode
!
     allocate( Interferogram_sb%Band( Interferogram_sb%NBand ),  stat=ErrCode )
     allocate( Interferogram_sb%Interferogram_subfov( Interferogram_sb%NBand ),  stat=ErrCode )
     if( ErrCode > 0 ) then
        write(*,*) 'allocation Interferogram_sb Error',ErrCode
        write(*,*) 'type_Interferogram_sb: fatal error'
        call exit(1)
     end if
! defaut initialisation
     Interferogram_sb%Band(:) = 0
     return
   end subroutine alloc_Interferogram_sb
!
!
   subroutine dalloc_Interferogram_sb( Interferogram_sb )
   implicit none
     type(type_Interferogram_sb), intent(inout) :: Interferogram_sb
!
     deallocate( Interferogram_sb%Band )
     deallocate( Interferogram_sb%Interferogram_subfov )
     return
   end subroutine dalloc_Interferogram_sb
!
!
   subroutine alloc_Interferogram_fov( Interferogram_fov )
   implicit none
     type(type_Interferogram_fov), intent(inout)    :: Interferogram_fov
     integer(kind=LONG)                        :: ErrCode
!
     allocate( Interferogram_fov%FovCol( Interferogram_fov%NcolFov),  stat=ErrCode )
     allocate( Interferogram_fov%FovLin(Interferogram_fov%NlinFov ),  stat=ErrCode )
     allocate( Interferogram_fov%Interferogram_sb( Interferogram_fov%NcolFov,Interferogram_fov%NlinFov ),  stat=ErrCode )
     if( ErrCode > 0 ) then
        write(*,*) 'allocation Interferogram_fov Error',ErrCode
        write(*,*) 'type_Interferogram_fov: fatal error'
        call exit(1)
     end if
 ! defaut initialisation
     Interferogram_fov%FovCol(:) = 0
     Interferogram_fov%FovLin(:) = 0
     return
   end subroutine alloc_Interferogram_fov
!
!
   subroutine dalloc_Interferogram_fov( Interferogram_fov )
   implicit none
     type(type_Interferogram_fov), intent(inout) :: Interferogram_fov
     deallocate( Interferogram_fov%FovCol )
     deallocate( Interferogram_fov%FovLin )
     deallocate( Interferogram_fov%Interferogram_sb )
     return
   end subroutine dalloc_Interferogram_fov
!
!
   subroutine alloc_Interferogram_for( Interferogram_for )
   implicit none
     type(type_Interferogram_for), intent(inout)    :: Interferogram_for
     integer(kind=LONG)                        :: ErrCode
!
     allocate( Interferogram_for%For( Interferogram_for%NFor ),  stat=ErrCode )
     allocate( Interferogram_for%Interferogram_fov( Interferogram_for%NFor ),  stat=ErrCode )
     if( ErrCode > 0 ) then
        write(*,*) 'allocation Interferogram_for Error',ErrCode
        write(*,*) 'type_Interferogram_for: fatal error'
        call exit(1)
     end if
     ! defaut initialisation
     Interferogram_for%For(:) = 0
     return
   end subroutine alloc_Interferogram_for
!
!
   subroutine dalloc_Interferogram_for( Interferogram_for )
   implicit none
     type(type_Interferogram_for), intent(inout) :: Interferogram_for
     deallocate( Interferogram_for%For)
     deallocate( Interferogram_for%Interferogram_fov )
     return
   end subroutine dalloc_Interferogram_for
!
!
   subroutine alloc_Interferogram_line( Interferogram_line )
   implicit none
     type(type_Interferogram_line), intent(inout)   :: Interferogram_line
     integer(kind=LONG)                        :: ErrCode
!
     allocate( Interferogram_line%Line( Interferogram_line%Nline ),  stat=ErrCode )
     allocate( Interferogram_line%Interferogram_for( Interferogram_line%Nline ),  stat=ErrCode )
     if( ErrCode > 0 ) then
        write(*,*) 'allocation Interferogram_line Error',ErrCode
        write(*,*) 'type_Interferogram_line: fatal error'
        call exit(1)
     end if
     ! defaut initialisation
     Interferogram_line%Line(:) = 0
     return
   end subroutine alloc_Interferogram_line
!
!
   subroutine dalloc_Interferogram_line( Interferogram_line )
   implicit none
     type(type_Interferogram_line), intent(inout) :: Interferogram_line
     deallocate( Interferogram_line%Line )
     deallocate( Interferogram_line%Interferogram_for )
     return
   end subroutine dalloc_Interferogram_line
!
!
   subroutine dalloc_Interferogram_full( Interferogram_full)
   implicit none
   type(type_Interferogram_full), intent(inout) :: Interferogram_full
   integer(kind=LONG) :: line, for, fov_col, fov_lin, sb
!
   do line = 1, Interferogram_full%Interferogram_line%Nline
      do for = 1, Interferogram_full%Interferogram_line%Interferogram_for(line)%NFor
         do fov_col = 1, Interferogram_full%Interferogram_line%Interferogram_for(line)%Interferogram_fov(for)%NcolFov
            do fov_lin = 1, Interferogram_full%Interferogram_line%Interferogram_for(line)%Interferogram_fov(for)%NlinFov
               do sb = 1, Interferogram_full%Interferogram_line%Interferogram_for(line) &
                           %Interferogram_fov(for)%Interferogram_sb(fov_col,fov_lin)%Nband
                  call dalloc_Interferogram_subfov( Interferogram_full%Interferogram_line%Interferogram_for(line) &
                           %Interferogram_fov(for)%Interferogram_sb(fov_col,fov_lin)%Interferogram_subfov(sb) )
               end do
            end do
         end do
      end do
   end do
   do line = 1, Interferogram_full%Interferogram_line%Nline
      do for = 1, Interferogram_full%Interferogram_line%Interferogram_for(line)%NFor
         do fov_col = 1, Interferogram_full%Interferogram_line%Interferogram_for(line)%Interferogram_fov(for)%NcolFov
            do fov_lin = 1, Interferogram_full%Interferogram_line%Interferogram_for(line)%Interferogram_fov(for)%NlinFov
               call dalloc_Interferogram_sb( Interferogram_full%Interferogram_line%Interferogram_for(line) &
                           %Interferogram_fov(for)%Interferogram_sb(fov_col,fov_lin) )
            end do
         end do
      end do
   end do
   do line = 1, Interferogram_full%Interferogram_line%Nline
      do for = 1, Interferogram_full%Interferogram_line%Interferogram_for(line)%NFor
         call dalloc_Interferogram_fov( Interferogram_full%Interferogram_line%Interferogram_for(line) &
                           %Interferogram_fov(for) )
      end do
   end do
   do line = 1, Interferogram_full%Interferogram_line%Nline
      call dalloc_Interferogram_for( Interferogram_full%Interferogram_line%Interferogram_for(line) )
   end do
   call dalloc_Interferogram_line( Interferogram_full%Interferogram_line )

   return
   end subroutine dalloc_Interferogram_full
!
!
   subroutine alloc_Interferogram_full_index( Interferogram_full, bound )
   implicit none
     type(type_Interferogram_full), intent(in) :: Interferogram_full
     type(type_Interferogram_full_index), intent(inout) :: bound
     integer(kind=LONG) :: ErrCode
!
     ErrCode = 0
     allocate( bound%indices_line( Interferogram_full%Interferogram_line%Nline ),  stat=ErrCode )
     allocate( bound%indices_for( Interferogram_full%Interferogram_line%Interferogram_for(1)%NFor),  stat=ErrCode )
     allocate( bound%indices_fovcol( Interferogram_full%Interferogram_line%Interferogram_for(1) &
       %Interferogram_fov(1)%NcolFov),  stat=ErrCode )
     allocate( bound%indices_fovlin( Interferogram_full%Interferogram_line%Interferogram_for(1) &
       %Interferogram_fov(1)%NlinFov),  stat=ErrCode )
     allocate( bound%indices_band( Interferogram_full%Interferogram_line%Interferogram_for(1) &
       %Interferogram_fov(1)%Interferogram_sb(1,1)%NBand),  stat=ErrCode )
     allocate( bound%indices_subfov( Interferogram_full%Interferogram_line%Interferogram_for(1) &
       %Interferogram_fov(1)%Interferogram_sb(1,1)%Interferogram_subfov(1)%NsubFov),  stat=ErrCode )
     if ( ErrCode > 0 ) then
       write(*,*) 'allocation indices_line Error', ErrCode
       call exit(1)
     end if

     return
   end subroutine alloc_Interferogram_full_index
!
!
   subroutine dalloc_Interferogram_full_index( bound )
    implicit none
     type(type_Interferogram_full_index), intent(inout) :: bound
!
     deallocate( bound%indices_line )
     deallocate( bound%indices_for )
     deallocate( bound%indices_fovcol )
     deallocate( bound%indices_fovlin )
     deallocate( bound%indices_band )
     deallocate( bound%indices_subfov )

     return
   end subroutine dalloc_Interferogram_full_index
!
! Checks indices of the compound type to be written
   subroutine check_indices_Interferogram_full( Interferogram_full, bound)
   implicit none
   type(type_Interferogram_full), intent(in) :: Interferogram_full
   type(type_Interferogram_full_index), intent(inout) :: bound
   integer(kind=LONG) :: line, for, fov_col, fov_lin, sb, sub_fov, loop
   integer(kind=LONG) :: nvalue, index
   !
   bound%indices_line(:) = 0
   bound%indices_for(:) = 0
   bound%indices_fovcol(:) = 0
   bound%indices_fovlin(:) = 0
   bound%indices_band(:) = 0
   bound%indices_subfov(:) = 0
   bound%index_Interferogram(1) = 1
   where ( Interferogram_full%Interferogram_line%Line .gt. 0 ) bound%indices_line = 1
   do line = 1, Interferogram_full%Interferogram_line%Nline
      where ( Interferogram_full%Interferogram_line%Interferogram_for(line)%For .gt. 0 ) bound%indices_for = 1
      do for = 1, Interferogram_full%Interferogram_line%Interferogram_for(line)%NFor
         where ( Interferogram_full%Interferogram_line%Interferogram_for(line) &
                 %Interferogram_fov(for)%FovCol .gt. 0 ) bound%indices_fovcol = 1
         where ( Interferogram_full%Interferogram_line%Interferogram_for(line) &
                 %Interferogram_fov(for)%FovLin .gt. 0 ) bound%indices_fovlin = 1
         do fov_col = 1, Interferogram_full%Interferogram_line%Interferogram_for(line)%Interferogram_fov(for)%NcolFov
            do fov_lin = 1, Interferogram_full%Interferogram_line%Interferogram_for(line)%Interferogram_fov(for)%NlinFov
               where ( Interferogram_full%Interferogram_line%Interferogram_for(line) &
                        %Interferogram_fov(for)%Interferogram_sb(fov_col,fov_lin)%Band .gt. 0 ) bound%indices_band = 1
               do sb = 1, Interferogram_full%Interferogram_line%Interferogram_for(line) &
                                %Interferogram_fov(for)%Interferogram_sb(fov_col,fov_lin)%Nband
                  where ( Interferogram_full%Interferogram_line%Interferogram_for(line)   &
                           %Interferogram_fov(for)%Interferogram_sb(fov_col,fov_lin) &
                           %Interferogram_subfov(sb)%SubFov .gt. 0 ) bound%indices_subfov = 1
                  do sub_fov = 1, Interferogram_full%Interferogram_line%Interferogram_for(line)   &
                           %Interferogram_fov(for)%Interferogram_sb(fov_col,fov_lin) &
                           %Interferogram_subfov(sb)%NsubFov
                     nvalue = size(bound%indices_band)
                     do loop = 1, nvalue
                        if ( bound%indices_band(loop) == 1 ) index = loop ! current band
                     end do
                     bound%index_Interferogram(2) = Interferogram_full%Interferogram_line%Interferogram_for(line) &
                      %Interferogram_fov(for)%Interferogram_sb(fov_col,fov_lin) &
                      %Interferogram_subfov(index)%Interferogram(sub_fov)%N_Sample
                  end do
               end do
            end do
         end do
      end do
   end do
   ! calculates start and end index for each dimension
   ! line
   nvalue = size(bound%indices_line)
   do loop= 1, nvalue
    if ( bound%indices_line(loop) == 1 ) bound%index_line(2) = loop ! last index of line
   end do
   do loop = nvalue, 1, -1
    if ( bound%indices_line(loop) == 1 ) bound%index_line(1) = loop ! first index of line
   end do
   ! for
   nvalue = size(bound%indices_for)
   do loop = 1, nvalue
    if ( bound%indices_for(loop) == 1 ) bound%index_for(2) = loop ! last index of FOR
   end do
   do loop = nvalue, 1, -1
    if ( bound%indices_for(loop) == 1 ) bound%index_for(1) = loop ! first index of FOR
   end do
   ! fovcol
   nvalue = size(bound%indices_fovcol)
   do loop = 1, nvalue
    if ( bound%indices_fovcol(loop) == 1 ) bound%index_fovcol(2) = loop ! last index of FovCol
   end do
   do loop = nvalue, 1, -1
    if ( bound%indices_fovcol(loop) == 1 ) bound%index_fovcol(1) = loop ! first index of FovCol
   end do
   ! fovlin
   nvalue = size(bound%indices_fovlin)
   do loop = 1, nvalue
    if ( bound%indices_fovlin(loop) == 1 ) bound%index_fovlin(2) = loop ! last index of FovLin
   end do
   do loop = nvalue, 1, -1
    if ( bound%indices_fovlin(loop) == 1 ) bound%index_fovlin(1) = loop ! first index of FovLin
   end do
   ! band
   nvalue = size(bound%indices_band)
   do loop = 1, nvalue
    if ( bound%indices_band(loop) == 1 ) bound%index_band(2) = loop ! last index of band
   end do
   do loop = nvalue, 1, -1
    if ( bound%indices_band(loop) == 1 ) bound%index_band(1) = loop ! first index of band
   end do
   ! subfov
   nvalue = size(bound%indices_subfov)
   do loop = 1, nvalue
    if ( bound%indices_subfov(loop) == 1 ) bound%index_subfov(2) = loop ! last index of subfov
   end do
   do loop = nvalue, 1, -1
    if ( bound%indices_subfov(loop) == 1 ) bound%index_subfov(1) = loop ! first index of subfov
   end do
   !
   return
   end subroutine check_indices_Interferogram_full
!
!
   subroutine Interferogram_Basis( Interferogram )
     implicit none
     type(type_Interferogram), intent(inout) :: Interferogram
!
     integer(kind=LONG)                      :: Ns
!
     Interferogram%Time(1:Interferogram%N_Sample) = &
         (/ (dble(Ns-1),Ns=1,Interferogram%N_Sample) /) * Interferogram%dTime
     Interferogram%Opd(1:Interferogram%N_Sample)  = &
         (/ (dble(Ns-1),Ns=1,Interferogram%N_Sample) /) * Interferogram%dOpd
!
     return
   end subroutine Interferogram_Basis
!
!
   subroutine Interf_Header_Transfer( Interf_In, Interf_Out )
     implicit none
     type(type_Interferogram), intent(in)    :: Interf_In
     type(type_Interferogram), intent(inout) :: Interf_Out
!
     Interf_Out%Type     = Interf_In%Type
     Interf_Out%N_Sample = Interf_In%N_Sample
     call alloc_Interferogram( Interf_Out )
     Interf_Out = Interf_In
     if( Interf_Out%Type(1:len_trim(Interf_Out%Type))      == 'C'  ) then
        Interf_Out%Complex(1:Interf_Out%N_Sample) = dcmplx(0.d+00,0.d+00)
     else if( Interf_Out%Type(1:len_trim(Interf_Out%Type)) == 'RI' ) then
        Interf_Out%Real_Part(1:Interf_Out%N_Sample) = 0.d+00
        Interf_Out%Imag_Part(1:Interf_Out%N_Sample) = 0.d+00
     else if( Interf_Out%Type(1:len_trim(Interf_Out%Type)) == 'R'  ) then
        Interf_Out%Real_Part(1:Interf_Out%N_Sample) = 0.d+00
     else if( Interf_Out%Type(1:len_trim(Interf_Out%Type)) == 'I'  ) then
        Interf_Out%Imag_Part(1:Interf_Out%N_Sample) = 0.d+00
     else if( Interf_Out%Type(1:len_trim(Interf_Out%Type)) == 'MA' ) then
        Interf_Out%Modulus(1:Interf_Out%N_Sample) = 0.d+00
        Interf_Out%Argument(1:Interf_Out%N_Sample) = 0.d+00
     else if( Interf_Out%Type(1:len_trim(Interf_Out%Type)) == 'M'  ) then
        Interf_Out%Modulus(1:Interf_Out%N_Sample) = 0.d+00
     else if( Interf_Out%Type(1:len_trim(Interf_Out%Type)) == 'A'  ) then
        Interf_Out%Argument(1:Interf_Out%N_Sample) = 0.d+00
     else
        write(*,*) 'Interferogram Type Error',Interf_Out%Type
        write(*,*) 'Interf_Header_Transfer Fatal Error'
        call exit(1)
     end if
!
     return
   end subroutine Interf_Header_Transfer
!
!
   subroutine Interf_Transfer( Interf_In, Interf_Out )
     implicit none
     type(type_Interferogram), intent(in)    :: Interf_In
     type(type_Interferogram), intent(inout) :: Interf_Out
!
     Interf_Out%Type     = Interf_In%Type
     Interf_Out%N_Sample = Interf_In%N_Sample
     call alloc_Interferogram( Interf_Out )
     Interf_Out = Interf_In
!
     return
   end subroutine Interf_Transfer
!
!
   subroutine readinterferogram( Interferogram,iostatus )
   implicit none
     type(type_Interferogram), intent(inout) :: Interferogram
!
     integer(kind=LONG), intent(out)         :: iostatus
     integer(kind=DOUBLE)                    :: ifile
     integer(kind=LONG)                      :: offset
     integer(kind=LONG)                      :: Type
     integer(kind=LONG)                      :: Size
!
      iostatus = 0
      call open_file_r(Interferogram%filename                    &
                       (1:len_trim(Interferogram%filename))      &
                       // char(0), ifile)
      if ( ifile .eq. 0 ) then
         iostatus = 1
      else
         offset = 0
         Type = ch2Type
         Size = 2
         call read_field( ifile, Interferogram%Type,                  &
                                 %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4
         call read_field( ifile, Interferogram%N_Sample,              &
                                 %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Interferogram%N_Top,              &
                                 %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call alloc_Interferogram( Interferogram )
         Type = r8Type
         Size = 8
         call read_field( ifile, Interferogram%FieldMeanAngle,        &
                                 %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Interferogram%TimeMax,               &
                                 %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Interferogram%dTime,                 &
                                 %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
          call read_field( ifile, Interferogram%OpdMax,               &
                                 %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Interferogram%dOpd,                  &
                                 %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8*Interferogram%N_Sample
         call read_field( ifile, Interferogram%Time,                  &
                                 %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Interferogram%Opd,                   &
                                 %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8*Interferogram%N_Top
         call read_field( ifile, Interferogram%Time_Top,              &
                                 %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Interferogram%Opd_Top,               &
                                 %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         if( Interferogram%Type(1:len_trim(Interferogram%Type))      == 'C'  ) then
            Type = c16Type
            Size = 16*Interferogram%N_Sample
            call read_field( ifile, Interferogram%Complex,            &
                                    %VAL(Type), %VAL(Size), %VAL(offset) )
            offset = offset + Size
         else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'RI' ) then
            Type = r8Type
            Size = 8*Interferogram%N_Sample
            call read_field( ifile, Interferogram%Real_Part,          &
                                    %VAL(Type), %VAL(Size), %VAL(offset) )
            offset = offset + Size
            Type = r8Type
            Size = 8*Interferogram%N_Sample
            call read_field( ifile, Interferogram%Imag_Part,          &
                                    %VAL(Type), %VAL(Size), %VAL(offset) )
            offset = offset + Size
         else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'MA' ) then
            Type = r8Type
            Size = 8*Interferogram%N_Sample
            call read_field( ifile, Interferogram%Modulus,            &
                                    %VAL(Type), %VAL(Size), %VAL(offset) )
            offset = offset + Size
            Type = r8Type
            Size = 8*Interferogram%N_Sample
            call read_field( ifile, Interferogram%Argument,           &
                                    %VAL(Type), %VAL(Size), %VAL(offset) )
            offset = offset + Size
         else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'R'  ) then
            Type = r8Type
            Size = 8*Interferogram%N_Sample
            call read_field( ifile, Interferogram%Real_Part,          &
                                    %VAL(Type), %VAL(Size), %VAL(offset) )
            offset = offset + Size
         else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'I'  ) then
            Type = r8Type
            Size = 8*Interferogram%N_Sample
            call read_field( ifile, Interferogram%Imag_Part,          &
                                    %VAL(Type), %VAL(Size), %VAL(offset) )
            offset = offset + Size
         else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'M'  ) then
            Type = r8Type
            Size = 8*Interferogram%N_Sample
            call read_field( ifile, Interferogram%Modulus,           &
                                    %VAL(Type), %VAL(Size), %VAL(offset) )
            offset = offset + Size
         else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'A'  ) then
            Type = r8Type
            Size = 8*Interferogram%N_Sample
            call read_field( ifile, Interferogram%Argument,          &
                                    %VAL(Type), %VAL(Size), %VAL(offset) )
            offset = offset + Size
         end if
         call close_file(Interferogram%filename                  &
                         (1:len_trim(Interferogram%filename))    &
                         // char(0), ifile)
      end if
!
   return
   end subroutine readinterferogram
!
!
   subroutine writeinterferogram( Interferogram,iostatus )
     implicit none
     type(type_Interferogram), intent(in) :: Interferogram
!
     integer(kind=LONG), intent(out) :: iostatus
     integer(kind=DOUBLE)            :: ifile
     integer(kind=LONG)              :: offset
     integer(kind=LONG)              :: Type
     integer(kind=LONG)              :: Size
!
      iostatus = 0
      call open_file_w(Interferogram%filename                    &
                       (1:len_trim(Interferogram%filename))      &
                       // char(0), ifile)
      if ( ifile .eq. 0 ) then
         iostatus = 1
      else
         offset = 0
         Type = ch2Type
         Size = 2
         call write_field( ifile, Interferogram%Type,                  &
                                  %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4
         call write_field( ifile, Interferogram%N_Sample,              &
                                  %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Interferogram%N_Top,                 &
                                  %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8
         call write_field( ifile, Interferogram%FieldMeanAngle,        &
                                  %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Interferogram%TimeMax,               &
                                  %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Interferogram%dTime,                 &
                                  %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Interferogram%OpdMax,                &
                                  %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Interferogram%dOpd,                  &
                                  %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8*Interferogram%N_Sample
         call write_field( ifile, Interferogram%Time,                  &
                                  %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Interferogram%Opd,                   &
                                  %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8*Interferogram%N_Top
         call write_field( ifile, Interferogram%Time_Top,              &
                                  %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Interferogram%Opd_Top,               &
                                  %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         if( Interferogram%Type(1:len_trim(Interferogram%Type))      == 'C'  ) then
            Type = c16Type
            Size = 16*Interferogram%N_Sample
            call write_field( ifile, Interferogram%Complex,            &
                                     %VAL(Type), %VAL(Size), %VAL(offset) )
            offset = offset + Size
         else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'RI' ) then
            Type = r8Type
            Size = 8*Interferogram%N_Sample
            call write_field( ifile, Interferogram%Real_Part,          &
                                     %VAL(Type), %VAL(Size), %VAL(offset) )
            offset = offset + Size
            call write_field( ifile, Interferogram%Imag_Part,          &
                                     %VAL(Type), %VAL(Size), %VAL(offset) )
            offset = offset + Size
         else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'MA' ) then
            Type = r8Type
            Size = 8*Interferogram%N_Sample
            call write_field( ifile, Interferogram%Modulus,            &
                                     %VAL(Type), %VAL(Size), %VAL(offset) )
            offset = offset + Size
            call write_field( ifile, Interferogram%Argument,           &
                                     %VAL(Type), %VAL(Size), %VAL(offset) )
            offset = offset + Size
         else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'R'  ) then
            Type = r8Type
            Size = 8*Interferogram%N_Sample
            call write_field( ifile, Interferogram%Real_Part,          &
                                     %VAL(Type), %VAL(Size), %VAL(offset) )
            offset = offset + Size
         else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'I'  ) then
            Type = r8Type
            Size = 8*Interferogram%N_Sample
            call write_field( ifile, Interferogram%Imag_Part,          &
                                     %VAL(Type), %VAL(Size), %VAL(offset) )
            offset = offset + Size
         else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'M'  ) then
            Type = r8Type
            Size = 8*Interferogram%N_Sample
            call write_field( ifile, Interferogram%Modulus,            &
                                     %VAL(Type), %VAL(Size), %VAL(offset) )
            offset = offset + Size
         else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'A'  ) then
            Type = r8Type
            Size = 8*Interferogram%N_Sample
            call write_field( ifile, Interferogram%Argument,           &
                                     %VAL(Type), %VAL(Size), %VAL(offset) )
            offset = offset + Size
         end if
         call close_file(Interferogram%filename                  &
                         (1:len_trim(Interferogram%filename))    &
                         // char(0), ifile)
      end if
!
      return
   end subroutine writeinterferogram
!
!
!>  writeinterferogram_netcdf -- Public
!!
!! * Purpose
!!
!!     Write interferogram type
!!
!! * Description
!!
!!     Write interferogram type
!!
!! * Inputs
!!
!!     - interferogram  : type_interferogram / type for declaration and allocation of interferogram parameters
!!
!! * Outputs
!!
!!     - iostatus : integer / error management variable
!!
!! * InOut
!!
!!     - init_file : integer / flag to initialize the netcdf file
!!
!! * References
!!
!!   SPS
!!
   subroutine writeinterferogram_netcdf( Interferogram,iostatus,init_file )

     implicit none

     type(type_Interferogram), intent(in) :: Interferogram
     integer(kind=LONG), intent(out) :: iostatus
     integer(kind=SHORT), intent(inout) :: init_file

     integer(kind=LONG) :: ncid, status, i_band

     iostatus = 0

     ! file creation and definition
     if (init_file == 0) then
        call init_interferogram_data(Interferogram, ncid)
        init_file = 1
     endif

     ! open netCDF dataset
     status = nf90_open(path = trim(Interferogram%TopLevelIn%header%filename), mode = nf90_write, ncid = ncid)
     if (status /= nf90_noerr) call handle_err(status)

     ! writes variables for interferogram group
     ! current spectral band
     i_band = Interferogram%TopLevelIn%Band
     call write_interferogram_data(ncid, i_band, Interferogram)

     ! close access to netCDF dataset
     status = nf90_close(ncid)
     if (status /= nf90_noerr) call handle_err(status)

     return

   end subroutine writeinterferogram_netcdf

   subroutine init_interferogram_data(Interferogram, ncid)

     implicit none

     type(type_Interferogram), intent(in) :: Interferogram
     integer(kind=LONG), intent(out) :: ncid

     integer(kind=LONG) :: status
     integer(kind=LONG) :: typeid
     integer(kind=LONG) :: varid, dimid

     ! create netCDF dataset
     status = nf90_create(path = trim(Interferogram%TopLevelIn%header%filename), cmode = NF90_NETCDF4, ncid = ncid)
     if (status /= nf90_noerr) call handle_err(status)

     ! define compound type
     status = nf90_def_compound(ncid, 16, "complex", typeid)
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_insert_compound(ncid, typeid, "r", 0, NF90_DOUBLE)
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_insert_compound(ncid, typeid, "i", 8, NF90_DOUBLE)
     if (status /= nf90_noerr) call handle_err(status)

     ! define and write global variables

     !
     ! LINE(Nline) fixed to 1 deliberately
     status = nf90_def_dim(ncid, "Nline", 1, dimid)
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_def_var(ncid, "LINE", NF90_INT, (/dimid/), varid)
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_att(ncid, varid, "units", "N.A.")
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_att(ncid, varid, "long_name", "line value")
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_var(ncid, varid, 1.)
     if (status /= nf90_noerr) call handle_err(status)
     ! For(NFor) fixed to 1 deliberately
     status = nf90_def_dim(ncid, "NFor", 1, dimid)
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_def_var(ncid, "For", NF90_INT, (/dimid/), varid)
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_att(ncid, varid, "units", "N.A.")
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_att(ncid, varid, "long_name", "field of view value")
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_var(ncid, varid, 1.)
     if (status /= nf90_noerr) call handle_err(status)
     ! ColFov(NcolFov)
     status = nf90_def_dim(ncid, "NcolFov", Interferogram%TopLevelIn%NcolFov, dimid)
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_def_var(ncid, "ColFov", NF90_INT, (/dimid/), varid)
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_att(ncid, varid, "units", "N.A.")
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_att(ncid, varid, "long_name", "column Fov value")
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_var(ncid, varid, Interferogram%TopLevelIn%ColFov)
     if (status /= nf90_noerr) call handle_err(status)
     ! LinFov(NlinFov)
     status = nf90_def_dim(ncid, "NlinFov", Interferogram%TopLevelIn%NlinFov, dimid)
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_def_var(ncid, "LinFov", NF90_INT, (/dimid/), varid)
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_att(ncid, varid, "units", "N.A.")
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_att(ncid, varid, "long_name", "line Fov value")
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_var(ncid, varid, Interferogram%TopLevelIn%LinFov)
     if (status /= nf90_noerr) call handle_err(status)

     ! define global attributes
     ! filename
     status = nf90_put_att(ncid, NF90_GLOBAL, name = "filename", values = trim(Interferogram%TopLevelIn%header%filename))
     if (status /= nf90_noerr) call handle_err(status)
     ! instrument name
     status = nf90_put_att(ncid, NF90_GLOBAL, name = "InsName", values = Interferogram%TopLevelIn%header%InsName)
     if (status /= nf90_noerr) call handle_err(status)
     ! instrument mode
     status = nf90_put_att(ncid, NF90_GLOBAL, name = "InsMode", values = Interferogram%TopLevelIn%header%InsMode)
     if (status /= nf90_noerr) call handle_err(status)
     ! S/W version
     status = nf90_put_att(ncid, NF90_GLOBAL, name = "SoftWareVersion", values = Interferogram%TopLevelIn%header%SoftWareVersion)
     if (status /= nf90_noerr) call handle_err(status)
     ! Operator
     status = nf90_put_att(ncid, NF90_GLOBAL, name = "Operator", values = Interferogram%TopLevelIn%header%Operator)
     if (status /= nf90_noerr) call handle_err(status)
     ! Processing step
     status = nf90_put_att(ncid, NF90_GLOBAL, name = "ProcessingStep", values = Interferogram%TopLevelIn%header%ProcessingStep)
     if (status /= nf90_noerr) call handle_err(status)
     ! Conf ID
     status = nf90_put_att(ncid, NF90_GLOBAL, name = "ConfId", values = trim(Interferogram%TopLevelIn%header%ConfId))
     if (status /= nf90_noerr) call handle_err(status)
     ! Type
     status = nf90_put_att(ncid, NF90_GLOBAL, name = "Type", values = Interferogram%Type)
     if (status /= nf90_noerr) call handle_err(status)
     ! Target
     status = nf90_put_att(ncid, NF90_GLOBAL, name = "Target", values = Interferogram%TopLevelIn%Target)
     if (status /= nf90_noerr) call handle_err(status)

     ! close access to netCDF dataset
     status = nf90_close(ncid)
     if (status /= nf90_noerr) call handle_err(status)

     return

   end subroutine init_interferogram_data

   subroutine write_interferogram_data(ncid, band, Interferogram)

     implicit none

     integer(kind=LONG), intent(in)  :: ncid, band
     type(type_Interferogram), intent(in) :: Interferogram

     integer(kind=LONG),dimension(6) :: start_w, count_w, start_w_c, count_w_c
     integer(kind=LONG) :: status
     integer(kind=LONG) :: grpid, varid, ntype, flag_grp
     integer(kind=LONG), dimension(1) :: typeid
     integer(kind=LONG) :: nsp_dimid, nsf_dimid, ndims
     integer(kind=LONG), dimension(:), allocatable :: dimids
     character(len=1) :: band_str
     integer(kind=LONG) :: parent
     integer(kind=LONG) :: ErrCode
     integer(kind=LONG) :: nsamp

     ! N_Sample
     nsamp = Interferogram%N_Sample

     ! start index and count_index
     ! The values to be written are associated with the netCDF variable by assuming
     ! that the first dimension of the netCDF variable varies fastest in the Fortran 90 interface.
     ! Nline,NFor,NlinFov,NcolFov,NsubFov,N_Sample
     start_w = (/1,1,1,1,Interferogram%TopLevelIn%SubFov,1/)
     count_w = (/1,1,1,1,1,nsamp/)
     ! C version
     ! The values to be written are associated with the netCDF variable by assuming
     ! that the last dimension of the netCDF variable varies fastest in the C interface.
     ! N_Sample,NsubFov,NcolFov,NlinFov,NFor,Nline
     start_w_c = (/0,Interferogram%TopLevelIn%SubFov - 1,0,0,0,0/)
     count_w_c = (/nsamp,1,1,1,1,1/)

     ! definition of the group (one group for one spectral band)
     write(band_str,'(i1)') band

     ! check the group existence
     flag_grp = nf90_inq_ncid(ncid, 'interferogram_sb'//band_str, grpid)

     if (flag_grp /= nf90_noerr) then
        ! group creation and initialization
        status = nf90_def_grp(ncid, 'interferogram_sb'//band_str, grpid)
        if (status /= nf90_noerr) call handle_err(status)

        ! definition of the dimensions
        ! N_Sample
        status = nf90_def_dim(grpid, "N_Sample", nsamp, nsp_dimid)
        if (status /= nf90_noerr) call handle_err(status)
        ! NsubFov
        status = nf90_def_dim(grpid, "NsubFov", Interferogram%TopLevelIn%NsubFov, nsf_dimid)
        if (status /= nf90_noerr) call handle_err(status)

        ! get the ids of global dimensions (NcolFov, NlinFov, NFor, Nline)
        status = nf90_inquire(ncid, nDimensions=ndims)
        allocate(dimids(ndims), stat=ErrCode)
        parent=0
        ! dimids(1):NcolFov, dimids(2):NlinFov, dimids(3):NFor, dimids(4):Nline
        status = nf90_inq_dimids(ncid, ndims, dimids, parent)
        if (status /= nf90_noerr) call handle_err(status)

        ! get the type of user defined type (Complex)
        status = nf90_inq_typeids(ncid, ntype, typeid)
        if (status /= nf90_noerr) call handle_err(status)

        ! definition of the variables
        ! SubFov
        call write_def_variable(grpid, "SubFov", NF90_INT, (/nsf_dimid/), "N.A.", "subFov value")
        ! Time
        call write_def_variable(grpid, "Time", NF90_DOUBLE, &
              (/dimids(4),dimids(3),dimids(2),dimids(1),nsf_dimid,nsp_dimid/), "s", "time base")
        ! Time_Top
        call write_def_variable(grpid, "Time_Top", NF90_DOUBLE, &
              (/dimids(4),dimids(3),dimids(2),dimids(1),nsf_dimid,nsp_dimid/), "s", "base provided by RPD laser")
        ! Opd
        call write_def_variable(grpid, "Opd", NF90_DOUBLE, &
              (/dimids(4),dimids(3),dimids(2),dimids(1),nsf_dimid,nsp_dimid/), "m", "optical path difference base")
        ! Opd_Top
        call write_def_variable(grpid, "Opd_Top", NF90_DOUBLE, &
              (/dimids(4),dimids(3),dimids(2),dimids(1),nsf_dimid,nsp_dimid/), "m", "OPD base provided by RPD laser")
        if ( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'R'  ) then
           ! Real part
           call write_def_variable(grpid, "Real_Part", NF90_DOUBLE, &
                 (/dimids(4),dimids(3),dimids(2),dimids(1),nsf_dimid,nsp_dimid/), "N.A.", "interferogram real part")
        else if ( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'I'  ) then
           ! Imaginary part
           call write_def_variable(grpid, "Imag_Part", NF90_DOUBLE, &
                 (/dimids(4),dimids(3),dimids(2),dimids(1),nsf_dimid,nsp_dimid/), "N.A.", "interferogram imaginary part")
        else if ( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'M'  ) then
           ! Modulus
           call write_def_variable(grpid, "Modulus", NF90_DOUBLE, &
                 (/dimids(4),dimids(3),dimids(2),dimids(1),nsf_dimid,nsp_dimid/), "N.A.", "interferogram modulus part")
        else if ( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'A'  ) then
           ! Argument
           call write_def_variable(grpid, "Argument", NF90_DOUBLE, &
                 (/dimids(4),dimids(3),dimids(2),dimids(1),nsf_dimid,nsp_dimid/), "rad", "interferogram argument part")
        else if ( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'C'  ) then
           ! Complex
           call write_def_variable(grpid, "Complex", typeid(1), &
                 (/dimids(4),dimids(3),dimids(2),dimids(1),nsf_dimid,nsp_dimid/), "N.A.", "complex interferogram")
        else if ( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'RI'  ) then
           ! RI
           call write_def_variable(grpid, "Real_Part", NF90_DOUBLE, &
                 (/dimids(4),dimids(3),dimids(2),dimids(1),nsf_dimid,nsp_dimid/), "N.A.", "interferogram real part")
           call write_def_variable(grpid, "Imag_Part", NF90_DOUBLE, &
                 (/dimids(4),dimids(3),dimids(2),dimids(1),nsf_dimid,nsp_dimid/), "N.A.", "interferogram imaginary part")
        else if ( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'MA'  ) then
           ! MA
           call write_def_variable(grpid, "Modulus", NF90_DOUBLE, &
                 (/dimids(4),dimids(3),dimids(2),dimids(1),nsf_dimid,nsp_dimid/), "N.A.", "interferogram modulus part")
           call write_def_variable(grpid, "Argument", NF90_DOUBLE, &
                 (/dimids(4),dimids(3),dimids(2),dimids(1),nsf_dimid,nsp_dimid/), "rad", "interferogram argument part")
        end if

        ! write group attributes
        ! Band
        status = nf90_put_att(grpid, NF90_GLOBAL, name= "Band", values = band)
        if (status /= nf90_noerr) call handle_err(status)
        ! N_Top
        status = nf90_put_att(grpid, NF90_GLOBAL, name = "N_Top", values = Interferogram%N_Top)
        if (status /= nf90_noerr) call handle_err(status)
        ! FieldMeanAngle
        status = nf90_put_att(grpid, NF90_GLOBAL, name = "FieldMeanAngle", values = Interferogram%FieldMeanAngle)
        if (status /= nf90_noerr) call handle_err(status)
        ! TimeMax
        status = nf90_put_att(grpid, NF90_GLOBAL, name = "TimeMax", values = Interferogram%TimeMax)
        if (status /= nf90_noerr) call handle_err(status)
        ! dTime
        status = nf90_put_att(grpid, NF90_GLOBAL, name = "dTime", values = Interferogram%dTime)
        if (status /= nf90_noerr) call handle_err(status)
        ! OpdMax
        status = nf90_put_att(grpid, NF90_GLOBAL, name = "OpdMax", values = Interferogram%OpdMax)
        if (status /= nf90_noerr) call handle_err(status)
        ! dOpd
        status = nf90_put_att(grpid, NF90_GLOBAL, name = "dOpd", values = Interferogram%dOpd)
        if (status /= nf90_noerr) call handle_err(status)

   end if ! end group creation and initialization

     ! write variables
     ! SubFov (current value)
     ! Interferogram%TopLevelIn%SubFov is a scalar variable !
     status = nf90_inq_varid(grpid, "SubFov", varid)
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_var(grpid, varid, (/Interferogram%TopLevelIn%SubFov/),&
              start=(/Interferogram%TopLevelIn%SubFov/), count=(/1/))
     if (status /= nf90_noerr) call handle_err(status)

     ! Time
     call write_variable(grpid, "Time", Interferogram%Time, start_w, count_w)
     ! Time_Top
     call write_variable(grpid, "Time_Top", Interferogram%Time_Top, start_w, count_w)
     ! Opd
     call write_variable(grpid, "Opd", Interferogram%Opd, start_w, count_w)
     ! Opd_Top
     call write_variable(grpid, "Opd_Top", Interferogram%Opd_Top, start_w, count_w)
     if ( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'R'  ) then
        ! Real Part
        call write_variable(grpid, "Real_Part", Interferogram%Real_Part, start_w, count_w)
     else if ( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'I'  ) then
        ! Imaginary part
        call write_variable(grpid, "Imag_Part", Interferogram%Imag_Part, start_w, count_w)
     else if ( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'M'  ) then
        ! Modulus
        call write_variable(grpid, "Modulus", Interferogram%Modulus, start_w, count_w)
     else if ( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'A'  ) then
        ! Argument
        call write_variable(grpid, "Argument", Interferogram%Argument, start_w, count_w)
     else if ( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'C'  ) then
        ! Complex
        status = nf90_inq_varid(grpid, "Complex", varid)
        if (status /= nf90_noerr) call handle_err(status)
        call write_data_c(grpid, varid, Interferogram%Complex, start_w_c, count_w_c)
     else if ( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'RI' ) then
        ! RI
        call write_variable(grpid, "Real_Part", Interferogram%Real_Part, start_w, count_w)
        call write_variable(grpid, "Imag_Part", Interferogram%Imag_Part, start_w, count_w)
     else if ( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'MA' ) then
        ! MA
        call write_variable(grpid, "Modulus", Interferogram%Modulus, start_w, count_w)
        call write_variable(grpid, "Argument", Interferogram%Argument, start_w, count_w)
     end if

     return

   end subroutine write_interferogram_data

!
!
!>  readinterferogram_netcdf -- Public
!!
!! * Purpose
!!
!!     Read interferogram type
!!
!! * Description
!!
!!     Read interferogram type
!!
!! * Inputs
!!
!!     - interferogram  : type_interferogram / type for declaration and allocation of interferogram parameters
!!
!! * Outputs
!!
!!     - interferogram  : type_interferogram / type for declaration and allocation of interferogram parameters
!!     - iostatus : integer / error management variable
!!
!! * References
!!
!!   SPS
!!

   subroutine readinterferogram_netcdf( Interferogram,iostatus)

     implicit none

     type(type_Interferogram), intent(inout) :: Interferogram
     integer(kind=LONG), intent(out) :: iostatus

     integer(kind=LONG) :: ncid, status, i_band

     iostatus = 0

     ! open existing netCDF dataset
     status = nf90_open(path = trim(Interferogram%TopLevelIn%header%filename), mode = nf90_nowrite, ncid = ncid)
     if (status /= nf90_noerr) call handle_err(status)

     ! get global data (variables and attributes)
     call get_interferogram_data(ncid, Interferogram)

     ! reads variables for interferogram group
     ! current spectral band
     i_band = Interferogram%TopLevelIn%Band
     call read_interferogram_data(ncid, i_band, Interferogram)

     ! close access to netCDF dataset
     status = nf90_close(ncid)
     if (status /= nf90_noerr) call handle_err(status)

     return

   end subroutine readinterferogram_netcdf


   subroutine read_interferogram_data(ncid, band, Interferogram)

     implicit none

     integer(kind=LONG), intent(in)  :: ncid, band
     type(type_Interferogram), intent(inout) :: Interferogram

     integer(kind=LONG),dimension(6) :: start_r, count_r, start_r_c, count_r_c

     integer(kind=LONG) :: status
     integer(kind=LONG) :: grpid

     integer(kind=LONG) :: ndimGrp, nvarGrp, nattGrp, nvar
     integer(kind=LONG), dimension(:), allocatable :: varidsGrp
     integer(kind=LONG) :: ErrCode
     integer(kind=LONG) :: dimid_nsample
     character(len=1)   :: band_str

     ! get the group id
     write(band_str,'(i1)') band
     status = nf90_inq_ncid(ncid, 'interferogram_sb'//band_str, grpid)
     if (status /= nf90_noerr) call handle_err(status)

     ! get information about the group dataset
     status = nf90_inquire(grpid, nDimensions=ndimGrp, nVariables=nvarGrp, nAttributes=nattGrp)
     if (status /= nf90_noerr) call handle_err(status)

     ! get variable ids from group dataset
     allocate(varidsGrp(nvarGrp), stat=ErrCode)
     status = nf90_inq_varids(grpid, nvar, varidsGrp)
     if (status /= nf90_noerr) call handle_err(status)

     ! read values for group attributes
     ! N_Top
     status = nf90_get_att(grpid, NF90_GLOBAL, name="N_Top", values=Interferogram%N_Top)
     if (status /= nf90_noerr) call handle_err(status)
     ! FieldMeanAngle
     status = nf90_get_att(grpid, NF90_GLOBAL, name="FieldMeanAngle", values=Interferogram%FieldMeanAngle)
     if (status /= nf90_noerr) call handle_err(status)
     ! TimeMax
     status = nf90_get_att(grpid, NF90_GLOBAL, name="TimeMax", values=Interferogram%TimeMax)
     if (status /= nf90_noerr) call handle_err(status)
     ! dTime
     status = nf90_get_att(grpid, NF90_GLOBAL, name="dTime", values=Interferogram%dTime)
     if (status /= nf90_noerr) call handle_err(status)
     ! OpdMax
     status = nf90_get_att(grpid, NF90_GLOBAL, name="OpdMax", values=Interferogram%OpdMax)
     if (status /= nf90_noerr) call handle_err(status)
     ! dOpd
     status = nf90_get_att(grpid, NF90_GLOBAL, name="dOpd", values=Interferogram%dOpd)
     if (status /= nf90_noerr) call handle_err(status)

     ! get N_Sample
     status = nf90_inq_dimid(grpid, "N_Sample", dimid_nsample)
     if (status /= nf90_noerr) call handle_err(status)

     status = nf90_inquire_dimension(grpid, dimid_nsample, len = Interferogram%N_Sample)
     if(status /= nf90_NoErr) call handle_err(status)

     ! Interferogram allocation
     call alloc_Interferogram( Interferogram )

     ! start index and count_index
     ! The values to be written are associated with the netCDF variable by assuming
     ! that the first dimension of the netCDF variable varies fastest in the Fortran 90 interface.
     ! Nline,NFor,NlinFov,NcolFov,NsubFov,N_Sample
     start_r = (/1,1,1,1,Interferogram%TopLevelIn%SubFov,1/)
     count_r = (/1,1,1,1,1,Interferogram%N_Sample/)
     ! C version
     ! The values to be written are associated with the netCDF variable by assuming
     ! that the last dimension of the netCDF variable varies fastest in the C interface.
     ! N_Sample,NsubFov,NcolFov,NlinFov,NFor,Nline
     start_r_c = (/0,Interferogram%TopLevelIn%SubFov - 1,0,0,0,0/)
     count_r_c = (/Interferogram%N_Sample,1,1,1,1,1/)

     ! read values for group variables

     ! note: SubFov is known before reading

     ! Time
     status = nf90_get_var(grpid, varidsGrp(2), Interferogram%Time, start=start_r, count=count_r)
     if (status /= nf90_noerr) call handle_err(status)

     ! Time_Top
     status = nf90_get_var(grpid, varidsGrp(3), Interferogram%Time_Top, start=start_r, count=count_r)
     if (status /= nf90_noerr) call handle_err(status)

     ! Opd
     status = nf90_get_var(grpid, varidsGrp(4), Interferogram%Opd, start=start_r, count=count_r)
     if (status /= nf90_noerr) call handle_err(status)

     ! Opd_Top
     status = nf90_get_var(grpid, varidsGrp(5), Interferogram%Opd_Top, start=start_r, count=count_r)
     if (status /= nf90_noerr) call handle_err(status)

     ! Type = R
     if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'R'  ) then
     ! Real part
        status = nf90_get_var(grpid, varidsGrp(6), Interferogram%Real_Part, start=start_r, count=count_r)
        if (status /= nf90_noerr) call handle_err(status)
     ! Type = I
     else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'I'  ) then
        ! Imaginary part
        status = nf90_get_var(grpid, varidsGrp(6), Interferogram%Imag_Part, start=start_r, count=count_r)
        if (status /= nf90_noerr) call handle_err(status)
     ! Type = M
     else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'M'  ) then
        ! Modulus
        status = nf90_get_var(grpid, varidsGrp(6), Interferogram%Modulus, start=start_r, count=count_r)
        if (status /= nf90_noerr) call handle_err(status)
     ! Type = A
     else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'A'  ) then
        ! Argument
        status = nf90_get_var(grpid, varidsGrp(6), Interferogram%Argument, start=start_r, count=count_r)
        if (status /= nf90_noerr) call handle_err(status)
     ! Type = RI
     else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'RI' ) then
        ! Real part
        status = nf90_get_var(grpid, varidsGrp(6), Interferogram%Real_Part, start=start_r, count=count_r)
        if (status /= nf90_noerr) call handle_err(status)
        ! Imaginary part
        status = nf90_get_var(grpid, varidsGrp(7), Interferogram%Imag_Part, start=start_r, count=count_r)
        if (status /= nf90_noerr) call handle_err(status)
     ! Type = MA
     else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'MA' ) then
        ! Modulus
        status = nf90_get_var(grpid, varidsGrp(6), Interferogram%Modulus, start=start_r, count=count_r)
        if (status /= nf90_noerr) call handle_err(status)
        ! Argument
        status = nf90_get_var(grpid, varidsGrp(7), Interferogram%Argument, start=start_r, count=count_r)
        if (status /= nf90_noerr) call handle_err(status)
     ! Type = Complex
     else if( Interferogram%Type(1:len_trim(Interferogram%Type))      == 'C'  ) then
        call read_data_c(grpid, varidsGrp(6), Interferogram%Complex, start_r_c, count_r_c)
     end if

     ! deallocate local variables
     deallocate(varidsGrp, stat=ErrCode)

     return

   end subroutine read_interferogram_data

   subroutine get_interferogram_data(ncid, Interferogram)

     implicit none

     integer(kind=LONG), intent(in)  :: ncid
     type(type_Interferogram), intent(inout) :: Interferogram

     integer(kind=LONG) :: status
     integer(kind=LONG) :: ndimRoot, nvarRoot, nattRoot, nvar, dimid
     integer(kind=LONG) :: ErrCode
     integer(kind=LONG), dimension(:), allocatable :: varidsRoot

     ! get information about the root dataset
     status = nf90_inquire(ncid, nDimensions=ndimRoot, nVariables=nvarRoot, nAttributes=nattRoot)
     if (status /= nf90_noerr) call handle_err(status)

     ! get variable ids from root dataset
     allocate(varidsRoot(nvarRoot), stat=ErrCode)
     ! varidsRoot(1):LINE, varidsRoot(2):For, varidsRoot(3):ColFov, varidsRoot(4):LinFov
     status = nf90_inq_varids(ncid, nvar, varidsRoot)
     if (status /= nf90_noerr) call handle_err(status)

     ! get Nline
     status = nf90_inq_dimid(ncid, "Nline", dimid)
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_inquire_dimension(ncid, dimid, len = Interferogram%TopLevelIn%Nline)
     if(status /= nf90_NoErr) call handle_err(status)

     ! get NFor
     status = nf90_inq_dimid(ncid, "NFor", dimid)
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_inquire_dimension(ncid, dimid, len = Interferogram%TopLevelIn%NFor)
     if(status /= nf90_NoErr) call handle_err(status)

     ! note: NcolFov is known before reading

     ! note: NlinFov is known before reading

     ! read values for global variables
     ! Line
     status = nf90_get_var(ncid, varidsRoot(1), Interferogram%TopLevelIn%Line)
     if (status /= nf90_noerr) call handle_err(status)

     ! For
     status = nf90_get_var(ncid, varidsRoot(2), Interferogram%TopLevelIn%For)
     if (status /= nf90_noerr) call handle_err(status)

     ! note: ColFov is known before reading

     ! note: LinFov is known before reading

     ! read values for global attributes
     ! instrument name
     status = nf90_get_att(ncid, NF90_GLOBAL, name="InsName", values=Interferogram%TopLevelIn%header%InsName)
     if (status /= nf90_noerr) call handle_err(status)

     ! instrument mode
     status = nf90_get_att(ncid, NF90_GLOBAL, name="InsMode", values=Interferogram%TopLevelIn%header%InsMode)
     if (status /= nf90_noerr) call handle_err(status)

     ! S/W version
     status = nf90_get_att(ncid, NF90_GLOBAL, name="SoftWareVersion", values=Interferogram%TopLevelIn%header%SoftWareVersion)
     if (status /= nf90_noerr) call handle_err(status)

     ! Operator
     status = nf90_get_att(ncid, NF90_GLOBAL, name="Operator", values=Interferogram%TopLevelIn%header%Operator)
     if (status /= nf90_noerr) call handle_err(status)

     ! Processing step
     status = nf90_get_att(ncid, NF90_GLOBAL, name="ProcessingStep", values=Interferogram%TopLevelIn%header%ProcessingStep)
     if (status /= nf90_noerr) call handle_err(status)

     ! Conf ID
     status = nf90_get_att(ncid, NF90_GLOBAL, name="ConfId", values=Interferogram%TopLevelIn%header%ConfId)
     if (status /= nf90_noerr) call handle_err(status)

     ! Type
     status = nf90_get_att(ncid, NF90_GLOBAL, name="Type", values=Interferogram%Type)
     if (status /= nf90_noerr) call handle_err(status)

     ! Target
     status = nf90_get_att(ncid, NF90_GLOBAL, name="Target", values=Interferogram%TopLevelIn%Target)
     if (status /= nf90_noerr) call handle_err(status)

     ! deallocate local variables
     deallocate(varidsRoot, stat=ErrCode)

     return

   end subroutine get_interferogram_data
!
!
end module interferogram_type
