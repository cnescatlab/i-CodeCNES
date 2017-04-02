!!* spectrum_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: october 2009
!!*           Version: $Revision: 1.8 $
!!* Last modification: $Date: 2011-06-22 10:06:27 $
!!
!> spectrum_type -- Module
!!
!! * Purpose
!!
!!   Module for spectrum type declaration and allocation.
!!
!! * Description
!!      
!!     This module defines the spectrum type and allows its allocation and declaration. 
!!
!! * Sub-routines and functions
!!
!!     * type_Spectrum     : type for declaration and allocation of Spectrum
!!     * type_Spectrum_subfov : type for declaration and allocation of Spectrum by sub-field of view
!!     * type_Spectrum_sb  : type for declaration and allocation of Spectrum by spectral band
!!     * type_Spectrum_for : type for declaration and allocation of Spectrum by field of regard
!!     * type_Spectrum_line: type for declaration and allocation of Spectrum by line
!!     * type_Spectrum_full: type for declaration and allocation of full Spectrum
!!     * type_Spectrum_full_index: type for declaration and allocation of spectrum indexes
!!     * alloc_Spectrum    : type_Spectrum allocation
!!     * dalloc_Spectrum   : type_Spectrum deallocation
!!     * alloc_Spectrum_subfov : type_Spectrum_subfov allocation
!!     * dalloc_Spectrum_subfov: type_Spectrum_subfov deallocation
!!     * alloc_Spectrum_sb     : type_Spectrum_sb  allocation
!!     * dalloc_Spectrum_sb    : type_Spectrum_sb  deallocation
!!     * alloc_Spectrum_fov    : type_Spectrum_fov allocation
!!     * dalloc_Spectrum_fov   : type_Spectrum_fov deallocation
!!     * alloc_Spectrum_for    : type_Spectrum_for allocation
!!     * dalloc_Spectrum_for   : type_Spectrum_for deallocation
!!     * alloc_Spectrum_line   : type_Spectrum_line allocation
!!     * dalloc_Spectrum_line  : type_Spectrum_line deallocation
!!     * Spectrum_Basis    : computes spectrum spectral base in wavenumber
!!     * readspectrum      : type_Spectrum reading routine
!!     * writespectrum     : type_Spectrum writing routine
!!     * Spectrum_Header_Transfer : transfers Spectrum type
!!     * writespectrum_netcdf     : type_Spectrum writing routine (netCDF format)
!!     * write_spectrum_data      : type_Spectrum writing routine (netCDF format)
!!     * init_spectrum_data       : creation and initialization of the netcdf file
!!     * readspectrum_netcdf      : type_Spectrum reading routine (netCDF format)
!!     * read_spectrum_data       : type_Spectrum reading routine (netCDF format)
!!     * get_spectrum_data        : get global data from the netcdf file
!!     * check_indices_spectrum_full : checks indices of the compounds type to write into output file
!!
!! * References
!!     
!!      SPS ATBD
!!
!! modified 05.07.2013 (JD) netCDF-4 interfaces
!! modified 17.07.2013 (JD) check_indices_spectrum_full added
module spectrum_type
   use precision_type
   use error_type
   use spect_interf_index_type
   use modinouttools
   use modionetcdf
   use netcdf

!
   implicit none
!
!
   public :: type_Spectrum   &
            ,type_Spectrum_subfov &
            ,type_Spectrum_sb     &
            ,type_Spectrum_fov    &
            ,type_Spectrum_for    &
            ,type_Spectrum_line   &
            ,type_Spectrum_full   &
            ,type_Spectrum_full_index &
            ,alloc_Spectrum  &
            ,dalloc_Spectrum &
            ,alloc_Spectrum_subfov  &
            ,dalloc_Spectrum_subfov &
            ,alloc_Spectrum_sb  &
            ,dalloc_Spectrum_sb &
            ,alloc_Spectrum_fov  &
            ,dalloc_Spectrum_fov &
            ,alloc_Spectrum_for  &
            ,dalloc_Spectrum_for &
            ,alloc_Spectrum_line  &
            ,dalloc_Spectrum_line &
            ,dalloc_Spectrum_full &
            ,alloc_Spectrum_full_index &
            ,dalloc_Spectrum_full_index &
            ,Spectrum_Basis  &
            ,readspectrum    &
            ,writespectrum   &
            ,Spectrum_Header_Transfer &
            ,spectrum_transfert &
            ,check_indices_spectrum_full &
            ,writespectrum_netcdf &
            ,readspectrum_netcdf
!
   type :: type_Spectrum
     type(type_spect_interf_index)                    :: TopLevelSp ! < spectrum top level indexes
     character(len=500)                               :: filename !< spectrum file name
     character(len=2)                                 :: Type !< spectrum type
     integer(kind=LONG)                               :: N_Sample !< spectrum samples number
     real(kind=DOUBLE)                                :: Date !< date
     real(kind=DOUBLE)                                :: Lat !< latitude
     real(kind=DOUBLE)                                :: Lon !< longitude
     real(kind=DOUBLE)                                :: Sat_Az !< Sat Azimuth
     real(kind=DOUBLE)                                :: Sat_El !< Sat Elevation
     real(kind=DOUBLE)                                :: Sun_Az !< Sun Azimuth
     real(kind=DOUBLE)                                :: Sun_El !< Sun Elevation
     integer(kind=LONG)                               :: Ns_First !< first sample
     integer(kind=LONG)                               :: Ns_Last !< last sample
     real(kind=DOUBLE)                                :: Wn_First !< first wavenumber
     real(kind=DOUBLE)                                :: Wn_Last !< last wavenumber
     real(kind=DOUBLE)                                :: WnMax !< maximum wavenumber
     real(kind=DOUBLE)                                :: dWn !< wavenumber sampling
     real(kind=DOUBLE)    , dimension(:), allocatable :: Wn !< wavenumber sampling
     real(kind=DOUBLE)    , dimension(:), allocatable :: Real_Part !< spectrum real;W/(m2 sr m-1)
     real(kind=DOUBLE)    , dimension(:), allocatable :: Imag_Part !< spectrum imaginary part -  Imag_Part(1:N_Sample)
     real(kind=DOUBLE)    , dimension(:), allocatable :: Modulus !< spectrum modulus part - Modulus(1:N_Sample)
     real(kind=DOUBLE)    , dimension(:), allocatable :: Argument !< spectrum argument part - Argument(1:N_Sample)
     complex(kind=DOUBLE) , dimension(:), allocatable :: Complex !< complex spectrum - Complex(1:N_Sample)
   end type type_Spectrum
!
!
   type :: type_Spectrum_subfov
     integer(kind=LONG) :: NsubFov ! < sub-field of view number;N.A.;1;nf_int;4;global group dimensions
     integer(kind=LONG),dimension(:),allocatable :: SubFov    !< sub fov;N.A.;NsubFov;nf_int;4;no group
     type(type_Spectrum),dimension(:),allocatable :: Spectrum !< type spectrum;N.A.;NsubFov;Spectrum
  end type type_Spectrum_subfov
!
!
   type :: type_Spectrum_sb
     integer(kind=LONG) :: Nband ! < number of spectral band;N.A.;UNLIMITED;nf_int;4;global group dimensions
     integer(kind=LONG),dimension(:),allocatable :: Band    !< spectral band;N.A.;Nband;nf_int;4; no group
     type(type_Spectrum_subfov),dimension(:),allocatable :: Spectrum_subfov ! < type spectrum_subfov;N.A.;Nband;Spectrum
   end type type_Spectrum_sb
!
!
   type :: type_Spectrum_fov
     integer(kind=LONG) :: NcolFov ! < number of column of the FoV matrix;N.A.;NcolFov;nf_int;4;global group dimensions
     integer(kind=LONG) :: NlinFov ! < number of line of the FoV matrix;N.A.;NlinFov;nf_int;4;global group dimensions
     integer(kind=LONG),dimension(:),allocatable  :: FovCol ! < FoV column;N.A.;NcolFov;nf_int;4;no group
     integer(kind=LONG),dimension(:),allocatable  :: FovLin ! < FoV line;N.A.;NlinFov;nf_int;4;no group
     type(type_Spectrum_sb),dimension(:,:),allocatable :: Spectrum_sb ! < type spectrum_sb;N.A.;NcolFov,NlinFov;Spectrum
   end type type_Spectrum_fov
!
!
   type :: type_Spectrum_for
     integer(kind=LONG) :: NFor ! < field of view number;N.A.;1;nf_int;4;global group dimensions
     integer(kind=LONG),dimension(:),allocatable      :: For ! < field of regard;N.A.;NFor;nf_int;4;no group
     type(type_Spectrum_fov),dimension(:),allocatable :: Spectrum_fov ! < type spectrum_fov;N.A.;NFoR;Spectrum
   end type type_Spectrum_for
!
!
   type :: type_Spectrum_line
     integer(kind=LONG) :: Nline ! < number of line of the product
     integer(kind=LONG),dimension(:),allocatable      :: Line ! < line
     type(type_Spectrum_for),dimension(:),allocatable :: Spectrum_for ! < type spectrum_for
   end type type_Spectrum_line
!
   type :: type_Spectrum_full
     type(type_header)       :: header_spectrum ! < type header
     character(len=2)        :: Type !< spectrum type
     character(len=2)        :: Target !< target type
     type(type_Spectrum_line):: Spectrum_line ! < type spectrum_line
   end type type_Spectrum_full
!
   type :: type_Spectrum_full_index
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
     integer(kind=LONG), dimension(:,:), allocatable :: index_spectrum !< start end of index spectrum
   end type type_Spectrum_full_index
   !
   contains
!
!
   subroutine alloc_Spectrum( Spectrum )
     implicit none
     type(type_Spectrum), intent(inout) :: Spectrum
     integer(kind=LONG)                 :: ErrCode
!
     allocate( Spectrum%Wn(Spectrum%N_Sample),           stat=ErrCode )

     if( Spectrum%Type(1:len_trim(Spectrum%Type))      == 'C'  ) then
        allocate( Spectrum%Complex(Spectrum%N_Sample),   stat=ErrCode )
     else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'RI' ) then
        allocate( Spectrum%Real_Part(Spectrum%N_Sample),   stat=ErrCode )
        allocate( Spectrum%Imag_Part(Spectrum%N_Sample),   stat=ErrCode )
     else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'MA' ) then
        allocate( Spectrum%Modulus(Spectrum%N_Sample),   stat=ErrCode )
        allocate( Spectrum%Argument(Spectrum%N_Sample),   stat=ErrCode )
     else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'R'  ) then
        allocate( Spectrum%Real_Part(Spectrum%N_Sample),   stat=ErrCode )
     else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'I'  ) then
        allocate( Spectrum%Imag_Part(Spectrum%N_Sample),   stat=ErrCode )
     else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'M'  ) then
        allocate( Spectrum%Modulus(Spectrum%N_Sample),   stat=ErrCode )
     else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'A'  ) then
        allocate( Spectrum%Argument(Spectrum%N_Sample),   stat=ErrCode )
     else
        write(*,'(a,a)') 'Spectrum%Type Error :',Spectrum%Type
        write(*,*) 'alloc_Spectrum Fatal Error'
        ErrCode = 1
     end if
!
     if (ErrCode > 0) then
        write(0,*) 'allocation Spectrum Error'
        write(0,*) 'Spectrum: fatal error'
        call exit(1)
     end if
     return
   end subroutine alloc_Spectrum
!
!
   subroutine dalloc_Spectrum( Spectrum, verbose_argument )
     implicit none
     type(type_Spectrum), intent(inout) :: Spectrum
     logical, intent(in), optional :: verbose_argument
     logical :: verbose
     integer(kind=LONG)                 :: ErrCode

!
     verbose=.false.
     if (present(verbose_argument)) verbose=verbose_argument
     if (verbose) write(*,*) "allocated(Spectrum%Wn)",allocated(Spectrum%Wn)
     if (allocated(Spectrum%Wn)) then
        deallocate( Spectrum%Wn,           stat=ErrCode )
        if (ErrCode > 0) then
           write(0,*) 'deallocation Spectrum Error'
           write(0,*) 'Spectrum: fatal error'
           call exit(1)
        end if
     end if
     if( Spectrum%Type(1:len_trim(Spectrum%Type))      == 'C'  ) then
        if (verbose) write(*,*) "allocated(Spectrum%Complex)",allocated(Spectrum%Complex)
        deallocate( Spectrum%Complex )
     else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'RI' ) then
        if (verbose) write(*,*) "allocated(Spectrum%Real_Part)",allocated(Spectrum%Real_Part)
        deallocate( Spectrum%Real_Part )
        if (verbose) write(*,*) "allocated(Spectrum%Imag_Part)",allocated(Spectrum%Imag_Part)
        deallocate( Spectrum%Imag_Part )
     else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'MA' ) then
        if (verbose) write(*,*) "allocated(Spectrum%Modulus)",allocated(Spectrum%Modulus)
        deallocate( Spectrum%Modulus )
        if (verbose) write(*,*) "allocated(Spectrum%Argument)",allocated(Spectrum%Argument)
        deallocate( Spectrum%Argument )
     else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'R'  ) then
        if (verbose) write(*,*) "allocated(Spectrum%Real_Part)",allocated(Spectrum%Real_Part)
        if (allocated(Spectrum%Real_Part)) then
           deallocate( Spectrum%Real_Part,           stat=ErrCode )
        if (ErrCode > 0) then
           write(0,*) 'deallocation Spectrum Error'
           write(0,*) 'Spectrum: fatal error'
           call exit(1)
        end if
     end if
     else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'I'  ) then
        if (verbose) write(*,*) "allocated(Spectrum%Imag_Part)",allocated(Spectrum%Imag_Part)
        deallocate( Spectrum%Imag_Part)
     else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'M'  ) then
        if (verbose) write(*,*) "allocated(Spectrum%Modulus)",allocated(Spectrum%Modulus)
        deallocate( Spectrum%Modulus )
     else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'A'  ) then
        if (verbose) write(*,*) "allocated(Spectrum%Argument)",allocated(Spectrum%Argument)
        deallocate( Spectrum%Argument )
     else
        write(*,'(a,a)') 'Spectrum%Type Error :',Spectrum%Type
        write(*,*) 'dalloc_Spectrum Fatal Error'
        call exit(1)
     end if
!
     return
   end subroutine dalloc_Spectrum
!
!
   subroutine alloc_Spectrum_subfov( Spectrum_subfov )
   implicit none
     type(type_Spectrum_subfov), intent(inout)     :: Spectrum_subfov
     integer(kind=LONG)                            :: ErrCode
!
     allocate( Spectrum_subfov%Spectrum( Spectrum_subfov%NsubFov ),  stat=ErrCode )
     allocate( Spectrum_subfov%SubFov( Spectrum_subfov%NsubFov ),  stat=ErrCode )
     if( ErrCode > 0 ) then
        write(*,*) 'allocation Spectrum_subfov Error',ErrCode
        write(*,*) 'type_Spectrum_subfov: fatal error'
        call exit(1)
     end if
! defaut initialisation
     Spectrum_subfov%SubFov(:) = 0
     return
   end subroutine alloc_Spectrum_subfov
!
!
   subroutine dalloc_Spectrum_subfov( Spectrum_subfov )
   implicit none
     type(type_Spectrum_subfov), intent(inout) :: Spectrum_subfov
!
     deallocate( Spectrum_subfov%Spectrum )
     deallocate( Spectrum_subfov%SubFov)
     return
   end subroutine dalloc_Spectrum_subfov
!
!
   subroutine alloc_Spectrum_sb( Spectrum_sb )
   implicit none
     type(type_Spectrum_sb), intent(inout)     :: Spectrum_sb
     integer(kind=LONG)                        :: ErrCode
!
     allocate( Spectrum_sb%Band( Spectrum_sb%Nband ),  stat=ErrCode )
     allocate( Spectrum_sb%Spectrum_subfov( Spectrum_sb%Nband ),  stat=ErrCode )
     if( ErrCode > 0 ) then
        write(*,*) 'allocation Spectrum_sb Error',ErrCode
        write(*,*) 'type_Spectrum_sb: fatal error'
        call exit(1)
     end if
! defaut initialisation
     Spectrum_sb%Band(:) = 0
     return
   end subroutine alloc_Spectrum_sb
!
!
   subroutine dalloc_Spectrum_sb( Spectrum_sb )
   implicit none
     type(type_Spectrum_sb), intent(inout) :: Spectrum_sb
!
     deallocate( Spectrum_sb%Band )
     deallocate( Spectrum_sb%Spectrum_subfov )
     return
   end subroutine dalloc_Spectrum_sb
!
!
   subroutine alloc_Spectrum_fov( Spectrum_fov )
   implicit none
     type(type_Spectrum_fov), intent(inout)    :: Spectrum_fov
     integer(kind=LONG)                        :: ErrCode
!
     allocate( Spectrum_fov%FovCol( Spectrum_fov%NcolFov),  stat=ErrCode )
     allocate( Spectrum_fov%FovLin(Spectrum_fov%NlinFov ),  stat=ErrCode )
     allocate( Spectrum_fov%Spectrum_sb( Spectrum_fov%NcolFov,Spectrum_fov%NlinFov ),  stat=ErrCode )
     if( ErrCode > 0 ) then
        write(*,*) 'allocation Spectrum_fov Error',ErrCode
        write(*,*) 'type_Spectrum_fov: fatal error'
        call exit(1)
     end if
 ! defaut initialisation
     Spectrum_fov%FovCol(:) = 0
     Spectrum_fov%FovLin(:) = 0
     return
   end subroutine alloc_Spectrum_fov
!
!
   subroutine dalloc_Spectrum_fov( Spectrum_fov )
   implicit none
     type(type_Spectrum_fov), intent(inout) :: Spectrum_fov
     deallocate( Spectrum_fov%FovCol )
     deallocate( Spectrum_fov%FovLin )
     deallocate( Spectrum_fov%Spectrum_sb )
     return
   end subroutine dalloc_Spectrum_fov
!
!
   subroutine alloc_Spectrum_for( Spectrum_for )
   implicit none
     type(type_Spectrum_for), intent(inout)    :: Spectrum_for
     integer(kind=LONG)                        :: ErrCode
!
     allocate( Spectrum_for%For( Spectrum_for%NFor ),  stat=ErrCode )
     allocate( Spectrum_for%Spectrum_fov( Spectrum_for%NFor ),  stat=ErrCode )
     if( ErrCode > 0 ) then
        write(*,*) 'allocation Spectrum_for Error',ErrCode
        write(*,*) 'type_Spectrum_for: fatal error'
        call exit(1)
     end if
     ! defaut initialisation
     Spectrum_for%For(:) = 0
     return
   end subroutine alloc_Spectrum_for
!
!
   subroutine dalloc_Spectrum_for( Spectrum_for )
   implicit none
     type(type_Spectrum_for), intent(inout) :: Spectrum_for
     deallocate( Spectrum_for%For)
     deallocate( Spectrum_for%Spectrum_fov )
     return
   end subroutine dalloc_Spectrum_for
!
!
   subroutine alloc_Spectrum_line( Spectrum_line )
   implicit none
     type(type_Spectrum_line), intent(inout)   :: Spectrum_line
     integer(kind=LONG)                        :: ErrCode
!
     allocate( Spectrum_line%Line( Spectrum_line%Nline ),  stat=ErrCode )
     allocate( Spectrum_line%Spectrum_for( Spectrum_line%Nline ),  stat=ErrCode )
     if( ErrCode > 0 ) then
        write(*,*) 'allocation Spectrum_line Error',ErrCode
        write(*,*) 'type_Spectrum_line: fatal error'
        call exit(1)
     end if
     ! defaut initialisation
     Spectrum_line%Line(:) = 0
     return
   end subroutine alloc_Spectrum_line
!
!
   subroutine dalloc_Spectrum_line( Spectrum_line )
   implicit none
     type(type_Spectrum_line), intent(inout) :: Spectrum_line
     deallocate( Spectrum_line%Line )
     deallocate( Spectrum_line%Spectrum_for )
     return
   end subroutine dalloc_Spectrum_line
!
!
   subroutine dalloc_Spectrum_full( Spectrum_full)
   implicit none
   type(type_Spectrum_full), intent(inout) :: Spectrum_full
   integer(kind=LONG) :: line, for, fov_col, fov_lin, sb
!
   do line = 1, Spectrum_full%Spectrum_line%Nline
      do for = 1, Spectrum_full%Spectrum_line%Spectrum_for(line)%NFor
         do fov_col = 1, Spectrum_full%Spectrum_line%Spectrum_for(line)%Spectrum_fov(for)%NcolFov
            do fov_lin = 1, Spectrum_full%Spectrum_line%Spectrum_for(line)%Spectrum_fov(for)%NlinFov
               do sb = 1, Spectrum_full%Spectrum_line%Spectrum_for(line) &
                           %Spectrum_fov(for)%Spectrum_sb(fov_col,fov_lin)%Nband
                  call dalloc_Spectrum_subfov( Spectrum_full%Spectrum_line%Spectrum_for(line) &
                           %Spectrum_fov(for)%Spectrum_sb(fov_col,fov_lin)%Spectrum_subfov(sb) )
               end do
            end do
         end do
      end do
   end do
   do line = 1, Spectrum_full%Spectrum_line%Nline
      do for = 1, Spectrum_full%Spectrum_line%Spectrum_for(line)%NFor
         do fov_col = 1, Spectrum_full%Spectrum_line%Spectrum_for(line)%Spectrum_fov(for)%NcolFov
            do fov_lin = 1, Spectrum_full%Spectrum_line%Spectrum_for(line)%Spectrum_fov(for)%NlinFov
               call dalloc_Spectrum_sb( Spectrum_full%Spectrum_line%Spectrum_for(line) &
                           %Spectrum_fov(for)%Spectrum_sb(fov_col,fov_lin) )
            end do
         end do
      end do
   end do
   do line = 1, Spectrum_full%Spectrum_line%Nline
      do for = 1, Spectrum_full%Spectrum_line%Spectrum_for(line)%NFor
         call dalloc_Spectrum_fov( Spectrum_full%Spectrum_line%Spectrum_for(line) &
                           %Spectrum_fov(for) )
      end do
   end do
   do line = 1, Spectrum_full%Spectrum_line%Nline
      call dalloc_Spectrum_for( Spectrum_full%Spectrum_line%Spectrum_for(line) )
   end do
   call dalloc_Spectrum_line( Spectrum_full%Spectrum_line )

   return
   end subroutine dalloc_Spectrum_full
!
!
   subroutine alloc_Spectrum_full_index( Spectrum_full, bound )
   implicit none
     type(type_Spectrum_full), intent(in) :: Spectrum_full
     type(type_Spectrum_full_index), intent(inout) :: bound
     integer(kind=LONG) :: ErrCode
!
     ErrCode = 0
     allocate( bound%indices_line( Spectrum_full%Spectrum_line%Nline ),  stat=ErrCode )
     allocate( bound%indices_for( Spectrum_full%Spectrum_line%Spectrum_for(1)%NFor),  stat=ErrCode )
     allocate( bound%indices_fovcol( Spectrum_full%Spectrum_line%Spectrum_for(1) &
       %Spectrum_fov(1)%NcolFov),  stat=ErrCode )
     allocate( bound%indices_fovlin( Spectrum_full%Spectrum_line%Spectrum_for(1) &
       %Spectrum_fov(1)%NlinFov),  stat=ErrCode )
     allocate( bound%indices_band( Spectrum_full%Spectrum_line%Spectrum_for(1) &
       %Spectrum_fov(1)%Spectrum_sb(1,1)%Nband),  stat=ErrCode )
     allocate( bound%indices_subfov( Spectrum_full%Spectrum_line%Spectrum_for(1) &
       %Spectrum_fov(1)%Spectrum_sb(1,1)%Spectrum_subfov(1)%NsubFov),  stat=ErrCode )
     allocate( bound%index_spectrum(2,Spectrum_full%Spectrum_line%Spectrum_for(1) &
       %Spectrum_fov(1)%Spectrum_sb(1,1)%Nband),  stat=ErrCode )
     if ( ErrCode > 0 ) then
       write(*,*) 'allocation indices_line Error', ErrCode
       call exit(1)
     end if

     return
   end subroutine alloc_Spectrum_full_index
!
!
    subroutine dalloc_Spectrum_full_index( bound )
    implicit none
     type(type_Spectrum_full_index), intent(inout) :: bound
!
     deallocate( bound%indices_line )
     deallocate( bound%indices_for )
     deallocate( bound%indices_fovcol )
     deallocate( bound%indices_fovlin )
     deallocate( bound%indices_band )
     deallocate( bound%indices_subfov )
     deallocate( bound%index_spectrum )

     return
   end subroutine dalloc_Spectrum_full_index

!
!
   subroutine Spectrum_Basis( Spectrum )
     implicit none
     type(type_Spectrum), intent(inout) :: Spectrum
!
     integer(kind=LONG)                 :: Ns
!
     Spectrum%Wn(1:Spectrum%N_Sample) =                            &
          (/ (dble(Ns-1),Ns=Spectrum%Ns_First,Spectrum%Ns_Last) /) &
          * Spectrum%dWn
!
     return
   end subroutine Spectrum_Basis
!
! Checks indices of the compound type to be written
   subroutine check_indices_spectrum_full( Spectrum_full, bound)
   implicit none
   type(type_Spectrum_full), intent(in) :: Spectrum_full
   type(type_Spectrum_full_index), intent(inout) :: bound
   integer(kind=LONG) :: line, for, fov_col, fov_lin, sb, sub_fov, loop
   integer(kind=LONG) :: nvalue
   !
   bound%indices_line(:) = 0
   bound%indices_for(:) = 0
   bound%indices_fovcol(:) = 0
   bound%indices_fovlin(:) = 0
   bound%indices_band(:) = 0
   bound%indices_subfov(:) = 0
   bound%index_spectrum(1,:) = 1
   where ( Spectrum_full%Spectrum_line%Line .gt. 0 ) bound%indices_line = 1
   do line = 1, Spectrum_full%Spectrum_line%Nline
      where ( Spectrum_full%Spectrum_line%Spectrum_for(line)%For .gt. 0 ) bound%indices_for = 1
      do for = 1, Spectrum_full%Spectrum_line%Spectrum_for(line)%NFor
         where ( Spectrum_full%Spectrum_line%Spectrum_for(line) &
                 %Spectrum_fov(for)%FovCol .gt. 0 ) bound%indices_fovcol = 1
         where ( Spectrum_full%Spectrum_line%Spectrum_for(line) &
                 %Spectrum_fov(for)%FovLin .gt. 0 ) bound%indices_fovlin = 1
         do fov_col = 1, Spectrum_full%Spectrum_line%Spectrum_for(line)%Spectrum_fov(for)%NcolFov
            do fov_lin = 1, Spectrum_full%Spectrum_line%Spectrum_for(line)%Spectrum_fov(for)%NlinFov
               where ( Spectrum_full%Spectrum_line%Spectrum_for(line) &
                        %Spectrum_fov(for)%Spectrum_sb(fov_col,fov_lin)%Band .gt. 0 ) bound%indices_band = 1
               do sb = 1, Spectrum_full%Spectrum_line%Spectrum_for(line) &
                                %Spectrum_fov(for)%Spectrum_sb(fov_col,fov_lin)%Nband
                  where ( Spectrum_full%Spectrum_line%Spectrum_for(line)   &
                           %Spectrum_fov(for)%Spectrum_sb(fov_col,fov_lin) &
                           %Spectrum_subfov(sb)%SubFov .gt. 0 ) bound%indices_subfov = 1
                  do sub_fov = 1, Spectrum_full%Spectrum_line%Spectrum_for(line)   &
                           %Spectrum_fov(for)%Spectrum_sb(fov_col,fov_lin) &
                           %Spectrum_subfov(sb)%NsubFov
                     bound%index_spectrum(2,sb) = Spectrum_full%Spectrum_line%Spectrum_for(line) &
                      %Spectrum_fov(for)%Spectrum_sb(fov_col,fov_lin) &
                      %Spectrum_subfov(sb)%Spectrum(sub_fov)%N_Sample
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
   end subroutine check_indices_spectrum_full
!
!

   subroutine readspectrum( Spectrum,iostatus )
     implicit none
     type(type_Spectrum), intent(inout) :: Spectrum
!
     integer(kind=LONG) , intent(out)   :: iostatus
     integer(kind=DOUBLE)               :: ifile
     integer(kind=LONG)                 :: offset
     integer(kind=LONG)                 :: Type
     integer(kind=LONG)                 :: Size
!
      iostatus = 0
      call open_file_r(Spectrum%filename(1:len_trim(Spectrum%filename))      &
                       // char(0), ifile)
      if ( ifile .eq. 0 ) then
         iostatus = 1
      else
         offset = 0
         Type = ch2Type
         Size = 2
         call read_field( ifile, Spectrum%Type, %VAL(Type),                  &
                                   %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4
         call read_field( ifile, Spectrum%N_Sample, %VAL(Type),              &
                                   %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8
         call read_field( ifile, Spectrum%Date, %VAL(Type),                  &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Spectrum%Lat, %VAL(Type),                   &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Spectrum%Lon, %VAL(Type),                   &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Spectrum%Sat_Az, %VAL(Type),                &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Spectrum%Sat_El, %VAL(Type),                &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Spectrum%Sun_Az, %VAL(Type),                &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Spectrum%Sun_El, %VAL(Type),                &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4
         call read_field( ifile, Spectrum%Ns_First, %VAL(Type),              &
                                   %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Spectrum%Ns_Last, %VAL(Type),               &
                                   %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8
         call read_field( ifile, Spectrum%Wn_First, %VAL(Type),              &
                                   %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Spectrum%Wn_Last, %VAL(Type),               &
                                   %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Spectrum%WnMax, %VAL(Type),                 &
                                   %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Spectrum%dWn, %VAL(Type),                   &
                                   %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call alloc_Spectrum( Spectrum )
         Type = r8Type
         Size = 8*Spectrum%N_Sample
         call read_field( ifile, Spectrum%Wn, %VAL(Type),                    &
                                   %VAL(Size), %VAL(offset) )
         offset = offset + Size
         if( Spectrum%Type(1:len_trim(Spectrum%Type))      == 'C'  ) then
            Type = c16Type
            Size = 16*Spectrum%N_Sample
            call read_field( ifile, Spectrum%Complex, %VAL(Type),         &
                                      %VAL(Size), %VAL(offset) )
            offset = offset + Size
         else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'RI' ) then
            Type = r8Type
            Size = 8*Spectrum%N_Sample
            call read_field( ifile, Spectrum%Real_Part, %VAL(Type),         &
                                      %VAL(Size), %VAL(offset) )
            offset = offset + Size
            call read_field( ifile, Spectrum%Imag_Part, %VAL(Type),         &
                                      %VAL(Size), %VAL(offset) )
            offset = offset + Size
         else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'MA' ) then
            Type = r8Type
            Size = 8*Spectrum%N_Sample
            call read_field( ifile, Spectrum%Modulus, %VAL(Type),         &
                                      %VAL(Size), %VAL(offset) )
            offset = offset + Size
            call read_field( ifile, Spectrum%Argument, %VAL(Type),         &
                                      %VAL(Size), %VAL(offset) )
            offset = offset + Size
         else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'R'  ) then
            Type = r8Type
            Size = 8*Spectrum%N_Sample
            call read_field( ifile, Spectrum%Real_Part, %VAL(Type),         &
                                      %VAL(Size), %VAL(offset) )
            offset = offset + Size
         else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'I'  ) then
            Type = r8Type
            Size = 8*Spectrum%N_Sample
            call read_field( ifile, Spectrum%Imag_Part, %VAL(Type),         &
                                      %VAL(Size), %VAL(offset) )
            offset = offset + Size
         else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'M'  ) then
            Type = r8Type
            Size = 8*Spectrum%N_Sample
            call read_field( ifile, Spectrum%Modulus, %VAL(Type),         &
                                      %VAL(Size), %VAL(offset) )
            offset = offset + Size
         else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'A'  ) then
            Type = r8Type
            Size = 8*Spectrum%N_Sample
            call read_field( ifile, Spectrum%Argument, %VAL(Type),         &
                                      %VAL(Size), %VAL(offset) )
            offset = offset + Size
         end if
         call close_file(Spectrum%filename(1:len_trim(Spectrum%filename))    &
                         // char(0), ifile)
      end if
!
   return
   end subroutine readspectrum
!
!

   subroutine readspectrum_old( Spectrum,iostatus )
     implicit none
     type(type_Spectrum), intent(inout) :: Spectrum
!
     integer(kind=LONG) , intent(out)   :: iostatus
     integer(kind=DOUBLE)               :: ifile
     integer(kind=LONG)                 :: offset
     integer(kind=LONG)                 :: Type
     integer(kind=LONG)                 :: Size
!
      iostatus = 0
      call open_file_r(Spectrum%filename(1:len_trim(Spectrum%filename))      &
                       // char(0), ifile)
      if ( ifile .eq. 0 ) then
         iostatus = 1
      else
         offset = 0
         Type = ch2Type
         Size = 2
         call read_field( ifile, Spectrum%Type, %VAL(Type),                  &
                                   %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4
         call read_field( ifile, Spectrum%N_Sample, %VAL(Type),              &
                                   %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Spectrum%Ns_First, %VAL(Type),              &
                                   %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Spectrum%Ns_Last, %VAL(Type),               &
                                   %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8
         call read_field( ifile, Spectrum%Wn_First, %VAL(Type),              &
                                   %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Spectrum%Wn_Last, %VAL(Type),               &
                                   %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Spectrum%WnMax, %VAL(Type),                 &
                                   %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Spectrum%dWn, %VAL(Type),                   &
                                   %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call alloc_Spectrum( Spectrum )
         Type = r8Type
         Size = 8*Spectrum%N_Sample
         call read_field( ifile, Spectrum%Wn, %VAL(Type),                    &
                                   %VAL(Size), %VAL(offset) )
         offset = offset + Size
         if( Spectrum%Type(1:len_trim(Spectrum%Type))      == 'C'  ) then
            Type = c16Type
            Size = 16*Spectrum%N_Sample
            call read_field( ifile, Spectrum%Complex, %VAL(Type),         &
                                      %VAL(Size), %VAL(offset) )
            offset = offset + Size
         else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'RI' ) then
            Type = r8Type
            Size = 8*Spectrum%N_Sample
            call read_field( ifile, Spectrum%Real_Part, %VAL(Type),         &
                                      %VAL(Size), %VAL(offset) )
            offset = offset + Size
            call read_field( ifile, Spectrum%Imag_Part, %VAL(Type),         &
                                      %VAL(Size), %VAL(offset) )
            offset = offset + Size
         else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'MA' ) then
            Type = r8Type
            Size = 8*Spectrum%N_Sample
            call read_field( ifile, Spectrum%Modulus, %VAL(Type),         &
                                      %VAL(Size), %VAL(offset) )
            offset = offset + Size
            call read_field( ifile, Spectrum%Argument, %VAL(Type),         &
                                      %VAL(Size), %VAL(offset) )
            offset = offset + Size
         else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'R'  ) then
            Type = r8Type
            Size = 8*Spectrum%N_Sample
            call read_field( ifile, Spectrum%Real_Part, %VAL(Type),         &
                                      %VAL(Size), %VAL(offset) )
            offset = offset + Size
         else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'I'  ) then
            Type = r8Type
            Size = 8*Spectrum%N_Sample
            call read_field( ifile, Spectrum%Imag_Part, %VAL(Type),         &
                                      %VAL(Size), %VAL(offset) )
            offset = offset + Size
         else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'M'  ) then
            Type = r8Type
            Size = 8*Spectrum%N_Sample
            call read_field( ifile, Spectrum%Modulus, %VAL(Type),         &
                                      %VAL(Size), %VAL(offset) )
            offset = offset + Size
         else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'A'  ) then
            Type = r8Type
            Size = 8*Spectrum%N_Sample
            call read_field( ifile, Spectrum%Argument, %VAL(Type),         &
                                      %VAL(Size), %VAL(offset) )
            offset = offset + Size
         end if
         call close_file(Spectrum%filename(1:len_trim(Spectrum%filename))    &
                         // char(0), ifile)
      end if
!
   return
   end subroutine readspectrum_old
!
!
   subroutine writespectrum( Spectrum,iostatus )
     implicit none
     type(type_Spectrum), intent(in) :: Spectrum
!
     integer(kind=LONG), intent(out) :: iostatus
     integer(kind=DOUBLE)            :: ifile
     integer(kind=LONG)              :: offset
     integer(kind=LONG)              :: Type
     integer(kind=LONG)              :: Size
!
      iostatus = 0
      call open_file_w(Spectrum%filename(1:len_trim(Spectrum%filename))      &
                       // char(0), ifile)
      if ( ifile .eq. 0 ) then
         iostatus = 1
      else
         offset = 0
         Type = ch2Type
         Size = 2
         call write_field( ifile, Spectrum%Type, %VAL(Type),                 &
                                   %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4
         call write_field( ifile, Spectrum%N_Sample, %VAL(Type),             &
                                   %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8
         call write_field( ifile, Spectrum%Date, %VAL(Type),                  &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Spectrum%Lat, %VAL(Type),                   &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Spectrum%Lon, %VAL(Type),                   &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Spectrum%Sat_Az, %VAL(Type),                &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Spectrum%Sat_El, %VAL(Type),                &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Spectrum%Sun_Az, %VAL(Type),                &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Spectrum%Sun_El, %VAL(Type),                &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4
         call write_field( ifile, Spectrum%Ns_First, %VAL(Type),             &
                                   %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Spectrum%Ns_Last, %VAL(Type),              &
                                   %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8
         call write_field( ifile, Spectrum%Wn_First, %VAL(Type),             &
                                   %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Spectrum%Wn_Last, %VAL(Type),              &
                                   %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Spectrum%WnMax, %VAL(Type),                &
                                   %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Spectrum%dWn, %VAL(Type),                  &
                                   %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8*Spectrum%N_Sample
         call write_field( ifile, Spectrum%Wn, %VAL(Type),                   &
                                   %VAL(Size), %VAL(offset) )
         offset = offset + Size
         if( Spectrum%Type(1:len_trim(Spectrum%Type))      == 'C'  ) then
            Type = c16Type
            Size = 16*Spectrum%N_Sample
            call write_field( ifile, Spectrum%Complex, %VAL(Type),        &
                                      %VAL(Size), %VAL(offset) )
            offset = offset + Size
         else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'RI' ) then
            Type = r8Type
            Size = 8*Spectrum%N_Sample
            call write_field( ifile, Spectrum%Real_Part, %VAL(Type),        &
                                      %VAL(Size), %VAL(offset) )
            offset = offset + Size
            call write_field( ifile, Spectrum%Imag_Part, %VAL(Type),        &
                                      %VAL(Size), %VAL(offset) )
            offset = offset + Size
         else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'MA' ) then
            Type = r8Type
            Size = 8*Spectrum%N_Sample
            call write_field( ifile, Spectrum%Modulus, %VAL(Type),        &
                                      %VAL(Size), %VAL(offset) )
            offset = offset + Size
            call write_field( ifile, Spectrum%Argument, %VAL(Type),        &
                                      %VAL(Size), %VAL(offset) )
            offset = offset + Size
         else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'R'  ) then
            Type = r8Type
            Size = 8*Spectrum%N_Sample
            call write_field( ifile, Spectrum%Real_Part, %VAL(Type),        &
                                      %VAL(Size), %VAL(offset) )
            offset = offset + Size
         else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'I'  ) then
            Type = r8Type
            Size = 8*Spectrum%N_Sample
            call write_field( ifile, Spectrum%Imag_Part, %VAL(Type),        &
                                      %VAL(Size), %VAL(offset) )
            offset = offset + Size
         else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'M'  ) then
            Type = r8Type
            Size = 8*Spectrum%N_Sample
            call write_field( ifile, Spectrum%Modulus, %VAL(Type),        &
                                      %VAL(Size), %VAL(offset) )
            offset = offset + Size
         else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'A'  ) then
            Type = r8Type
            Size = 8*Spectrum%N_Sample
            call write_field( ifile, Spectrum%Argument, %VAL(Type),        &
                                      %VAL(Size), %VAL(offset) )
            offset = offset + Size
         end if
         call close_file(Spectrum%filename(1:len_trim(Spectrum%filename))    &
                         // char(0), ifile)
      end if
!
      return
   end subroutine writespectrum
!
!
   subroutine Spectrum_Header_Transfer( Spectrum_In, Spectrum_Out )
     implicit none
     type(type_Spectrum), intent(in)    :: Spectrum_In
     type(type_Spectrum), intent(inout) :: Spectrum_Out

!
     ! header
     Spectrum_Out%Type     = Spectrum_In%Type     
     Spectrum_Out%N_Sample = Spectrum_In%N_Sample
     Spectrum_Out%Ns_First = Spectrum_In%Ns_First
     Spectrum_Out%Ns_Last  = Spectrum_In%Ns_Last
     Spectrum_Out%Wn_First = Spectrum_In%Wn_First 
     Spectrum_Out%Wn_Last  = Spectrum_In%Wn_Last 
     Spectrum_Out%WnMax    = Spectrum_In%WnMax
     Spectrum_Out%dWn      = Spectrum_In%dWn
     !
     call alloc_Spectrum ( Spectrum_Out )
     ! wavenumber basis 
     Spectrum_Out%Wn(1:Spectrum_Out%N_Sample) = &
          Spectrum_In%Wn(1:Spectrum_In%N_Sample)  
     ! Spectrum   
     if( Spectrum_Out%Type(1:len_trim(Spectrum_Out%Type))      == 'C'  ) then
        Spectrum_Out%Complex(1:Spectrum_Out%N_Sample) = cmplx(0,kind=DOUBLE)
     else if( Spectrum_Out%Type(1:len_trim(Spectrum_Out%Type)) == 'RI' ) then
        Spectrum_Out%Real_Part(1:Spectrum_Out%N_Sample) = real(0,kind=DOUBLE)
        Spectrum_Out%Imag_Part(1:Spectrum_Out%N_Sample) = real(0,kind=DOUBLE)
     else if( Spectrum_Out%Type(1:len_trim(Spectrum_Out%Type)) == 'MA' ) then
        Spectrum_Out%Modulus(1:Spectrum_Out%N_Sample)  = real(0,kind=DOUBLE)
        Spectrum_Out%Argument(1:Spectrum_Out%N_Sample) = real(0,kind=DOUBLE)
     else if( Spectrum_Out%Type(1:len_trim(Spectrum_Out%Type)) == 'R'  ) then
        Spectrum_Out%Real_Part(1:Spectrum_Out%N_Sample) = real(0,kind=DOUBLE)
     else if( Spectrum_Out%Type(1:len_trim(Spectrum_Out%Type)) == 'I'  ) then
        Spectrum_Out%Imag_Part(1:Spectrum_Out%N_Sample) = real(0,kind=DOUBLE)
     else if( Spectrum_Out%Type(1:len_trim(Spectrum_Out%Type)) == 'M'  ) then
        Spectrum_Out%Modulus(1:Spectrum_Out%N_Sample) = real(0,kind=DOUBLE)
     else if( Spectrum_Out%Type(1:len_trim(Spectrum_Out%Type)) == 'A'  ) then
        Spectrum_Out%Argument(1:Spectrum_Out%N_Sample) = real(0,kind=DOUBLE)
     else
        write(*,'(a,a)') 'Spectrum_Out%Type Error :',Spectrum_Out%Type
        write(*,*) 'Spectrum_Header_Transfer Fatal Error'
        call exit(1)
     end if
!
     return
   end subroutine Spectrum_Header_Transfer
!
!
!> spectrum_transfert -- Public
!!
!! * Purpose
!!
!!     Equality between two spectra
!!
!! * Description
!!
!!     This subroutine transferts the values of one spectrum type in another spectrum type.
!!
!! * Inputs
!!
!!     - Spectrum_In : type_Spectrum / type for declaration and allocation of spectrum
!!
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Spectrum_Out : type_Spectrum / type for declaration and allocation of spectrum
!!
!! * References
!!
   subroutine spectrum_transfert( Spectrum_In, Spectrum_Out )
   implicit none
     type(type_Spectrum)     , intent(in)                :: Spectrum_In
     type(type_Spectrum)     , intent(out)               :: Spectrum_Out
!
     Spectrum_Out%Type     = Spectrum_In%Type
     Spectrum_Out%dWn      = Spectrum_In%dWn
     Spectrum_Out%Ns_First = Spectrum_In%Ns_First
     Spectrum_Out%Ns_Last  = Spectrum_In%Ns_Last
     Spectrum_Out%Wn_First = Spectrum_In%Wn_First
     Spectrum_Out%Wn_Last  = Spectrum_In%Wn_Last
     Spectrum_Out%WnMax    = Spectrum_In%WnMax
     Spectrum_Out%N_Sample = Spectrum_In%N_Sample
     call alloc_Spectrum( Spectrum_Out )
     Spectrum_Out%Wn(1:Spectrum_Out%N_Sample) =           &
                   Spectrum_In%Wn(1:Spectrum_Out%N_Sample)
     if( Spectrum_Out%Type == 'C' ) then
        Spectrum_Out%Complex(1:Spectrum_Out%N_Sample) =   &
                   Spectrum_In%Complex(1:Spectrum_Out%N_Sample)
     else if( Spectrum_Out%Type == 'RI' ) then
        Spectrum_Out%Real_Part(1:Spectrum_Out%N_Sample) = &
                   Spectrum_In%Real_Part(1:Spectrum_Out%N_Sample)
        Spectrum_Out%Imag_Part(1:Spectrum_Out%N_Sample) = &
                   Spectrum_In%Imag_Part(1:Spectrum_Out%N_Sample)
     else if( Spectrum_Out%Type == 'MA' ) then
        Spectrum_Out%Modulus(1:Spectrum_Out%N_Sample) =   &
                   Spectrum_In%Modulus(1:Spectrum_Out%N_Sample)
        Spectrum_Out%Argument(1:Spectrum_Out%N_Sample) =  &
                   Spectrum_In%Argument(1:Spectrum_Out%N_Sample)
     else if( Spectrum_Out%Type == 'R'  ) then
        Spectrum_Out%Real_Part(1:Spectrum_Out%N_Sample) = &
                   Spectrum_In%Real_Part(1:Spectrum_Out%N_Sample)
     else if( Spectrum_Out%Type == 'I'  ) then
        Spectrum_Out%Imag_Part(1:Spectrum_Out%N_Sample) = &
                   Spectrum_In%Imag_Part(1:Spectrum_Out%N_Sample)
     else if( Spectrum_Out%Type == 'M'  ) then
        Spectrum_Out%Modulus(1:Spectrum_Out%N_Sample) =   &
                   Spectrum_In%Modulus(1:Spectrum_Out%N_Sample)
     else if( Spectrum_Out%Type == 'A'  ) then
        Spectrum_Out%Argument(1:Spectrum_Out%N_Sample) =  &
                   Spectrum_In%Argument(1:Spectrum_Out%N_Sample)
     else
        write(*,'(a,a)') 'Spectrum_Out%Type Error :',Spectrum_Out%Type
        write(*,*) 'spectrum_transfert Fatal Error'
        call exit(1)
     end if
     return
   end subroutine spectrum_transfert
!
!
!>  writespectrum_netcdf -- Public
!!
!! * Purpose
!!
!!     Write spectrum type
!!
!! * Description
!!
!!     Write spectrum type
!!
!! * Inputs
!!
!!     - spectrum  : type_spectrum / type for declaration and allocation of spectrum parameters
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
   subroutine writespectrum_netcdf( Spectrum,iostatus,init_file )

     implicit none

     type(type_Spectrum), intent(in) :: Spectrum
     integer(kind=LONG), intent(out) :: iostatus
     integer(kind=SHORT), intent(inout) :: init_file

     integer(kind=LONG) :: ncid, status, i_band

     iostatus = 0

     ! file creation and definition
     if (init_file == 0) then
        call init_spectrum_data(Spectrum, ncid)
        init_file = 1
     endif

     ! open netCDF dataset
     status = nf90_open(path = trim(Spectrum%TopLevelSp%header%filename), mode = nf90_write, ncid = ncid)
     if (status /= nf90_noerr) call handle_err(status)

     ! writes variables for spectrum group
     ! current spectral band
     i_band = Spectrum%TopLevelSp%Band
     call write_spectrum_data(ncid, i_band, Spectrum)

     ! close access to netCDF dataset
     status = nf90_close(ncid)
     if (status /= nf90_noerr) call handle_err(status)

     return

   end subroutine writespectrum_netcdf

   subroutine init_spectrum_data(Spectrum, ncid)

     implicit none

     type(type_Spectrum), intent(in) :: Spectrum
     integer(kind=LONG), intent(out) :: ncid

     integer(kind=LONG) :: status
     integer(kind=LONG) :: typeid
     integer(kind=LONG) :: varid, dimid

     ! create netCDF dataset
     status = nf90_create(path = trim(Spectrum%TopLevelSp%header%filename), cmode = NF90_NETCDF4, ncid = ncid)
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
     status = nf90_def_dim(ncid, "NcolFov", Spectrum%TopLevelSp%NcolFov, dimid)
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_def_var(ncid, "ColFov", NF90_INT, (/dimid/), varid)
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_att(ncid, varid, "units", "N.A.")
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_att(ncid, varid, "long_name", "column Fov value")
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_var(ncid, varid, Spectrum%TopLevelSp%ColFov)
     if (status /= nf90_noerr) call handle_err(status)
     ! LinFov(NlinFov)
     status = nf90_def_dim(ncid, "NlinFov", Spectrum%TopLevelSp%NlinFov, dimid)
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_def_var(ncid, "LinFov", NF90_INT, (/dimid/), varid)
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_att(ncid, varid, "units", "N.A.")
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_att(ncid, varid, "long_name", "line Fov value")
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_var(ncid, varid, Spectrum%TopLevelSp%LinFov)
     if (status /= nf90_noerr) call handle_err(status)

     ! define global attributes
     ! filename
     status = nf90_put_att(ncid, NF90_GLOBAL, name = "filename", values = trim(Spectrum%TopLevelSp%header%filename))
     if (status /= nf90_noerr) call handle_err(status)
     ! instrument name
     status = nf90_put_att(ncid, NF90_GLOBAL, name = "InsName", values = Spectrum%TopLevelSp%header%InsName)
     if (status /= nf90_noerr) call handle_err(status)
     ! instrument mode
     status = nf90_put_att(ncid, NF90_GLOBAL, name = "InsMode", values = Spectrum%TopLevelSp%header%InsMode)
     if (status /= nf90_noerr) call handle_err(status)
     ! S/W version
     status = nf90_put_att(ncid, NF90_GLOBAL, name = "SoftWareVersion", values = Spectrum%TopLevelSp%header%SoftWareVersion)
     if (status /= nf90_noerr) call handle_err(status)
     ! Operator
     status = nf90_put_att(ncid, NF90_GLOBAL, name = "Operator", values = Spectrum%TopLevelSp%header%Operator)
     if (status /= nf90_noerr) call handle_err(status)
     ! Processing step
     status = nf90_put_att(ncid, NF90_GLOBAL, name = "ProcessingStep", values = Spectrum%TopLevelSp%header%ProcessingStep)
     if (status /= nf90_noerr) call handle_err(status)
     ! Conf ID
     status = nf90_put_att(ncid, NF90_GLOBAL, name = "ConfId", values = trim(Spectrum%TopLevelSp%header%ConfId))
     if (status /= nf90_noerr) call handle_err(status)
     ! Type
     status = nf90_put_att(ncid, NF90_GLOBAL, name = "Type", values = Spectrum%Type)
     if (status /= nf90_noerr) call handle_err(status)
     ! Target
     status = nf90_put_att(ncid, NF90_GLOBAL, name = "Target", values = Spectrum%TopLevelSp%Target)
     if (status /= nf90_noerr) call handle_err(status)

     ! close access to netCDF dataset
     status = nf90_close(ncid)
     if (status /= nf90_noerr) call handle_err(status)

     return

   end subroutine init_spectrum_data

   subroutine write_spectrum_data(ncid, band, Spectrum)

     implicit none

     integer(kind=LONG), intent(in)               :: ncid, band
     type(type_Spectrum), intent(in)              :: Spectrum

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
     nsamp = Spectrum%N_Sample

     ! start index and count_index
     ! The values to be written are associated with the netCDF variable by assuming
     ! that the first dimension of the netCDF variable varies fastest in the Fortran 90 interface.
     ! Nline,NFor,NlinFov,NcolFov,NsubFov,N_Sample
     start_w = (/1,1,1,1,Spectrum%TopLevelSp%SubFov,1/)
     count_w = (/1,1,1,1,1,nsamp/)
     ! C version
     ! The values to be written are associated with the netCDF variable by assuming
     ! that the last dimension of the netCDF variable varies fastest in the C interface.
     ! N_Sample,NsubFov,NcolFov,NlinFov,NFor,Nline
     start_w_c = (/0,Spectrum%TopLevelSp%SubFov - 1,0,0,0,0/)
     count_w_c = (/nsamp,1,1,1,1,1/)

     ! definition of the group (one group for one spectral band)
     write(band_str,'(i1)') band

     ! check the group existence
     flag_grp = nf90_inq_ncid(ncid, 'spectrum_sb'//band_str, grpid)

     if (flag_grp /= nf90_noerr) then
        ! group creation and initialization
        status = nf90_def_grp(ncid, 'spectrum_sb'//band_str, grpid)
        if (status /= nf90_noerr) call handle_err(status)

        ! definition of the dimensions
        ! N_Sample
        status = nf90_def_dim(grpid, "N_Sample", nsamp, nsp_dimid)
        if (status /= nf90_noerr) call handle_err(status)
        ! NsubFov
        status = nf90_def_dim(grpid, "NsubFov", Spectrum%TopLevelSp%NsubFov, nsf_dimid)
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
        ! Wn
        call write_def_variable(grpid, "Wn", NF90_DOUBLE, (/nsp_dimid/), "m-1", "wavenumber base")
        if ( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'R'  ) then
           ! Real part
        call write_def_variable(grpid, "Real_Part", NF90_DOUBLE, &
                 (/dimids(4),dimids(3),dimids(2),dimids(1),nsf_dimid,nsp_dimid/), "W/(m2.sr.m-1)", "spectrum real part")
        else if ( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'I'  ) then
           ! Imaginary part
        call write_def_variable(grpid, "Imag_Part", NF90_DOUBLE, &
                 (/dimids(4),dimids(3),dimids(2),dimids(1),nsf_dimid,nsp_dimid/), "W/(m2.sr.m-1)", "spectrum imaginary part")
        else if ( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'M'  ) then
           ! Modulus
           call write_def_variable(grpid, "Modulus", NF90_DOUBLE, &
                 (/dimids(4),dimids(3),dimids(2),dimids(1),nsf_dimid,nsp_dimid/), "N.A.", "spectrum modulus part")
        else if ( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'A'  ) then
           ! Argument
           call write_def_variable(grpid, "Argument", NF90_DOUBLE, &
                 (/dimids(4),dimids(3),dimids(2),dimids(1),nsf_dimid,nsp_dimid/), "rad", "spectrum argument part")
        else if ( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'C'  ) then
           ! Complex
           call write_def_variable(grpid, "Complex", typeid(1), &
                 (/dimids(4),dimids(3),dimids(2),dimids(1),nsf_dimid,nsp_dimid/), "W/(m2.sr.m-1)", "complex spectrum")
        else if ( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'RI'  ) then
           ! RI
           call write_def_variable(grpid, "Real_Part", NF90_DOUBLE, &
                 (/dimids(4),dimids(3),dimids(2),dimids(1),nsf_dimid,nsp_dimid/), "W/(m2.sr.m-1)", "spectrum real part")
           call write_def_variable(grpid, "Imag_Part", NF90_DOUBLE, &
                 (/dimids(4),dimids(3),dimids(2),dimids(1),nsf_dimid,nsp_dimid/), "W/(m2.sr.m-1)", "spectrum imaginary part")
        else if ( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'MA'  ) then
           ! MA
           call write_def_variable(grpid, "Modulus", NF90_DOUBLE, &
                 (/dimids(4),dimids(3),dimids(2),dimids(1),nsf_dimid,nsp_dimid/), "N.A.", "spectrum modulus part")
           call write_def_variable(grpid, "Argument", NF90_DOUBLE, &
                 (/dimids(4),dimids(3),dimids(2),dimids(1),nsf_dimid,nsp_dimid/), "rad", "spectrum argument part")
        end if

        ! write group attributes
        ! Band
        status = nf90_put_att(grpid, NF90_GLOBAL, name= "Band", values = band)
        if (status /= nf90_noerr) call handle_err(status)
        ! date
        status = nf90_put_att(grpid, NF90_GLOBAL, name = "Date", values = Spectrum%Date)
        if (status /= nf90_noerr) call handle_err(status)
        ! latitude
        status = nf90_put_att(grpid, NF90_GLOBAL, name = "Lat", values = Spectrum%Lat)
        if (status /= nf90_noerr) call handle_err(status)
        ! longitude
        status = nf90_put_att(grpid, NF90_GLOBAL, name = "Lon", values = Spectrum%Lon)
        if (status /= nf90_noerr) call handle_err(status)
        ! Sat Azimuth
        status = nf90_put_att(grpid, NF90_GLOBAL, name = "Sat_Az", values = Spectrum%Sat_Az)
        if (status /= nf90_noerr) call handle_err(status)
        ! Sat Elevation
        status = nf90_put_att(grpid, NF90_GLOBAL, name = "Sat_El", values = Spectrum%Sat_El)
        if (status /= nf90_noerr) call handle_err(status)
        ! Sun Azimuth
        status = nf90_put_att(grpid, NF90_GLOBAL, name = "Sun_Az", values = Spectrum%Sun_Az)
        if (status /= nf90_noerr) call handle_err(status)
        ! Sun Elevation
        status = nf90_put_att(grpid, NF90_GLOBAL, name = "Sun_El", values = Spectrum%Sun_El)
        if (status /= nf90_noerr) call handle_err(status)
        ! Ns_First
        status = nf90_put_att(grpid, NF90_GLOBAL, name = "Ns_First", values = Spectrum%Ns_First)
        if (status /= nf90_noerr) call handle_err(status)
        ! Ns_Last
        status = nf90_put_att(grpid, NF90_GLOBAL, name = "Ns_Last", values = Spectrum%Ns_Last)
        if (status /= nf90_noerr) call handle_err(status)
        ! Wn_First
        status = nf90_put_att(grpid, NF90_GLOBAL, name = "Wn_First", values = Spectrum%Wn_First)
        if (status /= nf90_noerr) call handle_err(status)
        ! Wn_Last
        status = nf90_put_att(grpid, NF90_GLOBAL, name = "Wn_Last", values = Spectrum%Wn_Last)
        if (status /= nf90_noerr) call handle_err(status)
        ! WnMax
        status = nf90_put_att(grpid, NF90_GLOBAL, name = "WnMax", values = Spectrum%WnMax)
        if (status /= nf90_noerr) call handle_err(status)
        ! dWn
        status = nf90_put_att(grpid, NF90_GLOBAL, name = "dWn", values = Spectrum%dWn)
        if (status /= nf90_noerr) call handle_err(status)

        ! Wn
        call write_variable(grpid, "Wn", Spectrum%Wn)

   end if ! end group creation and initialization

     ! write variables
     ! SubFov (current value)
     ! Spectrum%TopLevelSp%SubFov is a scalar variable !
     status = nf90_inq_varid(grpid, "SubFov", varid)
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_var(grpid, varid, (/Spectrum%TopLevelSp%SubFov/),&
              start=(/Spectrum%TopLevelSp%SubFov/), count=(/1/))
     if (status /= nf90_noerr) call handle_err(status)

     if ( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'R'  ) then
        ! Real Part
        call write_variable(grpid, "Real_Part", Spectrum%Real_Part, start_w, count_w)
     else if ( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'I'  ) then
        ! Imaginary part
        call write_variable(grpid, "Imag_Part", Spectrum%Imag_Part, start_w, count_w)
     else if ( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'M'  ) then
        ! Modulus
        call write_variable(grpid, "Modulus", Spectrum%Modulus, start_w, count_w)
     else if ( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'A'  ) then
        ! Argument
        call write_variable(grpid, "Argument", Spectrum%Argument, start_w, count_w)
     else if ( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'C'  ) then
        ! Complex
        status = nf90_inq_varid(grpid, "Complex", varid)
        if (status /= nf90_noerr) call handle_err(status)
        call write_data_c(grpid, varid, Spectrum%Complex, start_w_c, count_w_c)
     else if ( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'RI' ) then
        ! RI
        call write_variable(grpid, "Real_Part", Spectrum%Real_Part, start_w, count_w)
        call write_variable(grpid, "Imag_Part", Spectrum%Imag_Part, start_w, count_w)
     else if ( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'MA' ) then
        ! MA
        call write_variable(grpid, "Modulus", Spectrum%Modulus, start_w, count_w)
        call write_variable(grpid, "Argument", Spectrum%Argument, start_w, count_w)
     end if

     return

   end subroutine write_spectrum_data

!
!
!>  readspectrum_netcdf -- Public
!!
!! * Purpose
!!
!!     Read spectrum type
!!
!! * Description
!!
!!     Read spectrum type
!!
!! * Inputs
!!
!!     - spectrum  : type_spectrum / type for declaration and allocation of spectrum parameters
!!
!! * Outputs
!!
!!     - spectrum  : type_spectrum / type for declaration and allocation of spectrum parameters
!!     - iostatus : integer / error management variable
!!
!! * References
!!
!!   SPS
!!
   subroutine readspectrum_netcdf( Spectrum,iostatus)

     implicit none

     type(type_Spectrum), intent(inout) :: Spectrum
     integer(kind=LONG), intent(out) :: iostatus

     integer(kind=LONG) :: ncid, status, i_band

     iostatus = 0

     ! open existing netCDF dataset
     status = nf90_open(path = trim(Spectrum%TopLevelSp%header%filename), mode = nf90_nowrite, ncid = ncid)
     if (status /= nf90_noerr) call handle_err(status)

     ! get global data (variables and attributes)
     call get_spectrum_data(ncid, Spectrum)

     ! reads variables for spectrum group
     ! current spectral band
     i_band = Spectrum%TopLevelSp%Band
     call read_spectrum_data(ncid, i_band, Spectrum)

     ! close access to netCDF dataset
     status = nf90_close(ncid)
     if (status /= nf90_noerr) call handle_err(status)

     return

   end subroutine readspectrum_netcdf


   subroutine read_spectrum_data(ncid, band, Spectrum)

     implicit none

     integer(kind=LONG), intent(in)  :: ncid, band
     type(type_Spectrum), intent(inout) :: Spectrum

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
     status = nf90_inq_ncid(ncid, 'spectrum_sb'//band_str, grpid)
     if (status /= nf90_noerr) call handle_err(status)

     ! get information about the group dataset
     status = nf90_inquire(grpid, nDimensions=ndimGrp, nVariables=nvarGrp, nAttributes=nattGrp)
     if (status /= nf90_noerr) call handle_err(status)

     ! get variable ids from group dataset
     allocate(varidsGrp(nvarGrp), stat=ErrCode)
     status = nf90_inq_varids(grpid, nvar, varidsGrp)
     if (status /= nf90_noerr) call handle_err(status)

     ! read values for group attributes
     ! date
     status = nf90_get_att(grpid, NF90_GLOBAL, name="Date", values=Spectrum%Date)
     if (status /= nf90_noerr) call handle_err(status)

     ! latitude
     status = nf90_get_att(grpid, NF90_GLOBAL, name="Lat", values=Spectrum%Lat)
     if (status /= nf90_noerr) call handle_err(status)

     ! longitude
     status = nf90_get_att(grpid, NF90_GLOBAL, name="Lon", values=Spectrum%Lon)
     if (status /= nf90_noerr) call handle_err(status)

     ! Sat Azimuth
     status = nf90_get_att(grpid, NF90_GLOBAL, name="Sat_Az", values=Spectrum%Sat_Az)
     if (status /= nf90_noerr) call handle_err(status)

     ! Sat Elevation
     status = nf90_get_att(grpid, NF90_GLOBAL, name="Sat_El", values=Spectrum%Sat_El)
     if (status /= nf90_noerr) call handle_err(status)

     ! Sun Azimuth
     status = nf90_get_att(grpid, NF90_GLOBAL, name="Sun_Az", values=Spectrum%Sun_Az)
     if (status /= nf90_noerr) call handle_err(status)

     ! Sun Elevation
     status = nf90_get_att(grpid, NF90_GLOBAL, name="Sun_El", values=Spectrum%Sun_El)
     if (status /= nf90_noerr) call handle_err(status)

     ! Ns_First
     status = nf90_get_att(grpid, NF90_GLOBAL, name="Ns_First", values=Spectrum%Ns_First)
     if (status /= nf90_noerr) call handle_err(status)

     ! Ns_Last
     status = nf90_get_att(grpid, NF90_GLOBAL, name="Ns_Last", values=Spectrum%Ns_Last)
     if (status /= nf90_noerr) call handle_err(status)

     ! Wn_First
     status = nf90_get_att(grpid, NF90_GLOBAL, name="Wn_First", values=Spectrum%Wn_First)
     if (status /= nf90_noerr) call handle_err(status)

     ! Wn_Last
      status = nf90_get_att(grpid, NF90_GLOBAL, name="Wn_Last", values=Spectrum%Wn_Last)
     if (status /= nf90_noerr) call handle_err(status)

     ! WnMax
     status = nf90_get_att(grpid, NF90_GLOBAL, name="WnMax", values=Spectrum%WnMax)
     if (status /= nf90_noerr) call handle_err(status)!

     ! dWn
     status = nf90_get_att(grpid, NF90_GLOBAL, name="dWn", values=Spectrum%dWn)
     if (status /= nf90_noerr) call handle_err(status)

     ! get N_Sample
     status = nf90_inq_dimid(grpid, "N_Sample", dimid_nsample)
     if (status /= nf90_noerr) call handle_err(status)

     status = nf90_inquire_dimension(grpid, dimid_nsample, len = Spectrum%N_Sample)
     if(status /= nf90_NoErr) call handle_err(status)

     ! Spectrum allocation
     call alloc_Spectrum( Spectrum )

     ! start index and count_index
     ! The values to be written are associated with the netCDF variable by assuming
     ! that the first dimension of the netCDF variable varies fastest in the Fortran 90 interface.
     ! Nline,NFor,NlinFov,NcolFov,NsubFov,N_Sample
     start_r = (/1,1,1,1,Spectrum%TopLevelSp%SubFov,1/)
     count_r = (/1,1,1,1,1,Spectrum%N_Sample/)
     ! C version
     ! The values to be written are associated with the netCDF variable by assuming
     ! that the last dimension of the netCDF variable varies fastest in the C interface.
     ! N_Sample,NsubFov,NcolFov,NlinFov,NFor,Nline
     start_r_c = (/0,Spectrum%TopLevelSp%SubFov - 1,0,0,0,0/)
     count_r_c = (/Spectrum%N_Sample,1,1,1,1,1/)

     ! read values for group variables

     ! note: SubFov is known before reading

     ! Wn
     status = nf90_get_var(grpid, varidsGrp(2), Spectrum%Wn)
     if (status /= nf90_noerr) call handle_err(status)

     ! Type = R
     if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'R'  ) then
        ! Real part
        status = nf90_get_var(grpid, varidsGrp(3), Spectrum%Real_Part, start=start_r, count=count_r)
        if (status /= nf90_noerr) call handle_err(status)
     ! Type = I
     else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'I'  ) then
        ! Imaginary part
        status = nf90_get_var(grpid, varidsGrp(3), Spectrum%Imag_Part, start=start_r, count=count_r)
        if (status /= nf90_noerr) call handle_err(status)
     ! Type = M
     else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'M'  ) then
        ! Modulus
        status = nf90_get_var(grpid, varidsGrp(3), Spectrum%Modulus, start=start_r, count=count_r)
        if (status /= nf90_noerr) call handle_err(status)
     ! Type = A
     else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'A'  ) then
        ! Argument
        status = nf90_get_var(grpid, varidsGrp(3), Spectrum%Argument, start=start_r, count=count_r)
        if (status /= nf90_noerr) call handle_err(status)
     ! Type = RI
     else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'RI' ) then
        ! Real part
        status = nf90_get_var(grpid, varidsGrp(3), Spectrum%Real_Part, start=start_r, count=count_r)
        if (status /= nf90_noerr) call handle_err(status)
        ! Imaginary part
        status = nf90_get_var(grpid, varidsGrp(4), Spectrum%Imag_Part, start=start_r, count=count_r)
        if (status /= nf90_noerr) call handle_err(status)
     ! Type = MA
     else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'MA' ) then
        ! Modulus
        status = nf90_get_var(grpid, varidsGrp(3), Spectrum%Modulus, start=start_r, count=count_r)
        if (status /= nf90_noerr) call handle_err(status)
        ! Argument
        status = nf90_get_var(grpid, varidsGrp(4), Spectrum%Argument, start=start_r, count=count_r)
        if (status /= nf90_noerr) call handle_err(status)
     ! Type = Complex
     else if( Spectrum%Type(1:len_trim(Spectrum%Type))      == 'C'  ) then
        call read_data_c(grpid, varidsGrp(3), Spectrum%Complex, start_r_c, count_r_c)
     end if

     ! deallocate local variables
     deallocate(varidsGrp, stat=ErrCode)

     return

   end subroutine read_spectrum_data

   subroutine get_spectrum_data(ncid, Spectrum)

     implicit none

     integer(kind=LONG), intent(in)  :: ncid
     type(type_Spectrum), intent(inout) :: Spectrum

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
     status = nf90_inquire_dimension(ncid, dimid, len = Spectrum%TopLevelSp%Nline)
     if(status /= nf90_NoErr) call handle_err(status)

     ! get NFor
     status = nf90_inq_dimid(ncid, "NFor", dimid)
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_inquire_dimension(ncid, dimid, len = Spectrum%TopLevelSp%NFor)
     if(status /= nf90_NoErr) call handle_err(status)

     ! note: NcolFov is known before reading

     ! note: NlinFov is known before reading

     ! read values for global variables
     ! Line
     status = nf90_get_var(ncid, varidsRoot(1), Spectrum%TopLevelSp%Line)
     if (status /= nf90_noerr) call handle_err(status)

     ! For
     status = nf90_get_var(ncid, varidsRoot(2), Spectrum%TopLevelSp%For)
     if (status /= nf90_noerr) call handle_err(status)

     ! note: ColFov is known before reading

     ! note: LinFov is known before reading

     ! read values for global attributes
     ! instrument name
     status = nf90_get_att(ncid, NF90_GLOBAL, name="InsName", values=Spectrum%TopLevelSp%header%InsName)
     if (status /= nf90_noerr) call handle_err(status)

     ! instrument mode
     status = nf90_get_att(ncid, NF90_GLOBAL, name="InsMode", values=Spectrum%TopLevelSp%header%InsMode)
     if (status /= nf90_noerr) call handle_err(status)

     ! S/W version
     status = nf90_get_att(ncid, NF90_GLOBAL, name="SoftWareVersion", values=Spectrum%TopLevelSp%header%SoftWareVersion)
     if (status /= nf90_noerr) call handle_err(status)

     ! Operator
     status = nf90_get_att(ncid, NF90_GLOBAL, name="Operator", values=Spectrum%TopLevelSp%header%Operator)
     if (status /= nf90_noerr) call handle_err(status)

     ! Processing step
     status = nf90_get_att(ncid, NF90_GLOBAL, name="ProcessingStep", values=Spectrum%TopLevelSp%header%ProcessingStep)
     if (status /= nf90_noerr) call handle_err(status)

     ! Conf ID
     status = nf90_get_att(ncid, NF90_GLOBAL, name="ConfId", values=Spectrum%TopLevelSp%header%ConfId)
     if (status /= nf90_noerr) call handle_err(status)

     ! Type
     status = nf90_get_att(ncid, NF90_GLOBAL, name="Type", values=Spectrum%Type)
     if (status /= nf90_noerr) call handle_err(status)

     ! Target
     status = nf90_get_att(ncid, NF90_GLOBAL, name="Target", values=Spectrum%TopLevelSp%Target)
     if (status /= nf90_noerr) call handle_err(status)

     ! deallocate local variables
     deallocate(varidsRoot, stat=ErrCode)

     return

   end subroutine get_spectrum_data
!
!
end module spectrum_type
