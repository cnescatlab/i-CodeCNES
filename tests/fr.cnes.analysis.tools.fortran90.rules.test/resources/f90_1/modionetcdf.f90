!!* modionetcdf.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS
!!*              Date: July 2013
!!*           Version: $ $
!!* Last modification: $ $
!!
!> modionetcdf -- Module
!!
!! * Purpose
!!
!!     netCDF files management.
!!
!! * Description
!!
!!     This module provides netCDF I/O functions.
!!
!! * Functions
!!
!!     * write_def_variable : define netcdf variable
!!     * write_variable     : write netcdf variable
!!     * handle_err         : handles errors
!!     * write_data_c       : write compound type data
!!     * read_data_c        : read compound type data
!!
!! * References
!!
module modionetcdf
   use netcdf
   use precision_type
!
   implicit none
!
   public :: handle_err
!
      external write_data_c !$pragma C( write_data_c )
      external read_data_c !$pragma C( read_data_c )
!        !$pragma C( write_data_c )
!        !$pragma C( read_data_c )

   interface write_variable

      module procedure write_variable_dim1, write_variable_dim2, write_variable_dim3

   end interface

   contains

   subroutine write_def_variable(ncid, name, type, dimid, units, lname)

     implicit none

     integer(kind=LONG), intent(in)               :: ncid, type
     integer(kind=LONG), dimension(:), intent(in) :: dimid
     character(len=*), intent(in)                 :: name
     character(len=*), intent(in)                 :: units, lname

     integer(kind=LONG) :: varid, status

     status = nf90_def_var(ncid, name, type, dimid, varid)
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_att(ncid, varid, name= "units", values = trim(units))
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_att(ncid, varid, name= "long_name", values = trim(lname))
     if (status /= nf90_noerr) call handle_err(status)

     return

   end subroutine write_def_variable

   subroutine write_variable_dim1(ncid, name, data, start_w, count_w)

      implicit none

      integer(kind=LONG), intent(in)  :: ncid
      character(len=*), intent(in)    :: name
      real(kind=DOUBLE), dimension(:), intent(in) :: data
      integer(kind=LONG), dimension(:), intent(in), optional :: start_w, count_w

      integer(kind=LONG) :: status, varid

     status = nf90_inq_varid(ncid, name, varid)
     if (status /= nf90_noerr) call handle_err(status)
     if (present(start_w) .and. present(count_w)) then
        status = nf90_put_var(ncid, varid, data, start=start_w, count=count_w)
        if (status /= nf90_noerr) call handle_err(status)
     else
        status = nf90_put_var(ncid, varid, data)
        if (status /= nf90_noerr) call handle_err(status)
     end if

     return

   end subroutine write_variable_dim1

   subroutine write_variable_dim2(ncid, name, data, start_w, count_w)

      implicit none

      integer(kind=LONG), intent(in)  :: ncid
      character(len=*), intent(in)    :: name
      real(kind=DOUBLE), dimension(:,:), intent(in) :: data
      integer(kind=LONG), dimension(:), intent(in), optional :: start_w, count_w

      integer(kind=LONG) :: status, varid

     status = nf90_inq_varid(ncid, name, varid)
     if (status /= nf90_noerr) call handle_err(status)
     if (present(start_w) .and. present(count_w)) then
        status = nf90_put_var(ncid, varid, data, start=start_w, count=count_w)
        if (status /= nf90_noerr) call handle_err(status)
     else
        status = nf90_put_var(ncid, varid, data)
        if (status /= nf90_noerr) call handle_err(status)
     end if

     return

   end subroutine write_variable_dim2

   subroutine write_variable_dim3(ncid, name, data, start_w, count_w)

      implicit none

      integer(kind=LONG), intent(in)  :: ncid
      character(len=*), intent(in)    :: name
      real(kind=DOUBLE), dimension(:,:,:), intent(in) :: data
      integer(kind=LONG), dimension(:), intent(in), optional :: start_w, count_w

      integer(kind=LONG) :: status, varid

     status = nf90_inq_varid(ncid, name, varid)
     if (status /= nf90_noerr) call handle_err(status)
     if (present(start_w) .and. present(count_w)) then
        status = nf90_put_var(ncid, varid, data, start=start_w, count=count_w)
        if (status /= nf90_noerr) call handle_err(status)
     else
        status = nf90_put_var(ncid, varid, data)
        if (status /= nf90_noerr) call handle_err(status)
     end if

     return

   end subroutine write_variable_dim3

   subroutine handle_err(status)

     implicit none

     integer(kind=LONG), intent (in) :: status

     if(status /= nf90_noerr) then
       write(0 ,*) trim(nf90_strerror(status))
       call exit(1)
     end if

   end subroutine handle_err

end module modionetcdf
