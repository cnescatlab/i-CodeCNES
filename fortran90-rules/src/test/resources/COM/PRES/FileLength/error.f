! $Id: $
module ncdf
!****m* Interface/Modules
!
! SYNOPSIS
!    use ncdf
! 
! DESCRIPTION
!    Access to the routines in the ncdf library is provided by various
!    Fortran 90 modules. 
!
! SEE ALSO
!    ncdf
!
! AUTHOR
!    C. Marquardt, Darmstadt, Germany              <christian@marquardt.sc>
!
! COPYRIGHT
!
!    Copyright (c) 2005 Christian Marquardt        <christian@marquardt.sc>
!
!    All rights reserved.
!
!    Permission is hereby granted, free of charge, to any person obtaining
!    a copy of this software and associated documentation files (the
!    "Software"), to deal in the Software without restriction, including
!    without limitation the rights to use, copy, modify, merge, publish,
!    distribute, sublicense, and/or sell copies of the Software, and to
!    permit persons to whom the Software is furnished to do so, subject to
!    the following conditions:
!
!    The above copyright notice and this permission notice shall be
!    included in all copies or substantial portions of the Software as well
!    as in supporting documentation.
!
!    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
!    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
!    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
!    NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
!    LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
!    OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
!    WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
!
!****
!****m* Modules/ncdf
!
! NAME
!    ncdf - A simple interface to read and write netCDF data.
!
! SYNOPSIS
!    use ncdf
! 
! DESCRIPTION
!    This module provides a simple interface to the netCDF library and
!    allows for easy creation, writing and reading of netCDF data files.
!
! NOTES
!    For reading data from netCDF files, the ncdf_open, read_variable
!    and ncdf_close routines should be sufficient. For writing netCDF
!    data files, it is probably neecessary to write a 'ncf_create_mystuff'
!    subroutine that handles the creation of the netCDF file using both
!    ncdf and native netCDF calls; ncdf currently only provides little
!    for this task.
!
! EXAMPLE
!    Assume that you have written a subroutine ncdf_create_alpha which
!    creates a netCDF data file for a scalar 'R_curve' and array variables
!    namd 'lon', 'lat' and 'alpha_o' (along with several others). From a
!    Fortran 90 program, ncdf routines might then be called as follows:
!
!       use ncdf
!         ...
!       integer, parameter   :: n_levels = <some_number>
!         ...
!       real(dp)             :: alpha(n_levels), impact(n_levels)
!         ...
!       call ncdf_create_alpha('test.nc', n_levels)
!       call ncdf_putvar('alpha_b', alpha)
!       call ncdf_putvar('impact',  impact)
!         ...
!       call ncdf_close()
!
!    To read these variables from another Fortran 90 program, try this:
!
!       use ncdf
!         ...
!       integer, parameter   :: n_levels = <some_number>
!         ...
!       real(dp)             :: alpha(n_levels), impact(n_levels)
!         ...
!       call ncdf_open('test.nc')
!       call ncdf_getvar('alpha_b', alpha)
!       call ncdf_getvar('impact',  impact)
!         ...
!       call ncdf_close()
!
! SEE ALSO
!    High level routines:
!
!      ncdf_open         - Open an (already existing) netCDF data file.
!      ncdf_close        - Close a netCDF data file.
!      ncdf_putvar       - Write data into an (already defined) variable of
!                            the current netCDF data file..
!      ncdf_getvar       - Read data from a variable in the current netCDF
!                            data file.
!
!    Low level routines (mainly used for the creation of new netCDF data files)
!
!      ncdf_create       - Create a new netCDF data file.
!      ncdf_defvar       - Create a new variable in a netCDF data file.
!
! AUTHOR
!    C. Marquardt, Darmstadt, Germany              <christian@marquardt.sc>
!
!****
!------------------------------------------------------------------------------
! 1. Global variables
!------------------------------------------------------------------------------
!****iv* netCDF/ncdf_ncname
!
! NAME
!    ncdf_ncname - Name of the current netCDF data file.
!
! SYNOPSIS
!    use ncdf
!
! DESCRIPTION
!    The name (including the path) of the current netCDF data file; it is
!    known to all ncdf routines.
!
! AUTHOR
!    C. Marquardt, Darmstadt, Germany              <christian@marquardt.sc>
!
!****
!****iv* netCDF/ncdf_ncid
!
! NAME
!    ncdf_ncid - NetCDF id of the current netCDF data file.
!
! SYNOPSIS
!    use ncdf
!
! DESCRIPTION
!    The netCDF id of the current opened netCDF data file; it is known to all
!    ncdf routines.
!
! AUTHOR
!    C. Marquardt, Darmstadt, Germany              <christian@marquardt.sc>
!
!****
  use typeSizes
  use netcdf
  character(len = 1024), save, public :: ncdf_ncname = ''
  integer,               save, public :: ncdf_ncid   = -1
  integer,               save, public :: ncdf_nvars  = 0
  logical, dimension(:), pointer, save, public :: ncdf_read => null() 
  logical,               save, public :: ncdf_delete_on_error = .false.
!------------------------------------------------------------------------------
! 2. Interfaces - Files
!------------------------------------------------------------------------------
!****m* netCDF/Files
!
! DESCRIPTION
!    Routines for handling netCDF data files.
!
! SEE ALSO
!    is_netcdf
!    ncdf_create
!    ncdf_open
!    ncdf_close
!    ncdf_sync
!
! AUTHOR
!    C. Marquardt, Darmstadt, Germany              <christian@marquardt.sc>
!
!****
  interface is_netcdf
     function is_netcdf(file) result(it_is)
       character(len = *), intent(in) :: file
       logical                        :: it_is
     end function is_netcdf
  end interface
  interface ncdf_create
     subroutine ncdf_create(ncfile, cmode, ncid)
       character (len = *), intent(in) :: ncfile
       integer,             optional   :: cmode
       integer,             optional   :: ncid
     end subroutine ncdf_create
  end interface
  interface ncdf_open
     subroutine ncdf_open(ncfile, ncid, mode, append)
       character (len = *), intent(in) :: ncfile
       integer,             optional   :: ncid
       integer,             optional   :: mode
       logical,             optional   :: append
     end subroutine ncdf_open
  end interface
  interface ncdf_close
     subroutine ncdf_close(ncfile, ncid)
       character (len = *), optional :: ncfile
       integer,             optional :: ncid
     end subroutine ncdf_close
  end interface
  interface ncdf_sync
     subroutine ncdf_sync(ncfile, ncid)
       character (len = *), optional :: ncfile
       integer,             optional :: ncid
     end subroutine ncdf_sync
  end interface
!------------------------------------------------------------------------------
! 3. Interfaces - Query
!------------------------------------------------------------------------------
!****m* netCDF/Query
!
! DESCRIPTION
!    Routines for obtaining information about the contents of a netCDF data
!    file.
!
! SEE ALSO
!    ncdf_getshape
!    ncdf_getsize
!    ncdf_getnrec
!
! AUTHOR
!    C. Marquardt, Darmstadt, Germany              <christian@marquardt.sc>
!
!****
  interface ncdf_getshape
     subroutine ncdf_getshape(name, shape, ncfile, ncid)
       character(len = *),        intent(in)  :: name
       integer,                   intent(out) :: shape
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_getshape
  end interface
  interface ncdf_getsize
     subroutine ncdf_sgetsize(name, size, dim, ncfile, ncid)
       character(len = *),        intent(in)  :: name
       integer,                   intent(out) :: size
       integer,                   optional    :: dim
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_sgetsize
     subroutine ncdf_agetsize(name, size, ncfile, ncid)
       character(len = *),        intent(in)  :: name
       integer, dimension(:),     intent(out) :: size
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_agetsize
  end interface
  interface ncdf_getnrec
     function ncdf_getnrec(ncfile, ncid) result(n_unlimited)
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer                                :: n_unlimited
     end function ncdf_getnrec
  end interface
!------------------------------------------------------------------------------
! 4. Interfaces - attributes
!------------------------------------------------------------------------------
!****m* netCDF/Attributes
!
! DESCRIPTION
!    Routines to deal with attributes, both global and variable specific, in
!    a netCDF data file.
!
! SEE ALSO
!
! AUTHOR
!    C. Marquardt, Darmstadt, Germany              <christian@marquardt.sc>
!
!****
  interface ncdf_isatt
     function ncdf_isatt(attname, varname, ncfile, ncid, xtype, len, attnum) result(it_is)
       character (len = *), intent(in)            :: attname
       character (len = *), intent(in), optional  :: varname
       character (len = *), intent(in),  optional :: ncfile
       integer,             intent(in),  optional :: ncid
       integer,             intent(out), optional :: xtype, len, attnum
       logical                                    :: it_is
     end function ncdf_isatt
  end interface
  interface ncdf_putatt
     subroutine ncdf_putatt_OneByteInt (attname, value, varname, ncfile, ncid)
       use typeSizes
       character(len = *),        intent(in ) :: attname
       integer(kind = OneByteInt), &
                                  intent(in ) :: value
       character(len = *),        optional    :: varname
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_putatt_OneByteInt
     subroutine ncdf_putatt_TwoByteInt (attname, value, varname, ncfile, ncid)
       use typeSizes
       character(len = *),        intent(in ) :: attname
       integer(kind = TwoByteInt), &
                                  intent(in ) :: value
       character(len = *),        optional    :: varname
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_putatt_TwoByteInt
     subroutine ncdf_putatt_FourByteInt (attname, value, varname, ncfile, ncid)
       use typeSizes
       character(len = *),        intent(in ) :: attname
       integer(kind = FourByteInt), &
                                  intent(in ) :: value
       character(len = *),        optional    :: varname
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_putatt_FourByteInt
     subroutine ncdf_putatt_EightByteInt (attname, value, varname, ncfile, ncid)
       use typeSizes
       character(len = *),        intent(in ) :: attname
       integer(kind = EightByteInt), &
                                  intent(in ) :: value
       character(len = *),        optional    :: varname
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_putatt_EightByteInt
     subroutine ncdf_putatt_FourByteReal (attname, value, varname, ncfile, ncid)
       use typeSizes
       character(len = *),        intent(in ) :: attname
       real(kind = FourByteReal), &
                                  intent(in ) :: value
       character(len = *),        optional    :: varname
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_putatt_FourByteReal
     subroutine ncdf_putatt_EightByteReal (attname, value, varname, ncfile, ncid)
       use typeSizes
       character(len = *),        intent(in ) :: attname
       real(kind = EightByteReal), &
                                  intent(in ) :: value
       character(len = *),        optional    :: varname
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_putatt_EightByteReal
     subroutine ncdf_putatt_text (attname, value, varname, ncfile, ncid)
       use typeSizes
       character(len = *),        intent(in ) :: attname
       character (len = *), &
                                  intent(in ) :: value
       character(len = *),        optional    :: varname
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_putatt_text
     subroutine ncdf_putatt_1D_OneByteInt (attname, values, varname, ncfile, ncid)
       use typeSizes
       character(len = *),        intent(in)  :: attname
       integer(kind = OneByteInt), dimension(:), &
                                  intent(in ) :: values
       character(len = *),        optional    :: varname
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_putatt_1D_OneByteInt
     subroutine ncdf_putatt_1D_TwoByteInt (attname, values, varname, ncfile, ncid)
       use typeSizes
       character(len = *),        intent(in)  :: attname
       integer(kind = TwoByteInt), dimension(:), &
                                  intent(in ) :: values
       character(len = *),        optional    :: varname
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_putatt_1D_TwoByteInt
     subroutine ncdf_putatt_1D_FourByteInt (attname, values, varname, ncfile, ncid)
       use typeSizes
       character(len = *),        intent(in)  :: attname
       integer(kind = FourByteInt), dimension(:), &
                                  intent(in ) :: values
       character(len = *),        optional    :: varname
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_putatt_1D_FourByteInt
     subroutine ncdf_putatt_1D_EightByteInt (attname, values, varname, ncfile, ncid)
       use typeSizes
       character(len = *),        intent(in)  :: attname
       integer(kind = EightByteInt), dimension(:), &
                                  intent(in ) :: values
       character(len = *),        optional    :: varname
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_putatt_1D_EightByteInt
     subroutine ncdf_putatt_1D_FourByteReal (attname, values, varname, ncfile, ncid)
       use typeSizes
       character(len = *),        intent(in)  :: attname
       real(kind = FourByteReal), dimension(:), &
                                  intent(in ) :: values
       character(len = *),        optional    :: varname
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_putatt_1D_FourByteReal
     subroutine ncdf_putatt_1D_EightByteReal (attname, values, varname, ncfile, ncid)
       use typeSizes
       character(len = *),        intent(in)  :: attname
       real(kind = EightByteReal), dimension(:), &
                                  intent(in ) :: values
       character(len = *),        optional    :: varname
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_putatt_1D_EightByteReal
  end interface
  interface ncdf_getatt
     subroutine ncdf_getatt_OneByteInt (attname, value, varname, ncfile, ncid)
       use typeSizes
       character(len = *),        intent(in ) :: attname
       integer(kind = OneByteInt), &
                                  intent(out) :: value
       character(len = *),        optional    :: varname
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_getatt_OneByteInt
     subroutine ncdf_getatt_TwoByteInt (attname, value, varname, ncfile, ncid)
       use typeSizes
       character(len = *),        intent(in ) :: attname
       integer(kind = TwoByteInt), &
                                  intent(out) :: value
       character(len = *),        optional    :: varname
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_getatt_TwoByteInt
     subroutine ncdf_getatt_FourByteInt (attname, value, varname, ncfile, ncid)
       use typeSizes
       character(len = *),        intent(in ) :: attname
       integer(kind = FourByteInt), &
                                  intent(out) :: value
       character(len = *),        optional    :: varname
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_getatt_FourByteInt
     subroutine ncdf_getatt_EightByteInt (attname, value, varname, ncfile, ncid)
       use typeSizes
       character(len = *),        intent(in ) :: attname
       integer(kind = EightByteInt), &
                                  intent(out) :: value
       character(len = *),        optional    :: varname
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_getatt_EightByteInt
     subroutine ncdf_getatt_FourByteReal (attname, value, varname, ncfile, ncid)
       use typeSizes
       character(len = *),        intent(in ) :: attname
       real(kind = FourByteReal), &
                                  intent(out) :: value
       character(len = *),        optional    :: varname
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_getatt_FourByteReal
     subroutine ncdf_getatt_EightByteReal (attname, value, varname, ncfile, ncid)
       use typeSizes
       character(len = *),        intent(in ) :: attname
       real(kind = EightByteReal), &
                                  intent(out) :: value
       character(len = *),        optional    :: varname
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_getatt_EightByteReal
     subroutine ncdf_getatt_text (attname, value, varname, ncfile, ncid)
       use typeSizes
       character(len = *),        intent(in ) :: attname
       character (len = *), &
                                  intent(out) :: value
       character(len = *),        optional    :: varname
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_getatt_text
     subroutine ncdf_getatt_1D_OneByteInt (attname, values, varname, ncfile, ncid)
       use typeSizes
       character(len = *),        intent(in)  :: attname
       integer(kind = OneByteInt), dimension(:), &
                                  intent(out) :: values
       character(len = *),        optional    :: varname
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_getatt_1D_OneByteInt
     subroutine ncdf_getatt_1D_TwoByteInt (attname, values, varname, ncfile, ncid)
            use typeSizes
       character(len = *),        intent(in)  :: attname
       integer(kind = TwoByteInt), dimension(:), &
                                  intent(out) :: values
       character(len = *),        optional    :: varname
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_getatt_1D_TwoByteInt
     subroutine ncdf_getatt_1D_FourByteInt (attname, values, varname, ncfile, ncid)
       use typeSizes
       character(len = *),        intent(in)  :: attname
       integer(kind = FourByteInt), dimension(:), &
                                  intent(out) :: values
       character(len = *),        optional    :: varname
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_getatt_1D_FourByteInt
     subroutine ncdf_getatt_1D_EightByteInt (attname, values, varname, ncfile, ncid)
       use typeSizes
       character(len = *),        intent(in)  :: attname
       integer(kind = EightByteInt), dimension(:), &
                                  intent(out) :: values
       character(len = *),        optional    :: varname
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_getatt_1D_EightByteInt
     subroutine ncdf_getatt_1D_FourByteReal (attname, values, varname, ncfile, ncid)
       use typeSizes
       character(len = *),        intent(in)  :: attname
       real(kind = FourByteReal), dimension(:), &
                                  intent(out) :: values
       character(len = *),        optional    :: varname
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_getatt_1D_FourByteReal
     subroutine ncdf_getatt_1D_EightByteReal (attname, values, varname, ncfile, ncid)
       use typeSizes
       character(len = *),        intent(in)  :: attname
       real(kind = EightByteReal), dimension(:), &
                                  intent(out) :: values
       character(len = *),        optional    :: varname
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_getatt_1D_EightByteReal
  end interface
  interface ncdf_getatt_alloc
     subroutine ncdf_getatl_1D_OneByteInt (attname, values, varname, ncfile, ncid)
       use typeSizes
       character(len = *),        intent(in)  :: attname
       integer(kind = OneByteInt), dimension(:), &
                                  pointer :: values
       character(len = *),        optional    :: varname
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_getatl_1D_OneByteInt
     subroutine ncdf_getatl_1D_TwoByteInt (attname, values, varname, ncfile, ncid)
       use typeSizes
       character(len = *),        intent(in)  :: attname
       integer(kind = TwoByteInt), dimension(:), &
                                  pointer :: values
       character(len = *),        optional    :: varname
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_getatl_1D_TwoByteInt
     subroutine ncdf_getatl_1D_FourByteInt (attname, values, varname, ncfile, ncid)
       use typeSizes
       character(len = *),        intent(in)  :: attname
       integer(kind = FourByteInt), dimension(:), &
                                  pointer :: values
       character(len = *),        optional    :: varname
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_getatl_1D_FourByteInt
     subroutine ncdf_getatl_1D_EightByteInt (attname, values, varname, ncfile, ncid)
       use typeSizes
       character(len = *),        intent(in)  :: attname
       integer(kind = EightByteInt), dimension(:), &
                                  pointer :: values
       character(len = *),        optional    :: varname
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_getatl_1D_EightByteInt
     subroutine ncdf_getatl_1D_FourByteReal (attname, values, varname, ncfile, ncid)
       use typeSizes
       character(len = *),        intent(in)  :: attname
       real(kind = FourByteReal), dimension(:), &
                                  pointer :: values
       character(len = *),        optional    :: varname
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_getatl_1D_FourByteReal
     subroutine ncdf_getatl_1D_EightByteReal (attname, values, varname, ncfile, ncid)
       use typeSizes
       character(len = *),        intent(in)  :: attname
       real(kind = EightByteReal), dimension(:), &
                                  pointer :: values
       character(len = *),        optional    :: varname
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_getatl_1D_EightByteReal
  end interface
!------------------------------------------------------------------------------
! 5. Interfaces - dimensions
!------------------------------------------------------------------------------
!****m* netCDF/Dimensions
!
! DESCRIPTION
!    Routines to deal with dimensions within a netCDF data file, including
!    defining them.
!
! SEE ALSO
!    ncdf_defdim
!
! AUTHOR
!    C. Marquardt, Darmstadt, Germany              <christian@marquardt.sc>
!
!****
  interface ncdf_defdim
     function ncdf_defdim(name, n, ncid) result(dimid)
        character(len = *),    intent(in) :: name
        integer,               intent(in) :: n
        integer,               optional   :: ncid
        integer                           :: dimid
     end function ncdf_defdim
  end interface
!------------------------------------------------------------------------------
! 6. Interfaces - variables
!------------------------------------------------------------------------------
!****m* netCDF/Variables
!
! DESCRIPTION
!    Routines to deal with variables within a netCDF data file, including
!    defining, writing and reading them.
!
! SEE ALSO
!    ncdf_isvar
!    ncdf_renvar
!    ncdf_defvar
!
! AUTHOR
!    C. Marquardt, Darmstadt, Germany              <christian@marquardt.sc>
!
!****
  interface ncdf_isvar
     function ncdf_isvar(name, ncfile, ncid) result(it_is)
       character(len = *), intent(in) :: name
       character(len = *), optional   :: ncfile
       integer,            optional   :: ncid
       logical                        :: it_is
     end function ncdf_isvar
  end interface
  interface ncdf_renvar
     subroutine ncdf_renvar(old_name, new_name, ncfile, ncid)
       character(len = *),        intent(in)  :: old_name
       character(len = *),        intent(in)  :: new_name
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_renvar
  end interface
  interface ncdf_defvar
     function ncdf_defvar_sca(name, long_name, units, type, ncid, &
                              standard_name, positive, formula_terms, &
                              calendar, coordinates) &
                              result(varid)
       character(len = *),    intent(in) :: name, long_name, units
       integer,               optional   :: type
       integer,               optional   :: ncid
       character(len = *),    optional   :: standard_name
       character(len = *),    optional   :: positive
       character(len = *),    optional   :: formula_terms
       character(len = *),    optional   :: calendar
       character(len = *),    optional   :: coordinates
       integer                           :: varid
     end function ncdf_defvar_sca
     function ncdf_defvar_arr(name, long_name, units, dimids, type, ncid, &
                              standard_name, positive, formula_terms, &
                              calendar, coordinates) &
                              result(varid)
       character(len = *),    intent(in) :: name, long_name, units
       integer, dimension(:), intent(in) :: dimids
       integer,               optional   :: type
       integer,               optional   :: ncid
       character(len = *),    optional   :: standard_name
       character(len = *),    optional   :: positive
       character(len = *),    optional   :: formula_terms
       character(len = *),    optional   :: calendar
       character(len = *),    optional   :: coordinates
       integer                           :: varid
     end function ncdf_defvar_arr
  end interface
  interface ncdf_putvar
     subroutine ncdf_putvar_OneByteInt (name, values, ncfile, ncid, rec, start, units, range)
       use typeSizes
       character(len = *),        intent( in) :: name
       integer(kind = OneByteInt), &
                                  intent(in ) :: values
       integer(kind = OneByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_OneByteInt
     subroutine ncdf_putvar_TwoByteInt (name, values, ncfile, ncid, rec, start, units, range)
       use typeSizes
       character(len = *),        intent( in) :: name
       integer(kind = TwoByteInt), &
                                  intent(in ) :: values
       integer(kind = TwoByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_TwoByteInt
     subroutine ncdf_putvar_FourByteInt (name, values, ncfile, ncid, rec, start, units, range)
       use typeSizes
       character(len = *),        intent( in) :: name
       integer(kind = FourByteInt), &
                                  intent(in ) :: values
       integer(kind = FourByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_FourByteInt
     subroutine ncdf_putvar_EightByteInt (name, values, ncfile, ncid, rec, start, units, range)
       use typeSizes
       character(len = *),        intent( in) :: name
       integer(kind = EightByteInt), &
                                  intent(in ) :: values
       integer(kind = EightByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_EightByteInt
     subroutine ncdf_putvar_FourByteReal (name, values, ncfile, ncid, rec, start, units, range)
       use typeSizes
       character(len = *),        intent( in) :: name
       real(kind = FourByteReal), &
                                  intent(in ) :: values
       real(kind = FourByteReal),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_FourByteReal
     subroutine ncdf_putvar_EightByteReal (name, values, ncfile, ncid, rec, start, units, range)
       use typeSizes
       character(len = *),        intent( in) :: name
       real(kind = EightByteReal), &
                                  intent(in ) :: values
       real(kind = EightByteReal),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_EightByteReal
     subroutine ncdf_putvar_text (name, values, ncfile, ncid, rec, start, units, range)
       use typeSizes
       character(len = *),        intent( in) :: name
       character (len = *), &
                                  intent(in ) :: values
       character (len = *),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_text
     subroutine ncdf_putvar_1D_OneByteInt (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       integer(kind = OneByteInt), dimension(:), &
                                  intent(in ) :: values
       integer(kind = OneByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_1D_OneByteInt
     subroutine ncdf_putvar_1D_TwoByteInt (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       integer(kind = TwoByteInt), dimension(:), &
                                  intent(in ) :: values
       integer(kind = TwoByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_1D_TwoByteInt
     subroutine ncdf_putvar_1D_FourByteInt (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       integer(kind = FourByteInt), dimension(:), &
                                  intent(in ) :: values
       integer(kind = FourByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_1D_FourByteInt
     subroutine ncdf_putvar_1D_EightByteInt (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       integer(kind = EightByteInt), dimension(:), &
                                  intent(in ) :: values
       integer(kind = EightByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_1D_EightByteInt
     subroutine ncdf_putvar_1D_FourByteReal (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       real(kind = FourByteReal), dimension(:), &
                                  intent(in ) :: values
       real(kind = FourByteReal),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_1D_FourByteReal
     subroutine ncdf_putvar_1D_EightByteReal (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       real(kind = EightByteReal), dimension(:), &
                                  intent(in ) :: values
       real(kind = EightByteReal),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_1D_EightByteReal
     subroutine ncdf_putvar_2D_OneByteInt (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       integer(kind = OneByteInt), dimension(:, :), &
                                  intent(in ) :: values
       integer(kind = OneByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_2D_OneByteInt
     subroutine ncdf_putvar_2D_TwoByteInt (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       integer(kind = TwoByteInt), dimension(:, :), &
                                  intent(in ) :: values
       integer(kind = TwoByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_2D_TwoByteInt
     subroutine ncdf_putvar_2D_FourByteInt (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       integer(kind = FourByteInt), dimension(:, :), &
                                  intent(in ) :: values
       integer(kind = FourByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_2D_FourByteInt
     subroutine ncdf_putvar_2D_EightByteInt (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       integer(kind = EightByteInt), dimension(:, :), &
                                  intent(in ) :: values
       integer(kind = EightByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_2D_EightByteInt
     subroutine ncdf_putvar_2D_FourByteReal (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       real(kind = FourByteReal), dimension(:, :), &
                                  intent(in ) :: values
       real(kind = FourByteReal),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_2D_FourByteReal
     subroutine ncdf_putvar_2D_EightByteReal (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       real(kind = EightByteReal), dimension(:, :), &
                                  intent(in ) :: values
       real(kind = EightByteReal),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_2D_EightByteReal
     subroutine ncdf_putvar_3D_OneByteInt (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       integer(kind = OneByteInt), dimension(:, :, :), &
                                  intent(in ) :: values
       integer(kind = OneByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_3D_OneByteInt
     subroutine ncdf_putvar_3D_TwoByteInt (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       integer(kind = TwoByteInt), dimension(:, :, :), &
                                  intent(in ) :: values
       integer(kind = TwoByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_3D_TwoByteInt
     subroutine ncdf_putvar_3D_FourByteInt (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       integer(kind = FourByteInt), dimension(:, :, :), &
                                  intent(in ) :: values
       integer(kind = FourByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_3D_FourByteInt
     subroutine ncdf_putvar_3D_EightByteInt (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       integer(kind = EightByteInt), dimension(:, :, :), &
                                  intent(in ) :: values
       integer(kind = EightByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_3D_EightByteInt
     subroutine ncdf_putvar_3D_FourByteReal (name, values, ncfile, ncid, rec, start, count, units, range)
            use typeSizes
       character(len = *),        intent(in)  :: name
       real(kind = FourByteReal), dimension(:, :, :), &
                                  intent(in ) :: values
       real(kind = FourByteReal),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_3D_FourByteReal
     subroutine ncdf_putvar_3D_EightByteReal (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       real(kind = EightByteReal), dimension(:, :, :), &
                                  intent(in ) :: values
       real(kind = EightByteReal),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_3D_EightByteReal
     subroutine ncdf_putvar_4D_OneByteInt (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       integer(kind = OneByteInt), dimension(:, :, :, :), &
                                  intent(in ) :: values
       integer(kind = OneByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_4D_OneByteInt
     subroutine ncdf_putvar_4D_TwoByteInt (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       integer(kind = TwoByteInt), dimension(:, :, :, :), &
                                  intent(in ) :: values
       integer(kind = TwoByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_4D_TwoByteInt
     subroutine ncdf_putvar_4D_FourByteInt (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       integer(kind = FourByteInt), dimension(:, :, :, :), &
                                  intent(in ) :: values
       integer(kind = FourByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_4D_FourByteInt
     subroutine ncdf_putvar_4D_EightByteInt (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       integer(kind = EightByteInt), dimension(:, :, :, :), &
                                  intent(in ) :: values
       integer(kind = EightByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_4D_EightByteInt
     subroutine ncdf_putvar_4D_FourByteReal (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       real(kind = FourByteReal), dimension(:, :, :, :), &
                                  intent(in ) :: values
       real(kind = FourByteReal),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_4D_FourByteReal
     subroutine ncdf_putvar_4D_EightByteReal (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       real(kind = EightByteReal), dimension(:, :, :, :), &
                                  intent(in ) :: values
       real(kind = EightByteReal),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_4D_EightByteReal
     subroutine ncdf_putvar_5D_OneByteInt (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       integer(kind = OneByteInt), dimension(:, :, :, :, :), &
                                  intent(in ) :: values
       integer(kind = OneByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_5D_OneByteInt
     subroutine ncdf_putvar_5D_TwoByteInt (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       integer(kind = TwoByteInt), dimension(:, :, :, :, :), &
                                  intent(in ) :: values
       integer(kind = TwoByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_5D_TwoByteInt
     subroutine ncdf_putvar_5D_FourByteInt (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       integer(kind = FourByteInt), dimension(:, :, :, :, :), &
                                  intent(in ) :: values
       integer(kind = FourByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_5D_FourByteInt
     subroutine ncdf_putvar_5D_EightByteInt (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       integer(kind = EightByteInt), dimension(:, :, :, :, :), &
                                  intent(in ) :: values
       integer(kind = EightByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_5D_EightByteInt
     subroutine ncdf_putvar_5D_FourByteReal (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       real(kind = FourByteReal), dimension(:, :, :, :, :), &
                                  intent(in ) :: values
       real(kind = FourByteReal),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_5D_FourByteReal
     subroutine ncdf_putvar_5D_EightByteReal (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       real(kind = EightByteReal), dimension(:, :, :, :, :), &
                                  intent(in ) :: values
       real(kind = EightByteReal),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_5D_EightByteReal
     subroutine ncdf_putvar_6D_OneByteInt (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       integer(kind = OneByteInt), dimension(:, :, :, :, :, :), &
                                  intent(in ) :: values
       integer(kind = OneByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_6D_OneByteInt
     subroutine ncdf_putvar_6D_TwoByteInt (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       integer(kind = TwoByteInt), dimension(:, :, :, :, :, :), &
                                  intent(in ) :: values
       integer(kind = TwoByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_6D_TwoByteInt
     subroutine ncdf_putvar_6D_FourByteInt (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       integer(kind = FourByteInt), dimension(:, :, :, :, :, :), &
                                  intent(in ) :: values
       integer(kind = FourByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_6D_FourByteInt
     subroutine ncdf_putvar_6D_EightByteInt (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       integer(kind = EightByteInt), dimension(:, :, :, :, :, :), &
                                  intent(in ) :: values
       integer(kind = EightByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_6D_EightByteInt
     subroutine ncdf_putvar_6D_FourByteReal (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       real(kind = FourByteReal), dimension(:, :, :, :, :, :), &
                                  intent(in ) :: values
       real(kind = FourByteReal),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_6D_FourByteReal
     subroutine ncdf_putvar_6D_EightByteReal (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       real(kind = EightByteReal), dimension(:, :, :, :, :, :), &
                                  intent(in ) :: values
       real(kind = EightByteReal),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_6D_EightByteReal
     subroutine ncdf_putvar_7D_OneByteInt (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       integer(kind = OneByteInt), dimension(:, :, :, :, :, :, :), &
                                  intent(in ) :: values
       integer(kind = OneByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_7D_OneByteInt
     subroutine ncdf_putvar_7D_TwoByteInt (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       integer(kind = TwoByteInt), dimension(:, :, :, :, :, :, :), &
                                  intent(in ) :: values
       integer(kind = TwoByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_7D_TwoByteInt
     subroutine ncdf_putvar_7D_FourByteInt (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       integer(kind = FourByteInt), dimension(:, :, :, :, :, :, :), &
                                  intent(in ) :: values
       integer(kind = FourByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_7D_FourByteInt
     subroutine ncdf_putvar_7D_EightByteInt (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       integer(kind = EightByteInt), dimension(:, :, :, :, :, :, :), &
                                  intent(in ) :: values
       integer(kind = EightByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_7D_EightByteInt
     subroutine ncdf_putvar_7D_FourByteReal (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       real(kind = FourByteReal), dimension(:, :, :, :, :, :, :), &
                                  intent(in ) :: values
       real(kind = FourByteReal),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_7D_FourByteReal
     subroutine ncdf_putvar_7D_EightByteReal (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       real(kind = EightByteReal), dimension(:, :, :, :, :, :, :), &
                                  intent(in ) :: values
       real(kind = EightByteReal),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_putvar_7D_EightByteReal
  end interface
  interface ncdf_getvar
     subroutine ncdf_getvar_OneByteInt (name, values, ncfile, ncid, rec, start, units, range)
       use typeSizes
       character(len = *),        intent( in) :: name
       integer(kind = OneByteInt), &
                                  intent(out) :: values
       integer(kind = OneByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       character(len = *),        optional    :: units
     end subroutine ncdf_getvar_OneByteInt
     subroutine ncdf_getvar_TwoByteInt (name, values, ncfile, ncid, rec, start, units, range)
       use typeSizes
       character(len = *),        intent( in) :: name
       integer(kind = TwoByteInt), &
                                  intent(out) :: values
       integer(kind = TwoByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       character(len = *),        optional    :: units
     end subroutine ncdf_getvar_TwoByteInt
     subroutine ncdf_getvar_FourByteInt (name, values, ncfile, ncid, rec, start, units, range)
       use typeSizes
       character(len = *),        intent( in) :: name
       integer(kind = FourByteInt), &
                                  intent(out) :: values
       integer(kind = FourByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       character(len = *),        optional    :: units
     end subroutine ncdf_getvar_FourByteInt
     subroutine ncdf_getvar_EightByteInt (name, values, ncfile, ncid, rec, start, units, range)
       use typeSizes
       character(len = *),        intent( in) :: name
       integer(kind = EightByteInt), &
                                  intent(out) :: values
       integer(kind = EightByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       character(len = *),        optional    :: units
     end subroutine ncdf_getvar_EightByteInt
     subroutine ncdf_getvar_FourByteReal (name, values, ncfile, ncid, rec, start, units, range)
       use typeSizes
       character(len = *),        intent( in) :: name
       real(kind = FourByteReal), &
                                  intent(out) :: values
       real(kind = FourByteReal),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       character(len = *),        optional    :: units
     end subroutine ncdf_getvar_FourByteReal
     subroutine ncdf_getvar_EightByteReal (name, values, ncfile, ncid, rec, start, units, range)
       use typeSizes
       character(len = *),        intent( in) :: name
       real(kind = EightByteReal), &
                                  intent(out) :: values
       real(kind = EightByteReal),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       character(len = *),        optional    :: units
     end subroutine ncdf_getvar_EightByteReal
     subroutine ncdf_getvar_text (name, values, ncfile, ncid, rec, start, units, range)
       use typeSizes
       character(len = *),        intent( in) :: name
       character (len = *), &
                                  intent(out) :: values
       character (len = *),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       character(len = *),        optional    :: units
     end subroutine ncdf_getvar_text
     subroutine ncdf_getvar_1D_OneByteInt (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       integer(kind = OneByteInt), dimension(:), &
                                  intent(out) :: values
       integer(kind = OneByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_getvar_1D_OneByteInt
     subroutine ncdf_getvar_1D_TwoByteInt (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       integer(kind = TwoByteInt), dimension(:), &
                                  intent(out) :: values
       integer(kind = TwoByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_getvar_1D_TwoByteInt
     subroutine ncdf_getvar_1D_FourByteInt (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       integer(kind = FourByteInt), dimension(:), &
                                  intent(out) :: values
       integer(kind = FourByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_getvar_1D_FourByteInt
     subroutine ncdf_getvar_1D_EightByteInt (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       integer(kind = EightByteInt), dimension(:), &
                                  intent(out) :: values
       integer(kind = EightByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_getvar_1D_EightByteInt
     subroutine ncdf_getvar_1D_FourByteReal (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       real(kind = FourByteReal), dimension(:), &
                                  intent(out) :: values
       real(kind = FourByteReal),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_getvar_1D_FourByteReal
     subroutine ncdf_getvar_1D_EightByteReal (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       real(kind = EightByteReal), dimension(:), &
                                  intent(out) :: values
       real(kind = EightByteReal),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
              character(len = *),        optional    :: units
     end subroutine ncdf_getvar_1D_EightByteReal
     subroutine ncdf_getvar_2D_OneByteInt (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       integer(kind = OneByteInt), dimension(:, :), &
                                  intent(out) :: values
       integer(kind = OneByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_getvar_2D_OneByteInt
     subroutine ncdf_getvar_2D_TwoByteInt (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       integer(kind = TwoByteInt), dimension(:, :), &
                                  intent(out) :: values
       integer(kind = TwoByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
     end subroutine ncdf_getvar_2D_TwoByteInt
     subroutine ncdf_getvar_2D_FourByteInt (name, values, ncfile, ncid, rec, start, count, units, range)
       use typeSizes
       character(len = *),        intent(in)  :: name
       integer(kind = FourByteInt), dimension(:, :), &
                                  intent(out) :: values
       integer(kind = FourByteInt),    dimension(2),     optional    :: range
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer,                   optional    :: rec
       integer, dimension(:),     optional    :: start
       integer, dimension(:),     optional    :: count
       character(len = *),        optional    :: units
       
     end subroutine ncdf_getvar_2D_FourByteInt

end module ncdf