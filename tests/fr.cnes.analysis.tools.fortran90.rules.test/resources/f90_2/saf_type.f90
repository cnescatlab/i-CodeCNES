!!* saf_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: october 2009
!!*           Version: $Revision: 1.7 $
!!* Last modification: $Date: 2011-04-12 08:32:32 $
!!
!> saf_type -- Module
!!
!! * Purpose
!!
!!   Module for saf type declaration and allocation.
!!
!! * Description
!!      
!!     This module defines the saf type and allows its allocation and declaration. 
!!
!! * Sub-routines and functions
!!
!!     * type_Saf   : type for declaration and allocation of Saf
!!     * alloc_Saf  : type_Saf allocation
!!     * dalloc_Saf : type_Saf deallocation
!!     * Saf_Base   : computes Saf base in Opd
!!     * readsaf    : type_Saf reading routine
!!     * writesaf   : type_Saf writing routine
!!     * writesaf_netcdf : type_Saf writing routine (netCDF format)
!!     * write_saf_data  : type_Saf writing routine (netCDF format)
!!     * init_saf_data   : creation and initialization of the netcdf file
!!     * readsaf_netcdf : type_Saf reading routine (netCDF format)
!!     * read_saf_data  : type_Saf reading routine (netCDF format)
!!     * get_saf_data    : get global data from the netcdf file
!!
!! * References
!!
!! Status:
!! modified 29.07.2013: (JD) writing routine netCDF


module saf_type
   use precision_type
   use error_type
   use constantes_type
   use isrf_index_type
   use modinouttools
   use modionetcdf
   use netcdf
!
   implicit none
!
!
   public :: type_Saf     &
            ,alloc_Saf    &
            ,dalloc_Saf   &
            ,Saf_Base     &
            ,Saf_Transfer &
            ,readsaf      &
            ,writesaf     &
            ,writesaf_netcdf
!
   type :: type_Saf
     type(type_isrf_index)                            :: TopLevelSaf ! < saf top level indexes
     character(len=500)                               :: filename !< self-apodisation function characterisation file name
     integer(kind=LONG)                               :: NsPixel !< sub-pixels number
     integer(kind=LONG)                               :: NsWn0 !< frequencies samples number
     integer(kind=LONG)                               :: NsOpd !< OPD samples number
     real(kind=DOUBLE)                                :: OpdMax !< maximum optical path difference (m)
     real(kind=DOUBLE)                                :: dOpd !< optical path difference sampling (m)
     real(kind=DOUBLE) ,dimension(:)    ,allocatable  :: Wn0 !< reference frequency
     real(kind=DOUBLE) ,dimension(:)    ,allocatable  :: Opd !< optical path difference (m)
     real(kind=DOUBLE) ,dimension(:,:,:),allocatable  :: Mod !< self-apodisation function modulus
     real(kind=DOUBLE) ,dimension(:,:,:),allocatable  :: Arg !< self-apodisation function argument
     real(kind=DOUBLE) ,dimension(:,:)  ,allocatable  :: Arg0 !< self-apodisation function argument at null field
     real(kind=DOUBLE) ,dimension(:,:)  ,allocatable  :: Contrast !< contrast function
     real(kind=DOUBLE) ,dimension(:,:)  ,allocatable  :: FieldMeanAngle !< field mean angle 
  end type type_Saf
   contains
!
!
   subroutine alloc_Saf( Saf )
   implicit none
     type(type_Saf)    , intent(inout)      :: Saf
     integer(kind=LONG)                     :: ErrCode
     integer(kind=LONG)                     :: ioalloc, i
!
     ioalloc = 0
     allocate( Saf%TopLevelSaf%SubFov(Saf%NsPixel),        stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
     ! Saf%TopLevelSaf%SubFov is the list of sub-pixel index (1:NsPixel)
     do i=1,Saf%NsPixel
        Saf%TopLevelSaf%SubFov(i) = i
     end do
     allocate( Saf%Wn0(Saf%NsWn0),                         stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
     allocate( Saf%Opd(Saf%NsOpd),                         stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
     allocate( Saf%Mod(Saf%NsOpd,Saf%NsWn0,Saf%NsPixel+1), stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
     allocate( Saf%Arg(Saf%NsOpd,Saf%NsWn0,Saf%NsPixel+1), stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
     allocate( Saf%Arg0(Saf%NsWn0,Saf%NsPixel+1),          stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
     allocate( Saf%Contrast(Saf%NsWn0,Saf%NsPixel+1),      stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
     allocate( Saf%FieldMeanAngle(Saf%NsWn0,Saf%NsPixel+1),stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
!
     if (ioalloc > 0) then
        write(0,*) 'allocation Saf Error'
        write(0,*) 'Saf: fatal error'
        call exit(1)
     end if
     return
   end subroutine alloc_Saf
!
!
   subroutine dalloc_Saf( Saf )
   implicit none
     type(type_Saf)    , intent(inout)         :: Saf
!
     deallocate( Saf%TopLevelSaf%SubFov )
     deallocate( Saf%Wn0 )
     deallocate( Saf%Opd )
     deallocate( Saf%Mod )
     deallocate( Saf%Arg )
     deallocate( Saf%Arg0 )
     deallocate( Saf%Contrast )
     deallocate( Saf%FieldMeanAngle )
     return
   end subroutine dalloc_Saf
!
!
   subroutine Saf_Base( Saf )
   implicit none
     type(type_Saf)    , intent(inout)                :: Saf
     integer(kind=LONG)                               :: NsOpd0
     integer(kind=LONG)                               :: Ns
!
     NsOpd0   = int( Saf%NsOpd/2 ) + 1
     Saf%Opd(1:Saf%NsOpd) = (/ (dble(Ns-NsOpd0),Ns=1,Saf%NsOpd) /) *  Saf%dOpd
!
     return
   end subroutine Saf_Base
!
!
   subroutine Saf_Transfer( Saf_In, Saf_Out )
     type(type_Saf)    , intent(in)          :: Saf_In
     type(type_Saf)    , intent(out)         :: Saf_Out
!
     Saf_Out%NsWn0 = Saf_In%NsWn0
     Saf_Out%NsPixel = Saf_In%NsPixel
     Saf_Out%NsOpd   = Saf_In%NsOpd
     call alloc_Saf( Saf_Out )
     Saf_Out = Saf_In
!
   return
   end subroutine Saf_Transfer
!
!
   subroutine readsaf( Saf,iostatus )
   implicit none
     !$PRAGMA C(inouttools)
     type(type_Saf)    , intent(inout) :: Saf
!
     integer(kind=LONG), intent(out)   :: iostatus
     integer(kind=DOUBLE)              :: ifile
     integer(kind=LONG)                :: offset
     integer(kind=LONG)                :: Type
     integer(kind=LONG)                :: Size
!
     iostatus = 0
     call open_file_r(Saf%filename                    &
                      (1:len_trim(Saf%filename))      &
                      // char(0), ifile)
     if ( ifile .eq. 0 ) then
        iostatus = 1
     else
        offset = 0
        Type = i4Type
        Size = 4
        call read_field( ifile, Saf%NsPixel,                  &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Saf%NsWn0,                    &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Saf%NsOpd,                    &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Type = r8Type
        Size = 8
        call read_field( ifile, Saf%OpdMax,                   &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Saf%dOpd,                     &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call alloc_Saf( Saf )
        Size = 8*Saf%NsWn0
        call read_field( ifile, Saf%Wn0,                      &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Saf%NsOpd
        call read_field( ifile, Saf%Opd,                      &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Saf%NsOpd*Saf%NsWn0*(Saf%NsPixel+1)
        call read_field( ifile, Saf%Mod,                      &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Saf%Arg,                      &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Saf%NsWn0*(Saf%NsPixel+1)
        call read_field( ifile, Saf%Arg0,                     &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Saf%Contrast,                 &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Saf%FieldMeanAngle,           &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call close_file(Saf%filename              &
                        (1:len_trim(Saf%filename))&
                        // char(0), ifile)
      end if
     return
  end subroutine readsaf
!
!
   subroutine writesaf( Saf,iostatus )
   implicit none
     !$PRAGMA C(inouttools)
     type(type_Saf)    , intent(in)  :: Saf
!
     integer(kind=LONG), intent(out) :: iostatus
     integer(kind=DOUBLE)            :: ifile
     integer(kind=LONG)              :: offset
     integer(kind=LONG)              :: Type
     integer(kind=LONG)              :: Size
!
     iostatus = 0
     call open_file_w(Saf%filename                    &
                      (1:len_trim(Saf%filename))      &
                      // char(0), ifile)
     if ( ifile .eq. 0 ) then
        iostatus = 1
     else
        offset = 0
        Type = i4Type
        Size = 4
        call write_field( ifile, Saf%NsPixel,                  &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Saf%NsWn0,                    &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Saf%NsOpd,                    &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Type = r8Type
        Size = 8
        call write_field( ifile, Saf%OpdMax,                   &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Saf%dOpd,                     &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Saf%NsWn0
        call write_field( ifile, Saf%Wn0,                      &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Saf%NsOpd
        call write_field( ifile, Saf%Opd,                      &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Saf%NsOpd*Saf%NsWn0*(Saf%NsPixel+1)
        call write_field( ifile, Saf%Mod,                      &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Saf%Arg,                      &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Saf%NsWn0*(Saf%NsPixel+1)
        call write_field( ifile, Saf%Arg0,                     &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Saf%Contrast,                 &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Saf%FieldMeanAngle,           &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call close_file(Saf%filename              &
                        (1:len_trim(Saf%filename))&
                        // char(0), ifile)
     end if
     return
  end subroutine writesaf
!
!
!>  writesaf_netcdf -- Public
!!
!! * Purpose
!!
!!     Write Saf type
!!
!! * Description
!!
!!     Write Saf type
!!
!! * Inputs
!!
!!     - Saf  : type_Saf / type for declaration and allocation of saf parameters
!!
!! * Outputs
!!
!!     - iostatus : integer / error management varaible
!!
!! * InOut
!!
!!     - init_file : integer / flag to initialize the netcdf file
!!
!! * References
!!
!!   SPS
!!
subroutine writesaf_netcdf( Saf,iostatus, init_file )

     implicit none

     type(type_Saf), intent(in) :: Saf
     integer(kind=LONG), intent(out) :: iostatus
     integer(kind=SHORT), intent(inout) :: init_file

     integer(kind=LONG) :: ncid, status, i_band

     iostatus = 0

     ! file creation and definition
     if (init_file == 0) then
        call init_saf_data(Saf, ncid)
        init_file = 1
     endif

     ! open netCDF dataset
     status = nf90_open(path = trim(Saf%TopLevelSaf%header%filename), mode = nf90_write, ncid = ncid)
     if (status /= nf90_noerr) call handle_err(status)

     ! writes variables for saf group
     ! current spectral band
     i_band = Saf%TopLevelSaf%Band
     call write_saf_data(ncid, i_band, Saf)

     ! close access to netCDF dataset
     status = nf90_close(ncid)
     if (status /= nf90_noerr) call handle_err(status)

     return

   end subroutine writesaf_netcdf
   !
   !
   subroutine init_saf_data(Saf, ncid)

     implicit none

     type(type_Saf), intent(in) :: Saf
     integer(kind=LONG), intent(out) :: ncid

     integer(kind=LONG) :: status
     integer(kind=LONG) :: varid, dimid

     ! create netCDF dataset
     status = nf90_create(path = trim(Saf%TopLevelSaf%header%filename), cmode = NF90_NETCDF4, ncid = ncid)
     if (status /= nf90_noerr) call handle_err(status)

     ! define and write global variables

     !
     ! ColFov(NcolFov)
     status = nf90_def_dim(ncid, "NcolFov", Saf%TopLevelSaf%NcolFov, dimid)
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_def_var(ncid, "ColFov", NF90_INT, (/dimid/), varid)
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_att(ncid, varid, "units", "N.A.")
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_att(ncid, varid, "long_name", "column Fov value")
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_var(ncid, varid, Saf%TopLevelSaf%ColFov)
     if (status /= nf90_noerr) call handle_err(status)
     ! LinFov(NlinFov)
     status = nf90_def_dim(ncid, "NlinFov", Saf%TopLevelSaf%NlinFov, dimid)
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_def_var(ncid, "LinFov", NF90_INT, (/dimid/), varid)
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_att(ncid, varid, "units", "N.A.")
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_att(ncid, varid, "long_name", "line Fov value")
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_var(ncid, varid, Saf%TopLevelSaf%LinFov)
     if (status /= nf90_noerr) call handle_err(status)

     ! define global attributes
     ! filename
     status = nf90_put_att(ncid, NF90_GLOBAL, name = "filename", values = trim(Saf%TopLevelSaf%header%filename))
     if (status /= nf90_noerr) call handle_err(status)
     ! instrument name
     status = nf90_put_att(ncid, NF90_GLOBAL, name = "InsName", values = Saf%TopLevelSaf%header%InsName)
     if (status /= nf90_noerr) call handle_err(status)
     ! instrument mode
     status = nf90_put_att(ncid, NF90_GLOBAL, name = "InsMode", values = Saf%TopLevelSaf%header%InsMode)
     if (status /= nf90_noerr) call handle_err(status)
     ! S/W version
     status = nf90_put_att(ncid, NF90_GLOBAL, name = "SoftWareVersion", values = Saf%TopLevelSaf%header%SoftWareVersion)
     if (status /= nf90_noerr) call handle_err(status)
     ! Operator
     status = nf90_put_att(ncid, NF90_GLOBAL, name = "Operator", values = Saf%TopLevelSaf%header%Operator)
     if (status /= nf90_noerr) call handle_err(status)
     ! Processing step
     status = nf90_put_att(ncid, NF90_GLOBAL, name = "ProcessingStep", values = Saf%TopLevelSaf%header%ProcessingStep)
     if (status /= nf90_noerr) call handle_err(status)
     ! Conf ID
     status = nf90_put_att(ncid, NF90_GLOBAL, name = "ConfId", values = trim(Saf%TopLevelSaf%header%ConfId))
     if (status /= nf90_noerr) call handle_err(status)

     ! close access to netCDF dataset
     status = nf90_close(ncid)
     if (status /= nf90_noerr) call handle_err(status)

     return

   end subroutine init_saf_data

   subroutine write_saf_data(ncid, band, Saf)

     implicit none

     integer(kind=LONG), intent(in)  :: ncid, band
     type(type_Saf), intent(in) :: Saf

     integer(kind=LONG),dimension(5) :: start_w, count_w
     integer(kind=LONG),dimension(4) :: start_w_agg, count_w_agg, start_w0, count_w0
     integer(kind=LONG),dimension(3) :: start_w0_agg, count_w0_agg

     integer(kind=LONG) :: status
     integer(kind=LONG) :: grpid, varid, flag_grp
     integer(kind=LONG) :: nsw_dimid, nso_dimid, nsf_dimid, ndims
     integer(kind=LONG), dimension(:), allocatable :: dimids
     character(len=1) :: band_str
     integer(kind=LONG) :: parent
     integer(kind=LONG) :: ErrCode
     integer(kind=LONG) :: nsopd, nswn0

     ! NsOpd
     nsopd = Saf%NsOpd
     ! NsWn0
     nswn0 = Saf%NsWn0

     ! start index
     ! NlinFov,NcolFov,NsubFov,NsWn0,NsOpd
     start_w = (/1,1,1,1,1/)
     ! count index
     count_w = (/1,1,Saf%TopLevelSaf%NSubfov,nswn0,nsopd/)

     ! NlinFov,NcolFov,NsWn0,NsOpd
     start_w_agg = (/1,1,1,1/)
     ! count index
     count_w_agg = (/1,1,nswn0,nsopd/)

     ! NlinFov,NcolFov,NSubFov,NsWn0
     start_w0 = (/1,1,1,1/)
     ! count index
     count_w0 = (/1,1,Saf%TopLevelSaf%NSubfov,nswn0/)

     ! NlinFov,NcolFov,NsWn0
     start_w0_agg = (/1,1,1/)
     ! count index
     count_w0_agg = (/1,1,nswn0/)

     ! definition of the group (one group for one spectral band)
     write(band_str,'(i1)') band

     ! check the group existence
     flag_grp = nf90_inq_ncid(ncid, 'saf_sb'//band_str, grpid)

     if (flag_grp /= nf90_noerr) then
        ! group creation and initialization
        status = nf90_def_grp(ncid, 'saf_sb'//band_str, grpid)
        if (status /= nf90_noerr) call handle_err(status)

        ! definition of the dimensions
        ! NsWn0
        status = nf90_def_dim(grpid, "NsWn0", nswn0, nsw_dimid)
        if (status /= nf90_noerr) call handle_err(status)
        ! NsOpd
        status = nf90_def_dim(grpid, "NsOpd", nsopd, nso_dimid)
        if (status /= nf90_noerr) call handle_err(status)
        ! NsubFov
        status = nf90_def_dim(grpid, "NsubFov", Saf%TopLevelSaf%NsubFov, nsf_dimid)
        if (status /= nf90_noerr) call handle_err(status)

       ! get the ids of global dimensions (NcolFov, NlinFov)
        status = nf90_inquire(ncid, nDimensions=ndims)
        allocate(dimids(ndims), stat=ErrCode)
        parent=0
        ! dimids(1):NcolFov, dimids(2):NlinFov
        status = nf90_inq_dimids(ncid, ndims, dimids, parent)
        if (status /= nf90_noerr) call handle_err(status)

        ! definition of the variables
        ! SubFov
        call write_def_variable(grpid, "SubFov", NF90_INT, (/nsf_dimid/), "N.A.", "subFov value")
        ! Wn0
        call write_def_variable(grpid, "Wn0", NF90_DOUBLE, (/nsw_dimid/), "m-1", "reference wavenumber")
        ! Opd
        call write_def_variable(grpid, "Opd", NF90_DOUBLE, (/nso_dimid/), "m", "optical path difference")
        ! Mod
        call write_def_variable(grpid, "Mod", NF90_DOUBLE, (/dimids(2),dimids(1),nsf_dimid,nsw_dimid,nso_dimid/),&
                                 "N.A.", "self-apodisation function modulus")
        ! Mod_agg
        call write_def_variable(grpid, "Mod_agg", NF90_DOUBLE, (/dimids(2),dimids(1),nsw_dimid,nso_dimid/),&
                                 "N.A.", "aggregated self-apodisation function modulus")
        ! Arg
        call write_def_variable(grpid, "Arg", NF90_DOUBLE, (/dimids(2),dimids(1),nsf_dimid,nsw_dimid,nso_dimid/),&
                                 "rad", "self-apodisation function argument")
        ! Arg_agg
        call write_def_variable(grpid, "Arg_agg", NF90_DOUBLE, (/dimids(2),dimids(1),nsw_dimid,nso_dimid/),&
                                 "rad", "aggregated self-apodisation function argument")
        ! Arg0
        call write_def_variable(grpid, "Arg0", NF90_DOUBLE, (/dimids(2),dimids(1),nsf_dimid,nsw_dimid/),&
                                 "rad", "self-apodisation function argument at null field")
        ! Arg0_agg
        call write_def_variable(grpid, "Arg0_agg", NF90_DOUBLE, (/dimids(2),dimids(1),nsw_dimid/),&
                                 "rad", "aggregated self-apodisation function argument at null field")
        ! Contrast
        call write_def_variable(grpid, "Contrast", NF90_DOUBLE, (/dimids(2),dimids(1),nsf_dimid,nsw_dimid/),&
                                 "N.A.", "contrast function")
        ! Contrast_agg
        call write_def_variable(grpid, "Contrast_agg", NF90_DOUBLE, (/dimids(2),dimids(1),nsw_dimid/),&
                                 "N.A.", "aggregated contrast function")
        ! FieldMeanAngle
        call write_def_variable(grpid, "FieldMeanAngle", NF90_DOUBLE, (/dimids(2),dimids(1),nsf_dimid,nsw_dimid/),&
                                 "rad", "field mean angle")
        ! FieldMeanAngle_agg
        call write_def_variable(grpid, "FieldMeanAngle_agg", NF90_DOUBLE, (/dimids(2),dimids(1),nsw_dimid/),&
                                 "rad", "aggregated field mean angle")

        ! write group attributes
        ! Band
        status = nf90_put_att(grpid, NF90_GLOBAL, name= "Band", values = band)
        if (status /= nf90_noerr) call handle_err(status)
        ! OpdMax
        status = nf90_put_att(grpid, NF90_GLOBAL, name= "OpdMax", values = Saf%OpdMax)
        if (status /= nf90_noerr) call handle_err(status)
        ! dOpd
        status = nf90_put_att(grpid, NF90_GLOBAL, name= "dOpd", values = Saf%dOpd)
        if (status /= nf90_noerr) call handle_err(status)

     end if ! end group creation and initialization

     ! write variables
     ! Saf%TopLevelSaf%SubFov is the list of sub-pixel index
     status = nf90_inq_varid(grpid, "SubFov", varid)
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_var(grpid, varid, Saf%TopLevelSaf%SubFov, start=(/1/), count=(/Saf%TopLevelSaf%NSubFov/))
     if (status /= nf90_noerr) call handle_err(status)

     ! Wn0
     call write_variable(grpid, "Wn0", Saf%Wn0)
     ! Opd
     call write_variable(grpid, "Opd", Saf%Opd)
     ! Mod
     call write_variable(grpid, "Mod", Saf%Mod(:,:,1:Saf%NsPixel), start_w, count_w)
     ! Mod_agg
     call write_variable(grpid, "Mod_agg", Saf%Mod(:,:,Saf%NsPixel+1), start_w_agg, count_w_agg)
     ! Arg
     call write_variable(grpid, "Arg", Saf%Arg(:,:,1:Saf%NsPixel), start_w, count_w)
     ! Arg_agg
     call write_variable(grpid, "Arg_agg", Saf%Arg(:,:,Saf%NsPixel+1), start_w_agg, count_w_agg)
     ! Arg0
     call write_variable(grpid, "Arg0", Saf%Arg0(:,1:Saf%NsPixel), start_w0, count_w0)
     ! Arg0_agg
     call write_variable(grpid, "Arg0_agg", Saf%Arg0(:,Saf%NsPixel+1), start_w0_agg, count_w0_agg)
     ! Contrast
     call write_variable(grpid, "Contrast", Saf%Contrast(:,1:Saf%NsPixel), start_w0, count_w0)
     ! Contrast_agg
     call write_variable(grpid, "Contrast_agg", Saf%Contrast(:,Saf%NsPixel+1), start_w0_agg, count_w0_agg)
     ! FieldMeanAngle
     call write_variable(grpid, "FieldMeanAngle", Saf%FieldMeanAngle(:,1:Saf%NsPixel), start_w0, count_w0)
     ! FieldMeanAngle_agg
     call write_variable(grpid, "FieldMeanAngle_agg", Saf%FieldMeanAngle(:,Saf%NsPixel+1), start_w0_agg, count_w0_agg)

     return

   end subroutine write_saf_data
!
!
!>  readsaf_netcdf -- Public
!!
!! * Purpose
!!
!!     Read saf type
!!
!! * Description
!!
!!     Read saf type
!!
!! * Inputs
!!
!!     - Saf  : type_saf / type for declaration and allocation of saf parameters
!!
!! * Outputs
!!
!!     - Saf  : type_saf / type for declaration and allocation of saf parameters
!!     - iostatus : integer / error management variable
!!
!! * References
!!
!!   SPS
!!
   subroutine readsaf_netcdf( Saf,iostatus )

     implicit none

     type(type_Saf),     intent(inout) :: Saf
     integer(kind=LONG), intent(out)   :: iostatus

     integer(kind=LONG) :: ncid, status, i_band

     iostatus = 0

     ! open existing netCDF dataset
     status = nf90_open(path = trim(Saf%TopLevelSaf%header%filename), mode = nf90_nowrite, ncid = ncid)
     if (status /= nf90_noerr) call handle_err(status)

     ! get global data (variables and attributes)
     call get_saf_data(ncid, Saf)

     ! reads variables for saf group
     ! current spectral band
     i_band = Saf%TopLevelSaf%Band
     call read_saf_data(ncid, i_band, Saf)

     ! close access to netCDF dataset
     status = nf90_close(ncid)
     if (status /= nf90_noerr) call handle_err(status)

     return

   end subroutine readsaf_netcdf
!
!
subroutine read_saf_data(ncid, band, Saf)

     implicit none

     integer(kind=LONG), intent(in)    :: ncid, band
     type(type_Saf),     intent(inout) :: Saf

     integer(kind=LONG),dimension(5) :: start_w, count_w
     integer(kind=LONG),dimension(4) :: start_w_agg, count_w_agg, start_w0, count_w0
     integer(kind=LONG),dimension(3) :: start_w0_agg, count_w0_agg

     integer(kind=LONG) :: status
     integer(kind=LONG) :: grpid

     integer(kind=LONG) :: ndimGrp, nvarGrp, nattGrp, nvar
     integer(kind=LONG), dimension(:), allocatable :: varidsGrp
     integer(kind=LONG) :: ErrCode
     integer(kind=LONG) :: dimid_nsopd, dimid_nswn0, dimid_nsubfov
     character(len=1)   :: band_str

     ! get the group id
     write(band_str,'(i1)') band
     status = nf90_inq_ncid(ncid, 'saf_sb'//band_str, grpid)
     if (status /= nf90_noerr) call handle_err(status)

     ! get information about the group dataset
     status = nf90_inquire(grpid, nDimensions=ndimGrp, nVariables=nvarGrp, nAttributes=nattGrp)
     if (status /= nf90_noerr) call handle_err(status)

     ! get variable ids from group dataset
     allocate(varidsGrp(nvarGrp), stat=ErrCode)
     status = nf90_inq_varids(grpid, nvar, varidsGrp)
     if (status /= nf90_noerr) call handle_err(status)

     ! read values for group attributes
     ! OpdMax
     status = nf90_get_att(grpid, NF90_GLOBAL, name="OpdMax", values=Saf%OpdMax)
     if (status /= nf90_noerr) call handle_err(status)

     ! dOpd
     status = nf90_get_att(grpid, NF90_GLOBAL, name="dOpd", values=Saf%dOpd)
     if (status /= nf90_noerr) call handle_err(status)

     ! get NsWn0
     status = nf90_inq_dimid(grpid, "NsWn0", dimid_nswn0)
     if (status /= nf90_noerr) call handle_err(status)

     status = nf90_inquire_dimension(grpid, dimid_nswn0, len = Saf%NsWn0)
     if(status /= nf90_NoErr) call handle_err(status)

     ! get NsOpd
     status = nf90_inq_dimid(grpid, "NsOpd", dimid_nsopd)
     if (status /= nf90_noerr) call handle_err(status)

     status = nf90_inquire_dimension(grpid, dimid_nsopd, len = Saf%NsOpd)
     if(status /= nf90_NoErr) call handle_err(status)

     ! get NsubFov
     status = nf90_inq_dimid(grpid, "NsubFov", dimid_nsubfov)
     if (status /= nf90_noerr) call handle_err(status)

     status = nf90_inquire_dimension(grpid, dimid_nsubfov, len = Saf%TopLevelSaf%NsubFov)
     if(status /= nf90_NoErr) call handle_err(status)

     Saf%NsPixel = Saf%TopLevelSaf%NsubFov

     ! Saf allocation
     call alloc_Saf( Saf )

     ! Wn0
     status = nf90_get_var(grpid, varidsGrp(2), Saf%Wn0)
     if (status /= nf90_noerr) call handle_err(status)

     ! Opd
      status = nf90_get_var(grpid, varidsGrp(3), Saf%Opd)
     if (status /= nf90_noerr) call handle_err(status)

     ! NlinFov,NcolFov,NsubFov,NsWn0,NsOpd
     ! start index
     start_w = (/1,1,1,1,1/)
     ! count index
     count_w = (/1,1,Saf%TopLevelSaf%NsubFov,Saf%NsWn0,Saf%NsOpd/)
     ! NlinFov,NcolFov,NsWn0,NsOpd
     ! start index
     start_w_agg = (/1,1,1,1/)
     ! count index
     count_w_agg = (/1,1,Saf%NsWn0,Saf%NsOpd/)
     ! NlinFov,NcolFov,NSubFov,NsWn0
     ! start index
     start_w0 = (/1,1,1,1/)
     ! count index
     count_w0 = (/1,1,Saf%TopLevelSaf%NsubFov,Saf%NsWn0/)
     ! NlinFov,NcolFov,NsWn0
     ! start index
     start_w0_agg = (/1,1,1/)
     ! count index
     count_w0_agg = (/1,1,Saf%NsWn0/)

     ! Mod
     status = nf90_get_var(grpid, varidsGrp(4), Saf%Mod(:,:,1:Saf%NsPixel), start=start_w, count=count_w)
     if (status /= nf90_noerr) call handle_err(status)
     ! Mod_agg
     status = nf90_get_var(grpid, varidsGrp(5), Saf%Mod(:,:,Saf%NsPixel+1), start=start_w_agg, count=count_w_agg)
     if (status /= nf90_noerr) call handle_err(status)
     ! Arg
     status = nf90_get_var(grpid, varidsGrp(6), Saf%Arg(:,:,1:Saf%NsPixel), start=start_w, count=count_w)
     if (status /= nf90_noerr) call handle_err(status)
     ! Arg_agg
     status = nf90_get_var(grpid, varidsGrp(7), Saf%Arg(:,:,Saf%NsPixel+1), start=start_w_agg, count=count_w_agg)
     if (status /= nf90_noerr) call handle_err(status)
     ! Arg0
     status = nf90_get_var(grpid, varidsGrp(8), Saf%Arg0(:,1:Saf%NsPixel), start=start_w0, count=count_w0)
     if (status /= nf90_noerr) call handle_err(status)
     ! Arg0_agg
     status = nf90_get_var(grpid, varidsGrp(9), Saf%Arg0(:,Saf%NsPixel+1), start=start_w0_agg, count=count_w0_agg)
     if (status /= nf90_noerr) call handle_err(status)
     ! Contrast
     status = nf90_get_var(grpid, varidsGrp(10), Saf%Contrast(:,1:Saf%NsPixel), start=start_w0, count=count_w0)
     if (status /= nf90_noerr) call handle_err(status)
     ! Contrast_agg
     status = nf90_get_var(grpid, varidsGrp(11), Saf%Contrast(:,Saf%NsPixel+1), start=start_w0_agg, count=count_w0_agg)
     if (status /= nf90_noerr) call handle_err(status)
     ! FieldMeanAngle
     status = nf90_get_var(grpid, varidsGrp(12), Saf%FieldMeanAngle(:,1:Saf%NsPixel), start=start_w0, count=count_w0)
     if (status /= nf90_noerr) call handle_err(status)
     ! FieldMeanAngle_agg
     status = nf90_get_var(grpid, varidsGrp(13), Saf%FieldMeanAngle(:,Saf%NsPixel+1), start=start_w0_agg, count=count_w0_agg)
     if (status /= nf90_noerr) call handle_err(status)

     ! deallocate local variables
     deallocate(varidsGrp, stat=ErrCode)

     return

   end subroutine read_saf_data

   subroutine get_saf_data(ncid, Saf)

     implicit none

     integer(kind=LONG), intent(in)  :: ncid
     type(type_Saf), intent(inout) :: Saf

     integer(kind=LONG) :: status

     ! note: ColFov(NcolFov), LinFov(NlinFov) are known before reading

     ! read values for global attributes
     ! instrument name
     status = nf90_get_att(ncid, NF90_GLOBAL, name="InsName", values=Saf%TopLevelSaf%header%InsName)
     if (status /= nf90_noerr) call handle_err(status)

     ! instrument mode
     status = nf90_get_att(ncid, NF90_GLOBAL, name="InsMode", values=Saf%TopLevelSaf%header%InsMode)
     if (status /= nf90_noerr) call handle_err(status)

     ! S/W version
     status = nf90_get_att(ncid, NF90_GLOBAL, name="SoftWareVersion", values=Saf%TopLevelSaf%header%SoftWareVersion)
     if (status /= nf90_noerr) call handle_err(status)

     ! Operator
     status = nf90_get_att(ncid, NF90_GLOBAL, name="Operator", values=Saf%TopLevelSaf%header%Operator)
     if (status /= nf90_noerr) call handle_err(status)

     ! Processing step
     status = nf90_get_att(ncid, NF90_GLOBAL, name="ProcessingStep", values=Saf%TopLevelSaf%header%ProcessingStep)
     if (status /= nf90_noerr) call handle_err(status)

     ! Conf ID
     status = nf90_get_att(ncid, NF90_GLOBAL, name="ConfId", values=Saf%TopLevelSaf%header%ConfId)
     if (status /= nf90_noerr) call handle_err(status)

     return

   end subroutine get_saf_data

end module saf_type
