!!* apf_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: october 2009
!!*           Version: $Revision: 1.6 $
!!* Last modification:  $Date: 2011-06-22 10:06:27 $
!!
!> apf_type -- Module
!!
!! * Purpose
!!
!!   Module for type apf declaration and allocation.
!!
!! * Description
!!      
!!     This module defines the apf type and allows its allocation and declaration. 
!!
!! * Sub-routines and functions
!!
!!     * type_Apf   : type for declaration and allocation of apf
!!     * alloc_Apf  : type_Apf allocation
!!     * dalloc_Apf : type_Apf deallocation
!!     * Apf_Base   : computes Apf base in Opd
!!     * Apf_Header_Transfer : header transfer between to Apf types
!!     * readapf    : type_Apf reading routine
!!     * writeapf   : type_Apf writing routine
!!     * writeapf_netcdf : type_Apf writing routine (netCDF format)
!!     * write_apf_data  : type_Apf writing routine (netCDF format)
!!     * init_apf_data   : creation and initialization of the netcdf file
!!     * readapf_netcdf  : type_Apf reading routine (netCDF format)
!!     * read_apf_data   : type_Apf reading routine (netCDF format)
!!     * get_apf_data    : get global data from the netcdf file
!!
!! * References
!!
!! Status:
!! modified 29.07.2013: (JD) writing routine netCDF



module apf_type
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
   public :: type_Apf      &
            ,type_Apf_ST   &
            ,type_Apf_SB   &
            ,type_Apf_PN   &
            ,alloc_Apf     &
            ,alloc_Apf_ST  &
            ,alloc_Apf_SB  &
            ,alloc_Apf_PN  &
            ,dalloc_Apf    &
            ,dalloc_Apf_ST &
            ,dalloc_Apf_SB &
            ,dalloc_Apf_PN &
            ,Apf_Base      &
            ,Apf_Header_Transfer&
            ,readapf            &
            ,writeapf           &
            ,writeapf_netcdf    &
            ,readapf_netcdf
!
   type :: type_Apf
     type(type_isrf_index)                              :: TopLevelApf ! < apf top level indexes
     character(len=500)                                 :: filename !< apodisation function characterisation file name
     integer(kind=LONG)                                 :: NsPixel !<  pixels number
     integer(kind=LONG)                                 :: NsWn0 !< frequencies samples number
     integer(kind=LONG)                                 :: NsOpd !< OPD samples number
     real(kind=DOUBLE)                                  :: OpdMax !< maximum optical path difference (m)
     real(kind=DOUBLE)                                  :: dOpd !< optical path difference sampling (m)
     real(kind=DOUBLE)   ,dimension(:)    ,allocatable  :: Wn0 !< reference frequency
     real(kind=DOUBLE)   ,dimension(:)    ,allocatable  :: Opd !< optical path difference (m)
     real(kind=DOUBLE)   ,dimension(:)    ,allocatable  :: L1cTF !< apodisation function
     real(kind=DOUBLE)   ,dimension(:,:,:),allocatable  :: Mod !< despatial function modulus
     real(kind=DOUBLE)   ,dimension(:,:,:),allocatable  :: Arg !< despatial function argument
     real(kind=DOUBLE)   ,dimension(:,:)  ,allocatable  :: Nrf !< noise reduction factor
  end type type_Apf
!
!
   type :: type_Apf_ST
     integer(kind=LONG)                                   :: NbState
     type(type_Apf),dimension(:),allocatable              :: Apf
     !dimension(NbState)
   end type type_Apf_ST
!
!
   type :: type_Apf_PN
     integer(kind=LONG)                                   :: NbPixel
     type(type_Apf_ST),dimension(:),allocatable           :: Apf_ST
     !dimension(NbPixel)
   end type type_Apf_PN
!
!
   type :: type_Apf_SB
     integer(kind=LONG)                                   :: Nband
     type(type_Apf_PN),dimension(:),allocatable           :: Apf_PN
     !dimension(Nband)
   end type type_Apf_SB
!
!
   contains
!
!
   subroutine alloc_Apf_ST( Apf_ST )
   implicit none
     type(type_Apf_ST), intent(inout)              :: Apf_ST
     integer(kind=LONG)                            :: ErrCode
!
     allocate( Apf_ST%Apf(Apf_ST%NbState),  stat=ErrCode )
     if( ErrCode > 0 ) then
        write(*,*) 'allocation Apf_ST Error',ErrCode
        write(*,*) 'type_Apf_ST: fatal error'
        call exit(1)
     end if
     return
   end subroutine alloc_Apf_ST
!
!
   subroutine alloc_Apf_PN( Apf_PN )
   implicit none
     type(type_Apf_PN), intent(inout)              :: Apf_PN
     integer(kind=LONG)                            :: ErrCode
!
     allocate( Apf_PN%Apf_ST(Apf_PN%NbPixel),  stat=ErrCode )
     if( ErrCode > 0 ) then
        write(*,*) 'allocation Apf_PN Error',ErrCode
        write(*,*) 'type_Apf_PN: fatal error'
        call exit(1)
     end if
     return
   end subroutine alloc_Apf_PN
!
!
   subroutine alloc_Apf_SB( Apf_SB )
   implicit none
     type(type_Apf_SB), intent(inout)              :: Apf_SB
     integer(kind=LONG)                            :: ErrCode
!
     allocate( Apf_SB%Apf_PN(Apf_SB%Nband),  stat=ErrCode )
     if( ErrCode > 0 ) then
        write(*,*) 'allocation Apf_SB Error',ErrCode
        write(*,*) 'type_Apf_SB: fatal error'
        call exit(1)
     end if
     return
   end subroutine alloc_Apf_SB
!
!
   subroutine alloc_Apf( Apf )
   implicit none
     type(type_Apf)    , intent(inout)      :: Apf
     integer(kind=LONG)                     :: ErrCode, i
!
     allocate( Apf%TopLevelApf%SubFov(Apf%NsPixel),        stat=ErrCode )
     ! Apf%TopLevelApf%SubFov is the list of sub-pixel index (1:NsPixel)
     do i=1,Apf%NsPixel
        Apf%TopLevelApf%SubFov(i) = i
     end do
     allocate( Apf%Wn0(Apf%NsWn0),                         stat=ErrCode )
     allocate( Apf%Opd(Apf%NsOpd),                         stat=ErrCode )
     allocate( Apf%L1cTF(Apf%NsOpd),                       stat=ErrCode )
     allocate( Apf%Mod(Apf%NsOpd,Apf%NsWn0,Apf%NsPixel+1), stat=ErrCode )
     allocate( Apf%Arg(Apf%NsOpd,Apf%NsWn0,Apf%NsPixel+1), stat=ErrCode )
     allocate( Apf%Nrf(Apf%NsWn0,Apf%NsPixel+1),           stat=ErrCode )
!
     if (ErrCode > 0) then
        write(0,*) 'allocation Apf Error'
        write(0,*) 'Apf: fatal error'
        call Err_outputErrorMessage(0)
        call exit(1)
     end if
     return
   end subroutine alloc_Apf
!
!
   subroutine dalloc_Apf_ST( Apf_ST )
   implicit none
     type(type_Apf_ST), intent(inout)              :: Apf_ST
!
     deallocate( Apf_ST%Apf )
     return
   end subroutine dalloc_Apf_ST
!
!
   subroutine dalloc_Apf_PN( Apf_PN )
   implicit none
     type(type_Apf_PN), intent(inout)              :: Apf_PN
!
     deallocate( Apf_PN%Apf_ST )
     return
   end subroutine dalloc_Apf_PN
!
!
   subroutine dalloc_Apf_SB( Apf_SB )
   implicit none
     type(type_Apf_SB), intent(inout)              :: Apf_SB
!
     deallocate( Apf_SB%Apf_PN )
     return
   end subroutine dalloc_Apf_SB
!
!
   subroutine dalloc_Apf( Apf )
   implicit none
     type(type_Apf)    , intent(inout)         :: Apf
!
     deallocate( Apf%TopLevelApf%SubFov )
     deallocate( Apf%Wn0 )
     deallocate( Apf%Opd )
     deallocate( Apf%L1cTF )
     deallocate( Apf%Mod )
     deallocate( Apf%Arg )
     deallocate( Apf%Nrf )
     return
   end subroutine dalloc_Apf
!
!
   subroutine Apf_Base( Apf )
   implicit none
     type(type_Apf)    , intent(inout)                :: Apf
     integer(kind=LONG)                               :: NsOpd0
     integer(kind=LONG)                               :: Ns
!
     NsOpd0   = int( Apf%NsOpd/2 ) + 1
     Apf%Opd(1:Apf%NsOpd) = (/ (dble(Ns-NsOpd0),Ns=1,Apf%NsOpd) /) *  Apf%dOpd
!
     return
   end subroutine Apf_Base
!
!
   subroutine Apf_Header_Transfer( Apf_In, Apf_Out )
   implicit none
     type(type_Apf)    , intent(in)                   :: Apf_In
     type(type_Apf)    , intent(inout)                :: Apf_Out
!
     Apf_Out%NsPixel = Apf_In%NsPixel
     Apf_Out%NsWn0   = Apf_In%NsWn0
     Apf_Out%NsOpd   = Apf_In%NsOpd
     call alloc_Apf( Apf_Out )
     Apf_Out = Apf_In
     Apf_Out%L1cTF = 0.d+00
     Apf_Out%Mod = 0.d+00
     Apf_Out%Arg = 0.d+00
     Apf_Out%Nrf = 0.d+00
!
     return
   end subroutine Apf_Header_Transfer
!
!
   subroutine readapf( Apf,iostatus )
   implicit none
     type(type_Apf)    , intent(inout) :: Apf
!
     integer(kind=LONG), intent(out)   :: iostatus
     integer(kind=DOUBLE)              :: ifile
     integer(kind=LONG)                :: offset
     integer(kind=LONG)                :: Type
     integer(kind=LONG)                :: Size
!
     iostatus = 0
     call open_file_r(Apf%filename                    &
                      (1:len_trim(Apf%filename))      &
                      // char(0), ifile)
     if ( ifile .eq. 0 ) then
        iostatus = 1
     else
        offset = 0
        Type = i4Type
        Size = 4
        call read_field( ifile, Apf%NsPixel,                  &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Apf%NsWn0,                    &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Apf%NsOpd,                    &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Type = r8Type
        Size = 8
        call read_field( ifile, Apf%OpdMax,                   &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Apf%dOpd,                     &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call alloc_Apf( Apf )
        Size = 8*Apf%NsWn0
        call read_field( ifile, Apf%Wn0,                      &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Apf%NsOpd
        call read_field( ifile, Apf%Opd,                      &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Apf%L1cTF,                    &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Apf%NsOpd*Apf%NsWn0*(Apf%NsPixel+1)
        call read_field( ifile, Apf%Mod,                      &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Apf%Arg,                      &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Apf%NsWn0*(Apf%NsPixel+1)
        call read_field( ifile, Apf%Nrf,                      &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call close_file(Apf%filename              &
                        (1:len_trim(Apf%filename))&
                        // char(0), ifile)
     end if
     return
   end subroutine readapf
!
!
   subroutine writeapf( Apf,iostatus )
   implicit none
     type(type_Apf)    , intent(in) :: Apf
!
     integer(kind=LONG), intent(out) :: iostatus
     integer(kind=DOUBLE)            :: ifile
     integer(kind=LONG)              :: offset
     integer(kind=LONG)              :: Type
     integer(kind=LONG)              :: Size
!
     iostatus = 0
     call open_file_w(Apf%filename                    &
                      (1:len_trim(Apf%filename))      &
                      // char(0), ifile)
     if ( ifile .eq. 0 ) then
        iostatus = 1
     else
        offset = 0
        Type = i4Type
        Size = 4
        call write_field( ifile, Apf%NsPixel,                  &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Apf%NsWn0,                    &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Apf%NsOpd,                    &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Type = r8Type
        Size = 8
        call write_field( ifile, Apf%OpdMax,                   &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Apf%dOpd,                     &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Apf%NsWn0
        call write_field( ifile, Apf%Wn0,                      &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Apf%NsOpd
        call write_field( ifile, Apf%Opd,                      &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Apf%L1cTF,                    &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Apf%NsOpd*Apf%NsWn0*(Apf%NsPixel+1)
        call write_field( ifile, Apf%Mod,                      &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Apf%Arg,                      &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Apf%NsWn0*(Apf%NsPixel+1)
        call write_field( ifile, Apf%Nrf,                      &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call close_file(Apf%filename              &
                        (1:len_trim(Apf%filename))&
                        // char(0), ifile)
     end if
     return
   end subroutine writeapf
!
!
!>  writeapf_netcdf -- Public
!!
!! * Purpose
!!
!!     Write apf type
!!
!! * Description
!!
!!     Write apf type
!!
!! * Inputs
!!
!!     - Apf  : type_apf / type for declaration and allocation of apf parameters
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
subroutine writeapf_netcdf( Apf,iostatus, init_file )

     implicit none

     type(type_Apf), intent(in) :: Apf
     integer(kind=LONG), intent(out) :: iostatus
     integer(kind=SHORT), intent(inout) :: init_file

     integer(kind=LONG) :: ncid, status, i_band

     iostatus = 0

     ! file creation and definition
     if (init_file == 0) then
        call init_apf_data(Apf, ncid)
        init_file = 1
     endif

     ! open netCDF dataset
     status = nf90_open(path = trim(Apf%TopLevelApf%header%filename), mode = nf90_write, ncid = ncid)
     if (status /= nf90_noerr) call handle_err(status)

     ! writes variables for apf group
     ! current spectral band
     i_band = Apf%TopLevelApf%Band
     call write_apf_data(ncid, i_band, Apf)

     ! close access to netCDF dataset
     status = nf90_close(ncid)
     if (status /= nf90_noerr) call handle_err(status)

     return

   end subroutine writeapf_netcdf
   !
   !
   subroutine init_apf_data(Apf, ncid)

     implicit none

     type(type_Apf), intent(in) :: Apf
     integer(kind=LONG), intent(out) :: ncid

     integer(kind=LONG) :: status
     integer(kind=LONG) :: varid, dimid

     ! create netCDF dataset
     status = nf90_create(path = trim(Apf%TopLevelApf%header%filename), cmode = NF90_NETCDF4, ncid = ncid)
     if (status /= nf90_noerr) call handle_err(status)

     ! define and write global variables

     !
     ! ColFov(NcolFov)
     status = nf90_def_dim(ncid, "NcolFov", Apf%TopLevelApf%NcolFov, dimid)
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_def_var(ncid, "ColFov", NF90_INT, (/dimid/), varid)
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_att(ncid, varid, "units", "N.A.")
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_att(ncid, varid, "long_name", "column Fov value")
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_var(ncid, varid, Apf%TopLevelApf%ColFov)
     if (status /= nf90_noerr) call handle_err(status)
     ! LinFov(NlinFov)
     status = nf90_def_dim(ncid, "NlinFov", Apf%TopLevelApf%NlinFov, dimid)
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_def_var(ncid, "LinFov", NF90_INT, (/dimid/), varid)
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_att(ncid, varid, "units", "N.A.")
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_att(ncid, varid, "long_name", "line Fov value")
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_var(ncid, varid, Apf%TopLevelApf%LinFov)
     if (status /= nf90_noerr) call handle_err(status)

     ! define global attributes
     ! filename
     status = nf90_put_att(ncid, NF90_GLOBAL, name = "filename", values = trim(Apf%TopLevelApf%header%filename))
     if (status /= nf90_noerr) call handle_err(status)
     ! instrument name
     status = nf90_put_att(ncid, NF90_GLOBAL, name = "InsName", values = Apf%TopLevelApf%header%InsName)
     if (status /= nf90_noerr) call handle_err(status)
     ! instrument mode
     status = nf90_put_att(ncid, NF90_GLOBAL, name = "InsMode", values = Apf%TopLevelApf%header%InsMode)
     if (status /= nf90_noerr) call handle_err(status)
     ! S/W version
     status = nf90_put_att(ncid, NF90_GLOBAL, name = "SoftWareVersion", values = Apf%TopLevelApf%header%SoftWareVersion)
     if (status /= nf90_noerr) call handle_err(status)
     ! Operator
     status = nf90_put_att(ncid, NF90_GLOBAL, name = "Operator", values = Apf%TopLevelApf%header%Operator)
     if (status /= nf90_noerr) call handle_err(status)
     ! Processing step
     status = nf90_put_att(ncid, NF90_GLOBAL, name = "ProcessingStep", values = Apf%TopLevelApf%header%ProcessingStep)
     if (status /= nf90_noerr) call handle_err(status)
     ! Conf ID
     status = nf90_put_att(ncid, NF90_GLOBAL, name = "ConfId", values = trim(Apf%TopLevelApf%header%ConfId))
     if (status /= nf90_noerr) call handle_err(status)

     ! close access to netCDF dataset
     status = nf90_close(ncid)
     if (status /= nf90_noerr) call handle_err(status)

     return

   end subroutine init_apf_data

   subroutine write_apf_data(ncid, band, Apf)

     implicit none

     integer(kind=LONG), intent(in)  :: ncid, band
     type(type_Apf), intent(in) :: Apf

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
     nsopd = Apf%NsOpd
     ! NsWn0
     nswn0 = Apf%NsWn0

     ! start index
     ! NlinFov,NcolFov,NsubFov,NsWn0,NsOpd
     start_w = (/1,1,1,1,1/)
     ! count index
     count_w = (/1,1,Apf%TopLevelApf%NSubfov,nswn0,nsopd/)

     ! NlinFov,NcolFov,NsWn0,NsOpd
     start_w_agg = (/1,1,1,1/)
     ! count index
     count_w_agg = (/1,1,nswn0,nsopd/)

     ! NlinFov,NcolFov,NsubFov,NsWn0
     start_w0 = (/1,1,1,1/)
     ! count index
     count_w0 = (/1,1,Apf%TopLevelApf%NSubfov,nswn0/)

     ! NlinFov,NcolFov,NsWn0
     start_w0_agg = (/1,1,1/)
     ! count index
     count_w0_agg = (/1,1,nswn0/)

     ! definition of the group (one group for one spectral band)
     write(band_str,'(i1)') band

     ! check the group existence
     flag_grp = nf90_inq_ncid(ncid, 'apf_sb'//band_str, grpid)

     if (flag_grp /= nf90_noerr) then
        ! group creation and initialization
        status = nf90_def_grp(ncid, 'apf_sb'//band_str, grpid)
        if (status /= nf90_noerr) call handle_err(status)

        ! definition of the dimensions
        ! NsWn0
        status = nf90_def_dim(grpid, "NsWn0", nswn0, nsw_dimid)
        if (status /= nf90_noerr) call handle_err(status)
        ! NsOpd
        status = nf90_def_dim(grpid, "NsOpd", nsopd, nso_dimid)
        if (status /= nf90_noerr) call handle_err(status)
        ! NsubFov
        status = nf90_def_dim(grpid, "NsubFov", Apf%TopLevelApf%NsubFov, nsf_dimid)
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
        ! L1cTF
        call write_def_variable(grpid, "L1cTF", NF90_DOUBLE, (/nso_dimid/), "N.A.",&
                                "spectral response function level 1c in Fourier Space")
        ! Mod
        call write_def_variable(grpid, "Mod", NF90_DOUBLE, (/dimids(2),dimids(1),nsf_dimid,nsw_dimid,nso_dimid/),&
                                 "N.A.", "apodisation function modulus")
        ! Mod_agg
        call write_def_variable(grpid, "Mod_agg", NF90_DOUBLE, (/dimids(2),dimids(1),nsw_dimid,nso_dimid/),&
                                 "N.A.", "aggregated apodisation function modulus")
        ! Arg
        call write_def_variable(grpid, "Arg", NF90_DOUBLE, (/dimids(2),dimids(1),nsf_dimid,nsw_dimid,nso_dimid/),&
                                 "rad", "apodisation function argument")
        ! Arg_agg
        call write_def_variable(grpid, "Arg_agg", NF90_DOUBLE, (/dimids(2),dimids(1),nsw_dimid,nso_dimid/),&
                                 "rad", "aggregated apodisation function argument")
        ! Nrf
        call write_def_variable(grpid, "Nrf", NF90_DOUBLE, (/dimids(2),dimids(1),nsf_dimid,nsw_dimid/),&
                                 "rad", "noise reduction factor")
        ! Nrf_agg
        call write_def_variable(grpid, "Nrf_agg", NF90_DOUBLE, (/dimids(2),dimids(1),nsw_dimid/),&
                                 "N.A.", "aggregated noise reduction factor")

        ! write group attributes
        ! Band
        status = nf90_put_att(grpid, NF90_GLOBAL, name= "Band", values = band)
        if (status /= nf90_noerr) call handle_err(status)
        ! OpdMax
        status = nf90_put_att(grpid, NF90_GLOBAL, name= "OpdMax", values = Apf%OpdMax)
        if (status /= nf90_noerr) call handle_err(status)
        ! dOpd
        status = nf90_put_att(grpid, NF90_GLOBAL, name= "dOpd", values = Apf%dOpd)
        if (status /= nf90_noerr) call handle_err(status)

   end if ! end group creation and initialization

     ! write variables
     ! Apf%TopLevelApf%SubFov is the list of sub-pixel index
     status = nf90_inq_varid(grpid, "SubFov", varid)
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_var(grpid, varid, Apf%TopLevelApf%SubFov, start=(/1/), count=(/Apf%TopLevelApf%NSubFov/))
     if (status /= nf90_noerr) call handle_err(status)

     ! Wn0
     call write_variable(grpid, "Wn0", Apf%Wn0)
     ! Opd
     call write_variable(grpid, "Opd", Apf%Opd)
     ! L1cTF
     call write_variable(grpid, "L1cTF", Apf%L1cTF)
     ! Mod
     call write_variable(grpid, "Mod", Apf%Mod(:,:,1:Apf%NsPixel), start_w, count_w)
     ! Mod_agg
     call write_variable(grpid, "Mod_agg", Apf%Mod(:,:,Apf%NsPixel+1), start_w_agg, count_w_agg)
     ! Arg
     call write_variable(grpid, "Arg", Apf%Arg(:,:,1:Apf%NsPixel), start_w, count_w)
     ! Arg_agg
     call write_variable(grpid, "Arg_agg", Apf%Arg(:,:,Apf%NsPixel+1), start_w_agg, count_w_agg)
     ! Nrf
     call write_variable(grpid, "Nrf", Apf%Nrf(:,1:Apf%NsPixel), start_w0, count_w0)
     ! Nrf_agg
     call write_variable(grpid, "Nrf_agg", Apf%Nrf(:,Apf%NsPixel+1), start_w0_agg, count_w0_agg)

     return

   end subroutine write_apf_data
!
!
!>  readapf_netcdf -- Public
!!
!! * Purpose
!!
!!     Read apf type
!!
!! * Description
!!
!!     Read apf type
!!
!! * Inputs
!!
!!     - Apf  : type_apf / type for declaration and allocation of apf parameters
!!
!! * Outputs
!!
!!     - Apf  : type_apf / type for declaration and allocation of apf parameters
!!     - iostatus : integer / error management varaible
!!
!! * References
!!
!!   SPS
!!
   subroutine readapf_netcdf( Apf,iostatus )

     implicit none

     type(type_Apf), intent(inout) :: Apf
     integer(kind=LONG), intent(out) :: iostatus

     integer(kind=LONG) :: ncid, status, i_band

     iostatus = 0

     ! open existing netCDF dataset
     status = nf90_open(path = trim(Apf%TopLevelApf%header%filename), mode = nf90_nowrite, ncid = ncid)
     if (status /= nf90_noerr) call handle_err(status)

     ! get global data (variables and attributes)
     call get_apf_data(ncid, Apf)

     ! reads variables for apf group
     ! current spectral band
     i_band = Apf%TopLevelApf%Band
     call read_apf_data(ncid, i_band, Apf)

     ! close access to netCDF dataset
     status = nf90_close(ncid)
     if (status /= nf90_noerr) call handle_err(status)

     return

   end subroutine readapf_netcdf
!
!
subroutine read_apf_data(ncid, band, Apf)

     implicit none

     integer(kind=LONG), intent(in)    :: ncid, band
     type(type_Apf),     intent(inout) :: Apf

     integer(kind=LONG),dimension(5) :: start_w, count_w
     integer(kind=LONG),dimension(4) :: start_w_agg, count_w_agg, start_w0, count_w0
     integer(kind=LONG),dimension(3) :: start_w0_agg, count_w0_agg

     integer(kind=LONG) :: status
     integer(kind=LONG) :: grpid

     integer(kind=LONG) :: ndimGrp, nvarGrp, nattGrp, nvar
     integer(kind=LONG), dimension(:), allocatable :: varidsGrp
     integer(kind=LONG) :: ErrCode
     integer(kind=LONG) :: dimid_nsopd, dimid_nswn0
     character(len=1)   :: band_str

     ! get the group id
     write(band_str,'(i1)') band
     status = nf90_inq_ncid(ncid, 'apf_sb'//band_str, grpid)
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
     status = nf90_get_att(grpid, NF90_GLOBAL, name="OpdMax", values=Apf%OpdMax)
     if (status /= nf90_noerr) call handle_err(status)

     ! dOpd
     status = nf90_get_att(grpid, NF90_GLOBAL, name="dOpd", values=Apf%dOpd)
     if (status /= nf90_noerr) call handle_err(status)

     ! get NsWn0
     status = nf90_inq_dimid(grpid, "NsWn0", dimid_nswn0)
     if (status /= nf90_noerr) call handle_err(status)

     status = nf90_inquire_dimension(grpid, dimid_nswn0, len = Apf%NsWn0)
     if(status /= nf90_NoErr) call handle_err(status)

     ! get NsOpd
     status = nf90_inq_dimid(grpid, "NsOpd", dimid_nsopd)
     if (status /= nf90_noerr) call handle_err(status)

     status = nf90_inquire_dimension(grpid, dimid_nsopd, len = Apf%NsOpd)
     if(status /= nf90_NoErr) call handle_err(status)

     Apf%NsPixel = Apf%TopLevelApf%NsubFov
     ! Apf allocation
     call alloc_Apf( Apf )

     ! Wn0
     status = nf90_get_var(grpid, varidsGrp(2), Apf%Wn0)
     if (status /= nf90_noerr) call handle_err(status)

     ! Opd
     status = nf90_get_var(grpid, varidsGrp(3), Apf%Opd)
     if (status /= nf90_noerr) call handle_err(status)

     ! L1cTF
     status = nf90_get_var(grpid, varidsGrp(4), Apf%L1cTF)
     if (status /= nf90_noerr) call handle_err(status)

     ! NlinFov,NcolFov,NsubFov,NsWn0,NsOpd
     ! start index
     start_w = (/1,1,1,1,1/)
     ! count index
     count_w = (/1,1,Apf%TopLevelApf%NsubFov,Apf%NsWn0,Apf%NsOpd/)
     ! NlinFov,NcolFov,NsWn0,NsOpd
     ! start index
     start_w_agg = (/1,1,1,1/)
     ! count index
     count_w_agg = (/1,1,Apf%NsWn0,Apf%NsOpd/)
     ! NlinFov,NcolFov,NSubFov,NsWn0
     ! start index
     start_w0 = (/1,1,1,1/)
     ! count index
     count_w0 = (/1,1,Apf%TopLevelApf%NsubFov,Apf%NsWn0/)
     ! NlinFov,NcolFov,NsWn0
     ! start index
     start_w0_agg = (/1,1,1/)
     ! count index
     count_w0_agg = (/1,1,Apf%NsWn0/)

     ! Mod
     status = nf90_get_var(grpid, varidsGrp(5), Apf%Mod(:,:,1:Apf%NsPixel), start=start_w, count=count_w)
     if (status /= nf90_noerr) call handle_err(status)
     ! Mod_agg
     status = nf90_get_var(grpid, varidsGrp(6), Apf%Mod(:,:,Apf%NsPixel+1), start=start_w_agg, count=count_w_agg)
     if (status /= nf90_noerr) call handle_err(status)
     ! Arg
     status = nf90_get_var(grpid, varidsGrp(7), Apf%Arg(:,:,1:Apf%NsPixel), start=start_w, count=count_w)
     if (status /= nf90_noerr) call handle_err(status)
     ! Arg_agg
     status = nf90_get_var(grpid, varidsGrp(8), Apf%Arg(:,:,Apf%NsPixel+1), start=start_w_agg, count=count_w_agg)
     if (status /= nf90_noerr) call handle_err(status)
     ! Nrf
     status = nf90_get_var(grpid, varidsGrp(9), Apf%Nrf(:,1:Apf%NsPixel), start=start_w0, count=count_w0)
     if (status /= nf90_noerr) call handle_err(status)
     ! Nrf_agg
     status = nf90_get_var(grpid, varidsGrp(10), Apf%Nrf(:,Apf%NsPixel+1), start=start_w0_agg, count=count_w0_agg)
     if (status /= nf90_noerr) call handle_err(status)

     ! deallocate local variables
     deallocate(varidsGrp, stat=ErrCode)

     return

   end subroutine read_apf_data

   subroutine get_apf_data(ncid, Apf)

     implicit none

     integer(kind=LONG), intent(in)  :: ncid
     type(type_Apf), intent(inout) :: Apf

     integer(kind=LONG) :: status

     ! note: ColFov(NcolFov), LinFov(NlinFov) are known before reading

     ! read values for global attributes
     ! instrument name
     status = nf90_get_att(ncid, NF90_GLOBAL, name="InsName", values=Apf%TopLevelApf%header%InsName)
     if (status /= nf90_noerr) call handle_err(status)

     ! instrument mode
     status = nf90_get_att(ncid, NF90_GLOBAL, name="InsMode", values=Apf%TopLevelApf%header%InsMode)
     if (status /= nf90_noerr) call handle_err(status)

     ! S/W version
     status = nf90_get_att(ncid, NF90_GLOBAL, name="SoftWareVersion", values=Apf%TopLevelApf%header%SoftWareVersion)
     if (status /= nf90_noerr) call handle_err(status)

     ! Operator
     status = nf90_get_att(ncid, NF90_GLOBAL, name="Operator", values=Apf%TopLevelApf%header%Operator)
     if (status /= nf90_noerr) call handle_err(status)

     ! Processing step
     status = nf90_get_att(ncid, NF90_GLOBAL, name="ProcessingStep", values=Apf%TopLevelApf%header%ProcessingStep)
     if (status /= nf90_noerr) call handle_err(status)

     ! Conf ID
     status = nf90_get_att(ncid, NF90_GLOBAL, name="ConfId", values=Apf%TopLevelApf%header%ConfId)
     if (status /= nf90_noerr) call handle_err(status)

     return

   end subroutine get_apf_data

!
!
end module apf_type
