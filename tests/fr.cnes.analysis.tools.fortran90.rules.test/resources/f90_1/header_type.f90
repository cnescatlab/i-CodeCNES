!!* header_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/J.DONNADILLE
!!*              Date: july 2013
!!
!> header_type -- Module
!!
!! * Purpose
!!
!!   Module for type header parameters declaration.
!!
!! * Sub-routines and functions
!!
!!     * type_header : type for declaration of header 
!!
!! * References
!!

module header_type
   use precision_type
   use error_type
!
   implicit none
!
!
   public :: type_header, &
             read_header
!
   type :: type_header
     character(len=500) :: filename !< spectrum file name
     character(len=10)  :: InsName !< instrument name
     character(len=20)  :: InsMode !< instrument mode
     character(len=4)   :: SoftWareVersion !< software version identifier
     character(len=3)   :: Operator !< name of the operator
     character(len=30)  :: ProcessingStep !< name of the processing step into the operator
     character(len=200) :: ConfId !< operator configuration file identifier
   end type type_header
!
!
contains
 !> header_type -- Module
!!
!! * Purpose
!!
!!   Module for type header parameters declaration.
!!
!! * Sub-routines and functions
!!
!!     * File_Header : character / input file with definition of reading parameters
!!     * type_header : type for declaration of header
!!
!! * References
!!
 subroutine read_header ( File_Header, header )
  character(len=500), intent(in)   :: File_Header
  type(type_header), intent(inout) :: header
  integer(kind=LONG) :: iFile
  integer(kind=LONG) :: iPos
  !
  iFile = 10
  ! reading parameters
  open(unit=iFile, file=File_Header, status='old', err=1999)
     iPos = 1
     read(iFile,fmt='(a)',err=999) header%InsName
     iPos = 2
     read(iFile,fmt='(a)',err=999) header%InsMode
     iPos = 3
     read(iFile,fmt='(a)',err=999) header%SoftWareVersion
     iPos = 4
     read(iFile,fmt='(a)',err=999) header%ConfId
  close( iFile )
  return
1999 write(0,*) 'error while opening file ', File_Header(1:len_trim(File_Header))
     call exit(1)
999  write(0,*) 'error while reading file ', File_Header(1:len_trim(File_Header))
     write(0,*) 'fatal error subroutine read_header', iPos
     call exit(1)
   end subroutine read_header
 end module header_type
