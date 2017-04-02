! error_type.f90 --
!
!           Project: SPS_GENERIC
!           Authors: NOVELTIS/B.TOURNIER
!              Date: october 2009
!           Version: $Revision: 1.1 $
! Last modification: $Date: 2010-01-11 09:43:06 $
!


! error -- Module
!
! * Purpose
! 
!   Error handling module.
!
! * Parameters
!     * Err_ok, Err_error: Parameters to help keeping track of error state.
!                          Each time an error can occur in a subprogram an
!                          out variable (integer) must get one of these
!                          values to notify the caller of the subprogram.
!
! * Sub-routines and functions
!     * Err_setErrorPosition
!     * Err_incrErrorPosition
!     * Err_notifyError
!     * Err_outputErrorMessage
!

module error_type

  implicit none

  private

  integer, public, parameter   :: &
       Err_ok = 0,                &
       Err_error = 1

  public ::                   &
       Err_setErrorPosition,  &
       Err_incrErrorPosition, &
       Err_notifyError,       &
       Err_outputErrorMessage

  integer, public                   :: ios
  integer, parameter, public        :: Err_maxStrLength = 256
  integer                           :: errorPosition
  character(len = Err_maxStrLength) :: errorMessageStr
  character(len = Err_maxStrLength) :: functionStr

contains


  ! Err_setErrorPosition -- Public
  !
  ! * Purpose
  !   Set the virtual position of the error in the source file. The values of
  !   the virtual position are up to the user of the error module. For example
  !   they can be set to match numerical stamps in source code comments.
  !
  ! * Inputs
  !     * theErrorPosition: The error position in the source file.
  !

  subroutine Err_setErrorPosition (theErrorPosition)

    integer, intent(in) :: theErrorPosition

    errorPosition = theErrorPosition

    return
  end subroutine Err_setErrorPosition


  ! Err_incrErrorPosition -- Public
  !
  ! * Purpose
  !   Increment the virtual position of the error in the source file.
  !

  subroutine Err_incrErrorPosition

    errorPosition = errorPosition + 1

    return
  end subroutine Err_incrErrorPosition


  ! Err_notifyError -- Public
  !
  ! * Purpose
  !   Notify an error with a message and the name of the corresponding
  !   sub-routine.
  !
  ! * Inputs
  !     * theErrorMessageStr: The string that contains the message. Only the
  !                           256 first characters are used.
  !     * theFunctionStr:     The string that contains the name of the function.
  !                           Only the 256 first characters are used.
  !

  subroutine Err_notifyError (theErrorMessageStr, theFunctionStr)

    character(len = *), intent(in) :: theErrorMessageStr
    character(len = *), intent(in) :: theFunctionStr

    errorMessageStr = theErrorMessageStr
    functionStr = theFunctionStr

    return
  end subroutine Err_notifyError


  ! Err_outputErrorMessage -- Public
  !
  ! * Purpose
  !   Output, to a given logical unit number, lines of text that contain
  !   the name of the function, the error message and the error position.
  !
  ! * Inputs
  !     * theLun: The logical unit number
  !

  subroutine Err_outputErrorMessage (theLun)

    integer, intent(in) :: theLun

    write (theLun, *) 'Error in: ' // functionStr(1:len_trim(functionStr))
    write (theLun, *) '  ' // errorMessageStr(1:len_trim(errorMessageStr))
    write (theLun, *) '  error position: ', errorPosition

    return
    end subroutine Err_outputErrorMessage

end module error_type
