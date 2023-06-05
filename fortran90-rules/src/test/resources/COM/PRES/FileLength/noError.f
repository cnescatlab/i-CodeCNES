! $Id: ropp_1dvar_iono.f90 4010 2014-01-10 11:07:40Z idculv $
!****m* Modules/ropp_1dvar_iono *
!
! NAME
!    ropp_1dvar_iono - Interface module for the ropp_1dvar direct_ion feature.
!
! SYNOPSIS
!    USE ropp_1dvar_iono
! 
! DESCRIPTION
!    This module provides interfaces for some "ionospheric" routines contained
!    in the ROPP 1DVar library.
!
! NOTES
!
! SEE ALSO
!
! AUTHOR
!   Met Office, Exeter, UK.
!   Any comments on this software should be given via the ROM SAF
!   Helpdesk at http://www.romsaf.org
!
! COPYRIGHT
!   (c) EUMETSAT. All rights reserved.
!   For further details please refer to the file COPYRIGHT
!   which you should have received as part of this distribution.
!
!****
MODULE ropp_1dvar_iono
!-------------------------------------------------------------------------------
! 1. Repacking routines
!-------------------------------------------------------------------------------
  INTERFACE ropp_1dvar_iono_repack
    SUBROUTINE ropp_1dvar_iono_repack_bangle(obs_data, obs, config)
      USE ropp_io_types
      USE ropp_1dvar_types
      TYPE(ROprof),      INTENT(inout)      :: obs_data
      TYPE(Obs1dBangle), INTENT(inout)      :: obs
      TYPE(VarConfig),   INTENT(in)         :: config
    END SUBROUTINE ropp_1dvar_iono_repack_bangle
    SUBROUTINE ropp_1dvar_iono_repack_bg(bg_data, bg, config)
      USE ropp_io_types
      USE ropp_1dvar_types
      TYPE(ROprof),      INTENT(inout)      :: bg_data
      TYPE(State1dFM),   INTENT(inout)      :: bg
      TYPE(VarConfig),   INTENT(in)         :: config
    END SUBROUTINE ropp_1dvar_iono_repack_bg
  END INTERFACE
!-------------------------------------------------------------------------------
! 2. Unpacking routines
!-------------------------------------------------------------------------------
  INTERFACE ropp_1dvar_iono_unpack
    SUBROUTINE ropp_1dvar_iono_unpack_bangle(res_data)
      USE ropp_io_types
      TYPE(ROprof), INTENT(inout)           :: res_data
    END SUBROUTINE ropp_1dvar_iono_unpack_bangle
  END INTERFACE
END MODULE ropp_1dvar_iono
