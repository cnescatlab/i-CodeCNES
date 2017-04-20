! instruments.f90 --
!
!           Project: 4A/OP
!           Version: $Revision: 1.5 $
! Last modification: $Date: 2003/03/13 08:49:53 $
!
!         Standards: CNES
!
! (c) CNES - LMD (CNRS/ENS) - NOVELTIS
!
! --------


! instruments -- Module
!
! * Purpose
!   This module contains the instrument identifiers 
!   required for the convolution computations.
!
!

module instruments

  use precision_type
  implicit none

  ! Radiometer with n channels (Meteosat, HIRS, ...)
  integer(kind=LONG), public, parameter :: CONV_RADIOMETER = 1
 
  ! Interferometer/spectrometer with a constant sampling
  ! step in wave number (IASI: step = 0.25 cm-1)
  integer(kind=LONG), public, parameter :: CONV_INTERF_CS  = 2

  ! Interferometer/spectrometer with a non constant sampling
  ! step and fixed wave numbers (AIRS, ...) 
  integer(kind=LONG), public, parameter :: CONV_INTERF_NCS = 3

end module instruments
