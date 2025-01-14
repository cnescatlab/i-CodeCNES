C-----------------------------------------------------------------------
C
C    Title         : CEARSAT
C
C    Function      : Give the Earth-sat position in the Earth Equatorial CS
C
C    Author        : Christelle Crozat
C
C    Date          : 27/OCT/1998
C
C    Update record :
C
! Date       Name          Sar No.  Change made
!-----------------------------------------------------------------------
! 12/04/00   M.Rodenhuis  ---       Made CPOS calculation more precise
! xx-Nov-07  S.Kranz    PEM V4.0    Merged PEM V3.0.2 with PEM KT
!-----------------------------------------------------------------------
!
	SUBROUTINE CEARSAT
!-----------------------------------------------------------------------
	IMPLICIT REAL*8(A-H,O-Z)
!-----------------------------------------------------------------------
! COMMON DATA
	INCLUDE 'SM_PEM_COMMON.LC' ! Position/Environment data
!     Calculates common block data: 
!               CPOSX, CPOSY, CPOSZ, CPOSR, CPOSUV      
!               CVELX, CVELY, CVELZ, CVELR, CVELUV      
!-----------------------------------------------------------------------
! LOCAL DATA
	REAL*8 VECT(3)
	REAL*8 VECT1(3)
!-----------------------------------------------------------------------
C BEGIN
!     MR: Changed the way CPOS is calculated. If the orbit is around the Earth,
!     it is more precise to calculate CPOS directly from CBXUV:
	IF (BARYC .EQ. 3) THEN
	  VECT(1) = CBXDIST*CBXUV(1)
	  VECT(2) = CBXDIST*CBXUV(2)
	  VECT(3) = CBXDIST*CBXUV(3)
	ELSE
	  DO J=1,3
		VECT(J) = -(SUN_PLANET(J,3) + SAT_SUN(J))
	  END DO
	END IF
!-----------------------------------------------------------------------
!     Give the vector from the ecliptic frame to the equatorial frame
	CALL CSYSCHANGE(VECT, VECT1, 1)
!-----------------------------------------------------------------------
	CPOSX = VECT1(1)
	CPOSY = VECT1(2)
	CPOSZ = VECT1(3)
!-----------------------------------------------------------------------
!     Give the unit vector in the Earth Equatorial frame of Earth-Sat vector
	CALL SVUNIT(VECT1, CPOSUV, CPOSR)
!-----------------------------------------------------------------------
!     Calculates the velocity in equatorial system if satellite orbits around Earth
	IF (BARYC .EQ. 3) THEN
	  VECT(1) = CBXVELX
	  VECT(2) = CBXVELY
	  VECT(3) = CBXVELZ
!-----------------------------------------------------------------------
	  CALL CSYSCHANGE(VECT, VECT1, 1)
	  CVELX = VECT1(1)
	  CVELY = VECT1(2)
	  CVELZ = VECT1(3)
!-----------------------------------------------------------------------
	  CALL SVUNIT(VECT1, CVELUV, CVELR)
	END IF
!-----------------------------------------------------------------------
	RETURN
!-----------------------------------------------------------------------
	END