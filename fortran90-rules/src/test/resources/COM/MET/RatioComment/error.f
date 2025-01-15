	SUBROUTINE CEARSAT
!     Calculates the velocity in equatorial system if satellite orbits around Earth
	IF (BARYC .EQ. 3) THEN
		VECT(1) = CBXVELX
		VECT(2) = CBXVELY
		VECT(3) = CBXVELZ
		CALL CSYSCHANGE(VECT, VECT1, 1)
		CVELX = VECT1(1)
		CVELY = VECT1(2)
		CVELZ = VECT1(3)
		CALL SVUNIT(VECT1, CVELUV, CVELR)
	  END IF
	  RETURN
  
	  END