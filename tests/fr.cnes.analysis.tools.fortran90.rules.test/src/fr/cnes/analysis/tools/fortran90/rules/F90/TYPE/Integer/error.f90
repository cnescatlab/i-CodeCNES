MODULE ma_precision

	integer, parameter :: DOUBLE = selected_real_kind(15)
	integer, parameter :: SIMPLE = selected_real_kind(6)
	integer, parameter :: ENTIER
	CHARACTER *128	   :: adresse
	CHARACTER *32	   :: nom_station
	
	REAL			   :: precision
	COMPLEX			   :: vent
	DOUBLE PRECISION   :: altitude
	
	integer, parameter :: ENTIER1

END MODULE ma_precision