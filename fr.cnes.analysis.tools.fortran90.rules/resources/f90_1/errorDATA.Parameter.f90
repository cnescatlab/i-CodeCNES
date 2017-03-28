
      IMPLICIT NONE
      CHARACTER *128                             :: ADRESSE
      CHARACTER *32                              :: NOM_STATION
      REAL(selected_real_kind(15))               :: PRESSION
      REAL(selected_real_kind(15, 307))          :: ALTITUDE
      REAL(selected_real_kind(10, 30))           :: RAYONNEMENT
      COMPLEX(selected_real_kind(6))             :: VENT
      REAL(selected_real_kind(6))                :: TEMPERATURE
      INTEGER(selected_int_kind(9))              :: NIVEAU
      LOGICAL                                    :: STATUS  
      
END BLOCK DATA Bloc_Mesure

MODULE ma_precision
	INTEGER(selected_int_kind(9))              	 :: DOUBLE
END MODULE ma_precision

PROGRAM ESSAI

      IMPLICIT NONE
      CHARACTER *128                             :: ADRESSE
      CHARACTER *32                              :: NOM_STATION
      REAL(selected_real_kind(15))               :: PRESSION
      REAL(selected_real_kind(15, 307))          :: ALTITUDE
      REAL(selected_real_kind(10, 30))           :: RAYONNEMENT
      COMPLEX(selected_real_kind(6))             :: VENT
      REAL(selected_real_kind(6))                :: TEMPERATURE
      INTEGER(selected_int_kind(9))              :: NIVEAU

END PROGRAM ESSAI
      
