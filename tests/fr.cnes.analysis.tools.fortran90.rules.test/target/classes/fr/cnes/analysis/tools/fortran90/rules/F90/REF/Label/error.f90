!
! --- Le module 'ma_precision' defini ici permet de standardiser les types de variables
! --- utilises dans l'unite de programme avec leur presision associee 
!
MODULE ma_precision 

     integer, parameter :: DOUBLE = selected_real_kind(15)
     integer, parameter :: SIMPLE = selected_real_kind(6)
     integer, parameter :: LONG   = selected_int_kind(18)
     integer, parameter :: ENTIER = selected_int_kind(9)
     integer, parameter :: COURT  = selected_int_kind(4)

! !!! La regle 'Pr.UniteNommee' n'est pas respectee ici, le module devrait se terminer par 
! !!! 'END MODULE ma_precision' 
END 

!
! --- Le common 'Bloc_Mesure' defini ici contient tous les types d'informations 
! --- relatifs a une mesure : localisation, parametres, ...
!
BLOCK DATA Bloc_Mesure

      USE ma_precision
      IMPLICIT NONE

      CHARACTER *128   ADRESSE
      CHARACTER *32    NOM_STATION
      REAL(DOUBLE)     PRESSION
      COMPLEX(SIMPLE)  VENT
      REAL(SIMPLE)     TEMPERATURE
      INTEGER(ENTIER)  NIVEAU
      LOGICAL          STATUS

      COMMON /MESURE_METEO/ ADRESSE, NOM_STATION, PRESSION, VENT, TEMPERATURE, NIVEAU, STATUS

      DATA  ADRESSE     / '3, Rue du Chemin des Cretes 31525 Espanes' /
      DATA  NOM_STATION / 'Station_ESPANES_1' /
      DATA  PRESSION, VENT / (1015.3D0, 2.0D0), (3.0, 4.0) /
      DATA  TEMPERATURE, NIVEAU, STATUS / 27.3, 15, .TRUE. /

! !!! La regle 'Pr.UniteNommee' n'est pas respectee ici, le bloc devrait se terminer par 
! !!! 'END BLOCK DATA Bloc_Mesure' 
END 

!
! --- La routine ci-dessous permet d'effectuer la conversion de degres 
! --- Celsius vers degres Fahrenheit
!
SUBROUTINE Celsius_to_Fahrenheit(Tc, Tf)

      USE ma_precision
      IMPLICIT NONE

      real(SIMPLE), intent(in)  :: Tc           ! temperature en celsius
      real(SIMPLE), intent(out) :: Tf           ! temperature en fahrenheit

      Tf = (9.0/5.0)*Tc+32.0

! !!! La regle 'Pr.UniteNommee' n'est pas respectee ici, la routine devrait se terminer par 
! !!! 'END SUBROUTINE Celsius_to_Fahrenheit' 
END 

!
! --- La fonction ci-dessous permet d'extraire l'UID (UNIX) avec lequel 
! --- s'execute l'unite de programme 
!
INTEGER FUNCTION MON_GETUID()

     USE ma_precision
     IMPLICIT NONE

     INTEGER(ENTIER) I_RET

     I_RET = GETUID ()
     WRITE(*,*) 'I_RET=', I_RET

     MON_GETUID = I_RET
     WRITE(*,*) 'MON_GETUID=', MON_GETUID

     RETURN

! !!! La regle 'Pr.UniteNommee' n'est pas respectee ici, la fonction devrait se terminer par 
! !!! 'END FUNCTION MON_GETUID' 
END 

!
! --- L'unite de programme ci-dessous permet de faire appel a toutes les entites 
! --- MODULE, SUBROUTINE, FUNCTION, BLOCK DATA declarees plus haut 
! 
PROGRAM ESSAI

     USE ma_precision
     IMPLICIT NONE

     integer(ENTIER) :: I_UID
     integer(ENTIER) :: MON_GETUID

     real(SIMPLE)    :: Temp_Fahr

!
! --- Les donnees presentes dans le common nomme, sont accedees dans le programme en 
! --- referencant ce common par son nom : 'MESURE_METEO' et en decrivant son organisation.

! --- Dans la description de l'organisation du common il faut veiller au type des variables et a
! --- leur ordre de definition : l'utilisation dans un INCLUDE est recommandee
! --- On ne le fait pas ici, pour que l'exemple soit explicite
!
     CHARACTER *128   ADRESSE
     CHARACTER *32    NOM_STATION
     REAL(DOUBLE)     PRESSION
     COMPLEX(SIMPLE)  VENT
     REAL(SIMPLE)     TEMPERATURE
     INTEGER(ENTIER)  NIVEAU
     LOGICAL          STATUS

     COMMON /MESURE_METEO/ ADRESSE, NOM_STATION, PRESSION, VENT, TEMPERATURE, NIVEAU, STATUS

! --- Mise en oeuvre de la fonction : mon_getuid
     WRITE(*, 1001)
     I_UID = MON_GETUID()
     WRITE(*, *) 'UID =', I_UID

! --- On accede aux donnees declarees dans le common 
     WRITE(*, 1002)
     WRITE(*,*) 'NOM_STATION:', NOM_STATION, 'ADRESSE:', ADRESSE
     WRITE(*,*) 'TEMPERATURE:', TEMPERATURE, 'PRESSION:', PRESSION, 'VENT:', VENT
     WRITE(*,*) 'NIVEAU:', NIVEAU, 'STATUS:', STATUS

! --- On fait appel a la routine de conversion de la temperature Celsius en Fahrenheit
     WRITE(*, 1003)
     CALL Celsius_to_Fahrenheit (TEMPERATURE, Temp_Fahr)
     WRITE(*,*) 'Temperature(CELSIUS):', TEMPERATURE, 'Temperature(FAHRENHEIT):', Temp_Fahr

!
! --------------------------------------------------------------------------
!      F O R M A T S
! --------------------------------------------------------------------------
!
1001   FORMAT(1X, '--- Recuperer le User Id ---')
1002   FORMAT(1X, '--- Afficher les donnees du common ---')
1003   FORMAT(1X, '--- Convertir la temperature de Celsius en Fahrenheit ---')

! !!! La regle 'Pr.UniteNommee' n'est pas respectee ici, le programme devrait se terminer par 
! !!! 'END PROGRAM ESSAI' 
END 
