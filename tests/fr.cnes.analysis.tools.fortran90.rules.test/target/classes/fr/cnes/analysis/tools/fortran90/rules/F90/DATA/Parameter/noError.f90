
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

     integer, parameter :: DISTANCE = selected_real_kind(10, 30)
!    est un paramètre de sous-type spécifiant une représentation machine 
!    permettant de garantir au moins dix chiffres décimaux significatifs entre 10-30 et 10+30.

     integer, parameter :: IEEE_DOUBLE = selected_real_kind(15, 307)
!    est le paramètre de sous-type correspondanta la double précision 64 bits IEEE754.

END MODULE ma_precision

!
! --- Le common 'Bloc_Mesure' defini ici contient tous les types d'informations
! --- relatifs a une mesure : localisation, parametres, ...
!
BLOCK DATA  Bloc_Mesure

      USE ma_precision
      IMPLICIT NONE

      CHARACTER *128    ADRESSE
      CHARACTER *32     NOM_STATION
      REAL(DOUBLE)      PRESSION
      REAL(IEEE_DOUBLE) ALTITUDE
      REAL(DISTANCE)    RAYONNEMENT
      COMPLEX(SIMPLE)   VENT
      REAL(SIMPLE)      TEMPERATURE
      INTEGER(ENTIER)   NIVEAU
      LOGICAL           STATUS

      COMMON /MESURE_METEO/ ADRESSE, NOM_STATION, PRESSION, ALTITUDE, RAYONNEMENT, &
                            VENT, TEMPERATURE, NIVEAU, STATUS

      DATA  ADRESSE     / '3, Rue du Chemin des Cretes 31525 Espanes' /
      DATA  NOM_STATION / 'Station_ESPANES_1' /
      DATA  PRESSION, ALTITUDE / (1015.3D0, 2.0D0), 212.53D0 /
      DATA  RAYONNEMENT, VENT / 1015.3D0, (3.0, 4.0) /
      DATA  TEMPERATURE, NIVEAU, STATUS / 27.3, 15, .TRUE. /

END BLOCK DATA Bloc_Mesure


!
! --- L'unite de programme ci-dessous permet dafficher dans sortie standard 
! --- le contenu des variables localisees dans le common nomme .
!
PROGRAM ESSAI

     USE ma_precision
     IMPLICIT NONE

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
     REAL(IEEE_DOUBLE) ALTITUDE
     REAL(DISTANCE)    RAYONNEMENT
     COMPLEX(SIMPLE)  VENT
     REAL(SIMPLE)     TEMPERATURE
     INTEGER(ENTIER)  NIVEAU
     LOGICAL          STATUS

     COMMON /MESURE_METEO/ ADRESSE, NOM_STATION, PRESSION, ALTITUDE, RAYONNEMENT, &
                           VENT, TEMPERATURE, NIVEAU, STATUS

! --- On accede aux donnees declarees dans le common
     WRITE(*, 1001)
     WRITE(*,*) 'NOM_STATION:', NOM_STATION, 'ADRESSE:', ADRESSE
     WRITE(*,*) 'TEMPERATURE:', TEMPERATURE, 'PRESSION:', PRESSION
     WRITE(*,*) 'ALTITUDE:', ALTITUDE, 'RAYONNEMENT:', RAYONNEMENT, 'VENT:', VENT
     WRITE(*,*) 'NIVEAU:', NIVEAU, 'STATUS:', STATUS

!
! --------------------------------------------------------------------------
!      F O R M A T S
! --------------------------------------------------------------------------
!
1001   FORMAT(1X, '--- Afficher les donnees du common ---')

END PROGRAM ESSAI
