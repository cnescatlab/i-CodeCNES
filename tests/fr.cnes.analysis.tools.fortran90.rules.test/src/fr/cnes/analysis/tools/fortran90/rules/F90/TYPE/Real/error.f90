
!
! --- Le common 'Bloc_Mesure' defini ici contient tous les types d'informations
! --- relatifs a une mesure : localisation, parametres, ...
!
BLOCK DATA  Bloc_Mesure

      IMPLICIT NONE

      CHARACTER *128                             :: ADRESSE
      CHARACTER *32                              :: NOM_STATION

      REAL                                 :: PRESSION
      REAL                                 :: ALTITUDE
      REAL                                 :: RAYONNEMENT
      COMPLEX                              :: VENT
      COMPLEX, DIMENSION(128)              :: SIGNAL
      REAL                                 :: TEMPERATURE
! !!! Pour toutes les lignes situees ci-dessus, la regle 'Don.SousTypeReel' n'est pas respectee : 

      INTEGER                              :: NIVEAU
      LOGICAL                              :: STATUS  

      COMMON /MESURE_METEO/ ADRESSE, NOM_STATION, PRESSION, ALTITUDE, RAYONNEMENT,&
                            VENT, SIGNAL, TEMPERATURE, NIVEAU, STATUS

      DATA  ADRESSE     / '3, Rue du Chemin des Cretes 31525 Espanes' /
      DATA  NOM_STATION / 'Station_ESPANES_1' /
      DATA  PRESSION, ALTITUDE / (1015.3D0, 2.0D0), 212.53D0 /
      DATA  RAYONNEMENT, VENT / 1015.3D0, (3.0, 4.0) /
      DATA  SIGNAL / 32*(1.0,1.0), 32*(1.5,1.5), 32*(2.0,2.0), 32*(2.5,2.5) /
      DATA  TEMPERATURE, NIVEAU, STATUS / 27.3, 15, .TRUE. /

END BLOCK DATA Bloc_Mesure

!
! --- L'unite de programme ci-dessous permet d'afficher dans la sortie standard 
! --- le contenu des variables localisees dans le common nomme .
!
PROGRAM ESSAI

      IMPLICIT NONE

!
! --- Les donnees presentes dans le common nomme, sont accedees dans le programme en
! --- referencant ce common par son nom : 'MESURE_METEO' et en decrivant son organisation.

! --- Dans la description de l'organisation du common il faut veiller au type des variables et a
! --- leur ordre de definition : l'utilisation dans un INCLUDE est recommandee
! --- On ne le fait pas ici, pour que l'exemple soit explicite
!
      CHARACTER *128                             :: ADRESSE
      CHARACTER *32                              :: NOM_STATION

      REAL                                       :: PRESSION
      REAL                                       :: ALTITUDE
      REAL                                       :: RAYONNEMENT
      COMPLEX                                    :: VENT
      COMPLEX, DIMENSION(128)                    :: SIGNAL
      REAL                                       :: TEMPERATURE
! !!! Pour toutes les lignes situees ci-dessus, la regle 'Don.SousTypeReel' n'est pas respectee : 
! !!!    en effet, la meilleure solution consiste a un module, dans lequel toutes les specifications 
! !!!    de precision des variables reelles et complexes mettant en oeuvre les fonctions 
! !!!    intrinseques 'selected_xxx' seront declarees . 
! !!!    Celles-ci seront reutilisees dans les declarations de variables, chaque fois que 
! !!!    c'est necessaire . 

      INTEGER                                    :: NIVEAU
      LOGICAL                                    :: STATUS  

      COMMON /MESURE_METEO/ ADRESSE, NOM_STATION, PRESSION, ALTITUDE, RAYONNEMENT,&
                           VENT, SIGNAL, TEMPERATURE, NIVEAU, STATUS

! --- On accede aux donnees declarees dans le common
      WRITE(*, 1001)
      WRITE(*,*) 'NOM_STATION:', NOM_STATION, 'ADRESSE:', ADRESSE
      WRITE(*,*) 'TEMPERATURE:', TEMPERATURE, 'PRESSION:', PRESSION
      WRITE(*,*) 'ALTITUDE:', ALTITUDE, 'RAYONNEMENT:', RAYONNEMENT, 'VENT:', VENT
      WRITE(*,*) 'SIGNAL(Extrait):', SIGNAL(1:10)
      WRITE(*,*) 'NIVEAU:', NIVEAU, 'STATUS:', STATUS

!
! --------------------------------------------------------------------------
!      F O R M A T S
! --------------------------------------------------------------------------
!
1001  FORMAT(1X, '--- Afficher les donnees du common ---')

END PROGRAM ESSAI
