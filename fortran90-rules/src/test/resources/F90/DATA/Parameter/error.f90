
!
! --- Le common 'Bloc_Mesure' defini ici contient tous les types d'informations
! --- relatifs a une mesure : localisation, parametres, ...
!
BLOCK DATA  Bloc_Mesure

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
! !!! Pour toutes les lignes situees ci-dessus, la regle 'Don.SpecSousType' n'est pas respectee : 
! !!!    en effet, il faut creer un module, dans lequel toutes les specifications de precision 
! !!!    mettant en oeuvre les fonctions intrinseques 'selected_xxx' seront declarees . 
! !!!    Celles-ci seront reutilisees dans les declarations de variables, 
! !!!    chaque fois que c'est necessaire . 

      COMMON /MESURE_METEO/ ADRESSE, NOM_STATION, PRESSION, ALTITUDE, RAYONNEMENT,&
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

      REAL(selected_real_kind(15))               :: PRESSION
      REAL(selected_real_kind(15, 307))          :: ALTITUDE
      REAL(selected_real_kind(10, 30))           :: RAYONNEMENT
      COMPLEX(selected_real_kind(6))             :: VENT
      REAL(selected_real_kind(6))                :: TEMPERATURE
      INTEGER(selected_int_kind(9))              :: NIVEAU
! !!! Pour toutes les lignes situees ci-dessus, la regle 'Don.SpecSousType' n'est pas respectee : 
! !!!    en effet, il faut creer un module, dans lequel toutes les specifications de precision 
! !!!    mettant en oeuvre les fonctions intrinseques 'selected_xxx' seront declarees . 
! !!!    Celles-ci seront reutilisees dans les declarations de variables, 
! !!!    chaque fois que c'est necessaire . 

      LOGICAL                                    :: STATUS  

      COMMON /MESURE_METEO/ ADRESSE, NOM_STATION, PRESSION, ALTITUDE, RAYONNEMENT,&
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
1001  FORMAT(1X, '--- Afficher les donnees du common ---')

END PROGRAM ESSAI
