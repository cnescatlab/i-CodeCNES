      PROGRAM ESSAI
!     
      IMPLICIT NONE
!
      INTEGER :: IOS
      INTEGER :: I_ARG
      INTEGER :: COUNT
      INTEGER, parameter :: FILE_UNIT = 15
      INTEGER, parameter :: EOF = -1
!
      CHARACTER(32)  :: C_ARG
      CHARACTER(128) :: C_BUFFER

! --- Recuperer le 1er parametre d'appel du programme 
      I_ARG = 1
      CALL getarg(I_ARG, C_ARG)
      WRITE(*,9001) 'ARG =', I_ARG, 'VALUE =', C_ARG

!
! --- Ouverture du fichier passe en parametre
!
      COUNT = 0

      OPEN (UNIT = FILE_UNIT, FILE = C_ARG, STATUS = 'OLD', IOSTAT=IOS)
      IF ( IOS .NE. 0) THEN
         WRITE(*,9002) '!!! Err. Ouverture Fichier:', C_ARG, 'IOS=', IOS, '!!!'
         STOP
      ENDIF

! --- Lecture du fichier ligne par ligne, imprimer chaque ligne lue sur stdout
      READ (UNIT = FILE_UNIT, FMT = 9011, IOSTAT = IOS) C_BUFFER
      IF (IOS .LT. 0) THEN
         WRITE(*,9003) '!!! Erreur lecture Fichier:', C_ARG, 'Enreg:', COUNT, 'IOS=', IOS, '!!!'
         CLOSE (UNIT = FILE_UNIT, IOSTAT=IOS)
         IF (IOS .LT. 0) THEN
            WRITE(*,9002) '!!! Err. Fermeture Fichier :', C_ARG, 'IOS=', IOS, '!!!'
         ENDIF
         STOP
      ENDIF

! --- Boucle de lecture, ligne par ligne jusqu'a la fin de fichier
      DO WHILE(IOS .GE. 0)

         WRITE (*,*) COUNT, C_BUFFER
         COUNT = COUNT + 1

      END DO

      IF ( (IOS .LT. 0) .AND. (IOS .NE. EOF) ) THEN

         WRITE(*,9003) '!!! Erreur lecture Fichier:', C_ARG, 'Enreg:', COUNT, 'IOS=', IOS, '!!!'

         CLOSE (UNIT = FILE_UNIT, IOSTAT=IOS)

         IF (IOS .LT. 0) THEN
            WRITE(*,9002) '!!! Err. Fermeture Fichier :', C_ARG, 'IOS=', IOS, '!!!'
         ENDIF
         STOP

      ENDIF

      CLOSE (UNIT = FILE_UNIT, IOSTAT=IOS)
      IF (IOS .LT. 0) THEN
         WRITE(*,9002) '!!! Err. Fermeture Fichier :', C_ARG, 'IOS=', IOS, '!!!'
      ENDIF
!
! ---------------------------------------------------------------------------
!     F O R M A T S 
! ---------------------------------------------------------------------------
!
9001  FORMAT(1X, A5, 1X, I3, 4X, A7, 1X, A32)
9002  FORMAT(1X, A32, 1X, A32, 1X, A6, 1X, I5, 1X, A3)
9003  FORMAT(1X, A32, 1X, A32, 1X, A6, 1X, I5, 1X, A6, 1X, I5, 1X, A3)
9011  FORMAT(A128)

!
! ---------------------------------------------------------------------------
!     F I N   D U   P R O G R A M M E  
! ---------------------------------------------------------------------------
!
      STOP
      END PROGRAM ESSAI
