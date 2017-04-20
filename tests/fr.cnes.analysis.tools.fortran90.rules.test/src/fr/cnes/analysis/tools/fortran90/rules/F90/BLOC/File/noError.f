! *********************************************************************************************
! --- Cette unite de programme (appel sans parametre) effectue la lecture 
! --- du fichier /etc/passwd ligne par ligne et affiche dans la sortie standard le contenu 
! --- Le fichier /etc/passwd est ouvert , lu jusqu'a la fin de fichier puis ferme explicitement 
! *********************************************************************************************
PROGRAM ESSAI

      IMPLICIT NONE

      INTEGER, parameter :: EOF = -1
      INTEGER            :: count = 0
      INTEGER            :: ios
      INTEGER            :: f_unit = 15

      CHARACTER(128)     :: c_buffer 

      OPEN (UNIT = f_unit, FILE = '/etc/passwd', STATUS = 'OLD', IOSTAT = ios)
      IF (ios /= 0) THEN 
         WRITE(*,9002) '!!! Err. Ouverture Fichier "/etc/passwd" IOS=', ios, '!!!'
         STOP
      ENDIF

      READ (UNIT = f_unit, FMT = 9011, IOSTAT = ios) c_buffer
      IF (ios < 0) THEN
         WRITE(*,9003) '!!! Erreur lecture Fichier "/etc/passwd" - Enreg:', count, 'IOS=', ios, '!!!'
         CLOSE (UNIT = f_unit, IOSTAT = ios)
         IF (ios < 0) THEN
            WRITE(*,9002) '!!! Err. Fermeture Fichier "/etc/passwd" IOS=', ios, '!!!'
         ENDIF
         STOP
      ENDIF

! --- Boucle de lecture, ligne par ligne jusqu'a la fin de fichier
      DO WHILE(ios >= 0)

         WRITE (*,*) count, c_buffer
         count = count + 1
         READ (UNIT = f_unit, FMT = 9011, IOSTAT = ios) c_buffer

      END DO

      IF ( (ios < 0) .AND. (IOS /= EOF) ) THEN

         WRITE(*,9003) '!!! Erreur lecture Fichier "/etc/passwd" - Enreg:', count, 'IOS=', ios, '!!!'

         CLOSE (UNIT = f_unit, IOSTAT = IOS)

         IF (IOS < 0) THEN
            WRITE(*,9002) '!!! Err. Fermeture Fichier "/etc/passwd" IOS=', ios, '!!!'
         ENDIF
         STOP

      ENDIF

      CLOSE (UNIT = f_unit, IOSTAT = ios)
      IF (ios < 0) THEN
         WRITE(*,9002) '!!! Err. Fermeture Fichier "/etc/passwd" IOS=', ios, '!!!'
      ENDIF

! ---------------------------------------------------------------------------
!     F O R M A T S
! ---------------------------------------------------------------------------
9002  FORMAT(1X, A, 1X, I5, 1X, A)
9003  FORMAT(1X, A, 1X, I5, 1X, A, 1X, I5, 1X, A)
9011  FORMAT(A128)


END PROGRAM ESSAI 
