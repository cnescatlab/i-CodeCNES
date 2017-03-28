PROGRAM ESSAI

      IMPLICIT NONE

!
! --- Cette unite de programme (appel avec parametre fichier a lire ) effectue la lecture 
! --- du fichier passe en parametre ligne par ligne et affiche dans la sortie standard le contenu 
! --- Le fichier est ouvert , lu jusqu'a la fin de fichier puis ferme explicitement 
!
      INTEGER, parameter :: EOF = -1
      INTEGER            :: count = 0
      INTEGER, parameter :: f_unit = 15
      INTEGER            :: ios
      INTEGER            :: i_arg

      CHARACTER(32)      :: c_arg
      CHARACTER(128)     :: c_buffer 

! --- Recuperer le 1er parametre d'appel du programme
      i_arg = 1
      CALL getarg(i_arg, c_arg)
      WRITE(*,9001) 'ARG =', i_arg, 'VALUE =', c_arg

! --- Ouverture du fichier 
! --- STATUS   ---> permet de preciser : 
!                   . s'il s'agit d'un fichier existant ('old'), 
!                   . a creer ('new'), 
!                   . a remplacer ('replace'), 
!                   . temporaire ('scratch') 
!                   ou de statut quelconque ('unknown)   
! --- POSITION ---> permet de se positionner dans le fichier ouvert, 
!                   . au debut ('rewind'), 
!                   . a la fin ('append'), 
!                   . ou a la derniere position en date si le fichier deja ouvert ('asis') 
! +++ Respect de la regle 'Int.ParamOpen' avec la presence dans l'ordre d'ouverture du fichier (OPEN) 
! +++    des parametres requis ; c'est a dire FILE, STATUS, POSITION 
      OPEN (UNIT = f_unit, FILE = c_arg, STATUS = 'OLD', POSITION = 'REWIND', IOSTAT = ios)
      IF (ios /= 0) THEN 
         WRITE(*,9002) '!!! Err. Ouverture Fichier:', c_arg, ' IOS=', ios, '!!!'
         STOP
      ENDIF

! --- Lecture du 1er enregistrement 
      READ (UNIT = f_unit, FMT = 9011, IOSTAT = ios) c_buffer
      IF (ios < 0) THEN
         WRITE(*,9003) '!!! Erreur lecture Fichier:', c_arg, ' - Enreg:', count, 'IOS=', ios, '!!!'
         CLOSE (UNIT = f_unit, IOSTAT = ios)
         IF (ios < 0) THEN
            WRITE(*,9002) '!!! Err. Fermeture Fichier:', c_arg, ' IOS=', ios, '!!!'
         ENDIF
         STOP
      ENDIF

! --- Boucle de lecture, ligne par ligne jusqu'a la fin de fichier
      DO WHILE(ios >= 0)

         WRITE (*,*) count, c_buffer
         count = count + 1
         READ (UNIT = f_unit, FMT = 9011, IOSTAT = ios) c_buffer

      END DO

      IF ( (ios < 0) .AND. (ios /= EOF) ) THEN
         WRITE(*,9003) '!!! Erreur lecture Fichier:', c_arg, ' - Enreg:', count, 'IOS=', ios, '!!!'
         CLOSE (UNIT = f_unit, IOSTAT = ios)
         IF (ios < 0) THEN
            WRITE(*,9002) '!!! Err. Fermeture Fichier:', c_arg, 'IOS=', ios, '!!!'
         ENDIF
         STOP
      ENDIF

      CLOSE (UNIT = f_unit, IOSTAT = ios)
      IF (ios < 0) THEN
         WRITE(*,9002) '!!! Err. Fermeture Fichier:', c_arg, 'IOS=', ios, '!!!'
      ENDIF

!
! ---------------------------------------------------------------------------
!     F O R M A T S
! ---------------------------------------------------------------------------
!
9001  FORMAT(1X, A5, 1X, I3, 4X, A7, 1X, A32)
9002  FORMAT(1X, A, 1X, A, 1X, A, 1X, I5, 1X, A)
9003  FORMAT(1X, A, 1X, A, 1X, A, 1X, I5, 1X, A, 1X, I5, 1X, A)
9011  FORMAT(A128)

END PROGRAM ESSAI 
