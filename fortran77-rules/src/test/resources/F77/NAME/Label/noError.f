      PROGRAM ESSAI
C     
      INTEGER IOS
      INTEGER I_ARG
      INTEGER COUNT
      INTEGER EOF
C
      CHARACTER*32 C_ARG
      CHARACTER*128 C_PASSWD

C --- Recuperer le 2nd parametre d'appel du programme 
      I_ARG = 1
      CALL getarg(I_ARG, C_ARG)
      WRITE(*,9001) 'ARG =', I_ARG, 'VALUE =', C_ARG

C
C --- Lecture du fichier passe comme second parametre, imprimer le contenu sur stdout ligne par ligne 
C
      COUNT = 0

      OPEN (UNIT = 15, FILE = C_ARG, STATUS = 'OLD', IOSTAT=IOS,        &
     1ERR=9911)
      READ (UNIT = 15, FMT = 9011, IOSTAT = EOF, ERR=9912) C_PASSWD

C --- Boucle de lecture, ligne par ligne jusqu'a la fin de fichier
      DO WHILE(EOF .GE. 0)

         WRITE (*,*) COUNT, C_PASSWD
         COUNT = COUNT + 1
         READ (UNIT = 15, FMT = 9011, IOSTAT = EOF, ERR=9912) C_PASSWD

      END DO

      CLOSE (UNIT = 15, IOSTAT=EOF, ERR=9913)
      GO TO 9999

C
C ---------------------------------------------------------------------------
C     F O R M A T S 
C ---------------------------------------------------------------------------
C
9001  FORMAT(1X, A5, 1X, I3, 4X, A7, 1X, A32)
9002  FORMAT(1X, A32, 1X, A32, 1X, A6, 1X, I5, 1X, A3)
9011  FORMAT(A128)

C
C ---------------------------------------------------------------------------
C     E R R E U R S 
C ---------------------------------------------------------------------------
C
9911  CONTINUE
      WRITE(*,9002) '!!! Err. Ouverture Fichier:', C_ARG, 'IOS=',       &
     1IOS, '!!!'
      GO TO 9999
9912  CONTINUE
      WRITE(*,9002) '!!! Err. Lecture Fichier :', C_ARG,                &
     1'COUNT=', COUNT, 'IOS=', IOS, '!!!'
      GO TO 9999
9913  CONTINUE
      WRITE(*,9002) '!!! Err. Fermeture Fichier :', C_ARG, 'IOS=',      &
     1IOS, '!!!'
      GO TO 9999

C
C ---------------------------------------------------------------------------
C     F I N   D U   P R O G R A M M E  
C ---------------------------------------------------------------------------
C
9999  CONTINUE
      STOP
      END PROGRAM
