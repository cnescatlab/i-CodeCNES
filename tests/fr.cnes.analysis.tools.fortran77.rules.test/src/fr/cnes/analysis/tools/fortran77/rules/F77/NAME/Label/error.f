      PROGRAM ESSAI
C     
      INTEGER IOS
      INTEGER I_ARG
      INTEGER COUNT
      INTEGER EOF
C
      CHARACTER*32 C_ARG
      CHARACTER*128 C_PASSWD

C
C --- Recuperer le 2nd parametre d'appel du programme 
C
      I_ARG = 1
      CALL getarg(I_ARG, C_ARG)
      WRITE(*,*) 'ARG =', I_ARG, 'VALUE =', C_ARG

C
C --- Lecture du fichier passe comme second parametre, imprimer le contenu sur stdout ligne par ligne 
C
      COUNT = 0

      OPEN (UNIT = 15, FILE = C_ARG, STATUS = 'OLD', IOSTAT=IOS,        &
     1ERR=9911)
      READ (UNIT = 15, FMT = '(A128)', IOSTAT = EOF, ERR=9912) C_PASSWD

C
C --- Boucle de lecture, ligne par ligne jusqu'a la fin de fichier
C
      DO WHILE(EOF .GE. 0)

         WRITE (*,*) COUNT, C_PASSWD
         COUNT = COUNT + 1
         READ (UNIT = 15, FMT = '(A128)', IOSTAT = EOF, ERR=9912) C_PASSWD

      END DO

      CLOSE (UNIT = 15, IOSTAT=EOF, ERR=9913)
      GO TO 9999

C
C ---------------------------------------------------------------------------
C     E R R E U R S 
C ---------------------------------------------------------------------------
C
9911  WRITE(*,*) '!!! Err. Ouverture Fichier:', C_ARG, 'IOS=',          &
     1IOS, '!!!'
      GO TO 9999
9912  WRITE(*,*) '!!! Err. Lecture Fichier :', C_ARG,                   &
     1'COUNT=', COUNT, 'IOS=', IOS, '!!!'
      GO TO 9999
9913  WRITE(*,*) '!!! Err. Fermeture Fichier :', C_ARG, 'IOS=',         &
     1IOS, '!!!'
      GO TO 9999

C
C ---------------------------------------------------------------------------
C     F I N   D U   P R O G R A M M E  
C ---------------------------------------------------------------------------
C
9999  WRITE(*,*) 'Fin du programme'
      STOP
      END PROGRAM
