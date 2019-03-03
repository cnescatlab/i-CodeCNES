
!
! Le programme ci-dessous, effectue l'ouverture d'un fichier dont le nom a ete 
! passe en parametre (situe dans le repertoire courant) , puis lit son contenu 
! et l'affiche dans la sortie standard .
! Par ailleurs, effectue quelques controles en utilisant des fonctions intrinseques 
! telles que : getuid, len, trim, ... 
! 

program ESSAI
!     
      IMPLICIT NONE
!
      INTEGER :: ios
      INTEGER :: i_arg
      INTEGER :: count
      INTEGER :: i_uid 
      INTEGER :: i_size
      INTEGER :: i_len

      INTEGER, parameter :: File_Unit = 15
      INTEGER, parameter :: Eof = -1
!
      CHARACTER(32)  :: c_arg
      CHARACTER(128) :: c_buffer

! --- Recuperer le 1er parametre d'appel du programme : son nom 
      i_arg = 0
      CALL getarg(i_arg, c_arg)
      WRITE(*,9001) 'ARG =', i_arg, 'VALUE =', c_arg

! --- Avec quel utilisateur execute t'on ce programme, s'il s'agit de root, sortie immediate
      i_uid = getuid()
      if (i_uid .EQ. 0) THEN
! !!! Regle 'Pr.OperRelationnel' pas respectee, il faudrait utiliser la forme symbolique '==' 
 
         WRITE(*,*) '!!! Programme [', c_arg, '] execute par utilisateur root ==> Sortie du programme' 
         STOP
      ENDIF

! --- Recuperer le 2eme parametre d'appel du programme : le nom du fichier a lire
      i_arg = 1
      CALL getarg(i_arg, c_arg)
      WRITE(*,9001) 'ARG =', i_arg, 'VALUE =', c_arg

! --- Recuperer la taille du fichier 
      inquire(FILE=c_arg, SIZE=i_size)
      print '("size is ", i9, " bytes")', i_size
      IF (i_size .LE. 0) THEN
! !!! Regle 'Pr.OperRelationnel' pas respectee, il faudrait utiliser la forme symbolique '<=' 

         WRITE(*,*) '!!! Le fichier [', c_arg, '] a une taille negative ou nulle ==> Sortie du programme' 
      ENDIF

!
! --- Ouverture du fichier passe en parametre
!
      count = 0

      OPEN (UNIT = File_Unit, FILE = c_arg, STATUS = 'OLD', IOSTAT = ios)
      IF ( ios .NE. 0) THEN
! !!! Regle 'Pr.OperRelationnel' pas respectee, il faudrait utiliser la forme symbolique '/=' 

         WRITE(*,9002) '!!! Err. Ouverture Fichier:', c_arg, 'IOS=', ios, '!!!'
         STOP
      ENDIF

! --- Lecture du fichier ligne par ligne, imprimer chaque ligne lue sur stdout
      READ (UNIT = File_Unit, FMT = 9011, IOSTAT = ios) c_buffer
      IF (ios .LT. 0) THEN
! !!! Regle 'Pr.OperRelationnel' pas respectee, il faudrait utiliser la forme symbolique '<' 

         WRITE(*,9003) '!!! Erreur lecture Fichier:', c_arg, 'Enreg:', count, 'IOS=', ios, '!!!'
         CLOSE (UNIT = File_Unit, IOSTAT = ios)
         IF (ios .LT. 0) THEN
! !!! Regle 'Pr.OperRelationnel' pas respectee, il faudrait utiliser la forme symbolique '<' 

            WRITE(*,9002) '!!! Err. Fermeture Fichier :', c_arg, 'IOS=', ios, '!!!'
         ENDIF
         STOP
      ENDIF

! --- Boucle de lecture, ligne par ligne jusqu'a la fin de fichier
      DO WHILE(ios .GE. 0)
! !!! Regle 'Pr.OperRelationnel' pas respectee, il faudrait utiliser la forme symbolique '>=' 

         i_len = len(trim(c_buffer))
         IF (i_len .GT. 0) THEN 
! !!! Regle 'Pr.OperRelationnel' pas respectee, il faudrait utiliser la forme symbolique '>' 

            WRITE (*,*) count, i_len, c_buffer
         ELSE
            WRITE (*,*) count, '--- Ligne vide ---'
         ENDIF
         count = count + 1
         READ (UNIT = File_Unit, FMT = 9011, IOSTAT = ios) c_buffer

      END DO

      IF ( (ios .LT. 0) .AND. (ios .NE. EOF) ) THEN
! !!! Regle 'Pr.OperRelationnel' pas respectee, il faudrait utiliser les formes symboliques '<' et '/=' 

         WRITE(*,9003) '!!! Erreur lecture Fichier:', c_arg, 'Enreg:', count, 'IOS=', ios, '!!!'

         CLOSE (UNIT = File_Unit, IOSTAT = ios)

         IF (ios .LT. 0) THEN
! !!! Regle 'Pr.OperRelationnel' pas respectee, il faudrait utiliser la forme symbolique '<' 

            WRITE(*,9002) '!!! Err. Fermeture Fichier :', c_arg, 'IOS=', ios, '!!!'
         ENDIF
         STOP

      ENDIF

      CLOSE (UNIT = File_Unit, IOSTAT = ios)
      IF (ios .LT. 0) THEN
! !!! Regle 'Pr.OperRelationnel' pas respectee, il faudrait utiliser la forme symbolique '<' 

         WRITE(*,9002) '!!! Err. Fermeture Fichier :', c_arg, 'IOS=', ios, '!!!'
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
