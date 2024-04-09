PROGRAM ESSAI

!
! --- Cette unite de programme (appel sans parametre) effectue la lecture du fichier /etc/passwd 
! --- ligne par ligne, puis affiche sur la sortie standard le contenu de chaque ligne lue .
!

! --- Non respect de la regle 'Int.ParamOpen' par ce programme : le fichier /etc/passwd est ouvert 
! ---    mais sans preciser son STATUS (on laisse le systeme d'exploitation decider) et sans 
! ---    preciser non plus la POSITION (il s'agit d'un fichier sequentiel). 
! --  De plus, en cas d'erreur lors des acces fichier, aucun messsage d'erreur n'est reporte  
!
      INTEGER, parameter :: EOF = -1
      INTEGER, parameter :: f_unit = 15 
      INTEGER            :: count = 0
      INTEGER            :: ios  
      INTEGER            :: i_arg

      CHARACTER(32)      :: c_arg
      CHARACTER(128)     :: c_buffer 

! --- Recuperer le 1er parametre d'appel du programme
      i_arg = 1
      CALL getarg(i_arg, c_arg)
      WRITE(*,9001) 'ARG =', i_arg, 'VALUE =', c_arg


      OPEN (UNIT = f_unit, FILE = c_arg, STATUS = 'UNKNOWN')
! !!! Regle 'Int.ParamOpen' n'est pas respectee ci-dessus : 
! !!! . Il manque le parametre POSITION
! !!! . Le positionnement de STATUS a la valeur 'UNKNOWN' est interdit 
      READ (UNIT = f_unit, FMT = 9011, IOSTAT = ios) c_buffer

! --- Boucle de lecture, ligne par ligne jusqu'a la fin de fichier
      DO WHILE(ios >= 0)

         WRITE (*,*) count, c_buffer 
         count = count + 1
         READ (UNIT = f_unit, FMT = 9011, IOSTAT = ios) c_buffer

      END DO

! ----------------------------------------------------------------------
!     F O R M A T S 
! ----------------------------------------------------------------------
9001  FORMAT(1X, A5, 1X, I3, 4X, A7, 1X, A32)
9011  FORMAT(A128)

END PROGRAM ESSAI 
