PROGRAM ESSAI

!
! --- Cette unite de programme (appel sans parametre) effectue la lecture du fichier /etc/passwd 
! --- ligne par ligne, puis affiche sur la sortie standard le contenu de chaque ligne lue .
!

! --- Non respect de la regle 'Int.OpenClose' par ce programme : le fichier /etc/passwd est ouvert 
! ---    mais n'est pas ferme explicitement (si tout va bien la fermeture effective sera forcee par 
! ---    le systeme d'exploitation a la liberation des ressources associees a l'arret du processus)
! --  De plus, en cas d'erreur sur les acces fichier, aucun messsage d'erreur n'est reporte  
!
      INTEGER, parameter :: EOF = -1
      INTEGER, parameter :: f_unit = 15 
      INTEGER            :: count = 0
      INTEGER            :: ios  

      CHARACTER(128)     :: c_buffer 

      OPEN (UNIT = f_unit, FILE = '/etc/passwd', STATUS = 'OLD')
      READ (UNIT = f_unit, FMT = 100, IOSTAT = ios) c_buffer

! --- Boucle de lecture, ligne par ligne jusqu'a la fin de fichier
      DO WHILE(ios >= 0)

         WRITE (*,*) count, c_buffer 
         count = count + 1
         READ (UNIT = f_unit, FMT = 100, IOSTAT = ios) c_buffer

      END DO

!
! ----------------------------------------------------------------------
!     F O R M A T S 
! ----------------------------------------------------------------------
100   FORMAT(A128)

END PROGRAM ESSAI 
