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
! ---    Lorsque les clauses IOSTAT=IOS ou ERR=XXX ne sont pas utilisées dans les primitives 
! ---    d'entree/sortie telles que OPEN, READ, CLOSE ,
! ---    => toute erreur I/O n'etant pas interceptee par l'application cela se traduit 
! ---    se traduit par une erreur de type FORTRAN RunTime Error  pas forcement explicite .
!
      COUNT = 0
      OPEN (UNIT = FILE_UNIT, FILE = C_ARG, STATUS = 'OLD')

! --- Boucle de lecture, ligne par ligne jusqu'a la fin de fichier
      DO 

         READ (UNIT = FILE_UNIT, FMT = 9011, END=100) C_BUFFER
         COUNT = COUNT + 1
         WRITE (*,*) COUNT, C_BUFFER

      END DO

!--   Pas de verification sur IOSTAT      
      READ (UNIT = FILE_UNIT, FMT = 9011, IOSTAT = IOS)
      
!--   IF condifiton, mais il ne verifie pas le IOSTAT
      READ (UNIT = FILE_UNIT, FMT = 9011, IOSTAT = IOS2) C_BUFFER
      IF (X .LT. 0) THEN
         WRITE(*,9003) '!!! Erreur lecture Fichier:', C_ARG, 'Enreg:', COUNT, 'IOS=', IOS, '!!!'
         CLOSE (UNIT = FILE_UNIT, IOSTAT=IOS)
         IF (IOS .LT. 0) THEN
            WRITE(*,9002) '!!! Err. Fermeture Fichier :', C_ARG, 'IOS=', IOS, '!!!'
         ENDIF
         STOP
      ENDIF
100   CONTINUE

! --- Fermeture du fichier 
      CLOSE (UNIT = FILE_UNIT, IOSTAT=IOS)
!
! ---------------------------------------------------------------------------
!     F O R M A T S 
! ---------------------------------------------------------------------------
!
9001  FORMAT(1X, A5, 1X, I3, 4X, A7, 1X, A32)
9011  FORMAT(A128)

!
! ---------------------------------------------------------------------------
!     F I N   D U   P R O G R A M M E  
! ---------------------------------------------------------------------------
!
      STOP
      END PROGRAM ESSAI
