      INTEGER FUNCTION SIN()

      INTEGER STATUS

      CHARACTER*32 ID_COMMAND

      PARAMETER ( ID_COMMAND = 'id -u' )
 
      STATUS = SYSTEM (ID_COMMAND)
      WRITE(*,*) 'STATUS=', STATUS

      GETUID = STATUS
      WRITE(*,*) 'GETUID=', GETUID

      RETURN
      END
C
C --- MAIN
C
      PROGRAM ESSAI

      INTEGER I_UID
      INTEGER I_STDOUT

      I_STDOUT = 6

      WRITE(I_STDOUT, 10)
      I_UID = GETUID()
      WRITE(I_STDOUT, *) 'UID =', I_UID
C
C --------------------------------------------------------------------------
C      F O R M A T S
C --------------------------------------------------------------------------
C
10    FORMAT(1X, '--- Recuperer le User Id ---')

      END PROGRAM

