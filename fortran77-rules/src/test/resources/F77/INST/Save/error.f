C
C ---  
C
      SUBROUTINE KM_RESTANTS(KILOMETRES) 
      IMPLICIT NONE
      INTEGER KILOMETRES
      INTEGER LAST
      COMMON /ARG/ LAST 

      IF ( LAST .EQ. 0) THEN
         LAST = KILOMETRES
      ELSE
         LAST = LAST - KILOMETRES 
      ENDIF
      WRITE(UNIT=*, FMT=*) 'Il reste ', LAST, ' kilometres a parcourir.' 
      END 
C
C --- MAIN
C
      PROGRAM ESSAI

      IMPLICIT NONE

C --- Declaration des variables 
      INTEGER I_STDOUT
      INTEGER KILOMETRES 
      INTEGER LAST

C --- Declaration du common  
      COMMON /ARG/ LAST
      SAVE LAST 
      DATA LAST /0/ 

C --- Initialisation des variables 
      I_STDOUT = 6
      KILOMETRES = 720 

C
C --- Mise en oeuvre de la fonction : extra 
C ---    1er appel : Distance totale a parcourir
C
      WRITE(I_STDOUT, 10)
      READ(UNIT=*, FMT=*) KILOMETRES
      CALL KM_RESTANTS(KILOMETRES)

C 
C --- Distance intermediaire parcourue : tant qu'il y a des kms a parcourir 
C
      DO WHILE (KILOMETRES .GT.0) 
         WRITE(I_STDOUT, 20)
         READ(UNIT=*, FMT=*) KILOMETRES
         CALL KM_RESTANTS(KILOMETRES)
      END DO

C
C --------------------------------------------------------------------------
C      F O R M A T S
C --------------------------------------------------------------------------
C
10    FORMAT(1X, '--- Saisir distance totale a parcourir ---')
20    FORMAT(1X, '--- Saisir distance partielle parcourue ---')

      END PROGRAM

