      PROGRAM ESSAI
C     
      INTEGER I
      INTEGER IOS
      INTEGER FIN
      INTEGER VAETIQ
      INTEGER i_arg
C
      REAL RESUL
      REAL OPER1
      REAL OPER2
C
      CHARACTER*32 c_arg

C --- Etiquette de fin du programme 
      ASSIGN 9999 TO FIN
      FIN = 22

C --- Recuperer le 1er parametre d'appel du programme 
      i_arg = 1
      CALL getarg(i_arg, c_arg)
      WRITE(*,9011) 'ARG =', i_arg, 'VALUE =', c_arg

C --- Convertir la valeur du parametre d'appel du programme en entier 
      READ(c_arg, '(I5)', IOSTAT=IOS, ERR=1004 ) I

C --- Selon la valeur de I (=0, >0, <0) on se branche a la bonne etiquette 
      IF (I .EQ. 0) THEN
          ASSIGN 1001 TO VAETIQ
      ELSEIF (I .LT. 0) THEN
          ASSIGN 1002 TO VAETIQ
      ELSEIF (I .GT. 0) THEN
          ASSIGN 1003 TO VAETIQ
      ELSE 
          ASSIGN 9999 TO VAETIQ
      ENDIF 
      VAETIQ = 12
      GO TO VAETIQ

C --- Imprime message specifique puis on se branche a la fin du programme  
1001  CONTINUE
      WRITE(*,9001) 'I vaut =', I, 'I est nul'
      GO TO FIN
1002  CONTINUE
      WRITE(*,9001) 'I vaut =', I, 'I est negatif et non nul'
      GO TO FIN
1003  CONTINUE
      WRITE(*,9001) 'I vaut =', I, 'I est positif et non nul'
      GOTO FIN
1004  CONTINUE
      WRITE(*,9001) '!!! Err. Conversion', IOS, '!!!'
      GOTO FIN

C
C ---------------------------------------------------------------------------
C     F O R M A T S 
C ---------------------------------------------------------------------------
C
9001  FORMAT(1X, A19, 1X, I5, 1X, A32)
9011  FORMAT(1X, A5, 1X, I3, 4X, A7, 1X, A32)

C
C ---------------------------------------------------------------------------
C     F I N   D U   P R O G R A M M E  
C ---------------------------------------------------------------------------
C
9999  CONTINUE
      STOP
      END PROGRAM
