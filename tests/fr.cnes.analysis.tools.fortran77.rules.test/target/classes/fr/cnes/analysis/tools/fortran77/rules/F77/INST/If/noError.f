      PROGRAM ESSAI

      INTEGER I_ARG
      INTEGER X

      CHARACTER*32 C_ARG

C --- Recuperer le 1er parametre d'appel du programme
      I_ARG = 1
      CALL getarg(I_ARG, C_ARG)
      WRITE(*,*) 'ARG =', I_ARG, 'VALUE =', C_ARG

C --- Convertir la valeur du parametre d'appel du programme en entier
      READ(C_ARG, '(I5)', IOSTAT=IOS, ERR=1004 ) X

      WRITE(*,*) 'X=', X

      IF (X .LT. 0) THEN
         WRITE(*,*) '===> x is negative'
      ELSEIF (X .EQ. 0) THEN
         WRITE(*,*) '===> x is null'
      ELSE
         WRITE(*,*) '===> x is positive'
      ENDIF
      GO TO 9999

1004  CONTINUE
      WRITE(*,*) '!!! Err. Conversion ARG=', C_ARG, 'IOS=', IOS, '!!!'

9999  CONTINUE

      STOP
      END PROGRAM ESSAI
