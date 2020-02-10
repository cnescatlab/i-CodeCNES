      PROGRAM ESSAI
C     
      INTEGER I
      INTEGER IOS
      INTEGER i_arg
C
      CHARACTER*32 c_arg

C --- Recuperer le 1er parametre d'appel du programme 
      i_arg = 1
      CALL getarg(i_arg, c_arg)
      WRITE(*,9011) 'ARG =', i_arg, 'VALUE =', c_arg

C --- Convertir la valeur du parametre d'appel du programme en entier 
      READ(c_arg, '(I5)', IOSTAT=IOS ) I

C --- Y a t'il eu une erreur de conversion 
      IF (IOS .EQ. 0) THEN 

C --- Selon la valeur de I (=0, >0, <0) on se branche a la bonne etiquette 
         IF (I .EQ. 0) THEN
             WRITE(*,9001) 'I vaut =', I, 'I est nul'
         ELSEIF (I .LT. 0) THEN
             WRITE(*,9001) 'I vaut =', I, 'I est negatif et non nul'
         ELSEIF (I .GT. 0) THEN
             WRITE(*,9001) 'I vaut =', I, 'I est positif et non nul'
         ENDIF 

C --- !!! Erreur de conversion, pas un entier numerique !!! 
      ELSE

         WRITE(*,9001) '!!! Err. Conversion', IOS, '!!!'

      ENDIF

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
      STOP
      END PROGRAM ESSAI
