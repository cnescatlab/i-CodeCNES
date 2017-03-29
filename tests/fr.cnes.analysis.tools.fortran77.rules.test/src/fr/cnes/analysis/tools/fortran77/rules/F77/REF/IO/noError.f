      PROGRAM ESSAI
C
C --- Le principe est ici de saisir un nombre sur l'entree standard puis de l'imprimer 
C --- sur la sortie standard 
C
      INTEGER I_STDIN
      INTEGER I_STDOUT
      INTEGER I_STDERR

      REAL    NOMBRE
C
C --- Valeur par defaut des 3 unites E/S standard
C
      PARAMETER (I_STDIN = 5, I_STDOUT = 6, I_STDERR = 0)

C
C --- Lecture d'un nombre quelconque sur STDIN, sortie sur STDOUT avec format 
C
      READ  (I_STDIN, *) NOMBRE
      WRITE (I_STDOUT, '(1X, A13, F8.3)') 'NOMBRE SAISI=', NOMBRE

      END PROGRAM ESSAI
