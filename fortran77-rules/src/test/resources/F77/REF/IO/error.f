      PROGRAM ESSAI
C
C --- Le principe est ici de saisir un nombre sur l'entree standard puis de 
C --- l'imprimer sur la sortie standard 
C
      REAL    NOMBRE
C
C --- Pour info, les valeurs par defaut des 3 unites E/S standard : 
C ---    . stdin = 5
C ---    . stdout = 6
C ---    . stderr = 0
C

C
C --- Lecture d'un nombre quelconque sur STDIN, sortie sur STDOUT avec format 
C
      READ  (5, *) NOMBRE
      WRITE (6, '(1X, A13, F8.3)') 'NOMBRE SAISI=', NOMBRE

      END PROGRAM ESSAI
