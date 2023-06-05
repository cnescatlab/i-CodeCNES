      PROGRAM ESSAI

      INTEGER I_UID
      INTEGER I_STDOUT

      I_STDOUT = 6

      WRITE(I_STDOUT, 10)
C
C --- En ne mettant pas de parentheses a MY_GETUID, le compilateur comprend que 
C --- c'est la variable MY_GETUID qui est adressee . Par defaut, cette variable n'ayant 
C --- pas ete declaree explicitement (elle est bien de type INTEGER car commence par lettre M) 
C --- mais n'ayant pas ete initialisee, elle a un contenu indefini .
C --- Le resultat est que I_UID a lui aussi un contenu indefini .
C
      I_UID = MY_GETUID
      WRITE(I_STDOUT, *) 'UID =', I_UID
C
C --------------------------------------------------------------------------
C      F O R M A T S
C --------------------------------------------------------------------------
C
10    FORMAT(1X, '--- Recuperer le User Id ---')

      END PROGRAM