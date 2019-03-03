      PROGRAM ESSAI
C
C --- Le principe est ici d'appeler la commande qui permet de lister le contenu 
C --- du repertoire courant puis de se mettre en attente d'une saisie sur l'entree standard 
C --- puis d'appeler la commande qui permet de lister les processus actifs puis 
C --- de se mettre en attente de saisie sur l'entree standard, pusi d'indiquer 
C --- que le programme est termine  
C
      INTEGER      I_STDIN
      INTEGER      I_STDOUT
      INTEGER      I_STDERR
      INTEGER      STATUS

      CHARACTER*1 CHAINE
      CHARACTER*32 LS_COMMAND
      CHARACTER*32 PS_COMMAND

C
C --- Valeur par defaut des 3 unites E/S standard
C
      PARAMETER (I_STDIN = 5, I_STDOUT = 6, I_STDERR = 0)
C
C --- Les commandes externes appelees par le module 
C
      PARAMETER (LS_COMMAND = 'ls -altr', PS_COMMAND = 'ps -edf')

C
C --- Appel de la commande ls -altr 
C
      STATUS = SYSTEM(LS_COMMAND) 
      IF (STATUS .NE. 0) THEN
         WRITE(I_STDOUT,*) LS_COMMAND, '-> STATUS=', STATUS
      ENDIF 

C 
C --- On se suspend en attente d'une action de l'operateur
C
      PAUSE 1

C
C --- Appel de la commande ps -edf
C
      STATUS = SYSTEM(PS_COMMAND) 
      IF (STATUS .NE. 0) THEN
         WRITE(I_STDOUT,*) PS_COMMAND, '-> STATUS=', STATUS
      ENDIF 

C 
C --- On se suspend en attente d'une action de l'operateur
C
      PAUSE 2

      WRITE(*,*) '--- Fin du programme ---'

      END PROGRAM ESSAI
