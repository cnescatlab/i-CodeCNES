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

      CHARACTER*2 CHAINE
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
C --- Lecture d'une chaine quelconque sur STDIN, sortie sur STDOUT avec format 
C
      WRITE (I_STDOUT, 9001) 
      READ  (I_STDIN, *) CHAINE
      IF (CHAINE .NE. 'go') GO TO 999

C
C --- Appel de la commande ps -edf
C
      STATUS = SYSTEM(PS_COMMAND) 
      IF (STATUS .NE. 0) THEN
         WRITE(I_STDOUT,*) PS_COMMAND, '-> STATUS=', STATUS
      ENDIF 

C
C --- Lecture d'une chaine quelconque sur STDIN, sortie sur STDOUT avec format 
C
      WRITE (I_STDOUT, 9001) 
      READ  (I_STDIN, *) CHAINE
      IF (CHAINE .NE. 'go') GO TO 999

      WRITE(*,*) '--- Fin du programme ---'

999   CONTINUE

C
C     ------------------------------------------------------------------------------
C     F O R M A T S 
C     ------------------------------------------------------------------------------
9001  FORMAT('To resume execution, type go.  Other input will terminate &
     1the job.')

      END PROGRAM ESSAI
