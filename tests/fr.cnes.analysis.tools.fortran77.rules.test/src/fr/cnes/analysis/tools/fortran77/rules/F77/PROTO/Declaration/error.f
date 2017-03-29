C
C --- MAIN
C
      PROGRAM ESSAI

C --- Declaration des variables 
C     INTEGER I_UID
C     INTEGER I_STDOUT
C     REAL    R_CIRCLE
C     REAL    R_RADIUS

C --- Declaration des fonctions externes 
C     INTEGER MY_GETUID
C     EXTERNAL MY_GETUID
C     REAL    MY_RADIUS
C     EXTERNAL MY_RADIUS

C --- Initialisation des variables 
      I_STDOUT = 6
      R_CIRCLE = 28.26

C --- Mise en oeuvre de la fonction : my_getuid 
      WRITE(I_STDOUT, 10)
      I_UID = MY_GETUID()
      WRITE(I_STDOUT, *) 'UID =', I_UID

C --- Mise en oeuvre de la fonction : my_circle 
      WRITE(I_STDOUT, 20)
      R_RADIUS = MY_RADIUS(R_CIRCLE)
      WRITE(I_STDOUT, *) 'R_CIRCLE =', R_CIRCLE, 'R_RADIUS=', R_RADIUS

C
C --------------------------------------------------------------------------
C      F O R M A T S
C --------------------------------------------------------------------------
C
10    FORMAT(1X, '--- Recuperer le User Id ---')
20    FORMAT(1X, '--- Calculer le rayon d un cercle ---')

      END PROGRAM

