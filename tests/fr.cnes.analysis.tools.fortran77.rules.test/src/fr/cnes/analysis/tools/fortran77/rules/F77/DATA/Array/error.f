C
C --- 
C
      PROGRAM ESSAI 

      REAL X(5) 
      REAL MOY, MOY_SUB

      DATA X / 1., 2., 3., 4., 5. /

C
C --- Appel correct : 5 elements a moyenner 
C
      PRINT *, 'Tableau : ', X
      PRINT *, 'Moyenne (Fonction) : ', MOY(X,500)

C 
C --- Appel incorrect : demande la moyenne sur les 500 premier elements du tableau 
C ---    alors que celui-ci ne comporte que 5 elements
C --- On obtient NaN
C
      CALL MOYENNE(X, 500, MOY_SUB)
      PRINT *, 'Tableau : ', X
      PRINT *, 'Moyenne (Routine) : ', MOY_SUB

      STOP
      END PROGRAM ESSAI 

C
C --- 
C
      REAL FUNCTION MOY(X, NB) 
      INTEGER I
      INTEGER NB
      REAL    SOMME
C
C --- L'utilisation du caractere * pour definir le nombre d'elements du tableau X, 
C ---   ne permet pas (erreur de compilation) d'en calculer sa taille (appel de la fonction SIZE) 
C 
      REAL    X(*)

      SOMME = X(1)
      DO I = 2, NB
         SOMME = SOMME + X(I)
C
C ---    La fonction intrinseque ISNAN (compilateur GNU) permet de detecter que 
C ---       la variable SOMME n'est plus un nombre (NaN : Not a Number) 
C ---    Cela indique uqu'il y a un débordement 
C
         IF (ISNAN(SOMME)) THEN
            PRINT *, 'AT indice : ', I, ' then ', SOMME, ' is a NaN'
            MOY = -1.0
            RETURN
         ENDIF
      END DO
      MOY = SOMME / NB
      RETURN
      END FUNCTION MOY

C
C --- 
C
      SUBROUTINE MOYENNE(X, NB, MOY) 
      INTEGER I
      INTEGER NB
      REAL    MOY
      REAL    SOMME 
      REAL    X(5,*)
C- 	Error
	  REAL	  X(*,5,*)
	  
	  REAL X(5)

      SOMME = X(1)
      DO I = 2, NB
         SOMME = SOMME + X(I)
      END DO
      MOY = SOMME / NB
      END SUBROUTINE MOYENNE 
