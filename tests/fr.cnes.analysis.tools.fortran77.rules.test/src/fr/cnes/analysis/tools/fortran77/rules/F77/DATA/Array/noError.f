C
C --- 
C
      PROGRAM ESSAI 

      REAL X(5) 
      REAL MOY, MOY_SUB

      DATA X / 1., 2., 3., 4., 5. /

      PRINT *, 'Tableau : ', X
      PRINT *, 'Moyenne (Fonction) : ', MOY(X,5)

C --- Appel incorrect avec en parametre un nombre d'elements qui deborde
      CALL MOYENNE(X, 500, MOY_SUB)
      PRINT *, 'Tableau : ', X
      PRINT *, 'Moyenne (Routine) : ', MOY_SUB

C --- Appel correct avec en parametre le nombre d'elements du tableau 
      CALL MOYENNE(X, 5, MOY_SUB)
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
      INTEGER NB_OF_ARRAY_ELTS
      REAL    SOMME
      REAL    X(5)

C --- On demande au systeme le nombre d'elements du tableau
      NB_OF_ARRAY_ELTS = SIZE(X)

C
C --- On verifie que le nombre d'elements sur laquelle la moyenne va etre 
C ---    calculee, ne deborde pas 
C 
      IF ( NB .LE. NB_OF_ARRAY_ELTS) THEN 
         SOMME = X(1)
         DO I = 2, NB
            SOMME = SOMME + X(I)
         END DO
         MOY = SOMME / NB
      ELSE
         MOY = -1.0
      ENDIF
      END FUNCTION MOY

C
C --- 
C
      SUBROUTINE MOYENNE(X, NB, MOY) 
      INTEGER I
      INTEGER NB
      INTEGER NB_OF_ARRAY_ELTS
      REAL    SOMME 
      REAL    MOY
C --- This is possible :-)
      REAL    X(5,*,*)

      NB_OF_ARRAY_ELTS = SIZE(X)

      IF ( NB .LE. NB_OF_ARRAY_ELTS) THEN 
         SOMME = X(1)
         DO I = 2, NB
            SOMME = SOMME + X(I)
         END DO
         MOY = SOMME / NB
      ELSE
         MOY = -1.0
      ENDIF
      END SUBROUTINE MOYENNE 
