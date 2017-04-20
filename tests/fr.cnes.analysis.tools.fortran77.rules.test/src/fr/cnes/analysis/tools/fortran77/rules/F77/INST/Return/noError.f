      PROGRAM ESSAI 
C
      INTEGER I
      INTEGER I_CARRE
      INTEGER I_CUBE
C
C --- Elevation au carre et au cube pour les entier positifs > 0
C
      I = 5
      CALL carre_cube ( I, I_CARRE, I_CUBE ) 
      IF ( I_CARRE .GT. 0) THEN
         WRITE(*,*) 'I,I^2,I^3=', I, I_CARRE, I_CUBE
      ELSE
         WRITE(*,*) 'I=', I, '!!! Bad value !!!'
      ENDIF

      END PROGRAM ESSAI 
C
C
C
      SUBROUTINE carre_cube (A, B, C, *)
      INTEGER A, B, C
C
      IF ( A .GT. 0 ) THEN
         B = A**2
         C = A**3
      ELSE
         B = -1
      ENDIF
C
      END SUBROUTINE carre_cube 
