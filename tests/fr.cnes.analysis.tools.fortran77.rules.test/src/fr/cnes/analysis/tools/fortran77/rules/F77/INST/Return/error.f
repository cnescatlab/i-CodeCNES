      PROGRAM ESSAI 
C
      INTEGER I
      INTEGER I_CARRE
      INTEGER I_CUBE
C
C --- Elevation au carre et au cube 
C
      I = 5
      CALL carre_cube ( I, I_CARRE, I_CUBE, *10 ) 
      WRITE(*,*) 'I,I^2,I^3=', I, I_CARRE, I_CUBE
      GO TO 99
C
10    CONTINUE
      WRITE(*,*) 'I=', I, '!!! Bad value !!!'

99    CONTINUE

      END PROGRAM ESSAI 
C
C
C
      SUBROUTINE carre_cube (A, B, C, *)
      INTEGER A, B, C
C
      IF ( A .LE. 0 ) THEN
         RETURN(1)
      ENDIF
C
      B = A**2
      C = A**3
C
      END SUBROUTINE carre_cube 
