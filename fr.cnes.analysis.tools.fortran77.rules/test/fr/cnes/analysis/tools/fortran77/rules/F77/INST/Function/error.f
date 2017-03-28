C
C --- La fonction MY_SQUARE est declaree de maniere implicite (son nom commence par M)
C --- Elle est donc consideree par le compilateur comme de type INTEGER 
C --- Il suffirait de lui changer son nom MY_SQUARE -> P_SQUARE pour que la fonction
C --- soit consideree comme etant de type REAL et risque de renvoyer des donnees erronees
C
      FUNCTION MY_SQUARE(I_PAR)

      INTEGER I_PAR

      INTEGER I_RESULT
 
      I_RESULT = I_PAR ** 2
      WRITE(*,*) 'I_PAR=', I_PAR, 'I_RESULT=', I_RESULT

      MY_SQUARE = I_RESULT
      WRITE(*,*) 'MY_SQUARE=', MY_SQUARE

      RETURN
      END
C
C --- MAIN
C
      PROGRAM ESSAI

      INTEGER I_VALUE
      INTEGER I_SQUARE
      INTEGER I_STDOUT

      PARAMETER (I_STDOUT = 6)

      I_VALUE = 8

      WRITE(I_STDOUT, 10)
      I_SQUARE = MY_SQUARE(I_VALUE)
      WRITE(I_STDOUT, *) 'I_VALUE =', I_VALUE, 'I_SQUARE=', I_SQUARE

C
C --------------------------------------------------------------------------
C      F O R M A T S
C --------------------------------------------------------------------------
C
10    FORMAT(1X, '--- Elevation d une valeur entiere au carre ---')

      END PROGRAM

