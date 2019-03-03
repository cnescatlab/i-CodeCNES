      REAL FUNCTION genchi(df)
C**********************************************************************
C
C     REAL FUNCTION GENCHI( DF )
C                Generate random value of CHIsquare variable
C
C
C                              Function
C
C
C     Generates random deviate from the distribution of a chisquare
C     with DF degrees of freedom random variable.
C
C
C                              Arguments
C
C
C     DF --> Degrees of freedom of the chisquare
C            (Must be positive)
C                         REAL DF
C
C
C                              Method
C
C
C     Uses relation between chisquare and gamma.
C
C**********************************************************************
C     .. Scalar Arguments ..
      REAL df
C     ..
C     .. External Functions ..
      REAL gengam
      EXTERNAL gengam
C     ..
C     .. Executable Statements ..
      IF (.NOT. (df.LE.0.0)) GO TO 10
      WRITE (*,*) 'DF <= 0 in GENCHI - ABORT'
      WRITE (*,*) 'Value of DF: ',df
      STOP 'DF <= 0 in GENCHI - ABORT'

   10 genchi = 2.0*gengam(1.0,df/2.0)
      RETURN

      END
