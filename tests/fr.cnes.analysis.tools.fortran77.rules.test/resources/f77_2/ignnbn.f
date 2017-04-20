      INTEGER FUNCTION ignnbn(n,p)
C**********************************************************************
C
C     INTEGER FUNCTION IGNNBN( N, P )
C
C                GENerate Negative BiNomial random deviate
C
C
C                              Function
C
C
C     Generates a single random deviate from a negative binomial
C     distribution.
C
C
C                              Arguments
C
C
C     N  --> Required number of events.
C                              INTEGER N
C
C     P  --> The probability of an event during a Bernoulli trial.
C                              REAL P
C
C
C
C                              Method
C
C
C     Algorithm from page 480 of
C
C     Devroye, Luc
C
C     Non-Uniform Random Variate Generation.  Springer-Verlag,
C     New York, 1986.
C
C**********************************************************************
C     ..
C     .. Scalar Arguments ..
      REAL p
      INTEGER n
C     ..
C     .. Local Scalars ..
      REAL y,a,r
C     ..
C     .. External Functions ..
      REAL gengam
      INTEGER ignpoi
      EXTERNAL gengam,ignpoi
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC real
C     ..
C     .. Executable Statements ..
C     Check Arguments
      IF (n.LT.0) STOP 'N < 0 in IGNNBN'
      IF (p.LE.0.0) STOP 'P <= 0 in IGNNBN'
      IF (p.GE.1.0) STOP 'P >= 1 in IGNNBN'

C     Generate Y, a random gamma (n,(1-p)/p) variable
      r = real(n)
      a = p/ (1.0-p)
      y = gengam(a,r)

C     Generate a random Poisson(y) variable
      ignnbn = ignpoi(y)
      RETURN

      END
