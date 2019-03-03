      SUBROUTINE stat(x,n,av,var,xmin,xmax)
C**********************************************************************
C
C     SUBROUTINE STAT( X, N, AV, VAR)
C
C               compute STATistics
C
C
C                              Function
C
C
C     Computes AVerage and VARiance of array X(N).
C
C**********************************************************************
C     .. Scalar Arguments ..
      REAL av,var,xmax,xmin
      INTEGER n
C     ..
C     .. Array Arguments ..
      REAL x(n)
C     ..
C     .. Local Scalars ..
      REAL sum
      INTEGER i
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC real
C     ..
C     .. Executable Statements ..
      xmin = x(1)
      xmax = x(1)
      sum = 0.0
      DO 10,i = 1,n
          sum = sum + x(i)
          IF (x(i).LT.xmin) xmin = x(i)
          IF (x(i).GT.xmax) xmax = x(i)
   10 CONTINUE
      av = sum/real(n)
      sum = 0.0
      DO 20,i = 1,n
          sum = sum + (x(i)-av)**2
   20 CONTINUE
      var = sum/real(n-1)
      RETURN

      END
      PROGRAM tstall
      IMPLICIT LOGICAL (q)
C     Interactive test for PHRTSD
C     .. Parameters ..
      INTEGER mxwh
      PARAMETER (mxwh=10)
C     ..
C     .. Local Scalars ..
      REAL av,avtr,var,vartr,xmin,xmax
      INTEGER i,is1,is2,itmp,iwhich,j,mxint,nperm,nrep,ntot
      CHARACTER type*4,phrase*100
C     ..
C     .. Local Arrays ..
      REAL array(1000),param(3)
      INTEGER iarray(1000),perm(500)
C     ..
C     .. External Functions ..
      REAL genbet,genchi,genf,gennch,gennf,genunf
      INTEGER ignuin
      EXTERNAL genbet,genchi,genf,gennch,gennf,genunf,ignuin
C     ..
C     .. External Subroutines ..
      EXTERNAL genprm,phrtsd,setall,stat,trstat
C     ..
C     .. Equivalences ..
      EQUIVALENCE (array,iarray)
C     ..
C     .. Executable Statements ..
      WRITE (*,9000)

 9000 FORMAT (' Tests most generators of specific distributions.'/
     +       ' Generates 1000 deviates: reports mean and variance.'/
     +       ' Also reports theoretical mean and variance.'/
     +       ' If theoretical mean or var doesn''t exist prints -1.'/
     +       ' For permutations, generates one permutation of 1..n'/
     +       '     and prints it.'/
     +       ' For uniform integers asks for upper bound, number of'/
     +       '     replicates per integer in 1..upper bound.'/
     +       '     Prints table of num times each integer generated.')
C
C     Menu for choosing tests
C
   10 WRITE (*,9010)

 9010 FORMAT (' Enter number corresponding to choice:'/
     +       '      (0) Exit this program'/
     +       '      (1) Generate Chi-Square deviates'/
     +       '      (2) Generate noncentral Chi-Square deviates'/
     +       '      (3) Generate F deviates'/
     +       '      (4) Generate noncentral F  deviates'/
     +       '      (5) Generate random permutation'/
     +       '      (6) Generate uniform integers'/
     +       '      (7) Generate uniform reals'/
     +       '      (8) Generate beta deviates'/
     +       '      (9) Generate binomial outcomes'/
     +       '     (10) Generate Poisson ourcomes'/)

      READ (*,*) iwhich
      IF (.NOT. (iwhich.LT.0.OR.iwhich.GT.mxwh)) GO TO 20
      WRITE (*,*) ' Choices are 1..',mxwh,' - try again.'
      GO TO 10

   20 IF (iwhich.EQ.0) STOP ' Normal termination rn tests'
      WRITE (*,*) ' Enter phrase to initialize rn generator'
      READ (*,'(a)') phrase
      CALL phrtsd(phrase,is1,is2)
      CALL setall(is1,is2)

      IF ((1).NE. (iwhich)) GO TO 40
C
C     Chi-square deviates
C
      type = 'chis'
      WRITE (*,*) ' Enter (real) df for the chi-square generation'
      READ (*,*) param(1)
      DO 30,i = 1,1000
          array(i) = genchi(param(1))
   30 CONTINUE
      CALL stat(array,1000,av,var,xmin,xmax)
      CALL trstat(type,param,avtr,vartr)
      WRITE (*,9020) av,avtr,var,vartr,xmin,xmax

 9020 FORMAT (' Mean Generated: ',T30,G15.7,5X,'True:',T60,
     +       G15.7/' Variance Generated:',T30,G15.7,5X,'True:',T60,
     +       G15.7/' Minimun: ',T30,G15.7,5X,'Maximum:',T60,G15.7)

      GO TO 280

   40 IF ((2).NE. (iwhich)) GO TO 60

C
C     Noncentral Chi-square deviates
C
      type = 'ncch'
      WRITE (*,*) ' Enter (real) df'
      WRITE (*,*) '       (real) noncentrality parameter'
      READ (*,*) param(1),param(2)
      DO 50,i = 1,1000
          array(i) = gennch(param(1),param(2))
   50 CONTINUE
      CALL stat(array,1000,av,var,xmin,xmax)
      CALL trstat(type,param,avtr,vartr)
      WRITE (*,9020) av,avtr,var,vartr,xmin,xmax
      GO TO 280

   60 IF ((3).NE. (iwhich)) GO TO 80

C
C     F deviates
C
      type = 'f'
      WRITE (*,*) ' Enter (real) df of the numerator'
      WRITE (*,*) '       (real) df of the denominator'
      READ (*,*) param(1),param(2)
      DO 70,i = 1,1000
          array(i) = genf(param(1),param(2))
   70 CONTINUE
      CALL stat(array,1000,av,var,xmin,xmax)
      CALL trstat(type,param,avtr,vartr)
      WRITE (*,9020) av,avtr,var,vartr,xmin,xmax
      GO TO 280

   80 IF ((4).NE. (iwhich)) GO TO 100


C
C     Noncentral F deviates
C
      type = 'ncf'
      WRITE (*,*) ' Enter (real) df of the numerator'
      WRITE (*,*) '       (real) df of the denominator'
      WRITE (*,*) '       (real) noncentrality parameter'
      READ (*,*) param(1),param(2),param(3)
      DO 90,i = 1,1000
          array(i) = gennf(param(1),param(2),param(3))
   90 CONTINUE
      CALL stat(array,1000,av,var,xmin,xmax)
      CALL trstat(type,param,avtr,vartr)
      WRITE (*,9020) av,avtr,var,vartr,xmin,xmax
      GO TO 280

  100 IF ((5).NE. (iwhich)) GO TO 140

C
C     Random permutation
C
  110 WRITE (*,*) ' Enter size of permutation'
      READ (*,*) nperm
      IF (.NOT. (nperm.LT.1.OR.nperm.GT.500)) GO TO 120
      WRITE (*,*) ' Permutation size must be between 1 and 500 ',
     +  '- try again!'
      GO TO 110

  120 WRITE (*,*) '       Random Permutation Generated - Size',nperm
      DO 130,i = 1,500
          perm(i) = i
  130 CONTINUE
      CALL genprm(perm,nperm)
      WRITE (*,*) ' Perm Generated'
      WRITE (*,'(20I4)') (perm(i),i=1,nperm)
      GO TO 280

  140 IF ((6).NE. (iwhich)) GO TO 170

C
C     Uniform integer
C
      WRITE (*,*) ' Enter maximum uniform integer'
      READ (*,*) mxint
      WRITE (*,*) ' Enter number of replications per integer'
      READ (*,*) nrep
      DO 150,i = 1,1000
          iarray(i) = 0
  150 CONTINUE
      ntot = mxint*nrep
      DO 160,i = 1,ntot
          itmp = ignuin(1,mxint)
          iarray(itmp) = iarray(itmp) + 1
  160 CONTINUE
      WRITE (*,*) '         Counts of Integers Generated'
      WRITE (*,'(20I4)') (iarray(j),j=1,mxint)
      GO TO 280

  170 IF ((7).NE. (iwhich)) GO TO 190

C
C     Uniform real
C
      type = 'unif'
      WRITE (*,*) ' Enter Low then High bound for uniforms'
      READ (*,*) param(1),param(2)
      DO 180,i = 1,1000
          array(i) = genunf(param(1),param(2))
  180 CONTINUE
      CALL stat(array,1000,av,var,xmin,xmax)
      CALL trstat(type,param,avtr,vartr)
      WRITE (*,9020) av,avtr,var,vartr,xmin,xmax
      GO TO 280

  190 IF ((8).NE. (iwhich)) GO TO 210

C
C     Beta deviate
C
      type = 'beta'
      WRITE (*,*) ' Enter A, B for Beta deviate'
      READ (*,*) param(1),param(2)
      DO 200,i = 1,1000
          array(i) = genbet(param(1),param(2))
  200 CONTINUE
      CALL stat(array,1000,av,var,xmin,xmax)
      CALL trstat(type,param,avtr,vartr)
      WRITE (*,9020) av,avtr,var,vartr,xmin,xmax
      GO TO 280

  210 IF ((9).NE. (iwhich)) GO TO 240
C
C     Binomial outcomes
C
      type = 'bin'
      WRITE (*,*) ' Enter number of trials, Prob event for ',
     +  'binomial outcomes'
      READ (*,*) ntry,pevt
      DO 220,i = 1,1000
          iarray(i) = ignbin(ntry,pevt)
  220 CONTINUE
      DO 230,i = 1,1000
          array(i) = iarray(i)
  230 CONTINUE
      CALL stat(array,1000,av,var,xmin,xmax)
      param(1) = ntry
      param(2) = pevt
      CALL trstat(type,param,avtr,vartr)
      WRITE (*,9020) av,avtr,var,vartr,xmin,xmax
      GO TO 280

  240 IF ((10).NE. (iwhich)) GO TO 270
C
C     Poisson outcomes
C
      type = 'pois'
      WRITE (*,*) ' Enter mean for Poisson generation'
      READ (*,*) param(1)
      DO 250,i = 1,1000
          iarray(i) = ignpoi(param(1))
  250 CONTINUE
      DO 260,i = 1,1000
          array(i) = iarray(i)
  260 CONTINUE
      CALL stat(array,1000,av,var,xmin,xmax)
      CALL trstat(type,param,avtr,vartr)
      WRITE (*,9020) av,avtr,var,vartr,xmin,xmax
      GO TO 280

  270 CONTINUE
  280 GO TO 10

      END
      SUBROUTINE trstat(type,parin,av,var)
      IMPLICIT INTEGER (i-n),REAL (a-h,o-p,r-z),LOGICAL (q)
C**********************************************************************
C
C     SUBROUTINE TRSTAT( TYPE, PARIN, AV, VAR )
C               TRue STATistics
C
C     Returns mean and variance for a number of statistical distribution
C     as a function of their parameters.
C
C
C                              Arguments
C
C
C     TYPE --> Character string indicating type of distribution
C             'chis' chisquare
C             'ncch' noncentral chisquare
C             'f'    F (variance ratio)
C             'ncf'  noncentral f
C             'unif' uniform
C             'beta' beta distribution
C                         CHARACTER*(4) TYPE
C
C     PARIN --> Array containing parameters of distribution
C              chisquare
C               PARIN(1) is df
C              noncentral chisquare
C               PARIN(1) is df
C               PARIN(2) is noncentrality parameter
C              F (variance ratio)
C               PARIN(1) is df numerator
C               PARIN(2) is df denominator
C              noncentral F
C               PARIN(1) is df numerator
C               PARIN(2) is df denominator
C               PARIN(3) is noncentrality parameter
C              uniform
C               PARIN(1) is LOW bound
C               PARIN(2) is HIGH bound
C              beta
C               PARIN(1) is A
C               PARIN(2) is B
C                         REAL PARIN(*)
C              binonial
C               PARIN(1) is Number of trials
C               PARIN(2) is Prob Event at Each Trial
C              poisson
C               PARIN(1) is Mean
C
C     AV <-- Mean of specified distribution with specified parameters
C                         REAL AV
C
C     VAR <-- Variance of specified distribution with specified paramete
C                         REAL VAR
C
C
C                              Note
C
C
C     AV and Var will be returned -1 if mean or variance is infinite
C
C**********************************************************************
C     .. Scalar Arguments ..
      REAL av,var
      CHARACTER type* (4)
C     ..
C     .. Array Arguments ..
      REAL parin(*)
C     ..
C     .. Local Scalars ..
      REAL a,b,range
C     ..
C     .. Executable Statements ..
      IF (('chis').NE. (type)) GO TO 10
      av = parin(1)
      var = 2.0*parin(1)
      GO TO 170

   10 IF (('ncch').NE. (type)) GO TO 20
      a = parin(1) + parin(2)
      b = parin(2)/a
      av = a
      var = 2.0*a* (1.0+b)
      GO TO 170

   20 IF (('f').NE. (type)) GO TO 70
      IF (.NOT. (parin(2).LE.2.0001)) GO TO 30
      av = -1.0
      GO TO 40

   30 av = parin(2)/ (parin(2)-2.0)
   40 IF (.NOT. (parin(2).LE.4.0001)) GO TO 50
      var = -1.0
      GO TO 60

   50 var = (2.0*parin(2)**2* (parin(1)+parin(2)-2.0))/
     +      (parin(1)* (parin(2)-2.0)**2* (parin(2)-4.0))
   60 GO TO 170

   70 IF (('ncf').NE. (type)) GO TO 120
      IF (.NOT. (parin(2).LE.2.0001)) GO TO 80
      av = -1.0
      GO TO 90

   80 av = (parin(2)* (parin(1)+parin(3)))/ ((parin(2)-2.0)*parin(1))
   90 IF (.NOT. (parin(2).LE.4.0001)) GO TO 100
      var = -1.0
      GO TO 110

  100 a = (parin(1)+parin(3))**2 + (parin(1)+2.0*parin(3))*
     +    (parin(2)-2.0)
      b = (parin(2)-2.0)**2* (parin(2)-4.0)
      var = 2.0* (parin(2)/parin(1))**2* (a/b)
  110 GO TO 170

  120 IF (('unif').NE. (type)) GO TO 130
      range = parin(2) - parin(1)
      av = parin(1) + range/2.0
      var = range**2/12.0
      GO TO 170

  130 IF (('beta').NE. (type)) GO TO 140
      av = parin(1)/ (parin(1)+parin(2))
      var = (av*parin(2))/ ((parin(1)+parin(2))*
     +      (parin(1)+parin(2)+1.0))
      WRITE (*,*) ' A, B, AV, VAR ',parin(1),parin(2),av,var
      GO TO 170

  140 IF (('bin').NE. (type)) GO TO 150
      av = parin(1)*parin(2)
      var = av* (1.0-parin(2))
      GO TO 170

  150 IF (('pois').NE. (type)) GO TO 160
      av = parin(1)
      var = parin(1)
      GO TO 170

  160 WRITE (*,*) 'Unimplemented type ',type
      STOP 'Unimplemented type in TRSTAT'

  170 RETURN

      END
