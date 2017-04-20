        COMMON /STDIO/ IN, OUT
        INTEGER IN, OUT
        COMMON /PGNUM/PGNUMB
        INTEGER       PGNUMB
        COMMON /ROUNDN/  R1, R2, R3, R4, STICKY
        REAL R1, R2, R3, R4, STICKY
C
        COMMON /I3TYPE/ IEEE
        INTEGER         IEEE
C        IEEE   ... FLAG WHETHER GRADUAL UNDERFLOWS ARE DOUBLY ROUNDED
C
        COMMON /GLOBAL/ FAILS, SDEFCT, DEFECT, FLAWS, RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
        INTEGER         FAILS, SDEFCT, DEFECT, FLAWS
        REAL                              RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
        LOGICAL FLAGF
C
C        FAILS  ... NUMBER OF FAILURES
C        SDEFCT ... NUMBER OF SERIOUS DEFECTS
C        DEFECT ... NUMBER OF DEFECTS
C        FLAWS  ... NUMBER OF FLAWS
C
C        RADIX  ... COMPUTED RADIX OF THE MACHINE
C        ULPPLS ... ONE UNIT IN THE LAST PLACE (ULP) OF 1+EPSILON
C        ULPMIN ... ONE UNIT IN THE LAST PLACE (ULP) OF 1-EPSILON
C        PRECIS ... COMPUTED PRECISION OF THE MACHINE
C        W      ... RADIX ** PRECIS
C        MULGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN MULTIPLY
C        DIVGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN DIVIDE
C        SUBGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN ADD/SUBTRACT
C        A1     ...
C        ONEMIN ... ONE MINUS ULPMIN = ~.99999999999999
C
        COMMON /CONST/   FP0, FP1, FP2, FP3, FP4, FP8, FP9, FP27, FP32,
     ~                   HALF, MINUS1
        REAL FP0, FP1, FP2, FP3, FP4, FP8, FP9, FP27, FP32,
     ~                   HALF, MINUS1
C
        COMMON /ERRDAT/ OVRFLW, UNDFLW, DIVZER, ERRFLG
        INTEGER         OVRFLW, UNDFLW, DIVZER, ERRFLG
C        OVRFLW ... NUMBER OF OVERFLOWS SINCE LAST RESET
C        UNDFLW ... NUMBER OF UNDERFLOWS SINCE LAST RESET
C        DIVZER ... NUMBER OF DIVISIONS BY ZERO SINCE LAST RESET
C        ERRFLG ... FLAG THAT IS SET WHENEVER AN ERROR OCCURS
C
C
        COMMON /OVUNFL/  C1, H1, MINPOS, NULPS, UFLTHR, PHONY0
        REAL C1, H1, MINPOS, NULPS, UFLTHR, PHONY0
C        C1     ... 1/C ~= RADIX^LARGE_INTEGER
C        H1     ... MAX (2,RADIX)
C        MINPOS ... MINIMUM POSITIVE NUMBER FOUND BY MULT./DIVISION
C        NULPS  ... (SMALL INTEGER) * 1 UNIT IN LAST PLACE OF 1+ ... E9
C        UFLTHR ... THE UNDERFLOW THRESHOLD U0
C        PHONY0 ... CANDIDATE AND FINAL PHONY ZERO IF IT EXISTS. Z0
C
        INTEGER START
C        ... FLAG TO TELL WHETHER WE ARE RESTARTING OR
C        ... STARTING FROM SCRATCH
        INTEGER TEMP
        INTEGER MILES, NUMTRY, FROM
C        MILES  ... NUMBER OF MILESTONE REACHED SO FAR IN TESTING
C        NUMTRY ... NUMBER OF TIMES TO TRY RANDOM TRIALS
C        FROM   ... NUMBER OF MILESTONE TO RETURN TO ON RESTART
C
C
C
C DISABLE INTERUPTS FOR LAHEY F77L.
C       LOGICAL FLAG
C       CALL INVALOP(FLAG)
C       CALL OVEFL(FLAG)
C       CALL UNDFL(FLAG)
C       CALL DVCHK(FLAG)
C
C DISABLE INTERUPTS FOR MICROSOFT FORTRAN  (MASK MUST BE INTERGER*2)
C       INTEGER*2 MASK
C       MASK=4927
C       CALL LCWRQQ(MASK)
C DISABLE INTERUPTS FOR WATFIV
C       CALL TRAPS(100,100,100,100,100)
C NO SPECIAL CALL IS REQUIRED FOR FORTVS.
C
C
        FROM = 0
C
C IN = INPUT UNIT, OUT = OUTPUT UNIT -- YOU MAY HAVE TO CHANGE THESE.
        IN = 5
        OUT = 6
C THE FOLLOWING OPENS MAY BE NEEDED FOR SOME VERSIONS.
C       OPEN(IN,FILE='CON')
C       OPEN(OUT,FILE='CON')
C#######################################################################
C
C
C       CHECK FOR RESTART
C
C
C#######################################################################
        WRITE(OUT,100)
        WRITE(OUT,110)
100     FORMAT(' Is this a program restart after failure (1)')
110     FORMAT(' or a start from scratch (0) ?')
        READ(IN,120) START
120     FORMAT(I1)
        FLAGF = START .NE. 0
C  ***  FOR FORTRAN 66 AND FORTRAN 77 SUBSET, COMMENT OUT THE INQUIRE:
        INQUIRE(FILE='TST',EXIST=FLAGF)
        IF(FLAGF) THEN
            OPEN(3,FILE='TST',FORM='UNFORMATTED',STATUS='OLD')
            REWIND 3
            OPEN(4,FILE='LOG',FORM='UNFORMATTED',STATUS='OLD')
            REWIND 4
        ELSE
            OPEN(3,FILE='TST',FORM='UNFORMATTED',STATUS='NEW')
            OPEN(4,FILE='LOG',FORM='UNFORMATTED',STATUS='NEW')
        ENDIF
        IF (START .EQ. 0) GO TO 10000
        READ(4) FP0,FP1,FP2,FP3,FP4,FP8,FP9,FP27,FP32,HALF,MINUS1
        READ(4) FAILS,SDEFCT,DEFECT,FLAWS,RADIX,ULPPLS,ULPMIN,PRECIS
        READ(4) W, MULGRD,DIVGRD, SUBGRD,A1,ONEMIN
        READ(4) C1, H1, MINPOS, NULPS, UFLTHR, PHONY0,IEEE
        READ(4) R1,R2,R3,R4,STICKY
        READ(4) PGNUMB,MILES
        REWIND 4
        FROM = MILES
        WRITE(OUT,10001)FROM
10001   FORMAT(' Restarting from milestone ',I5,'.')
        IF (FROM .EQ.   7) GO TO   881
        IF (FROM .EQ.  79) GO TO  3959
        IF (FROM .EQ.  90) GO TO  3960
        IF (FROM .GE. 105 .AND. FROM .LE. 109) GO TO 10100
        IF (FROM .EQ. 115) GO TO 10100
        IF (FROM .GE. 120 .AND. FROM .LE. 125) GO TO 10100
        IF (FROM .EQ. 131) GO TO 10100
        IF (FROM .EQ. 161) GO TO 10200
        IF (FROM .GE. 201 .AND. FROM .LE. 205) GO TO 10200
        IF (FROM .EQ. 211) GO TO 10300
        IF (FROM .EQ. 212) GO TO 10300
        CALL BADMIL
C
C             FIRST TWO ASSIGNMENTS USE INTEGERS ON RIGHT HAND SIDE
C
10000   FP0 = 0
        FP1 = 1
910     FP2 = FP1 + FP1
960     FP3 = FP2 + FP1
        FP4 = FP3 + FP1
980     MINUS1 = -FP1
1000    HALF = FP1 / FP2
1100    FP8 = FP4 + FP4
        FP9 = FP3 * FP3
        FP27 = FP9 * FP3
        FP32 = FP8 * FP4
C
        WRITE(OUT, 10)
        WRITE(OUT, 40)
10      FORMAT(' A  Paranoid  Program  to  Diagnose  Floating-point',
     ~         ' Arithmetic')
40      FORMAT('          ... Single-Precision Version  ...')
C
C#######################################################################
C
        NUMTRY = 20
C        ...  NUMTRY = #( RANDOM TRIALS OF X*Y=Y*X , ETC.)
        PGNUMB=0
C        ... PGNUMB = #( PAGE OF DIAGNOSIS ); MILES=MILESTONE IN PROGRAM
        MILES=0
C        ... COUNT FAILURES, SERIOUS DEFECTS, DEFECTS, FLAWS
        FAILS = 0
        SDEFCT = 0
        DEFECT = 0
        FLAWS = 0
C#######################################################################
C
C
C       PRINT BIG INTRO MESSAGES
C
C
C#######################################################################
        CALL INTRO (MILES)
C#######################################################################
C
C
C       SMALL INTEGER TESTING
C
C
C#######################################################################
880     MILES = 7
        CALL PAGE (MILES)
881     CALL SMLINT (MILES,FROM)
        FROM = 0
C#######################################################################
C
C
C       FIND RADIX B AND PRECISION P
C
C
C#######################################################################
        CALL RADX (MILES)
C#######################################################################
C
C
C       TEST FOR EXTRA PRECISION IN SUBEXPRESSIONS
C
C
C#######################################################################
1680    MILES = 30
        CALL EXTRA
1860    CALL PAGE (MILES)
C#######################################################################
C
C
C       CHECK FOR GUARD DIGITS AND NORMALIZATION IN SUBTRACTION
C
C
C#######################################################################
1870    MILES = 35
        CALL GUARD
2240    MILES = 40
        CALL PAGE (MILES)
C#######################################################################
C
C
C       TEST ROUNDING IN MULTIPLY, DIVIDE, AND ADD/SUBTRACT.
C
C
C#######################################################################
        CALL ROUND (MILES)
2910    MILES = 60
C#######################################################################
C
C
C       TEST FOR COMMUTATIVE MULTIPLICATION
C
C
C#######################################################################
        CALL COMMUT (NUMTRY)
3010    MILES = 70
C#######################################################################
C
C
C       TEST SQUARE ROOT
C
C
C#######################################################################
3959    CALL SQUARE (FROM, MILES, NUMTRY)
        FROM = 0
3960    MILES = 90
        CALL PAGE (MILES)
C#######################################################################
C
C
C       TEST Y TO POWER X
C
C
C#######################################################################
        CALL POWER (MILES,FROM)
        FROM = 0
C#######################################################################
C
C
C       TEST UNDERFLOW THRESHOLDS
C
C
C#######################################################################
10100   CALL UNDERF (MILES,NUMTRY,FROM)
        FROM = 0
        CALL PAGE (MILES)
C#######################################################################
C
C
C       TEST OVERFLOW THRESHOLDS
C
C
C#######################################################################
10200   CALL OVERF (MILES, FROM)
        FROM = 0
6110    MILES = 210
C
C
C
10300   CALL ZEROS(MILES,FROM)
        FROM = 0
        CALL PAGE (MILES)
6150    IF (FAILS .GT. 0) WRITE(OUT, 6151) FAILS
6151    FORMAT (' The number of  FAILUREs  encountered =       ',I4)
6160    IF (SDEFCT .GT. 0) WRITE(OUT, 6161) SDEFCT
6161    FORMAT (' The number of  SERIOUS DEFECTs  discovered = ',I4)
6170    IF (DEFECT .GT. 0) WRITE(OUT, 6171) DEFECT
6171    FORMAT (' The number of  DEFECTs  discovered =         ',I4)
6180    IF (FLAWS .GT. 0) WRITE(OUT, 6181) FLAWS
6181    FORMAT (' The number of  FLAWs  discovered =           ',I4)
6190    IF (FAILS+SDEFCT+DEFECT+FLAWS .GT. 0) GOTO 6270
        WRITE(OUT, 6200)
6200    FORMAT(' No failures, defects nor flaws have been discovered.')
6210    IF (R1+R2+R3+R4 .LT. FP4) GOTO 6260
6220    IF (STICKY .LT. FP1 .OR. (RADIX-FP2)*(RADIX-FP9-FP1) .NE. FP0)
     ~     GOTO 6250
6230    TEMP = 854
        IF (RADIX .EQ. FP2 .AND.
     ~    (PRECIS - FP4*FP3*FP2) * (PRECIS - FP27-FP27+FP1) .EQ. FP0)
     ~ TEMP = 754
        WRITE(OUT,6240) TEMP
6240    FORMAT (' Rounding appears to conform to the proposed',
     ~         ' IEEE standard  P', I3)
        IF (IEEE .EQ. 0) WRITE(OUT, 6241)
6241    FORMAT (' except possibly for Single Rounding during Gradual',
     ~         ' Underflow.')
6250    WRITE(OUT, 6251)
6251    FORMAT(' The arithmetic diagnosed appears to be Excellent!')
        GOTO 6310
6260    WRITE(OUT, 6261)
6261    FORMAT(' The arithmetic diagnosed seems Satisfactory.')
        GOTO 6310
6270    IF (FAILS+SDEFCT+DEFECT .EQ. 0 .AND. FLAWS .GT. 0)
     ~     WRITE(OUT, 6271)
6271    FORMAT(' The arithmetic diagnosed seems Satisfactory though',
     ~         ' flawed.')
6280    IF (FAILS+SDEFCT .EQ. 0 .AND. DEFECT .GT. 0) WRITE(OUT, 6281)
6281    FORMAT(' The arithmetic diagnosed may be Acceptable despite',
     ~         ' inconvenient Defects.')
6290    IF (FAILS+SDEFCT .GT. 0) WRITE(OUT, 6291)
6291    FORMAT(' The arithmetic diagnosed has unacceptable Serious',
     ~         ' Defects.')
6300    IF (FAILS .GT. 0) WRITE(OUT, 6301)
6301    FORMAT(' Potentially fatal FAILURE may have spoiled this',
     ~         ' program''s subsequent diagnoses.')
6310    WRITE(OUT, 6311)
6311    FORMAT(' End of Test.')
        STOP
        END
        SUBROUTINE COMMUT( NUMTRY)
C
        COMMON /STDIO/ IN, OUT
        INTEGER IN, OUT
        COMMON /GLOBAL/ FAILS, SDEFCT, DEFECT, FLAWS, RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
        INTEGER         FAILS, SDEFCT, DEFECT, FLAWS
        REAL                              RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
C
C        FAILS  ... NUMBER OF FAILURES
C        SDEFCT ... NUMBER OF SERIOUS DEFECTS
C        DEFECT ... NUMBER OF DEFECTS
C        FLAWS  ... NUMBER OF FLAWS
C
C        RADIX  ... COMPUTED RADIX OF THE MACHINE
C        ULPPLS ... ONE UNIT IN THE LAST PLACE (ULP) OF 1+EPSILON
C        ULPMIN ... ONE UNIT IN THE LAST PLACE (ULP) OF 1-EPSILON
C        PRECIS ... COMPUTED PRECISION OF THE MACHINE
C        W      ... RADIX ** PRECIS
C        MULGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN MULTIPLY
C        DIVGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN DIVIDE
C        SUBGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN ADD/SUBTRACT
C        A1     ...
C        ONEMIN ... ONE MINUS ULPMIN = ~.99999999999999
C
        COMMON /CONST/   FP0, FP1, FP2, FP3, FP4, FP8, FP9, FP27, FP32,
     ~                   HALF, MINUS1
        REAL FP0, FP1, FP2, FP3, FP4, FP8, FP9, FP27, FP32,
     ~                   HALF, MINUS1
C
        INTEGER NUMTRY
        REAL R9, X, X9, Y, Y9, Z, Z9
        INTEGER I, NN
C
2920    WRITE(OUT,2921) NUMTRY
2921    FORMAT(/' Does multiplication commute?',
     ~           ' Testing if  x*y = y*x  for', I4,' random pairs:')
2930    R9 =  SQRT(FP3)
        I = NUMTRY + 1
        X9 = FP0 / FP3
2960    CALL RANDOM (X, Y, X9, R9)
        Y9=X9
        CALL RANDOM (X, Y, X9, R9)
        Z=X9*Y9
        Y=Y9*X9
        Z9=Z-Y
        I=I-1
        IF (I .GT. 0 .AND. Z9 .EQ. FP0) GOTO 2960
2970    IF (I .GT. 0) GOTO 3000
2980    X9=FP0+HALF/FP3
        Y9=(ULPPLS+ULPMIN)+FP0
        Z=X9*Y9
        Y=Y9*X9
        Z9=(FP0+HALF/FP3)*((ULPPLS+ULPMIN)+FP0)
     ~ -((ULPPLS+ULPMIN)+FP0)*(FP0+HALF/FP3)
        IF (Z9 .NE. FP0) GOTO 3000
        WRITE(OUT,2990) NUMTRY
2990    FORMAT(' No failure found in ',I4,' randomly chosen pairs.')
        RETURN
3000    DEFECT=DEFECT+1
        WRITE(OUT, 3001) X9, Y9
        WRITE(OUT, 3002) Z, Y, Z9
        NN=NUMTRY-I+1
        WRITE(OUT, 3003) NN
3001    FORMAT(' DEFECT:  x*y = y*x  violated at  x = ',E15.7,', y = ',
     ~ E15.7)
3002    FORMAT('  x*y =',E15.7,',  y*x =',E15.7,',  x*y-y*x =',E15.7)
3003    FORMAT('    ... pair no.', I4)
        RETURN
        END
C---
        SUBROUTINE RANDOM (X, Y, X9, R9)
        REAL X, Y, X9, R9
2950    X=X9+R9
        Y=X*X
        Y=Y*Y
        X=X*Y
        Y=X-AINT(X)
        X9=Y+X*.000005
        RETURN
        END
C---
        SUBROUTINE EXTRA
C
        COMMON /STDIO/ IN, OUT
        INTEGER IN, OUT
        COMMON /GLOBAL/ FAILS, SDEFCT, DEFECT, FLAWS, RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
        INTEGER         FAILS, SDEFCT, DEFECT, FLAWS
        REAL                              RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
C
C        FAILS  ... NUMBER OF FAILURES
C        SDEFCT ... NUMBER OF SERIOUS DEFECTS
C        DEFECT ... NUMBER OF DEFECTS
C        FLAWS  ... NUMBER OF FLAWS
C
C        RADIX  ... COMPUTED RADIX OF THE MACHINE
C        ULPPLS ... ONE UNIT IN THE LAST PLACE (ULP) OF 1+EPSILON
C        ULPMIN ... ONE UNIT IN THE LAST PLACE (ULP) OF 1-EPSILON
C        PRECIS ... COMPUTED PRECISION OF THE MACHINE
C        W      ... RADIX ** PRECIS
C        MULGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN MULTIPLY
C        DIVGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN DIVIDE
C        SUBGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN ADD/SUBTRACT
C        A1     ...
C        ONEMIN ... ONE MINUS ULPMIN = ~.99999999999999
C
        COMMON /CONST/   FP0, FP1, FP2, FP3, FP4, FP8, FP9, FP27, FP32,
     ~                   HALF, MINUS1
        REAL FP0, FP1, FP2, FP3, FP4, FP8, FP9, FP27, FP32,
     ~                   HALF, MINUS1
C
        REAL Q, X, X1, Y, Y1, Z, Z1, Z2, XX
C
        WRITE(OUT,1681)
1681    FORMAT (' Test for extra-precise subexpressions:')
C
1690    X =  ABS( ((FP4 / FP3 - FP1) - FP1 / FP4) * FP3 - FP1 / FP4)
1700    Z2 = X
        X = (FP1 + (HALF * Z2 + FP32 * Z2 * Z2)) - FP1
        IF (Z2 .GT. X .AND. X .GT. FP0) GOTO 1700
1710    Y =  ABS( (FP3/FP4 - FP2/FP3) * FP3 - FP1/FP4)
        Z=Y
        X=Y
1720    Z1=Z
        Z=(FP1/FP2 - ((FP1/FP2-(HALF*Z1 + FP32*Z1*Z1))+FP1/FP2)) + FP1/
     ~ FP2
        IF (Z1 .GT. Z .AND. Z .GT. FP0) GOTO 1720
1730    Y1=Y
        Y=(HALF - ((HALF-(HALF*Y1 + FP32*Y1*Y1))+HALF)) + HALF
        IF (Y1 .GT. Y .AND. Y .GT. FP0) GOTO 1730
1740    X1=X
        X=((HALF*X1+FP32*X1*X1)-ONEMIN)+ONEMIN
        IF (X1 .GT. X  .AND. X  .GT. FP0) GOTO 1730
1750    IF (X1 .EQ. Y1 .AND. X1 .EQ. Z1)  GOTO 1780
1760    SDEFCT=SDEFCT+1
        WRITE(OUT, 1761)
        WRITE(OUT, 1762) X1, Y1, Z1
        WRITE(OUT, 1763)
        WRITE(OUT, 1770)
        WRITE(OUT, 1771)
        WRITE(OUT, 1772)
1761    FORMAT(' SERIOUS DEFECT: disagreements among the values  X1, Y1,
     ~ Z1')
1762    FORMAT(' respectively ',E15.7,',    ',E15.7,',    ',E15.7)
1763    FORMAT(' are symptoms of inconsistencies introduced by extra-pre
     ~cise')
1770    FORMAT(' evaluation of allegedly  "optimized"  arithmetic')
1771    FORMAT(' subexpressions.  Possibly some part of this')
1772    FORMAT(' test is inconsistent; PLEASE NOTIFY KARPINSKI !')
        IF (X1 .EQ. ULPMIN .OR. Y1 .EQ. ULPMIN .OR. Z1 .EQ. ULPMIN)
     ~      GOTO 1850
1780    IF (Z1 .NE. ULPMIN .OR. Z2 .NE. ULPPLS) GOTO 1790
        WRITE(OUT, 1781)
1781    FORMAT(' Subexpressions do not appear to be calculated'/
     ~          '  with extra precision.')
        RETURN
1790    IF (Z1 .LT. ULPMIN .AND. Z2 .LT. ULPPLS) GOTO 1810
1800    FAILS=FAILS+1
        WRITE(OUT, 1801)
        WRITE(OUT, 1802)
        XX=Z1-ULPMIN
        WRITE(OUT, 1803) ULPMIN, XX
        XX=Z2-ULPPLS
        WRITE(OUT, 1804) ULPPLS, XX
1801    FORMAT(' FAILURE: precision test is inconsistent.')
1802    FORMAT(' PLEASE NOTIFY KARPINSKI !')
1803    FORMAT(' ulpmin =  ', E15.7, '    z1 - ulpmin = ', E15.7)
1804    FORMAT(' ulppls =  ', E15.7, '    z1 - ulppls = ', E15.7)
        RETURN
1810    IF (Z1 .GT. FP0 .AND. Z2 .GT. FP0) GOTO 1830
1820    WRITE(OUT, 1821)     RADIX
        WRITE(OUT, 1822)
        WRITE(OUT, 1823) Z1, Z2
        WRITE(OUT, 1824)
        WRITE(OUT, 1825)
1821    FORMAT(' Because of an unusual radix  b =', F4.0,',')
1822    FORMAT(' or exact rational arithmetic,')
1823    FORMAT(' a result  z1 =',E15.7,'  or  z2 =',E15.7)
1824    FORMAT(' of an extra-precision test is inconsistent.')
1825    FORMAT(' PLEASE NOTIFY KARPINSKI !')
        IF (Z1 .EQ. Z2) GOTO 1850
1830    X = Z1/ULPMIN
        Y = Z2/ULPPLS
        IF (Y .GT. X) X=Y
1840    Q = -ALOG(X)
        WRITE(OUT, 1841)
        XX=Q/ALOG(RADIX)
        WRITE(OUT, 1842) XX
        XX=Q/ALOG(FP8+FP2)
        WRITE(OUT, 1843) XX
1841    FORMAT(' Some subexpressions appear to be calculated extra-preci
     ~cely')
1842    FORMAT(' with about   ',E15.7,' extra base b digits, i.e.')
1843    FORMAT(' roughly ',E15.7,' extra significant decimals.')
1850    WRITE(OUT, 1851)
1851    FORMAT(' That feature is not tested further by this program.')
        RETURN
        END
C---
        SUBROUTINE GUARD
C
        COMMON /STDIO/ IN, OUT
        INTEGER IN, OUT
        COMMON /GLOBAL/ FAILS, SDEFCT, DEFECT, FLAWS, RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
        INTEGER         FAILS, SDEFCT, DEFECT, FLAWS
        REAL                              RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
C
C        FAILS  ... NUMBER OF FAILURES
C        SDEFCT ... NUMBER OF SERIOUS DEFECTS
C        DEFECT ... NUMBER OF DEFECTS
C        FLAWS  ... NUMBER OF FLAWS
C
C        RADIX  ... COMPUTED RADIX OF THE MACHINE
C        ULPPLS ... ONE UNIT IN THE LAST PLACE (ULP) OF 1+EPSILON
C        ULPMIN ... ONE UNIT IN THE LAST PLACE (ULP) OF 1-EPSILON
C        PRECIS ... COMPUTED PRECISION OF THE MACHINE
C        W      ... RADIX ** PRECIS
C        MULGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN MULTIPLY
C        DIVGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN DIVIDE
C        SUBGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN ADD/SUBTRACT
C        A1     ...
C        ONEMIN ... ONE MINUS ULPMIN = ~.99999999999999
C
        COMMON /CONST/   FP0, FP1, FP2, FP3, FP4, FP8, FP9, FP27, FP32,
     ~                   HALF, MINUS1
        REAL FP0, FP1, FP2, FP3, FP4, FP8, FP9, FP27, FP32,
     ~                   HALF, MINUS1
C
C        ... LOCAL VARIABLES
        REAL R, S, T, X, Y, Z
C        ... CONSTANTS
        REAL B9
C
        B9 = RADIX - ULPPLS
C
1040    MULGRD = 1.0
        DIVGRD = 1.0
        SUBGRD = 1.0
C
1880    IF (RADIX .LT. FP2) GOTO 1920
1890    X = W / (RADIX * RADIX)
        Y = X + FP1
        Z = Y - X
        T = Z + ULPPLS
        X = T - Z
        IF (X .EQ. ULPPLS) GOTO 1910
1900    FAILS = FAILS + 1
        WRITE(OUT, 1905)
1905    FORMAT(' FAILURE: subtraction is not normalized',
     ~          ' so  x=y  does not imply  x+z=y+z !')
        GOTO 1920
1910    WRITE(OUT,1911)
1911    FORMAT(' Subtraction appears to be normalized',
     ~            ' as it should.')
1920    WRITE(OUT,1930)
1930    FORMAT(' Checking for guard digits in multiply',
     ~            ' divide and subtract.')
1940    Y=ONEMIN*FP1
        Z=FP1*ONEMIN
        X=ONEMIN-HALF
        Y=(Y-HALF)-X
        Z=(Z-HALF)-X
        X=FP1+ULPPLS
        T = X * RADIX
1950    R = RADIX * X
        X=T-RADIX
        X=X-RADIX*ULPPLS
        T=R-RADIX
        T=T-RADIX*ULPPLS
        X=X*(RADIX-FP1)
        T=T*(RADIX-FP1)
        IF (X .EQ. FP0 .AND. Y .EQ. FP0 .AND. Z .EQ. FP0 .AND.
     ~                                       T .EQ. FP0)
     ~     GOTO 1980
1960    SDEFCT=SDEFCT+1
1970    MULGRD=FP0
        WRITE(OUT, 1971)
1971    FORMAT(' SERIOUS DEFECT: multiplication lacks a guard digit',
     ~          ' violating  1*x = x .')
1980    Z = RADIX * ULPPLS
        X=FP1+Z
        Y= ABS((X+Z)-X*X)-ULPPLS
        X=FP1-ULPPLS
        Z= ABS((X-ULPPLS)-X*X)-ULPMIN
        IF (Y .LE. FP0 .AND. Z .LE. FP0) GOTO 2000
1990    FAILS=FAILS+1
        WRITE(OUT, 1991)
1991    FORMAT(' FAILURE: multiplication  gets too many last digits',
     ~          ' wrong.')
2000    Y=FP1-ULPPLS
        X=FP1+ULPPLS
        Z=FP1/Y
        Y=Z-X
        X = FP1/FP3
        Z = FP3/FP9
        X=X-Z
        T = FP9/FP27
2010    Z=Z-T
        IF (X .EQ. FP0 .AND. Y .EQ. FP0 .AND. Z .EQ. FP0) GOTO 2040
2020    DEFECT=DEFECT+1
2030    DIVGRD=FP0
        WRITE(OUT,2031)
        WRITE(OUT,2032)
2031    FORMAT(' DEFECT: division lacks a guard digit',
     ~          ' so error can exceed 1 ulp')
2032    FORMAT(' or  1/3  and  3/9  and  9/27  may disagree.')
2040    Y=ONEMIN/FP1
        X=ONEMIN-HALF
        Y=(Y-HALF)-X
        X=FP1+ULPPLS
        T=X/FP1
        X=T-X
        IF (X .EQ. FP0 .AND. Y .EQ. FP0) GOTO 2070
2050    SDEFCT = SDEFCT + 1
        DEFECT = DEFECT - 1 + DIVGRD
2060    DIVGRD=FP0
        WRITE(OUT, 2061)
2061    FORMAT(' SERIOUS DEFECT:  division lacks a guard digit',
     ~          ' violating  x/1 = x .')
2070    X=FP1/(FP1+ULPPLS)
        Y=X-HALF-HALF
        IF (Y .LT. FP0) GOTO 2100
2080    SDEFCT=SDEFCT+1
2090    WRITE(OUT, 2091)
        WRITE(OUT, 2092)
2091    FORMAT(' VERY SERIOUS DEFECT:  computed value of  1/1.00...001')
2092    FORMAT(' is not less than  1 .')
2100    X=FP1-ULPPLS
        Y = FP1 + RADIX * ULPPLS
        Z = X * RADIX
        T = Y * RADIX
        R = Z / RADIX
        S = T / RADIX
        X = R - X
        Y = S - Y
        IF (X .EQ. FP0 .AND. Y .EQ. FP0) GOTO 2130
2110    FAILS = FAILS + 1
        WRITE(OUT, 2120)
        WRITE(OUT, 2121)
2120    FORMAT(' FAILURE: multiplication  and/or  division')
2121    FORMAT(' gets too many last digits wrong.')
2130    Y = FP1-ULPMIN
        X = FP1-ONEMIN
        Y = FP1-Y
        T = RADIX - ULPPLS
        Z = RADIX - B9
        T = RADIX - T
2140    IF (X .EQ. ULPMIN .AND. Y .EQ. ULPMIN .AND. Z .EQ. ULPPLS .AND.
     ~     T .EQ. ULPPLS) GOTO 2230
2150    SDEFCT=SDEFCT+1
2160    SUBGRD=FP0
        WRITE(OUT, 2161)
2161    FORMAT(' SERIOUS DEFECT: subtraction lacks a guard digit',
     ~          ' so cancellation is obscured.')
2170    IF (ONEMIN .EQ. FP1 .OR. ONEMIN-FP1 .LT. FP0) RETURN
2180    SDEFCT=SDEFCT+1
        WRITE(OUT, 2190)
        WRITE(OUT, 2191)
        WRITE(OUT, 2200)
        WRITE(OUT, 2210)
        WRITE(OUT, 2220)
2190    FORMAT(' VERY SERIOUS DEFECT:')
2191    FORMAT('   comparison alleges  (1-ulpmin) < 1  although')
2200    FORMAT('   subtraction yields  (1-ulpmin) - 1 = 0  ,
     ~ thereby vitiating')
2210    FORMAT('   such precautions against division by zero as')
2220    FORMAT('   ...  if (x=1.0) then ..... else .../(x-1.0)...')
C
2230    IF (MULGRD * DIVGRD * SUBGRD .EQ. FP1) WRITE(OUT, 2231)
2231    FORMAT(' These operations appear to have guard digits',
     ~          ' as they should.')
        RETURN
        END
C---
        SUBROUTINE INTRO(MILES)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C
C
C       PRINT THE LARGE BANNER INTRODUCTION - GOES ON FOR SEVERAL
C       PAGES
C
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        INTEGER MILES
        COMMON /STDIO/ IN, OUT
        INTEGER IN, OUT
C        ... NUMBER OF MILESTONE TO PRINTOUT AS WE GO ALONG
        WRITE(OUT,170)
        WRITE(OUT,180)
        CALL PAGE(MILES)
        WRITE(OUT,190)
        CALL PAGE(MILES)
        WRITE(OUT,200)
170     FORMAT(
     &    ' Lest this program stop prematurely, i.e. before displaying'
     &   /'   "End of Test"   '
     &   /' try to persuade the computer NOT to terminate execution'
     &   /' whenever an error such as Over/Underflow or Division by'
     &   /' Zero occurs, but rather to persevere with a surrogate value'
     &   /' after, perhaps, displaying some warning.  If persuasion'
     &   /' avails naught, don''t despair but run this program anyway')
180     FORMAT(
     &    ' to see how many milestones it passes, and then run it'
     &   /' again.  It should pick up just beyond the error and'
     &   /' continue.  If it does not, it needs further debugging.'
     &  //' Users are invited to help debug and augment this program'
     &   /' so that it will cope with unanticipated and newly found'
     &   /' compilers and arithmetic pathologies.')
190     FORMAT(
     &   /' Please send suggestions and interesting results to'
     &        /9X,'Richard Karpinski'
     &        /9X,'Computer Center U-76'
     &        /9X,'University of California'
     &        /9X,'San Francisco, CA 94143-0704'
     &        /9X,'USA'/
     &        /' In doing so, please include the following information:'
     &        /9X,'Precision:   Single;'/9X,'Version: 31 July 1986;'
     &        /9X,'Computer:'//9X,'Compiler:'//9X,'Optimization level:'/
     &        /9X,'Other relevant compiler options:'/)
200     FORMAT(
     &        /' BASIC version (C) 1983 by Prof. W. M. Kahan.'
     &        /' Translated to FORTRAN by T. Quarles and G. Taylor.'
     &        /' Modified to ANSI 66/ANSI 77 compatible subset by'
     &        /' Daniel Feenberg and David Gay.'
     &        /' You may redistribute this program freely if you'
     &        /' acknowledge the source.')
C#####################################################################
        WRITE(OUT,480)
        CALL PAGE (MILES)
        WRITE(OUT,530)
        WRITE(OUT,590)
        WRITE(OUT,640)
480     FORMAT(//
     &  ' Running this program should reveal these characteristics:'//
     &  ' b = radix ( 1, 2, 4, 8, 10, 16, 100, 256, or ... ) .'/
     &  ' p = precision, the number of significant  b-digits carried.'/
     &  ' u2 = b/b^p = one ulp (unit in the last place) of 1.000xxx..'/
     &  ' u1 = 1/b^p = one ulp of numbers a little less than 1.0.')
530     FORMAT(' g1, g2, g3 tell whether adequate guard digits are carri
     &ed;'/
     &  ' 1 = yes, 0 = no;  g1 for mult.,  g2 for div., g3 for subt.'/
     &  ' r1,r2,r3,r4  tell whether arithmetic is rounded or chopped;'/
     &  ' 0=chopped, 1=correctly rounded, -1=some other rounding;'/
     &  ' r1 for mult., r2 for div., r3 for add/subt., r4 for sqrt.'/
     &  ' s=1 when a sticky bit is used correctly in rounding; else s=0
     &.')
590     FORMAT(' u0 = an underflow threshold.'/
     & ' e0 and z0 tell whether underflow is abrupt, gradual or fuzzy'/
     & ' v = an overflow threshold, roughly.'/
     & ' v0  tells, roughly, whether  infinity  is represented.'/
     & ' Comparisons are checked for consistency with subtraction')
640     FORMAT('        and for contamination by pseudo-zeros.'/
     & ' Sqrt is tested. so is  y^x  for (mostly) integers  x .'/
     & ' Extra-precise subexpressions are revealed but not yet tested.'
     & /' Decimal-binary conversion is not yet tested for accuracy.')
C#####################################################################
        CALL PAGE (MILES)
        WRITE(OUT,690)
        WRITE(OUT,760)
        WRITE(OUT,780)
        WRITE(OUT,820)
690     FORMAT(' The program attempts to discriminate among:'/
     &         '     >FLAWs, like lack of a sticky bit, '/
     &         '     >SERIOUS DEFECTs, like lack of a guard digit, and'/
     &         '     >FAILUREs, like  2+2 = 5 .'/
     &         ' Failures may confound subsequent diagnoses.')
760     FORMAT(/
     &  ' The diagnostic capabilities of this program go beyond an'/
     &  ' earlier program called  "Machar", which can be found at the'/
     &' end of the book "Software Manual for the Elementary Functions"')
780     FORMAT(
     &  ' (1980) by W. J. Cody and W. Waite. Although both programs'/
     &  ' try to discover the radix (b), precision (p) and         '/
     &  ' range (over/underflow thresholds) of the arithmetic, this'/
     &  ' program tries to cope with a wider variety of pathologies')
820     FORMAT(
     &  ' and to say how well the arithmetic is implemented.'/
     &  ' The program is based upon a conventional radix'/
     &  ' representation for floating-point numbers,'/
     &  ' but also allows for logarithmic encoding (b = 1)'/
     &  ' as used by certain early wang machines.'/)
        RETURN
        END
C---
        SUBROUTINE LOGIT (MILE)
C
        INTEGER MILE
C        ... MILESTONE REACHED - PASSED THROUGH BY THE SUCCESSFUL CODE
C
        COMMON /GLOBAL/ FAILS, SDEFCT, DEFECT, FLAWS, RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
        INTEGER         FAILS, SDEFCT, DEFECT, FLAWS
        REAL                              RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
C
C        FAILS  ... NUMBER OF FAILURES
C        SDEFCT ... NUMBER OF SERIOUS DEFECTS
C        DEFECT ... NUMBER OF DEFECTS
C        FLAWS  ... NUMBER OF FLAWS
C
C        RADIX  ... COMPUTED RADIX OF THE MACHINE
C        ULPPLS ... ONE UNIT IN THE LAST PLACE (ULP) OF 1+EPSILON
C        ULPMIN ... ONE UNIT IN THE LAST PLACE (ULP) OF 1-EPSILON
C        PRECIS ... COMPUTED PRECISION OF THE MACHINE
C        W      ... RADIX ** PRECIS
C        MULGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN MULTIPLY
C        DIVGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN DIVIDE
C        SUBGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN ADD/SUBTRACT
C        A1     ...
C        ONEMIN ... ONE MINUS ULPMIN = ~.99999999999999
C
        COMMON /CONST/   FP0, FP1, FP2, FP3, FP4, FP8, FP9, FP27, FP32,
     ~                   HALF, MINUS1
        REAL FP0, FP1, FP2, FP3, FP4, FP8, FP9, FP27, FP32,
     ~                   HALF, MINUS1
C
        COMMON /OVUNFL/  C1, H1, MINPOS, NULPS, UFLTHR, PHONY0
        REAL C1, H1, MINPOS, NULPS, UFLTHR, PHONY0
C        C1     ... 1/C ~= RADIX^LARGE_INTEGER
C        H1     ... MAX (2,RADIX)
C        MINPOS ... MINIMUM POSITIVE NUMBER FOUND BY MULT./DIVISION
C        NULPS  ... (SMALL INTEGER) * 1 UNIT IN LAST PLACE OF 1+ ... E9
C        UFLTHR ... THE UNDERFLOW THRESHOLD U0
C        PHONY0 ... CANDIDATE AND FINAL PHONY ZERO IF IT EXISTS. Z0
C
        COMMON /PGNUM/  PGNUMB
        INTEGER         PGNUMB
C
        COMMON /ROUNDN/  R1, R2, R3, R4, STICKY
        REAL R1, R2, R3, R4, STICKY
C
        COMMON /I3TYPE/ IEEE
        INTEGER         IEEE
C        IEEE   ... FLAG WHETHER GRADUAL UNDERFLOWS ARE DOUBLY ROUNDED
C
C        THIS ROUTINE FORCES A CHECKPOINT BY  WRITING ALL GLOBAL
C        INFORMATION TO A FILE FOR LATER RESTARTING.
C
        WRITE(4) FP0,FP1,FP2,FP3,FP4,FP8,FP9,FP27,FP32,HALF,MINUS1
        WRITE(4) FAILS,SDEFCT,DEFECT,FLAWS,RADIX,ULPPLS,ULPMIN,PRECIS
        WRITE(4) W, MULGRD,DIVGRD, SUBGRD,A1,ONEMIN
        WRITE(4) C1, H1, MINPOS, NULPS, UFLTHR, PHONY0, IEEE
        WRITE(4) R1, R2, R3, R4, STICKY
        WRITE(4) PGNUMB,MILE
        REWIND 4
        RETURN
        END
C---
C       SUBROUTINE TO TEST IF  Y = X
C
        SUBROUTINE CMPXY (X, Y, Z, Q, N)
C
        COMMON /STDIO/ IN, OUT
        INTEGER IN, OUT
        COMMON /GLOBAL/ FAILS, SDEFCT, DEFECT, FLAWS, RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
        INTEGER         FAILS, SDEFCT, DEFECT, FLAWS
        REAL                              RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
C
C        FAILS  ... NUMBER OF FAILURES
C        SDEFCT ... NUMBER OF SERIOUS DEFECTS
C        DEFECT ... NUMBER OF DEFECTS
C        FLAWS  ... NUMBER OF FLAWS
C
C        RADIX  ... COMPUTED RADIX OF THE MACHINE
C        ULPPLS ... ONE UNIT IN THE LAST PLACE (ULP) OF 1+EPSILON
C        ULPMIN ... ONE UNIT IN THE LAST PLACE (ULP) OF 1-EPSILON
C        PRECIS ... COMPUTED PRECISION OF THE MACHINE
C        W      ... RADIX ** PRECIS
C        MULGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN MULTIPLY
C        DIVGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN DIVIDE
C        SUBGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN ADD/SUBTRACT
C        A1     ...
C        ONEMIN ... ONE MINUS ULPMIN = ~.99999999999999
C
        REAL FP0, X, XX, Y, Z
        INTEGER Q, N
        DATA FP0/0.E0/
        Y=Z**Q
4040    IF (Y .EQ. X) GOTO 4080
        IF (Z .GT. FP0) GO TO 4050
        IF (Q .GT. FP0) GO TO 4050
        WRITE(OUT, 40401) Z, Q, Y
40401   FORMAT(' WARNING: computed  (',E16.8,')^(',I3,') = ',E16.8)
        GO TO 40601
4050    IF (N .GT. 0) GOTO 4070
4060    DEFECT = DEFECT + 1
        WRITE(OUT, 4061) Z, Q, Y
40601   WRITE(OUT, 4062) X
        XX=Y-X
        WRITE(OUT, 4063) XX
4061    FORMAT(' DEFECT: computed  (',E16.8,')^(',I3,') = ',E16.8)
4062    FORMAT('    compares unequal to correct ',E16.8)
4063    FORMAT('    they differ by  ',E16.8)
C                       INCREMENT COUNT OF DISCREPANCIES
4070    N = N + 1
4080    RETURN
        END
C---
C       SUBROUTINE TO PRINT N AND PAUSE IF N > 0.
C
        SUBROUTINE PRT2 (N, MILES)
        INTEGER N, MILES
        COMMON /STDIO/ IN, OUT
        INTEGER IN, OUT
4290    IF (N .EQ. 0) WRITE(OUT, 4291)
4291    FORMAT(' No discrepancies found.'/)
4310    IF (N .GT. 0) CALL PAGE(MILES)
C       ---- PAUSE ----
4320    RETURN
        END
C---
C       SUBROUTINE TO COMPARE  Z^I  WITH  X = Z*Z*...*Z  ( I TIMES )
C
        SUBROUTINE PWRCMP (X, Z, I, M, N)
C
        COMMON /GLOBAL/ FAILS, SDEFCT, DEFECT, FLAWS, RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
        INTEGER         FAILS, SDEFCT, DEFECT, FLAWS
        REAL                              RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
C
C        FAILS  ... NUMBER OF FAILURES
C        SDEFCT ... NUMBER OF SERIOUS DEFECTS
C        DEFECT ... NUMBER OF DEFECTS
C        FLAWS  ... NUMBER OF FLAWS
C
C        RADIX  ... COMPUTED RADIX OF THE MACHINE
C        ULPPLS ... ONE UNIT IN THE LAST PLACE (ULP) OF 1+EPSILON
C        ULPMIN ... ONE UNIT IN THE LAST PLACE (ULP) OF 1-EPSILON
C        PRECIS ... COMPUTED PRECISION OF THE MACHINE
C        W      ... RADIX ** PRECIS
C        MULGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN MULTIPLY
C        DIVGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN DIVIDE
C        SUBGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN ADD/SUBTRACT
C        A1     ...
C        ONEMIN ... ONE MINUS ULPMIN = ~.99999999999999
C
        REAL X, Z
        INTEGER I, M, N
        REAL Y
        INTEGER Q
3990    Y = Z ** I
        Q = I
C                               TEST WHETHER  Y=X
        CALL CMPXY (X, Y, Z, Q, N)
4000    I = I + 1
C                               WITH  X = Z^M
        IF (I .GT. M) RETURN
4010    X = Z * X
        IF (X .LT. W) GOTO 3990
4020    RETURN
        END
C---
C       SUBROUTINE TO PRINT COUNT  N  OF DISCREPANCIES
C
        SUBROUTINE PRTCNT (N)
        INTEGER N
        COMMON /STDIO/ IN, OUT
        INTEGER IN, OUT
4100    IF (N .GT. 0) WRITE(OUT, 4101) N
4101    FORMAT(' Similar discrepancies have occurred ',I4,' times.')
4110    RETURN
        END
C---
        SUBROUTINE BADSQR(SFLAG,Z,Y)
        INTEGER SFLAG
C        ... INTEGER FLAG TO INDICATE NEED TO USE PREFIX "SERIOUS"
        REAL Z
C        ... SQUARE OF SQUARE ROOT OF Z (WRONG)
        REAL Y
C        ... REAL NUMBER WHOSE SQUARE ROOT SQUARED IS WRONG
        COMMON /STDIO/ IN, OUT
        INTEGER IN, OUT
        IF(SFLAG .EQ. 1) GO TO 5745
        WRITE(OUT,5740) Z
5740    FORMAT(' DEFECT:  comparison alleges that what prints as  z = ',
     ~ E16.8)
        GO TO 5748
5745    WRITE(OUT,5747) Z
5747    FORMAT(' SERIOUS DEFECT:  comparison alleges that what prints as
     ~ z = ',E16.8)
5748    WRITE(OUT,5749) Y
5749    FORMAT(17X,'is too far from  sqrt(z)^2 = ',E16.8,'.')
        RETURN
        END
C---
        SUBROUTINE OVERF (MILES, FROM)
C
        COMMON /STDIO/ IN, OUT
        INTEGER IN, OUT
        COMMON /GLOBAL/ FAILS, SDEFCT, DEFECT, FLAWS, RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
        INTEGER         FAILS, SDEFCT, DEFECT, FLAWS
        REAL                              RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
C
C        FAILS  ... NUMBER OF FAILURES
C        SDEFCT ... NUMBER OF SERIOUS DEFECTS
C        DEFECT ... NUMBER OF DEFECTS
C        FLAWS  ... NUMBER OF FLAWS
C
C        RADIX  ... COMPUTED RADIX OF THE MACHINE
C        ULPPLS ... ONE UNIT IN THE LAST PLACE (ULP) OF 1+EPSILON
C        ULPMIN ... ONE UNIT IN THE LAST PLACE (ULP) OF 1-EPSILON
C        PRECIS ... COMPUTED PRECISION OF THE MACHINE
C        W      ... RADIX ** PRECIS
C        MULGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN MULTIPLY
C        DIVGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN DIVIDE
C        SUBGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN ADD/SUBTRACT
C        A1     ...
C        ONEMIN ... ONE MINUS ULPMIN = ~.99999999999999
C
        COMMON /CONST/   FP0, FP1, FP2, FP3, FP4, FP8, FP9, FP27, FP32,
     ~                   HALF, MINUS1
        REAL FP0, FP1, FP2, FP3, FP4, FP8, FP9, FP27, FP32,
     ~                   HALF, MINUS1
C
        COMMON /ERRDAT/ OVRFLW, UNDFLW, DIVZER, ERRFLG
        INTEGER         OVRFLW, UNDFLW, DIVZER, ERRFLG
C        OVRFLW ... NUMBER OF OVERFLOWS SINCE LAST RESET
C        UNDFLW ... NUMBER OF UNDERFLOWS SINCE LAST RESET
C        DIVZER ... NUMBER OF DIVISIONS BY ZERO SINCE LAST RESET
C        ERRFLG ... FLAG THAT IS SET WHENEVER AN ERROR OCCURS
C
        COMMON /OVUNFL/  C1, H1, MINPOS, NULPS, UFLTHR, PHONY0
        REAL C1, H1, MINPOS, NULPS, UFLTHR, PHONY0
C        C1     ... 1/C ~= RADIX^LARGE_INTEGER
C        H1     ... MAX (2,RADIX)
C        MINPOS ... MINIMUM POSITIVE NUMBER FOUND BY MULT./DIVISION
C        NULPS  ... (SMALL INTEGER) * 1 UNIT IN LAST PLACE OF 1+ ... E9
C        UFLTHR ... THE UNDERFLOW THRESHOLD U0
C        PHONY0 ... CANDIDATE AND FINAL PHONY ZERO IF IT EXISTS. Z0
C
C
C        ... NUMBER OF MILESTONES PASSED
        INTEGER MILES
C        ... COUNTER OF MILESTONES PREVIOUSLY REACHED
        INTEGER FROM
C
C        ... LOCAL VARIABLES
        INTEGER I
        REAL V9, X, Y, Z, TEMP
C        ... SATURATION VALUE AFTER FLOATING POINT OVERFLOW
        REAL SAT
C        ... OVERFLOW THRESHOLD
        REAL V
C        ... FLAG TO INDICATE WHETHER DEFECT IS SERIOUS OR NOT
        INTEGER ZFLAG
C
        INTEGER I0
C
C       IBM        SIGN-MAGNITUDE, SATURATION VALUE, NO TRAP
C       PRIME      TWOS-COMPLEMENT, SATURATION VALUE, NO TRAP
C       VAX        SIGN-MAGNITUDE, TRAP
C       ELXSI      SIGN-MAGNITUDE, TRAP OR INFINITY SYMBOL IF NO TRAP
C       CDC        SIGN-MAGNITUDE, INFINITY SYMBOL, NO TRAP
C
C           CALL TO UNIX TO ENABLE TRAPS (FAULTS)
C           ON FP UNDERFLOW AND FIXED POINT OVERFLOW
C
        IF (FROM .EQ. 0) GOTO 5500
C
C           REASSIGN VALUES TO VARIABLES USING THE LOG FILE,
C           THEN GO TO RESTART POINT
C
        READ(3) I,V,SAT,V9,X,Y,Z,ZFLAG
        REWIND 3
        IF (FROM .EQ. 161) GOTO 5582
        IF (FROM .EQ. 170) GOTO 5680
        IF (FROM .EQ. 175) GOTO 5810
        IF (FROM .GE. 201 .AND. FROM .LE. 205) GO TO 5999
        CALL BADMIL
C
5500    WRITE(OUT,5510)
5510    FORMAT(' Searching for overflow threshold:')
        MILES = 161
        CALL LOGIT (MILES)
C
C                  SET Y TO -1 * A LARGE POWER OF THE RADIX
5520    Y = -C1
        V9 = H1 * Y
C
C                  MULTIPLY BY RADIX (H1) UNTIL OVERFLOW OCCURS
C
5530    V = Y
        Y = V9
        WRITE(3) I,V,SAT,V9,X,Y,Z,ZFLAG
        REWIND 3
        V9 = H1 * Y
        IF (V9 .LT. Y) GOTO 5530
C
C       SYSTEM DOES NOT TRAP ON OVERFLOW
C
C           POSSIBILITIES:
C               V9 > Y,  V9 IS THE VALUE RETURNED AFTER OVERFLOW
C                        Y IS THE LARGEST POWER OF THE RADIX
C                        V IS THE SECOND LARGEST POWER OF THE RADIX
C
C               V9 == Y, BOTH ARE SATURATION VALUE
C                        V IS THE LARGEST POWER OF THE RADIX
C
C               V9 == Y, BOTH ARE INFINITY SYMBOL
C                        V IS THE LARGEST POWER OF THE RADIX
C
C       TEST 1: VALUE RETURNED AFTER OVERFLOW SHRINKS IN MAGNITUDE
C
5540    IF (V9 .EQ. Y) GOTO 5545
        SDEFCT = SDEFCT + 1
        WRITE(OUT, 5541) Y, V9
5541    FORMAT(' SERIOUS DEFECT: overflow past  ', 1PE16.8,
     ~          '  shrinks to ', 1PE16.8)
C
C       TEST 2: TWO'S COMPLEMENT MACHINE SATURATES AT NEGATIVE
C               LARGEST POWER OF THE RADIX
C               NEED TO DISTINGUISH SYSTEM WITH OVERFLOW SYMBOLS
C               FROM ONE WITHOUT THEM
C
5545    WRITE(OUT,5546) Y
5546    FORMAT(' Can " z = -y " overflow?  trying it on  y = ',1PE16.8)
        SAT = -Y
        IF (V - Y .EQ. V + SAT) GOTO 5560
5550    FLAWS = FLAWS + 1
        WRITE(OUT, 5551)
5551    FORMAT(' Finds a FLAW:  -(-y) differs from y.')
        GOTO 5590
5560    WRITE(OUT, 5561)
5561    FORMAT(' Seems O.K.')
        GOTO 5590
C
C       RESTART POINT FOR SYSTEMS THAT TRAP ON OVERFLOW
C
C             V9 = Y =  -(LARGEST POWER OF RADIX)
C             V      =  -(SECOND LARGEST POWER OF RADIX)
C
C       TEST 2: TWO'S COMPLEMENT MACHINE
C
5582    WRITE(OUT,5583) Y
5583    FORMAT(' Can " z = -y " overflow?  trying it on  y = ',1PE16.8)
C
C           PUT SOMETHING HERE TO HANDLE THE TRAP
C
        SAT = -Y
        IF (V - Y .EQ. V + SAT) GOTO 5585
        FLAWS = FLAWS + 1
        WRITE(OUT, 5584)
5584    FORMAT('  Finds a FLAW:  -(-y) differs from y.')
        GOTO 5587
5585    WRITE(OUT, 5586)
5586    FORMAT(' Seems O.K.')
C
C           THIS CODE WORKS FOR A SIGN-MAGNITUDE MACHINE,
C           BUT FAILS FOR A TWOS-COMPLEMENT ONE
C
5587    V = Y * (H1 * ULPPLS - H1)
        V = V + Y * ((FP1 - H1) * ULPPLS)
C
        WRITE(OUT, 5588) V
5588    FORMAT(' Overflow threshold is  v = ',1PE16.8)
        WRITE(OUT, 5589)
5589    FORMAT(' There is no saturation value because'/
     ~          ' the system traps on overflow.')
        GOTO 5640
C
C       NON-TRAPPING SYSTEMS (CONTINUED)
C
5590    Y = V * (H1 * ULPPLS - H1)
        Z = Y + V * ((FP1 - H1) * ULPPLS)
        IF (Z .LT. SAT) Y = Z
5600    IF (Y .LT. SAT) V = Y
C
C                  THE OVERFLOW THRESHOLD EQUALS THE SATURATION VALUE
C                  IF THE LATTER BEHAVES AS A NUMBER RATHER THAN AN
C                  OVERFLOW SYMBOL.  AN OVERFLOW SYMBOL IS NOT
C                  CHANGED WHEN ANY NUMBER IS ADDED TO OR SUBTRACTED
C                  FROM IT.
C
5610    IF (SAT - V .LT. SAT) V = SAT
C
        WRITE(OUT, 5620) V
5620    FORMAT(' Overflow threshold is  v = ',1PE16.8)
        WRITE(OUT, 5630) SAT
5630    FORMAT(' Overflow saturates at  sat = ',1PE16.8)
C
C
C
5640    MILES = 163
        WRITE(OUT, 5641)
5641    FORMAT(' No overflow should be signaled for  v*1 = ')
        TEMP = V * FP1
        WRITE(OUT, 5642) TEMP
5642    FORMAT('                                           ',1PE16.8)
        WRITE(OUT, 5643)
5643    FORMAT('                            nor for  v/1 = ')
        TEMP = V / FP1
        WRITE(OUT, 5644) TEMP
5644    FORMAT('                                           ',1PE16.8)
        WRITE(OUT,5649)
5649    FORMAT(' Any overflow signal separating this  *  from one above'
     ~        ,' is a DEFECT.')
C
C       NEED TO ADD CODE HERE TO HANDLE OVERFLOWS JUST ABOVE
C
C
C
5650    MILES=170
C
C       PROBLEM: SAT NOT DEFINED IF WE TRAPPED ON OVERFLOW ABOVE
C
5660    IF (-V .LT. V .AND. -SAT .LT. SAT .AND. -UFLTHR .LT. V .AND.
     ~     UFLTHR .LT. V) GOTO 5680
5670    FAILS = FAILS + 1
        WRITE(OUT,5672)
5672    FORMAT(' FAILURE: comparisons are confused by overflow.')
C
C
C
5680    MILES = 175
        I = 0
        Z = UFLTHR
5700    I = I + 1
        IF (Z .EQ. FP0) GO TO 5770
5710    V9= SQRT(Z)
        Y=V9*V9
        IF (.NOT. (Y/(FP1-RADIX * NULPS) .LT. Z .OR.
     ~            Y .GT. (FP1+RADIX*NULPS)*Z))
     ~     GOTO 5770
5720    IF (V9 .GT. ULPMIN) GOTO 5750
5730    ZFLAG=0
        DEFECT=1+DEFECT
        GOTO 5760
5750    ZFLAG=1
        SDEFCT=1+SDEFCT
5760    CALL BADSQR(ZFLAG,Z,Y)
5770    GOTO (5780, 5790, 5800),I
5780    Z = MINPOS
        GOTO 5700
5790    Z = PHONY0
        GOTO 5700
C
C
C
5800    I=0
        Z=V
5810    MILES=180
5820    IF (RADIX .NE. 2. .OR. PRECIS .NE. 56. .OR.
     ~         PHONY0 .EQ. 0. .OR. -FP0 .EQ. FP0)
     ~    GOTO 5850
5830    FAILS=1+FAILS
C FAILURE: ATTEMPTS TO EVALUATE  SQR(OVERFLOW THRESHOLD V)  IN DOUBLE
C PRECISION IN  BASIC  ON THE  IBM PC  DISPLAY THE WORD  " OVERFLOW "
C AND THEN DISABLE THE KEYBOARD !  THIS IS DISASTROUS.
        GOTO 5920
C
C
C
5850    V9 =  SQRT(Z)
        X = (FP1 - RADIX * NULPS) * V9
        V9 = V9 * X
        IF (.NOT. (V9 .LT. (FP1-FP2*RADIX*NULPS)*Z .OR. V9 .GT. Z))
     ~     GOTO 5900
5860    Y=V9
        IF (X .LT. W) GO TO 5880
5870    ZFLAG = 0
        DEFECT = DEFECT + 1
        GOTO 5890
5880    ZFLAG = 1
        SDEFCT = SDEFCT + 1
5890    I = 1
        CALL BADSQR(ZFLAG,Z,Y)
5900    IF (I .EQ. 1) GO TO 5920
5910    I = 1
        Z = SAT
        GOTO 5850
C
C
C
5920    MILES = 190
        CALL PAGE (MILES)
5940    X = UFLTHR * V
        Y = RADIX * RADIX
        IF (X * Y .GE. FP1 .AND. X .LE. Y) GOTO 5990
5950    IF (X * Y .GE. ULPMIN .AND. X .LE. Y / ULPMIN) GOTO 5970
5960    DEFECT = DEFECT + 1
        WRITE(OUT, 5961) X
5961    FORMAT(' DEFECT: badly unbalanced range;',
     ~ '  UFLTHR * V  =', E13.5,' IS TOO FAR FROM 1.')
        GOTO 5990
5970    FLAWS = FLAWS + 1
        WRITE(OUT, 5971) X
5971    FORMAT(' FLAW: unbalanced range;',
     ~ '  UFLTHR * V  =', E13.5,' IS TOO FAR FROM 1.')
C
C                               TEST  X/X   VS.  1
5990    I0 = 1
        GO TO 6000
5999    I0 = FROM - 200
C
6000    DO 6100 I = I0, 5
6010    X = ONEMIN
        IF (I .EQ. 2) X = FP1 + ULPPLS
        IF (I .EQ. 3) X = V
        IF (I .EQ. 4) X = UFLTHR
        IF (I .EQ. 5) X = RADIX
        MILES = 200 + I
        IF (FROM .NE. MILES) GO TO 6050
        SDEFCT = SDEFCT + 1
        WRITE(OUT,6011) X
6011    FORMAT(' SERIOUS DEFECT: x/x traps when x = ',E13.5)
        GO TO 6100
6050    Y = X
        CALL LOGIT(MILES)
        V9 = (Y / X - HALF) - HALF
        IF (V9 .EQ. FP0) GOTO 6100
        IF (V9 .EQ. -ULPMIN .AND. I .LT. 5) GOTO 6080
6070    FAILS = FAILS + 1
        WRITE(OUT, 6071) X
6071    FORMAT(' FAILURE:  x/x differs from 1 when x = ',E13.5)
        GOTO 6090
6080    SDEFCT = SDEFCT + 1
        WRITE(OUT, 6081) X
6081    FORMAT(' SERIOUS DEFECT:  x/x differs from 1 when x = ',E13.5)
6090    WRITE(OUT, 6091) V9
6091    FORMAT('           instead,  x/x - 1/2 - 1/2 = ',E13.5)
6100    CONTINUE
        RETURN
        END
C---
        SUBROUTINE PAGE(MILE)
C
C
        COMMON /STDIO/ IN, OUT
        INTEGER IN, OUT
        COMMON /GLOBAL/ FAILS, SDEFCT, DEFECT, FLAWS, RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
        INTEGER         FAILS, SDEFCT, DEFECT, FLAWS
        REAL                              RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
C
C        FAILS  ... NUMBER OF FAILURES
C        SDEFCT ... NUMBER OF SERIOUS DEFECTS
C        DEFECT ... NUMBER OF DEFECTS
C        FLAWS  ... NUMBER OF FLAWS
C
C        RADIX  ... COMPUTED RADIX OF THE MACHINE
C        ULPPLS ... ONE UNIT IN THE LAST PLACE (ULP) OF 1+EPSILON
C        ULPMIN ... ONE UNIT IN THE LAST PLACE (ULP) OF 1-EPSILON
C        PRECIS ... COMPUTED PRECISION OF THE MACHINE
C        W      ... RADIX ** PRECIS
C        MULGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN MULTIPLY
C        DIVGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN DIVIDE
C        SUBGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN ADD/SUBTRACT
C        A1     ...
C        ONEMIN ... ONE MINUS ULPMIN = ~.99999999999999
C
        COMMON /CONST/   FP0, FP1, FP2, FP3, FP4, FP8, FP9, FP27, FP32,
     ~                   HALF, MINUS1
        REAL FP0, FP1, FP2, FP3, FP4, FP8, FP9, FP27, FP32,
     ~                   HALF, MINUS1
C
C
        COMMON /PGNUM/PGNUMB
        INTEGER       PGNUMB
C        ... PAGE NUMBER - MAINTAINED AND UPDATED HERE
C
        COMMON /OVUNFL/  C1, H1, MINPOS, NULPS, UFLTHR, PHONY0
        REAL C1, H1, MINPOS, NULPS, UFLTHR, PHONY0
C        C1     ... 1/C ~= RADIX^LARGE_INTEGER
C        H1     ... MAX (2,RADIX)
C        MINPOS ... MINIMUM POSITIVE NUMBER FOUND BY MULT./DIVISION
C        NULPS  ... (SMALL INTEGER) * 1 UNIT IN LAST PLACE OF 1+ ... E9
C        UFLTHR ... THE UNDERFLOW THRESHOLD U0
C        PHONY0 ... CANDIDATE AND FINAL PHONY ZERO IF IT EXISTS. Z0
C
        COMMON /ROUNDN/  R1, R2, R3, R4, STICKY
        REAL R1, R2, R3, R4, STICKY
C
        COMMON /I3TYPE/ IEEE
        INTEGER         IEEE
C        IEEE   ... FLAG WHETHER GRADUAL UNDERFLOWS ARE DOUBLY ROUNDED
C
        INTEGER MILE
C        ... MILESTONE REACHED - PASSED THROUGH BY THE SUCCESSFUL CODE
C
C        THIS PROGRAM RUNS INTERACTIVELY PUTTING ALL OUTPUT TO A SCREEN.
C        THE NEXT SUBPROGRAM PAUSES, ALLOWING YOU TO READ THE SCREEN OR
C        COPY IT.
        WRITE(OUT,110)
110     FORMAT(/' To continue diagnosis, press return.')
        READ(IN,111)
111     FORMAT (A4)
        PGNUMB=PGNUMB+1
        WRITE(OUT,112) MILE, PGNUMB
112     FORMAT(' Diagnosis resumes after milestone  #',I5,',    ... page
     ~ ',I5/)
        WRITE(4) FP0,FP1,FP2,FP3,FP4,FP8,FP9,FP27,FP32,HALF,MINUS1
        WRITE(4) FAILS,SDEFCT,DEFECT,FLAWS,RADIX,ULPPLS,ULPMIN,PRECIS
        WRITE(4) W, MULGRD,DIVGRD, SUBGRD,A1,ONEMIN
        WRITE(4) C1, H1, MINPOS, NULPS, UFLTHR, PHONY0, IEEE
        WRITE(4) R1, R2, R3, R4, STICKY
        WRITE(4) PGNUMB,MILE
        REWIND 4
        MILE=MILE+1
        RETURN
        END
C---
        SUBROUTINE PARTUF (Z, ZNAME, MILES, PARTU, RESTRT)
        LOGICAL RESTRT
C
        COMMON /STDIO/ IN, OUT
        INTEGER IN, OUT
        COMMON /GLOBAL/ FAILS, SDEFCT, DEFECT, FLAWS, RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
        INTEGER         FAILS, SDEFCT, DEFECT, FLAWS
        REAL                              RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
C
C        FAILS  ... NUMBER OF FAILURES
C        SDEFCT ... NUMBER OF SERIOUS DEFECTS
C        DEFECT ... NUMBER OF DEFECTS
C        FLAWS  ... NUMBER OF FLAWS
C
C        RADIX  ... COMPUTED RADIX OF THE MACHINE
C        ULPPLS ... ONE UNIT IN THE LAST PLACE (ULP) OF 1+EPSILON
C        ULPMIN ... ONE UNIT IN THE LAST PLACE (ULP) OF 1-EPSILON
C        PRECIS ... COMPUTED PRECISION OF THE MACHINE
C        W      ... RADIX ** PRECIS
C        MULGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN MULTIPLY
C        DIVGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN DIVIDE
C        SUBGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN ADD/SUBTRACT
C        A1     ...
C        ONEMIN ... ONE MINUS ULPMIN = ~.99999999999999
C
        COMMON /CONST/   FP0, FP1, FP2, FP3, FP4, FP8, FP9, FP27, FP32,
     ~                   HALF, MINUS1
        REAL FP0, FP1, FP2, FP3, FP4, FP8, FP9, FP27, FP32,
     ~                   HALF, MINUS1
C
        COMMON /ERRDAT/ OVRFLW, UNDFLW, DIVZER, ERRFLG
        INTEGER         OVRFLW, UNDFLW, DIVZER, ERRFLG
C        OVRFLW ... NUMBER OF OVERFLOWS SINCE LAST RESET
C        UNDFLW ... NUMBER OF UNDERFLOWS SINCE LAST RESET
C        DIVZER ... NUMBER OF DIVISIONS BY ZERO SINCE LAST RESET
C        ERRFLG ... FLAG THAT IS SET WHENEVER AN ERROR OCCURS
C
        INTEGER PARTU
C        ... FLAG TO INDICATE THE PRESENCE OF PARTIAL UNDERFLOW
        REAL Z
C        ... VALUE TO TEST FOR PARTIAL UNDERFLOW
        CHARACTER*8 ZNAME
C        ... NAME OF THE VARIABLE Z (IN A8 FORMAT) FOR OUTPUT
        INTEGER MILES
C        ... NUMBER OF MILESTONE REACHED SO FAR FOR OUTPUT
        REAL DUMMY
C        ... TEMPORARY VARIABLE TO HOLD A RESULT THAT MAY ACTUALLY
C        ... UNDERFLOW
        REAL MULTP1
C        ... TEMP. VARIABLE TO HOLD RESULT OF TEST FOR UNDERFLOW ON
C        ... MULT BY 1
        REAL MULTP2
C        ... TEMP. VARIABLE TO HOLD RESULT OF TEST FOR UNDERFLOW ON
C        ... 1 * SMALL
        REAL DIVTMP
C        ... TEMP. VARIABLE TO HOLD RESULT OF TEST FOR UNDERFLOW ON
C        ... DIV BY 1
C       ___ SUBROUTINE TO TEST  Z  FOR PARTIAL UNDERFLOW ___
4640    PARTU=0
        IF (RESTRT) GO TO 4740
        CALL LOGIT(MILES)
        IF (Z .EQ. FP0) GOTO 4850
        WRITE(OUT, 4660) ZNAME, ZNAME, ZNAME, ZNAME
4660    FORMAT(' Since comparison denies  ',A8,' = 0,'/
     ~      '  evaluating  (',A8,' + ',A8,') / ',A8,'  should be safe;')
        DUMMY = (Z + Z) / Z
        WRITE(OUT, 4665) ZNAME, ZNAME, ZNAME, DUMMY
4665    FORMAT(' what the machine gets for  (',A8,' + ',A8,') / ',A8,
     ~ '  is'/10X, E15.7)
        IF ( ABS(DUMMY-FP2) .LT. RADIX*ULPPLS) GO TO 4750
4670    IF (DUMMY .LT. FP1 .OR. DUMMY .GT. FP2) GOTO 4740
        PARTU=1
        DEFECT=DEFECT+1
        WRITE(OUT,4675)
4675    FORMAT(' This is a DEFECT.'/)
        GOTO 4760
4740    PARTU=1
        SDEFCT=SDEFCT+1
        WRITE(OUT,4745)
4745    FORMAT(' This is a VERY SERIOUS DEFECT.')
        GOTO 4760
4750    WRITE(OUT,4751)
4751    FORMAT(' This is O.K. provided over/underflow has not just been
     ~ signaled.')
4760    MULTP1=Z*FP1
        MULTP2=FP1*Z
        DIVTMP=Z/FP1
        IF (Z .EQ. MULTP1 .AND. Z .EQ. MULTP2 .AND. Z .EQ. DIVTMP)
     ~ GO TO 4840
4770    PARTU=1
        DEFECT=DEFECT+1
        WRITE(OUT,4780)ZNAME,Z
4780    FORMAT(' DEFECT:  what prints as  ',A8,' =',E16.8,
     ~ ' compares different from')
4790    IF (.NOT. (Z .EQ. MULTP1)) WRITE(OUT,4795)ZNAME,MULTP1
4795    FORMAT('           ',A8,'*1 = ',E16.8)
4800    IF (.NOT. (Z .EQ. MULTP2 .OR. MULTP2 .EQ. MULTP1)) WRITE(OUT,
     ~      4805) ZNAME,MULTP2
4805    FORMAT('           1*',A8,' = ',E16.8)
4810    IF (.NOT. (Z .EQ. DIVTMP)) WRITE(OUT,4815)ZNAME,DIVTMP
4815    FORMAT('           ',A8,'/1 = ',E16.8)
4820    IF (MULTP2 .EQ. MULTP1) GO TO 4840
4830    DEFECT=DEFECT+1
        WRITE(OUT,4831)
4831    FORMAT(' DEFECT: multiplication does not commute; comparison all
     ~eges that')
        WRITE(OUT,4833)ZNAME,MULTP2,ZNAME,MULTP1
4833    FORMAT('         1*',A8,' =',E16.8,'  differs from  ',A8,'*1 ='
     ~ ,E16.8)
4840    IF (PARTU .GT. 0) CALL PAGE(MILES)
C       ---- PAUSE ----
4850    RETURN
        END
        SUBROUTINE POWER(MILES,FROM)
C---
        COMMON /STDIO/ IN, OUT
        INTEGER IN, OUT
        COMMON /GLOBAL/ FAILS, SDEFCT, DEFECT, FLAWS, RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
        INTEGER         FAILS, SDEFCT, DEFECT, FLAWS
        REAL                              RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
C
C        FAILS  ... NUMBER OF FAILURES
C        SDEFCT ... NUMBER OF SERIOUS DEFECTS
C        DEFECT ... NUMBER OF DEFECTS
C        FLAWS  ... NUMBER OF FLAWS
C
C        RADIX  ... COMPUTED RADIX OF THE MACHINE
C        ULPPLS ... ONE UNIT IN THE LAST PLACE (ULP) OF 1+EPSILON
C        ULPMIN ... ONE UNIT IN THE LAST PLACE (ULP) OF 1-EPSILON
C        PRECIS ... COMPUTED PRECISION OF THE MACHINE
C        W      ... RADIX ** PRECIS
C        MULGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN MULTIPLY
C        DIVGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN DIVIDE
C        SUBGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN ADD/SUBTRACT
C        A1     ...
C        ONEMIN ... ONE MINUS ULPMIN = ~.99999999999999
C
        INTEGER MILES,FROM
C       ... LOCAL VARIABLES
        INTEGER I, M, N, N1
        INTEGER NUMTRY
        REAL X, Z, A
C       ... CONSTANTS
        REAL FP0, FP1, FP2, FP3, FP4, FP8, MINUS1
C
        FP0 = 0.0
        FP1 = 1.0
        FP2 = FP1 + FP1
        FP3 = FP2 + FP1
        FP4 = FP3 + FP1
        FP8 = FP4 + FP4
        MINUS1 = -FP1
        A = FP1 / A1
        NUMTRY = 20
C
3970    WRITE(OUT,3971)
3971    FORMAT(' Testing powers  z^i  for small integers  z  and  i :')
        IF(FROM .NE. 90) WRITE(OUT,3972)
3972    FORMAT(' Start with 0.**0 .')
4120    N = 0
        I = 0
        Z = -FP0
        M = 3
        IF(FROM.EQ.90) GOTO 4160
        IF (FROM .NE. 0) CALL BADMIL
C                       TEST POWERS OF ZERO
4130    X = FP1
        CALL PWRCMP (X, Z, I, M, N)
        IF (I .GT. 10) GOTO 4150
4140    I = 1023
        CALL PWRCMP (X, Z, I, M, N)
4150    IF (Z .EQ. MINUS1) GOTO  4170
C                       IF (-1)^N IS INVALID, REPLACE 'MINUS1' BY 'FP1'
4160    Z = MINUS1
        I = -4
        GOTO 4130
C                       PRINT  N  IF  N > 0.
4170    CALL PRTCNT (N)
4180    N1 = N
        N = 0
        Z = A1
        M =   INT(FP2 * ALOG(W) / ALOG(A1) )
C
C                       LOOP
C
4190    X = Z
        I = 1
        CALL PWRCMP (X, Z, I, M, N)
        IF (Z .EQ. A) GOTO 4210
4200    Z = A
        GOTO 4190
C
C       POWERS OF RADIX  B  HAVE BEEN TESTED; NEXT TRY A FEW PRIMES.
C
4210    MILES=100
4220    M = NUMTRY
        Z = FP3
4230    X = Z
        I = 1
        CALL PWRCMP (X, Z, I, M, N)
4240    Z = Z + FP2
        IF (FP3 * AINT (Z / FP3) .EQ. Z) GOTO 4240
4250    IF (Z .LT. FP8 * FP3) GOTO 4230
4260    IF (N .GT. 0) WRITE(OUT,4261)
4261    FORMAT(' Error like this may invalidate financial'/
     ~ ' calculations involving interest rates.')
4270    CALL PRTCNT (N)
        N = N + N1
        CALL PRT2 (N, MILES)
        RETURN
        END
C---
        SUBROUTINE RADX(MILES)
C
        COMMON /STDIO/ IN, OUT
        INTEGER IN, OUT
        COMMON /GLOBAL/ FAILS, SDEFCT, DEFECT, FLAWS, RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
        INTEGER         FAILS, SDEFCT, DEFECT, FLAWS
        REAL                              RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
C
C        FAILS  ... NUMBER OF FAILURES
C        SDEFCT ... NUMBER OF SERIOUS DEFECTS
C        DEFECT ... NUMBER OF DEFECTS
C        FLAWS  ... NUMBER OF FLAWS
C
C        RADIX  ... COMPUTED RADIX OF THE MACHINE
C        ULPPLS ... ONE UNIT IN THE LAST PLACE (ULP) OF 1+EPSILON
C        ULPMIN ... ONE UNIT IN THE LAST PLACE (ULP) OF 1-EPSILON
C        PRECIS ... COMPUTED PRECISION OF THE MACHINE
C        W      ... RADIX ** PRECIS
C        MULGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN MULTIPLY
C        DIVGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN DIVIDE
C        SUBGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN ADD/SUBTRACT
C        A1     ...
C        ONEMIN ... ONE MINUS ULPMIN = ~.99999999999999
C
        COMMON /CONST/   FP0, FP1, FP2, FP3, FP4, FP8, FP9, FP27, FP32,
     ~                   HALF, MINUS1
        REAL FP0, FP1, FP2, FP3, FP4, FP8, FP9, FP27, FP32,
     ~                   HALF, MINUS1
C
        INTEGER MILES
C        ... COUNT OF MILESTONES PASSED
        REAL X,Y,Z
C        ... SCRATCH VARIABLES
        REAL ULPMSV,RADSAV
C        ... TEMPS TO SAVE ULPMIN, ULPPLS, RADIX, PRECIS IN WHILE
C        ... RECOMPUTING
        REAL THIRD,SIXTH
C        ... TEMPS TO HOLD APPROX VALUES OF 1/3 AND 1/6 IN WHILE
C        ... ACCUMULATING ERROR FOR RADIX COMPUTATIONS.
        REAL B9, T8
        INTEGER I
C
        T8 = 240.0
C
        WRITE(OUT,1160)
1160    FORMAT(/' Searching for radix and precision...')
C       LOOKING FOR W TO BE BIG ENOUGH TO MAKE 1 INSIGNIFICANT AT THE
C       PRECISION AVAILABLE.  INCREASE BY POWERS OF 2 UNTIL WE FIND IT.
1170    W=FP1
1180    W=W+W
        Y=W+FP1
        Z=Y-W
        Y=Z-FP1
        IF ((-FP1+ ABS(Y)) .LT. FP0) GOTO 1180
C       ... NOW  W  IS JUST BIG ENOUGH THAT  |((W+1)-W)-1| >= 1 ...
C        I.E. 1 IS INSIGNIFICANT RELATIVE TO W.
1200    PRECIS=FP0
        Y=FP1
1210    RADIX=W+Y
        Y=Y+Y
        RADIX=RADIX-W
        IF (RADIX .EQ. FP0) GOTO 1210
C
1220    IF (RADIX .GE. FP2) GOTO 1235
        RADIX = FP1
        WRITE(OUT,1230)     RADIX
1230    FORMAT(' Radix = ',F7.0)
        GOTO 1270
C          BASE IS 1, SO IT IS NOT CHARACTERIZED BY A PRECISION, SO
C          DON'T BOTHER TO HUNT FOR IT..
C
C
C               ... RADIX >= 2 ...
C        NOW TRY TO FIND THE PRECISION (# SIG. DIGITS)
1235    WRITE(OUT,1236) RADIX
1236    FORMAT(' Radix = ',F4.0)
1240    W=FP1
1250    PRECIS=PRECIS+FP1
        W=W*RADIX
        Y=W+FP1
        Z=Y-W
        IF (Z .EQ. FP1) GOTO 1250
C        ... NOW  W = RADIX^PRECIS  IS BARELY TOO BIG TO SATISFY
C        ... (W+1)-W = 1  .
1270    ULPMIN=FP1/W
        ULPPLS=RADIX*ULPMIN
        WRITE(OUT,1271) ULPMIN
        WRITE(OUT,1280)
1271    FORMAT(' Closest relative separation found is ',1PE16.8)
1280    FORMAT(' Recalculating radix and precision ')
1290    RADSAV=RADIX
        ULPMSV=ULPMIN
C                                               ...  SAVE OLD VALUES
1300    X=FP4/3.0E0
        THIRD=X-FP1
        SIXTH=(FP1/FP2)-THIRD
        X=SIXTH+SIXTH
        X= ABS(X-THIRD)
        IF (X .LT. ULPPLS) X=ULPPLS
C       ... NOW  X = (UNKNOWN NO.) ULPS OF  1 + ...
1320    ULPPLS=X
        Y=(FP1/FP2)*ULPPLS+3.2E1*ULPPLS*ULPPLS
        Y=FP1+Y
        X=Y-FP1
C       X=> ((X/2) + EPSILON) MOD (1 ULP OF 1+)
        IF (ULPPLS .GT. X .AND. X .GT. FP0) GOTO 1320
C        IF X DOES NOT UNDERFLOW TO 0, THEN IT IS STILL (UNKNOWN) * ULP
C        SO TRY AGAIN....  OTHERWISE, PREVIOUS VALUE (ULPPLS) IS 1 ULP
C       ... NOW  ULPPLS = 1 ULP OF  1 + ...
1340    X=FP2/3.0E0
        SIXTH=X-(FP1/FP2)
        THIRD=SIXTH+SIXTH
        X=THIRD-(FP1/FP2)
        X= ABS(X+SIXTH)
        IF (X .LT. ULPMIN) X=ULPMIN
C       ... NOW  X = (UNKNOWN NO.) ULPS OF  1 - ...
1360    ULPMIN=X
        Y=(FP1/FP2)*ULPMIN+3.2E1*ULPMIN*ULPMIN
        Y=(FP1/FP2)-Y
        X=(FP1/FP2)+Y
        Y=(FP1/FP2)-X
        X=(FP1/FP2)+Y
C        X => (X/2 = EPSILON) MOD (1 ULP OF 1-)
        IF (ULPMIN .GT. X .AND. X .GT. FP0) GOTO 1360
C       ... NOW  ULPMIN = 1 ULP OF  1 - ...
C
C       NOW TO SUMMARIZE THE RESULTS
C
1380    IF (ULPMIN .EQ. ULPMSV) WRITE(OUT,1381)
1381    FORMAT(' confirms closest relative separation .')
1390    IF (ULPMIN .NE. ULPMSV) WRITE(OUT,1391) ULPMIN
1391    FORMAT(' gets better closest relative separation = ', E13.5)
1400    W=FP1/ULPMIN
        ONEMIN = (HALF - ULPMIN) + HALF
C       ... = 1 - ULPMIN = NEXTAFTER(1.0, 0)
1410    RADIX=AINT(.01 + ULPPLS/ULPMIN)
        IF (RADIX .EQ. RADSAV) WRITE(OUT,1411)
1411    FORMAT(' Radix confirmed.')
1420    IF (RADIX .NE. RADSAV) WRITE(OUT,1421) RADIX
1421    FORMAT(' mystery: recalculated radix = ', E13.5)
C
C       ... RADICES 1, 2 AND 10 PASS MUSTER
C
1430    IF (RADIX .EQ. FP2 .OR. RADIX .EQ. 1.0E1 .OR. RADIX .EQ. FP1)
     ~ GOTO 1470
1440    IF (RADIX .GT. 1.6E1) GOTO 1460
1450    FLAWS=FLAWS+1
        WRITE(OUT,1451) RADIX
1451    FORMAT(' FLAW: radix =',F4.0,' is not so good as 2 or 10.')
        GOTO 1470
1460    DEFECT=DEFECT+1
        WRITE(OUT,1461) RADIX
1461    FORMAT(' DEFECT: radix =',F4.0,' is so big that roundoff propaga
     ~tes capriciously.')
1470    MILES=20
C       TEST FOR FUZZY COMPARISON ... ==================================
1480    IF (ONEMIN-HALF .LT. HALF) GOTO 1510
1490    FAILS=FAILS+1
        WRITE(OUT,1500)
1500    FORMAT(' FAILURE: (1-u1)-1/2 < 1/2  is false, so this program ma
     ~y malfunction.')
1510    X=ONEMIN
        I=1
1520    Y=X-HALF
        Z=Y-HALF
        IF (X .NE. FP1 .OR. Z .EQ. FP0) GOTO 1540
1530    FAILS=FAILS+1
        WRITE(OUT,1535)
1535    FORMAT(' FAILURE: comparison is fuzzy; it alleges x=1 although')
        WRITE(OUT,1537)Z
1537    FORMAT('         subtraction yields  (x - 1/2) - 1/2 = ',D16.8)
1540    IF (I .EQ. 0) GOTO 1560
1550    X=FP1+ULPPLS
        I=0
        GOTO 1520
1560    MILES=25
C       END OF TEST FOR FUZZY COMPARISON.===============================
1570    B9 = RADIX - FP1
        B9=(B9-ULPPLS)+FP1
        IF (RADIX .EQ. FP1) GO TO 1610
C       ... B9 = NEXTAFTER(RADIX, 0)
1580    X=-T8*ALOG(ULPMIN)/ALOG(RADIX)
        Y=AINT(HALF+X)
        IF (  ABS(X-Y)*FP4 .LT. FP1 ) X=Y
1590    PRECIS = X/T8
        Y=AINT(HALF + PRECIS)
        IF (  ABS(PRECIS-Y)*T8 .LT. HALF ) PRECIS=Y
C       PURIFY INTEGERS.
1600    IF (PRECIS .EQ. AINT(PRECIS)) GOTO 1640
1610    WRITE(OUT,1611)
1611    FORMAT(' Precision cannot be characterized by an integer number
     ~of sig. digits;')
        IF (RADIX .GT. FP1) GOTO 1625
        WRITE(OUT,1620)
1620    FORMAT(' Logarithmic encoding (radix=1) has precision characteri
     ~zed solely by  u1 .')
        GOTO 1650
1625    WRITE(OUT,1630)
1630    FORMAT(' but, by itself, this is a minor flaw.')
1640    WRITE(OUT,1641) RADIX,PRECIS
1641    FORMAT(' The number of significant digits of radix ',F4.0,
     ~        ' is ' , F6.2)
1650    IF (ULPPLS*FP9*FP9*T8 .LT. FP1) GO TO 1670
1660    SDEFCT=SDEFCT+1
        WRITE(OUT,1665)
1665    FORMAT(' SERIOUS DEFECT: precision worse than  5 sig. dec. is us
     ~ually inadequate.')
1670    RETURN
        END
C---
        SUBROUTINE ROUND (MILES)
        INTEGER MILES
C
C
        COMMON /STDIO/ IN, OUT
        INTEGER IN, OUT
        COMMON /GLOBAL/ FAILS, SDEFCT, DEFECT, FLAWS, RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
        INTEGER         FAILS, SDEFCT, DEFECT, FLAWS
        REAL                              RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
C
C        FAILS  ... NUMBER OF FAILURES
C        SDEFCT ... NUMBER OF SERIOUS DEFECTS
C        DEFECT ... NUMBER OF DEFECTS
C        FLAWS  ... NUMBER OF FLAWS
C
C        RADIX  ... COMPUTED RADIX OF THE MACHINE
C        ULPPLS ... ONE UNIT IN THE LAST PLACE (ULP) OF 1+EPSILON
C        ULPMIN ... ONE UNIT IN THE LAST PLACE (ULP) OF 1-EPSILON
C        PRECIS ... COMPUTED PRECISION OF THE MACHINE
C        W      ... RADIX ** PRECIS
C        MULGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN MULTIPLY
C        DIVGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN DIVIDE
C        SUBGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN ADD/SUBTRACT
C        A1     ...
C        ONEMIN ... ONE MINUS ULPMIN = ~.99999999999999
C
        COMMON /CONST/   FP0, FP1, FP2, FP3, FP4, FP8, FP9, FP27, FP32,
     ~                   HALF, MINUS1
        REAL FP0, FP1, FP2, FP3, FP4, FP8, FP9, FP27, FP32,
     ~                   HALF, MINUS1
C
C
        COMMON /ROUNDN/  R1, R2, R3, R4, STICKY
        REAL R1, R2, R3, R4, STICKY
C
C       ... LOCAL VARIABLES
        REAL A, B1, Q, S1, T, X, Y, Y1, Y2, Z
C       ... CONSTANTS
        REAL B2, T5, B9
C
        B2 = RADIX / FP2
        T5 = FP1 + HALF
        B9 = ((RADIX - FP1) - ULPPLS) + FP1
C
        WRITE(OUT,2250)
2250    FORMAT(' Checking for rounding in multiply,',
     ~          ' divide and add/subtract:')
2260    R1 = MINUS1
        R2 = MINUS1
        R3 = MINUS1
C               IS  RADIX  A POWER OF  2  OR  10 ?
2270    A1 = FP2
2280    A = RADIX
2290    X = A
        A = A / A1
        IF (AINT(A) .EQ. A) GOTO 2290
2300    IF (X .EQ. FP1) GOTO 2340
C               RADIX  IS A POWER OF  A1; IF RADIX=1 THEN  A1=2.
2310    IF (A1 .GT. FP3) GOTO 2330
2320    A1 = FP9 + FP1
        GOTO 2280
C               IS  RADIX  A POWER OF  10 ?
2330    A1 = RADIX
C               UNLESS  B  IS A POWER OF  A1  AND  A1 = 2 OR 10.
2340    A=FP1/A1
        X=A1
        Y=A
2350    Z=X*Y-HALF
        IF (Z .EQ. HALF) GOTO 2370
2360    FAILS=FAILS+1
        WRITE(OUT,2361) X, Y, X, X
2361    FORMAT(' FAILURE:  1/',E13.5,' = ',E13.5,', and  ',
     ~         E13.5,'*(1/',E13.5,') differs from  1.')
2370    IF (X .EQ. RADIX) GOTO 2390
2380    X = RADIX
        Y = FP1 / X
        GOTO 2350
2390    Y2=FP1+ULPPLS
        Y1=FP1-ULPPLS
        X=T5-ULPPLS
        Y=T5+ULPPLS
        Z=(X-ULPPLS)*Y2
        T=Y*Y1
        Z=Z-X
        T=T-X
        X=X*Y2
        Y=(Y+ULPPLS)*Y1
        X=X-T5
        Y=Y-T5
        IF (.NOT. ( X .EQ. FP0 .AND. Y .EQ. FP0 .AND. Z .EQ. FP0 .AND.
     ~             T .LE. FP0 )) GOTO 2460
2400    X=(T5+ULPPLS)*Y2
        Y=T5-ULPPLS-ULPPLS
        Z=T5+ULPPLS+ULPPLS
        T=(T5-ULPPLS)*Y1
        X=X-(Z+ULPPLS)
        STICKY = Y * Y1
        S1 = Z * Y2
        T = T - Y
        Y = (ULPPLS-Y) + STICKY
        Z = S1 - (Z+ULPPLS+ULPPLS)
        STICKY = (Y2+ULPPLS) * Y1
        Y1 = Y2 * Y1
        STICKY = STICKY - Y2
        Y1 = Y1 - HALF
2410    IF ( X .EQ. FP0 .AND. Y      .EQ. FP0 .AND. Z  .EQ. FP0 .AND.
     ~       T .EQ. FP0 .AND. STICKY .EQ. FP0 .AND. Y1 .EQ. HALF )
     ~    R1 = FP1
2420    IF ( X+ULPPLS        .EQ. FP0 .AND. Y  .LT. FP0 .AND.
     ~       Z+ULPPLS        .EQ. FP0 .AND. T  .LT. FP0 .AND.
     ~       STICKY + ULPPLS .EQ. FP0 .AND. Y1 .LT. HALF )
     ~    R1 = FP0
2430    IF (R1 .EQ. FP0) WRITE(OUT,2431)
2431    FORMAT(' Multiplication appears to be chopped.')
2440    IF (R1 .EQ. FP1) WRITE(OUT,2441)
2441    FORMAT (' Multiplication appears to be correctly rounded.')
2450    IF (R1-MULGRD .EQ. FP1) WRITE(OUT,2451)
2451    FORMAT(' FAILURE: multiplication test is inconsistent; ',
     ~ 'PLEASE NOTIFY KARPINSKI !')
2460    IF (R1 .EQ. MINUS1) WRITE(OUT,2461)
2461    FORMAT(' Multiplication is neither chopped nor correctly rounded
     ~.')
2470    MILES=45
C       ================================================================
2480    Y2=FP1+ULPPLS
        Y1=FP1-ULPPLS
        Z=T5+ULPPLS+ULPPLS
        X=Z/Y2
        T=T5-ULPPLS-ULPPLS
        Y=(T-ULPPLS)/Y1
        Z=(Z+ULPPLS)/Y2
        X=X-T5
        Y=Y-T
        T=T/Y1
        Z=Z-(T5+ULPPLS)
        T=(ULPPLS-T5)+T
        IF ( X .GT. FP0 .OR. Y .GT. FP0 .OR. Z .GT. FP0 .OR. T .GT. FP0)
     ~     GOTO 2540
2490    X=T5/Y2
        Y=T5-ULPPLS
        Z=T5+ULPPLS
        X=X-Y
        T=T5/Y1
        Y=Y/Y1
        T=T-(Z+ULPPLS)
        Y=Y-Z
        Z=Z/Y2
        Y1=(Y2+ULPPLS)/Y2
        Z=Z-T5
        Y2=Y1-Y2
        Y1=(ONEMIN-ULPMIN)/ONEMIN
        IF ( X .EQ. FP0 .AND. Y  .EQ. FP0 .AND. Z       .EQ. FP0   .AND.
     ~      T .EQ. FP0 .AND. Y2 .EQ. FP0 .AND. Y1-HALF .EQ.
     ~ ONEMIN-HALF ) R2=FP1
2500    IF ( X .LT. FP0 .AND. Y  .LT. FP0 .AND. Z       .LT. FP0   .AND.
     ~      T .LT. FP0 .AND. Y2 .LT. FP0 .AND. Y1-HALF .LT.
     ~ ONEMIN-HALF ) R2=FP0
2510    IF (R2 .EQ. FP0) WRITE(OUT,2511)
2511    FORMAT (' Division appears to be chopped.')
2520    IF (R2 .EQ. FP1) WRITE(OUT,2521)
2521    FORMAT (' Division appears to be correctly rounded.')
2530    IF (R2-DIVGRD .EQ. FP1) WRITE(OUT,2531)
2531    FORMAT(' FAILURE:  division test is inconsistent;
     ~ PLEASE NOTIFY KARPINSKI !')
2540    IF (R2 .EQ. MINUS1) WRITE(OUT,2541)
2541    FORMAT (' Division is neither chopped nor correctly rounded.')
C
C       ================================================================
C
2550    B1 = FP1 / RADIX
        IF (B1 * RADIX - HALF .EQ. HALF) GOTO 2580
2560    FAILS=FAILS+1
        WRITE(OUT,2570)
2570    FORMAT(' FAILURE:  radix * (1 / radix)  differs from  1.')
2580    MILES=50
C
C       ================================================================
C
2590    IF ( (ONEMIN + ULPMIN) - HALF .EQ. HALF         .AND.
     ~      (B9 + ULPPLS) - FP1  .EQ. RADIX - FP1 )
     ~     GOTO 2610
2600    FAILS=FAILS+1
        WRITE(OUT,2601)
2601    FORMAT(' FAILURE: incomplete carry-propagation in addition.')
2610    X = FP1 - ULPMIN * ULPMIN
        Y = FP1 + ULPPLS * (FP1 - ULPPLS)
        Z = ONEMIN - HALF
        X = (X - HALF) - Z
        Y = Y - FP1
2620    IF (X .NE. FP0 .OR. Y .NE. FP0) GOTO 2640
2630    R3 = FP0
        WRITE(OUT,2631)
2631    FORMAT (' Add/subtract appears to be chopped.')
2640    IF (SUBGRD .EQ. FP0) GOTO 2710
2650    X = (HALF + ULPPLS) * ULPPLS
        Y = (HALF - ULPPLS) * ULPPLS
        X = FP1 + X
        Y = FP1 + Y
        X = (FP1 + ULPPLS) - X
        Y = FP1 - Y
2660    IF (X .NE. FP0 .OR. Y .NE. FP0) GOTO 2710
2670    X = (HALF + ULPPLS) * ULPMIN
        Y = (HALF - ULPPLS) * ULPMIN
        X = FP1 - X
        Y = FP1 - Y
        X = ONEMIN - X
        Y = FP1 - Y
2680    IF (X .NE. FP0 .OR. Y .NE. FP0) GOTO 2710
2690    R3 = MINUS1 - FP2 * R3
        WRITE(OUT,2691)
2691    FORMAT (' Add/subtract appears to be correctly rounded.')
2700    IF (R3 - SUBGRD .EQ. FP1) WRITE(OUT,2701)
2701    FORMAT(' FAILURE:  add/subtract test is inconsistent;
     ~ PLEASE NOTIFY KARPINSKI !')
2710    IF (R3 .EQ. MINUS1) WRITE(OUT,2711)
2711    FORMAT(' Add/subtract neither chopped nor correctly rounded.')
2720    S1 = FP1
        X = FP1+HALF*(FP1+HALF)
        Y = (FP1+ULPPLS)*HALF
        Z = X-Y
        T = Y-X
        STICKY = Z+T
2730    IF (STICKY .EQ. FP0) GOTO 2770
2740    S1=FP0
        FLAWS=FLAWS+1
        WRITE(OUT,2750) STICKY
        WRITE(OUT,2760) X, Y
2750    FORMAT(' FLAW: nonzero  (x-y)+(y-x) = ',E16.8,' when')
2760    FORMAT('      x = ',E16.8,'  and  y = ',E16.8)
C
C       ================================================================
C
2770    STICKY = FP0
        IF (MULGRD*DIVGRD*SUBGRD .LT. FP1 .OR.
     ~      R1 .LT. FP1                   .OR.
     ~      R2 .LT. FP1                   .OR.
     ~      R3 .LT. FP1                   .OR.
     ~      AINT(B2) .NE. B2                    ) GOTO 2890
        WRITE(OUT,2780)
2780    FORMAT(' checking for sticky bit:')
2790    X=(HALF+ULPMIN)*ULPPLS
        Y=HALF*ULPPLS
        Z=FP1+Y
        T=FP1+X
        IF (Z-FP1 .GT. FP0 .OR. T-FP1 .LT. ULPPLS) GOTO 2890
2800    Z=T+Y
        Y=Z-X
        IF (Z-T .LT. ULPPLS .OR. Y-T .NE. FP0) GOTO 2890
2810    X=(HALF+ULPMIN)*ULPMIN
        Y=HALF*ULPMIN
        Z=FP1-Y
        T=FP1-X
        IF (Z-FP1 .NE. FP0 .OR. T-ONEMIN .NE. FP0) GOTO 2890
2820    Z=(HALF-ULPMIN)*ULPMIN
        T=ONEMIN-Z
        Q=ONEMIN-Y
        IF (T-ONEMIN .NE. FP0 .OR. (ONEMIN-ULPMIN)-Q .NE. FP0) GOTO 2890
2830    Z = (FP1 + ULPPLS) * T5
        T = (T5 + ULPPLS) - Z + ULPPLS
        X = FP1 + HALF / RADIX
        Y = FP1 + RADIX * ULPPLS
        Z = X * Y
2840    IF (T .NE. FP0 .OR. (X + RADIX * ULPPLS) - Z .NE. FP0) GOTO 2890
2850    IF (RADIX .EQ. FP2) GOTO 2870
2860    X = FP2 + ULPPLS
        Y = X / FP2
        IF (Y - FP1 .NE. FP0) GOTO 2890
2870    STICKY = S1
2880    IF (STICKY .EQ. FP1) WRITE(OUT,2881)
2881    FORMAT (' Sticky bit appears to be used correctly.')
2890    IF (STICKY .EQ. FP0)  WRITE(OUT,2891)
2891    FORMAT (' Sticky bit used incorrectly or not at all.')
2900    IF (MULGRD*DIVGRD*SUBGRD .EQ. FP0 .OR.
     ~      R1 .LT. FP0                   .OR.
     ~      R2 .LT. FP0                   .OR.
     ~      R3 .LT. FP0                        ) THEN
           FLAWS=FLAWS+1
           WRITE(OUT,29001)
29001      FORMAT(' FLAW: lack(s) of guard digits or failure(s) to corre
     ~ctly round or chop'/' (noted above) count as one flaw in the final
     ~ tally below.')
           END IF
        RETURN
        END
C---
        SUBROUTINE SMLINT(MILES,FROM)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C       TESTS ON SMALL INTEGERS
C
C
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        COMMON /STDIO/ IN, OUT
        INTEGER IN, OUT
        COMMON /GLOBAL/ FAILS, SDEFCT, DEFECT, FLAWS, RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
        INTEGER  FROM, FAILS, SDEFCT, DEFECT, FLAWS
        REAL                              RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
C
C        FAILS  ... NUMBER OF FAILURES
C        SDEFCT ... NUMBER OF SERIOUS DEFECTS
C        DEFECT ... NUMBER OF DEFECTS
C        FLAWS  ... NUMBER OF FLAWS
C
C        RADIX  ... COMPUTED RADIX OF THE MACHINE
C        ULPPLS ... ONE UNIT IN THE LAST PLACE (ULP) OF 1+EPSILON
C        ULPMIN ... ONE UNIT IN THE LAST PLACE (ULP) OF 1-EPSILON
C        PRECIS ... COMPUTED PRECISION OF THE MACHINE
C        W      ... RADIX ** PRECIS
C        MULGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN MULTIPLY
C        DIVGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN DIVIDE
C        SUBGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN ADD/SUBTRACT
C        A1     ...
C        ONEMIN ... ONE MINUS ULPMIN = ~.99999999999999
C
        INTEGER IPARTU
        INTEGER MILES
C        ... INTEGER NUMBER IF MILESTONES REACHED
        CHARACTER*8 CHARZ
C        ... CHARACTER CONSTANT 'Z'
        REAL MINONE
C        ... TEMPORARY TO HOLD MINUS ONE
        REAL HALF
C        ... TEMPORARY TO HOLD ONE HALF
        REAL FIVE
C        ... TEMPORARY TO HOLD FIVE
        REAL EIGHT
C        ... TEMPORARY TO HOLD EIGHT
        REAL NINE
C        ... TEMPORARY TO HOLD NINE
        REAL TEMP12
C        ... TEMPORARY TO HOLD 12
        REAL TEMP20
C        ... TEMPORARY TO HOLD 20
        REAL TEMP27
C        ... TEMPORARY TO HOLD 27
        REAL TEMP32
C        ... TEMPORARY TO HOLD 32
        REAL TEMP48
C        ... TEMPORARY TO HOLD 48
        REAL TEMP60
C        ... TEMPORARY TO HOLD 60
        REAL TEMP80
C        ... TEMPORARY TO HOLD 80
        REAL TMP240
C        ... TEMPORARY TO HOLD 240
        REAL TEMP
C        ... TEMPORARY VARIABLE TO HOLD VARIOUS VERY SHORT TERM VALUES
        REAL TEMPZ
C        ... TEMPORARY VARIABLE TO HOLD A TEMP. PREVIOUSLY KNOWN AS Z
C        ... INITIALIZE SOME CONSTANTS
        DATA CHARZ/'Z'/
        IF (FROM .EQ. 7) GO TO 951
        IF (FROM .NE. 0) CALL BADMIL
        WRITE(OUT,891)
891     FORMAT(' Program is now RUNNING tests on small integers:')
C
C       ... LOOK FOR SOME OBVIOUS MISTAKES
        IF (0.0E0+0.0E0 .EQ. 0.0E0 .AND.
     ~     1.0E0-1.0E0 .EQ. 0.0E0 .AND.
     ~     1.0E0       .GT. 0.0E0 .AND.
     ~     1.0E0+1.0E0 .EQ. 2.0E0) GOTO 930
        FAILS=FAILS+1
        WRITE(OUT,920)
920     FORMAT(' FAILURE: violation of  0+0=0  or  1-1=0  or  1>0  or
     ~ 1+1 = 2.')
930     TEMP=0.0E0
        TEMPZ=-TEMP
        IF (TEMPZ .EQ. 0.0E0) GOTO 960
        FAILS=FAILS+1
        WRITE(OUT,940)
        WRITE(OUT,941)
940     FORMAT(' FAILURE: comparison alleges that minus zero, obtained
     ~by')
941     FORMAT(' setting  x = 0.  and then  z = -x ,  is nonzero!')
C        ... CALL TO ROUTINE TO CHECK FOR PARTIAL UNDERFLOW USING MINUS
C        ... ZERO DON'T REALLY HAVE INFO ON WHAT A UNIT IN THE LAST
C        ... PLACE IS OR WHAT THE RADIX IS SINCE WE HAVEN'T GOTTEN TO
C        ... SUCH SOPHISTICATED STUFF YET, SO PICK SOME ARBITRARY VALUES
C        ... FOR NOW TO GET US THROUGH THIS NEXT TEST.
950     ULPPLS=.001
        RADIX=1
951     CALL PARTUF(TEMPZ, CHARZ, MILES, IPARTU, FROM .EQ. 7)
C
C
960     IF (4.0E0+2.0E0*(-2.0E0) .EQ. 0.0E0 .AND. (4.0E0-3.0E0)-1.0E0
     ~ .EQ. 0.0E0) GOTO 980
970     FAILS=FAILS+1
        WRITE(OUT,971)
971     FORMAT(' FAILURE: violation of   3+1 = 2*2 .')
C
980     MINONE=-1.0E0
        IF (MINONE+1.0E0 .EQ. 0.0E0 .AND. 1.0E0+MINONE .EQ. 0.0E0 .AND.
     ~ MINONE+ ABS(MINONE) .EQ. 0.0E0 .AND.
     ~ MINONE+MINONE*MINONE .EQ. 0.0E0) GOTO 1000
990     FAILS=FAILS+1
        WRITE(OUT,991)
991     FORMAT(' FAILURE: violation of   -1 + 1 = 0 .')
C
1000    HALF=1.0E0/2.0E0
        IF (HALF+MINONE+HALF .EQ. 0.0E0) GOTO 1020
1010    FAILS=FAILS+1
        WRITE(OUT,1011)
1011    FORMAT(' FAILURE: violation of   1/2 - 1 + 1/2 = 0 .')
1020    MILES=10
1100    NINE=3.0E0*3.0E0
        TEMP27=NINE*3.0E0
        EIGHT=4.0E0+4.0E0
        TEMP32=4.0E0*EIGHT
        IF (TEMP32-TEMP27-4.0E0-1.0E0 .EQ. 0.0E0) GOTO 1120
1110    FAILS=FAILS+1
        WRITE(OUT,1111)
1111    FORMAT(' FAILURE: violation of   32 - 27 - 4 - 1 = 0 .')
C
1120    FIVE=4.0E0+1.0E0
        TEMP20=4.0E0*FIVE
        TEMP12=3.0E0*4.0E0
        TMP240=TEMP20*TEMP12
1130    TEMP80=TMP240/3.0E0
        TEMP60=TMP240/4.0E0
        TEMP48=TMP240/FIVE
        TEMP80=TEMP80-4.0E0*TEMP20
        TEMP60=TEMP60-FIVE*TEMP12
        TEMP48=TEMP48-4.0E0*TEMP12
        IF ( TEMP80 .EQ. 0.0E0 .AND. TEMP60 .EQ. 0.0E0 .AND. TEMP48 .EQ.
     ~ 0.0E0 ) GOTO 1150
1140    FAILS=FAILS+1
        WRITE(OUT,1141)
1141    FORMAT(' FAILURE: violation of 240/3 = 80 or 240/4 = 60 or 240/5
     ~ = 48 .')
1150    IF (FAILS .NE. 0) GOTO 1160
        WRITE(OUT,1151)
1151    FORMAT (' -1, 0, 1/2 , 1, 2, 3, 4, 5, 9, 27, 32 & 240 are O.K.')
1160    RETURN
        END
C---
        SUBROUTINE SQUARE (FROM, MILES, NUMTRY)
C
        COMMON /STDIO/ IN, OUT
        INTEGER IN, OUT
        COMMON /GLOBAL/ FAILS, SDEFCT, DEFECT, FLAWS, RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
        INTEGER         FAILS, SDEFCT, DEFECT, FLAWS
        REAL                              RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
C
C        FAILS  ... NUMBER OF FAILURES
C        SDEFCT ... NUMBER OF SERIOUS DEFECTS
C        DEFECT ... NUMBER OF DEFECTS
C        FLAWS  ... NUMBER OF FLAWS
C
C        RADIX  ... COMPUTED RADIX OF THE MACHINE
C        ULPPLS ... ONE UNIT IN THE LAST PLACE (ULP) OF 1+EPSILON
C        ULPMIN ... ONE UNIT IN THE LAST PLACE (ULP) OF 1-EPSILON
C        PRECIS ... COMPUTED PRECISION OF THE MACHINE
C        W      ... RADIX ** PRECIS
C        MULGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN MULTIPLY
C        DIVGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN DIVIDE
C        SUBGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN ADD/SUBTRACT
C        A1     ...
C        ONEMIN ... ONE MINUS ULPMIN = ~.99999999999999
C
        COMMON /CONST/   FP0, FP1, FP2, FP3, FP4, FP8, FP9, FP27, FP32,
     ~                   HALF, MINUS1
        REAL FP0, FP1, FP2, FP3, FP4, FP8, FP9, FP27, FP32,
     ~                   HALF, MINUS1
C
        COMMON /ERRDAT/ OVRFLW, UNDFLW, DIVZER, ERRFLG
        INTEGER         OVRFLW, UNDFLW, DIVZER, ERRFLG
C        OVRFLW ... NUMBER OF OVERFLOWS SINCE LAST RESET
C        UNDFLW ... NUMBER OF UNDERFLOWS SINCE LAST RESET
C        DIVZER ... NUMBER OF DIVISIONS BY ZERO SINCE LAST RESET
C        ERRFLG ... FLAG THAT IS SET WHENEVER AN ERROR OCCURS
C
C
        COMMON /ROUNDN/  R1, R2, R3, R4, STICKY
        REAL R1, R2, R3, R4, STICKY
C
        INTEGER FROM, MILES, NUMTRY
C       ... LOCAL VARIABLES
        REAL D, D4, E5, E6, E7, Q, U, X, X1, X8,
     ~                   Y, Y1, Y2, Z, Z1, Z2
        REAL TEMP,TEMP1,TEMP2
        INTEGER I, J
C       ... CONSTANTS
        REAL B2
        REAL B9, B1
C
C                       TRAP INTEGER OVERFLOWS
C                       OTHER EXCEPTIONS ARE TRAPPED BY DEFAULT
        B2 = RADIX / FP2
        B9 = ((RADIX - FP1) - ULPPLS) + FP1
        B1 = FP1 / RADIX
        IF (FROM .EQ. 79) GO TO 3058
        IF (FROM .NE. 0) CALL BADMIL
C
3020    WRITE(OUT,3021)
3021    FORMAT(/' Running tests of square root...')
        MILES = 79
        CALL LOGIT(MILES)
        X = FP0
        I = 0
3030    Y =  SQRT(X)
        IF (Y .EQ. X .AND. Y - HALF .EQ. X - HALF) GOTO 3050
3040    FAILS = FAILS + 1
        WRITE(OUT,3041) X, Y
3041    FORMAT(' FAILURE:  sqrt(',E9.1,'), miscalculated as ',E15.7)
3050    X = -X
        I = I + 1
        IF (I .EQ. 1) GOTO 3030
        GO TO 3060
3058    WRITE(OUT,3059)
3059    FORMAT(' FAILURE:  sqrt(-0.0) stops the machine.')
        FAILS = FAILS + 1
        I = 2
3060    X = FP1
        I = I + 1
        IF (I .EQ. 3) GOTO 3030
C       ... RECORD MIN AND MAX ERRORS.
3070    E5 = FP0
        E7 = FP0
C       ... TEST WHETHER  SQRT(X*X)  =  X
3150    J = 0
        X = RADIX
        U = ULPPLS
        CALL SQRERR (X, U, J, E5, E7, .TRUE.)
3160    X = B1
        U = B1 * ULPMIN
        CALL SQRERR (X, U, J, E5, E7, .TRUE.)
3170    X = W
        U = FP1
        CALL SQRERR (X, U, J, E5, E7, .TRUE.)
3180    X = ULPMIN
        U = ULPMIN * ULPMIN
        CALL SQRERR (X, U, J, E5, E7, .TRUE.)
C                       IF SQRT HAS SERIOUS DEFECTS, THEN PAUSE
3190    IF (J .EQ. 0) GOTO 3210
3200    SDEFCT = SDEFCT + J
        CALL PAGE(MILES)
C
C
C
3210    WRITE(OUT,3211) NUMTRY
3211    FORMAT(' Testing if  sqrt(x*x)  =  x  for  ',I4,' integers  x.')
3220    J = 0
        X = FP2
        Y = RADIX
        IF (RADIX .EQ. FP1) GOTO 3240
C                       LOOP TO DETERMINE ??
3230    X = Y
        Y = RADIX * X
        IF (Y - X .LT. NUMTRY) GOTO 3230
3240    U = X*ULPPLS
C
        DO 3260 I = 1,NUMTRY
        X = X + FP1
        CALL SQRERR (X, U, J, E5, E7, .FALSE.)
        IF (J .GT. 0) GOTO 3280
3260    CONTINUE
C
        WRITE(OUT,3270)
3270    FORMAT(' Found no discrepancies.')
        GOTO 3300
3280    DEFECT = DEFECT + J
C
C                       TEST SQRT FOR MONOTONICITY.
C
3300    I = -1
        X = B9
        Y = RADIX
        Z = RADIX + RADIX * ULPPLS
C
C                       LOOP
C
3310    I = I + 1
        X =  SQRT(X)
        Q =  SQRT(Y)
        Z =  SQRT(Z)
        IF (.NOT. (X .GT. Q .OR. Q .GT. Z)) GOTO 3330
3320    DEFECT = DEFECT + 1
        WRITE(OUT,3321) Y
3321    FORMAT(' DEFECT:  sqrt(x) is non - monotonic for  x  near ',
     ~E15.7)
        GOTO 3390
3330    Q = AINT(Q + HALF)
        IF (.NOT. (I .GT. 0 .OR. Q*Q .EQ. RADIX)) GOTO 3380
3340    IF (I .GT. 0) GOTO 3360
3350    Y = Q
        X = Y - ULPPLS
        Z = Y + ULPPLS
        GOTO 3310
C
3360    IF (I .GT. 1) GOTO 3380
3370    Y = Y * B1
        X = Y - ULPMIN
        Z = Y + ULPMIN
        GOTO 3310
3380    WRITE(OUT,3381)
3381    FORMAT(' Sqrt has passed a test for monotonicity.')
3390    MILES = 80
C
C       TEST SQRT FOR ACCURACY  =====================================
C               E5 = MIN{ERROR + 1/2}
C               E7 = MAX{ERROR - 1/2}
C
3400    E5 = E5 + HALF
        E7 = E7 - HALF
3410    Y = ( SQRT(FP1 + ULPPLS) - FP1)/ULPPLS
        E6 = (Y - FP1) + ULPPLS/FP8
        IF (E6 .GT. E7) E7 = E6
3420    E6 = Y + ULPPLS/FP8
        IF (E6 .LT. E5) E5 = E6
3430    Y = (( SQRT(ONEMIN) - ULPPLS) - (FP1 - ULPPLS))/ULPMIN
        E6 = Y + ULPMIN/FP8
        IF (E6 .GT. E7) E7 = E6
3440    E6 = (Y + FP1) + ULPMIN/FP8
        IF (E6 .LT. E5) E5 = E6
3450    I = 0
        U = ULPPLS
        X = U
C
C                       LOOP
C
3460    I = I + 1
        Y =  SQRT((X + ULPMIN + X) + ONEMIN)
        Y = ((Y - ULPPLS) - ((FP1 - ULPPLS) + X))/U
        Z = ((ULPMIN - X) + ONEMIN)*HALF*X*X/U
        E6 = (Y + HALF) + Z
        IF (E6 .LT. E5) E5 = E6
3470    E6 = (Y - HALF) + Z
        IF (E6 .GT. E7) E7 = E6
3480    IF (I .EQ. 4) GOTO 3530
        IF (I .EQ. 2) GOTO 3520
3500    X = U *  SIGN(FP1,X) * AINT( FP8 / (FP9 *  SQRT(U)) )
        GOTO 3460
C
3520    U = ULPMIN
        X = -U
        GOTO 3460
C
3530    MILES = 85
        R4 = MINUS1
3540    IF (RADIX .EQ. FP1) GOTO 3900
3550    WRITE(OUT,3551)
3551    FORMAT(' Testing whether  sqrt  is rounded or chopped:')
3560    D = AINT(HALF + RADIX ** (FP1 + PRECIS - AINT(PRECIS)))
C
C       ...  =  B^(1 + FRACT)  IF  P  =  INTEGER  +  FRACT.
C
3570    X = D / RADIX
        Y = D / A1
        IF (X .NE. AINT(X) .OR. Y .NE. AINT(Y)) GOTO 3700
3580    X = FP0
        Z2 = X
        Y = FP1
        Y2 = Y
        Z1 = RADIX - FP1
        D4 = FP4 * D
C
C       LOOP: FOR  Y  =  1, 3, 5, ...  MAXIMIZE  Y2  =  Y*Y MOD 4D .
C
3600    IF (.NOT. (Y2 .GT. Z2)) GOTO 3650
3610    Q = RADIX
        Y1 = Y
C                       IF NEW Y2 > OLD, CHECK THAT  GCD(Y,B)  =  1
3620    TEMP = HALF - Q / Y1
        TEMP1 = AINT(TEMP)
        IF (TEMP1 .GT. TEMP) TEMP1 = TEMP1 - FP1
        X1 =  ABS(Q + TEMP1 * Y1)
        Q = Y1
        Y1 = X1
        IF (X1 .GT. FP0) GOTO 3620
C
3630    IF (Q .GT. FP1) GOTO 3650
C                       IF GCD(Y,B)  .GT.  1 THEN SKIP OVER Y ;  ELSE
3640    Z2 = Y2
        Z = Y
C                       AND GCD(Z, RADIX)  = 1
3650    Y = Y + FP2
        X = X + FP8
        Y2 = Y2 + X
        IF (.NOT. (Y2 .LT. D4)) Y2 = Y2 - D4
C                       =  Y*Y MOD 4D
3660    IF (Y .LT. D) GOTO 3600
C                       ELSE  0 < Z < D  &  Z2 = Z^2 MOD 4D  IS MAXIMAL
3670    X8 = D4 - Z2
        Q = (X8 + Z * Z) / D4
        X8 = X8 / FP8
        IF (Q .NE. AINT(Q)) GOTO 3700
3680    X = Z1 * Z
        X = X - AINT(X / RADIX) * RADIX
        IF (X .EQ. FP1) GOTO 3800
C                       WITH  1  =  Z*Z1 MOD B
3690    Z1 = Z1 - FP1
        IF (Z1 .GT. FP0) GOTO 3680
C                       ELSE FAILURE!
3700    FAILS = FAILS + 1
        WRITE(OUT,3701) W
        WRITE(OUT,3702)
3701    FORMAT(' FAILURE: anomalous arithmetic with integers < b^p  = ',
     ~ E15.7)
3702    FORMAT ('         foils test whether  sqrt  rounds or chops.')
        GOTO 3940
C
C                       - B/2   <=   Z1 == 1/Z MOD B   <=   B/2
C
3800    IF (Z1 .GT. B2) Z1 = Z1 - RADIX
C
C                       LOOP UNTIL  D  =  B^(P - 1) .
C
3810    CALL NEWD (X, Z1, Q, Z, D)
        IF (ULPPLS * D .LT. ONEMIN) GOTO 3810
C
3820    IF (D * RADIX - D .NE. W - D) GOTO 3700
3830    Z2 = D
        I = 0
C               COUNT HOW MANY TESTS OF  SQRT(D*X) = Y YIELD RESULTS.
3840    Y = D + (FP1 + Z) * HALF
        X = D + Z + Q
        CALL SQRTDX (X, Z2, I, D, Y2, Y, X8, E5, E7)
3850    Y = D + (FP1 - Z) * HALF + D
        X = D - Z + D
        X = X + Q + X
        CALL SQRTDX (X, Z2, I, D, Y2, Y, X8, E5, E7)
3860    CALL NEWD (X, Z1, Q, Z, D)
        IF (D - Z2 .NE. W - Z2) GOTO 3700
3870    Y = (D - Z2) + (Z2 + (FP1 - Z) * HALF)
        X = (D - Z2) + (Z2 - Z + Q)
        CALL SQRTDX (X, Z2, I, D, Y2, Y, X8, E5, E7)
3880    Y = (FP1 + Z) * HALF
        X = Q
        CALL SQRTDX (X, Z2, I, D, Y2, Y, X8, E5, E7)
3890    IF (I .EQ. 0) GOTO 3700
3900    IF (E5 .LT. 0 .OR. E7 .GT. 0) GOTO 3920
3910    R4 = FP1
        WRITE(OUT,3911)
3911    FORMAT (' Square root appears to be correctly rounded.')
        RETURN
C
3920    IF (E7 + ULPPLS .GT. ULPPLS - HALF .OR.
     ~     E5          .GT. HALF          .OR.
     ~     E5 + RADIX  .LT. HALF)
     ~   GOTO 3940
3930    R4 = FP0
        WRITE(OUT,3931)
3931    FORMAT (' Square root appears to be chopped.')
        RETURN
3940    WRITE(OUT,3941)
        TEMP=E5-HALF
        TEMP2=HALF+E7
        WRITE(OUT,3942) TEMP, TEMP2
3941    FORMAT (' Square root is neither chopped nor correctly',
     ~          ' rounded.')
3942    FORMAT(' Observed errors run from  ',E15.7,'  to  ',E15.7,
     ~ ' ulps.')
        IF (E7 - E5 .LT. RADIX * RADIX) RETURN
3950    SDEFCT = SDEFCT + 1
        WRITE(OUT,3951)
3951    FORMAT(' SERIOUS DEFECT: sqrt gets too many last digits wrong.')
        RETURN
        END
C---
C       ____ SUBROUTINE TO ASSESS ERROR   SQRT(X*X) - X  IN ULPS. ____
C
        SUBROUTINE SQRERR (X, U, J, E5, E7, SEROUS)
C
        COMMON /STDIO/ IN, OUT
        INTEGER IN, OUT
        COMMON /GLOBAL/ FAILS, SDEFCT, DEFECT, FLAWS, RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
        INTEGER         FAILS, SDEFCT, DEFECT, FLAWS
        REAL                              RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
C
C        FAILS  ... NUMBER OF FAILURES
C        SDEFCT ... NUMBER OF SERIOUS DEFECTS
C        DEFECT ... NUMBER OF DEFECTS
C        FLAWS  ... NUMBER OF FLAWS
C
C        RADIX  ... COMPUTED RADIX OF THE MACHINE
C        ULPPLS ... ONE UNIT IN THE LAST PLACE (ULP) OF 1+EPSILON
C        ULPMIN ... ONE UNIT IN THE LAST PLACE (ULP) OF 1-EPSILON
C        PRECIS ... COMPUTED PRECISION OF THE MACHINE
C        W      ... RADIX ** PRECIS
C        MULGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN MULTIPLY
C        DIVGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN DIVIDE
C        SUBGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN ADD/SUBTRACT
C        A1     ...
C        ONEMIN ... ONE MINUS ULPMIN = ~.99999999999999
C
        INTEGER J
        REAL X, U,    E5, E7
        LOGICAL SEROUS
        REAL E6, B1
        B1 = 1.0 / RADIX
3090    E6 = (( SQRT(X * X) - X * B1) - (X - X * B1)) / U
        IF (E6 .EQ. 0.0) RETURN
3100    IF (E6 .LT. E5) E5 = E6
3110    IF (E6 .GT. E7) E7 = E6
3120    J = J + 1
        IF (.NOT. SEROUS) WRITE(OUT,31210) X*X, X, U*E6
        IF (SEROUS) WRITE(OUT,31211) X*X, X, U*E6
31210   FORMAT (' DEFECT: sqrt(', E15.7,') - ',E15.7,'  =  ', E15.7)
31211   FORMAT (' SERIOUS DEFECT: sqrt(', E15.7,') - ',E15.7,'  =  ',
     ~ E15.7)
        WRITE(OUT,3122)
3122    FORMAT(' instead of correct value  0 .')
        RETURN
        END
C---
C       THIS SUBROUTINE PUTS  NEWD = B*D  AND
C                             NEWZ^2 MOD NEWD = Z^2 MOD D
C
        SUBROUTINE NEWD (X, Z1, Q, Z, D)
C
        COMMON /GLOBAL/ FAILS, SDEFCT, DEFECT, FLAWS, RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
        INTEGER         FAILS, SDEFCT, DEFECT, FLAWS
        REAL                              RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
C
C        FAILS  ... NUMBER OF FAILURES
C        SDEFCT ... NUMBER OF SERIOUS DEFECTS
C        DEFECT ... NUMBER OF DEFECTS
C        FLAWS  ... NUMBER OF FLAWS
C
C        RADIX  ... COMPUTED RADIX OF THE MACHINE
C        ULPPLS ... ONE UNIT IN THE LAST PLACE (ULP) OF 1+EPSILON
C        ULPMIN ... ONE UNIT IN THE LAST PLACE (ULP) OF 1-EPSILON
C        PRECIS ... COMPUTED PRECISION OF THE MACHINE
C        W      ... RADIX ** PRECIS
C        MULGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN MULTIPLY
C        DIVGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN DIVIDE
C        SUBGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN ADD/SUBTRACT
C        A1     ...
C        ONEMIN ... ONE MINUS ULPMIN = ~.99999999999999
C
        COMMON /CONST/   FP0, FP1, FP2, FP3, FP4, FP8, FP9, FP27, FP32,
     ~                   HALF, MINUS1
        REAL FP0, FP1, FP2, FP3, FP4, FP8, FP9, FP27, FP32,
     ~                   HALF, MINUS1
C
        REAL X, Z1, Q, Z, D
        REAL TEMP, TEMP1
C
3720    X = Z1 * Q
        TEMP = HALF - X / RADIX
        TEMP1 = AINT(TEMP)
        IF (TEMP1 .GT. TEMP) TEMP1 = TEMP1 - FP1
        X = TEMP1 * RADIX + X
        Q = (Q - X*Z) / RADIX + X * X * (D / RADIX)
        Z = Z - FP2 * X * D
        IF (Z .GT. FP0) GOTO 3740
3730    Z = -Z
        Z1 = -Z1
3740    D = RADIX * D
        RETURN
        END
C---
C       THIS SUBROUTINE TESTS IF
C                SQRT(D*X) =  SQRT((Y - 1/2)^2 + X8/2) ROUNDS TO  Y
C
        SUBROUTINE SQRTDX (X, Z2, I, D, Y2, Y, X8, E5, E7)
C
        COMMON /GLOBAL/ FAILS, SDEFCT, DEFECT, FLAWS, RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
        INTEGER         FAILS, SDEFCT, DEFECT, FLAWS
        REAL                              RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
C
C        FAILS  ... NUMBER OF FAILURES
C        SDEFCT ... NUMBER OF SERIOUS DEFECTS
C        DEFECT ... NUMBER OF DEFECTS
C        FLAWS  ... NUMBER OF FLAWS
C
C        RADIX  ... COMPUTED RADIX OF THE MACHINE
C        ULPPLS ... ONE UNIT IN THE LAST PLACE (ULP) OF 1+EPSILON
C        ULPMIN ... ONE UNIT IN THE LAST PLACE (ULP) OF 1-EPSILON
C        PRECIS ... COMPUTED PRECISION OF THE MACHINE
C        W      ... RADIX ** PRECIS
C        MULGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN MULTIPLY
C        DIVGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN DIVIDE
C        SUBGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN ADD/SUBTRACT
C        A1     ...
C        ONEMIN ... ONE MINUS ULPMIN = ~.99999999999999
C
        COMMON /CONST/   FP0, FP1, FP2, FP3, FP4, FP8, FP9, FP27, FP32,
     ~                   HALF, MINUS1
        REAL FP0, FP1, FP2, FP3, FP4, FP8, FP9, FP27, FP32,
     ~                   HALF, MINUS1
C
        REAL X, Z2, X2, D, Y2, Y, X8, E5, E7
        INTEGER I
        REAL E6
C
3760    IF (X - RADIX .LT. Z2 - RADIX .OR. X - Z2 .GT. W - Z2) RETURN
3770    I = I + 1
        X2 =  SQRT(X * D)
        Y2 = (X2 - Z2) - (Y - Z2)
        X2 = X8/(Y - HALF)
        X2 = X2 - HALF * X2 * X2
        E6 = (Y2 + HALF) + (HALF - X2)
        IF (E6 .LT. E5) E5 = E6
3780    E6 = Y2 - X2
        IF (E6 .GT. E7) E7 = E6
3790    RETURN
        END
C---
        SUBROUTINE UNDERF(MILES, NUMTRY, FROM)
        INTEGER           MILES, NUMTRY, FROM
C        MILES  ... NUMBER OF MILESTONE REACHED SO FAR IN TESTING
C        NUMTRY ... NUMBER OF TIMES TO TRY RANDOM TRIALS
C        FROM   ... NUMBER OF MILESTONE TO RETURN TO ON RESTART
C
        COMMON /STDIO/ IN, OUT
        INTEGER IN, OUT
C
        COMMON /GLOBAL/ FAILS, SDEFCT, DEFECT, FLAWS, RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
        INTEGER         FAILS, SDEFCT, DEFECT, FLAWS
        REAL                              RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
C
C        FAILS  ... NUMBER OF FAILURES
C        SDEFCT ... NUMBER OF SERIOUS DEFECTS
C        DEFECT ... NUMBER OF DEFECTS
C        FLAWS  ... NUMBER OF FLAWS
C
C        RADIX  ... COMPUTED RADIX OF THE MACHINE
C        ULPPLS ... ONE UNIT IN THE LAST PLACE (ULP) OF 1+EPSILON
C        ULPMIN ... ONE UNIT IN THE LAST PLACE (ULP) OF 1-EPSILON
C        PRECIS ... COMPUTED PRECISION OF THE MACHINE
C        W      ... RADIX ** PRECIS
C        MULGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN MULTIPLY
C        DIVGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN DIVIDE
C        SUBGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN ADD/SUBTRACT
C        A1     ...
C        ONEMIN ... ONE MINUS ULPMIN = ~.99999999999999
C
        COMMON /CONST/   FP0, FP1, FP2, FP3, FP4, FP8, FP9, FP27, FP32,
     ~                   HALF, MINUS1
        REAL FP0, FP1, FP2, FP3, FP4, FP8, FP9, FP27, FP32,
     ~                   HALF, MINUS1
C
C
        COMMON /OVUNFL/  C1, H1, MINPOS, NULPS, UFLTHR, PHONY0
        REAL C1, H1, MINPOS, NULPS, UFLTHR, PHONY0
C        C1     ... 1/C ~= RADIX^LARGE_INTEGER
C        H1     ... MAX (2,RADIX)
C        MINPOS ... MINIMUM POSITIVE NUMBER FOUND BY MULT./DIVISION
C        NULPS  ... (SMALL INTEGER) * 1 UNIT IN LAST PLACE OF 1+ ... E9
C        UFLTHR ... THE UNDERFLOW THRESHOLD U0
C        PHONY0 ... CANDIDATE AND FINAL PHONY ZERO IF IT EXISTS. Z0
C
        COMMON /I3TYPE/ IEEE
        INTEGER         IEEE
C        IEEE   ... FLAG WHETHER GRADUAL UNDERFLOWS ARE DOUBLY ROUNDED
C
        INTEGER ACCUR, ERROR, I, IQ, PARTU
C        ACCUR  ... FLAG TO INDICATE SUCCESS/FAILURE OF ACCURACY TESTS
C        ERROR  ... COUNT OF ERRORS DETECTED TESTING POWERS.
C        I      ... SCRATCH FOR ENUMERATING CASES
C        IQ     ... TEMPORARY FOR HOLDING INTEGER EXPONENTS
C        PARTU  ... FLAG TO INDICATE THE DETECION OF PARTIAL UNDERFLOW
C
        REAL C, EPSP1, EXP2, H, MINDIF
C        C      ... 1/(RADIX^LARGE_INTEGER)
C        EPSP1  ... EPSILON + 1 (1 + (SMALL INTEGER)* 1 ULP OF 1+...)
C        EXP2   ... VALUE OF E ^ 2
C        H      ... MIN (1/RADIX, 1/2)
C        MINDIF ... MINIMUM POSITIVE NUMBER FOUND BY ADDITION/SUBTR.
C
C        ... LOCAL VARIABLES
        REAL D, Q, R, T0, V9, X, Y, Y1, Y2, Z, Z9,TEMP
        CHARACTER*8 CHARZ0, CHARE0
C        CHARZ0 ... CHARACTER CONSTANT 'Z0'
C        CHARE0 ... CHARACTER CONSTANT 'E0'
        DATA CHARZ0/ ' PHONY0'/,CHARE0/ ' MINPOS'/
C
        IF(FROM .EQ. 0 ) GO TO 4330
C          WE MUST BE DOING A RESTART.  FIGURE OUT WHERE, AND GO DO IT
C       MUST READ THE LOG FILE BACK IN.
        READ(3) ACCUR, C, EPSP1, ERROR, EXP2, H, I, IQ, MINDIF, PARTU
        READ(3) D, Q, R, T0, V9, X, Y, Y1, Y2, Z, Z9
        REWIND 3
        IF (FROM .EQ. 105) GO TO 4390
        IF (FROM .EQ. 106) GO TO 4410
        IF (FROM .EQ. 107) GO TO 4450
        IF (FROM .EQ. 108) PHONY0 = 0
        IF (FROM .EQ. 108) GO TO 4522
        IF (FROM .EQ. 109) GO TO 4860
        IF (FROM .EQ. 115) GO TO 4631
        IF (FROM .EQ. 120) GO TO 4890
        IF (FROM .EQ. 121) GO TO 4941
        IF (FROM .EQ. 122) GO TO 5011
        IF (FROM .EQ. 123) GO TO 5160
        IF (FROM .EQ. 124) GO TO 5190
        IF (FROM .EQ. 125) GO TO 5175
        IF (FROM .EQ. 131) GO TO 53021
C               MAKES NO SENSE TO TALK ABOUT UNDERFLOW STICKING, SINCE
C               UNDERFLOW ABORTS THE PROGRAM....
        CALL BADMIL
4330    WRITE(OUT,4335)
4335    FORMAT(' Seeking underflow threshold and min positive number:')
        MILES = 105
        CALL LOGIT(MILES)
4340    D = ULPMIN
        IF (PRECIS .EQ. AINT(PRECIS)) GOTO 4370
4350    D = FP1 / RADIX
        X = PRECIS
4360    D = D / RADIX
        X = X - FP1
        IF (X .GT. FP0) GO TO 4360
C       IF NON-INTEGRAL PRECISION NOW HAVE D = 1 RIGHT SHIFTED BY PRECIS
C       DIGITS (IN BASE "RADIX")
C       IF INTEGRAL PRECISION, ULPMIN IS THIS NUMBER - PRE-COMPUTED.
4370    Y = FP1
        Z = D
C       ... D = A POWER OF  1/RADIX < 1
4380    C=Y
        Y=Z
        WRITE(3) ACCUR, C, EPSP1, ERROR, EXP2, H, I, IQ, MINDIF, PARTU
        WRITE(3) D, Q, R, T0, V9, X, Y, Y1, Y2, Z, Z9
        REWIND 3
        CALL LOGIT(MILES)
        Z=Y*Y
        IF (Y .GT. Z .AND. Z+Z .GT. Z) GO TO 4380
C          MILESTONE 106
4390    MILES = 106
        CALL LOGIT(MILES)
        Y=C
        Z=Y*D
4400    C=Y
        Y=Z
        WRITE(3) ACCUR, C, EPSP1, ERROR, EXP2, H, I, IQ, MINDIF, PARTU
        WRITE(3) D, Q, R, T0, V9, X, Y, Y1, Y2, Z, Z9
        REWIND 3
        CALL LOGIT(MILES)
        Z=Y*D
        IF (Y .GT. Z .AND. Z+Z .GT. Z) GO TO 4400
C       MILESTONE 107
4410    MILES = 107
        CALL LOGIT(MILES)
        H1=RADIX
        IF (H1 .LT. FP2) H1=FP2
4420    H=FP1/H1
C        ... 1/H1 = H = MIN{ 1/RADIX, 1/2 }
4430    C1=FP1/C
        MINPOS=C
        Z=MINPOS*H
C       ... C = 1/RADIX^(BIG INTEGER) << 1 << C1 = 1/C
4440    Y=MINPOS
        MINPOS=Z
        WRITE(3) ACCUR, C, EPSP1, ERROR, EXP2, H, I, IQ, MINDIF, PARTU
        WRITE(3) D, Q, R, T0, V9, X, Y, Y1, Y2, Z, Z9
        REWIND 3
        CALL LOGIT(MILES)
        Z=MINPOS*H
        IF (MINPOS .GT. Z .AND. Z+Z .GT. Z) GO TO 4440
C       MILESTONE 108
4450    MILES = 108
        CALL LOGIT(MILES)
        UFLTHR = MINPOS
        MINDIF=FP0
        Q=FP0
        NULPS=ULPPLS
        EPSP1=FP1+NULPS
        D=C*EPSP1
        IF (D .GT. C) GO TO 4490
4460    NULPS=RADIX*ULPPLS
        EPSP1=FP1+NULPS
        D=C*EPSP1
        IF (D .GT. C) GO TO 4490
        WRITE(OUT,4470)
4470    FORMAT(' FAILURE: multiplication  gets too many last digits wron
     ~g.')
C       ... MULTIPLICATION IS TOO CRUDE
4480    FAILS=FAILS+1
        T0 = MINPOS
        Y1=FP0
        PHONY0 = Z
        CALL PAGE(MILES)
        GOTO 4570
4490    T0=D
        PHONY0=T0*H
        UFLTHR=FP0
4500    Y1=T0
        T0=PHONY0
        IF (MINDIF+MINDIF .GT. MINDIF) GO TO 4520
4510    Y2 = T0 * H1
        MINDIF= ABS(Y1-Y2)
        Q=Y1
        IF (UFLTHR .EQ. FP0 .AND. Y1 .NE. Y2) UFLTHR=Y1
        WRITE(3) ACCUR, C, EPSP1, ERROR, EXP2, H, I, IQ, MINDIF, PARTU
        WRITE(3) D, Q, R, T0, V9, X, Y, Y1, Y2, Z, Z9
        REWIND 3
        MILES = 108
        CALL LOGIT(MILES)
4520    PHONY0=T0*H
        IF (T0 .GT. PHONY0 .AND. PHONY0+PHONY0 .GT. PHONY0) GO TO 4500
4522    MILES = 109
        CALL LOGIT(MILES)
C        ... NOW  1 >> C=1/RADIX^(INTEGER)  >=   Y    >   MINPOS=Y*H
C                  >~ Z:=MINPOS*H >~ 0 ,
C        ... AND  1 >> D=(1+NULPS)*C >= UFLTHR >= Q >= Y1 > T0:=Y1*H
C                  >~ PHONY0:=T0*H >~ 0 ,
C        ... AND  UFLTHR = D/RADIX^INTEGER  IS FIRST TO VIOLATE
C                  (UFLTHR*H)/H=UFLTHR , ELSE  UFLTHR=0 ;
C        ... AND  Q:=UFLTHR/RADIX^INTEGER  IS FIRST WITH  MINDIF :=
C                  |(Q*H)/H - Q| > 0, ELSE Q=Y1.
4570    IF (PHONY0 .EQ. FP0) GO TO 4860
C        ... TEST  PHONY0  FOR 'PHONEY-ZERO' VIOLATING  PHONY0<T0 OR
C                  PHONY0<PHONY0+PHONY0  ...
        WRITE(OUT,4590)
4590    FORMAT(/)
        Z=PHONY0
        IF (PHONY0 .GT. FP0) GOTO 4620
4600    FAILS=FAILS+1
        WRITE(OUT,4601)
4601    FORMAT(' FAILURE:  positive expressions can underflow to an alle
     ~gedly')
        WRITE(OUT,4602)PHONY0
4602    FORMAT('          negative value z0 that prints out as ',
     ~E16.8)
        X=-PHONY0
        IF (X .GT. 0) GO TO 4630
        WRITE(OUT,4610)X
4610    FORMAT('          but  -z0, which should then be positive, isn''
     ~t; it prints out as', E16.8)
        GOTO 4630
4620    FLAWS=FLAWS+1
        WRITE(OUT,4623)
4623    FORMAT(' FLAW: underflow can stick at an allegedly positive valu
     ~e  z0')
        WRITE(OUT,4626)PHONY0
4626    FORMAT( '       that prints out as ', E16.8)
4630    MILES=115
C       PARTUF INCLUDES CALL LOGIT(MILES)
        WRITE(3) ACCUR, C, EPSP1, ERROR, EXP2, H, I, IQ, MINDIF, PARTU
        WRITE(3) D, Q, R, T0, V9, X, Y, Y1, Y2, Z, Z9
        REWIND 3
4631    CALL PARTUF (Z, CHARZ0, MILES, PARTU, FROM .EQ. 115)
C       ... END OF TEST FOR 'PHONEY-ZERO'.
4860    MILES=120
        WRITE(3) ACCUR, C, EPSP1, ERROR, EXP2, H, I, IQ, MINDIF, PARTU
        WRITE(3) D, Q, R, T0, V9, X, Y, Y1, Y2, Z, Z9
        REWIND 3
        CALL LOGIT(MILES)
C       ==============================================================
4870    IF (C1*Y .LE. C1*Y1) GO TO 4890
C       ... AS HAPPENS ON MOST MACHINES.
4880    EPSP1=H*EPSP1
        MINPOS=T0
C       = LEAST POSITIVE NO. ON HP 3000
4890    IF (MINDIF .EQ. 0 .OR. MINDIF .EQ. MINPOS) GO TO 4930
4900    IF (MINDIF .LT. MINPOS) GO TO 4920
4910    DEFECT=DEFECT+1
        WRITE(OUT,4912)
4912    FORMAT(' DEFECT: differences underflow at a higher threshold tha
     ~n products.')
        GOTO 4930
4920    DEFECT=DEFECT+1
        WRITE(OUT,4922)
4922    FORMAT(' DEFECT: products underflow at a higher threshold than d
     ~ifferences.')
        IF (PHONY0 .EQ. FP0) MINPOS=MINDIF
C       ... BUT NOT IF PSEUDO-ZEROS EXIST.
4930    WRITE(OUT,4935) MINPOS
4935    FORMAT(' Smallest strictly positive number found is  minpos  =',
     ~         1PE16.8)
4940    Z = MINPOS
        MILES = 121
        WRITE(3) ACCUR, C, EPSP1, ERROR, EXP2, H, I, IQ, MINDIF, PARTU
        WRITE(3) D, Q, R, T0, V9, X, Y, Y1, Y2, Z, Z9
        REWIND 3
4941    CALL PARTUF (Z, CHARE0, MILES, PARTU, FROM .EQ. 121)
        T0=MINPOS
        IF (PARTU .EQ. 1) T0=Y
C       FOR CDC 7600
4950    I=4
        IF (MINDIF .EQ. FP0) I=3
C       ...  I=1 IF MINDIF=0=UFLTHR  ,   I=2 IF MINDIF>0=UFLTHR  ,
4960    IF (UFLTHR .EQ. FP0) I=I-2
C           ...  I=3 IF MINDIF=0<UFLTHR  ,   I=4 IF MINDIF>0 & UFLTHR>0
4970    GOTO (4980, 5090, 5010, 5130),I
C       ... CASE STATEMENT
4980    UFLTHR=T0
        IF (C1*Q .EQ. (C1*Y)*EPSP1) GO TO 5010
4990    FAILS=FAILS+1
        UFLTHR=Y
        WRITE(OUT,4993)
4993    FORMAT(' FAILURE: either accuracy deteriorates as numbers approa
     ~ch a threshold')
        WRITE(OUT,4996)UFLTHR,C
4996    FORMAT(' of ',E16.8,' coming down from  ',E16.8,',')
        WRITE(OUT,4997)
4997    FORMAT(' or else  multiplication  gets too many last digits wron
     ~g.')
        CALL PAGE(MILES)
C
C        ___ TEST FOR  X-Z = 0  ALTHOUGH  X  .NE.  Z ___
C
5010    MILES = 122
        WRITE(3) ACCUR, C, EPSP1, ERROR, EXP2, H, I, IQ, MINDIF, PARTU
        WRITE(3) D, Q, R, T0, V9, X, Y, Y1, Y2, Z, Z9
        REWIND 3
        CALL LOGIT(MILES)
        R =  SQRT(T0 / UFLTHR)
        GO TO 5012
5011    R = FP1
5012    IF (R .GT. H) GOTO 5030
5020    Z=R*UFLTHR
        X=Z*(FP1+R*H*(FP1+H))
        GOTO 5040
5030    Z=UFLTHR
        X=Z*(FP1+H*H*(FP1+H))
5040    MILES = 123
        WRITE(3) ACCUR, C, EPSP1, ERROR, EXP2, H, I, IQ, MINDIF, PARTU
        WRITE(3) D, Q, R, T0, V9, X, Y, Y1, Y2, Z, Z9
        REWIND 3
        CALL LOGIT(MILES)
        IF (X .EQ. Z .OR. X-Z .NE. FP0) GO TO 5160
5050    FLAWS=FLAWS+1
        WRITE(OUT,5055)X,Z
5055    FORMAT(' FLAW:  x =',E16.8,' is unequal to  z =',E16.8,' ,')
        Z9 = X - Z
        WRITE(OUT,5057) Z9
5057    FORMAT(' yet  x-z  yields ', E15.7)
        WRITE(OUT,5060)
5060    FORMAT(' Should this not signal underflow, this is a SERIOUS',
     ~  /' DEFECT that causes confusion when innocent statements like')
        WRITE(OUT,5063)
5063    FORMAT(' if (x.eq.z) then ... else ... ( f(x)-f(z) )/(x-z) ...')
        WRITE(OUT,5070)+(X/Z-HALF)-HALF
5070    FORMAT(' encounter division by zero although actually  x/z = 1 +
     ~ ',E16.8)
        GO TO 5160
C       ... END OF TEST FOR  X-Z = 0  &  X  .NE.  Z
5090    CONTINUE
C        CASE I=2
C       UFLTHR = 0 < MINDIF  !
5100    FAILS=FAILS+1
        WRITE(OUT,5102)
5102    FORMAT(' FAILURE: underflow confuses comparison, which alleges t
     ~hat  q = y ')
        WRITE(OUT,5104)
5104    FORMAT('         while denying that  |q-y| = 0 ; these values pr
     ~int out as')
        TEMP= ABS(Q-Y2)
        WRITE(OUT,5106)Q,Y2,TEMP
5106    FORMAT(' q =',E16.8,',  y =',E16.8,',  |q-y| =',E16.8,' ,')
        TEMP = Q/Y2 - HALF
        WRITE(OUT,5110) TEMP - HALF
5110    FORMAT(' and  q/y = 1 + ',E16.8)
5120    UFLTHR=Q
        GOTO 5010
C        CASE I=4 ;  UFLTHR > 0  &  MINDIF > 0
5130    CONTINUE
5140    IF (.NOT. (Q .EQ. UFLTHR .AND. MINDIF .EQ. MINPOS .AND.
     ~    ABS(UFLTHR-MINDIF/NULPS) .LE. MINDIF)) GO TO 5010
        WRITE(OUT,5150)
        WRITE(OUT,5155)
5150    FORMAT(' Underflow is gradual; it incurs  absolute error = ')
5155    FORMAT(' (roundoff in underflow threshold) < minpos.')
        Y=MINPOS*C1
        Y=Y*(1.5E0+ULPPLS)
        X=C1*(FP1+ULPPLS)
        Y=Y/X
        IEEE=0
        IF (Y .EQ. MINPOS) IEEE=1
C       ... IEEE=1 UNLESS GRADUAL UNDERFLOWS ARE DOUBLY ROUNDED.)
5160    WRITE(OUT,5163)UFLTHR
5163    FORMAT(' The  underflow threshold is ',E16.8,' , below which')
        WRITE(OUT,5165)
5165    FORMAT(' calculation may suffer larger relative error than merel
     ~y roundoff.')
5170    MILES = 124
        WRITE(3) ACCUR, C, EPSP1, ERROR, EXP2, H, I, IQ, MINDIF, PARTU
        WRITE(3) D, Q, R, T0, V9, X, Y, Y1, Y2, Z, Z9
        REWIND 3
        CALL LOGIT(MILES)
        Y2=ULPMIN*ULPMIN
        Y=Y2*Y2
        MILES = 125
        WRITE(3) ACCUR, C, EPSP1, ERROR, EXP2, H, I, IQ, MINDIF, PARTU
        WRITE(3) D, Q, R, T0, V9, X, Y, Y1, Y2, Z, Z9
        REWIND 3
        CALL LOGIT(MILES)
        Y2=Y*ULPMIN
5175    IF (Y2 .GT. UFLTHR) GO TO 5220
5180    IF (Y .GT. MINPOS) GO TO 5200
5190    SDEFCT=SDEFCT+1
        I=4
        WRITE(OUT,5195)
5195    FORMAT(' SERIOUS ')
        GOTO 5210
5200    DEFECT=DEFECT+1
        I=5
5210    WRITE(OUT,5212)I
5212    FORMAT(' DEFECT:  range is too narrow;   ulpmin^',I5,
     ~  '  underflows.')
5220    MILES=130
        CALL PAGE(MILES)
C       ---- PAUSE ---- ==================================
5230    Y = -AINT(HALF - 240.0 * ALOG(UFLTHR) / ALOG(H1)) / 240
        Y2=Y+Y
        WRITE(OUT,5240)H1,Y
5240    FORMAT(' since underflow occurs below the threshold  ='/10X,'(',
     ~     1PE16.8,')^(',1PE16.8,') ,')
        WRITE(OUT,5245)H1,Y2
5245    FORMAT(' only underflow should afflict the expression'/10X,'(',
     ~1PE16.8,')^(',1PE16.8,') ;')
        WRITE(OUT,5247)
5247    FORMAT(' actually calculating it yields   ')
        MILES = 131
        CALL LOGIT(MILES)
5250    V9 = H1 ** (Y2)
        WRITE(OUT,5255) V9
5255    FORMAT(1X,E16.8)
        IF (V9 .GE. FP0 .AND. V9 .LE. (RADIX+RADIX*NULPS)*UFLTHR)
     ~GO TO 5270
5260    SDEFCT=SDEFCT+1
        WRITE(OUT,5263)
5263    FORMAT(' SERIOUS')
        GOTO 5300
5270    IF (V9 .GT. UFLTHR*(FP1+NULPS)) GO TO 5290
        WRITE(OUT,5280)
5280    FORMAT(' This computed value is O.K.')
        GOTO 5310
5290    DEFECT=DEFECT+1
5300    WRITE(OUT,5302)UFLTHR
5302    FORMAT(' DEFECT: this is not between 0 and  underflow threshold
     ~=',E16.8)
        GO TO 5310
53021   FLAWS = FLAWS + 1
        WRITE(OUT,53022)
53022   FORMAT(' FLAW: underflow trap from ** .')
5310    MILES=140
C       ======================================================
C       CALCULATE  EXP2 = EXP(2) = 7.389056099...
5330    X=FP0
        I=2
        Y=FP2*FP3
        Q=FP0
        ACCUR=0
5340    Z=X
        I=I+1
        Y=Y/(I+I)
        R=Y+Q
        X=Z+R
        Q=(Z-X)+R
        IF (X .GT. Z) GO TO 5340
5350    Z=(1.5E0+FP1/FP8)+X/(1.5E0 * FP32)
        X=Z*Z
        EXP2=X*X
        X=ONEMIN
        Y=X-ULPMIN
        WRITE(OUT,5360) EXP2
5360    FORMAT(' Testing  x^((x+1)/(x-1)) vs. exp(2) = ',E16.8,'  as  x
     ~-> 1.')
5370    DO 5415 I=1 , NUMTRY
5380    Z=X-(1/RADIX)
        Z=(X+FP1)/(Z-(FP1-(1/RADIX)))
        Q=X**Z-EXP2
        IF ( ABS(Q) .GT. 240. * ULPPLS) GO TO 5420
5390    Z=(Y-X)*FP2+Y
        X=Y
        Y=Z
        Z = FP1+(X-ONEMIN)*(X-ONEMIN)
        IF (Z .LE. FP1) GOTO 5400
5415    CONTINUE
5400    IF (X .GT. FP1) GO TO 5440
5410    X=FP1+ULPPLS
        Y=ULPPLS+ULPPLS+X
        GOTO 5370
5420    ACCUR=1
        DEFECT=DEFECT+1
        TEMP=+(X-(1/RADIX))-(FP1-(1/RADIX))
        WRITE(OUT,5425)TEMP,Z
5425    FORMAT(' DEFECT:  calculated  (1 + (',E16.8,'))^(',E16.8,')')
        WRITE(OUT,5427)Q
5427    FORMAT('         differs from correct value by  ',E16.8)
        WRITE(OUT,5430)
5430    FORMAT(' This much error may spoil financial calculations involv
     ~ing tiny interest rates.')
        GOTO 5450
5440    IF (ACCUR .EQ. 0) WRITE(OUT,5445)
5445    FORMAT(' Accuracy seems adequate.')
5450    MILES=150
C       =======================================================
        WRITE(OUT,5460)
5460    FORMAT(' Testing powers  z^q  at four nearly extreme values:')
        ERROR=0
        Z=A1
        IQ =  INT(HALF-ALOG(C) / ALOG(A1))
5470    X=C1
        CALL CMPXY(X,Y,Z,IQ,ERROR)
        IQ=-IQ
        X=C
        CALL CMPXY(X,Y,Z,IQ,ERROR)
        IF (Z .LT. FP1) GO TO 5490
5480    Z=1/A1
        GOTO 5470
5490    CALL PRTCNT (ERROR)
        CALL PRT2(ERROR,MILES)
C       ... PRINT COUNT OF DISCREPANCIES.
5500    MILES=160
        RETURN
        END
C---
        SUBROUTINE ZEROS(MILES,FROM)
        INTEGER MILES
C
        COMMON /STDIO/ IN, OUT
        INTEGER IN, OUT
        COMMON /GLOBAL/ FAILS, SDEFCT, DEFECT, FLAWS, RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
        INTEGER         FAILS, SDEFCT, DEFECT, FLAWS
        REAL                              RADIX, ULPPLS,
     ~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
C
C        FAILS  ... NUMBER OF FAILURES
C        SDEFCT ... NUMBER OF SERIOUS DEFECTS
C        DEFECT ... NUMBER OF DEFECTS
C        FLAWS  ... NUMBER OF FLAWS
C
C        RADIX  ... COMPUTED RADIX OF THE MACHINE
C        ULPPLS ... ONE UNIT IN THE LAST PLACE (ULP) OF 1+EPSILON
C        ULPMIN ... ONE UNIT IN THE LAST PLACE (ULP) OF 1-EPSILON
C        PRECIS ... COMPUTED PRECISION OF THE MACHINE
C        W      ... RADIX ** PRECIS
C        MULGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN MULTIPLY
C        DIVGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN DIVIDE
C        SUBGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN ADD/SUBTRACT
C        A1     ...
C        ONEMIN ... ONE MINUS ULPMIN = ~.99999999999999
C
        COMMON /CONST/   FP0, FP1, FP2, FP3, FP4, FP8, FP9, FP27, FP32,
     ~                   HALF, MINUS1
        REAL FP0, FP1, FP2, FP3, FP4, FP8, FP9, FP27, FP32,
     ~                   HALF, MINUS1
C
C       MILESTONE REACHED SO FAR
        INTEGER FROM
C       MILESTONE TO RESTART AT
        REAL Q9
C       TEMPORARY TO THROW RANDOM STUFF INTO.
        IF(FROM .EQ. 0) GO TO 6110
C       MUST BE DOING A RESTART.  FIGURE OUT WHERE, AND GO DO IT.
C       DON'T NEED A LOG FILE FOR THIS ROUTINE.
        IF (FROM .EQ. 211) GO TO 7000
        IF (FROM .EQ. 212) GO TO 6130
        CALL BADMIL
6110    WRITE(OUT,6120)
6120    FORMAT (/' What messages and/or values does',
     ~          ' division by zero produce?')
        WRITE(OUT,6123)
6123    FORMAT(' About to compute 1/0...')
        MILES = 211
        CALL LOGIT(MILES)
        Q9 = FP1 / FP0
        WRITE(OUT,6121) Q9
6121    FORMAT(' Trying to compute  1/0  produces ', 1PE15.7)
7000    MILES = 212
        WRITE(OUT,6124)
6124    FORMAT(' About to compute 0/0...')
        CALL LOGIT(MILES)
        Q9 = FP0 / FP0
        WRITE(OUT,6122) Q9
6122    FORMAT (' Trying to compute  0/0  produces ', 1PE15.7/)
6130    MILES = 220
        RETURN
        END
      SUBROUTINE BADMIL
      COMMON /STDIO/ IN, OUT
      INTEGER IN, OUT
      WRITE(OUT,11110)
11110 FORMAT(' Unrecognized restart milestone - PLEASE NOTIFY ',
     ~   'KARPINSKI !')
      STOP
      END
