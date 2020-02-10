PROGRAM ESSAI
!
! --- Ce qu'il ne faut pas faire : definir des variables qui portent
! --- le meme nom que des mots cles FORTRAN
! --- De meme ne pas creer des fonction internes qui portent le meme nom 
! --- que des fonctions intrinseques 
!
! --- Les mots cles F90 : 
!     allocatable, allocate, assign, backspace, block data, call, 
!     case, close, common, contains, continue, cycle, data, deallocate, 
!     dimension, do, else if, else, elsewhere, end, endfile, endif, 
!     entry, equivalence, exit, external, format, function, goto, 
!     if, implicit, include, inquire, intent, interface, intrinsic, 
!     module, namelist, nullify, only, open, operator, optional, 
!     parameter, pause, pointer, print, private, procedure, program, 
!     public, read, recursive, result, return, rewind, rewrite, save, 
!     select, sequence, stop, subroutine, target, then, use, where, 
!     while, write.
!
! --- Les fonctions intrinseques F90 : 
! 
!     ABS, ACHAR, ACOS, ADJUSTL, ADJUSTR, AIMAG, AINT, ALL, ALLOCATED,
!     ANINT, ANY, ASIN, ASSOCIATED, ATAN, ATAN2, BIT_SIZE, BTEST, CEILING,
!     CHAR, CMPLX, CONJG, COS, COSH, COUNT, CSHIFT, DBLE, DIGITS, DIM, DPROD,
!     EOSHIFT, EPSILON, EXP, EXPONENT, FLOOR, FRACTION, HUGE, IACHAR, IAND, 
!     IBCLR.  IBITS, IBSET, ICHAR, IEOR, INDEX, INT, INTENT, IOR, ISHFT, 
!     ISHFTC, KIND, LBOUND, LBOUND, LEN, LEN_TRIM, LGE, LGT, LLE, LLT, LOG,
!     LOG10, LOGICAL, MAX, MAXEXPONENT, MAXLOC, MAXVAL, MERGE, MIN, MINEXPONENT,
!     MINLOC, MINVAL, MOD, MODULO, MVBITS, NEAREST, NINT, NOT, PACK, PRECISION,
!     PRESENT, PRODUCT, RADIX, RANGE, REAL, REPEAT, RESHAPE, RRSPACING, SCALE,
!     SCAN, SELECTED_INT_KIND, SELECTED_REAL_KIND, SET_EXPONENT, SHAPE, SIGN,
!     SIN, SINH, SIZE, SPACING, SPREAD, SQRT, SUM, TAN, TANH, TINY, TRANSFER,
!     TRANSPOSE, TRIM, UBOUND, UNPACK, VERIFY
! 
! --- Les subroutines intrinseques F90 :
!     DATE_AND_TIME, MVBITS, RANDOM_NUMBER, RANDOM_SEED, SYSTEM_CLOCK
! 

      IMPLICIT NONE

      INTEGER            :: READ
! !!! Regle 'Id.MotsCles' pas respectee ci-dessus, la variable porte le ^meme nom qu'un mot cle

      REAL               :: WRITE
! !!! Regle 'Id.MotsCles' pas respectee ci-dessus, la variable porte le ^meme nom qu'un mot cle

      COMPLEX            :: INTEGER
! !!! Regle 'Id.MotsCles' pas respectee ci-dessus, la variable porte le ^meme nom qu'un mot cle

      DOUBLE PRECISION   :: GOTO
! !!! Regle 'Id.MotsCles' pas respectee ci-dessus, la variable porte le ^meme nom qu'un mot cle

      REAL               :: x = 3.14159
      REAL               :: sin

      CHARACTER *32      :: SUBROUTINE
! !!! Regle 'Id.MotsCles' pas respectee ci-dessus, la variable porte le ^meme nom qu'un mot cle

      DATA SUBROUTINE / 'Ceci est une chaine' /

      READ = 1
      WRITE(*,*) 'READ=', READ

      WRITE=2.0
      WRITE(*,*) 'WRITE=', WRITE

      INTEGER = (1.0, 2.0)
      WRITE(*,*) 'INTEGER=', INTEGER

      GOTO = 3.0D0
      WRITE(*,*) 'GOTO=', GOTO

      WRITE(*,*) 'SUBROUTINE=', SUBROUTINE

      WRITE(*,*) 'Appel fonction interne sin(x), avec x=', x, ' -> resultat obtenu = ', sin(x) 





END PROGRAM ESSAI

!
! Fonction interne 'sin' qui porte le meme nom que la fonction intrinseque 'SIN'
!    Nota : FORTRAN ne fait pas de distingo sur la casse des lettres
! 
REAL FUNCTION sin (X)
      REAL x
      sin = x - (x**3)/6.0
      RETURN
END FUNCTION sin 
