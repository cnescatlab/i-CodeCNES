PROGRAM ESSAI

!
! --- Ce qu'il faut faire : definir des variables qui ne portent
! --- pas le meme nom que des mots cles FORTRAN
! --- De meme, ne pas creer des fonctions ou routines qui portent 
! --- le meme nom que des fonctions intrinseques (voir liste ci-dessous) 
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

      INTEGER            :: i
      REAL               :: r
      REAL               :: x = 1.5
      REAL               :: mon_sin
      COMPLEX            :: c
      DOUBLE PRECISION   :: dp 
      CHARACTER *32      :: ma_chaine

      DATA ma_chaine     / 'Ceci est une chaine' /

      i = 1
      WRITE(*,*) 'i=', i

      r=2.0
      WRITE(*,*) 'r=', r

      c = (1.0, 2.0)
      WRITE(*,*) 'c=', c

      dp = 3.0D0
      WRITE(*,*) 'dp=', dp

      WRITE(*,*) 'ma_chaine=', ma_chaine

      WRITE(*,*) 'Appel fonction interne mon_sin(x), avec x=', x, ' -> resultat obtenu = ', mon_sin(x) 

END PROGRAM ESSAI

!
! Fonction interne 
!
REAL FUNCTION mon_sin (X)
      REAL x
      mon_sin = x - (x**3)/6.0
      RETURN
END FUNCTION mon_sin 
