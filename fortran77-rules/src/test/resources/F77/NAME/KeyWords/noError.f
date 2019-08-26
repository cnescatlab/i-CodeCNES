      PROGRAM ESSAI

C
C --- Ce qu'il faut faire : definir des variables qui ne portent
C --- pas le meme nom que des mots cles FORTRAN
C
C --- Les mots cles F77 : 
C        assign, backspace, block data, call, close, common, continue, 
C        data, dimension, do, else, else if, end, endfile, endif, entry, 
C        equivalence, external, format, function, goto, if, implicit, 
C        inquire, intrinsic, open, parameter, pause, print, program, 
C        read, return, rewind, rewrite, save, stop, subroutine, then, write.
C
      INTEGER I 
      REAL    R
      COMPLEX C
      DOUBLE PRECISION DP 
      CHARACTER *32  MY_STRING

      DATA MY_STRING / 'Ceci est une chaine' /

      I = 1
      WRITE(*,*) 'I=', I

      R=2.0
      WRITE(*,*) 'R=', R

      C = (1.0, 2.0)
      WRITE(*,*) 'C=', C

      DP = 3.0D0
      WRITE(*,*) 'DP=', DP

      WRITE(*,*) 'MY_STRING=', MY_STRING

      END PROGRAM ESSAI
