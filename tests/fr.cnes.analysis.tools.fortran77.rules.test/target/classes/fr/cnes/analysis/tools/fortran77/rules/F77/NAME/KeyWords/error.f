      PROGRAM ESSAI
C
C --- Ce qu'il ne faut pas faire : definir des variables qui portent
C --- le meme nom que des mots cles FORTRAN
C
C --- Les mots cles F77 :
C        assign, backspace, block data, call, close, common, continue,
C        data, dimension, do, else, else if, end, endfile, endif, entry,
C        equivalence, external, format, function, goto, if, implicit,
C        inquire, intrinsic, open, parameter, pause, print, program,
C        read, return, rewind, rewrite, save, stop, subroutine, then, write.
C
      INTEGER READ
      REAL    WRITE
      COMPLEX INTEGER
      DOUBLE PRECISION GOTO
      CHARACTER *32  THEN

      DATA THEN / 'Ceci est une chaine' /

      READ = 1
      WRITE(*,*) 'READ=', READ

      WRITE=2.0
      WRITE(*,*) 'WRITE=', WRITE

      INTEGER = (1.0, 2.0)
      WRITE(*,*) 'INTEGER=', INTEGER

      GOTO = 3.0D0
      WRITE(*,*) 'GOTO=', GOTO

      WRITE(*,*) 'THEN=', SUBROUTINE

      END PROGRAM ESSAI
