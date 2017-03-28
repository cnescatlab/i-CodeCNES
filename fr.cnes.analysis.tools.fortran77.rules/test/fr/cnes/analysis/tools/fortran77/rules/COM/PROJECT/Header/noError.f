
! noError.f
! We start with a comment and a blank line to verify if it is not read.     
! program test
PROGRAM TEST

INTEGER A,B,C
 A = 15
 B = 20
 C = 10

 F(A,B)
 
END

! This function also has his own comment.
! function f
 FUNCTION F(A,B)
   INTEGER A,B
   INTEGER F
 
   F = A + B
 END FUNCTION   