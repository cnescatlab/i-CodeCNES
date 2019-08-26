program ESSAI

      IMPLICIT NONE

      INTEGER :: i
      INTEGER :: j
      INTEGER :: k
      INTEGER total (3,2)
      INTEGER sum (6)
      EQUIVALENCE (sum, total)

      DO i = 1, 6, 1
         sum(i) = I**2
      END DO

      WRITE (*, 9001) 'i', 'j', 'k', 'sum(k)', 'total(i,j)'
      DO j = 1, 2, 1
         DO i = 1, 3, 1
            k = (j-1) * 3 + i
            WRITE (*, 9002) i, j, k, sum(k), total(i,j)
         END DO
      END DO 

!
! --- F O R M A T S 
!
9001  FORMAT (A5,1X,A5,1X,A5,1X,A16,1X,A16)
9002  FORMAT (I5,1X,I5,1X,I5,11X,I5,11X,I5)

      STOP

END program ESSAI
