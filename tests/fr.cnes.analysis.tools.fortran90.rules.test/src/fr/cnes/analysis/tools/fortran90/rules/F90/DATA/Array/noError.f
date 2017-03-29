Subroutine somme(a,la)
implicit none
integer la
real ::a(la)
real total
integer j

total=0
print*, "dans somme"
do j=1,la
   total=total+a(j)
end do
print*,"total", total

END Subroutine somme



PROGRAM test
implicit none
REAL, DIMENSION(4)::P

P(1)=1
P(2)=2
P(3)=2
print*,"P(1), P(2), P(3)", P(1),P(2), P(3)

!l'appel de la procedure somme se fait en passant le tableau P et sa dimension
call somme(P(1:3),size(P(1:3)))


END PROGRAM test
