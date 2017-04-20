Subroutine somme(a)
implicit none
real ::a(100)
real total
integer j

total=0
print*, "dans somme"
print*,"a(1), a(2), a(3), a(4)",a(1), a(2), a(3), a(4)
do j=1,100
   !on sort du tableau passe en parametre, le programme continue
   !son execution
   total=total+a(j)
end do
print*,"total", total

END Subroutine somme

Subroutine somme2(a)
implicit none
real, dimension(100) :: a
real total
integer j

total=0
print*, "dans somme"
print*,"a(1), a(2), a(3), a(4)",a(1), a(2), a(3), a(4)
do j=1,100
   !on sort du tableau passe en parametre, le programme continue
   !son execution
   total=total+a(j)
end do
print*,"total", total

END Subroutine somme2

PROGRAM test
implicit none
REAL, DIMENSION(4)::P

P(1)=1
P(2)=2
P(3)=2
print*,"P(1), P(2), P(3)", P(1),P(2), P(3)

! En ne passant pas la dimension du tableau en meme temps que celui-ci lors de
! l'appel a la procedure somme, cela genere des resultats erratiques

call somme(P(1:3))


END PROGRAM test
