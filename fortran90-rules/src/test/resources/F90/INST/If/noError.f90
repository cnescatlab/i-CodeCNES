program ESSAI

  implicit none

  integer :: i
  integer :: j
  integer :: n
  integer, dimension (10) :: buffer

! initialisation des scalaires
  n = 10

! initialisation du tableau (de tous ses elements)
  buffer = 0

! boucle de lecture sur le clavier : chaque mot saisi est stocke dans un tableau
  iloop: do i=1,n

! lecture sur l'entree standard
     write(*,*) 'Entrer un entier positif, sinon 0 pour terminer'
     read(*,*)  j 

! on sort de la boucle si i vaut 0
     if (j == 0) exit iloop

! on stocke le mot lu dans un tableau a l'indice suivant 
     buffer(i) = j

  end do iloop

! resultats obtenus sur ortie standard
  print*, '-- Tableau buffer --', buffer

end program ESSAI
