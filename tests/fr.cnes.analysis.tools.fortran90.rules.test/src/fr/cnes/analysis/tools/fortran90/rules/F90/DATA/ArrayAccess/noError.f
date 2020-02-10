program ESSAI

  implicit none

  integer, dimension(4) :: u

! initialisation du contenu du tableau u en direct (pas de tableau d'index)
  u = (/ 1, 2, 3, 4 /)

! resultats obtenus sur ortie standard
  print*, '-- Tableau u --', u

end program ESSAI
