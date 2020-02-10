program ESSAI

  implicit none

  INTEGER, parameter :: NbDim = 3
  INTEGER, parameter :: NbPoints = 5

  DOUBLE PRECISION, dimension(NbDim, NbPoints) :: Points
  DOUBLE PRECISION, dimension(NbDim)           :: Deplacement

! initialisation des tableaux 
  data Deplacement / 0.5, -2.5, 3.0 /
  Points = reshape((/ 0., 1., 2., 3., 4., 5., 6., 7., 8., 9., 10., 11., 12., 13., 14. /), shape(Points))

! impression sur sortie standard du contenu des tableaux
  print*, '-- tableau deplacement --', Deplacement
  print*, '-- tableau de points  --', Points

! traitement : on applique un deplacement sur les coordonnees des points 
  call changer_coordonnees(Deplacement, NbPoints, Points) 

! impression sur sortie standard du contenu des tableaux
  print*, '-- tableau de points (apres) --', Points
 
end program ESSAI

! 
! Cette routine effectue une modification de coordonnees sur un tableau de points 
!    en appliquant un deplacement  sur les 3 axes x, y et z
!
! remarque : En absence d'instruction IMPLICIT NONE, les variables c et l n'etant pas declarees explicitement, 
! le compilateur leur affecte par defaut les types respectifs : 
!    - c : de type reel  (ce qui genere des warning a la compilation car les indices d'un tableau doivent etre des entiers)
!    - l : de type entier 
!
! => par chance, pas d'impact visible sur les resultats produits par le programme executable 
!
subroutine changer_coordonnees(Deplacement, NbPoints, Points)

   INTEGER, parameter :: NbDim = 3
   INTEGER, intent(in) :: NbPoints

   DOUBLE PRECISION, intent(inout), dimension(NbDim,NbPoints) :: Points
   DOUBLE PRECISION, intent(in), dimension(NbDim) :: Deplacement

! On applique a chaque point une valeur de deplacement selon les 3 axes 
   do c=1, NbDim, 1
      do l=1, NbPoints, 1 
         Points(c, l) = Points(c, l) + Deplacement(c)
      end do 
   end do 

end subroutine 

