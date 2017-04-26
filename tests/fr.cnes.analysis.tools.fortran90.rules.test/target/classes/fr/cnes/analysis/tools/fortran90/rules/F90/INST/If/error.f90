!
! Contre exemple : regle 'F90_Tr.IFLogique'
! 
! Le non respect de la regle vient du fait que dans l'instruction comportant 
! le IF logique, une affectation de valeur a une variable est effectuee .
! La regle stipule, que dans un IF logique il ne peut y avoir qu'une 
! instruction de branchement c'est a dire EXIT,CYCLE, GOTO ou RETURN
!  
program ESSAI

  implicit none

  integer :: i
  integer :: j
  integer :: n
  integer, dimension (10) :: buffer

! initialisation des scalaires
  i = 1
  j = -1
  n = 10

! initialisation du tableau (de tous ses elements)
  buffer = 0

! boucle de lecture sur le clavier : chaque mot saisi est stocke dans un tableau
  do while ((i <= n) .and. (j .ne. 0)) 

! lecture sur l'entree standard
     write(*,*) 'Entrer un entier non nul, sinon 0 pour terminer'
     read(*,*)  j 

! on stocke le mot lu (si different de 0) dans un tableau a l'indice courant
     if (j .ne. 0) buffer(i) = j
     i = i + 1

  enddo

! affichage des resultats obtenus sur sortie standard
  print*, '-- Tableau buffer --', buffer

end program ESSAI
