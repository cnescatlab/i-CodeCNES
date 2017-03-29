MODULE Tableau_Dynamique
!---------------------------------------------------------------------
!
!  Ce module contient les definitions permettant d'allouer dynamiquement 
!  un tableau de n elements dans lequel on va stocker des donnees 
!
!---------------------------------------------------------------------
   INTEGER :: n
   REAL, DIMENSION(:), ALLOCATABLE :: Tableau
END MODULE Tableau_Dynamique

PROGRAM ESSAI
!---------------------------------------------------------------------
!
!  Le programme ci-dessous, appelle une subroutine qui va lire au clavier 
!  des valeurs qui seront stockees dans un tableau qui aura ete alloue dynamiquement .
!  Puis, plueiurs fonctions intrinseques F90 sont appelees pour effectuer des 
!  calculs sur les donnees qui ont ete saisies 
!
!---------------------------------------------------------------------
USE Tableau_Dynamique
IMPLICIT NONE

INTERFACE 
   SUBROUTINE Lire_Donnees
   END SUBROUTINE Lire_Donnees
END INTERFACE

INTERFACE 
   SUBROUTINE Desallouer_Tableau
   END SUBROUTINE Desallouer_Tableau
END INTERFACE

! Declarer les variables locales 
REAL :: Produit_Tableau
REAL :: Somme_Tableau
REAL :: Plus_petit
REAL :: Plus_Grand 

! Appel de la subroutine, qui alloue le tableau, et le rempli avec les donnees saisies
CALL Lire_Tableau 

! Appel de la fonction intrinseque MINVAL afin de determiner la valeur minimale des elements du tableau 
plus_petit = MINVAL(Tableau)
write(*,100) "Plus petite valeur du tableau", Plus_Petit 

! Appel de la fonction intrinseque MAXVAL afin de determiner la valeur maximale des elements du tableau 
plus_grand = MAXVAL(Tableau)
write(*,100) "Plus grande valeur du tableau", Plus_Grand

! On utilise la fonction intrinseque PRODUCT pour obtenir le resultat de la multiplication 
!    de tous les elements du tableau
Produit_Tableau = PRODUCT(Tableau)

! Impression sur sortie standard du resultat obtenu 
write(*,100) "Le produit des elements du tableau est ", Produit_Tableau

! Idem, appel fonction intrinseque SUM pour calcul de la somme des elements du tableau
Somme_Tableau = SUM(Tableau)

! Impression sur sortie standard du resultat obtenu
write(*,100) "La somme des elements du tableau est ", Somme_Tableau

! Desallouer le tableau et la memoire associee
CALL Desallouer_Tableau

! 
! --- F O R M A T S ---
!
100 format (A, 2x, F11.2)

END PROGRAM ESSAI

!-----Lire_Tableau----------------------------------------------------
!
!  Subroutine qui permet de lire le nombre d'elements du tableau, 
!  qui alloue la memoire permettant d'heberger les valeurs du tableau, 
!  et qui stocke les valeurs lues au clavier dans le tableau
!
!---------------------------------------------------------------------
SUBROUTINE Lire_Tableau
USE Tableau_Dynamique

IMPLICIT NONE

! variables locales
INTEGER :: AllocateStatus

! Lire le nombre d'elements du tableau
write(*,'(A)', ADVANCE = "NO") "Saisir le nombre d'elements du tableau:  "
read(*,*) n

! Allouer la memoire permettant de stocker les valeurs du tableau
ALLOCATE( Tableau(n), STAT = AllocateStatus)
IF (AllocateStatus /= 0) STOP "*** Pas assez de memoire ***"

! Lecture des valeurs du tableau
write(*, '(A)', ADVANCE = "NO") "Saisir les valeurs: "
read(*,*) Tableau 

END SUBROUTINE Lire_Tableau

!-----Desallouer_Tableau----------------------------------------------
!
!  Subroutine qui permet de desallouer la zone memoire associee au tableau
!
!---------------------------------------------------------------------
SUBROUTINE Desallouer_Tableau
USE Tableau_Dynamique

IMPLICIT NONE

! Variables locales 
INTEGER :: DeAllocateStatus

! Desallouer la memoire associee au tableau
DEALLOCATE( Tableau, STAT = DeAllocateStatus)
IF (DeAllocateStatus /= 0) STOP "*** Erreur de liberation memoire ***"

END SUBROUTINE Desallouer_Tableau
