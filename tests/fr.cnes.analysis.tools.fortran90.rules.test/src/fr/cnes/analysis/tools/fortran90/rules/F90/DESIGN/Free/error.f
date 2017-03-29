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
!  Le programme ci-dessous, effectue l'allocation dynamique d'un tableau a une 
!  dimension dont la taille a ete specifiee par l'operateur, puis les valeurs 
!  qui seront stockees dans ce tableau sont egalement saisies .
!  Ensuite, plueiurs fonctions intrinseques F90 sont appelees pour effectuer des 
!  calculs sur les donnees de ce tableau.
!  Enfin, une subroutine est appelee pour liberer la memoire associee au tableau
!
!  Non respect de la regle : car l'allocation et la liberation s'effectuent sur 
!  des niveaux dans l'arbre d'appel differents (prog principal, subroutine)
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
INTEGER :: AllocateStatus
REAL :: Produit_Tableau
REAL :: Somme_Tableau
REAL :: Plus_petit
REAL :: Plus_Grand 

! Lire le nombre d'elements du tableau
write(*,'(A)', ADVANCE = "NO") "Saisir le nombre d'elements du tableau:  "
read(*,*) n

! Allouer la memoire permettant de stocker les valeurs du tableau
ALLOCATE( Tableau(n), STAT = AllocateStatus)
IF (AllocateStatus /= 0) STOP "*** Pas assez de memoire ***"

! Lecture des valeurs du tableau
write(*, '(A)', ADVANCE = "NO") "Saisir les valeurs: "
read(*,*) Tableau

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
