
! 
! --- MAUVAIS EXEMPLE ---
! 

! ***********************************************************************
! Cette unite de programme effectue la mise en oeuvre de pointeur .
! Cependant cette mise en oeuvre est incorrecte car l'usage de pointeurs doit etre 
! effectue dans les cas suivants : 
! - Arbre binaire
! - Partie de tableau
! - Tableau alloue par un sous-programme utilise dans le programme appelant
! - Manipulation de structures de donnes complexes 
! Dans l'exemple ci-dessous on utilise les pointeurs pour : 
! - referencer une valeur reelle (le nombre pi initialement) 
! - adresser un tableau de reels
! - mettre en oeuvre un tableau de pointeurs  permet de gerer un nombre de 
!   chaines de caracteres allouees dynamiquement, dont le nombre n'est pas 
!   connu lors du lancement du programme. 
! ***********************************************************************
program ESSAI

      implicit none

      integer, target :: i
      integer         :: j 
      integer         :: k
      integer         :: num
      real, target :: pi, b(100), c(10,10)

! --- Declaration des pointeurs 
      real, pointer :: ppi
      real, pointer :: aptr

! --- Declaration du tableau de pointeurs 
      type pp
        character(len=80), dimension(:), pointer :: p
      end type pp
      type(pp), dimension(:), allocatable :: array

! --- Initalisation des pointeurs
      nullify (ppi)
      nullify (aptr)

! --- On affecte a pa, l'adresse de a 
      pi = 3.141592654
      ppi => pi
      print *, 'pi = ', pi, 'ppi = ', ppi

! --- On initialise le tableau c
      c = reshape((/(K*1.01,K=1,100)/), shape(c))
      print *, 'c = ', c

! --- Il devient possible d'utiliser la valeur pointee par ppi
      do j=1,10
         do k=1,10
            i=(j-1)*10+k
            b(i) = ppi*c(j,k)
         end do
      end do
      print *, 'b = ', b

! --- Cela revient au meme d'effectuer ce qui suit 
      b(i) = pi*c(i,i)
      print *, 'b = ', b

! --- On change la valeur de Pi en referencant le pointeur mais la variable initiale pi est affectee 
! --- Car ils pointent tous les deux au meme endroit en memoire 
      ppi = 1.23456
      print *, 'pi = ', pi, 'ppi = ', ppi

! --- Ci-dessous on associe le pointeur a une autre variable 
      ppi => b(1)
      print *, 'b(1) = ', b(1), 'ppi = ', ppi

! --- Ici, le pointeur est desassocie de toute variable au moyen de l'instruction NULLIFY 
      nullify (ppi)

! --- Ci-dessous, on peut demander au systeme si le pointeur est associe a une variable : pour cela on utilise la fonction intrinseque ASSOCIATED 
      if (associated(ppi)) then
         print *, 'ppi est actuellement associe'
      else
         print *, 'ppi n''est pas actuellement associe'
      endif

! --- Afin d'obtenir plus de details, on peut utiliser le parametre optionnel TARGET afin de determiner 
! ---    si le pointeur est associe a une certaine variable (ou non) 
      if (associated(ppi, target=pi)) then
         print *, 'ppi est actuellement associe avec la variable "pi"'
      else 
         print *, 'ppi n''est pas actuellement associe avec la variable "pi"'
      endif

! --- Il est egalement possible de verifier si deux pointeurs pointent vers la meme variable cible
      if (associated(ppi, target=aptr)) then
         print *, 'les pointeurs ppi et aptr pointent tous les deux vers la meme variable cible'
      else
         print *, 'les pointeurs ppi et aptr ne pointent pas (actuellement) vers la meme variable cible'
      endif    

! 
! -----------------------------------------------------------------------
! --- Ci-dessous, on effectue la mise en oeuvre d'un tableau de pointeurs 
! -----------------------------------------------------------------------
! 

! --- Combien va t'il y avoir de chaines en memoire (nombre variable)
      PRINT*, 'Quel est le nombre de chaines a saisir? '
      READ(*,*) num

! --- On effectue l'allocation dynamique 
      allocate(array(num))
      do j = 1, size(array)
          allocate(array(j)%p(j))
      end do

! --- Boucle de saisie des messages a stocker en memoire vive
      lis : DO j = 1, num
               PRINT*, ' Saisir votre message : '
               READ 1001, array(j)%p(j)
            END DO lis

! --- Boucle d'affichage de tous les messages qui ont ete saisis
      aff_tous : DO j = 1, num
                    PRINT *, 'j=', j, 'chaine=[', array(j)%p(j), ']'
                 END DO aff_tous

! -------------------------
! ---   F O R M A T S   ---
! -------------------------
1001          format (a)

END PROGRAM ESSAI
