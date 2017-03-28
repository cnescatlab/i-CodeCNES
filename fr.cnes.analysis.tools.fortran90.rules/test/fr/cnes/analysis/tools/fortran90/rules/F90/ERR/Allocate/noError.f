!
! Ce programme effectue l'allocation dynamique de deux tableaux.
! Lees deux tableaux sont initialises avec des données qui sont saisies au clavier .
! Une fois, le tableau rempli, on appelle 3 fonctions intrinsèques :
! . MINVAL : qui permet d'extraire la valeur minimale d'un tableau
! . MAXVAL : qui permet d'extraire la valeur maximale d'un tableau
! . SUM : qui permet d'effectuer le calcul de la somme de tous les elements d'un tableau
!
PROGRAM ESSAI

      IMPLICIT NONE 

      INTEGER, parameter :: n1 = 20
      INTEGER, parameter :: n2 = 10
      INTEGER            :: iom

      REAL               :: plus_petit
      REAL               :: plus_grand
      REAL               :: Somme_A
      REAL               :: Somme_B

      REAL, DIMENSION(:), ALLOCATABLE :: A 
      REAL, DIMENSION(:), ALLOCATABLE :: B 

! --- Allocation du tableau A
      allocate(A(n1), STAT = iom)
      if (iom > 0) then
         call f_Syslog('Allocation du tableau A echouee')
         stop
      end if

! --- Allocation du tableau B
      allocate(B(n2))
      if ( .NOT. allocated(B)) then
         call f_Syslog('Allocation du tableau B echouee')
         stop
      end if

! --- On alimente le contenu des 2 tableaux avec des valeurs saisies au clavier 
      write(*, '(A,I2,A)', ADVANCE = "NO") "Saisir les valeurs (", n1, ") de A : "
      read(*,*) A
      write(*, '(A,I2,A)', ADVANCE = "NO") "Saisir les valeurs (", n2, ") de B : "
      read(*,*) B

! Appel de la fonction intrinseque MINVAL afin de determiner la valeur minimale des elements du tableau
      plus_petit = MINVAL(A)
      write(*,'(A,F8.2)') "Plus petite valeur du tableau A", Plus_Petit

! Appel de la fonction intrinseque MAXVAL afin de determiner la valeur maximale des elements du tableau
      plus_grand = MAXVAL(B)
      write(*,'(A,F8.2)') "Plus grande valeur du tableau B", Plus_Grand

! Appel fonction intrinseque SUM pour calcul de la somme des elements des tableaux A puis B
      Somme_A = SUM(A)
      Somme_B = SUM(B)

! Impression sur sortie standard du resultat obtenu
      write(*,'(A,F8.2)') "La somme des elements du tableau A est ", Somme_A
      write(*,'(A,F8.2)') "La somme des elements du tableau B est ", Somme_B

! Desallouer la memoire associee aux tableaux 
      if (allocated(A)) then 
         deallocate(A, stat=iom)
         if (iom > 0) then
            call f_Syslog('Desallocation du tableau A echouee')
         endif 
      else 
            call f_Syslog('Desallocation du tableau A deja effectuee')
      endif 
      if (allocated(B)) then 
         deallocate(B, stat=iom)
         if (iom > 0) then
            call f_Syslog('Desallocation du tableau B echouee')
         endif 
      else 
            call f_Syslog('Desallocation du tableau B deja effectuee')
      endif 

END PROGRAM ESSAI
