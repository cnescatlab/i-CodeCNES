!
! Cette unite de programme 
PROGRAM ESSAI

      IMPLICIT NONE 

! --- Correspond au nombre de digits pour representer un nombre : 18, 9, 5 ou 3 chiffres
      integer, parameter :: LONG   = selected_int_kind(18)
      integer, parameter :: ENTIER = selected_int_kind(9)
      integer, parameter :: COURT  = selected_int_kind(5)
      integer, parameter :: BYTE   = selected_int_kind(3)

! --- Declaration des variables de differents type d'entiers 
      INTEGER(BYTE), parameter     :: n = 10
      INTEGER(BYTE)                :: i
      INTEGER(BYTE)                :: octet

      INTEGER(LONG)                :: l_entier_1, l_entier_2
      INTEGER(ENTIER)              :: i_entier_1, i_entier_2
      INTEGER(COURT)               :: s_entier_1, s_entier_2
      INTEGER(BYTE)                :: b_entier_1, b_entier_2
      INTEGER(COURT), DIMENSION(n) :: TAB

      DATA TAB / 5*1, 2, 3, 7, 0, 8 /
 
! Calcul de la valeur MAX d'un entier LONG (sur 64 bits) et affichage de cette valeur sur la sortie standard  
      write(*, '(" --- Calcul de la valeur MAX d''un entier LONG (sur 64 bits) --- ")')
      l_entier_1 = 2_LONG
      l_entier_2 = l_entier_1 ** 63 - 1
      write(*,'(3X,"l_entier_1=",I19)') l_entier_1
      write(*,'(3X,"l_entier_2=",I19)') l_entier_2

! Calcul de la valeur MAX d'un entier (sur 32 bits) et affichage de cette valeur sur la sortie standard  
      write(*, '(" --- Calcul de la valeur MAX d''un entier (sur 32 bits) --- ")')
      i_entier_1 = 2_ENTIER
      i_entier_2 = i_entier_1 ** 31 - 1
      write(*,'(3X,"i_entier_1=",I10)') i_entier_1
      write(*,'(3X,"i_entier_2=",I10)') i_entier_2

! Calcul de la valeur MAX d'un entier court (sur 16 bits) et affichage de cette valeur sur la sortie standard  
      write(*, '(" --- Calcul de la valeur MAX d''un entier court (sur 16 bits) ---")')
      s_entier_1 = 2_COURT
      s_entier_2 = s_entier_1 ** 16 - 1
      write(*,'(3X,"s_entier_1=",I5)') s_entier_1
      write(*,'(3X,"s_entier_2=",I5)') s_entier_2

! Calcul de la valeur MAX d'un octet (sur 8 bits) et affichage de cette valeur sur la sortie standard  
      write(*, '(" --- Calcul de la valeur MAX d''un octet (sur 8 bits) ---")')
      b_entier_1 = 2_BYTE
      b_entier_2 = s_entier_1 ** 8 - 1
      write(*,'(3X,"b_entier_1=",I3)') b_entier_1
      write(*,'(3X,"b_entier_2=",I3)') b_entier_2

! --- Boucle DO en utilisant un entier code sur 8 bits car peu d'elements a parcourir 
      write(*, '(" --- Boucle de parcours d''un tableau de ",I3," elements avec un indice de type entier sur 3 digits (0->255)")') n
      WRITE(*, '(3X,5(1X,I5))') (TAB(i), i=1,n)

! --- Mise en oeuvre des fonctions de manipulations de bits
      octet = 3
      write(*, '(" --- Decalage d''un bit vers la droite (MSB a droite) d''un octet (sur 8 bits) == multiplication par 2 ---")')
      write(*,'(3X,"valeur initiale=",I3," -> apres decalage=",I3)') octet, ishft(octet,1)
      octet = 120
      write(*, '(" --- Decalage d''un bit vers la gauche (MSB a droite) d''un octet (sur 8 bits) == division par 2 ---")')
      write(*,'(3X,"valeur initiale=",I3," -> apres decalage=",I3)') octet, ishft(octet,-1)

END PROGRAM ESSAI
