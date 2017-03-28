!
! Cette unite de programme (appel sans parametre) effectue limplementation 
! d'une structure de donnees nommee 'Personne' dans laquelle sont stockées 
! des caracteristiques personnelles .
! Un membre de structure est initialise avec des donnnees 
! Une subroutine 'Affiche' imprime sur la sortie standard les caracteristiques du membre de 
! la structure passee en parametre 
! 
program PERSONNEL

      type PERSONNE
         sequence
         character (len=20) :: nom
         integer :: date
      end type PERSONNE

      type (PERSONNE) :: DURAND

! --- Initialisation des caracteristiques de la personne
      DURAND%nom = 'Durand Jean.Pierre'
      DURAND%date = 1234

! --- Affichage des caracteristiques de DURAND
      call AFFICHE (DURAND)

end program PERSONNEL

!
! Cette subroutine affiche sur la sortie standard les caracteristiques (nom et date) 
! d'un membre de la structure de type P (Personne) 
! 
subroutine AFFICHE (X)

      type P
         sequence
         character (len=20) :: nom
         integer :: date
      end type P
! !!! Dans l'instruction ci-dessus, la regle 'Don.TypDerive' n'est pas respectee . 
! !!! La subroutine redefinit un autre type derive qui est identique a celui declare 
! !!! dans le programme proncipal mais sous un autre nom . Cela n'a pas grand interet .
! !!! L'utilisation de l'attribut SEQUENCE est obligatoire afin que FORTRAN organise 
! !!! la structure de donnees dans l'ordre souhaite ; si l'attribut SEQUENCE est absent 
! !!! les membres de structure peuvent ne pas etre dans l'ordre attendu .
 
      type (P), INTENT(IN) :: X

      write(*,'("Nom:",A20," Date:",I8)') X%nom, X%date

end subroutine AFFICHE
