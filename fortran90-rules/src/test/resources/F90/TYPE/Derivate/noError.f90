module T_PERSONNE

      type PERSONNE
         character (len=20) :: nom
         integer :: date
      end type PERSONNE

end module T_PERSONNE

!
! Cette unite de programme (appel sans parametre) effectue l'implementation
! d'un nouveau type de donnees nommee 'T_Personne' (structure) dans laquelle sont stockées
! des caracteristiques personnelles .
! Un membre de cette structure est cree puis initialise avec des donnnees
! Une subroutine 'Affiche' imprime sur la sortie standard les caracteristiques du membre de
! la structure passee en parametre
! Le module T_PERSONNE definit l'organisation de la structure avec les differents champs qui la compose. 
!
program ESSAI

      use T_PERSONNE
      type (PERSONNE) :: DURAND
      
! --- Initialisation des caracteristiques de la personne 
      DURAND%nom = 'Durand Jean.Pierre'
      DURAND%date = 1234

! --- Affichage des caracteristiques de DURAND
      call AFFICHE (DURAND)

end program ESSAI

!
! Cette subroutine affiche sur la sortie standard les caracteristiques (nom et date)
! d'un membre de la structure de type Personne qui a ete passe en parametre
!

subroutine AFFICHE (X)

      use T_PERSONNE
      type (PERSONNE), INTENT(IN) :: X

      write(*,'("Nom:",A20," Date:",I8)') X%nom, X%date

end subroutine AFFICHE

