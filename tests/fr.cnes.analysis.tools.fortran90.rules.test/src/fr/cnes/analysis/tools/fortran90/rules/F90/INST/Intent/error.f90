! 
! routine Pas_1 : appel sans aucun parametre
! 
subroutine Pas_1

   USE precision 
   IMPLICIT NONE

   REAL(DOUBLE) :: oper0
   REAL(DOUBLE) :: oper1
   REAL(DOUBLE) :: resul

! Initialisation des variables locales 
   oper0 = 3.0_DOUBLE
   oper1 = 4.0_DOUBLE

! Impression sur sortie standard des variables avant traitement  
   write (*,*) '--- Avant --- oper0:', oper0, 'oper1:', oper1, 'resul:', resul

! Apppel de la routine de calcul 
   call  Mouv(oper0, oper1, resul)

! Impression sur sortie standard des variables apres traitement  
   write (*,*) '--- Apres --- oper0:', oper0, 'oper1:', oper1, 'resul:', resul

contains

! l'interface de Mouv est visible implicitement (sous-programme interne)
! cependant la regle n'est pas respectee car la clause INTENT n'etant pas 
! precisee pour chaque parametre, le compilateur ne peut controler si la routine 
! manipule correctement le contenu de ces parametres 

   subroutine Mouv(oper0,oper1,resul)

      USE precision 
      IMPLICIT NONE

      real(DOUBLE) ::   oper0
      real(DOUBLE) ::   oper1
      real(DOUBLE) ::   resul

      resul = oper0 + oper1
      oper0 = 8.0_DOUBLE

   end subroutine Mouv 

end subroutine Pas_1

!
! Programme principal : pour la forme mais obligatoire si l'on veut obtenir un programme executable 
!    car le coeur du traitement se situe dans la routine 
! 
PROGRAM ESSAI

   CALL Pas_1

END PROGRAM ESSAI
