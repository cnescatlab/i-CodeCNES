!
! F90_Tr.InterfaceVisible_2_ok : correspond au 2eme cas de figure enonce dans la regle 
!                                c'est a dire que le sous-programme est un sous-programme interne de l'unite courante 
!                                ou de l'unite englobant(module ou sous-programme hote)
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

! L'appel formulé el que ci-dessous est incorrect : 
!  call  Mouv(3.0_DOUBLE, oper1, resul)
!
! Le compilateur detecte l'erreur suivante :  
!
!   call  Mouv(3.0_DOUBLE, oper1, resul)
!              1
! Error: Actual argument at (1) must be definable as the dummy argument 'oper0' is INTENT = OUT/INOUT

! Impression sur sortie standard des variables apres traitement  
   write (*,*) '--- Apres --- oper0:', oper0, 'oper1:', oper1, 'resul:', resul

contains

! l'interface de Mouv est visible implicitement (sous-programme interne)

   subroutine Mouv(oper0,oper1,resul)

      USE precision 
      IMPLICIT NONE

      real(DOUBLE), intent(inout) ::   oper0
      real(DOUBLE), intent(in)    ::   oper1
      real(DOUBLE), intent(out)   ::   resul

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
