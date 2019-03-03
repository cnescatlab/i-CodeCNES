SUBROUTINE FIN( A, B, C ) 

       INTEGER A, B 
       CHARACTER C*4 
          A = 2
          B = 5
          C = 'Fin'
       RETURN 

       ENTRY HELP( A, B, C ) 
          A = 12
          B = 15
          C = 'Help'
       RETURN 

       ENTRY RIEN 
          WRITE(*,*) '--- Rien a faire ---'
       RETURN 

END SUBROUTINE FIN

! 
! Le programme ci-dessous appelle les 3 routines suivantes : 
! - FIN
! - RIEN
! - HELP
! Les routines HELP et RIEN, sont des points d'entree dans la subroutine 'FIN'
!
! La regle interdit l'emploi de l'instruction ENTRY 
! 

PROGRAM ESSAI 

      USE precision 
      IMPLICIT NONE

      INTEGER A, B 
      CHARACTER C*4 

      CALL FIN( A, B, C ) 
      WRITE(*,*) 'A:', A, 'B:', B, 'C:', C
      CALL RIEN
      CALL HELP( A, B, C ) 
      WRITE(*,*) 'A:', A, 'B:', B, 'C:', C

END PROGRAM ESSAI
