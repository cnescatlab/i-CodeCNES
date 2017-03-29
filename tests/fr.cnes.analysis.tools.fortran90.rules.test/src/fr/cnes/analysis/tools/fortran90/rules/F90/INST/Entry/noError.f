SUBROUTINE FIN( A, B, C ) 

   INTEGER, INTENT(INOUT) :: A
   INTEGER, INTENT(INOUT) :: B
   CHARACTER, INTENT(INOUT) :: C*4 

   A = 2
   B = 5
   C = 'Fin'

   RETURN 
END SUBROUTINE FIN

SUBROUTINE HELP( A, B, C ) 
   INTEGER, INTENT(INOUT) :: A
   INTEGER, INTENT(INOUT) :: B
   CHARACTER, INTENT(INOUT) :: C*4 
   A = 12
   B = 15
   C = 'Help'
   RETURN 
END SUBROUTINE HELP 

SUBROUTINE RIEN 
   WRITE(*,*) '--- Rien a faire ---'
   RETURN 
END SUBROUTINE RIEN

! 
! Le programme ci-dessous appelle successivement les 3 routines suivantes : 
!    - FIN
!    - RIEN
!    - HELP
! 

PROGRAM ESSAI 

   USE precision 
   IMPLICIT NONE

   INTEGER A
   INTEGER B
   CHARACTER C*4 

   CALL FIN( A, B, C ) 
   WRITE(*,*) 'A:', A, 'B:', B, 'C:', C
   CALL RIEN
   CALL HELP( A, B, C ) 
   WRITE(*,*) 'A:', A, 'B:', B, 'C:', C

END PROGRAM ESSAI
