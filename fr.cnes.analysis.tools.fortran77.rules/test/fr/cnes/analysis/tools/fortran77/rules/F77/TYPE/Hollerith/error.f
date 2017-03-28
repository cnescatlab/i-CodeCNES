      PROGRAM ESSAI

      COMPLEX*16 X(2)
      DATA X /16Habcdefghijklmnop, 16Hqrstuvwxyz012345/
      X(1) = 16HABCDEFGHIJKLMNOP

      CALL MY_SUB1 ( x )
      CALL MY_SUB2 ( 4habcd )

      END PROGRAM ESSAI

C
C --- 
C
      SUBROUTINE MY_SUB1 (X_SP)
      COMPLEX*16 X_SP(2) 
      WRITE(*,*) 'X_SP=', X_SP 
      RETURN
      END

C
C --- 
C
      SUBROUTINE MY_SUB2 (C_STRING)
      CHARACTER *4   C_STRING
      WRITE(*,*) 'C_STRING=', C_STRING
      RETURN
      END
