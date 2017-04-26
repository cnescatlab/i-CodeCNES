      PROGRAM ESSAI
C
C --- En appellant successivement SUB1 puis SUB2, on peut observer 
C --- l'effet de la precision (simple ou double) sur la valeur d'une constante telle que Pi
C
      CALL SUB1
      CALL SUB2

      STOP
      END PROGRAM ESSAI

C
C --- Dans la routine SUB1, la constante Pi est declare avec une valeur en simple precision  
C
      SUBROUTINE SUB1
      DOUBLE PRECISION CONSTANTE_PI
      PARAMETER (CONSTANTE_PI = 3.141592654)
      WRITE(*,*) 'PI (Simple precision)=', CONSTANTE_PI
      RETURN
      END

C
C --- Dans la routine SUB2, la constante Pi est declare avec une valeur en double precision  
C
      SUBROUTINE SUB2
      DOUBLE PRECISION CONSTANTE_PI
      PARAMETER (CONSTANTE_PI = 3.141592654D+00)
      WRITE(*,*) 'PI (Double precision)=', CONSTANTE_PI
      RETURN
      END

