C
C --- Le common 'CONTROL' defini ici contient 4 REAL
C
      BLOCK DATA  ABCD_VAL
      COMMON /CONTROL/ A, B, C, D
      DATA  A,B,C,D / 1.0, 2.0, 3.0, 4.0 /
      END
C
C --- Main
C
      PROGRAM ESSAI  
      CALL MY_SUB1
      STOP
      END PROGRAM ESSAI
C
C --- Subroutine 
C
      SUBROUTINE MY_SUB1
C
C --- Dans la definition du common nomme, il faut veiller au type des variables et a 
C --- leur ordre de definition : dans la routine, les donnees du common sont declarees comme 4 INTEGER 
C --- alors que les donnees du common sont 4 REAL 
C --- Les valeurs prises ci-dessous par I,J,K,L sont erronees 
C
      COMMON /CONTROL/ I, J, K, L
C
      WRITE(*,*) 'I=', I, 'J=', J
      RETURN
      END
