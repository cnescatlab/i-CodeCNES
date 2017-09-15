WRITE(*,*) '--- 1ere Boucle ---'
      DO    20,    I  =  I_MIN, I_MAX, I_STEP
         DO    10,    J  =  J_MIN, J_MAX, J_STEP
            WRITE(*,*) 'I=', I, 'J=', J
10    CONTINUE	
20    CONTINUE

C
C --- Autre possibilite sans utiliser des etiquettes
C
      WRITE(*,*) '--- 2eme Boucle ---'
      DO    I  =  I_MIN, I_MAX, I_STEP
         DO   J  =  J_MIN, J_MAX, J_STEP
            WRITE(*,*) 'I=', I, 'J=', J
         END DO 
      END DO    
