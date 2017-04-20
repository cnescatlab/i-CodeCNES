 PROGRAM ESSAI

      INTEGER i

	  IF ( .NOT. A .OR. .NOT. B) i = 0
	  
	  IF ( .NOT. (.NOT. A)) i = 1
	  
	  IF ( .NOT. (.NOT. A .OR. .NOT. (B .AND. c))) i = 2
	  
	  IF ( (.NOT. ( a .OR. b) .XOR. (a .AND. b)) .AND. (x .LT. y)) i = 3
	  
	  IF ( .NOT. ( A .OR. .NOT. B)) i = 4
	  
	  IF( .NOT.CHB .AND. .NOT.GLM .AND. .NOT.GQR .AND. .NOT. &
		 GSV .AND. .NOT.CSD .AND. .NOT.LSE ) i = 5
	   
  END PROGRAM
