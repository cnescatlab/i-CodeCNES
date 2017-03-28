 PROGRAM ESSAI

      INTEGER i

	  IF ( .NOT. A .OR. .NOT. B) i = 0
	  
	  IF ( .NOT. (.NOT. A)) i = 1
	  
	  IF ( .NOT. (.NOT. A .OR. & 
	  	   .NOT. B)) i = 2
	  
	  IF ( .NOT. .NOT. A) i = 3
	  
	  IF ( .NOT. ( A .OR. .NOT. B)) i = 4
	   
  END PROGRAM
