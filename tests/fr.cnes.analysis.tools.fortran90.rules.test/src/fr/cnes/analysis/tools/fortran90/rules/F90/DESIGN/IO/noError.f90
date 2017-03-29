PROGRAM ESSAI

      INTEGER            :: f_unit
      INTEGER            :: newunit

! --- Allocation d'un numero d'unite logique 
      f_unit = newunit(n) 

! --- Ouverture du fichier 
      OPEN (UNIT = f_unit, FILE = '/etc/passwd', STATUS = 'OLD', &
			POSITION = 'REWIND', IOSTAT = ios)


END PROGRAM ESSAI
