PROGRAM ESSAI

	  INTEGER            :: f_unit = 15
	  INTEGER            :: f_unit2
	  INTEGER            :: f_unit3
	  
	  f_unit2 = 15
	  f_unit3 = newunit(2)


! --- Ouverture du fichier 
      OPEN (UNIT = f_unit, FILE = '/etc/passwd', STATUS = 'OLD', &
			 POSITION = 'REWIND', IOSTAT = ios)
	  OPEN (UNIT = f_unit2, FILE = '/etc/passwd', STATUS = 'OLD', &
			 POSITION = 'REWIND', IOSTAT = ios)
	  OPEN (UNIT = 15, FILE = '/etc/passwd', STATUS = 'OLD', &
			 POSITION = 'REWIND', IOSTAT = ios)
	  OPEN (f_unit3, FILE = '/etc/passwd', STATUS = 'OLD', &
			 POSITION = 'REWIND', IOSTAT = ios)
	  OPEN (15, FILE = '/etc/passwd', STATUS = 'OLD', &
			 POSITION = 'REWIND', IOSTAT = ios)


END PROGRAM ESSAI
