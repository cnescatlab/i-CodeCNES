! Ouverture du fichier 
! - STATUS   -> permet de preciser : 
!        . s'il s'agit d'un fichier existant ('old'), 
!        . a creer ('new'), 
!        . a remplacer ('replace'), 
!        . temporaire ('scratch') 
!        . quelconque ('unknown)   
! - POSITION -> permet de se positionner dans le fichier ouvert, 
!        . au debut ('rewind'), 
!        . a la fin ('append'), 
!        . ou a la derniere position en date ('asis') 
      OPEN (UNIT = f_unit, FILE = c_arg, STATUS = 'OLD', 		&			POSITION = 'REWIND', IOSTAT = ios)
      IF (IOS .LT. 0) THEN
		    …

! - Lecture du 1er enregistrement 
      READ (UNIT = f_unit, FMT = 9011, IOSTAT = ios) c_buffer
