	OPEN (UNIT = f_unit, FILE = c_arg, STATUS = 'UNKNOWN')
! Regle 'Int.ParamOpen' n'est pas respectee ci-dessus : 
! . Il manque le parametre POSITION
! . Le positionnement de STATUS a la valeur 'UNKNOWN' est interdit 
	READ (UNIT = f_unit, FMT = 9011, IOSTAT = ios) c_buffer
