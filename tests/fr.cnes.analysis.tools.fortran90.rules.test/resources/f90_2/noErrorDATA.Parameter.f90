MODULE ma_precision
	integer, parameter :: DOUBLE = selected_real_kind(15)
	integer, parameter :: SIMPLE = selected_real_kind(6)
	integer, parameter :: LONG   = selected_int_kind(18)
	integer, parameter :: ENTIER = selected_int_kind(9)
	integer, parameter :: COURT  = selected_int_kind(4)
	integer, parameter :: DISTANCE = selected_real_kind(10, 30)
	integer, parameter :: IEEE_DOUBLE = selected_real_kind(15, 307)
END MODULE ma_precision
