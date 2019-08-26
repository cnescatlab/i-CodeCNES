 PROGRAM ESSAI

     select case (month)
	    case ("January")
	       num_days = 31
	    case ("February")
	       num_days = 28
	       print *,"You can put more stuff here."
	    case ("March")
	       num_days = 31
	    case default
	       num_days = 30
	 end select
	   
  END PROGRAM
