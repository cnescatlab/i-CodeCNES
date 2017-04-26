	subroutine s1
		
	  type cpsi_acces
		character(len=CPS_MAXLG) :: fichier
		integer :: acces
		integer(kind=4) :: frequence
	  end type cpsi_acces
	
	  type (cpsi_acces), dimension(:), allocatable :: tab_acces
	  
	  ok = cps_existe(corps, mu=mu, requa=requa, apla=apla, J2=J2, vrot=vrot, &
          potentiel=potentiel, atmosphere=atmosphere, geometrie=geometrie, &
          dga=dga, exc=exc, inc=inc, corpsc=corpsc, typec=typec, &
          G=G, vlum=vlum, ua=ua)
		
	end subroutine 

	module mod1
		real :: y=2
		
		contains
			
			function f1()
				real :: y
			end function
			
			
			function f2()
				real :: z
			end function
			
	end module
	
	module mod2
		real :: x=2
		
		contains
			
			contains
			
				subroutine s2()
					real :: x
					
					function f3()
						real :: w
						real :: x
					end function
			
				end function
			
	end module