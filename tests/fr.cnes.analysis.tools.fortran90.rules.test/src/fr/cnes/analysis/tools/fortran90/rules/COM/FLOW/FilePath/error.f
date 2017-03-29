 PROGRAM ESSAI

          ! Numero d'unite disponible
		    call cps_file_unit(unit_read,ierr)
		    if(ierr.lt.0)then
		       call MSP_signaler_message (cle_mes="CPS_ERR_UNIT", &
		            routine="cps_lireAcsol2", &
		            partie_variable=trim(fichier))
		       return
		    endif
		    
		    open(unit=unit_read,file="fichier.txt",status='old',&
		         form='formatted', convert='big_endian', iostat=ierr)
		#ifdef __GFORTRAN__
		    open(unit=unit_read,file='../path/to/file.f90',status='old',&
		         form='formatted', convert='big_endian', iostat=ierr)
		#else
		    open(unit=unit_read,file=fichier,status='old',&
		         form='formatted', iostat=ierr)
		#endif
		
		    if ( ierr /= 0 ) then
		       call MSP_signaler_message (cle_mes="CPS_ERR_OPEN", &
		            routine="cps_lireAcsol2", &
		            partie_variable=trim(fichier))
		       return
		    endif       

	   
  END PROGRAM
