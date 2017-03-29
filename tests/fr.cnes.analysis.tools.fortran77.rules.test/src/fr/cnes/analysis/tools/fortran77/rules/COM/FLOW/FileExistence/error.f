 PROGRAM ESSAI

	inquire(unit_read, access)

	read(unit=unit_read, fmt=FORMAT_LIGNE_TXT)
	
    open(unit=unit_read,file=fichier,status='old',&
         form='formatted', iostat=ierr)

    if ( ierr /= 0 ) then
       call MSP_signaler_message (cle_mes="CPS_ERR_OPEN", &
            routine="cps_lireAcsol2", &
            partie_variable=trim(fichier))
       return
    endif       
    
    inquire(file=unit_read, exist)
    
    ! Lecture des lignes de commentaires
    do i = 1,7
       read(unit=unit_read, fmt=FORMAT_LIGNE_TXT)
    end do
    
    ! Détermination du nombre de lignes
    ierr = 0
    do while (ierr == 0 ) 
       ! Lecture de la ligne dans le fichier
       read(unit=unit_read2, fmt=FORMAT_LIGNE_TXT, iostat=ierr) buff
       ! S'il on n'est pas à la fin du fichier 
       ! et qu'il ne s'agit pas d'une ligne blanche
       if ( ierr == 0 .and. len_trim(buff) /= 0 ) then
          if (ierr2 == 0 .and. ligne_tmp%indflu /= 0 ) then
             nb_lignes = nb_lignes+1
          end if
       endif
    end do
    ! Fermeture du fichier
    close (unit_read)     

	read(unit=unit_read, fmt=FORMAT_LIGNE_TXT)
	   
  END PROGRAM