 PROGRAM ESSAI

C..   Formal Arguments .. 
      double precision APX
      double precision AKPX
      integer IND
      logical ERREUR
C 
C..   Local Scalars .. 
      integer I,IM
      logical FIN_TRAITEMENT
      double precision epsilon
      save epsilon
C 
C..   Local Arrays .. 
      double precision AKP(28),AP(28)
C 
C..   Intrinsic Functions .. 
      intrinsic INT
      ERREUR = .false.
C
      if (IND .gt. 0) then
C
C***********************************************************************
C*FON PASSAGE AP ---> KP
C***********************************************************************
C
        if (APX .lt. 0.d0) then
          ERREUR = .true.
          return
        endif
        FIN_TRAITEMENT = .false.
        I = 1
        do while ((I .le. 28) .and. (.not. FIN_TRAITEMENT))
C
          if ( abs(APX-AP(I)) .le. epsilon  ) then
            AKPX = AKP(I)
            FIN_TRAITEMENT = .true.
          endif
C
          if (APX .le. AP(I)) then
            IM = I - 1
            AKPX = AKP(IM) +
     &             (AKP(I)-AKP(IM))*(APX-AP(IM))/(AP(I)-AP(IM))
            FIN_TRAITEMENT = .true.
          else
            I = I+1
          endif
C
        end do
	  
	   
  END PROGRAM
