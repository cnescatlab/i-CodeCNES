 PROGRAM ESSAI

C..   Formal Arguments .. 
      double precision :: APX
      double precision :: AKPX
      integer, parameter :: IND
      logical, intnt(in) :: ERREUR
C 
C..   Local Scalars .. 
      integer :: I,IM
      logical, parameter :: FIN_TRAITEMENT
      double precision :: epsilon
C 
C..   Local Arrays .. 
      double precision, dimension :: AKP, AP
C 

C
      if (IND .gt. 0) then

        if (APX .lt. 0.d0) then
          return
        endif
        FIN_TRAITEMENT = .false.
        I = 1
        do while ((I .le. 28) .and. (.not. FIN_TRAITEMENT))
C
          if ( abs(APX-AP(I)) .le. APX  ) then
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
