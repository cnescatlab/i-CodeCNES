 PROGRAM ESSAI

C..   Formal Arguments .. 
      double precision, public :: APX
      double precision save :: AKPX
      integer :: IND
      logical (LEN:1) :: ERREUR
C 
C..   Local Scalars .. 
      integer :: I,IM
      logical, private :: FIN_TRAITEMENT
      double precision :: epsilon
      save epsilon
C 
C..   Local Arrays .. 
      double precision, dimension :: AKP(28),AP(28)
C 

ERREUR = .false.
C
      if (IND .gt. 0) then

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
  
  
subroutine cps_liste(liste, nbliste, typec, corpsc, codes)

    integer :: codeCorpsc
    integer, dimension(:), pointer :: listeCodeCorps => NULL()
    integer :: trouve, ii, acces
    character(LEN=CPS_MAXLG), dimension(:), pointer :: fichiers => NULL()
    character(LEN=CPS_MAXLG) :: nom_corps, mess

    if (present(corpsc)) then
       ! trouver le code du corps dont le nom vaut corpsc
       mess="pour le corps central "//trim(corpsc)
       call cps_getListFichiersCateg(CPS_CATEG_CORPS, fichiers)
       trouve = CPS_ERR_DEF
       do ii=1, cpsi_size_ptr(fichiers)
          call cpsi_getAccesMadona(fichiers(ii), acces)
          ! recherche avec corpc = nom_fr
          trouve = cps_getCritere(acces, "nom_id", corpsc, "code", codeCorpsc)
          if (trouve.eq.CPS_OK) then
             exit
          end if
       end do

       if (trouve.eq.CPS_OK) then
          call cps_getCorps("corpsc", codeCorpsc, listeCodeCorps)
          nbliste = cpsi_size_ptr(listeCodeCorps)
          if (nbliste.gt.MAXCORPS) then
             nbliste = MAXCORPS
          end if
          do ii=1, nbliste
             trouve = cps_getAtt(listeCodeCorps(ii), "nom_id", nom_corps)
             liste(ii) = trim(nom_corps)
          end do

          if (present(codes)) then
             codes(1:nbliste) = listeCodeCorps(1:nbliste)
          end if
       end if
    end if

    
    
  end subroutine cps_liste
