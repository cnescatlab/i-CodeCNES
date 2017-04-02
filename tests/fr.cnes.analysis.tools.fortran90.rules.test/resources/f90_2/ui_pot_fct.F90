module ui_potentiel_fct

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Version
!      $Id: ui_pot_fct.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!      $Log: ui_pot_fct.F90,v $
!      Revision 1.9  2010/10/25 08:35:51  ogarat
!      VERSION::AQ::24/10/2010:Ajout des FinHistorique
!
!      Revision 1.8  2009/09/09 15:07:27  cmartel
!      FA-ID 1196 : Correction mineures sur les sorties des utilitaires
!
!      Revision 1.7  2008/11/07 10:13:44  cml
!      AQ : Correction de tests d egalite entre un reel et zero
!
!      Revision 1.6  2008/11/07 09:56:12  cml
!      AQ : Correction de warning foresys
!
!      Revision 1.5  2008/04/28 13:02:21  vivaresf
!      FA-ID 664 : présentation des sorties et cartouches
!
!      Revision 1.4  2008/04/11 12:47:03  vivaresf
!      FA-ID 778 : rajout des cartouches
!      AQ : correction des cartouches existant
!
!      Revision 1.3  2008/04/11 10:57:35  vivaresf
!      FA-ID 778 : suppression des variables inutilisées
!      Revision 1.2  2008/04/11 10:09:28  vivaresf
!      FA-ID 778 : renommage des variables mal nommées
!      suppression des doubles déclarations
!      rajout de implicit none
!      rajout de intent(in/out)
!      suppression des lignes de code commentées
!      Revision 1.1  2008/02/08 17:51:27  vivaresf
!      FA-ID 889, DM-ID 820 :
!      - réorganisation du code pour que tout soit dans le répertoire src
!      Revision 1.4  2007/02/14 14:44:44  mle
!      FA-ID 679 : incoherence entre les modeles de potentiel issus du format MSDON et les modeles GRGS
!      Revision 1.3  2006/12/01 10:00:24  vivaresf
!      FA 647 : Mauvaise extraction du nom du modèle dans ui_pot_grgs
!
!$FinHistorique
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use cps_potentiel
  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: ui_pot_fct.F90 69 2012-09-11 08:33:34Z ffsm $'


contains

    subroutine cps_lirePotentielMSDON(fich_MSDON,zb,clmb,slmb,mu,requa,apla,degmax,ordremax)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_lirePotentielMSDON
!
!$Resume
! Routine de lecture d'un fichier potentiel au format MSDON
!
!$Description
! Routine de lecture d'un fichier potentiel au format GRGS
!
!$Auteur
! mle
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_lirePotentielMSDON(fich_MSDON,zb,clmb,slmb,mu,requa,apla,degmax,ordremax)
!.    character(LEN=*) :: fich_MSDON 
!.    real(KIND=pm_reel), dimension(:), pointer :: zb
!.    real(KIND=pm_reel), dimension(:,:), pointer :: clmb, slmb
!.    real(KIND=pm_reel) :: mu,requa,apla
!.    integer :: degmax, ordremax
!
!$Arguments
!>E     fich_MSDON  :<LEN=*>                       nom du fichier de potentiel au format MSDON
!>E/S   zb          :<pm_reel,DIM=(:),pointer>     coefficients zonaux (en cosinus)
!>E/S   clmb        :<pm_reel,DIM=(:,:),pointer>   coefficient tesseral normalisés en cosinus
!>E/S   slmb        :<pm_reel,DIM=(:,:),pointer>   coefficient tesseral normalisés en sinus
!>S     mu          :<pm_reel>                     mu (m**3.sec-2)
!>S     requa       :<pm_reel>                     rayon équatorial (m)
!>S     apla        :<pm_reel>                     1/aplatissement
!>S     degmax      :<integer>                     degré maximal des coefficients présents dans le fichier
!>S     ordremax    :<integer>                     ordre maximal des coefficients présents dans le fichier
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      use eph_util

      implicit none

      !arguments
      character(LEN=*),intent(IN) :: fich_MSDON        
      real(KIND=pm_reel), dimension(:), pointer :: zb
      real(KIND=pm_reel), dimension(:,:), pointer :: clmb, slmb
      real(KIND=pm_reel), intent(OUT) :: mu,requa,apla
      integer, intent(OUT) :: degmax, ordremax
 
      ! variables locales
      real (KIND=pm_reel), dimension(5) :: vallue
      integer, dimension(5) :: i1,i2
      integer :: ier,j,ifich

	
      !initialisation
      ier = 0
      i1(:) = 0
      i2(:) = 0
      ordremax = 0
      ! unite logique pour la lecture du fichier
      call eph_util_ficunit90(ifich,ier)
      if (ier/=0) then
	! erreur : aucune unite logique libre n'a ete trouvee
	call MSP_signaler_message(cle_mes="CPS_ERR_UNIT",&
            routine="cps_lirePotentielMSDON", &
            partie_variable=trim(fich_MSDON))
        return
      end if

      !ouverture du fichier
      open (unit=ifich, file=fich_MSDON, status="old", access="sequential", &
           iostat=ier)
      if (ier/=0) then
        call MSP_signaler_message(cle_mes="CPS_ERR_OPEN",&
            routine="cps_lirePotentielMSDON", &
            partie_variable=trim(fich_MSDON))
        return
      end if

      ! LECTURE DU FICHIER:
      !lignes 1 et 2 : texte, pas de valeurs
      read (ifich,*,end=1000,err=2000)
      read (ifich,*,end=1000,err=2000)

      ! MU, RAYON EQUATORIAL, APLATISSEMENT:
      !ligne 3
      read(ifich,*,iostat=ier)mu,requa,apla
      if ( ier < 0 ) then
         call MSP_signaler_message (cle_mes="CPS_FIN_FICHIER",&
              partie_variable='"lors de la lecture des constantes planéto"', &
              routine="cps_lirePotentielMSDON")
         return
      endif
      if ( ier > 0 ) then
         call MSP_signaler_message (cle_mes="CPS_ERR_READ",&
              partie_variable='"des constantes planéto"', &
              routine="cps_lirePotentielMSDON")
         return
      endif

      ! DEGRE MAXIMAL
      !lignes 4 et 5
  
      read (ifich,*,end=1000,err=2000) 
      read (ifich,*,iostat=ier) degmax
      if ( ier < 0 ) then
         call MSP_signaler_message (cle_mes="CPS_FIN_FICHIER",&
              partie_variable='"lors de la lecture du degré max"', &
              routine="cps_lirePotentielMSDON")
         return
      endif
      if ( ier > 0 ) then
         call MSP_signaler_message (cle_mes="CPS_ERR_READ",&
              partie_variable='"du degré max"', &
              routine="cps_lirePotentielMSDON")
         return
      endif

      if (associated(zb)) then
         deallocate(zb,stat=ier)
      end if
      if ( associated(clmb)) then
         deallocate(clmb,stat=ier)
      end if
      if ( associated(slmb)) then
         deallocate(slmb,stat=ier)
      end if

      ALLOCATE(zb(degmax))
      ALLOCATE(clmb(degmax,degmax))
      ALLOCATE(slmb(degmax,degmax))

      ! MISE A ZERO DES TABLES:
      zb(:) = 0._pm_reel
      clmb(:,:) = 0._pm_reel
      slmb(:,:) = 0._pm_reel

      ! COEFFICIENTS ZB
      ! (coefficients zonaux : m=0)
      do while ( (i1(5)/=1) .or. (i2(5)/=1) )
         read (ifich,9050,iostat=ier) (i1(j),i2(j),vallue(j),j=1,5)
         if ( ier < 0 ) then
            call MSP_signaler_message (cle_mes="CPS_FIN_FICHIER",&
                 partie_variable='"lors de la lecture des termes zonaux"', &
                 routine="cps_lirePotentielMSDON")
            return
         endif
         if ( ier > 0 ) then
            call MSP_signaler_message (cle_mes="CPS_ERR_READ",&
                 partie_variable='"des termes zonaux"', &
                 routine="cps_lirePotentielMSDON")
            return
         endif
         ordremax=max(maxval(i2)-1,ordremax)
         do j = 1,5
            if (  i1(j)>1 ) then
               zb(i1(j)) = vallue (j)
            endif
         enddo
      enddo

      ! COEFFICIENTS CLM BARRE (CLMB)

      i1(5) = 0
      i2(5) = 0
      do while ( (i1(5)/=1) .or. (i2(5)/=1) )
         read (ifich,9050,iostat=ier) (i1(j),i2(j),vallue(j),j=1,5)
         if ( ier < 0 ) then
            call MSP_signaler_message (cle_mes="CPS_FIN_FICHIER"&
                 ,partie_variable='"lors de la lecture des termes tesseraux clmb"', &
                 routine="cps_lirePotentielMSDON")
            return
         endif
         if ( ier > 0 ) then
            call MSP_signaler_message (cle_mes="CPS_ERR_READ",&
                 partie_variable='"des termes tesseraux clmb"', &
                 routine="cps_lirePotentielMSDON")
            return
         endif
	ordremax=max(maxval(i2)-1,ordremax)
         do j = 1,5
            if ( i1(j)>1 ) then
               clmb (i1(j),i2(j)-1) = vallue (j)
            endif
         enddo
      enddo

      ! COEFFICIENTS SLM BARRE  (SLMB)
      i1(5)=0
      i2(5)=0
	do while ( (i1(5)/=1) .or. (i2(5)/=1))
        read (ifich,9050,iostat=ier) (i1(j),i2(j),vallue(j),j=1,5)
         if ( ier < 0 ) then
            call MSP_signaler_message (cle_mes="CPS_FIN_FICHIER", &
                 partie_variable='"lors de la lecture des termes tesseraux slmb"', &
                 routine="cps_lirePotentielMSDON")
            return
         endif
         if ( ier > 0 ) then
            call MSP_signaler_message (cle_mes="CPS_ERR_READ", &
                 partie_variable='"des termes termes tesseraux slmb"', &
                 routine="cps_lirePotentielMSDON")
            return
         endif
         ordremax=max(maxval(i2)-1,ordremax)
         do j = 1,5
            if ( i1(j)>1 ) then
               slmb (i1(j),i2(j)-1) = vallue (j)
            endif
         enddo
      enddo

    ! fermeture du fichier
    close(ifich, iostat=ier)
    if (ier/=0) then
       call MSP_signaler_message(cle_mes="CPS_ERR_CLOSE",&
            routine="cps_lirePotentielMSDON")
       return
    end if

      return

 9050 format (5(2i3,f10.5))

 1000 continue
      call MSP_signaler_message (cle_mes="CPS_FIN_FICHIER",routine="MSP_e_lecpot")

      return

 2000 continue
      call MSP_signaler_message (cle_mes="CPS_ERR_READ",routine="MSP_e_lecpot")

      return

  end subroutine cps_lirePotentielMSDON




  subroutine cps_ecrirePotentielGRGS(fichierGRGS,code,zb,clmb,slmb,mu,requa,apla,degmax,ordremax,fichierMSDON)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ecrirePotentielGRGS
!
!$Resume
! Routine d'écriture d'un fichier potentiel au format GRGS
!
!$Description
! Routine d'ecriture d'un fichier potentiel au format GRGS (a partir du format MSDON)
!
!$Auteur
! mle
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_ecrirePotentielGRGS(fichierGRGS,code,zb,clmb,slmb,mu,requa,apla,degmax,ordremax,fichierMSDON)
!.    character(LEN=*) :: fichierGRGS 
!.    integer :: code
!.    real(KIND=PM_REEL), dimension(:), pointer :: zb
!.    real(KIND=PM_REEL), dimension(:,:), pointer :: clmb, slmb
!.    real(KIND=PM_REEL) :: mu,requa,apla
!.    integer :: degmax, ordremax
!.    character(LEN=*) :: fichierMSDON
!
!$Arguments
!>E     fichierGRGS   :<LEN=*>                       nom du fichier de potentiel au format GRGS
!>E     code          :<integer>                     code du corps concerné
!>E/S   zb            :<PM_REEL,DIM=(:),pointer>     coefficient zonaux
!>E/S   clmb          :<PM_REEL,DIM=(:,:),pointer>   coefficient tesseral en cosinus
!>E/S   slmb          :<PM_REEL,DIM=(:,:),pointer>   coefficient tesseral en sinus
!>E     mu            :<PM_REEL>                     mu (m**3.sec-2)
!>E     requa         :<PM_REEL>                     rayon équatorial (m)
!>E     apla          :<PM_REEL>                     1/aplatissement
!>E     degmax        :<integer>                     degré maximal des coefficients présents dans le fichier
!>E     ordremax      :<integer>                     ordre maximal des coefficients présents dans le fichier
!>E     fichierMSDON  :<LEN=*>                       nom du fichier de potentiel au format MSDON
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use eph_util
    use cps_constantes

    implicit none

    !arguments
    character(LEN=*),intent(IN) :: fichierGRGS        
    integer, intent(IN) :: code
    real(KIND=PM_REEL), dimension(:), pointer :: zb
    real(KIND=PM_REEL), dimension(:,:), pointer :: clmb, slmb
    real(KIND=PM_REEL), intent(IN) :: mu,requa,apla
    integer, intent(IN) :: degmax, ordremax
    character(LEN=*), intent(IN) :: fichierMSDON

    !variables locales
    integer :: ios, ll, mm, unit_grgs
    character(LEN=250) :: rep, modele
    real(KIND=pm_reel) :: dateref, sigmas_calib_factor,sigma_c, sigma_s, slmb0, &
                          vrot
    integer :: compare, compare2   ! Code de retour de cpsi_compare
    character :: car
    character(LEN=2) :: lib 
    character(LEN=10) :: unite

    !initialisation
    dateref=0._pm_reel
    sigmas_calib_factor=0._pm_reel
    car=" "
    sigma_c=0._pm_reel
    sigma_s=0._pm_reel
    lib="00"
    slmb0=0._pm_reel
    ! unite logique pour la lecture du fichier
    call eph_util_ficunit90(unit_grgs,ios)
    if (ios/=0) then
	! erreur : aucune unite logique libre n'a ete trouvee
	call MSP_signaler_message(cle_mes="CPS_ERR_UNIT",&
            routine="cps_ecrirePotentielGRGS", &
            partie_variable=trim(fichierGRGS))
       return
    end if
    ! calcul de la vitesse de rotation a partir du code du corps
    ios = cps_getCsteThCourante(code, "vrot", vrot, unite)
    if (ios/=CPS_OK) then
	! erreur : la vitesse de rotation n'a pas été trouvée
	call MSP_signaler_message(cle_mes="CPS_ERR_INFO",&
            routine="cps_ecrirePotentielGRGS", &
            partie_variable="vitesse de rotation")
	vrot = 0._pm_reel
    endif	

    ! détermination du nom du modèle  == nomdufichier
    call cpsi_getRepAndNameFic(trim(fichierMSDON), rep, modele)


    !ECRITURE DU FICHIER

    ! ouvrir le fichier
    open (unit=unit_grgs, file=fichierGRGS, status="replace", &
         access="sequential", iostat=ios)
    if (ios/=0) then
       call MSP_signaler_message(cle_mes="CPS_ERR_OPEN",&
            routine="cps_ecrirePotentielGRGS", &
            partie_variable=trim(fichierGRGS))
       return
    end if

    ! écriture de l'entête du fichier
    call cpsi_ecrireEnteteGRGS(unit_grgs, modele, fichierGRGS,    &
       requa, apla, mu, vrot, dateref, degmax, sigmas_calib_factor)
    
    ! ecriture des coefficients normalises
    ! un facteur 10-6 apparait pour passer du format MSDON au format GRGS
    ! coefficients zonaux
    mm=0
    do ll = 1,degmax
       compare = cpsi_compareReels( zb(ll), 0._pm_reel)
       if (compare /= 0) then
         write(unit_grgs, 1007, iostat=ios) ll, mm, car, zb(ll)*1e-6_pm_reel, slmb0, &
              sigma_c, sigma_s, lib
       endif
       if (ios>0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_WRITE", &
               routine="cps_ecrirePotentielGRGS", &
               partie_variable=trim(fichierGRGS))
          return
       end if
    enddo	
    ! coefficients tesseraux
    do mm= 1, ordremax
       do ll = 1, degmax
       
       compare = cpsi_compareReels( clmb(ll,mm), 0._pm_reel)
       compare2 = cpsi_compareReels( slmb(ll,mm), 0._pm_reel)
       if ( (compare /= 0) .or. (compare2 /= 0 ) ) then
	  write(unit_grgs, 1007, iostat=ios) ll, mm, car,clmb(ll,mm)*1e-6_pm_reel, &
              slmb(ll,mm)*1e-6_pm_reel, sigma_c, sigma_s, lib
       endif
       if (ios>0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_WRITE", &
               routine="cps_ecrirePotentielGRGS", &
               partie_variable=trim(fichierGRGS))
          return
       end if
      enddo
    enddo

    ! fermer le fichier
    close(unit_grgs, iostat=ios)
    if (ios/=0) then
       call MSP_signaler_message(cle_mes="CPS_ERR_CLOSE",&
            routine="cps_ecrirePotentielGRGS", &
            partie_variable=trim(fichierGRGS))
       return
    end if

    ! declaration des formats
    1007 format (I3, I3, a3, 2E21.14, 2E13.6, a3)



  end subroutine cps_ecrirePotentielGRGS


  subroutine cpsi_ecrireEnteteGRGS(unit_grgs, modele, fichierGRGS,   &
       requa, apla, mu, vrot, dateref, degmax, sigmas_calib_factor)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_ecrireEnteteGRGS
!
!$Resume
!  Routine interne qui écrit l'entête d'un fichier au format du GRGS.
!
!$Description
!  Routine interne qui écrit l'entête d'un fichier au format du GRGS.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_ecrireEnteteGRGS(unit_grgs, modele, fichierGRGS,   &
!.           requa, apla, mu, vrot, dateref, degmax, sigmas_calib_factor)
!.    integer :: unit_grgs, degmax
!.    character(LEN=*) :: modele, fichierGRGS
!.    real(KIND=PM_REEL) :: requa, apla, mu, vrot, dateref, sigmas_calib_factor
!
!$Arguments
!>E     unit_grgs            :<integer>   unité logique sur le fichier en écriture
!>E     modele               :<LEN=*>     nom du modèle de potentiels
!>E     fichierGRGS          :<LEN=*>     nom du fichier à écrire
!>E     requa                :<PM_REEL>   rayon équatorial
!>E     apla                 :<PM_REEL>   aplatissement
!>E     mu                   :<PM_REEL>   constante d'attraction
!>E     vrot                 :<PM_REEL>   vitesse de rotation
!>E     dateref              :<PM_REEL>   date de référence du modèle
!>E     degmax               :<integer>   degré maximal du modèle
!>E     sigmas_calib_factor  :<PM_REEL>   ???
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    implicit none
    
    ! arguments
    integer, intent(in) :: unit_grgs, degmax
    character(LEN=*), intent(in) :: modele, fichierGRGS
    real(KIND=PM_REEL), intent(in) :: requa, apla, mu, vrot, dateref, sigmas_calib_factor
    
    ! variables locales
    integer :: ios

    ! ecriture des lignes 1 et 2
    write(unit_grgs,*, iostat=ios) "FIELD - ", trim(modele)
    if (ios/=0) then
       call MSP_signaler_message(cle_mes="CPS_ERR_WRITE", routine="cps_ecrirePotentielGRGS", &
               partie_variable=trim(fichierGRGS))
       return
    end if
    write(unit_grgs, 1001, iostat=ios) "AE","1/F","GM","OMEGA"
    if (ios/=0) then
       call MSP_signaler_message(cle_mes="CPS_ERR_WRITE", routine="cps_ecrirePotentielGRGS", &
               partie_variable=trim(fichierGRGS))
       return
    end if
   
    ! ecriture de la ligne 3 : REQUA, 1/APLA, MU, VROT
    write(unit_grgs, 1003, iostat=ios) requa, apla, mu, vrot
    if (ios/=0) then
       call MSP_signaler_message(cle_mes="CPS_ERR_WRITE", routine="cps_ecrirePotentielGRGS", &
            partie_variable=trim(fichierGRGS))
       return
    end if

    ! ecriture de la ligne 4 : DATEREF (pas définie pour un fichier au format MSDON)
    write(unit_grgs, 1004, iostat=ios) "REFERENCE DATE :", dateref
    if (ios/=0) then
       call MSP_signaler_message(cle_mes="CPS_ERR_WRITE", routine="cps_ecrirePotentielGRGS", &
            partie_variable=trim(fichierGRGS))
       return
    end if
    
    ! ecriture de la ligne 5 : DEGMAX, SIGMAS_CALIB_FACTOR
    write(unit_grgs, 1005, iostat=ios) "MAXIMAL DEGREE :", degmax, "Sigmas calibration factor :", &
            sigmas_calib_factor, "(not applied)"
    if (ios/=0) then
       call MSP_signaler_message(cle_mes="CPS_ERR_WRITE", routine="cps_ecrirePotentielGRGS", &
            partie_variable=trim(fichierGRGS))
       return
    end if
    ! ecriture de la ligne 6
    write(unit_grgs, 1000, iostat=ios)" L  M DOT         CBAR                SBAR             SIGMA C      SIGMA S  LIB"
    
    
1000 format (a80)
1001 format (9x, a2, 18x, a3, 17x, a2, 17x, a5)
1003 format (4E20.14)
1004 format (a16, 1x, F20.3)
1005 format (a16, 1x, I3, a32, 1x, F9.4, a18)

  end subroutine cpsi_ecrireEnteteGRGS


  subroutine cps_comparePotentiels( C1, S1, mu1, requa1, apla1, vrot1, degmax1, ordremax1,&
       C2, S2, requa2, apla2, mu2, vrot2, degmax2, ordremax2, fich1, fich2)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_comparePotentiels
!
!$Resume
! Routine de comparaison de deux potentiels
!
!$Description
! Routine de comparaison de deux potentiel
!
!$Auteur
! mle
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_comparePotentiels( C1, S1, mu1, requa1, apla1, vrot1, degmax1, ordremax1,&
!.           C2, S2, requa2, apla2, mu2, vrot2, degmax2, ordremax2, fich1, fich2)
!.    real(KIND=PM_REEL), dimension(:,:), pointer :: C1, S1, C2, S2
!.    real(KIND=PM_REEL) :: mu1, requa1, apla1, vrot1, requa2, apla2, mu2, vrot2
!.    integer :: degmax1, ordremax1, degmax2, ordremax2
!.    character(LEN=*) :: fich1, fich2
!
!$Arguments
!>E/S   C1         :<PM_REEL,DIM=(:,:),pointer>   coefficient tesseral en cosinus du premier potentiel
!>E/S   S1         :<PM_REEL,DIM=(:,:),pointer>   coefficient tesseral en sinus du premier potentiel
!>E     mu1        :<PM_REEL>                     mu (m**3.sec-2) du premier potentiel
!>E     requa1     :<PM_REEL>                     rayon équatorial (m) du premier potentiel
!>E     apla1      :<PM_REEL>                     1/aplatissement du premier potentiel
!>E     vrot1      :<PM_REEL>                     
!>E     degmax1    :<integer>                     degré maximal des coefficients pour le premier potentiel
!>E     ordremax1  :<integer>                     ordre maximal des coefficients pour le premier potentiel
!>E/S   C2         :<PM_REEL,DIM=(:,:),pointer>   coefficient tesseral en cosinus du deuxieme potentiel
!>E/S   S2         :<PM_REEL,DIM=(:,:),pointer>   coefficient tesseral en sinus du deuxieme potentiel
!>E     requa2     :<PM_REEL>                     rayon équatorial (m) du deuxieme potentiel
!>E     apla2      :<PM_REEL>                     1/aplatissement du deuxieme potentiel
!>E     mu2        :<PM_REEL>                     mu (m**3.sec-2) du deuxieme potentiel
!>E     vrot2      :<PM_REEL>                     
!>E     degmax2    :<integer>                     degré maximal des coefficients pour le deuxieme potentiel
!>E     ordremax2  :<integer>                     ordre maximal des coefficients pour le deuxieme potentiel
!>E     fich1      :<LEN=*>                       nom du fichier correspondant aux données 1
!>E     fich2      :<LEN=*>                       nom du fichier correspondant aux données 2
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use cps_constantes

    implicit none

    !arguments
    real(KIND=PM_REEL), dimension(:,:), pointer :: C1, S1, C2, S2
    real(KIND=PM_REEL), intent(IN) :: mu1, requa1, apla1, vrot1, requa2, apla2, mu2, vrot2
    integer, intent(IN) :: degmax1, ordremax1, degmax2, ordremax2
    character(LEN=*),intent(IN) :: fich1, fich2

    !variables locales
    integer :: degre, ordre, compare, ll, mm
    real(KIND = pm_reel) :: errR
    character(LEN=50) :: fic1, fic2, rep1, rep2

    !initialisation
    degre = degmax1
    ordre = ordremax1
    errR = 0._pm_reel
    !on recupere les noms des fichiers au lieu des chemins entiers pour les ecrire
    call cpsi_getRepAndNameFic(trim(fich1), rep1, fic1)
    call cpsi_getRepAndNameFic(trim(fich2), rep2, fic2)

    ! comparaison des éléments d'entête
    call cpsi_compareEntetePotentiels(fic1, fic2, requa1,requa2,       &
         mu1, mu2, apla1, apla2, vrot1, vrot2, degmax1, degmax2,       &
         ordremax1, ordremax2)

    !COMPARAISON DES COEFFICIENTS :
    
    !les coefficients clm et slm
    do mm=0, ordre
	do ll=1, degre
		compare = cpsi_compareReels( S1(ll,mm),S2(ll,mm))
        	if (compare/=0) then
		   compare = cpsi_compareReels( 0._pm_reel,S2(ll,mm))
                   if (compare/=0 ) then
                      errR = abs((S2(ll,mm)-S1(ll,mm))/ S2(ll,mm))
                      write(*,1004)"Premier coefficient différent S l=",ll," m=",mm,S1(ll,mm), fic1 ,&
                           S2(ll,mm),  fic2 ,"err. relative ",errR 
                   else
                      write(*,1007)"Premier coefficient différent S l=",ll," m=",mm,S1(ll,mm), fic1 ,&
                           S2(ll,mm),  fic2 
                   endif
                   return
		endif	
        	compare = cpsi_compareReels( C1(ll,mm),C2(ll,mm))
        	if (compare/=0) then
		   compare = cpsi_compareReels( 0._pm_reel,C2(ll,mm))
                   if (compare/=0 ) then
                      errR = abs((C2(ll,mm)-C1(ll,mm))/ C2(ll,mm))
                      write(*,1004)"Premier coefficient différent C l=",ll," m=",mm,C1(ll,mm), fic1 ,&
                           C2(ll,mm),  fic2 ,"err. relative ",errR 
                   else
                      write(*,1007)"Premier coefficient différent C l=",ll," m=",mm,C1(ll,mm), fic1 ,&
                           C2(ll,mm),  fic2 
                   endif
                   return
		endif
             enddo
          enddo

   write(*,*) "Pas de différence entre les potentiels des deux fichiers"
   write(*,*) " jusqu'à l'ordre et au degré les plus faibles donnés "
    
    1004 format (a34, I3, a3, I3, 5x, E20.14, 1x, a18, E20.14, 1x, a18, a14, E20.14  )
    1007 format (a34, I3, a3, I3, 5x, E20.14, 1x, a18, E20.14, 1x, a18  )
  end subroutine cps_comparePotentiels


  subroutine cpsi_compareEntetePotentiels(fic1, fic2, requa1,requa2,   &
       mu1, mu2, apla1, apla2, vrot1, vrot2, degmax1, degmax2,         &
       ordremax1, ordremax2)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_compareEntetePotentiels
!
!$Resume
!  Comparaison des entêe des fichiers potentiels
!$Description
!  Comparaison des entêe des fichiers potentiels
!  - mu
!  - rayon équatorial
!  - aplatissement
!  - vitesse de rotation
!  - degré max
!  - ordre max
! et écriture écran de la veleur commune, sinon des valeurs et du 
! min (valeurs entières) ou de l'écart relatif (valeurs réelles)
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_compareEntetePotentiels(fic1, fic2, requa1,requa2,   &
!.           mu1, mu2, apla1, apla2, vrot1, vrot2, degmax1, degmax2,         &
!.           ordremax1, ordremax2)
!.    character(LEN=*) :: fic1, fic2
!.    real(KIND=PM_REEL) :: requa1, requa2, mu1, mu2
!.    real(KIND=PM_REEL) :: apla1, apla2, vrot1, vrot2
!.    integer :: degmax1, degmax2, ordremax1, ordremax2
!
!$Arguments
!>E     fic1       :<LEN=*>     fichier 1
!>E     fic2       :<LEN=*>     fichier 2
!>E     requa1     :<PM_REEL>   rayon équatorial de fic1
!>E     requa2     :<PM_REEL>   rayon équatorial de fic1
!>E     mu1        :<PM_REEL>   mu de fic1
!>E     mu2        :<PM_REEL>   mu de fic1
!>E     apla1      :<PM_REEL>   aplatissement de fic1
!>E     apla2      :<PM_REEL>   aplatissement de fic1
!>E     vrot1      :<PM_REEL>   vitesse de rotation de fic1
!>E     vrot2      :<PM_REEL>   vitesse de rotation de fic1
!>E     degmax1    :<integer>   degré max. de fic1 
!>E     degmax2    :<integer>   degré max. de fic1 
!>E     ordremax1  :<integer>   ordre max. de fic1 
!>E     ordremax2  :<integer>   ordre max. de fic1
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none
    
    ! arguments
    character(LEN=*), intent(in) :: fic1, fic2
    real(KIND=PM_REEL), intent(in) :: requa1, requa2, mu1, mu2
    real(KIND=PM_REEL), intent(in) :: apla1, apla2, vrot1, vrot2
    integer, intent(in) :: degmax1, degmax2, ordremax1, ordremax2

    ! variables locales
    integer :: compare, ordre, degre
    real(KIND=PM_REEL) :: errR

    !rayon equatorial
    compare = cpsi_compareReels(requa1,requa2)
    if (compare == 0) then
	write(*,1000)"Rayon équatorial : ", requa1
    else
       compare = cpsi_compareReels(0._pm_reel, requa2)
       if ( compare /= 0 ) then
          errR = abs((requa2-requa1)/requa2)
          write(*,1001)"Rayon équatorial : ", requa1,  fic1 , requa2,  fic2,"err. relative ",errR 
    
       else
          write(*,1006)"Rayon équatorial : ", requa1,  fic1 , requa2,  fic2,"err. relative : indisponible" 
       endif
    endif
    !mu
    compare = cpsi_compareReels(mu1, mu2)
    if (compare == 0) then
	write(*,1000)"Mu : ", mu1
    else
       compare = cpsi_compareReels(0._pm_reel, mu2)
       if ( compare /= 0 ) then
          errR = abs((mu2-mu1)/mu2)
          write(*,1001)"Mu : ", mu1,  fic1 , mu2,  fic2 ,"err. relative ",errR
       else
          write(*,1006)"Mu : ", mu1,  fic1 , mu2,  fic2 ,"err. relative : indisponible"
       endif
    endif
    !aplatissement
    compare = cpsi_compareReels(apla1,apla2)
    if (compare == 0) then
	write(*,1000)"Aplatissement : ", apla1
    else
       compare = cpsi_compareReels(0._pm_reel, apla2)
       if ( compare /= 0 ) then
          errR = abs((apla2-apla1)/apla2 )
          write(*,1001)"Aplatissement : ", apla1,  fic1 , apla2,  fic2 ,"err. relative ",errR
       else
          write(*,1006)"Aplatissement : ", apla1,  fic1 , apla2,  fic2 ,"err. relative : indisponible"
       endif
    endif
    !vitesse de rotation
    compare = cpsi_compareReels(vrot1,vrot2)
    if (compare == 0) then
	write(*,1000)"Vitesse de rotation : ", vrot1
    else
       compare = cpsi_compareReels(0._pm_reel, vrot2)
       if ( compare /= 0 ) then
          errR = abs((vrot2-vrot1)/vrot2) 
          write(*,1001)"Vitesse de rotation : ", vrot1,  fic1 , vrot2,  fic2 ,"err. relative ",errR
       else
          write(*,1005)"Vitesse de rotation : ", vrot1,  fic1 , " inconnue ",  fic2 ,"err. relative : indisponible"
       endif
    endif
    !degre maximal
    if (degmax1 == degmax2) then
	write(*,1002)"Degré maximal : ", degmax1
    else
        write(*,1003)"Degré maximal : ", degmax1,  fic1 , degmax2, fic2
	degre = min(degmax1, degmax2)
    endif
    !ordre maximal
    if (ordremax1 == ordremax2) then
	write(*,1002)"Ordre maximal : ", ordremax1
    else
        write(*,1003)"Ordre maximal : ", ordremax1,  fic1 , ordremax2,  fic2 
        ordre = min(ordremax1, ordremax2)
    endif
    

    1000 format (a25, E20.14)
    1001 format (a25, E20.14, 1x, a18, E20.14, 1x, a18, a14, E20.14 )
    1002 format (a25, I3)
    1003 format (a25, I3, 1x, a30, I3, 1x, a20)
    1005 format (a25, E20.14, 12x, a18, a10, a18, a28 )
    1006 format (a25, E20.14, 1x, a18, E20.14, 1x, a18, a28 )

  end subroutine cpsi_compareEntetePotentiels

end module ui_potentiel_fct
