module cps_potentiel

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  cps_potentiel
!
!$Resume
! Module contenant la routine de lecture des fichiers de potentiels au
! format du GRGS, ainsi que la fonction calculant le coefficient de
! dénormalisation.
!
!$Description
! Module contenant la routine de lecture des fichiers de potentiels au
! format du GRGS, ainsi que la fonction calculant le coefficient de
! dénormalisation.
!
!$Auteur
! vpg
!
!$Version
!  $Id: cps_potentiel.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: cps_potentiel.F90,v $
!  Revision 1.23  2010/10/21 13:46:22  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.22  2009/09/09 09:17:03  cmartel
!  FA-ID 1264 : Correction des tests d'égalité entre réels
!
!  Revision 1.21  2009/01/05 13:50:08  cml
!  FA-ID 1133 : Suppression des constantes numeriques non nommees
!
!  Revision 1.20  2008/08/04 13:40:59  gss
!  DM-ID 1058 : (portage g95) suppression des variables tampon utilisées dans la
!  lecture de fichier et modification des déclarations de formats utilisés
!  (remplacement des formats des parties non utilisées par des x).
!
!  Revision 1.19  2006/11/20 18:03:50  vivaresf
!  Version 2.1 / DM 462 : fonction de gestion des potentiels dans COMPAS_UI
!
!  Revision 1.18  2006/10/31 09:27:18  vpg
!  Passage understand
!
!  Revision 1.17  2006/10/25 15:30:15  mle
!  DM-ID 462 : amelioration de l'acces a la description du contenu de la base COMPAS
!  Revision 1.16  2006/09/26 07:43:09  mle
!  DM-ID : 462 petites modifications
!  Revision 1.15  2006/09/25 13:43:04  mle
!  DM-ID 462 : nouvelles routines pour comparer les potentiels
!  Revision 1.14  2006/08/30 09:39:33  vivaresf
!  FA-ID 576 : gestion d'erreur
!  Revision 1.13  2006/07/05 09:21:33  vivaresf
!  DM-ID 387 : validation sorties format OBELIX
!  Revision 1.12  2006/06/15 09:58:43  vpg
!  Correction de la routine de conversion au format OBELIX
!  Revision 1.11  2006/06/13 15:50:12  vpg
!  Rajout des tableaux des coefficients en sortie dont le format est comatible OBELIX
!  Revision 1.10  2006/05/12 12:06:04  bouillaj
!  Amelioration qualite : complements sur les cartouches
!  Revision 1.9  2006/05/02 09:38:13  vpg
!  Suppression des variables non utilisees
!  Revision 1.8  2006/04/12 15:48:49  bouillaj
!  Lecture aplatissement et non vitesse lumiere
!  Revision 1.7  2006/03/23 17:36:02  bouillaj
!  Ajout option denormalisation
!  Revision 1.6  2006/03/20 15:54:00  vpg
!  Mise a jour des cartouches
!
!$FinHistorique
!
!$Usage
!  use cps_potentiel
!
!$Structure
!
!$Global
!
!$Common
!
!$Routines
!- cps_lirePotentielGRGS
!- cpsi_convertirFormatObelix
!- cps_lirePotentielMSDON
!- cps_ecrirePotentielGRGS
!- cpsi_ecrireEnteteGRGS
!- cps_comparePotentiels
!
!$Fonctions
!- cpsi_denorm
!
!$Include
!
!$Module
!#V
!- cps_acces
!- eph_util
!- cps_constantes
!#
!
!$Interface
!#V
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!.  cpsi_denorm cps_lirePotentielGRGS cpsi_convertirFormatObelix cps_lirePotentielMSDON cps_ecrirePotentielGRGS
!.  cpsi_ecrireEnteteGRGS cps_comparePotentiels
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use cps_acces
  
  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: cps_potentiel.F90 69 2012-09-11 08:33:34Z ffsm $'

  
contains

  subroutine cps_lirePotentielGRGS(fichier, jdeb, denorm, C, S, requa, apla, &
       mu, vrot, degremax, ordremax, czmax, cz, cs)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_lirePotentielGRGS
!
!$Resume
! Routine de lecture d'un fichier potentiel au format GRGS
!
!$Description
! V2.0
! Routine de lecture d'un fichier potentiel au format GRGS
! (S) PM_REEL, dimension(0:(degremax+1)*(degremax+2)/2-1) : C
!     Coefficients CLM denormalises
!     CLM = C(L*(L+1)/2+M), L=0.. degremax et M=0..L
! (S) PM_REEL, dimension(0:(degremax+1)*(degremax+2)/2-1) : S
!     Coefficients SLM denormalises
!     SLM = S(L*(L+1)/2+M), L=0.. degremax et M=0..L
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_lirePotentielGRGS(fichier, jdeb, denorm, C, S, requa, apla, &
!.           mu, vrot, degremax, ordremax, [czmax], [cz], [cs])
!.    character(LEN=*) :: fichier
!.    real(KIND=PM_REEL) :: jdeb
!.    logical :: denorm
!.    real(KIND=PM_REEL), dimension(:,:), pointer :: C, S
!.    real(KIND=PM_REEL) :: requa, apla, mu, vrot
!.    integer :: degremax, ordremax
!.    integer :: czmax
!.    real(KIND=PM_REEL), dimension(:) :: cz
!.    real(KIND=PM_REEL), dimension(:) :: cs
!
!$Arguments
!>E     fichier   :<LEN=*>                       nom du fichier de potentiel
!>E     jdeb      :<PM_REEL>                     date d'entrée du fichier (MJD)
!>E     denorm    :<logical>                     option de normalisation
!>E/S   C         :<PM_REEL,DIM=(:,:),pointer>   coefficient tesseral en cosinus
!>E/S   S         :<PM_REEL,DIM=(:,:),pointer>   coefficient tesseral en sinus
!>S     requa     :<PM_REEL>                     rayon équatorial (m)
!>S     apla      :<PM_REEL>                     aplatissement
!>S     mu        :<PM_REEL>                     mu (m**3.sec-2)
!>S     vrot      :<PM_REEL>                     vitesse de rotation (rad.sec-1)
!>S     degremax  :<integer>                     degré maximal des coefficients présents dans le fichier
!>S     ordremax  :<integer>                     ordre maximal des coefficients présents dans le fichier
!>[E]   czmax     :<integer>                     taille du tableau cz (format compatible OBELIX)
!>[S]   cz        :<PM_REEL,DIM=(:)>             tableau des coefficients zonaux normalisés (format compatible OBELIX)
!>[S]   cs        :<PM_REEL,DIM=(:)>             tableau des coefficients tesseraux normalisés (format compatible OBELIX)
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cpsi_convertirFormatObelix
!
!$Include
!
!$Module
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
    ! nom complet du fichier de potentiel
    character(LEN=*), intent(in) :: fichier
    ! date d'entree en JJ1950
    real(KIND=PM_REEL), intent(in) :: jdeb
    logical, intent(IN) :: denorm
    real(KIND=PM_REEL), dimension(:,:), pointer :: C, S
    real(KIND=PM_REEL), intent(out) :: requa, apla, mu, vrot
    integer, intent(out) :: degremax, ordremax
    integer, optional, intent(in) :: czmax
    real(KIND=PM_REEL), dimension(:), optional, intent(out) :: cz
    real(KIND=PM_REEL), dimension(:), optional, intent(out) :: cs

    ! Constantes numériques
    ! unite logique sur laquel sera ouvert le fichier
    integer, parameter :: file_unit = 11
    integer, parameter :: annee_1950 = 1950
    real(KIND=PM_REEL), parameter :: nb_jours_par_an = 365.25_pm_reel

    ! variables locales
    integer ::ios, L, M, i
    !integer :: nbElts
    real(KIND=PM_REEL) :: dateref, date, cbar, sbar, cmul
    character(LEN=CPS_MAXLG) :: car
    
    ! Initialisations
    date = 0._PM_REEL
    ordremax = 0
    degremax = 0

    ! ouvrir le fichier
    open(file_unit, &
         access='SEQUENTIAL', &
         file=fichier, &
         iostat=ios, &
         status='OLD')
    if (ios.NE.0) then
       call MSP_signaler_message(cle_mes="CPS_ERR_OPEN",&
            routine="cps_lirePotentielGRGS", &
            partie_variable=trim(fichier))
       return
    end if

    ! lecture des lignes 1 et 2
    do i=1, 2
       read(file_unit, 1000, iostat=ios)
       if (ios.NE.0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_READ", &
               routine="cps_lirePotentielGRGS", &
               partie_variable=trim(fichier))
          return
       end if
    end do
   
    ! lecture de la ligne 3 : REQUA, 1/APLA, MU, VROT
    read(file_unit, 1003, iostat=ios) requa, apla, mu, vrot
    if (ios.NE.0) then
       call MSP_signaler_message(cle_mes="CPS_ERR_READ", &
            routine="cps_lirePotentielGRGS", &
            partie_variable=trim(fichier))
       return
    end if
    ! calcul de l'aplatissement
    apla = 1/apla

    ! lecture de la ligne 4 : DATEREF
    read(file_unit, 1004, iostat=ios) dateref
    if (ios.NE.0) then
       call MSP_signaler_message(cle_mes="CPS_ERR_READ", &
            routine="cps_lirePotentielGRGS", &
            partie_variable=trim(fichier))
       return
    end if
    ! Calcul de l'écart à la date en années fractionnaires
    !date = date_julienne(jdeb, 0.) - dateref
    date = (jdeb/nb_jours_par_an) + annee_1950 - dateref

    ! lecture de la ligne 5 : DEGMAX, SIGMAS_CALIB_FACTOR
    read(file_unit, 1005, iostat=ios) degremax
    if (ios.NE.0) then
       call MSP_signaler_message(cle_mes="CPS_ERR_READ", &
            routine="cps_lirePotentielGRGS", &
            partie_variable=trim(fichier))
       return
    end if
    
    ! lecture de la ligne 6
    read(file_unit, 1000, iostat=ios)
    
    ! allocation memoire des tableaux de coefficients
    allocate(C(0:degremax, 0:degremax))
    allocate(S(0:degremax, 0:degremax))
    
    ! initialisation des tableaux de coefficients
    C(:,:) = 0._PM_REEL
    S(:,:) = 0._PM_REEL

    ! lecture des coefficients normalises
    do
       read(file_unit, 1007, iostat=ios) L, M, car, cbar, sbar
       if (ios.lt.0) then
          exit
       else if (ios.gt.0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_READ", &
               routine="cps_lirePotentielGRGS", &
               partie_variable=trim(fichier))
          return
       else
          ! lecture de la ligne correcte
          ! indice courant des vecteurs C et S
          if (car.eq.'DOT') then
             ! gestion des termes variables
             C(L,M) = C(L,M) + cbar*date
             S(L,M) = S(L,M) + sbar*date
          else
             ! gestion des termes courants
             C(L,M) = C(L,M) + cbar
             S(L,M) = S(L,M) + sbar
          end if
          ordremax = max(ordremax, M)
       end if
    end do

    ! initialisation du C00 si cela n'a pas ete fait lors de la lecture du champ
    ! Comparaison de la valeur absolu à l'epsilon machine (<=> comparer C(0,0) à 0.O )
    if (abs(C(0,0)) < epsilon(C(0,0)) ) then
       C(0,0) = 1._PM_REEL
    end if

    ! fermer le fichier
    close(file_unit, iostat=ios)
    if (ios.NE.0) then
       call MSP_signaler_message(cle_mes="CPS_ERR_CLOSE",&
            routine="cps_lirePotentielGRGS", &
            partie_variable=trim(fichier))
       return
    end if

    ! denormalisation des coefficients
    if (denorm) then
       do L=0, degremax
          do M=0, L
             cmul = cpsi_denorm(L,M)
             if (cmul.ne.0) then
                C(L,M) = cmul*C(L,M)
                S(L,M) = cmul*S(L,M)
             end if
          end do
       end do
    endif

    ! 
    if (present(czmax).and.       &
         present(cz).and.         &
         present(cs)) then
       call cpsi_convertirFormatObelix(C,S,czmax, cz, cs)
    end if

    ! declaration des formats
    1000 format (a80)
    1003 format (4F20.14)
    1004 format (17x, F20.18)
    1005 format (17x, I3)
    1007 format (I3, I3, a3, 2E21.14)
    
  end subroutine cps_lirePotentielGRGS

  
  function cpsi_denorm(L, M) result (cmul)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_denorm
!
!$Resume
! V2.0
! Fonction qui calcule le coefficient de denormalisation (cmul)
! des harmoniques CLM et SLM d'un modele de potentiel
!
!$Description
! V2.0
! Fonction qui calcule le coefficient de denormalisation (cmul)
! des harmoniques CLM et SLM d'un modele de potentiel
!            _
! CNM = cmul*CNM
!            _
! SNM = cmul*SNM
! avec cmul = [ ( (L-M)!*(2L+1)!(2-kronecker(0,M)) ) / (L+M)! ]^(1/2)
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  cmul = cpsi_denorm(L, M)
!.    integer :: L, M
!.    real(KIND=PM_REEL) :: cmul
!
!$Arguments
!>E     L     :<integer>   coefficient de lecture dans la table
!>E     M     :<integer>   coefficient de lecture dans la table
!>S     cmul  :<PM_REEL>   coefficient de dénormalisation
!
!$Common
!
!$Routines
!- MSP_signaler_message
!
!$Include
!
!$Module
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
    ! arguments d'entree
    integer, intent(in) :: L, M
    
    ! variables locales
    real(KIND=PM_REEL) :: cmul, num
    integer :: i, lmmpu, lpm

    cmul=0
    if ((L.eq.0).and.(M.eq.0)) then
       cmul = 1
    elseif ((L.gt.0).and.(M.ge.0).and.(M.le.L)) then
       ! cmul = num/rf
       ! avec
       ! num = (2-kronecker(0,M))*(2*L+1) / rf
       ! et
       ! rf  = (L+M)! / (L-M)!
       num = 2*L+1
       if (M.ne.0) then
          num = 2*num
       end if

       ! calcul du rapport des factorielles rf
       ! (L+M)! / (L-M)! = (L+M)*(L+M-1)*...*(L-M+1)
       lmmpu = L-M+1
       lpm = L+M
       cmul = dble(1)/sqrt(num)
       do i=lmmpu, lpm
          cmul = cmul*sqrt(DBLE(i))
       end do
       cmul = 1/cmul
    else
       call MSP_signaler_message(cle_mes="CPS_ERR_DENORM", &
            routine="cps_denorm")
       return
    end if

  end function cpsi_denorm


  subroutine cpsi_convertirFormatObelix(C,S,czmax,cz,cs)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_convertirFormatObelix
!
!$Resume
!  Conversion des tableaux des coefficients au format OBELIX
!
!$Description
!  Cette routine convertie les tableaux de coefficients C et S en tableaux
!  compatibles au format OBELIX.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_convertirFormatObelix(C,S,czmax,cz,cs)
!.    real(KIND=PM_REEL), dimension(0:,0:) :: C,S
!.    integer :: czmax
!.    real(KIND=PM_REEL), dimension(:) :: cz
!.    real(KIND=PM_REEL), dimension(:) :: cs
!
!$Arguments
!>E     C      :<PM_REEL,DIM=(0:,0:)>   tableau des coefficients en cosinus
!>E     S      :<PM_REEL,DIM=(0:,0:)>   tableau des coefficients en sinus
!>E     czmax  :<integer>               taille du tableau cz
!>S     cz     :<PM_REEL,DIM=(:)>       tableau des coefficients zonaux normalisés (format compatible OBELIX)
!>S     cs     :<PM_REEL,DIM=(:)>       tableau des coefficients tesseraux normalisés (format compatible OBELIX)
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
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
    real(KIND=PM_REEL), dimension(0:,0:), intent(in) :: C,S
    integer, intent(in) :: czmax
    real(KIND=PM_REEL), dimension(:), intent(out) :: cz
    real(KIND=PM_REEL), dimension(:), intent(out) :: cs
    
    ! variables locales
    integer :: ll, mm, ind
    
    ! tableau des coefficients zonaux
    do ll=1,czmax
       cz(ll) = C(ll,0)
    end do
    
    ! tableau des coefficients tesseraux
    do mm=1,czmax
       do ll=1,czmax
          ind = 2*( (mm-1)*czmax + ll ) - 1
          cs(ind) = C(ll,mm)
          cs(ind+1) = S(ll,mm)
       end do
    end do
    
  end subroutine cpsi_convertirFormatObelix

end module cps_potentiel
