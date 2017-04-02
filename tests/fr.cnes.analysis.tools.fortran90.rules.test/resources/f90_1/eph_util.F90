module eph_util

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  eph_util
!
!$Resume
!  Fonctions utilitaires élémentaires
!
!$Description
!  Fonctions utilitaires élémentaires
!
!$Auteur
!
!$Version
!  $Id: eph_util.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: eph_util.F90,v $
!  Revision 1.6  2010/10/21 13:46:20  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.5  2009/02/11 10:10:55  cml
!  DM-ID 960 : Ajout de methodes de conversion jj1950 en jour juliens
!
!  Revision 1.4  2006/05/30 15:21:28  vivaresf
!  Metriques : supression d'un niveau d'imbrication
!  Revision 1.3  2006/03/09 12:03:41  vivaresf
!  FA-ID 500 : trace de la DM 391
!  Revision 1.2  2005/12/08 18:25:05  vivaresf
!  implicit none
!  Revision 1.1.1.1  2005/12/07 07:23:09  vivaresf
!  Refonte de COMPAS
!  Revision 1.7  2005/11/07 15:56:26  bouillaj
!  DM-ID 391 : Amelioration qualite sur la LIBEPHEM
!  Revision 1.6  2005/05/09 14:17:33  vivaresf
!  V2-1, documentation : correction des cartouches
!  Revision 1.5  2005/03/09 09:12:05  vivaresf
!  Correction des cartouches
!  Revision 1.4  2004/12/17 14:58:11  vivaresf
!  Documentation
!  Revision 1.3  2004/05/24 16:34:22  vivaresf
!  codes reperes internes
!  Revision 1.2  2004/05/24 16:29:42  vivaresf
!  eph_util_ficunit90 : initialisation du parametre de sortie a -1 en cas d'erreur
!  codes reperes (code AMLIB) definis en constantes
!  Revision 1.1.1.1  2004/04/02 09:07:24  vivaresf
!  Gestion de configuration locale
!  Revision 1.4  2004/01/13 09:16:26  bremard
!  Mise à jour cartouche
!  Revision 1.1  2004/01/08 12:56:27  bremard
!  Version initiale archivée
!
!$FinHistorique
!
!$Usage
!  use eph_util
!
!$Structure
!
!$Global
!
!>  EPHREP_gv1950       : <integer,parameter>  
!>  EPHREP_gv2000       : <integer,parameter>  
!>  EPHREP_gvd          : <integer,parameter>  
!>  EPHREP_mer_gm1950   : <integer,parameter>  
!>  EPHREP_ven_gm1950   : <integer,parameter>  
!>  EPHREP_gm1950       : <integer,parameter>  
!>  EPHREP_mar_gm1950   : <integer,parameter>  
!>  EPHREP_jup_gm1950   : <integer,parameter>  
!>  EPHREP_sat_gm1950   : <integer,parameter>  
!>  EPHREP_ura_gm1950   : <integer,parameter>  
!>  EPHREP_nep_gm1950   : <integer,parameter>  
!>  EPHREP_plu_gm1950   : <integer,parameter>  
!>  EPHREP_mer_gm2000   : <integer,parameter>  
!>  EPHREP_ven_gm2000   : <integer,parameter>  
!>  EPHREP_gm2000       : <integer,parameter>  
!>  EPHREP_mar_gm2000   : <integer,parameter>  
!>  EPHREP_jup_gm2000   : <integer,parameter>  
!>  EPHREP_sat_gm2000   : <integer,parameter>  
!>  EPHREP_ura_gm2000   : <integer,parameter>  
!>  EPHREP_nep_gm2000   : <integer,parameter>  
!>  EPHREP_plu_gm2000   : <integer,parameter>  
!>  EPHREP_mer_gmd      : <integer,parameter>  
!>  EPHREP_ven_gmd      : <integer,parameter>  
!>  EPHREP_gmd          : <integer,parameter>  
!>  EPHREP_mar_gmd      : <integer,parameter>  
!>  EPHREP_jup_gmd      : <integer,parameter>  
!>  EPHREP_sat_gmd      : <integer,parameter>  
!>  EPHREP_ura_gmd      : <integer,parameter>  
!>  EPHREP_nep_gmd      : <integer,parameter>  
!>  EPHREP_plu_gmd      : <integer,parameter>  
!>  EPHREP_ecl1950      : <integer,parameter>  
!>  EPHREP_ecl2000      : <integer,parameter>  
!>  EPHREP_ecld         : <integer,parameter>  
!>  EPHREP_de118        : <integer,parameter>  
!>  EPHREP_de200        : <integer,parameter>  
!>  EPHREP_veis         : <integer,parameter>  
!>  EPHREP_veis1950     : <integer,parameter>  
!>  EPHREP_mer_pld      : <integer,parameter>  
!>  EPHREP_ven_pld      : <integer,parameter>  
!>  EPHREP_pld          : <integer,parameter>  
!>  EPHREP_mar_pld      : <integer,parameter>  
!>  EPHREP_jup_pld      : <integer,parameter>  
!>  EPHREP_sat_pld      : <integer,parameter>  
!>  EPHREP_ura_pld      : <integer,parameter>  
!>  EPHREP_nep_pld      : <integer,parameter>  
!>  EPHREP_plu_pld      : <integer,parameter>  
!>  EPHREP_mer_PQ       : <integer,parameter>  
!>  EPHREP_ven_PQ       : <integer,parameter>  
!>  EPHREP_ter_PQ       : <integer,parameter>  
!>  EPHREP_mar_PQ       : <integer,parameter>  
!>  EPHREP_jup_PQ       : <integer,parameter>  
!>  EPHREP_sat_PQ       : <integer,parameter>  
!>  EPHREP_ura_PQ       : <integer,parameter>  
!>  EPHREP_nep_PQ       : <integer,parameter>  
!>  EPHREP_plu_PQ       : <integer,parameter>  
!>  EPHREP_g50_cne      : <integer,parameter>  
!>  EPHREP_station      : <integer,parameter>  
!>  EPHREP_rampe        : <integer,parameter>  
!>  EPHREP_tiv          : <integer,parameter>  
!$Common
!
!$Routines
!- eph_util_ficunit90
!- eph_conv_minusc
!- eph_cdatejj50
!- eph_jjfrac1950_jjuliens
!
!$Fonctions
!- eph_decode_mois
!
!$Include
!
!$Module
!#V
!- msp_gestion_erreur
!- mslib
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
!.  eph_decode_mois eph_util_ficunit90 eph_conv_minusc eph_cdatejj50 eph_jjfrac1950_jjuliens
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use msp_gestion_erreur
  use mslib

  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: eph_util.F90 69 2012-09-11 08:33:34Z ffsm $'


  ! codes reperes AMLIB (a remplacer par les reperes MSLIB/MSPRO)
  integer, parameter :: EPHREP_gv1950= 113
  integer, parameter :: EPHREP_gv2000= 123
  integer, parameter :: EPHREP_gvd= 133
  integer, parameter :: EPHREP_mer_gm1950= 211
  integer, parameter :: EPHREP_ven_gm1950= 212
  integer, parameter :: EPHREP_gm1950= 213
  integer, parameter :: EPHREP_mar_gm1950= 214
  integer, parameter :: EPHREP_jup_gm1950= 215
  integer, parameter :: EPHREP_sat_gm1950= 216
  integer, parameter :: EPHREP_ura_gm1950= 217
  integer, parameter :: EPHREP_nep_gm1950= 218
  integer, parameter :: EPHREP_plu_gm1950= 219
  integer, parameter :: EPHREP_mer_gm2000= 221
  integer, parameter :: EPHREP_ven_gm2000= 222
  integer, parameter :: EPHREP_gm2000= 223
  integer, parameter :: EPHREP_mar_gm2000= 224
  integer, parameter :: EPHREP_jup_gm2000= 225
  integer, parameter :: EPHREP_sat_gm2000= 226
  integer, parameter :: EPHREP_ura_gm2000= 227
  integer, parameter :: EPHREP_nep_gm2000= 228
  integer, parameter :: EPHREP_plu_gm2000= 229
  integer, parameter :: EPHREP_mer_gmd= 231
  integer, parameter :: EPHREP_ven_gmd= 232
  integer, parameter :: EPHREP_gmd= 233
  integer, parameter :: EPHREP_mar_gmd= 234
  integer, parameter :: EPHREP_jup_gmd= 235
  integer, parameter :: EPHREP_sat_gmd= 236
  integer, parameter :: EPHREP_ura_gmd= 237
  integer, parameter :: EPHREP_nep_gmd= 238
  integer, parameter :: EPHREP_plu_gmd= 239
  integer, parameter :: EPHREP_ecl1950= 313
  integer, parameter :: EPHREP_ecl2000=323
  integer, parameter :: EPHREP_ecld= 333
  integer, parameter :: EPHREP_de118= 400
  integer, parameter :: EPHREP_de200= 500
  integer, parameter :: EPHREP_veis= 600
  integer, parameter :: EPHREP_veis1950= 610
  integer, parameter :: EPHREP_mer_pld= 731
  integer, parameter :: EPHREP_ven_pld= 732
  integer, parameter :: EPHREP_pld= 733
  integer, parameter :: EPHREP_mar_pld= 734
  integer, parameter :: EPHREP_jup_pld= 735
  integer, parameter :: EPHREP_sat_pld= 736
  integer, parameter :: EPHREP_ura_pld= 737
  integer, parameter :: EPHREP_nep_pld= 738
  integer, parameter :: EPHREP_plu_pld= 739
  integer, parameter :: EPHREP_mer_PQ= 801
  integer, parameter :: EPHREP_ven_PQ= 802
  integer, parameter :: EPHREP_ter_PQ= 803
  integer, parameter :: EPHREP_mar_PQ= 804
  integer, parameter :: EPHREP_jup_PQ= 805
  integer, parameter :: EPHREP_sat_PQ= 806
  integer, parameter :: EPHREP_ura_PQ= 807
  integer, parameter :: EPHREP_nep_PQ= 808
  integer, parameter :: EPHREP_plu_PQ= 809
  integer, parameter :: EPHREP_g50_cne= 903
  integer, parameter :: EPHREP_station= 1000
  integer, parameter :: EPHREP_rampe= 1100
  integer, parameter :: EPHREP_tiv=1500
  
contains

  subroutine eph_util_ficunit90(numf,ier) 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_util_ficunit90
!
!$Resume
!  Détermination d'un numéro logique de fichier non ouvert
!
!$Description
!  Détermination d'un numéro logique de fichier non ouvert
!
!$Auteur
!   Philippe Brémard (SchlumbergerSema)
!
!$Arguments
!>E/S   numf  :<integer>   numéro logique fourni
!>S     ier   :<integer>   code d'erreur 
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        implicit none

! Arguments
	integer,intent(inout) :: numf
	integer,intent(out)   :: ier

! Variables locales
        logical :: ouvert, lfn_trouve 
	integer :: num
	integer, parameter :: numax = 99

! Code
	ier = 0
        num = 50
        lfn_trouve = .false.

        do while (num <= numax .and. .not. lfn_trouve) 

           inquire (unit=num,opened=ouvert)
           if (ouvert) then
              num = num + 1
 
!/-------------------------------------------------------------------------------
!/	le numéro logique 5 est réservé à la lecture, le 6 à l'écriture
!/-------------------------------------------------------------------------------
 
              if ( num .eq. 5 ) then
                 num = 7
              endif
           else
              lfn_trouve = .true.
           endif

        enddo


	if (num >  numax) then
	   ier = - 1
           call MSP_signaler_message(cle_mes="EPH_ERR_LFNSUP", &
                     routine="eph_util_ficunit90", &
                     partie_variable="99")     
	   return
	endif

	numf = num

        return 
        end subroutine eph_util_ficunit90

	subroutine  eph_conv_minusc(chaine)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_conv_minusc
!
!$Resume
!  Mise d'une chaine de caractères en minuscules
!
!$Description
!  Mise d'une chaine de caractères en minuscules
!
!$Auteur
!   Philippe Brémard (SchlumbergerSema) 
!    (reprise de AM_util_minusc de l'AMLIB)
!
!$Arguments
!>E/S   chaine  :<LEN=??>   chaine de caractères
!
!$Remarques
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          implicit none

! Arguments
	character(LEN=*),intent(inout) :: chaine
 
! Variables locales
        character :: c
	integer :: i,iamin,iamaj,izmaj
        integer :: lg,ic

! Codes ascii de a et z minuscules et majuscules
	data iamin,iamaj,izmaj /97,65,90/

! Code

!	Longueur utile de la chaine:

	lg = len_trim(chaine)

!	Boucle de transcription des caracteres majuscules 
!	en minuscules:

	do i=1,lg
	   ic = ichar(chaine(i:i))
	   if (ic .ge. iamaj .and. ic .le. izmaj) then
	      c = char(ic - iamaj + iamin)
	      chaine(i:i) = c
	   endif
	end do

	return
	end subroutine eph_conv_minusc

      subroutine eph_cdatejj50(cdate, jj50)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_cdatejj50
!
!$Resume
! Traitement des dates calendaires de différentes formes.
!
!$Description
! Traitement des dates calendaires de différentes formes.
! Exemples : 
!.      dd/mm/yyyy hh:mn:ss.sss
!. ou   dd/mm/yyyy 00h00m00s000
!. ou   dd mmm yyyy 00:00:00:000
!. ou   dd mmmmmmmm yyyy hh mn ss mss
! avec :
!>    dd : jour sur 1 ou 2 positions entières
!>    mm : mois sur 1 ou 2 positions entières ou en texte 
!>    yy : année sur 2 ou 4 positions
!.         (sur 2 positions, si yy < 50, année = 2000 + yy)
!.         (sur 2 positions, si yy >= 50, année = 1900 + yy)
!>    hh : heure sur 1 ou 2 positions entières
!>    mn : minute sur 1 ou 2 positions entières
!>    ss : seconde sur 1 ou 2 positions entières
!>    mss : millieme de seconde sur 1 à 3 positions entières
! Les séparateurs entre champs sont des espaces ou :
!>    - "/" pour les jours, mois, année,
!>    - ":" pour les heure, minute, seconde,
!>    - "." ou "s" pour seconde et millième de seconde,
!>    - "h" pour heure et minute,
!>    - "m" pour minute et seconde.
!
!$Auteur
!  Philippe Brémard (SchlumbergerSema)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call eph_cdatejj50(cdate, jj50)
!.    character(LEN=*) :: cdate
!.    real(KIND=PM_REEL) :: jj50
!
!$Arguments
!>E     cdate  :<LEN=*>     date calendaire (format texte)    
!>S     jj50   :<PM_REEL>   date jj50
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- md_calend_julien
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

    ! Arguments
    character(LEN=*),intent(in)     :: cdate
    real(KIND=PM_REEL), intent(out) :: jj50

    ! Variables locales
    type(tm_code_retour) :: code_retour
    integer :: yy, mm, dd, hh, mn, ss, mss
    real(kind=PM_REEL) :: sec
    integer :: ipos
    character(len=12) :: var, cpv
    integer :: nbiter,i
    integer :: asc_prec, asc_suiv
    logical :: forceblanc

    integer, parameter :: eph_ldatemax = 128
    character(len=eph_ldatemax) :: cdate0
    type(tm_jour_sec) :: msjj50

    ! Initialisations
    yy=0
    mm=0
    dd=0
    hh=0
    mn=0
    ss=0
    mss=0
    sec=0._PM_REEL
    var=""
    msjj50%jour = 0
    msjj50%sec  = 0._PM_REEL
    jj50 = -1._PM_REEL

    ! Remplacer les caractères : "/", ":", "h", "m", "s", "." 
    ! par " " en faisant attention au "m" et "s" des noms de mois. 
    
    cdate0=trim(cdate)
    nbiter=len_trim(cdate0)
    forceblanc = .false.
    do i=1,nbiter

       if (cdate0(i:i)=="~") then
       ! Tous les caractères après ~ sont mis à blanc
          forceblanc=.true.
       endif
          
       if (cdate0(i:i)=="/" .or. cdate0(i:i)==":" .or. &
           cdate0(i:i)=="." .or. cdate0(i:i)=="h" .or. forceblanc ) then
          cdate0(i:i)=" "
       endif

       if (cdate0(i:i)=="m" .or. cdate0(i:i)=="s") then
          if (i > 1 .and. i < nbiter) then
             asc_prec = ichar(cdate0(i-1:i-1))
             asc_suiv = ichar(cdate0(i+1:i+1))
             ! On ne supprime les caractères m et s que si :
             !     - le caractères précédent est un blanc (32) ou un chiffre (48 à 57)
             !  ET - le caractères suivant est un blanc (32) ou un chiffre (48 à 57)
             if ((asc_prec==32 .or. (asc_prec>=48 .and. asc_prec <=57)) .and. &
                 (asc_suiv==32 .or. (asc_suiv>=48 .and. asc_suiv <=57))) then
                cdate0(i:i)=" "
             endif
          endif
       endif

    enddo
    
    ! Supprimer les caractères en début
    do while (cdate0(1:1)==" " .and. len_trim(cdate0) > 0) 
       cdate0=cdate0(2:)
    enddo

    ! Décodage de la date

    !----------------------------
    ! Traitement du jour
    !----------------------------

    ! Position du blanc suivant
    ipos=index(cdate0," ")
    if ( ipos <=1 ) then
       ! Date incomplète: pas de jours
       call MSP_signaler_message(cle_mes="EPH_ERR_DATE_INC", &
                                 partie_variable="date", &
                                 routine="eph_cdatejj50" )
       return       
    endif

    if (ipos >= len_trim(cdate0) .and. len_trim(cdate0) > 0) then
       ! Date incomplète
       call MSP_signaler_message(cle_mes="EPH_ERR_DATE_INC", &
            partie_variable="mois", &
            routine="eph_cdatejj50" )
       return
    endif

    if (ipos ==2 .or. ipos==3) then
       ! Jour sur 1 ou 2 positions (ex: 3 ou 18)
       read(cdate0(1:ipos-1),*) dd
    else
       ! Format de jour erroné
       read(cdate0(1:ipos-1),*) dd
       write(cpv,'(a)') dd
       call MSP_signaler_message(cle_mes="EPH_ERR_DATE_FMT", &
            partie_variable=trim(cpv), &
            routine="eph_cdatejj50")
       return
       
    endif
    cdate0=cdate0(ipos:)

    ! Suppression des blancs intermédiaires    
    do while (cdate0(1:1)==" " .and. len_trim(cdate0) > 0) 
       cdate0=cdate0(2:)
    enddo

    !----------------------------
    ! Traitement du mois
    !----------------------------

    ! Position du blanc suivant
    ipos=index(cdate0," ")
    if ( ipos <=1 ) then
       ! Date incomplète: pas de mois
       call MSP_signaler_message(cle_mes="EPH_ERR_DATE_INC", &
                                 partie_variable="mois", &
                                 routine="eph_cdatejj50" )
       return       
    endif

    if (ipos >= len_trim(cdate0) .and. len_trim(cdate0) > 0) then
       ! Date incomplète: pas d'année
       call MSP_signaler_message(cle_mes="EPH_ERR_DATE_INC", &
            partie_variable="année", &
            routine="eph_cdatejj50" )
       return
    endif

    if (ipos >= 2 ) then
       
       ! Au moins 1 caractère pour le mois
       read(cdate0(1:ipos-1),*) var
       if (ipos == 2) then
          ! Mois sur 1 position (ex: 2)
          read(var,'(i1)') mm
       elseif (ipos == 3) then
          ! Mois sur 2 positions (ex: 02)
          read(var,'(i2)') mm
       else 
          ! Mois sur au moins 3 positions donc en texte (ex: FEB ou février)
          if (mm.eq.0.and.len_trim(var)>=3) then
             mm = eph_decode_mois(var)
             if (mm == 0) then
                ! Format de mois erroné
                call MSP_signaler_message(cle_mes="EPH_ERR_DATE_FMT", &
                     partie_variable=trim(var), &
                     routine="eph_cdatejj50")
                return
             endif
          endif
       endif
    else
       ! Format de mois erroné
       read(cdate0(1:1),*) var
       call MSP_signaler_message(cle_mes="EPH_ERR_DATE_FMT", &
            partie_variable=trim(var), &
            routine="eph_cdatejj50")
       return
       
    endif
    cdate0=cdate0(ipos:)
          
       
    ! Suppression des blancs intermédiaires    
    do while (cdate0(1:1)==" " .and. len_trim(cdate0) > 0) 
       cdate0=cdate0(2:)
    enddo

    !----------------------------
    ! Taitement de l'année
    !----------------------------

    ! Position du blanc suivant
    ipos=index(cdate0," ")
    if ( ipos <=1 ) then
       ! Date incomplète: pas d'année
       call MSP_signaler_message(cle_mes="EPH_ERR_DATE_INC", &
                                 partie_variable="année", &
                                 routine="eph_cdatejj50" )
       return       
    endif

    if (ipos >= 3 ) then
       
       ! Au moins 2 caractères pour l'année
       read(cdate0(1:ipos-1),*) var
       
       if (ipos.eq.5) then
          ! Année sur 4 positions (ex: 2004)
          read(var,'(i4)') yy
       else if (ipos.eq.3) then
          ! Année sur 2 positions (ex: 04)
          read(var,'(i2)') yy
          if (yy < 50) then
             yy = 2000 + yy
          else
             yy = 1900 + yy
          endif
       else
          ! Format année erroné
          call MSP_signaler_message(cle_mes="EPH_ERR_DATE_FMT", &
               partie_variable=trim(var), &
               routine="eph_cdatejj50")
          return
          
       endif
    else
       ! Format de année erroné
       read(cdate0(1:1),*) var
       call MSP_signaler_message(cle_mes="EPH_ERR_DATE_FMT", &
            partie_variable=trim(var),&
            routine="eph_cdatejj50" )
       return 
    endif
    
    cdate0=cdate0(ipos:)
    
 
    ! Suppression des blancs intermédiaires    
    do while (cdate0(1:1)==" " .and. len_trim(cdate0) > 0) 
       cdate0=cdate0(2:)
    enddo


    !----------------------------
    ! Taitement de heure
    !----------------------------

    ! Position du blanc suivant
    ipos=index(cdate0," ")
    if (ipos <= 1) then
       ! Pas de : hh mm ss mss
       hh = 0
       mn = 0
       ss = 0
       mss = 0
       sec = 0._PM_REEL
    else
       if (ipos ==2 .or. ipos==3) then
          ! Heure sur 1 ou 2 positions (ex: 3 ou 18)
          read(cdate0(1:ipos-1),*) hh
       else
          ! Format de heure erroné
          read(cdate0(1:ipos-1),*) hh
          write(cpv,'(a)') hh
          call MSP_signaler_message(cle_mes="EPH_ERR_DATE_FMT", &
                                    partie_variable=trim(cpv), &
                                    routine="eph_cdatejj50")
          return
          
       endif
       cdate0=cdate0(ipos:)
    endif
       

    ! Suppression des blancs intermédiaires    
    do while (cdate0(1:1)==" " .and. len_trim(cdate0) > 0) 
       cdate0=cdate0(2:)
    enddo


    !----------------------------
    ! Taitement de minute
    !----------------------------

    ! Position du blanc suivant
    ipos=index(cdate0," ")
    if (ipos <= 1 ) then
       ! Pas de : mn ss mss
       mn = 0
       ss = 0
       mss = 0
       sec = 0._PM_REEL
    else
       if (ipos ==2 .or. ipos==3) then
          ! minute sur 1 ou 2 positions (ex: 3 ou 18)
          read(cdate0(1:ipos-1),*) mn
       else
          ! Format de minute erroné
          read(cdate0(1:ipos-1),*) mn
          write(cpv,'(a)') mn
          call MSP_signaler_message(cle_mes="EPH_ERR_DATE_FMT", &
                                    partie_variable=trim(cpv), &
                                    routine="eph_cdatejj50")
          return
          
       endif
       cdate0=cdate0(ipos:)
    endif
       
    ! Suppression des blancs intermédiaires    
    do while (cdate0(1:1)==" " .and. len_trim(cdate0) > 0) 
       cdate0=cdate0(2:)
    enddo

    !----------------------------
    ! Taitement de seconde
    !----------------------------

    ! Position du blanc suivant
    ipos=index(cdate0," ")
    if (ipos <= 1) then
       ! Pas de : ss mss  
       ss = 0
       mss = 0
       sec = 0._PM_REEL
    else
       if (ipos ==2 .or. ipos==3) then
          ! seconde sur 1 ou 2 positions (ex: 3 ou 59)
          read(cdate0(1:ipos-1),*) ss
       else
          ! Format de seconde erroné
          read(cdate0(1:ipos-1),*) ss
          write(cpv,'(a)') ss
          call MSP_signaler_message(cle_mes="EPH_ERR_DATE_FMT", &
                                    partie_variable=trim(cpv), &
                                    routine="eph_cdatejj50")
          return
          
       endif
       cdate0=cdate0(ipos:)
    endif
       
    ! Suppression des blancs intermédiaires    
    do while (cdate0(1:1)==" " .and. len_trim(cdate0) > 0) 
       cdate0=cdate0(2:)
    enddo

    !-----------------------------------
    ! Taitement des milliemes de seconde
    !-----------------------------------

    ! Position du blanc suivant
    ipos=index(cdate0," ")

    ! Dernier champs lu
    if (ipos <= 1) then
       ! Pas de :mss  
       mss = 0
    else
       if (ipos>4) ipos=4
       ! millieme de seconde sur au moins 1 positions 
       read(cdate0(1:ipos-1),*) mss
    endif

    !-----------------------------------
    ! Calcul sec réelle à partir des sec 
    ! et milli-sec entières
    !-----------------------------------

    sec = dfloat(ss) + dfloat(mss)/1000._PM_REEL

    !-----------------------------------
    ! Conversion au format tm_jour_sec
    !-----------------------------------

    ! md_calend_julien fait les contrôles de cohérences des paramètres en entrée
    call md_calend_julien(yy, mm, dd, hh, mn, sec, msjj50,code_retour)
    if (code_retour%valeur /= 0) then
       call MSP_signaler_message (ier_mslib=code_retour)   
       return
    endif
    
    ! Conversion msjj50 en réel
    jj50= msjj50%jour + msjj50%sec/86400._PM_REEL
    
  end subroutine eph_cdatejj50

  integer function eph_decode_mois(cmois)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_decode_mois
!
!$Resume
!  Conversion du mois littéral en mois entier
!
!$Description
!  Cette fonction retourne le numéro du mois dans l'année.
!
!$Auteur
!  Philippe Brémard (SchlumbergerSema)
!
!$Acces
!  PUBLIC
!
!$Usage
!.      integer function eph_decode_mois(cmois)
!.    character(LEN=*) :: cmois
!
!$Arguments
!>E     cmois  :<LEN=*>   mois à convertir (format texte)
!
!$Common
!
!$Routines
!- eph_conv_minusc
!
!$Include
!
!$Module
!
!$Remarques
!  Cette fonction accepte les noms anglais et les noms français.
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none 

! Arguments    
    character(LEN=*), intent(in) :: cmois

! Variables locales
    character(LEN=32) :: cmm
    integer :: mm

! Code

    cmm = trim(cmois)

    ! Conversion en minuscule pour faciliter le décodage
    call eph_conv_minusc(cmm)

    mm = 0
    
    select case (trim(cmm))
    case("jan","jv","january","janvier")
       mm=1
    case("feb","fev","february","fevrier","février")
       mm=2
    case("mar","mars")
       mm=3
    case("apr","avr","april","avril")
       mm=4
    case("may","mai")
       mm=5
    case("jun","june","juin")
       mm=6
    case("jul","july","juillet")
       mm=7
    case("aug","august","aout")
       mm=8
    case("sep","september","septembre")
       mm=9
    case("oct","october","octobre")
       mm=10
    case("nov","november","novembre")
       mm=11
    case("dec","december","decembre","décembre")
       mm=12
    case default
       mm=0
    endselect
    
    eph_decode_mois = mm

   end function eph_decode_mois


   subroutine eph_jjfrac1950_jjuliens(t1950, date_jj)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_jjfrac1950_jjuliens
!
!$Resume
!  Routine de conversion d'une date en jour juliens 1950 fractionnaires
!  en jour juliens fractionnaires depuis 4713 av J.C.
!
!$Description
!  Routine de conversion d'une date en jour juliens 1950 fractionnaires
!  en jour juliens fractionnaires depuis 4713 av J.C.
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call eph_jjfrac1950_jjuliens(t1950, date_jj)
!.    real(kind=pm_reel) :: t1950
!.    real(kind=pm_reel) :: date_jj
!
!$Arguments
!>E     t1950    :<pm_reel>   
!>S     date_jj  :<pm_reel>   
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- mslib
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     use mslib

     implicit none

     real(kind=pm_reel), intent(in)  :: t1950
     real(kind=pm_reel), intent(out) :: date_jj

     ! Constante correspondant au 01/01/1950 à midi
     ! exprimé en jours juliens fractionnaires depuis 4713 av JC
     real(kind=pm_reel), parameter :: jj_01_01_1950 = 2433282.5_pm_reel

     date_jj = t1950 + jj_01_01_1950
     
   end subroutine eph_jjfrac1950_jjuliens


   subroutine eph_jjuliens_jjfrac1950(date_jj, t1950 )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_jjuliens_jjfrac1950
!
!$Resume
!  Routine de conversion d'une date en jour juliens fractionnaires 
!  depuis 4713 av J.C. en jour juliens 1950 fractionnaires
!
!$Description
!  Routine de conversion d'une date en jour juliens fractionnaires 
!  depuis 4713 av J.C. en jour juliens 1950 fractionnaires
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call eph_jjuliens_jjfrac1950(date_jj, t1950 )
!.    real(kind=pm_reel) :: date_jj
!.    real(kind=pm_reel) :: t1950
!
!$Arguments  
!>E     date_jj  :<pm_reel> 
!>S     t1950    :<pm_reel>   
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- mslib
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     use mslib

     implicit none

     real(kind=pm_reel), intent(in) :: date_jj
     real(kind=pm_reel), intent(out)  :: t1950

     ! Constante correspondant au 01/01/1950 à midi
     ! exprimé en jours juliens fractionnaires depuis 4713 av JC
     real(kind=pm_reel), parameter :: jj_01_01_1950 = 2433282.5_pm_reel

     t1950 = date_jj - jj_01_01_1950

   end subroutine eph_jjuliens_jjfrac1950

end module eph_util
