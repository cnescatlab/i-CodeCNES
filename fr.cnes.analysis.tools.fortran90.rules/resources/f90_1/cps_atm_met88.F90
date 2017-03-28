module cps_atm_met88_mod

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  cps_atm_met88_mod
!
!$Resume
!  Modele d'atmosphere MET88.
!
!$Description
!  Modele d'atmosphere MET88.
!
!$Auteur
!  intégration COMPAS : E. Aitier (ATOS Origin)
!
!*                              Mike Hickey                                   *
!*                Universities Space Research Association                     *
!*                           NASA / MSFC , ED44                               *
!*                           Tel. (205) 544-5692                              *
!
!$Version
! $Id: cps_atm_met88.F90 355 2013-02-14 12:16:41Z aadt $
!$Historique
! $Log: cps_atm_met88.F90,v $
! Revision 355  2013/02/14 aadt
! DM-ID 1513: Suppression des warnings de compilation
!
! Revision 1.13  2010/10/21 13:46:21  ogarat
! VERSION::AQ::21/10/2010:Ajout du fin historique
!
! Revision 1.12  2008/10/14 07:49:37  cml
! DM-ID 1058 : Suppression d une variable inutilisee
!
! Revision 1.11  2008/03/18 16:09:07  vivaresf
! DM-ID 553 : découpage des équations en opérations simples pour améliorer les résultats des portages
!
! Revision 1.10  2008/03/14 13:41:03  vivaresf
! DM-ID 553 : portage Solaris, précision
!
! Revision 1.9  2008/03/13 18:03:19  vivaresf
! DM-ID 553 : portage Solaris 10, précision
!
! Revision 1.8  2007/11/13 16:52:05  sbd
! FA-ID 827 suppression variables inutilisees
!
! Revision 1.7  2007/04/12 11:10:57  vivaresf
! FA-ID 716 : message d'erreur complet
!
! Revision 1.6  2006/11/17 07:01:51  vivaresf
! DM-ID 425 : code plus portable (float en real, alog en log, dsqrt en sqrt)
!
! Revision 1.5  2006/11/15 08:25:06  vivaresf
! DM-ID 425 : mod  au lieu de amod
!
! Revision 1.4  2006/09/12 14:01:58  tanguyy
! DM-ID 428 : Ajout de routines chapeaux pour les modeles MSIS90 / MSIS2000 et MET88
!
!$FinHistorique
!
!$Usage
!  use cps_atm_met88_mod
!
!$Remarques
!   Ce modèle d'atmosphere est un modele Jacchia1970 modifié
!   Les calculs sont menés dans la subroutine J70
!                                                                            
!$Voir-Aussi
!  Module cps_atmosphere : routine chapeau cps_calculer_met88
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use mslib
  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: cps_atm_met88.F90 355 2013-02-14 12:16:41Z aadt $'


  ! Variables globales du module
  !=============================
  real(kind=4),private,parameter :: pi = 3.14159265
  real(kind=4),private,parameter :: tpi = 6.28318531
  real(kind=4),private,parameter :: pi2 = 1.57079633
  real(kind=4),private,parameter :: pi32 = 4.71238898

  integer,dimension(12),private :: iday
	data iday / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /


  private cps_met88_j70
  private cps_met88_j70sup
  private cps_met88_tme
  private cps_met88_tinf
  private cps_met88_jac
  private cps_met88_slv
  private cps_met88_slvh
  private cps_met88_fair5
  private cps_met88_gauss
  private temp
  private gravity
  private mol_wt

  contains

      subroutine cps_atm_met88 (z, xlat, xlng, iyr, mon, ida, ihr, minu, &
           igeo_ind, f10, f10b, gi, outdata, auxdata) 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_atm_met88
!
!$Resume
!   calcule les données atmosphériques (en utilisant les routines J70 et J70SUP)
!
!$Description
!   calcule les données atmosphériques 
!
!$Auteur
! Mike Hickey ( USRA, NASA/ED44 )
!
!$Usage
! call cps_atm_met88 ( indata, outdata, auxdata, switch )
! <E> altitude		= z     ( en km )
! <E> latitude		= xlat	( en radians )
! <E> longitude		= xlng	( en radians )
! <E> year (yy)		= iyr	( en annees )
! <E> month (mm)        = mn	( en mois )
! <E> day (dd)		= ida	( en jours )
! <E> hour (hh)		= ihr	( en heures )
! <E> minutes(mm)	= minu	( en minutes )
! <E> geomagnetic index     = igeo_ind ( sans unite 1:gi=kp, 2:gi=ap)
! <E> solar radio noise flux= f10      ( ? )
! <E> 162-day average f10    = f10b    ( ? )
! <E> geomagnetic activity index = gi=ap ( sans unites )
! NOTE :  All output in MKS units               
! <S> exospheric temperature (K)     
! <S> temperature at altitude Z		
! <S> N2 number density  (per meter-cubed)	
! <S> O2 number density  (      ..       )	
! <S> O  number density  (      ..       )	
! <S> A  number density  (      ..       )	
! <S> He number density  (      ..       )  
! <S> H  number density  (      ..       )	
! <S> average molecular weight              
! <S> total density                         
! <S> log10 ( total density )               
! <S> total pressure ( Pa )                 
!						
!  AUXDATA 
! (1) -- gravitational acceleration ( m/s-s )     
! (2) -- ratio of specific heats                
! (3) -- pressure scale-height ( m )            
! (4) -- specific heat at constant pressure   
! (5) -- specific heat at constant volume    
!
!$Remarques
!									  
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

use msp_gestion_erreur

        implicit none

    ! Arguments
    ! =========
        real(kind=4), intent(in) :: z, xlat, xlng
        integer, intent(in) :: iyr, mon, ida, ihr, minu
        integer, intent(in) :: igeo_ind
        real(kind=4),intent(in) :: f10, f10b, gi
        real(kind=4),dimension(12),intent(out) :: outdata
        real(kind=4),dimension(5),intent(out)  :: auxdata

!DM-ID 428: On vérifie qu'on est dans le domaine de fonctionnement du modèle

        if (z .lt. 86.) then
           call MSP_signaler_message (cle_mes="CPS_ERR_ATM_ALT_BASSE", &
                partie_variable="Met88", routine="cps_atm_met88")
           return
        endif


          call cps_met88_j70 ( z, xlat, xlng, iyr, mon, ida, ihr, minu, &
                igeo_ind, f10, f10b, gi, outdata )
          call cps_met88_j70sup ( z, outdata, auxdata )
          
      end subroutine cps_atm_met88


      subroutine cps_met88_j70sup ( z, outdata, auxdata )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_met88_j70sup
!
!$Resume
!   La subroutine cps_met88_j70sup calcule les variables auxiliaires du tableau
! auxdata, en fonction des données de sortie de la subroutine cps_met88_j70, 
! qui sont contenues dans le tableau outdata
!
!$Description
!   La subroutine cps_met88_j70sup calcule les variables auxiliaires du tableau
! auxdata, en fonction des données de sortie de la subroutine cps_met88_j70, 
! qui sont contenues dans le tableau outdata
!
!$Auteur
! Mike Hickey ( USRA, NASA/ED44 )
!
!$Usage
! call cps_met88_j70sup (z, outdata, auxdata)
! <E>   z  --  altitude (km)                 
!   <E>  tzz --  temperature at altitude z        =  outdata (2)
!   <E>      --  n2 number density                =     ..   (3)
!   <E>      --  o2  ..      ..                   =     ..   (4)
!   <E>      --  o   ..      ..                   =     ..   (5)
!   <E>      --  a   ..      ..                   =     ..   (6)
!   <E>      --  he  ..      ..                   =     ..   (7)
!   <E>      --  h   ..      ..                   =     ..   (8)
!   <E>  em  --  average molecular weight         =     ..   (9)
!   <E> dens --  total density                    =     ..   (10)
!   <E>  p   --  total pressure                   =     ..   (12)
! <S>  g  --  gravitational acceleration          =  auxdata (1) 
! <S> gam --  ratio of specific heats             =  auxdata (2) 
! <S>  h  --  pressure scale-height               =  auxdata (3) 
! <S>  cp --  specific heat at constant pressure  =  auxdata (4) 
! <S>  cv --  specific heat at constant volume    =  auxdata (5) 
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
    ! =========
        real(kind=4),intent(in) :: z
        real(kind=4),dimension(12),intent(in) :: outdata
        real(kind=4),dimension(5),intent(out)  :: auxdata

    ! Variables 
    ! =========
        real(kind=4) :: g, h
        real(kind=4) :: sum1, sum2, gam, cp, cv
        integer :: i
    ! Variables intermédiaire pour la portabilité des calculs
        real(kind=4) :: tmp1, tmp2, tmp3

        g = gravity (z)
        ! h = outdata (12) / ( g * outdata (10) )
        tmp1 = ( g * outdata (10) )
        h = outdata (12) / tmp1
        
        sum1 = outdata (3) + outdata (4)
        sum2 = 0.0
        
        do i = 5,8
           sum2 = sum2 + outdata (i)
        enddo
        
        ! gam = ( 1.4 * sum1 + 1.67 * sum2 ) / ( sum1 + sum2 )
        tmp1 = 1.4 * sum1
        tmp2 = 1.67 * sum2 
        tmp3 = ( tmp1 + tmp2)
        tmp1 = ( sum1 + sum2 )
        gam = tmp3 / tmp1

        ! cv = g * h / (   ( gam - 1.0 ) * outdata (2)   )
        tmp1 = g * h
        tmp2 = ( gam - 1.0 ) 
        tmp3 = (   tmp2* outdata (2)   )
        cv = tmp1 / tmp3
        cp = gam * cv
        
        auxdata (1) = g
        auxdata (2) = gam
        auxdata (3) = h
        auxdata (4) = cp
        auxdata (5) = cv
        
      end subroutine cps_met88_j70sup

      subroutine  cps_met88_j70 (z, xlat, xlng, iyr, mn, ida, ihr, minu, &
           igeo_ind, f10, f10b, gi, outdata )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_met88_j70
!
!$Resume
!  Routine de calcul effectif des valeurs du modèle d'atmosphère
!
!$Description
!  Routine de calcul effectif des valeurs du modèle d'atmosphère
!
!$Auteur
! Mike Hickey ( USRA, NASA/ED44 )
!
!$Usage
! call cps_met88_j70 ( z, xlat, xlng, iyr, mn, ida, ihr, min, &
!           igeo_ind, f10, f10b, gi, outdata )
! <E>     z    --  altitude
! <E>     xlat -- latitude 
! <E>     xlng -- longitude
! <E>     iyr  -- année 
! <E>     mn   -- mois
! <E>     ida  -- jour
! <E>     ihr  -- heure
! <E>     minu -- minutes
! <E>     i1   -- indice geomagnetique
! <E>     f10  -- flux solaire
! <E>     f10b -- flux solaire moyen 
! <E>     gi   -- indice d'activité geomagnetique
!    <S>     t   -- exospheric temperature      = outdata (1) 
!    <S>     tzz -- temperature at altitude z = outdata (2) 
!    <S>     a(1)-- n2 number density        = outdata (3) 
!    <S>     a(2)-- o2 number density        = outdata (4) 
!    <S>     a(3)-- o  number density        = outdata (5) 
!    <S>     a(4)-- a  number density        = outdata (6) 
!    <S>     a(5)-- he number density        = outdata (7) 
!    <S>     a(6)-- h  number density        = outdata (8) 
!    <S>     em-- average molecular weight   = outdata (9) 
!    <S>     dens-- total density            = outdata (10)
!    <S>     dl-- log10 ( total density )    = outdata (11)
!    <S>     p-- total pressure              = outdata (12)
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
    ! =========
        real(kind=4), intent(in) :: xlat, xlng
        real(kind=4), intent(in) :: z
        integer, intent(in) :: iyr, mn, ida, ihr, minu
        integer, intent(in) :: igeo_ind
        real(kind=4),intent(in) :: f10, f10b, gi
        real(kind=4),dimension(12),intent(out) :: outdata

    ! Constantes
    ! ==========
        real(kind=4),parameter :: rgas = 8.31432e3 		!  j/kmol-k
        real(kind=4),parameter :: bfh = 440.0 

    ! Variables
    ! =========
        real(kind=4), dimension(6) :: a
        real(kind=4) :: sda, sha, dy, fdhel, fdlg
        real(kind=4) :: te, tz
        real(kind=4) :: em, dens, dl, dhel1, dhel2, den
        real(kind=4) :: denlg, dlg1, dlg2, dummy, p
        integer :: i, i1, ih, dd
        real(kind=4) :: tmp1, tmp2

    ! Initialisations
    !================
        i1=igeo_ind

    ! Calculs
    !========

        call cps_met88_tme ( mn, ida, iyr, ihr, minu, xlng, sda, sha, dd, dy )

        call cps_met88_tinf ( f10, f10b, gi, xlat, sda, sha, dy, i1, te)

        call cps_met88_jac ( z, te, tz, a(1), a(2), a(3), a(4), a(5), a(6), &
             em, dens, dl )

      	denlg = 0.
      	dummy = dl
      	den   = dl
        
        if ( z .le. 170. )  then
           call cps_met88_slv ( dummy , z , xlat , dd )
           denlg = dummy
        endif

!assure la continuité de la densité en hélium entre l'altitude bfh  et 500 km

      if ( z .ge. 500. )  then
         call cps_met88_slvh ( den , a(5) , xlat , sda )
         dl = den
      else if ( z .gt. bfh )  then
         dhel1 = a ( 5 )
         dhel2 = a ( 5 )
         dlg1 = dl
         dlg2 = dl
         call cps_met88_slvh ( dlg2 , dhel2 , xlat , sda )
!!!DM_428 on prend la partie entiere de l'altitude
         ih = floor(z)

         call cps_met88_fair5 ( dhel1 , dhel2 , dlg1 , dlg2 , ih , fdhel , fdlg )
         dl = fdlg
         a ( 5 ) = fdhel
      endif

      dl = dl + denlg
      dens = 10.**dl

!  on renseigne le tableau outdata
      outdata (1) = te
      outdata (2) = tz
      
      do  i = 1, 6
         ! outdata (i+2) = 1.e6 * ( 10. ** a(i) )
         tmp1 = 10. ** a(i)
         outdata (i+2) = 1.e6 * tmp1
      enddo
      
      outdata (9) = em
      outdata (10) = dens * 1000.
      outdata (11) = dl

      ! p = outdata (10) * rgas * tz / em
      tmp1 = outdata (10) * rgas
      tmp2 = tmp1 * tz
      p = tmp2 / em
      outdata (12) = p
      
    end subroutine cps_met88_j70
    


      subroutine cps_met88_tme ( mn , ida , iyr , ihr , minu , xlng , &
      			sda , sha , dd , dy )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_met88_tme
!
!$Resume
!   La subroutine cps_met88_tme effectue le calcul de l'angle de declinaison
! du soleil et celui de l'angle horaire du soleil
!
!$Description
!   La subroutine cps_met88_tme effectue le calcul de l'angle de declinaison
! du soleil et celui de l'angle horaire du soleil
!
!$Auteur
! Mike Hickey ( USRA, NASA/ED44 )
!
!$Usage
! call cps_met88_tme (mn,ida,iyr,ihr,minu,xlat,xlng,sda,sha,dd,dy)
! <E>   mn  = month  
! <E>   ida = day      
! <E>   iyr = year     
! <E>   ihr = hour     
! <E>   minu = minute   
! <E>   xmjd= mean julian date  
! <E>   xlng= longitude ( input-geocentric longitude, -180,+180 )
! <S>   sda = solar declination angle (rad)  
! <S>   sha = solar hour angle (rad)  
! <S>   dd  = day number from 1 jan.
! <S>   dy  = dd / tropical year    
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
    ! =========
        real(kind=4), intent(in)  :: xlng
        integer,      intent(in)  :: iyr, mn, ida, ihr, minu
        real(kind=4), intent(out) :: sda, sha, dy
        integer,      intent(out) :: dd

    ! Constantes
    ! ==========
        real(kind=4),parameter :: year = 365.2422
        real(kind=4),parameter :: a1 = 99.6909833 
        real(kind=4),parameter :: a2 = 36000.76892
        real(kind=4),parameter :: a3 = 0.00038708
        real(kind=4),parameter :: a4 = 0.250684477
        real(kind=4),parameter :: b1 = 0.0172028
        real(kind=4),parameter :: b2 = 0.0335
        real(kind=4),parameter :: b3 = 1.407
        real(kind=4),parameter :: rad_deg = 0.017453293
        
    ! Variables
    ! =========
        real(kind=4) :: xj, gp, rap, y1, y2
        real(kind=4) :: xls, b4, arg, ras
        real(kind=4) :: tmp, lng
        real(kind=4) :: xmjd, gmt, fmjd
        real(kind=4) :: gptmp1, gptmp2, gptmp3, gptmp4
        integer :: id, i

! Initialisations

        lng = xlng


! nombres de jours en février selon que l'année est, ou non, bissextile
      if ( mod(iyr,4) .eq. 0 ) then
         if ( mod(iyr,100) .ne. 0 ) iday(2) = 29 ! les années divisibles par 100 ne sont pas bissextiles
         if ( mod(iyr,400) .eq. 0 ) iday(2) = 29 !DM_ID428: mais celles divisibles par 400 le sont 
      else
         iday(2) = 28
      endif
      id = 0
      if ( mn .gt. 1 ) then
         do  i = 1 , mn-1
            id = id + iday(i)
         enddo
      endif
      id = id + ida
      dd = id
      dy = dd/year

! calcul de la date julienne
      xmjd = 2415020.+ 365.*( real(iyr, kind=4)-1900.) + real(dd, kind=4) + &
           ( real(iyr, kind=4) - 1901.)/4.

! calcul du temps moyen Greenwich, en minutes gmt
      gmt = 60. *  real(ihr, kind=4) +  real(minu, kind=4)
      fmjd = xmjd - 2435839. + gmt / 1440.

! calcul de la position moyenne Greenwich - gp ( rad )
      gptmp1 = ( xmjd - 2415020.5 )
      xj = gptmp1 / ( 36525.0 )

     ! calculs intermédiaires pour la portabilité
      ! gp = mod ( a1 + a2*xj + a3*xj*xj + a4*gmt , 360. )
      gptmp1= a2*xj 
      gptmp4= a1 + gptmp1

      gptmp1=a3*xj
      gptmp2= gptmp1*xj 
      gptmp4= gptmp4 + gptmp2
      gptmp3= a4*gmt
      gptmp4= gptmp4 + gptmp3
      gp = mod ( gptmp4 , 360. )

! calcul du point d'ascension droite - rap ( in rad )
! convention longitude - vers l'ouest, + vers l'est
     if ( lng .gt. pi ) lng = lng - tpi
     gp = gp * rad_deg

     rap = mod ( gp + lng , tpi )


! calcul de la longitude celeste - xls ( en rad ) -- [0;2pi]
     y1 = b1 * fmjd

     ! y2 = 0.017202 * ( fmjd - 3. )
     gptmp1 = ( fmjd - 3. )
     y2 = 0.017202 * gptmp1
     ! calculs intermédiaires pour la portabilité
     ! xls = mod ( y1 + b2 * sin(y2) - b3 , tpi )
     gptmp1 = sin(y2)
     gptmp2 =  b2 * gptmp1
     gptmp3 = y1 + gptmp2
     gptmp4 = gptmp3 - b3
     xls = mod ( gptmp4, tpi )

! calcul de l'angle de declinaison solaire - sda (rad)
     ! b4 = rad_deg * ( 23.4523 - 0.013 * xj ) 
     gptmp1 = 0.013 * xj
     gptmp2 = ( 23.4523 - gptmp1 ) 
     b4 = rad_deg * gptmp2
     ! calculs intermédiaires pour la portabilité
     ! sda = asin ( sin ( xls ) * sin ( b4 ) )
     gptmp1 = sin ( xls )
     gptmp2 = sin ( b4 )
     gptmp3 = gptmp1*gptmp2
     sda = asin ( gptmp3 )

! calcul de l'ascension droite du soleil - ras ( en rad ) -- [0;2pi]
! précaution : on vérifie que l'argument du arcsin est compris entre -1et 1
     ! calculs intermédiaires pour la portabilité
     ! arg = tan ( sda ) / tan ( b4 )
     gptmp1 = tan ( sda )
     gptmp2 = tan ( b4 )
     arg = gptmp1 / gptmp2
     if ( arg .gt. 1.0 ) arg = 1.0
     if ( arg .lt. -1. ) arg = -1.0
     ras = asin ( arg )
     
! on place ras dans le meme quadrant que xls
     ras = abs ( ras )
     tmp = abs ( xls )
     
     if ( tmp.le.pi .and. tmp.gt.pi2 )  then
      	ras = pi - ras
     else if ( tmp.le.pi32 .and. tmp.gt.pi )  then
      	ras = pi + ras
     else if ( tmp.gt.pi32 )  then
      	ras = tpi - ras
     endif
     if ( xls .lt. 0. ) ras = -ras
     
 ! calcul de l'angle horaire - sha ( in rad ) 
     sha = rap - ras 
     if ( sha .gt. pi ) sha = sha - tpi
     if ( sha .lt.-pi ) sha = sha + tpi
     
     
   end subroutine cps_met88_tme




   subroutine  cps_met88_tinf ( f10,f10b,gi,lat,sda,sha,dy,i1,te)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_met88_tinf
!
!$Resume
!  la  subroutine cps_met88_tinf calcule la temperature exosphérique
!
!$Description
!  la  subroutine cps_met88_tinf calcule la temperature exosphérique
!
!$Auteur
! Mike Hickey ( USRA, NASA/ED44 )
!
!$Usage
! call cps_met88_tinf ( f10,f10b,gi,lat,sda,sha,dy,i1,te)
! <E>     f10  -- flux solaire
! <E>     f10b -- flux solaire moyen 
! <E>     gi   -- indice d'activité geomagnetique
! <E>     lat  -- latitude 
! <E>     sda  -- angle de déclinaison solaire
! <E>     sha  -- angle horaire du soleil
! <E>     dy   -- d/y = numero du jour / année tropicale
! <E>     i1   -- indice de l'équation géomagnétique 
!                 ( 1--gi=kp , 2--gi=ap )
! <S>     te   --  température troposphérique
!
!$Remarques
!    constantes  -- c = variation d el'activité solaire
!                -- beta , etc = variations diurnes
!                -- d = variation géomagnétique
!                -- e = variation semi-annuelle
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        implicit none

    ! Arguments
    ! =========
        real(kind=4),intent(in) :: f10, f10b, gi
        real(kind=4), intent(in) :: lat
        real(kind=4), intent(in) :: sda, sha, dy
        integer, intent(in) :: i1
        real(kind=4), intent(out) :: te

    ! Constantes
    ! ==========
        real(kind=4),parameter :: xm = 2.5
        real(kind=4),parameter :: xnn = 3.0
        real(kind=4),parameter :: beta = -0.6457718
        real(kind=4),parameter :: gamma = 0.7504916
        real(kind=4),parameter :: p = 0.1047198
        real(kind=4),parameter :: re = 0.31
! les ci sont les parametres de variation de l'activite solaire
        real(kind=4),parameter :: c1 = 383.0
        real(kind=4),parameter :: c2 = 3.32
        real(kind=4),parameter :: c3 = 1.80
! les di sont les parametres de variation geomagnetique
        real(kind=4),parameter :: d1 = 28.0
        real(kind=4),parameter :: d2 = 0.03
        real(kind=4),parameter :: d3 = 1.0
        real(kind=4),parameter :: d4 = 100.0
        real(kind=4),parameter :: d5 = -0.08
! les ei sont les parametres de variation semi-annuelle
        real(kind=4),parameter :: e1 = 2.41
        real(kind=4),parameter :: e2 = 0.349
        real(kind=4),parameter :: e3 = 0.206
        real(kind=4),parameter :: e4 = 6.2831853
        real(kind=4),parameter :: e5 = 3.9531708
        real(kind=4),parameter :: e6 = 12.5663706
        real(kind=4),parameter :: e7 = 4.3214352
        real(kind=4),parameter :: e8 = 0.1145
        real(kind=4),parameter :: e9 = 0.5
        real(kind=4),parameter :: e10 = 6.2831853
        real(kind=4),parameter :: e11 = 5.9742620
        real(kind=4),parameter :: e12 = 2.16

    ! Variables
    ! ==========
        real(kind=4) :: eta, theta, tau, tau1
        real(kind=4) :: a1, a2, a3, b1, b2
        real(kind=4) :: tc, tv, tl, ts, tg
        real(kind=4) :: g1, g2, g3
        real(kind=4) :: tmp1, tmp2,tmp3, tmp4

!variation de l'activité solaire
      ! tc = c1 + c2 * f10b + c3 * ( f10 - f10b )
      tmp1 = ( f10 - f10b )
      tmp2 = c3 * tmp1
      tmp3 = c2 * f10b 
      tmp4 = c1 + tmp3
      tc = tmp4 + tmp2

!variation diurne
      ! eta    = 0.5 * abs (lat - sda  )
      tmp1 = lat - sda
      tmp2 = abs ( tmp1  )
      eta    = 0.5 * tmp2
      ! theta  = 0.5 * abs ( lat + sda )
      tmp1 = lat + sda
      tmp2 = abs ( tmp1 )
      theta  = 0.5 * tmp2

      ! calculs intermédiaires pour la portabilité
      ! tau = sha + beta + p * sin ( sha + gamma )
      tmp1 = sha + gamma
      tmp2 = sin ( tmp1 )
      tmp3 = p * tmp2
      tmp4 = sha + beta
      tau  = tmp4 + tmp3

      if ( tau .gt. pi ) tau = tau - tpi
      if ( tau .lt.-pi ) tau = tau + tpi

      ! calculs intermédiaires pour la portabilité
      ! a1 = ( sin ( theta ) )**xm
      tmp1 = sin ( theta )
      a1 = ( tmp1 )**xm

      ! calculs intermédiaires pour la portabilité
      ! a2 = ( cos ( eta ) )**xm
      tmp1 =  cos ( eta )
      a2 = ( tmp1 )**xm

      ! calculs intermédiaires pour la portabilité
      ! a3 = ( cos ( tau / 2. ) )**xnn
      tmp1 =  tau / 2.
      tmp2 = cos(tmp1)
      a3 = ( tmp2 )**xnn

      ! calculs intermédiaires pour la portabilité
      ! b1 = 1.0 + re * a1
      tmp1 = re * a1
      b1 = 1.0 + tmp1
      ! b2 = ( a2 - a1 ) / b1
      tmp1 = a2 - a1
      b2 = ( tmp1 ) / b1

      ! calculs intermédiaires pour la portabilité
      ! tv = b1 * ( 1. + re * b2 * a3 )
      tmp1 = re * b2 
      tmp2 = tmp1*a3
      tmp3 = 1.+tmp2
      tv = b1 * ( tmp3 )

      tl = tc * tv

!variation géomagnétique
      if ( i1.eq.1 ) then
         ! calculs intermédiaires pour la portabilité
         ! tg = d1 * gi + d2 * exp(gi)
         tmp1 = exp(gi)
         tmp2 = d2 * tmp1
         tmp3 = d1 * gi
         tg = tmp3 + tmp2
      else 
         ! calculs intermédiaires pour la portabilité
         ! tg = d3 * gi + d4 * ( 1.0 - exp ( d5 * gi ) )
         tmp1 = d5 * gi
         tmp2 = exp(tmp1)
         tmp3 = 1.0 - tmp2
         tmp4 = d4 * ( tmp3 )
         tmp2 = d3 * gi
         tg = tmp2 + tmp4
      endif
      
! variation semi-annuelle
      ! calculs intermédiaires pour la portabilité
      ! g3 = 0.5 * ( 1.0 + sin ( e10 * dy + e11 ) )
      tmp1 = e10 * dy
      tmp2 = tmp1+e11
      tmp3 = sin(tmp2)
      tmp1=1.0 + tmp3
      g3 = 0.5 * tmp1
      ! g3 = g3 ** e12
      tmp1 = g3
      g3 = tmp1 ** e12

      ! calculs intermédiaires pour la portabilité
      ! tau1 = dy + e8 * ( g3 - e9 )
      tmp1 = g3 - e9
      tmp2 = e8 * tmp1
      tau1 = dy + tmp2

      ! calculs intermédiaires pour la portabilité
      ! g1 = e2 + e3 * ( sin ( e4 * tau1 + e5 ) )
      tmp1 = e4 * tau1 
      tmp2 = tmp1 + e5
      tmp3 = sin(tmp2)
      tmp1 = e3 * tmp3
      g1 = e2 + tmp1

      ! calculs intermédiaires pour la portabilité
      ! g2 = sin ( e6 * tau1+ e7 )
      tmp1 = e6 * tau1
      tmp2 = tmp1 + e7
      g2 = sin ( tmp2 )

      ! calculs intermédiaires pour la portabilité
      ! ts = e1 + f10b * g1 * g2
      tmp1 = f10b * g1
      tmp2 = tmp1 * g2
      ts = e1 + tmp2

! temperature exosphérique
      ! te = tl + tg + ts
      tmp1 = tl + tg
      te = tmp1 + ts

      end subroutine  cps_met88_tinf


      subroutine cps_met88_jac ( z, t, tz, an, ao2, ao, aa, ahe, ah, em, &
      			 dens, dl )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_met88_jac
!
!$Resume
!    Cette subroutine calcule la temperature 'tz' , la densité totale 
! 'dens' ainsi que son logarithme dl, le poids moléculaire moyen 'em', 
! les densités individuelles pour N, O2, O, A, He et H, à l'altitude z
! et pour une température exosphérique t.
!                                                                          **
!$Description
!    Cette subroutine calcule la temperature 'tz' , la densité totale 
! 'dens' ainsi que son logarithme dl, le poids moléculaire moyen 'em', 
! les densités individuelles pour N, O2, O, A, He et H, à l'altitude z
! et pour une température exosphérique t.
!                                                                          **
!$Auteur
! Mike Hickey ( USRA, NASA/ED44 )
!
!$Usage
! call cps_met88_jac
! <E>     z   --  altitude
! <E>     t   --  temperature exosphérique
!    <S>     tz  -- temperature a l'altitude z
!    <S>     an  -- densité en N2 
!    <S>     ao2 -- densité en O2
!    <S>     ao  -- densité en O
!    <S>     aa  -- densité en A
!    <S>     ahe -- densité en He
!    <S>     ah  -- densité en H
!    <S>     em  -- le poids moléculaire moyen
!    <S>     dens-- densité globale
!    <S>     dl  -- log10 (dens)
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
    ! =========
        real(kind=4), intent(in) :: z, t
        real(kind=4), intent(out) :: an ,ao2, ao, aa, ahe, ah
        real(kind=4), intent(out):: em, tz, dens, dl

    ! Constantes
    ! ==========
        real(kind=4),parameter :: av = 6.02257e23
        real(kind=4),parameter :: qn   = .78110
        real(kind=4),parameter :: qo2  = .20955
        real(kind=4),parameter :: qa   = .009343
        real(kind=4),parameter :: qhe  = 1.289e-05
        real(kind=4),parameter :: rgas = 8.31432
        real(kind=4),parameter :: t0   = 183.

    ! Variables
    ! =========
        integer :: i
        real(kind=4) ::  s
        real(kind=4) ::  tx, tx_t0, t1, t3, t4, td
        real(kind=4) ::  a1, a2, d, r, factor, par
        real(kind=4), dimension(6) :: alpha, ei, di, dit

        ! Variables temporaires pour assurer que les calculs sont toujours
        ! faits dans le même ordre
        real(kind=4) :: tmp1, tmp2,tmp3, tmp4

	data alpha /0.0,0.0,0.0,0.0,-.380,0.0/
	data ei    /28.0134,31.9988,15.9994,39.948,4.0026,1.00797/
        
        ! tx = 444.3807 + .02385 * t - 392.8292 * exp ( -.0021357 * t )
        tmp1 = -.0021357 * t
        tmp2 = exp ( tmp1 )
        tmp3 = 392.8292 * tmp2
        tmp4 = .02385 * t
        tmp1 = 444.3807 + tmp4 
        tx = tmp1 - tmp3

        ! a2 = 2. * (t-tx) / pi
        tmp1 = t-tx
        tmp2 = 2. * tmp1
        a2 = tmp2 / pi

        tx_t0 = tx - t0
        ! t1 =  (1.9 * tx_t0) /   35.
        tmp1 = (1.9 * tx_t0)
        t1 =  tmp1 /   35.
        ! t3 = (-1.7 * tx_t0) / ( 35.**3 )
        tmp1 =  (-1.7 * tx_t0) 
        tmp2 = ( 35.**3. )
        t3 = tmp1 / tmp2
        ! t4 = (-0.8 * tx_t0) / ( 35.**4 )
        tmp1 = 0.8 * tx_t0
        tmp2 = (-tmp1)
        tmp3 = ( 35.**4. )
        t4 = tmp2 / tmp3
        ! tz = temp ( z, tx, t1, t3, t4, a2 )
        tz = temp ( z, tx, t1, t3, t4, a2 )
        
        d = min ( z , 105. )
        
!  integrate  gm/t  from  90  to  minimum of z or 105 km :-

        call cps_met88_gauss ( d, 1, r, tx , t1 , t3 , t4 , a2 )
        
!  the number 2.1926e-8 = density x temperature/mean molecular weight at 90 km.

        em = mol_wt (d)
        td = temp (d ,tx ,t1 ,t3 ,t4 ,a2 )
        
        ! dens = ((2.1926e-8 * em) * exp( -r / rgas )) / td
        tmp1 =  -r / rgas
        tmp2 = exp(tmp1 )
        tmp3 = (2.1926e-8 * em)
        tmp4 = (tmp3 * tmp2)
        dens = tmp4 / td
        
        factor = av * dens
        par = factor / em

        ! factor = factor / 28.96
        tmp1 = factor
        factor = tmp1 / 28.96
        
        
!  Dans le cas ou z<105 km, calcul des densités individuelles 
!  à partir du poids moléculaire moyen et de la densité globale
!  ###
        if ( z .le. 105. )  then
           
           dl = log10 ( dens )
           ! an = log10 ( qn * factor )
           tmp1 = qn * factor 
           an  = log10 ( tmp1 )
           ! aa = log10 ( qa * factor )
           tmp1 = qa * factor
           aa  = log10 ( tmp1 )
           ! ahe = log10 ( qhe * factor )
           tmp1 = qhe * factor
           ahe = log10 ( tmp1 )
           ! ao  = log10 ( 2. * par * ( 1.-em / 28.96 ) )
           tmp1 = em / 28.96
           tmp2 = ( 1. - tmp1 ) 
           tmp3 = 2. * par
           tmp4 =  tmp3 * tmp2
           ao  = log10 ( tmp4 )
           ! ao2 = log10 ( par * ( em * ( 1.+qo2 ) / 28.96-1. ) )
           tmp1 = ( 1.+qo2 ) 
           tmp2 =  tmp1/ 28.96
           tmp3 = (em * tmp2 )
           tmp1 = tmp3 -1.
           tmp4 =   par * tmp1
           ao2 = log10 ( tmp4 )
           ! ao2 = log10 ( par * ( em * ( 1.+qo2 ) / 28.96-1. ) )
           ah  = 0.
           return
           
        endif
        

!NB : la partie qui suit n'est exécutée que dans le cas z> 105km

!  Dans le cas ou z>105 km, calcul des densités individuelles 
!  à partir du poids moléculaire moyen et de la densité globale

      	di(1) = qn * factor

      	! di(2) = par * (em * (1.+qo2) / 28.96-1.)
        tmp1 = (1.+qo2)
        tmp2 = em * tmp1
        tmp3 = (tmp2 / 28.96)
        tmp4 = tmp3 -1.
      	di(2) = par * tmp4

      	! di(3) = (2. * par) * (1.- em / 28.96)
        tmp1 = em / 28.96
        tmp2 = (1.- tmp1)
        tmp3 = (2. * par)
      	di(3) = tmp3 * tmp2

      	di(4) = qa * factor
      	di(5) = qhe * factor

!  integre g/t de 105 km à z km :-

        call cps_met88_gauss ( z, 2, r, tx , t1 , t3 , t4 , a2 )
        
      	do  i = 1 , 5
           ! dit(i) = (di(i)*((td/tz)**(1.+alpha(i))))*exp(-ei(i)*r/rgas)
           tmp1 = ei(i)*r
           tmp2 = -tmp1
           tmp3 = tmp2/rgas
           tmp4 = exp(tmp3)
           tmp1 = (1.+alpha(i))
           tmp2 = (td/tz)
           tmp3 = (tmp2**tmp1)
           tmp1 = (di(i)*tmp3)
           dit(i) = tmp1*tmp4
           if ( dit(i) .le. 0. ) dit(i) = 1.e-6
        enddo
        
! Cette partie calcule la densité atomique de l'hydrogène d pour 
! z > 500km. Dans le cas ou z<500 km, on pose que cette densité 
! est de 10**-6.

        if ( z .gt. 500. )  then
           
           a1 = 500.
           s = temp ( a1, tx, t1, t3, t4, a2)

           ! di(6) = 10.**( (73.13 - (39.4 * log10 (s))) + (5.5*log10(s)) *log10(s))
           tmp1 = log10 (s)
           tmp2 = 39.4 * tmp1
           tmp3 = 5.5 * tmp1
           tmp4 = tmp3 * tmp1
           tmp1 = 73.13 - tmp2 
           tmp2 = tmp1 + tmp4
           di(6) = 10.** ( tmp2)
           call cps_met88_gauss ( z, 7, r, tx, t1, t3, t4, a2)

           ! dit(6) = (di(6) * (s/tz)) * exp ( (-ei(6) * r) / rgas )
           tmp1 = ei(6) * r
           tmp2 = (-tmp1)
           tmp3 = tmp2 / rgas
           tmp4 = exp ( tmp3 )
           tmp1 = (s/tz)
           tmp2 = (di(6) * tmp1)
           dit(6) = tmp2 * tmp4
           
        else
           dit (6) = 1.0
        endif
        
        !   for altitudes greater than 105 km , calculate total density and mean
!   molecular weight from individual specie number densities.
        dens=0.0
        do i = 1,6
           tmp1 = (ei(i) * dit(i))
           tmp2 = tmp1 / av
           dens = dens + tmp2
        enddo

        ! em = dens * av / ( dit(1)+dit(2)+dit(3)+dit(4)+dit(5)+dit(6) )
        tmp1 = (dit(1)+dit(2))
        tmp2 = (tmp1+dit(3))
        tmp3 = (tmp2+dit(4))
        tmp4 = (tmp3+dit(5))
        tmp1 = (tmp4+dit(6) )
        tmp2 = (dens * av)
        em = tmp2 / tmp1
        dl = log10 (dens)
        
        an  = log10(dit(1))
        ao2 = log10(dit(2))
        ao  = log10(dit(3))
        aa  = log10(dit(4))
        ahe = log10(dit(5))
        ah  = log10(dit(6))
        
      end subroutine cps_met88_jac
      



      real(kind=4) function temp(alt,tx,t1,t3,t4,a2) result(t)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!   temp
!
!$Resume
! calcule la valeur de la temperature a l'altitude alt de 2 manières 
! différentes selon que 90 km<alt<125 km, ou 125 km<alt
!
!$Description
! calcule la valeur de la temperature a l'altitude alt de 2 manières 
! différentes selon que 90 km<alt<125 km, ou 125 km<alt
!
!$Auteur
!
!$Usage
!   t = temp(alt, tx, t1, t3, t4, a2)
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
    ! =========
        real(kind=4), intent(in) :: alt, tx, t1, t3, t4, a2

    ! Constante
    ! =========
        real(kind=4),parameter :: bb=4.5e-6

    ! Variables
    ! =========
        real(kind=4) :: u
        real(kind=4) :: tmp1, tmp2, tmp3, tmp4

      	u = alt - 125.
        if ( u .gt. 0. )  then
           ! t = tx  +  a2 * atan ( t1 * u * ( 1. + bb * (u**2.5)) / a2 )
           tmp1 = (u**2.5)
           tmp2 = bb * tmp1
           tmp3 = 1. + tmp2
           tmp4 = t1 * u
           tmp1 = tmp4 * tmp3
           tmp2 = tmp1/a2
           tmp3 = atan(tmp2)
           tmp1 = a2 * tmp3
           t = tx  +  tmp1
        else
           ! t = tx  +  t1 * u  +  t3 * (u**3)  +  t4 * (u**4)
           tmp1 = u**4.
           tmp2 = t4 * tmp1
           tmp1 = u**3.
           tmp3 = t3 * tmp1
           tmp1 = t1 * u
           tmp4 = tx + tmp1
           tmp1 = tmp4 + tmp3
           t = tmp1 +  tmp2
        endif
        
      end function temp





      real(kind=4) function gravity (altitude) result (grav)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!   gravity
!
!$Resume
! calcule la valeur de g en fonction de l'altitude
!
!$Description
! calcule la valeur de g en fonction de l'altitude
!
!$Auteur
!
!$Usage
!   g = gravity(altitude)
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
    ! =========
        real(kind=4), intent(in) :: altitude
        ! real(kind=4) :: grav

    ! Variables intermédiaire pour la portabilité des calculs
        real(kind=4) :: tmp1, tmp2, tmp3

    !  grav = 9.80665 / ( ( 1. + altitude / 6.356766e3 )**2 )
        tmp1 = altitude / 6.356766e3
        tmp2 = 1. + tmp1
        tmp3 = ( tmp2**2. )

      grav = 9.80665 / tmp3

        end function





      real(kind=4) function mol_wt (a) result (mol)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!   mol_wt
!
!$Resume
! calcule le poids molaire pour les altitudes comprises entre 90 km et 
! 105 km; dans les autres cas, le poids molaire est initialisé à 1
!
!$Description
! calcule le poids molaire pour les altitudes comprises entre 90 km et 
! 105 km; dans les autres cas, le poids molaire est initialisé à 1
!
!$Auteur
!
!$Usage
!   m = mol_wt(a)
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
    ! =========
        real(kind=4), intent(in) :: a
        ! real(kind=4) :: mol
    ! Constantes
    ! ==========
        real(kind=4), dimension(7) :: b
	data b /28.15204,-0.085586,1.284e-4,-1.0056e-5,-1.021e-5,1.5044e-6,9.9826e-8/

    ! Variables
    ! =========
        real(kind=4) :: u
        integer :: i
    ! Variables intermédiaire pour la portabilité des calculs
        real(kind=4) :: tmp1, tmp2
        integer :: ii1

      if ( a .gt. 105. ) then
      	mol = 1.
      else

      	u = a - 100.
      	mol = b (1)
        do  i = 2,7
           ii1 = i-1
           tmp1 = u ** ii1
           tmp2 = b (i) * tmp1

           mol = mol  +  tmp2
        enddo

     endif

   end function mol_wt




      subroutine cps_met88_gauss (z2, nmin, r, tx, t1, t3, t4, a2)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
! cps_met88_gauss
!
!$Resume
! sous-divise l'intervalle d'intégration en altitude en intervalles sur 
! lesquel il est possible d'appliquer la quadrature gaussienne, initialise 
! le nombre de points pour l'intégration sur chacun de ces intervalles, 
! et réalise enfin la quadrature gaussienne
!
!$Description
! sous-divise l'intervalle d'intégration en altitude en intervalles sur 
! lesquel il est possible d'appliquer la quadrature gaussienne, initialise 
! le nombre de points pour l'intégration sur chacun de ces intervalles, 
! et réalise enfin la quadrature gaussienne
!
!$Auteur
! Mike Hickey ( USRA, NASA/ED44 )
!
!$Usage
! call cps_met88_gauss ( z1 , z2 , nmin , r , tx , t1 , t3 , t4 , a2 )
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
    ! =========
        real(kind=4), intent(in) :: z2, tx, t1, t3, t4, a2
        integer, intent(in) :: nmin
        real(kind=4), intent(out):: r

    ! Constantes
    ! ==========
      real(kind=4), dimension(9) :: altmin
      integer, dimension(8) ::  ng
      real(kind=4), dimension(8,6) :: c, x

    ! Variables
    ! =========
      real(kind=4) :: a, d, rr, del, z
      integer :: ngauss, i, j, k
    ! Variables intermédiaire pour la portabilité des calculs
        real(kind=4) :: tmp1, tmp2, tmp3

!!$      gravity ( altitude ) = 9.80665 / ( ( 1. + altitude / 6.356766e3 )**2 )

      data altmin/90.,105.,125.,160.,200.,300.,500.,1500.,2500./
      data ng    /4,5,6,6,6,6,6,6 /

!  coefficients for gaussian quadrature ...

      data c / .5555556 , .8888889 , .5555556 , .0000000 , &! n=3 
               .0000000 , .0000000 , .0000000 , .0000000 , &! n=3 
               .3478548 , .6521452 , .6521452 , .3478548 , &! n=4 
               .0000000 , .0000000 , .0000000 , .0000000 , &! n=4 
               .2369269 , .4786287 , .5688889 , .4786287 , &! n=5 
               .2369269 , .0000000 , .0000000 , .0000000 , &! n=5 
               .1713245 , .3607616 , .4679139 , .4679139 , &! n=6 
               .3607616 , .1713245 , .0000000 , .0000000 , &! n=6 
               .1294850 , .2797054 , .3818301 , .4179592 , &! n=7 
               .3818301 , .2797054 , .1294850 , .0000000 , &! n=7 
               .1012285 , .2223810 , .3137067 , .3626838 , &! n=8 
               .3626838 , .3137067 , .2223810 , .1012285 /  ! n=8

!  abscissas for gaussian quadrature ...

      data  x / -.7745967 ,  .0000000 ,  .7745967 ,  .0000000 , &! n=3 
                 .0000000 ,  .0000000 ,  .0000000 ,  .0000000 , &! n=3 
                -.8611363 , -.3399810 ,  .3399810 ,  .8611363 , &! n=4 
                 .0000000 ,  .0000000 ,  .0000000 ,  .0000000 , &! n=4 
                -.9061798 , -.5384693 ,  .0000000 ,  .5384693 , &! n=5 
                 .9061798 ,  .0000000 ,  .0000000 ,  .0000000 , &! n=5 
                -.9324695 , -.6612094 , -.2386192 ,  .2386192 , &! n=6 
                 .6612094 ,  .9324695 ,  .0000000 ,  .0000000 , &! n=6 
                -.9491079 , -.7415312 , -.4058452 ,  .0000000 , &! n=7 
                 .4058452 ,  .7415312 ,  .9491079 ,  .0000000 , &! n=7 
                -.9602899 , -.7966665 , -.5255324 , -.1834346 , &! n=8 
                 .1834346 ,  .5255324 ,  .7966665 ,  .9602899 /  ! n=8

      	r  =  0.0

      do k = nmin , 8

         ngauss = ng (k)
         a      = altmin (k)
         d      = min ( z2 , altmin (k+1) )
         rr     = 0.0
         del    = 0.5 * ( d - a )
         j      = ngauss - 2
         
         do     i = 1 , ngauss
            ! z = del * ( x(i,j) + 1. ) + a
            tmp1 =  x(i,j) + 1.
            tmp2 = del * (tmp1 ) 
            z = tmp2 + a

            ! rr = rr + ((c(i,j) * mol_wt(z)) * gravity(z)) / &
            !      temp ( z,tx,t1,t3,t4,a2 )
            tmp1 = (c(i,j) * mol_wt(z))
            tmp3 = gravity(z)
            tmp2 = (tmp1 * tmp3)
            tmp1 = temp ( z,tx,t1,t3,t4,a2 )
            tmp3 = tmp2 / tmp1
            rr = rr + tmp3
                 
         enddo
         rr = del * rr
         r  = r + rr
         if ( d .eq. z2 )  return
         
      enddo

    end subroutine cps_met88_gauss





      subroutine cps_met88_slv ( den , alt , xlat , day )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
! cps_met88_slv
!
!$Resume
! cette routine calcule les variations de densité latitudinale dans la 
! basse thermosphere; ce calcul est pertinent entre 90 km et 170 km 
! d'altitude (au dessus de 170 km, on n'observe pas d'effet)
! la variation doit etre calculee apres le calcul de l'effet des variations 
! de temperature, et la densite (den) doit etre sous la forme d'un log 
! base 10
!
!$Description
! cette routine calcule les variations de densités latitudinale dans la 
! basse thermosphere; ce calcul est pertinent entre 90 km et 170 km 
! d'altitude (au dessus de 170 km, on n'observe pas d'effet)
! la variation doit etre calculee apres le calcul de l'effet des variations 
! de temperature, et la densite (den) doit etre sous la forme d'un log 
! base 10
!
!$Auteur
! Mike Hickey ( USRA, NASA/ED44 )
!
!$Usage
! call cps_met88_slv ( den , alt , xlat , day )
!**                      den    = density (log10)                            **
!**                      alt    = altitude (km)                              **
!**                      xlat   = latitude (rad)                             **
!**                      day    = day number                                 **
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
    ! =========
        real(kind=4), intent(in) :: alt, xlat
        integer, intent(in) :: day
        real(kind=4), intent(out) :: den

    ! Variables
    ! =========
        real(kind=4)::x, y, z, p, sp, s, d
    ! Variables intermédiaire pour la portabilité des calculs
        real(kind=4) :: tmp1, tmp2

! initialisation
      den = 0.0
! on vérifie que le calcul est pertinent (altitude<170 km)
      if ( alt .gt. 170. ) return

! calcul des changements de densite dans la basse thermosphere
!
      z = alt - 90.

      ! x = (-0.0013 * z) * z
      tmp1 = -0.0013 * z
      x = (tmp1) * z

      ! y = (0.0172 * real(day, kind=4)) + 1.72
      tmp1 = real(day, kind=4)
      tmp2 = (0.0172 * tmp1)
      y = tmp2 + 1.72
      p = sin (y)

      ! sp = ( sin (xlat) ) **2
      tmp1 = sin (xlat)
      sp = ( tmp1 ) **2.
      ! s = (0.014 * z) * exp (x)
      tmp1 = (0.014 * z)
      tmp2 = exp (x)
      s = tmp1 * tmp2

      ! d = (s * p) * sp
      tmp1 = s * p
      d = tmp1 * sp
!
!** check to compute absolute value of 'xlat'
!
      if ( xlat .lt. 0. ) d = -d
      den = d

    end subroutine cps_met88_slv




      subroutine cps_met88_slvh ( den , denhe , xlat , sda )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
! cps_met88_slvh
!
!$Resume
! cette subroutine calcule la variation latitudinale de la densite en 
! helium; ce calcul n'est pas pertinent en dessous des 500km
!
!$Description
! cette subroutine calcule la variation latitudinale de la densite en 
! helium; ce calcul n'est pas pertinent en dessous des 500km
!
!$Auteur
! Mike Hickey ( USRA, NASA/ED44 )
!
!$Usage
! call cps_met88_slvh ( den , denhe , xlat , sda )
!**                     den   = density (log10)                              **
!**                     denhe = helium number density (log10)                **
!**                     xlat  = latitude (rad)                               **
!**                     sda   = solar declination angle (rad)                **
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
    ! =========
        real(kind=4), intent(in) :: xlat, sda
        real(kind=4), intent(inout) :: den, denhe

    ! Variables
    ! =========
        real(kind=4):: a, b, d0, x, y
        real(kind=4):: d1, del, rho, drho, dhe
    ! Variables intermédiaire pour la portabilité des calculs
        real(kind=4) :: tmp1, tmp2

        d0 = 10. ** denhe

        ! a = abs ( 0.65 * ( sda / 0.40909079 ) )
        tmp1 =  sda / 0.40909079
        tmp2 =  0.65 * (tmp1 )
        a = abs (tmp2 )

        b  = 0.5 * xlat

! test, pour prendre ou b ou sa valeur absolue
      if ( sda .lt. 0. ) b = -b

      x  = 0.7854 - b

      ! y  = ( sin (x) ) ** 3
      tmp1 = sin (x)
      y  = ( tmp1 ) ** 3.

      ! dhe= a * ( y - 0.35356 )
      tmp1 = ( y - 0.35356 )
      dhe= a * tmp1

      denhe = denhe + dhe

      

      d1 = 10. ** denhe
      del= d1 - d0
      rho= 10. ** den 
      drho = ( 6.646e-24 ) * del
      rho  = rho + drho

      den  = log10 (rho)

    end subroutine cps_met88_slvh
   



      subroutine cps_met88_fair5 ( dhel1 ,dhel2 ,dlg1 ,dlg2 ,ih ,fdhel ,fdlg )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
! cps_met88_fair5
!
!$Resume
! cette subroutine lisse la densité en helium à la frontière des 500km 
!
!$Description
! cette subroutine lisse la densité en helium à la frontière des 500km 
!
!$Auteur
! bill jeffries, csc, huntsville, al.
!
!$Usage
! call cps_met88_fair5( dhel1 ,dhel2 ,dlg1 ,dlg2 ,ih ,fdhel ,fdlg )
! <E> dhel1 = helium number density before invoking slvh
! <E> dhel2 = helium number density after invoking slvh 
! <E> dlg1  = total density before invoking slvh        
! <E> dlg2  = total density after invoking slvh         
! <E> ih    = height  ( km )                             -- integer                 
! <E> ibfh  = base fairing height ( km )                 -- integer     
! <S> fdhel = faired helium number density              
! <S> fdlg  = faired total density                      
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
    ! =========
        real(kind=4), intent(in) :: dhel1, dhel2, dlg1, dlg2
        integer, intent(in) :: ih
        real(kind=4), intent(out) ::fdhel, fdlg

    ! Constantes
    ! ========== 
        integer,parameter :: ibfh = 440               ! base fairing height 
        real(kind=4), dimension(6) :: cz
            data cz / 1.0, 0.9045085, 0.6545085, 0.3454915, 0.0954915, 0.0 /

    ! Variables
    ! =========
            integer :: i, itmp1, itmp2
            real(kind=4) :: czi, szi
    ! Variables intermédiaire pour la portabilité des calculs
        real(kind=4) :: tmp1, tmp2

!  height index
      ! i = ( ih - ibfh ) /10 + 1
      itmp1 = ( ih - ibfh )
      itmp2 = itmp1 /10
      i = itmp2 + 1
!  non-slvh fairing coefficient
      czi = cz ( i )
!  slvh fairing coefficient
      szi = 1.0 - czi
!  faired density
      ! fdlg = ( dlg1 * czi ) + ( dlg2 * szi )
      tmp1 = ( dlg1 * czi ) 
      tmp2 = ( dlg2 * szi )
      fdlg = tmp1 + tmp2
!  faired helium number density
      ! fdhel = ( dhel1 * czi ) + ( dhel2 * szi )
      tmp1 = ( dhel1 * czi )
      tmp2 = ( dhel2 * szi )
      fdhel = tmp1 + tmp2

    end subroutine cps_met88_fair5



    end module cps_atm_met88_mod
