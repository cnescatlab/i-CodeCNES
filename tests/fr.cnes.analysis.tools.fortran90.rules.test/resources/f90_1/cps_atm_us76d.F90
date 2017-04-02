module cps_atm_us76d_mod

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  cps_atm_us76d_mod
!
!$Resume
! Modele d'atmosphere US 76 avec dispersions possibles en temperature ou en densite
!
!$Description
! Modele d'atmosphere US 76 avec dispersions possibles en temperature ou en densite
!
!$Auteur
! Julien Bouillant (ATOS ORIGIN)
!
!$Version
!  $Id: cps_atm_us76d.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: cps_atm_us76d.F90,v $
!  Revision 1.16  2010/10/21 13:46:21  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.15  2010/10/07 13:51:47  ogarat
!  VERSION::AQ::07/10/2010:Suppression de variable inutilisée
!
!  Revision 1.14  2010/10/05 15:00:18  ogarat
!  VERSION::FA-ID:1432:05/10/2010:Modification du cas au limite alt<=0
!
!  Revision 1.13  2010/10/04 12:49:25  ogarat
!  VERSION::FA-ID:1432:04/10/2010:Mise a jour du test au limite (altitude <= 0km)
!
!  Revision 1.12  2010/06/17 08:30:04  jlrobin
!  VERSION::FA-ID:1411:17/06/2010:retrait des warning et densite nulle en hte altitude
!
!  Revision 1.11  2010/06/15 14:06:36  jlrobin
!  VERSION::FA-ID:1411:15/06/2010:gestion des cas aux altitudes limites
!
!  Revision 1.10  2009/03/11 18:00:18  cml
!  FA-ID 1154 : La correction de cette FA a ete faite dans le cadre de la FA 1225
!
!  Revision 1.9  2009/01/27 16:37:20  cml
!  FA-ID 1225 : Correction d un indicage et du typage de g0
!
!  Revision 1.8  2008/10/14 07:52:16  cml
!  DM-ID 1058 : Suppression cible inutile
!
!  Revision 1.7  2008/07/04 11:59:43  huec
!  DM-ID 1058 : Precision numerique : correction du parametre ttab
!
!  Revision 1.6  2007/11/05 12:40:39  jpi
!  DM-ID551 : coquilles (apostrophes, print inutiles)
!
!  Revision 1.5  2007/07/03 14:18:50  vivaresf
!  DM-ID 724 : gestion des erreurs
!
!  Revision 1.4  2007/06/27 10:24:12  tanguyy
!  DM-ID 724 : rajout d'un test sur l'altitude dans le cas du modele US 76
!
!  Revision 1.3  2006/05/30 15:26:41  vivaresf
!  regle de codage : utilisation de mage
!
!  Revision 1.2  2006/05/12 12:05:57  bouillaj
!  Amelioration qualite : complements sur les cartouches
!
!  Revision 1.1  2006/01/30 09:08:34  bouillaj
!  creation a partir de la routine MSPRO mp_atm_us76d
!
!
!$FinHistorique
!
!$Usage
!  use cps_atm_us76d_mod
!
!$Structure
!
!$Global
!
!$Common
!
!$Routines
!- cps_atm_us76d
!- cps_IO_e_us76
!
!$Fonctions
!
!$Include
!
!$Module
!#V
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
!.  cps_atm_us76d cps_IO_e_us76
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use mslib
  use msp_gestion_erreur
  
  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: cps_atm_us76d.F90 69 2012-09-11 08:33:34Z ffsm $'

  
contains
  
  subroutine cps_atm_us76d (delta_t,alt,dens,code_retour,delta_dens,vit_son,temp,pres,visco)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_atm_us76d
!
!$Resume
! Ce modele d'atmosphere est limite a des:
!             altitudes: 0<alt<1000 km 
!
!$Description
! Ce modele d'atmosphere est limite a des:
!             altitudes: 0<alt<1000 km 
!
!$Auteur
! Julien Bouillant (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_atm_us76d (delta_t,alt,dens,code_retour,[delta_dens],[vit_son],[temp],[pres],[visco])
!.    real(pm_reel),dimension(8) :: delta_t 
!.    real(pm_reel) :: alt 
!.    real(pm_reel) :: dens 
!.    type(tm_code_retour) :: code_retour 
!.    real(pm_reel) :: delta_dens 
!.    real(pm_reel) :: vit_son 
!.    real(pm_reel) :: temp 
!.    real(pm_reel) :: pres 
!.    real(pm_reel) :: visco 
!
!$Arguments
!>E     delta_t      :<pm_reel,DIM=(8)>   valeur des deltaTM a ajouter aux 8 valeur de TM
!                                         défini dans le modèle US76 pour les altitudes 
!                                         géopotentielle H0 et H7
!>E     alt          :<pm_reel>           altitude (m)
!>S     dens         :<pm_reel>           densité atmosphérique (kg.m-3)
!>S     code_retour  :<tm_code_retour>    structure pour le code retour
!>[E]   delta_dens   :<pm_reel>           delta pour la variation de densité atmosphérique
!>[S]   vit_son      :<pm_reel>           vitesse du son (m/s)
!>[S]   temp         :<pm_reel>           température (Kelvin)
!>[S]   pres         :<pm_reel>           pression (Pa)
!>[S]   visco        :<pm_reel>           viscosité dynamique (kg/m/s)
!
!$Common
!
!$Routines
!- cps_IO_e_us76
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
    
    
    ! Modules
    ! =======
    use mslib
    
    
    ! Declarations
    ! ============
    implicit none
    
    real(pm_reel),dimension(8),intent(in)   ::delta_t ! valeur des deltaTM a ajouter aux 8 valeur de TM
    ! defini dans le modele US76 pour les altitudes 
    ! geopotentielle H0 et H7
    
    real(pm_reel),intent(in)                   :: alt          ! altitude
    real(pm_reel),intent(out)                  :: dens         ! densite atmospherique
    type(tm_code_retour), intent(out)          :: code_retour 
    real(pm_reel), intent(in),  optional       :: delta_dens   ! delta pour la variation de densite atmospherique
    real(pm_reel), intent(out), optional       :: vit_son      ! vitesse du son
    real(pm_reel), intent(out), optional       :: temp         ! temperature
    real(pm_reel), intent(out), optional       :: pres         ! pression
    real(pm_reel), intent(out), optional       :: visco        ! viscosite dynamique
    
    ! Autres declarations
    ! ===================
    integer      :: i                ! indice 
    real(pm_reel),dimension(8),save :: delta_t_ref=1.e6_pm_reel   ! ecart de reference
    
    ! Declarations pour l'appel a la routine COMPAS cps_IO_e_us76
    integer      :: init      ! entier indiquant si on reinitialise les tables de temperature/pression (oui si !=0)
    real(pm_reel):: zgeo,temp_IO_e_us76,ro,vson   ! altitude, temperature, densite, vitesse du son
    real(pm_reel):: dro,pres_IO_e_us76,xmu        ! diff de densite / au profil nominal, pression, viscosite dynamique
    intrinsic present
    
    
    !************************************************************************
    
    ! initialisations
    ! ===============
    
    code_retour%valeur = pm_OK
    zgeo = alt

    ! verification des arguments d'entree
    ! ===================================
    
    if (zgeo <0._pm_reel) then ! altitude negative
       !       call MSP_signaler_message(cle_mes="CPS_WARN_US76",&
       !            partie_variable="Altitude < 0 km",&
       !            routine ="cps_atm_us76d")
       zgeo = 0._pm_reel

    end if
    
    

    ! calcul du modele atmospherique us76d
    ! ======================================
    
    ! pour les unites des donnees en entre et en sortie: 
    ! utilisation des commentaires en debut de code de cps_IO_e_us76
    
    
    
    
    do i = 1,8
       if ( (abs(delta_t_ref(i)-delta_t(i)) < 1.e-3_pm_reel) ) then ! si ecart sur les delta_t inferieur au millieme, 
          init=0                                                    ! pas de reinitialisation des tables.  
       else
          init=1
          go to 100
       end if
    end do

100 continue
    delta_t_ref(:)=delta_t(:) ! mise en memoire pour l'appel suivant
    
    
    if (.not.present(delta_dens))then
       dro=0._pm_reel
    else 
       dro=delta_dens
    end if
    
    call cps_IO_e_us76 (init,zgeo,delta_t,dro,temp_IO_e_us76,pres_IO_e_us76,ro,vson,xmu) ! pas de code retour en sortie
    
    
    ! affectation des sorties
    ! =======================
    
    dens = ro
    if (present(vit_son)) vit_son = vson
    if (present(temp)) temp = TEMP_IO_e_us76
    if (present(pres)) pres = PRES_IO_e_us76
    if (present(visco)) visco= xmu
    
    if (code_retour%valeur /= pm_OK) code_retour%message = ' '
    
  end subroutine cps_atm_us76d
  
  subroutine cps_IO_e_us76 (init,zgeo,deltat, dro,temp,pres,ro,vson,xmu)
    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_IO_e_us76
!
!$Resume
!     Modèle d atmosphère US76 avec  possibilité d y apporter des dispersions en température.
!
!$Description
!     Sous-programme caculant les caractéristiques atmosphériques \
!     à une altitude donnée d après le modèle standard US76 et avec \
!     possibilité d y apporter des dispersions en température.
!
!$Auteur
!      Julien Bouillant (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_IO_e_us76 (init,zgeo,deltat, dro,temp,pres,ro,vson,xmu)
!.    integer :: init
!.    real(kind=pm_reel) :: zgeo, dro
!.    real(kind=pm_reel) :: temp, pres, ro, vson, xmu
!.    real(kind=pm_reel),dimension(8) :: deltat
!
!$Arguments
!>E     init    :<integer>           entier indiquant si on réinitialise les \
!                              tables de température/pression (oui si 
!>E     zgeo    :<pm_reel>           altitude (m)
!>E/S   deltat  :<pm_reel,DIM=(8)>   
!>E     dro     :<pm_reel>           différence de densité par rapport au \
!                              profil nominal (kg/m3)
!>S     temp    :<pm_reel>           température (K)
!>S     pres    :<pm_reel>           pression (Pa)
!>S     ro      :<pm_reel>           densité (kg/m3)
!>S     vson    :<pm_reel>           vitesse du son (m/s)
!>S     xmu     :<pm_reel>           viscosité dynamique (kg/m/s)
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
    
    
    integer, intent(in) :: init
    real(kind=pm_reel), intent(in) :: zgeo, dro
    real(kind=pm_reel), intent(OUT) :: temp, pres, ro, vson, xmu
    
    integer, parameter ::nmax=140, n10max=14
    
    real(kind=pm_reel),dimension(13) ::  htab,ztab, dttab, xmm0tab,ttab
    real(kind=pm_reel),dimension(7)  ::  rmoltab
    real(kind=pm_reel),dimension(8)  ::  tmtab0,tmtab, ptab, deltat
    real(kind=pm_reel),dimension(nmax, 7)  :: rntab
    save ttab,dttab,ptab,cfp,tmtab
    
    
    integer :: l, ii, jj, k, kl, kk, i, j
    real(kind=pm_reel) :: tmol, hpot, xmol, rnn, zj, coeff, temp76, dt76
    real(kind=pm_reel) :: rcar, xi, rlambda, pen, rn, tc, gda, pta, cfp
    real(kind=pm_reel) :: r0, g0, xmol0, boltz, rstar, gama, beta, tsuth
    
    !   * r0 (m): rayon de la Terre utilise pour le calcul des altitudes
    !   *         geopotentielles
    !   * g0 (m/s**2): acceleration due a la gravite au niveau de la mer
    data r0,g0 / 6356.766d+03,9.80665_pm_reel /
    
    !   * xmol0 (kg/kmol): masse moleculaire de l'atmosphere au sol
    !   * boltz (N.m/K): constante de Boltzmann
    !   * rstar (N.m/(kmol.K)): constante des gaz parfaits
    !   * gama (sd): constante des gaz parfaits
    !   * beta (kg)/(s.m.K**0.5): constante pour le calcul de la vicosite
    !   *                         dynamique
    !   * tsuth (K): temperature de Sutherland
    
    data xmol0,boltz,rstar,gama,beta,tsuth / 28.9644d0,1.380622d-23, &
         8.31432d+03,1.4d0,1.458d-06,110.4d0 /
    
    !   * altitudes geopotentielles de debut et de fin des differents
    !   * gradients de temperature
    data htab / 0.d+03,11.d+03,20.d+03,32.d+03,47.d+03,51.d+03, &
         71.d+03,84.852d+03,0.d0,0.d0,0.d0,0.d0,0.d0 /
    
    !   * altitudes geometriques
    data ztab / 0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,86.d+03,91.d+03, &
         110.d+03,120.d+03,500.d+03,1000.d+03 /
    
    !   * temperatures correspondant a htab ou ztab
    data tmtab0 / 288.15d0,216.65d0,216.65d0,228.65d0,270.65d0, &
         270.65d0,214.65d0,186.946d0 /
    data ttab / 7*0.d0,186.8673d0,186.8673d0,240.d0,360.d0,999.24d0, &
         1000.d0 /
    
    !   * derivees de la temperature par rapport a hpot ou zgeo
    !     data dttab / -6.5d-03,0.d0,1.d-03,2.8d-03,0.d0,-2.8d-03,-2.0d-03,
    !    &             0.d0,0.d0,12.d-03,0.d0,0.d0,0.d0 /
    
    !   * pressions pour htab
    data ptab / 1.01325d+05,2.2632d+04,5.4748d+03,8.6801d+02, &
         1.1090d+02,6.6938d+01,3.9564d0,3.7338d-01 /
    
    !   * constantes necessaires au calcul de la temperature entre
    !   * 91 et 110 km
    data tc,gda,pta / 263.1905d0,-76.3232,-19.9429d+03 /
    
    !   * masses molaires des elements N2,O,O2,Ar,HE et H
    data rmoltab / 0.d0,28.0134d0,15.9994d0,31.9988d0,39.948d0, &
         4.0026d0,1.0080d0 /
    
    !   * coefficients correcteurs pour le calcul de la temperature
    !   * entre 80 et 86 km d'altitude
    data xmm0tab / 1.d0,0.999996d0,0.999989d0,0.999971d0,0.999941d0, &
         0.999909d0,0.999870d0,0.999829d0,0.999786d0, &
         0.999741d0,0.999694d0,0.999641d0,0.999579d0 /
    
    !   * composition de l'atmosphere en fonction des differents elements
    !   * qui la composent ... N2,O,O2,Ar,HE et H et en fonction de
    !   * l'altitude a partir de 86 km
    data ((rntab(i,j),j=1,7),i=1,9) / &
         86.0d+03,1.130d+20,8.600d+16,3.031d+19,1.351d+18,7.582d+14,0.d0, &
         86.5d+03,1.034d+20,9.939d+16,2.772d+19,1.236d+18,6.976d+14,0.d0, &
         87.0d+03,9.456d+19,1.147d+17,2.535d+19,1.130d+18,6.422d+14,0.d0, &
         87.5d+03,8.651d+19,1.320d+17,2.319d+19,1.033d+18,5.915d+14,0.d0, &
         88.0d+03,7.915d+19,1.513d+17,2.120d+19,9.437d+17,5.453d+14,0.d0, &
         88.5d+03,7.242d+19,1.724d+17,1.938d+19,8.624d+17,5.031d+14,0.d0, &
         89.0d+03,6.626d+19,1.952d+17,1.772d+19,7.880d+17,4.296d+14,0.d0, &
         89.5d+03,6.062d+19,2.193d+17,1.619d+19,7.918d+17,4.296d+14,0.d0, &
         90.0d+03,5.547d+19,2.443d+17,1.479d+19,6.574d+17,3.976d+14,0.d0/
    data ((rntab(i,j),j=1,7),i=10,19) / &
         90.5d+03,5.075d+19,2.699d+17,1.351d+19,6.002d+17,3.685d+14,0.d0, &
         91.0d+03,4.643d+19,2.953d+17,1.234d+19,5.478d+17,3.419d+14,0.d0, &
         91.5d+03,4.248d+19,3.200d+17,1.126d+19,4.998d+17,3.177d+14,0.d0, &
         92.0d+03,3.886d+19,3.434d+17,1.027d+19,4.557d+17,2.956d+14,0.d0, &
         92.5d+03,3.553d+19,3.651d+17,9.361d+18,4.152d+17,2.753d+14,0.d0, &
         93.0d+03,3.249d+19,3.846d+17,8.527d+18,3.781d+17,2.568d+14,0.d0, &
         93.5d+03,2.970d+19,4.016d+17,7.761d+18,4.441d+17,2.399d+14,0.d0, &
         94.0d+03,2.715d+19,4.159d+17,7.060d+18,3.129d+17,2.244d+14,0.d0, &
         94.5d+03,2.481d+19,4.275d+17,6.418d+18,2.844d+17,2.103d+14,0.d0, &
         95.0d+03,2.268d+19,4.365d+17,5.830d+18,2.583d+17,1.973d+14,0.d0/
    data ((rntab(i,j),j=1,7),i=20,29) / &
         95.5d+03,2.072d+19,4.429d+17,5.293d+18,2.345d+17,1.854d+14,0.d0, &
         96.0d+03,1.894d+19,4.471d+17,4.801d+18,2.127d+17,1.745d+14,0.d0, &
         96.5d+03,1.730d+19,4.493d+17,4.353d+18,1.928d+17,1.645d+14,0.d0, &
         97.0d+03,1.581d+19,4.500d+17,3.943d+18,1.746d+17,1.553d+14,0.d0, &
         97.5d+03,1.445d+19,4.494d+17,3.570d+18,1.581d+17,1.468d+14,0.d0, &
         98.0d+03,1.320d+19,4.476d+17,3.230d+18,1.430d+17,1.390d+14,0.d0, &
         98.5d+03,1.206d+19,4.447d+17,2.920d+18,1.292d+17,1.317d+14,0.d0, &
         99.0d+03,1.102d+19,4.408d+17,2.639d+18,1.167d+17,1.251d+14,0.d0, &
         99.5d+03,1.008d+19,4.358d+17,2.383d+18,1.053d+17,1.190d+14,0.d0, &
         100.0d+03,9.210d+18,4.298d+17,2.151d+18,9.501d+16,1.133d+14,0.d0/
    data ((rntab(i,j),j=1,7),i=30,39) / &
         101.0d+03,7.740d+18,4.168d+17,1.756d+18,7.735d+16,1.034d+14,0.d0, &
         102.0d+03,6.508d+18,4.007d+17,1.430d+18,6.279d+16,9.497d+13,0.d0, &
         103.0d+03,5.475d+18,3.821d+17,1.163d+18,5.082d+16,8.776d+13,0.d0, &
         104.0d+03,4.609d+18,3.619d+17,9.434d+17,4.101d+16,8.160d+13,0.d0, &
         105.0d+03,3.883d+18,3.406d+17,7.645d+17,3.299d+16,7.633d+13,0.d0, &
         106.0d+03,3.273d+18,3.188d+17,6.189d+17,2.645d+16,7.181d+13,0.d0, &
         107.0d+03,2.760d+18,2.968d+17,5.005d+17,2.113d+16,6.789d+13,0.d0, &
         108.0d+03,2.327d+18,2.748d+17,4.045d+17,1.681d+16,6.443d+13,0.d0, &
         109.0d+03,1.959d+18,2.528d+17,3.263d+17,1.331d+16,6.128d+13,0.d0, &
         110.0d+03,1.641d+18,2.303d+17,2.621d+17,1.046d+16,5.821d+13,0.d0/
    data ((rntab(i,j),j=1,7),i=40,49) / &
         111.0d+03,1.373d+18,2.083d+17,2.104d+17,8.200d+15,5.526d+13,0.d0, &
         112.0d+03,1.158d+18,1.889d+17,1.706d+17,6.481d+15,5.271d+13,0.d0, &
         113.0d+03,9.841d+17,1.718d+17,1.398d+17,5.169d+15,5.044d+13,0.d0, &
         114.0d+03,8.422d+17,1.565d+17,1.156d+17,4.162d+15,4.838d+13,0.d0, &
         115.0d+03,7.254d+17,1.428d+17,9.646d+16,3.386d+15,4.648d+13,0.d0, &
         116.0d+03,6.285d+17,1.305d+17,8.120d+16,2.779d+15,4.473d+13,0.d0, &
         117.0d+03,5.475d+17,1.194d+17,6.891d+16,2.301d+15,4.310d+13,0.d0, &
         118.0d+03,4.794d+17,1.096d+17,5.892d+16,1.920d+15,4.160d+13,0.d0, &
         119.0d+03,4.217d+17,1.007d+17,5.072d+16,1.614d+15,4.019d+13,0.d0, &
         120.0d+03,3.726d+17,9.275d+16,4.395d+16,1.366d+15,3.888d+13,0.d0/
    data ((rntab(i,j),j=1,7),i=50,59) / &
         121.0d+03,3.306d+17,8.562d+16,3.832d+16,1.164d+15,3.766d+13,0.d0, &
         122.0d+03,2.947d+17,7.925d+16,3.360d+16,9.979d+14,3.652d+13,0.d0, &
         123.0d+03,2.637d+17,7.354d+16,2.963d+16,8.606d+14,3.547d+13,0.d0, &
         124.0d+03,2.368d+17,6.840d+16,2.625d+16,7.460d+14,3.448d+13,0.d0, &
         125.0d+03,2.135d+17,6.376d+16,2.336d+16,6.498d+14,3.356d+13,0.d0, &
         126.0d+03,1.930d+17,5.956d+16,2.087d+16,5.685d+14,3.270d+13,0.d0, &
         127.0d+03,1.750d+17,5.576d+16,1.871d+16,4.994d+14,3.189d+13,0.d0, &
         128.0d+03,1.592d+17,5.229d+16,1.683d+16,4.403d+14,3.112d+13,0.d0, &
         129.0d+03,1.451d+17,4.914d+16,1.519d+16,3.896d+14,3.040d+13,0.d0, &
         130.0d+03,1.326d+17,4.625d+16,1.375d+16,3.458d+14,2.972d+13,0.d0/
    data ((rntab(i,j),j=1,7),i=60,69) / &
         131.0d+03,1.215d+17,4.361d+16,1.247d+16,3.078d+14,2.907d+13,0.d0, &
         132.0d+03,1.116d+17,4.118d+16,1.134d+16,2.748d+14,2.846d+13,0.d0, &
         133.0d+03,1.026d+17,3.894d+16,1.034d+16,2.460d+14,2.787d+13,0.d0, &
         134.0d+03,9.460d+16,3.688d+16,9.444d+15,2.207d+14,2.732d+13,0.d0, &
         135.0d+03,8.735d+16,3.497d+16,8.645d+15,1.985d+14,2.679d+13,0.d0, &
         136.0d+03,8.080d+16,3.320d+16,7.927d+15,1.789d+14,2.629d+13,0.d0, &
         137.0d+03,7.487d+16,3.156d+16,7.283d+15,1.616d+14,2.581d+13,0.d0, &
         138.0d+03,6.947d+16,3.004d+16,6.702d+15,1.463d+14,2.535d+13,0.d0, &
         139.0d+03,6.456d+16,2.862d+16,6.177d+15,1.326d+14,2.491d+13,0.d0, &
         140.0d+03,6.009d+16,2.729d+16,5.702d+15,1.205d+14,2.449d+13,0.d0/
    data ((rntab(i,j),j=1,7),i=70,79) / &
         141.0d+03,5.600d+16,2.605d+16,5.272d+15,1.096d+14,2.408d+13,0.d0, &
         142.0d+03,5.225d+16,2.489d+16,4.881d+15,9.989d+13,2.369d+13,0.d0, &
         143.0d+03,4.881d+16,2.380d+16,4.524d+15,9.118d+13,2.332d+13,0.d0, &
         144.0d+03,4.565d+16,2.278d+16,4.199d+15,8.335d+13,2.296d+13,0.d0, &
         145.0d+03,4.275d+16,2.183d+16,3.903d+15,7.630d+13,2.261d+13,0.d0, &
         146.0d+03,4.007d+16,2.092d+16,3.631d+15,6.994d+13,2.228d+13,0.d0, &
         147.0d+03,3.760d+16,2.007d+16,3.382d+15,6.420d+13,2.196d+13,0.d0, &
         148.0d+03,3.531d+16,1.927d+16,3.153d+15,5.900d+13,2.165d+13,0.d0, &
         149.0d+03,3.320d+16,1.852d+16,2.943d+15,5.428d+13,2.135d+13,0.d0, &
         150.0d+03,3.124d+16,1.780d+16,2.750d+15,5.000d+13,2.106d+13,3.167d+11/
    data ((rntab(i,j),j=1,7),i=80,89) / &
         155.0d+03,2.333d+16,1.475d+16,1.984d+15,3.368d+13,1.974d+13,3.283d+11, &
         160.0d+03,1.774d+16,1.238d+16,1.460d+15,2.321d+13,1.861d+13,2.911d+11, &
         165.0d+03,1.369d+16,1.050d+16,1.092d+15,1.630d+13,1.763d+13,2.619d+11, &
         170.0d+03,1.070d+16,8.996d+15,8.277d+14,1.163d+13,1.676d+13,2.386d+11, &
         175.0d+03,8.452d+15,7.765d+15,6.350d+14,8.417d+12,1.599d+13,2.197d+11, &
         180.0d+03,6.740d+15,6.747d+15,4.921d+14,6.162d+12,1.530d+13,2.041d+11, &
         185.0d+03,5.417d+15,5.897d+15,3.847d+14,4.558d+12,1.467d+13,1.911d+11, &
         190.0d+03,4.385d+15,5.181d+15,3.031d+14,3.401d+12,1.410d+13,1.802d+11, &
         195.0d+03,3.572d+15,4.572d+15,2.404d+14,2.558d+12,1.358d+13,1.709d+11, &
         200.0d+03,2.925d+15,4.050d+15,1.918d+14,1.938d+12,1.310d+13,1.630d+11/
    data ((rntab(i,j),j=1,7),i=90,99) / &
         205.0d+03,2.407d+15,3.600d+15,1.538d+14,1.477d+12,1.266d+13,1.561d+11, &
         210.0d+03,1.989d+15,3.211d+15,1.239d+14,1.131d+12,1.224d+13,1.501d+11, &
         215.0d+03,1.650d+15,2.871d+15,1.003d+14,8.711d+11,1.185d+13,1.448d+11, &
         220.0d+03,1.373d+15,2.573d+15,8.145d+13,6.737d+11,1.149d+13,1.402d+11, &
         225.0d+03,1.147d+15,2.312d+15,6.637d+13,5.230d+11,1.115d+13,1.361d+11, &
         230.0d+03,9.600d+14,2.081d+15,5.425d+13,4.075d+11,1.083d+13,1.324d+11, &
         235.0d+03,8.058d+14,1.876d+15,4.446d+13,3.185d+11,1.052d+13,1.291d+11, &
         240.0d+03,6.778d+14,1.695d+15,3.653d+13,2.497d+11,1.023d+13,1.261d+11, &
         245.0d+03,5.714d+14,1.533d+15,3.008d+13,1.962d+11,9.953d+12,1.234d+11, &
         250.0d+03,4.826d+14,1.388d+15,2.482d+13,1.546d+11,9.690d+12,1.210d+11/
    data ((rntab(i,j),j=1,7),i=100,109) / &
         255.0d+03,4.082d+14,1.259d+15,2.052d+13,1.221,9.438,1.188d+11, &
         260.0d+03,3.459d+14,1.143d+15,1.700d+13,9.658d+10,9.196d+12,1.167d+11, &
         265.0d+03,2.935d+14,1.039d+15,1.410d+13,7.655d+10,8.965d+12,1.148d+11, &
         270.0d+03,2.494d+14,9.447d+14,1.171d+13,6.078d+10,8.743d+12,1.131d+11, &
         275.0d+03,2.121d+14,8.599d+14,9.739d+12,4.834d+10,8.529d+12,1.115d+11, &
         280.0d+03,1.806d+14,7.834d+14,8.110d+12,3.850d+10,8.322d+12,1.100d+11, &
         285.0d+03,1.540d+14,7.143d+14,6.761d+12,3.070d+10,8.124d+12,1.086d+11, &
         290.0d+03,1.314d+14,6.516d+14,5.643d+12,2.451d+10,7.931d+12,1.073d+11, &
         295.0d+03,1.122d+14,5.948d+14,4.714d+12,1.960d+10,7.746d+12,1.060d+11, &
         300.0d+03,9.593d+13,5.433d+14,3.942d+12,1.568d+10,7.566d+12,1.049d+11/
    data ((rntab(i,j),j=1,7),i=110,119) / &
         310.0d+03,7.024d+13,4.540d+14,2.763d+12,1.007d+10,7.224d+12,1.027d+11, &
         320.0d+03,5.158d+13,3.800d+14,1.942d+12,6.493d+09,6.901d+12,1.008d+11, &
         330.0d+03,3.796d+13,3.186d+14,1.369d+12,4.199d+09,6.597d+12,9.903d+10, &
         340.0d+03,2.800d+13,2.675d+14,9.674d+11,2.723d+09,6.310d+12,9.741d+10, &
         350.0d+03,2.069d+13,2.249d+14,6.850d+11,1.771d+09,6.038d+12,9.591d+10, &
         360.0d+03,1.532d+13,1.893d+14,4.859d+11,1.154d+09,5.779d+12,9.450d+10, &
         370.0d+03,1.136d+13,1.594d+14,3.454d+11,7.536d+08,5.534d+12,9.318d+10, &
         380.0d+03,8.434d+12,1.344d+14,2.459d+11,4.932d+08,5.301d+12,9.193d+10, &
         390.0d+03,6.271d+12,1.135d+14,1.753d+11,3.234d+08,5.079d+12,9.074d+10, &
         400.0d+03,4.669d+12,9.584d+13,1.252d+11,2.124d+08,4.868d+12,8.960d+10/
    data ((rntab(i,j),j=1,7),i=120,129) / &
         410.0d+03,3.480d+12,8.101d+13,8.948d+10,1.397d+08,4.666d+12,8.851d+10, &
         420.0d+03,2.597d+12,6.853d+13,6.406d+10,9.207d+07,4.474d+12,8.745d+10, &
         430.0d+03,1.940d+12,5.800d+13,4.592d+10,6.076d+07,4.290d+12,8.643d+10, &
         440.0d+03,1.451d+12,4.913d+13,3.295d+10,4.016d+07,4.115d+12,8.544d+10, &
         450.0d+03,1.086d+12,4.164d+13,2.638d+10,2.658d+07,3.948d+12,8.448d+10, &
         460.0d+03,8.142d+11,3.531d+13,1.703d+10,1.762d+07,3.788d+12,8.354d+10, &
         470.0d+03,6.107d+11,2.996d+13,1.226d+10,1.169d+07,3.635d+12,8.263d+10, &
         480.0d+03,4.585d+11,2.543d+13,8.839d+09,7.771d+06,3.489d+12,8.173d+10, &
         490.0d+03,3.446d+11,2.160d+13,6.378d+09,5.171d+06,3.349d+12,8.085d+10, &
         500.0d+03,2.592d+11,1.836d+13,4.607d+09,3.445d+06,3.215d+12,8.000d+10/
    data ((rntab(i,j),j=1,7),i=130,140) / &
         510.0d+03,1.951d+11,1.561d+13,3.331d+09,2.299d+06,3.087d+12,7.918d+10, &
         520.0d+03,1.470d+11,1.328d+13,2.411d+09,1.535d+06,2.965d+12,7.838d+10, &
         530.0d+03,1.109d+11,1.130d+13,1.747d+09,1.027d+06,2.848d+12,7.758d+10, &
         540.0d+03,8.370d+10,9.624d+12,1.267d+09,6.875d+05,2.735d+12,7.680d+10, &
         550.0d+03,6.323d+10,8.200d+12,9.196d+08,4.609d+05,2.628d+12,7.602d+10, &
         600.0d+03,1.575d+10,3.707d+12,1.880d+08,6.351d+04,2.154d+12,7.231d+10, &
         650.0d+03,4.003d+09,1.695d+12,3.932d+07,9.006d+03,1.771d+12,6.883d+10, &
         700.0d+03,1.038d+09,7.840d+11,8.410d+06,1.313d+03,1.461d+12,6.556d+10, &
         750.0d+03,2.741d+08,3.666d+11,1.838d+06,1.967d+02,1.208d+12,6.249d+10, &
         800.0d+03,7.377d+07,1.732d+11,4.105d+05,3.027d+01,1.001d+12,5.961d+10, &
         100.0d+04,4.626d+05,9.562d+09,1.251d+03,2.188d-02,4.850d+11,4.967d+10/
    
    !     Test si zgeo > 1000 km:
    !     ======================
    
    if (zgeo.gt.1000.d+03) then
       
       !        write(6,1000)
       ! 1000   format('Altitude superieure a 1000 km ...',/,
       !    &'  Calcul des conditions atmospheriques impossible !',/)
       !        stop
       temp = 0.D0
       pres = 0.D0
       ro   = 0.D0
       vson = 0.D0
       xmu  = 0.D0

       !call MSP_signaler_message(cle_mes="CPS_WARN_US76", &
       !     routine="cps_IO_e_us76", partie_variable="Altitude > 1000 km")

       goto 999
       
    endif
       
    !     Reactualisation des tables:
    !     ==========================
    
    if (init.ne.0) then
       do  i=1,8
          tmtab(i)=tmtab0(i)+deltat(i)
       enddo
       do  i=1,7
          dttab(i)=(tmtab(i+1)-tmtab(i))/(htab(i+1)-htab(i))
          tmol=tmtab(i+1)
          hpot=htab(i+1)
          if (dabs(dttab(i)).lt.1.d-06) then
             ptab(i+1)=ptab(i)* &
                  dexp(-g0*xmol0*(hpot-htab(i))/(rstar*tmtab(i)))
          else
             ptab(i+1)=ptab(i)*(tmtab(i)/tmol)** &
                  (g0*xmol0/(rstar*dttab(i)))
          end if
       enddo
       dttab(10)=12.d-03
       !           Calcul des "ni" pour chaque element N2,O,O2,Ar,He et H:
       xmol=0.d0
       rn=0.d0
       do  l=2,7
          rnn=rntab(1,l)
          xmol=xmol+rnn*rmoltab(l)
          rn=rn+rnn
       enddo
       xmol=xmol/rn
       temp=tmtab(8)*xmol/xmol0
       !           Calcul de la pression:
       pres=rn*boltz*temp
       cfp=ptab(8)/pres
    end if
       
    
    !     Calcul de temp,pres et xmol pour zgeo < 86 km:
    !     =============================================
    
    if (zgeo.lt.86.d+03) then
       
       !        Calcul de l'altitude geopotentielle:
       hpot=r0*zgeo/(r0+zgeo)
       do i=2,7
          if (hpot.lt.htab(i)) then
             ii=i-1
             goto 150
          end if
       enddo
       ii=7
150    continue
       !        Calcul de la temperature molaire:
       tmol=tmtab(ii)+dttab(ii)*(hpot-htab(ii))
       !        Calcul de la temperature cinetique et de la masse molaire:
       if (zgeo.lt.80.d+03) then
          temp=tmol
          xmol=xmol0
       else
          do  j=2,12
             zj=80000.d0+j*500.d0
             if (zgeo.le.zj) then
                jj=j-1
                goto 250
             end if
          enddo
          jj=12
250       continue
          coeff=xmm0tab(jj)+(xmm0tab(jj+1)-xmm0tab(jj))/500.d0* &
               (zgeo-zj+500.d0)
          temp=tmol*coeff
          xmol=xmol0*coeff
       end if

       !        Calcul de la pression:
       if (dabs(dttab(ii)).lt.1.d-06) then
          pres=ptab(ii)* &
               dexp(-g0*xmol0*(hpot-htab(ii))/(rstar*tmtab(ii)))
       else
          pres=ptab(ii)*(tmtab(ii)/tmol)** &
               (g0*xmol0/(rstar*dttab(ii)))
       end if
       
       !     Calcul de temp,pres et xmol pour zgeo >= 86 km:
       !     ==============================================
       
    else
       
       !        Calcul de la temperature cinetique:
       if (zgeo.lt.ztab(9)) then
          temp76=ttab(8)
          dt76=0.d0
       else if (zgeo.lt.ztab(10)) then
          rcar=dsqrt(1.d0-((zgeo-ztab(9))/pta)**2)
          temp76=tc+gda*rcar
          dt76=-(gda/pta)*((zgeo-ztab(9))/pta)/rcar
       else if (zgeo.lt.ztab(11)) then
          temp76=ttab(10)+dttab(10)*(zgeo-ztab(10))
          dt76=dttab(10)
       else
          xi=(zgeo-ztab(11))*(r0+ztab(11))/(r0+zgeo)
          rlambda=dttab(10)/(ttab(13)-ttab(11))
          temp76=ttab(13)-(ttab(13)-ttab(11))*dexp(-rlambda*xi)
          dt76=rlambda*(ttab(13)-ttab(11))*dexp(-rlambda*xi)
          dt76=dt76*((r0+ztab(11))/(r0+zgeo))**2
       end if

       !        Calcul des "ni" pour chaque element N2,O,O2,Ar,He et H:
       do  k=1,n10max
          if (zgeo.le.rntab(10*k,1)) then
             kl=10*(k-1)
             do  l=1,10
                if (zgeo.le.rntab(kl+l,1)) then
                   kk=kl+(l-1)
                   goto 350
                end if
             enddo
          end if
       enddo
       temp = 0.D0
       pres = 0.D0
       ro   = 0.D0
       vson = 0.D0
       xmu  = 0.D0

       goto 999


350    continue
       !        Calcul de xmol:
       xmol=0.d0
       rn=0.d0
       do  l=2,7
          pen=(rntab(kk+1,l)-rntab(kk,l))/(rntab(kk+1,1)-rntab(kk,1))
          rnn=rntab(kk,l)+pen*(zgeo-rntab(kk,1))
          xmol=xmol+rnn*rmoltab(l)
          rn=rn+rnn
       enddo
       xmol=xmol/rn
       temp=temp76+deltat(8)*xmol/xmol0
       !        Calcul de la pression:
       pres=cfp*rn*boltz*temp
       
    end if
    
    !     Calcul de la densite:
    !     ====================
    ro=(pres*xmol)/(rstar*temp)
    
    !     Modulation des parametres atmospheriques en fonction de dro:
    !     ===========================================================
    
    temp=temp*(1.d0+dro/ro)
    ro=ro+dro
    
    !     Calcul de la vitesse du son:
    !===========================
    vson=dsqrt(gama*rstar*temp/xmol)
    
    !     Calcul de la viscosite:
    !     ======================
    xmu=(beta*temp**1.5)/(temp+tsuth)
    
999 return

  end subroutine  cps_IO_e_us76
  
end module cps_atm_us76d_mod

