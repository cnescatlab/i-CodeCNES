MODULE MSP_ALEATOIRE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  MSP_ALEATOIRE
!
!$Resume
!  Module contenant des outils de génération de nombre aléatoire
!
!$Description
!  Module contenant des outils de génération de nombre aléatoire
!
!$Auteur
!  J.-J. WASBAUER
!
!$Version
!  $Id: MSP_ALEATOIRE.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: MSP_ALEATOIRE.F90,v $
!  Revision 1.12  2010/10/20 09:35:42  mercadig
!  VERSION::AQ::20/10/2010:Ajout du marqueur de fin historique dans le cartouche
!
!  Revision 1.11  2008/11/19 13:29:11  mercadig
!  DM-ID 733 : Mise a jour cartouche
!
!  Revision 1.10  2006/11/15 10:09:35  tanguyy
!  AQ : mise a jour des commentaires dans les cartouches
!  Revision 1.9  2006/11/09 09:13:53  mle
!  DM-ID 487 : noms des parameter dans MECASPA
!  Revision 1.8  2006/06/02 11:21:52  vpg
!  DM-ID 232 : qualite. Nommage des arguments optionnels lors des appels de fonctions et de routines
!  Revision 1.7  2005/03/08 07:32:33  fabrec
!  DM-ID 111 : mise à jour des cartouches
!  Revision 1.6  2004/07/12 17:51:53  vivaresf
!  Passage Foresys : typage coherent
!  Revision 1.5  2003/03/20 17:45:33  adm_ipsi
!  Ajout du paramètre ini_seed sur plusieurs fonctions
!  Revision 1.4  2003/01/08 15:12:38  adm_ipsi
!   Explicitation des conversions de type implicites
!  Revision 1.3  2002/12/04 18:08:24  adm_ipsi
!  Utilisation du parametre NB_LONG_CHAINE
!  Revision 1.2  2002/12/03 17:21:00  adm_ipsi
!   Ajout de implicit none
!  Revision 1.1.1.1  2002/09/30 14:09:35  adm_ipsi
!  Industrialisation de la MECASPA sans les modules de gestion d'erreurs
!  Revision 1.3  2000/06/13 11:16:02  util_am
!  Mise à jour des cartouches : sections Voir-Aussi, Mots-cles
!  Revision 1.2  1999/09/22 14:46:41  util_am
!  Mise a jour des cartouches
!  Revision 1.1  1999/09/03 16:42:37  util_am
!  MSP_ACCES.F90 :Ajout de routine pour la connexion et la deconnexion de section MADONA
!  MSP_BULLETIN_DEF.F90 : Ajout de MSP_modifier_bulletin, MSP_lire_BULLETIN
!  MECASPA.F90 : Ajout des use aux nouveaux modules
!
!$FinHistorique
!
!$Usage
!  use MSP_ALEATOIRE
!
!$Structure
!
!$Global
!
!$Common
!
!$Routines
!- MSP_gaussn
!- MSP_inivar
!- MSP_var_08
!- MSP_varuni
!
!$Fonctions
!- MSP_acorn
!
!$Include
!
!$Module
!#V
!- MSLIB
!- MSP_MECASPA_DEF
!#
!
!$Interface
!#V
!#
!
!$Remarques
!
!$Mots-cles
!  NOMBRE ALEATOIRE 
!
!$Voir-Aussi
!.  MSP_acorn MSP_gaussn MSP_inivar MSP_var_08 MSP_varuni
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use MSLIB, only : PM_REEL
  use MSP_MECASPA_DEF

  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: MSP_ALEATOIRE.F90 69 2012-09-11 08:33:34Z ffsm $'


contains


  real(KIND=PM_REEL) FUNCTION MSP_acorn (korder, xv, maxop1) result(ran_number)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_acorn
!
!$Resume
!  Cette fonction génère un nombre aléatoire uniforme entre 0 et 1
!
!$Description
!  Cette fonction génère un nombre aléatoire uniforme entre 0 et 1
!
!$Auteur
!  J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  ran_number = MSP_acorn (korder, xv, maxop1)
!.    integer :: korder, maxop1
!.    real (KIND=PM_REEL) :: xv(maxop1)
!
!$Arguments
!>E     korder      :<integer>                ordre de la fonction génératrice
!>S     xv          :<PM_REEL,DIM=(maxop1)>   tableau contenant l'historique
!>E     maxop1      :<integer>                taille du tableau xv
!>S     ran_number  :<PM_REEL>                Valeur du nombre aléatoire
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
!  NOMBRE ALEATOIRE 
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
   implicit none

    integer, intent(in) :: korder, maxop1
    real (KIND=PM_REEL), intent(out) :: xv(maxop1)
    integer :: i

    do i = 1, korder
       xv(i+1)= (xv(i+1) + xv(i)) - real(int(xv(i+1)+xv(i)),kind=pm_reel)
    end do
    ran_number=xv(korder+1)

  end FUNCTION MSP_acorn



  SUBROUTINE MSP_gaussn (xm, sigma, val, xsol)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_gaussn
!
!$Resume
!  Cette routine permet de générer un nombre aléatoire suivant une loi gaussienne
!
!$Description
!  Cette routine permet de générer un nombre aléatoire suivant une loi gaussienne
!
!$Auteur
!  J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_gaussn (xm, sigma, val, xsol)
!.    real (KIND=PM_REEL) :: xm, sigma, val, xsol
!
!$Arguments
!>E/S   xm     :<PM_REEL>   Moyenne de la gaussienne
!>E/S   sigma  :<PM_REEL>   Sigma de la gaussienne
!>E/S   val    :<PM_REEL>   
!>E/S   xsol   :<PM_REEL>   valeur du nombre aléatoire
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
!  NOMBRE ALEATOIRE 
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   implicit none


    real (KIND=PM_REEL) :: xm, sigma, val, xsol
    integer :: i
    real (KIND=PM_REEL), dimension(78) :: x, fx
    data (x(i),i=1,78)/-4.5_PM_REEL,-4._PM_REEL,-3.8_PM_REEL,-3.6_PM_REEL,-3.5_PM_REEL,-3.4_PM_REEL,-3.3_PM_REEL, &
         -3.2_PM_REEL,-3.1_PM_REEL,-3._PM_REEL, -2.9_PM_REEL,-2.8_PM_REEL,-2.7_PM_REEL,-2.6_PM_REEL,-2.5_PM_REEL, &
         -2.4_PM_REEL,-2.3_PM_REEL,-2.2_PM_REEL,-2.1_PM_REEL,-2.0_PM_REEL, -1.9_PM_REEL,-1.8_PM_REEL,-1.7_PM_REEL,&
         -1.6_PM_REEL,-1.5_PM_REEL,-1.4_PM_REEL,-1.3_PM_REEL,-1.2_PM_REEL,-1.1_PM_REEL,-1.0_PM_REEL, &
         -0.9_PM_REEL,-0.8_PM_REEL,-0.7_PM_REEL,-0.6_PM_REEL,-0.5_PM_REEL,-0.4_PM_REEL,-0.3_PM_REEL,-0.2_PM_REEL,&
         -0.1_PM_REEL,0.0_PM_REEL, 0.1_PM_REEL,0.2_PM_REEL,0.3_PM_REEL,0.4_PM_REEL,0.5_PM_REEL,0.6_PM_REEL,&
         0.7_PM_REEL,0.8_PM_REEL,0.9_PM_REEL,1._PM_REEL,1.1_PM_REEL,1.2_PM_REEL,1.3_PM_REEL,1.4_PM_REEL,1.5_PM_REEL,&
         1.6_PM_REEL,1.7_PM_REEL,1.8_PM_REEL,1.9_PM_REEL,2._PM_REEL, &
         2.1_PM_REEL,2.2_PM_REEL,2.3_PM_REEL,2.4_PM_REEL,2.5_PM_REEL,2.6_PM_REEL,2.7_PM_REEL,2.8_PM_REEL,2.9_PM_REEL,3._PM_REEL, &
         3.1_PM_REEL,3.2_PM_REEL,3.3_PM_REEL,3.4_PM_REEL,3.6_PM_REEL,3.8_PM_REEL,4._PM_REEL,4.5_PM_REEL/

    data (fx(i),i=1,78)/3.e-6_PM_REEL,32.e-6_PM_REEL,72.e-6_PM_REEL,1.59e-4_PM_REEL,2.4e-4_PM_REEL, &
         3.4e-4_PM_REEL,4.8e-4_PM_REEL, &
         6.9e-4_PM_REEL,9.6e-4_PM_REEL,1.35e-3_PM_REEL,1.9e-3_PM_REEL,2.6e-3_PM_REEL,3.5e-3_PM_REEL,4.7e-3_PM_REEL,6.2e-3_PM_REEL, &
         8.2e-3_PM_REEL,1.07e-2_PM_REEL,1.39e-2_PM_REEL,1.79e-2_PM_REEL,2.28e-2_PM_REEL,2.87e-2_PM_REEL,3.59e-2_PM_REEL, &
         4.46e-2_PM_REEL,5.48e-2_PM_REEL,6.68e-2_PM_REEL,8.08e-2_PM_REEL,9.68e-2_PM_REEL,1.151e-1_PM_REEL,1.357e-1_PM_REEL, &
         1.587e-1_PM_REEL,1.841e-1_PM_REEL,2.119e-1_PM_REEL,2.42e-1_PM_REEL,2.743e-1_PM_REEL,3.085e-1_PM_REEL,3.446e-1_PM_REEL, &
         3.821e-1_PM_REEL,4.207e-1_PM_REEL,4.602e-1_PM_REEL,0.5_PM_REEL,5.398e-1_PM_REEL,5.793e-1_PM_REEL,6.179e-1_PM_REEL, &
         6.554e-1_PM_REEL,6.915e-1_PM_REEL,7.257e-1_PM_REEL,7.58e-1_PM_REEL,7.881e-1_PM_REEL,8.159e-1_PM_REEL,8.413e-1_PM_REEL, &
         8.643e-1_PM_REEL,8.849e-1_PM_REEL,9.032e-1_PM_REEL,9.192e-1_PM_REEL,9.332e-1_PM_REEL,9.452e-1_PM_REEL,9.554e-1_PM_REEL, &
         9.641e-1_PM_REEL,9.713e-1_PM_REEL,9.772e-1_PM_REEL,9.821e-1_PM_REEL,9.861e-1_PM_REEL,9.893e-1_PM_REEL,9.918e-1_PM_REEL, &
         9.938e-1_PM_REEL,9.953e-1_PM_REEL,9.965e-1_PM_REEL,9.974e-1_PM_REEL,9.981e-1_PM_REEL,9.9865e-1_PM_REEL, &
         9.9904e-1_PM_REEL,9.9931e-1_PM_REEL,9.9952e-1_PM_REEL,9.9966e-1_PM_REEL,9.99841e-1_PM_REEL,.999928_PM_REEL, &
         .999968_PM_REEL,.999997_PM_REEL/
    !
    if (val >= fx(78)) then
       xsol = 4.5_PM_REEL
    else if (val.le.fx(1)) then
       xsol = -4.5_PM_REEL
    else
       do i = 2, 77
          if (val < fx(i)) exit
       end do
       xsol=(val-fx(i-1))*(x(i)-x(i-1))/(fx(i)-fx(i-1))+x(i-1)
    end if
    xsol = xsol*sigma+xm

  end SUBROUTINE MSP_gaussn


  SUBROUTINE MSP_inivar (korder, xv, maxop1, ini_seed)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_inivar
!
!$Resume
!  Routine d'initialisation du tableau xv
!
!$Description
!  Routine d'initialisation du tableau xv
!
!$Auteur
!  J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_inivar (korder, xv, maxop1, [ini_seed])
!.    integer :: korder, maxop1
!.    real (KIND=PM_REEL) :: xv(maxop1)
!.    integer :: ini_seed
!
!$Arguments
!>E/S   korder    :<integer>                taille utile du tableau xv
!>E/S   xv        :<PM_REEL,DIM=(maxop1)>   tableau à initialiser
!>E/S   maxop1    :<integer>                taille du tableau xv (déclaration)
!>[E]   ini_seed  :<integer>                argument optionnel permettant d'initialiser le tableau à une valeur fixe 
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
!  NOMBRE ALEATOIRE 
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   implicit none

    integer :: korder, maxop1
    real (KIND=PM_REEL) :: xv(maxop1)
    integer, optional, intent(IN) :: ini_seed

    real   ::  secnds, temps, top

    integer :: i

    temps = 0.

    if ( present(ini_seed) ) then
       top = real(ini_seed,kind=4)
    else
       top = secnds (temps)
    endif
    temps = MODULO(top, 80.) / 100. + 0.1
    xv(1)=dble(temps)
    do  i=1,korder
       xv(i+1)=0._PM_REEL
    end do

  end SUBROUTINE MSP_inivar



  SUBROUTINE MSP_var_08(xm,sigma,xsol,ini_seed)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_var_08
!
!$Resume
!  Programme générant une variable aléatoire gaussienne de moyenne
!  et d'écart type donné.
!
!$Description
!  Programme générant une variable aléatoire gaussienne de moyenne
!  et d'écart type donné.
!
!$Auteur
!  J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_var_08(xm,sigma,xsol,[ini_seed])
!.    real (KIND=PM_REEL) :: xm, sigma, xsol
!.    integer :: ini_seed
!
!$Arguments
!>E/S   xm        :<PM_REEL>   moyenne de la Gaussienne
!>E/S   sigma     :<PM_REEL>   Ecrat type de la Gaussienne
!>E/S   xsol      :<PM_REEL>   Valeur du nombre aléatoire généré
!>[E]   ini_seed  :<integer>   argument optionnel permettant de générer une valeur fixe 
!
!$Common
!
!$Routines
!- MSP_inivar
!- MSP_gaussn
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  NOMBRE ALEATOIRE 
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   implicit none

    real (KIND=PM_REEL) :: xm, sigma, xsol
    integer, optional, intent(IN) :: ini_seed

    integer, parameter :: maxord = 12
    integer, parameter :: maxop1 = maxord + 1
    integer :: ia, korder

    real (KIND=PM_REEL) :: val, xv(maxop1)

    data ia/0/korder/8/
    save ia, xv

    if(ia == 0)then
       call MSP_inivar(korder,xv,maxop1, ini_seed=ini_seed)
       ia=ia+1
    end if
    val=MSP_acorn(korder,xv,maxop1)
    call MSP_gaussn(xm,sigma,val,xsol)

  end SUBROUTINE MSP_var_08


  SUBROUTINE MSP_varuni (xsol,ini_seed)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_varuni
!
!$Resume
!  Routine générant une variable aléatoire uniforme entre 0 et 1
!
!$Description
!  Routine générant une variable aléatoire uniforme entre 0 et 1
!
!$Auteur
!  J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_varuni (xsol,[ini_seed])
!.    real (KIND=PM_REEL) :: xsol
!.    integer :: ini_seed
!
!$Arguments
!>E/S   xsol      :<PM_REEL>   Valeur de la variable aléatoire
!>[E]   ini_seed  :<integer>   argument optionnel permettant de générer une valeur fixe 
!
!$Common
!
!$Routines
!- MSP_inivar
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  NOMBRE ALEATOIRE 
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   implicit none

    real (KIND=PM_REEL) :: xsol
    integer, optional, intent(IN) :: ini_seed

    ! Constantes
    integer, parameter :: maxord = 12
    integer, parameter :: maxop1 = maxord + 1

    ! Variables locales remanantes
    integer :: ia, korder

    real (KIND=PM_REEL) :: xv(maxop1)

    data ia/0/korder/7/
    save ia, xv

    ! Fonction
    if(ia.eq.0)then
       call MSP_inivar(korder,xv,maxop1,ini_seed=ini_seed)
       ia=ia+1
    end if
    xsol = MSP_acorn (korder,xv,maxop1)

  end SUBROUTINE MSP_varuni


end MODULE MSP_ALEATOIRE
