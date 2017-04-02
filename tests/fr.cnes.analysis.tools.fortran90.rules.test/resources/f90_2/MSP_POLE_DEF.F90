module MSP_POLE_DEF


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  MSP_POLE_DEF
!
!$Resume
!	Module permettant de définir un repère à l'aide des angles d'Euler
!
!$Description
!	Module permettant de définir un repère à l'aide des angles d'Euler (phi,theta,psi)
!	L'évolution des angles d'Euler est considérée linéaire : tetha = theta0 + dtheta/dt*(t-teuler0)
!-	2 Modèles sont implémentés :	
!.	- Fixe:
!.	   phi = phi0
!.	   theta = theta0
!.	   psi = psi0 + dpsi/dt*(t-teuler0)
!.	- Mobile:
!.	   phi = phi0 + dphi/dt*(t-teuler0)
!.	   theta = theta0 + dtheta/dt*(t-teuler0)
!.	   psi = psi0 + dpsi/dt*(t-teuler0)
!
!$Auteur
!	S. ROUSSEAU
!
!$Version
!	$Id: MSP_POLE_DEF.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!	$Log: MSP_POLE_DEF.F90,v $
!	Revision 1.10  2010/10/20 09:35:43  mercadig
!	VERSION::AQ::20/10/2010:Ajout du marqueur de fin historique dans le cartouche
!
!	Revision 1.9  2008/11/19 13:33:01  mercadig
!	DM-ID 733 : Mise a jour cartouche
!
!	Revision 1.8  2005/03/08 07:32:36  fabrec
!	DM-ID 111 : mise à jour des cartouches
!	Revision 1.7  2003/03/12 14:30:04  adm_ipsi
!	 Utilisation de MSP_gen_messages
!	Revision 1.6  2003/02/05 16:58:43  adm_ipsi
!	MSP_calculer_euler_mat, remplacement de muangl par mu_angle2
!	Revision 1.5  2003/01/07 18:11:40  adm_ipsi
!	 suppression des variables non utilisées
!	Revision 1.4  2002/12/06 18:00:13  adm_ipsi
!	Utilisation de la fonction .different. pour les comparaisons entre reel
!	Revision 1.3  2002/12/03 17:21:03  adm_ipsi
!	 Ajout de implicit none
!	Revision 1.2  2002/10/25 16:31:58  adm_ipsi
!	Dans MSP_creer_pole, les calculs de cphi,sphi,cthe et sthe se font sur pole%euler0
!	Revision 1.1.1.1  2002/09/30 14:09:35  adm_ipsi
!	Industrialisation de la MECASPA sans les modules de gestion d'erreurs
!	Revision 1.5  2001/09/14 12:08:45  util_am
!	correction d'un bug sur l'ordre des rotation :
!	        1 = phi
!	        2 = theta
!	        3 = psy
!	Revision 1.4  2000/06/14 16:19:43  util_am
!	- Ajout du champ flag_func dans la structure MSP_POLE pour la gestion des fuites mémoires
!	- Privatisation du contenu de la structure MSP_POLE
!	- Ajout des intefaces anglaises
!	- Mise à jour des cartouches
!	Revision 1.3  1999/08/09 14:57:52  util_am
!	initialisation de la structure pole a sa creation
!	Revision 1.2  1999/08/05 13:34:38  util_am
!	correction BUG enum_pole_fixe remplace par MSP_enum_pole_fixe
!	Revision 1.1.1.1  1999/07/13 08:37:58  util_am
!	Version 1.0 de MECASPA mise sous CVS
!
!$FinHistorique
!
!$Usage
!  use MSP_POLE_DEF
!
!$Structure
!
!: MSP_POLE : Type permettant de définir le pôle et le premier méridien d'une planète
!#V
!>     type      : <integer,private>           type de pole ( fixe, mobile) 
!>     euler0    : <PM_REEL,DIM=(3),private>   Angles d'Euler à T0
!>     deuler    : <PM_REEL,DIM=(3),private>   Dérivées dea angles d'Euler 
!>     teuler0   : <PM_REEL,private>           Date des angles d'Euler0 
!>     const     : <PM_REEL,DIM=(13),private>  Constantes permettant d'optimeser le calcul dans le cas du pole fixe 
!#
!
!$Global
!
!>  MSP_enum_pole_fixe     : <integer,parameter>  Pole fixe
!>  MSP_enum_pole_mobile   : <integer,parameter>  Pole mobile
!$Common
!
!$Routines
!- MSP_create_pole
!- MSP_pole_euler_angles
!- MSP_pole_transform_matrix
!- MSP_get_pole_rotation_axis
!#V
!- egaler_pole
!#
!
!$Fonctions
!- MSP_creer_pole
!- MSP_calculer_euler_pole
!- MSP_calculer_mat_pole
!- MSP_calculer_wrot_pole
!- MSP_calculer_euler_mat
!#V
!- cal_mat_repere_pole_fixe
!- cal_mat_repere_pole_mobile
!#
!
!$Include
!
!$Module
!#V
!- MSLIB
!- MSP_MATH
!#
!
!$Interface
!> msp_get_pole_rotation_axis :  MSP_calculer_wrot_pole
!> assignment :                  egaler_pole
!> msp_pole_transform_matrix :   MSP_calculer_mat_pole
!> msp_create_pole :             MSP_creer_pole
!> msp_pole_euler_angles :       MSP_calculer_euler_pole
!#V
!#
!
!$Remarques
!
!$Mots-cles
! POLE
!
!$Voir-Aussi
!#V
!.  cal_mat_repere_pole_fixe cal_mat_repere_pole_mobile MSP_calculer_euler_mat egaler_pole
!#
!.  MSP_creer_pole MSP_calculer_euler_pole MSP_calculer_mat_pole MSP_calculer_wrot_pole MSP_create_pole
!.  MSP_pole_euler_angles MSP_pole_transform_matrix MSP_get_pole_rotation_axis
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
use MSLIB, only : PM_REEL,PM_PI
use MSP_MATH

  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: MSP_POLE_DEF.F90 69 2012-09-11 08:33:34Z ffsm $'

! Définition du type MSP_POLE
  type MSP_POLE
     private
     integer :: type
     real(kind=PM_REEL),dimension(3)::euler0
     real(kind=PM_REEL),dimension(3)::deuler
     real(kind=PM_REEL)::teuler0
     real(kind=PM_REEL),dimension(13)::const ! valeurs constantes dans le cas du pôle fixe
  end type MSP_POLE



  ! Définition des différents types de poles implémentés
  integer,parameter::MSP_enum_pole_fixe = 1
  integer,parameter::MSP_enum_pole_mobile = 2


  interface MSP_create_pole

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_create_pole
!
!$Resume
!  Creat a pole structure
!
!$Description
!  Creat a pole structure
!
!$Acces
!  PUBLIC
!
!$Usage
!  pole = MSP_create_pole([euler0],[deuler0],[teuler0],[vitrot],[mat])
!.    real(kind=PM_REEL),dimension(3) :: euler0
!.    real(kind=PM_REEL),dimension(3) :: deuler0
!.    real(kind=PM_REEL) :: teuler0
!.    real(kind=PM_REEL) :: vitrot
!.    real(kind=PM_REEL),dimension(3,3) :: mat
!.    type (MSP_POLE) :: pole
!
!$Procedures
!- MSP_creer_pole
!
!$Remarques
!
!$Mots-cles
! POLE CREER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_creer_pole
  end interface
  
  interface MSP_pole_euler_angles

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_pole_euler_angles
!
!$Resume
!  Compute the euler pole angles
!
!$Description
!  Compute the euler pole angles
!
!$Acces
!  PUBLIC
!
!$Usage
!  a_euler = MSP_pole_euler_angles(pole,t)
!.    type(MSP_POLE) :: pole
!.    real(kind=PM_REEL) :: t
!.    real(kind=PM_REEL),dimension(3) :: a_euler
!
!$Procedures
!- MSP_calculer_euler_pole
!
!$Remarques
!
!$Mots-cles
! POLE CALCULER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_calculer_euler_pole
  end interface
  
  interface MSP_pole_transform_matrix

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_pole_transform_matrix
!
!$Resume
!  Compute the pole transformation matrix
!
!$Description
!  Compute the pole transformation matrix
!
!$Acces
!  PUBLIC
!
!$Usage
!  mat = MSP_pole_transform_matrix(pole,t)
!.    type (MSP_POLE) :: pole
!.    real(kind=PM_REEL) :: t
!.    real(kind=PM_REEL),dimension(3,3) :: mat
!
!$Procedures
!- MSP_calculer_mat_pole
!
!$Remarques
!
!$Mots-cles
! POLE CALCULER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_calculer_mat_pole
  end interface
  
  interface MSP_get_pole_rotation_axis

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_get_pole_rotation_axis
!
!$Resume
!  Get the pole rotation axis
!
!$Description
!  Get the pole rotation axis
!
!$Acces
!  PUBLIC
!
!$Usage
!  w = MSP_get_pole_rotation_axis(pole,t)
!.    type(MSP_POLE) :: pole
!.    real(kind=PM_REEL) :: t
!.    real(kind=PM_REEL),dimension(3) :: w
!
!$Procedures
!- MSP_calculer_wrot_pole
!
!$Remarques
!
!$Mots-cles
! POLE CALCULER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_calculer_wrot_pole
  end interface

  interface assignment (=)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  assignment
!
!$Resume
!
!$Description
!
!$Acces
!  PUBLIC
!
!$Usage
!  polea=poleb
!.    type (MSP_POLE) :: polea
!.    type (MSP_POLE) :: poleb
!
!$Procedures
!#V
!- egaler_pole
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
     module procedure egaler_pole
  end interface

  private ::cal_mat_repere_pole_fixe
  private ::cal_mat_repere_pole_mobile
  private :: MSP_calculer_euler_mat
  private :: egaler_pole
CONTAINS

  subroutine egaler_pole(polea,poleb)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  egaler_pole
!
!$Resume
!
!$Description
!
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!  call egaler_pole(polea,poleb)
!.    type (MSP_POLE) :: polea
!.    type (MSP_POLE) :: poleb
!
!$Arguments
!>S     polea  :<MSP_POLE>   
!>E     poleb  :<MSP_POLE>   
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

    type (MSP_POLE),intent(out)::polea
    type (MSP_POLE),intent(in)::poleb

    polea%type = poleb%type
    polea%euler0(:) = poleb%euler0(:)
    polea%deuler(:) = poleb%deuler(:)
    polea%teuler0 = poleb%teuler0
    polea%const(:) = poleb%const(:)
  end subroutine egaler_pole
  ! Définition d'un repère

  function MSP_creer_pole(euler0,deuler0,teuler0,vitrot,mat) result (pole)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_creer_pole
!
!$Resume
!	Routine de création d'une variable de type MSP_POLE
!
!$Description
!	Cette routine permet de créer une variable de type MSP_POLE
!-	Il existe différentes façons de créer un pôle:
!.	Les angles d'Euler initiaux (soit Euler0 soit la matrice du repère de référence au repère de la planète)
!.	Lee dérivées des angles (soit vitrot dans le cas d'un pôle fixe, soit deuler dans le cas d'un pôle mobile)
!
!$Auteur
!	S. ROUSSEAU
!
!$Acces
!  PUBLIC
!
!$Usage
!  pole = MSP_creer_pole([euler0],[deuler0],[teuler0],[vitrot],[mat])
!.    real(kind=PM_REEL),dimension(3) :: euler0
!.    real(kind=PM_REEL),dimension(3) :: deuler0
!.    real(kind=PM_REEL) :: teuler0
!.    real(kind=PM_REEL) :: vitrot
!.    real(kind=PM_REEL),dimension(3,3) :: mat
!.    type (MSP_POLE) :: pole
!
!$Arguments
!>[E]   euler0   :<PM_REEL,DIM=(3)>     Angles d'Euler à teuler0
!>[E]   deuler0  :<PM_REEL,DIM=(3)>     Dérivée des angles d'euler
!>[E]   teuler0  :<PM_REEL>             Date de définition des angles d'euler euler0
!>[E]   vitrot   :<PM_REEL>             Dans le cas d'un pole fixe seul un angle évolue le temps sidéral
!>[E]   mat      :<PM_REEL,DIM=(3,3)>   Matrice de passage du repère de référence au repère planéto à teuler0
!>S     pole     :<MSP_POLE>            Structure contenant la description du pole et du premier méridien d'une planète 
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
! POLE CREER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       
    implicit none

    real(kind=PM_REEL),dimension(3),optional,intent(in)::euler0
    real(kind=PM_REEL),dimension(3),optional,intent(in)::deuler0
    real(kind=PM_REEL),optional,intent(in)::teuler0
    real(kind=PM_REEL),optional,intent(in)::vitrot
    real(kind=PM_REEL),optional,intent(in),dimension(3,3) :: mat
    type (MSP_POLE)::pole

 
    ! Variable locales
     real(kind=PM_REEL):: cphi
     real(kind=PM_REEL)::  sphi
     real(kind=PM_REEL)::  cthe 
     real(kind=PM_REEL)::  sthe 


     ! Initialisation de la structure

     ! Par défault le pole est considéré comme mobile
     pole%type=MSP_enum_pole_mobile

     pole%euler0(:) = 0._PM_REEL
     pole%deuler(:) = 0._PM_REEL
     pole%teuler0 = 0._PM_REEL
     pole%const(:) = 0._PM_REEL


     ! Affectation de la date de définition des angles euler0
     ! Si la date n'est pas présente elle prise égale à 0
     if ( present(teuler0)) then
        pole%teuler0 = teuler0
     else
        pole%teuler0 = 0._PM_REEL
     end if

     ! Affectation des angles d'euler initiaux (euler0)
     if ( present(euler0)) then
        pole%euler0(:) = euler0(:)
     else if ( present(mat)) then
        pole%euler0=MSP_calculer_euler_mat(mat)
     else
        pole%euler0(:)=0._PM_REEL
     end if



    ! cas d'un repère de type pôle fixe
    if (present(vitrot)) then
       pole%type = MSP_enum_pole_fixe
       pole%deuler(3) = vitrot

       cphi=cos(pole%euler0(1))
       sphi=sin(pole%euler0(1))

       cthe = cos(pole%euler0(2))
       sthe = sin(pole%euler0(2))

       pole%const(1)  = cphi
       pole%const(2)  = - cthe*sphi
       pole%const(3)  = sphi
       pole%const(4)  = cthe*cphi
       pole%const(5)  = sthe
       pole%const(6)  = -cphi
       pole%const(7)  = - cthe*sphi
       pole%const(8)  = -sphi 
       pole%const(9)  = cthe*cphi
       pole%const(10) = sthe
       pole%const(11) = sthe*sphi
       pole%const(12) = -sthe*cphi
       pole%const(13) = cthe
       

    end if

    ! Cas d'un repère à pôle mobile type UAI
    if (present(deuler0)) then
       pole%type = MSP_enum_pole_mobile
       pole%deuler(:) = deuler0(:)
    end if
  end function MSP_creer_pole



  ! Calcul de la valeur des trois angles d'euler en fonction du temps
  function MSP_calculer_euler_pole(pole,t) result (a_euler)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_calculer_euler_pole
!
!$Resume
!	Fonction permettant de calculer la valeur des trois angles d'Euler en fonction du temps
!
!$Description
!	Fonction permettant de calculer la valeur des trois angles d'Euler en fonction du temps
!
!$Auteur
!	S. ROUSSEAU
!
!$Acces
!  PUBLIC
!
!$Usage
!  a_euler = MSP_calculer_euler_pole(pole,t)
!.    type(MSP_POLE) :: pole
!.    real(kind=PM_REEL) :: t
!.    real(kind=PM_REEL),dimension(3) :: a_euler
!
!$Arguments
!>E     pole     :<MSP_POLE>           Structure contenant la description du pôle et du premier méridien d'une planète
!>E     t        :<PM_REEL,DIM=(in)>   Date (cohérente avec teuler0 et les dérivées des angles
!>S     a_euler  :<PM_REEL,DIM=(3)>    angles d'euler à t
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
! POLE CALCULER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    implicit none

    type(MSP_POLE),intent(in) ::pole
    real(kind=PM_REEL),intent(in) :: t
    real(kind=PM_REEL),dimension(3):: a_euler

    
     a_euler(1)   = pole%euler0(1) + pole%deuler(1)*(t - pole%teuler0)
     a_euler(2)   = pole%euler0(2) + pole%deuler(2)*(t - pole%teuler0)
     a_euler(3)   = pole%euler0(3) + pole%deuler(3)*(t - pole%teuler0)

   end function MSP_calculer_euler_pole



  ! Calcul de la matrice de passage dans le cas d'un pole fixe
  function cal_mat_repere_pole_fixe(pole,t) result (mat)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cal_mat_repere_pole_fixe
!
!$Resume
!	Calcul de la matrice dans le cas d'un pôle fixe
!
!$Description
!	Calcul de la matrice dans le cas d'un pôle fixe
!
!$Auteur
!	S. ROUSSEAU
!
!$Acces
!  PRIVE
!
!$Usage
!  mat = cal_mat_repere_pole_fixe(pole,t)
!.    type(MSP_POLE) :: pole
!.    real(kind=PM_REEL) :: t
!.    real(kind=PM_REEL),dimension(3,3) :: mat
!
!$Arguments
!>E     pole  :<MSP_POLE>            type POLE
!>E     t     :<PM_REEL,DIM=(in)>    date
!>S     mat   :<PM_REEL,DIM=(3,3)>   matrice de passge du repère de référence au repère planéto
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
! POLE CALCULER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    implicit none

    ! Arguments d'appel
     type(MSP_POLE),intent(in) ::pole
     real(kind=PM_REEL),intent(in) :: t
     real(kind=PM_REEL),dimension(3,3)::mat


     ! Variables locales
     real(kind=PM_REEL)::psi,cpsi,spsi


     ! Calcul de psi(t) 
     ! Attention vitesse de rotation en rad/s 
     psi = pole%euler0(3) + pole%deuler(3)*(t-pole%teuler0)
     cpsi = cos(psi)
     spsi = sin(psi)


     mat(1,1) =  cpsi*pole%const(1) + pole%const(2)*spsi 
     mat(1,2) =  cpsi*pole%const(3) + pole%const(4)*spsi
     mat(1,3) =  pole%const(5)*spsi
     mat(2,1) = spsi*pole%const(6) + pole%const(7)*cpsi
     mat(2,2) = spsi*pole%const(8) + pole%const(9)*cpsi
     mat(2,3) =  cpsi*pole%const(10)
     mat(3,1) =  pole%const(11)
     mat(3,2) =  pole%const(12)
     mat(3,3) =  pole%const(13)

     
   end function cal_mat_repere_pole_fixe

   ! Calcul de la matrice dans le cas d'un pôle mobile
   function cal_mat_repere_pole_mobile(pole,t) result (mat)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cal_mat_repere_pole_mobile
!
!$Resume
!	Calcul de la matrice de passage du repère de réf'erence au repère planéto dan sle cas d'un pôle mobile
!
!$Description
!	Calcul de la matrice de passage du repère de réf'erence au repère planéto dan sle cas d'un pôle mobile
!
!$Auteur
!	S. ROUSSEAU
!
!$Acces
!  PRIVE
!
!$Usage
!  mat = cal_mat_repere_pole_mobile(pole,t)
!.    type(MSP_POLE) :: pole
!.    real(kind=PM_REEL) :: t
!.    real(kind=PM_REEL),dimension(3,3) :: mat
!
!$Arguments
!>E     pole  :<MSP_POLE>            POLE
!>E     t     :<PM_REEL,DIM=(in)>    DATE
!>S     mat   :<PM_REEL,DIM=(3,3)>   matrice de passage du repère de réf'erence au repère planéto
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
! POLE CALCULER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       
     implicit none

     ! Arguments d'appel
     type(MSP_POLE),intent(in) ::pole
     real(kind=PM_REEL),intent(in) :: t
     real(kind=PM_REEL),dimension(3,3)::mat

     real(kind=PM_REEL)::  cthe 
     real(kind=PM_REEL)::  sthe 


     ! Variables locales
     real(kind=PM_REEL)::phi,theta,psi,cphi,sphi,spsi,cpsi

     ! Calcul de theta(t),phi(t),psi(t)
     ! Attention vitesse de rotation en rad/s 

     phi = pole%euler0(1) + pole%deuler(1)*(t - pole%teuler0)
     theta   = pole%euler0(2) + pole%deuler(2)*(t - pole%teuler0)
     psi   = pole%euler0(3) + pole%deuler(3)*(t - pole%teuler0)


     ! Calcul des cosinus et sinus associés
     cpsi   = cos(psi)
     spsi   = sin(psi)
     
     cphi   = cos(phi)
     sphi   = sin(phi)
     
     cthe   = cos(theta)
     sthe   = sin(theta)
     
     ! Calcul de la matrice de passage
     mat(1,1) =  cpsi*cphi - cthe*sphi*spsi 
     mat(1,2) =  cpsi*sphi + cthe*cphi*spsi
     mat(1,3) =  sthe*spsi
     mat(2,1) = -spsi*cphi - cthe*sphi*cpsi
     mat(2,2) = -spsi*sphi + cthe*cphi*cpsi
     mat(2,3) =  cpsi*sthe
     mat(3,1) =  sthe*sphi
     mat(3,2) = -sthe*cphi
     mat(3,3) =  cthe
       
   end function cal_mat_repere_pole_mobile


   function MSP_calculer_mat_pole(pole,t) result (mat)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_calculer_mat_pole
!
!$Resume
!	Fonction permettant de calculer la matrice de passage du repère de référence au repère planéto (pole+premier meridien)
!
!$Description
!	Fonction permettant de calculer la matrice de passage du repère de référence au repère planéto (pole+premier meridien)
!
!$Auteur
!	S. ROUSSEAU
!
!$Acces
!  PUBLIC
!
!$Usage
!  mat = MSP_calculer_mat_pole(pole,t)
!.    type (MSP_POLE) :: pole
!.    real(kind=PM_REEL) :: t
!.    real(kind=PM_REEL),dimension(3,3) :: mat
!
!$Arguments
!>E     pole  :<MSP_POLE>            Définition du pôle et du premier méridien
!>E     t     :<PM_REEL,DIM=(in)>    date (cohérente avec teuler0 et les dérivées des angles)
!>S     mat   :<PM_REEL,DIM=(3,3)>   matrice de passage
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
! POLE CALCULER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     implicit none

     type (MSP_POLE) ,intent(in) :: pole
     real(kind=PM_REEL),intent(in)::t
     real(kind=PM_REEL),dimension(3,3)::mat


     select case (pole%type)
     case (MSP_enum_pole_fixe)
        mat =  cal_mat_repere_pole_fixe(pole,t)
     case (MSP_enum_pole_mobile)
        mat =  cal_mat_repere_pole_mobile(pole,t)
     end select
   end function MSP_calculer_mat_pole
  
   function MSP_calculer_wrot_pole(pole,t) result (w)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_calculer_wrot_pole
!
!$Resume
!	Fonction permettant de calculer le vecteur rotation instantanné du pôle dans le repère de référence
!
!$Description
!	Fonction permettant de calculer le vecteur rotation instantanné du pôle dans le repère de référence
!
!$Auteur
!	S. ROUSSEAU
!
!$Acces
!  PUBLIC
!
!$Usage
!  w = MSP_calculer_wrot_pole(pole,t)
!.    type(MSP_POLE) :: pole
!.    real(kind=PM_REEL) :: t
!.    real(kind=PM_REEL),dimension(3) :: w
!
!$Arguments
!>E     pole  :<MSP_POLE>           définition du pôle et du prime méridien   
!>E     t     :<PM_REEL,DIM=(in)>   date  
!>S     w     :<PM_REEL,DIM=(3)>    vecteur rotation instantané  
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
! POLE CALCULER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     
     implicit none

     ! Calcul du vecteur rotation 
     type(MSP_POLE),intent(in)::pole
     real(kind=PM_REEL),intent(in)::t
     real(kind=PM_REEL),dimension(3)::w
    
     ! Variable locales
     real(kind=PM_REEL),dimension(3)::uw2,uw3
     real(kind=PM_REEL),dimension(3),parameter::uw1=(/0._PM_REEL,0._PM_REEL,1._PM_REEL/)
     real(kind=PM_REEL)::phi,theta,cphi,sphi

     real(kind=PM_REEL)::  cthe 
     real(kind=PM_REEL)::  sthe 

     ! Calcul de theta(t),phi(t),psi(t)
     ! Attention vitesse de rotation en rad/s 

     phi = pole%euler0(1) + pole%deuler(1)*(t - pole%teuler0)
     theta   = pole%euler0(2) + pole%deuler(2)*(t - pole%teuler0)


     ! Calcul des cosinus et sinus associés
     
     cphi   = cos(phi)
     sphi   = sin(phi)
     
     cthe   = cos(theta)
     sthe   = sin(theta)

     ! Calcul du vecteur uw2 correspondant à une rotation de phi
     uw2(1) = cphi
     uw2(2) = sphi
     uw2(3) = 0._PM_REEL

     ! Calcul du vecteur uw3 correspondant l'axe Z du nouveau repère
     ! Calcul de la matrice de passage
     uw3(1) =  sphi*sthe
     uw3(2) = -cphi*sthe
     uw3(3) =  cthe

     ! Calcul du vecteur rotation 
     w(:) = pole%deuler(1)*uw1(:)+pole%deuler(2)*uw2(:)+pole%deuler(3)*uw3(:)
   end function MSP_calculer_wrot_pole
     

  function  MSP_calculer_euler_mat(p) result (a_euler)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_calculer_euler_mat
!
!$Resume
!	Fonction permettant de calculer les angles d'Eulers à partir d'une matrice
!
!$Description
!	Fonction permettant de calculer les angles d'Eulers à partir d'une matrice
!
!$Auteur
!	S. ROUSSEAU
!
!$Acces
!  PRIVE
!
!$Usage
!  a_euler = MSP_calculer_euler_mat(p)
!.    real(kind=PM_REEL),dimension(3,3) :: p
!.    real(kind=PM_REEL),dimension(3) :: a_euler
!
!$Arguments
!>E     p        :<PM_REEL,DIM=(3,3)>   Matrice
!>S     a_euler  :<PM_REEL,DIM=(3)>     angles d'Euler associés
!
!$Common
!
!$Routines
!- mu_angle2
!- MSP_signaler_message
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
! POLE CALCULER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
    implicit none

    ! Arguments 
     real(kind=PM_REEL),dimension(3,3),intent(in):: p
     real(kind=PM_REEL),dimension(3) ::a_euler
 
    ! Déclaration des variables 
    real(kind=PM_REEL) :: x, y
    real(kind=PM_REEL) :: sin2psi, sin2phi, cos2psi
    real(kind=PM_REEL) ::  cos2phi, sintheta, eps,phi,theta,psi
    type(tm_code_retour)      :: code_retour

 
      eps = 1.0e-12_PM_REEL
 
      ! Début du programme 
      
      IF (abs(p(3,3) - 1._PM_REEL) < eps) THEN
         !- Cas theta = 0
         theta = 0.0_PM_REEL
         x = p(1,1)
         y = p(1,2)
         call mu_angle2(x,y,phi,code_retour)
         call MSP_signaler_message (ier_mslib=code_retour)
         if (MSP_gen_messages("MSP_calculer_euler_mat")) return

         psi = 0.0_PM_REEL
      ELSE IF (abs(p(3,3) + 1._PM_REEL)< eps) THEN
         !- Cas theta = PI
         theta  = PM_PI
         x = p(1,1)
         y = p(1,2)
         call mu_angle2(x,y,phi,code_retour)
         if (MSP_gen_messages("MSP_calculer_euler_mat")) return

         psi = 0.0_PM_REEL
      ELSE
         ! Cas general
         sin2psi = 2.0_PM_REEL*p(1,3)*p(2,3)/(1.0_PM_REEL - p(3,3)*p(3,3))
         sin2phi = 2.0_PM_REEL*p(3,1)*p(3,2)/(p(3,3)*p(3,3) - 1.0_PM_REEL)
         cos2psi = (p(2,3)*p(2,3) - p(1,3)*p(1,3))/(1.0_PM_REEL - p(3,3)*p(3,3))
         cos2phi = (p(3,2)*p(3,2) - p(3,1)*p(3,1))/(1.0_PM_REEL - p(3,3)*p(3,3))
         call mu_angle2(cos2phi, sin2phi, phi,code_retour)
         call MSP_signaler_message (ier_mslib=code_retour)
         if (MSP_gen_messages("MSP_calculer_euler_mat")) return

         phi = 0.5_PM_REEL*phi
         call mu_angle2(cos2psi, sin2psi, psi,code_retour)
         call MSP_signaler_message (ier_mslib=code_retour)
         if (MSP_gen_messages("MSP_calculer_euler_mat")) return

         psi = 0.5_PM_REEL*psi

         IF (COS(phi) .different. 0._PM_REEL) THEN
            sintheta = - p(3,2)/COS(phi)
         ELSE
            sintheta = p(3,1)/SIN(phi)
         ENDIF
         call mu_angle2(p(3,3),sintheta,theta,code_retour)
         call MSP_signaler_message (ier_mslib=code_retour)
         if (MSP_gen_messages("MSP_calculer_euler_mat")) return



         IF (COS(psi)*SIN(theta)*p(2,3) < 0.0_PM_REEL) THEN
            psi = psi + PM_PI
         ENDIF
      ENDIF
 

      a_euler(1) = phi
      a_euler(2) = theta
      a_euler(3) = psi

     return
   end function MSP_calculer_euler_mat


end module MSP_POLE_DEF
