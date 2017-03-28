MODULE MSP_ERF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  MSP_ERF
!
!$Resume
!  Module contenant la description de la fonction inverse de erf
!
!$Description
!  Module contenant la description de la fonction inverse de erf
!
!$Auteur
!  J.-J. WASBAUER
!
!$Version
!  $Id: MSP_ERF.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: MSP_ERF.F90,v $
!  Revision 1.15  2011/07/06 14:16:07  vivaresf
!  VERSION::FA-ID:1488:06/07/2011:rajout d'une valeur à val_inverf avec initialisation
!
!  Revision 1.14  2010/10/20 09:35:43  mercadig
!  VERSION::AQ::20/10/2010:Ajout du marqueur de fin historique dans le cartouche
!
!  Revision 1.13  2008/11/19 13:31:57  mercadig
!  DM-ID 733 : Mise a jour cartouche
!
!  Revision 1.12  2008/08/08 14:23:03  gss
!  DM-ID 1058 : (portage g95) suppression de variables non utilisées. Initialisation
!  à 0 de la sortie de la fonction MSP_inverf.
!  Revision 1.11  2008/07/04 15:00:29  huec
!  DM-ID 1058 : Gestion memoire, creation d\'une nouvelle routine pour desallouer le tableau val_inverf, variable globale
!  Revision 1.10  2008/04/24 16:24:13  huec
!  AQ : Correction d un commentaire
!  Revision 1.9  2008/02/26 12:59:38  huec
!  FA-ID 982 : Mise a jour des commentaires par rapport a la fonction ERF
!  Revision 1.8  2007/11/27 14:55:12  huec
!  DM-ID 699 : Amelioration des moyens de tests MECASPA
!  Revision 1.7  2007/11/19 09:39:15  huec
!  DM-ID 820 : Scripts de generation automatique, utilisation de makemake V2.0
!  Revision 1.6  2006/10/11 08:05:25  tanguyy
!  DM-ID 425 : Cloture du FT (Passage PSIMU sous Linux)
!  Revision 1.5.2.1  2006/09/25 15:13:34  vpg
!  DM-ID 425 : Version initiale du FT (Passage PSIMU sous Linux)
!  Revision 1.5  2005/03/08 07:32:35  fabrec
!  DM-ID 111 : mise à jour des cartouches
!  Revision 1.4  2003/01/08 15:12:38  adm_ipsi
!   Explicitation des conversions de type implicites
!  Revision 1.3  2003/01/07 18:11:40  adm_ipsi
!   suppression des variables non utilisées
!  Revision 1.2  2002/12/03 17:21:02  adm_ipsi
!   Ajout de implicit none
!  Revision 1.1.1.1  2002/09/30 14:09:35  adm_ipsi
!  Industrialisation de la MECASPA sans les modules de gestion d'erreurs
!  Revision 1.3  2000/06/14 16:04:54  util_am
!  - Ajout des interfaces anglaises
!  - Mise à jour des cartouches (section Voir-Aussi et Mots-cles)
!  Revision 1.2  1999/09/22 15:05:40  util_am
!  Mise a jour des cartouches
!  Revision 1.1  1999/09/03 16:42:39  util_am
!  MSP_ACCES.F90 :Ajout de routine pour la connexion et la deconnexion de section MADONA
!  MSP_BULLETIN_DEF.F90 : Ajout de MSP_modifier_bulletin, MSP_lire_BULLETIN
!  MECASPA.F90 : Ajout des use aux nouveaux modules
!
!$FinHistorique
!
!$Usage
!  use MSP_ERF
!
!$Structure
!
!$Global
!
!>  erf_step       : <PM_REEL>                  pas de discretisation en x
!>  f_step         : <PM_REEL>                  pas de discretisation en erf(x)
!>  exp_bornemax   : <PM_REEL>                  valeur de la gaussienne normalisée à la borne max
!>  erf_bornemax   : <PM_REEL>                  valeur de la fonction erf à la borne max
!>  bornemax       : <PM_REEL>                  valeur de la borne max à ne pas de passer lors des tirages
!>  n_inv          : <integer>                  Nombre de points dans le tableau
!#V
!>  val_inverf     : <PM_REEL,DIM=(:),private>  Tableau de stockage de la fonction erf-1
!#
!$Common
!
!$Routines
!- MSP_initialize_erf
!- MSP_inverse_erf
!- MSP_initialise_erf
!- MSP_close_erf
!
!$Fonctions
!- MSP_inverf
!
!$Include
!
!$Module
!#V
!- MSLIB
!- MSP_GESTION_ERREUR
!#
!
!$Interface
!> msp_initialize_erf :  MSP_initialise_erf
!> msp_inverse_erf :     MSP_inverf
!#V
!#
!
!$Remarques
!
!$Mots-cles
! ERF
!
!$Voir-Aussi
!.  MSP_inverf MSP_initialize_erf MSP_inverse_erf MSP_initialise_erf MSP_close_erf
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  USE MSLIB, only:PM_REEL
  USE MSP_GESTION_ERREUR

  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: MSP_ERF.F90 69 2012-09-11 08:33:34Z ffsm $'


  real(KIND=PM_REEL), dimension(:), allocatable, private :: val_inverf
  real(KIND=PM_REEL) :: erf_step, f_step
  real(KIND=PM_REEL) :: exp_bornemax, erf_bornemax, bornemax
  integer :: n_inv

  interface MSP_initialize_erf

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_initialize_erf
!
!$Resume
!  Initialize the erf inverse function table
!
!$Description
!  Initialize the erf inverse function table
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_initialize_erf([vmax], [nb_inter_inv])
!.    integer :: nb_inter_inv
!.    real(KIND=PM_REEL) :: vmax
!
!$Procedures
!- MSP_initialise_erf
!
!$Remarques
!
!$Mots-cles
! ERF INITIALISER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_initialise_erf
  end interface

  interface MSP_inverse_erf

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_inverse_erf
!
!$Resume
!  The erf inverse function
!
!$Description
!  The erf inverse function
!
!$Acces
!  PUBLIC
!
!$Usage
!  x = MSP_inverse_erf(y)
!.    real(KIND=PM_REEL) :: y
!
!$Procedures
!- MSP_inverf
!
!$Remarques
!
!$Mots-cles
! ERF
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_inverf
  end interface


 
CONTAINS

  SUBROUTINE MSP_initialise_erf(vmax, nb_inter_inv)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_initialise_erf
!
!$Resume
!  Calcule un tableau contenant la reciproque de la fonction erf 
!
!$Description
!  Calcule un tableau contenant la reciproque de la fonction erf 
!
!$Auteur
!  J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_initialise_erf([vmax], [nb_inter_inv])
!.    integer :: nb_inter_inv
!.    real(KIND=PM_REEL) :: vmax
!
!$Arguments
!>[E]   vmax          :<PM_REEL>   Valeur maximum en x stockée dans le tableau.
!                                   Par défaut la valeur est 10.
!>[E]   nb_inter_inv  :<integer>   Nombre de points dans le tableau.
!                                   Par défaut la valeur est 1000
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
! ERF INITIALISER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none

    integer, intent(in), optional :: nb_inter_inv
    real(KIND=PM_REEL), intent(in), optional :: vmax
    real(KIND=PM_REEL) :: f, xmin, xmil, xmax, bornesup
    integer :: j

    !! DM 425 : portage Linux
    !! erf est une macro dont la definition est passee
    !! au compilateur via l'option -D
    !! - avec f90 : ERF=""
    !! - avec pgf90 : ERF=derf (passe au compilo avec -D)
    double precision :: ERF

    n_inv = 1000
    if (PRESENT(nb_inter_inv)) n_inv = nb_inter_inv
    ALLOCATE(val_inverf(0:n_inv+1))

    bornesup = 10._PM_REEL
    if (PRESENT(vmax)) bornesup = vmax


    f = 0._pm_reel
    f_step = (1._PM_REEL-1.e-10_PM_REEL)/n_inv
    erf_step = bornesup/n_inv
    val_inverf(0) = 0._PM_REEL
    val_inverf(n_inv+1) = 0._PM_REEL
    xmax = 0._PM_REEL

    do j = 1, n_inv
       
       f = f + f_step
       do while (ERF(xmax) <= f)
          xmax = xmax + erf_step
       enddo
       xmin = xmax - erf_step 
       
       do while ( abs(xmin-xmax) >= 1.e-10_PM_REEL )
          xmil = 0.5_PM_REEL*(xmin+xmax)
          if ( (f-ERF(xmil))*(f-ERF(xmax)) <= 0._pm_reel) xmin = xmil
          if ( (f-ERF(xmil))*(f-ERF(xmin)) <= 0._pm_reel) xmax = xmil
       end do
       
       val_inverf(j) = 0.5_PM_REEL*(xmin+xmax)

    end do

    bornemax = 5._PM_REEL
    erf_bornemax = ERF(bornemax/sqrt(2._PM_REEL))
    exp_bornemax = exp(-bornemax*bornemax*0.5_PM_REEL)

       

  end SUBROUTINE MSP_initialise_erf



  real(KIND=PM_REEL) FUNCTION MSP_inverf(y) result(x)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_inverf
!
!$Resume
!  Cette fonction retourne la valeur de la fonction réciproque de erf pour tout
!  réel compris entre -1 et 1.
!
!$Description
!  Cette fonction retourne la valeur de la fonction réciproque de erf pour tout
!  réel compris entre -1 et 1.
!
!$Auteur
!  J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  x = MSP_inverf(y)
!.    real(KIND=PM_REEL) :: y
!
!$Arguments
!>E     y  :<PM_REEL>   valeur de la fonction erf-1.
!>S     x  :<PM_REEL>   valeur a laquelle est calculee la fonction erf-1.
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
! ERF
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none

    ! Arguments en entrée
    real(KIND=PM_REEL), intent(in) :: y
    ! Variables locales
    integer :: k
    real(KIND=PM_REEL) :: pente

    ! Initialisation de la sortie
    x = 0._pm_reel

    if (abs(y) >  1._PM_REEL) then 
       call MSP_signaler_message (cle_mes="MSP_DEPASSEMENT_BORNE_ERF", &
            routine="MSP_inverf",type=MSP_ENUM_ERREUR)
       return
    end if

    k = int(abs(y)/f_step)
    pente = (val_inverf(k+1) - val_inverf(k))/f_step
    x = val_inverf(k) + pente*(abs(y)-real(k,kind=pm_reel)*f_step)

    if (y < 0._PM_REEL) x = -x

  end FUNCTION MSP_inverf

  SUBROUTINE MSP_close_erf()
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_close_erf
!
!$Resume
!  Désalloue le tableau inv_erf
!
!$Description
!  Désalloue le tableau inv_erf
!
!$Auteur
!  C. Hue
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_close_erf()
!
!$Arguments
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
! ERF DESALLOCATION
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none

    DEALLOCATE(val_inverf)

  end SUBROUTINE MSP_close_erf

end MODULE MSP_ERF
