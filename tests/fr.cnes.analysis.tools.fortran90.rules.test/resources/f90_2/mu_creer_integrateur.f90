subroutine mu_creer_integrateur_pas_fixe (type_integrateur,n,pas,integrateur,code_retour, &
     ordre,icirc,ireg,rxmu,mode_iteratif,eps_init,eps_prog,liber_param,&
     eps_init_var_integ,eps_prog_var_integ)

! (C) Copyright CNES - MSPRO - 2005

!************************************************************************
!
! But:  Creation d'un integrateur a pas fixe
! ===
!
! Note d'utilisation:  
! ==================
!
! * L'architecture et le code ont ete recopies directement depuis l'architecture et le code
! de la bibliotheque Mantissa (http://www.spaceroots.org/software/mantissa/index.html), apres
! transcription du code Java en Fortran 90
! Mantissa est un produit libre developpe par Luc Maisonobe et diffuse
! sous une licence BSD modifiee autorisant cet emprunt et ces modifications.
! * L'intégrateur de Cowell nécessite certains paramètres supplémentaires qui ne sont pas utilisés
! dans Mantissa. L'intégrateur de Cowell codé dans la MSPRO ne provient pas de Mantissa.
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 5.2 : creation
!                         (Date: 01/2005 - Realisation: Bruno Revelin)
!     + Version 5.3 : modification
!                     DM-ID 408 : mettre un Cowell dans les integrateurs MSPRO
!                     (Date: 10/2005 - Realisation: Equipe Patrimoine Mecanique Spatiale - Atos Origin)
!     + Version 5.4 : modification
!                     FA-ID 439 : remarques qualite
!                     (Date: 02/2006 - Realisation: Atos Origin)
!                     DM-ID 492 : optimisation de l'utilisation du critere de convergence de
!                     l'integrateur de Cowell
!                     (Date: 02/2006 - Realisation: Atos Origin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!                   FA-ID 625 : Non initialisations de pointeurs dans la mspro
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 5.7 : DM-ID 738 : Evolution du Cowell
!                   (Date: 06/2007 -Realisation: Sandrine Avril-Atos_origin)
!   + Version 5.9 : DM-ID 983 : modification de l'epsilon sur la variable d'intégration
!                   (Date: 03/2008 -Réalisation: Yannick Tanguy-ATOS Origin)
!   + Version 5.10: DM-ID 1058 : Correction des warnings levés par g95
!                   (Date: 8/2008 - Realisation: Atos origin)
!
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======
use mslib
 
use int_util_internes_mspro, only : mui_integ_butcher

use type_mspro
use parametre_interne_mspro
use parametre_mspro

use valeur_code_retour_mspro
use numero_routine_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
integer,                              intent(in)     :: type_integrateur! type de l'integrateur
integer,                              intent(in)     :: n               ! dimension du vecteur d'etat
real(pm_reel),                        intent(in)     :: pas             ! pas d'integration (pas fixe)
type(tm_integrateur),                 intent(out)    :: integrateur     ! integrateur cree
type(tm_code_retour), intent(out)                    :: code_retour
integer, intent(in),  optional                       :: ordre		! ordre de l'integrateur (nombre pair de 2 a 16)
integer, intent(in),  optional                       :: icirc		! cle de circularisation
integer, intent(in),  optional                       :: ireg		! cle de regularisation (chgt de variable temps)
real(pm_reel), intent(in),  optional                 :: rxmu		! mu
logical, intent(in),  optional                       :: mode_iteratif	! mode iteratif
real(pm_reel), intent(in), optional                           :: eps_init        ! epsilon d'initialisation de l'integration (cowell)
real(pm_reel), intent(in), optional                           :: eps_prog        ! epsilon de progression de l'integration (cowell)
integer,intent(in), dimension(6), optional           :: liber_param     ! indicateur de liberation des paramètres
real(pm_reel), intent(in), optional                           :: eps_init_var_integ ! epsilon d'initialisation pour la variable d'intégration (cowell)
real(pm_reel), intent(in), optional                           :: eps_prog_var_integ ! epsilon de progression pour la variable d'intégration (cowell)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

integer :: alloc_ok    ! retour de l'allocation
integer :: retour_local
integer :: i           ! compteur de boucle
intrinsic abs

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mu_creer_integrateur.f90: derniere modification V5.15 >'

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour

code_retour%valeur = pm_OK

! Verifications des parametres
! =======================

!  Dimension du vecteur d etat

if (n < 1) then
   code_retour%valeur = pm_err_val_para
   go to 6000
end if

! Pas d integration

if (abs(pas) < epsilon(1._pm_reel)) then
   code_retour%valeur = pm_err_val_para
   go to 6000
end if

! type de l integrateur

if ((type_integrateur /= pm_Gill).and.(type_integrateur /= pm_Cowell)) then
   code_retour%valeur = pm_err_type_integ
   go to 6000
end if

! Initialisation des parametres integrateur
! =========================================

integrateur%type = type_integrateur
integrateur%pas_variable = pm_i_non
integrateur%nb_etapes = pm_i_nb_etapes(type_integrateur)
integrateur%n = n
integrateur%pas = abs(pas)


! Initialisations des pointeurs de la structure integrateur
nullify(integrateur%tol_rel)
nullify(integrateur%tol_abs)
nullify(integrateur%a)
nullify(integrateur%b)
nullify(integrateur%c)
nullify(integrateur%g_commut)
nullify(integrateur%yDotK)
nullify(integrateur%y_fin)
nullify(integrateur%v)
nullify(integrateur%corr)
nullify(integrateur%alpha)
nullify(integrateur%beta)
nullify(integrateur%spys)
nullify(integrateur%cys)
nullify(integrateur%cyps)
nullify(integrateur%cypsi)
nullify(integrateur%cysi)
nullify(integrateur%dels)


! Pour integrateur Gill
! Allocations memoire pour les tableaux a, b, c, DotK
! initialisation du tableau de Butcher
! =============================================
if (type_integrateur == pm_Gill) then

   integrateur%ordre = pm_i_ordre(type_integrateur)

   allocate(integrateur%a(integrateur%nb_etapes-1,integrateur%nb_etapes-1),stat=alloc_ok)
   if (alloc_ok /= pm_OK) then ! l'allocation dynamique a echoue
      code_retour%valeur = pm_err_allocate
      go to 6000
   end if
   allocate(integrateur%b(integrateur%nb_etapes),stat=alloc_ok)
   if (alloc_ok /= pm_OK) then ! l'allocation dynamique a echoue
      code_retour%valeur = pm_err_allocate
      go to 6000
   end if
   allocate(integrateur%c(integrateur%nb_etapes-1),stat=alloc_ok)
   if (alloc_ok /= pm_OK) then ! l'allocation dynamique a echoue
      code_retour%valeur = pm_err_allocate
      go to 6000
   end if
   call mui_integ_butcher (integrateur,retour_local)  ! recuperation des valeurs du tableau de Butcher
   if (retour_local /= pm_OK) then 
      code_retour%valeur = retour_local
      go to 6000
   end if
   
   integrateur%nb_commut = 0
   integrateur%size_commut = 0
   integrateur%adr_gest_pas = 0
   allocate(integrateur%yDotK(integrateur%nb_etapes,n),stat=alloc_ok)
   if (alloc_ok /= pm_OK) then ! l'allocation dynamique a echoue
      code_retour%valeur = pm_err_allocate
      go to 6000
   end if
   integrateur%t_deb = 0._pm_reel
   integrateur%t_fin = 0._pm_reel
   allocate(integrateur%y_fin(n),stat=alloc_ok)
   if (alloc_ok /= pm_OK) then ! l'allocation dynamique a echoue
      code_retour%valeur = pm_err_allocate
      go to 6000
   end if

endif

! Verification des parametres optionnels pour integrateur Cowell
! ===================================================
if (type_integrateur == pm_Cowell) then

! Verification presence de l ordre

   if (.not. present(ordre)) then
      code_retour%valeur = pm_err_integ_ordre_1
      goto 6000
   endif
      
! Verification de l ordre
   if(ordre < 2) then
      code_retour%valeur = pm_err_integ_ordre_2
      goto 6000
   endif
      
   if(ordre > 16) then
      code_retour%valeur = pm_err_integ_ordre_3
      goto 6000
   endif
      
   if( mod(ordre,2) /= 0 ) then
      code_retour%valeur = pm_err_integ_ordre_4
      goto 6000
   endif

   integrateur%ordre = ordre

! Initialisation de la cle de circularisation
   if (.not. present(icirc)) then
      integrateur%icirc = 0
   else
      integrateur%icirc =  icirc  
   endif

! Verification de la cle de regularisation
   if (.not. present(ireg)) then
      integrateur%ireg = 0
   else
      if((ireg < 0)  .or. (ireg > 2)) then
         code_retour%valeur = pm_err_integ_ireg
         goto 6000
      else
         integrateur%ireg = ireg
      endif
   endif

! Verification de la présence de l'epsilon d'initialisation
   if (.not. present(eps_init)) then
      integrateur%eps_init = 0.1e-13_pm_reel
   else
      integrateur%eps_init = eps_init
   endif

! vérification de la présence de l'epsilon de progression
    if (.not. present(eps_prog)) then
      integrateur%eps_prog = 0.1e-8_pm_reel
   else
      integrateur%eps_prog = eps_prog
   endif

! Vérification de la présence de l'epsilon d'initialisation
! relatif à la variable d'intégration (temps, anomalie..)
   if (.not. present(eps_init_var_integ)) then
      ! Si la valeur n'est pas présente, on prend l'epsilon utilisé pour les
      ! variables à intégrer
      integrateur%eps_init_var_integ = integrateur%eps_init
   else
      integrateur%eps_init_var_integ = eps_init_var_integ
   end if

! Vérification de la présence de l'epsilon de progression
! relatif à la variable d'intégration (temps, anomalie..)
   if (.not. present(eps_prog_var_integ)) then
      ! Si la valeur n'est pas présente, on prend l'epsilon utilisé pour les
      ! variables à intégrer
      integrateur%eps_prog_var_integ = integrateur%eps_prog
   else
      integrateur%eps_prog_var_integ = eps_prog_var_integ
   end if
   



   integrateur%nb_param_lib = 0
! initialisation des paramètres à libérer
   if (.not. present(liber_param))then
      !! pour une compatibilité ascendante, si n=66, on libere tous les paramètres
      if (n == 66) then 
         do i=1,6 
            integrateur%liber_param(i) = 1
         enddo
         integrateur%nb_param_lib = 6
      else
         do i=1,6
            integrateur%liber_param(i) = 0
         enddo
      endif
   else
      do i=1,6
         integrateur%liber_param(i) = liber_param(i)
         integrateur%nb_param_lib = integrateur%nb_param_lib + integrateur%liber_param(i)
      enddo
      ! cohérence entre le nombre de paramètres à libérer et la dimension de l'état
       if ((integrateur%n /= (6+6*(integrateur%nb_param_lib+4))) .and. &
          (integrateur%n /= (7+6*(integrateur%nb_param_lib+4)))) then
        code_retour%valeur = pm_err_dim_etat_cowell
        goto 6000
     endif
   endif

! Verification presence de mu

   if (integrateur%icirc /= 0) then
      if (.not. present(rxmu)) then
         code_retour%valeur = pm_err_integ_rxmu
         goto 6000
      else
         integrateur%rxmu =  rxmu  
      endif
   endif
  
   if (integrateur%ireg /= 0) then
      if (.not. present(rxmu)) then
         code_retour%valeur = pm_err_integ_rxmu
         goto 6000
      else
         integrateur%rxmu =  rxmu  
      endif
   endif

! Initialisation de mode_iteratif
   if (.not. present(mode_iteratif)) then
      integrateur%iteratif = pm_i_oui
   else
      if (.not.mode_iteratif) then
         integrateur%iteratif = pm_i_non
      else
         integrateur%iteratif = pm_i_oui
      endif
   endif

! Allocation des tableaux utiles pour le Cowell

   allocate(integrateur%corr(4,pm_i_dim17),stat=alloc_ok)
   if (alloc_ok /= pm_OK) then ! l'allocation dynamique a echoue
      code_retour%valeur = pm_err_allocate
      go to 6000
   end if

   allocate(integrateur%alpha(pm_i_dim16,pm_i_dim15),stat=alloc_ok)
   if (alloc_ok /= pm_OK) then ! l'allocation dynamique a echoue
      code_retour%valeur = pm_err_allocate
      go to 6000
   end if

   allocate(integrateur%beta(pm_i_dim16,pm_i_dim15),stat=alloc_ok)
   if (alloc_ok /= pm_OK) then ! l'allocation dynamique a echoue
      code_retour%valeur = pm_err_allocate
      go to 6000
   end if

   allocate(integrateur%spys(integrateur%n,pm_i_dim17),stat=alloc_ok)
   if (alloc_ok /= pm_OK) then ! l'allocation dynamique a echoue
      code_retour%valeur = pm_err_allocate
      go to 6000
   end if

   allocate(integrateur%cys(integrateur%n,pm_i_dim17),stat=alloc_ok)
   if (alloc_ok /= pm_OK) then ! l'allocation dynamique a echoue
      code_retour%valeur = pm_err_allocate
      go to 6000
   end if

   allocate(integrateur%cyps(integrateur%n,pm_i_dim17),stat=alloc_ok)
   if (alloc_ok /= pm_OK) then ! l'allocation dynamique a echoue
      code_retour%valeur = pm_err_allocate
      go to 6000
   end if

   allocate(integrateur%cypsi(integrateur%n,pm_i_dim17),stat=alloc_ok)
   if (alloc_ok /= pm_OK) then ! l'allocation dynamique a echoue
      code_retour%valeur = pm_err_allocate
      go to 6000
   end if

   allocate(integrateur%cysi(integrateur%n,pm_i_dim17),stat=alloc_ok)
   if (alloc_ok /= pm_OK) then ! l'allocation dynamique a echoue
      code_retour%valeur = pm_err_allocate
      go to 6000
   end if

   allocate(integrateur%dels(integrateur%n,pm_i_dim17),stat=alloc_ok)
   if (alloc_ok /= pm_OK) then ! l'allocation dynamique a echoue
      code_retour%valeur = pm_err_allocate
      go to 6000
   end if

! Initialisation des parametres integrateur
! =========================================

   integrateur%nb_commut = 0
   integrateur%size_commut = 0
   integrateur%adr_gest_pas = 0
   !! DM-ID 738
   integrateur%adr_fcowell_simp = 0
   if ( mod(integrateur%n,2) == 0) then
      integrateur%calcul_masse = .false.
   else
      integrateur%calcul_masse = .true.
   endif

   integrateur%t_deb = 0._pm_reel
   integrateur%t_fin = 0._pm_reel

   allocate(integrateur%y_fin(n),stat=alloc_ok)
   if (alloc_ok /= pm_OK) then ! l'allocation dynamique a echoue
      code_retour%valeur = pm_err_allocate
      go to 6000
   end if

   integrateur%theta0  = 0._pm_reel ! partie constante de l'equation du temps
   integrateur%a0  = 0._pm_reel     ! 1/2 grand axe osculateur a l'origine
   integrateur%c02 = 0._pm_reel     ! cste des aires au carre , a l'origine
   integrateur%anv0  = 0._pm_reel   ! anomalie vraie osculatrice a l'origine
   integrateur%param  = 0._pm_reel  ! parametre de l'ellipse osculatrice a l'origine
   integrateur%exc  = 0._pm_reel    ! excentricite osculatrice a l'origine
   integrateur%xn  = 0._pm_reel     ! moyen mouvement
   integrateur%ume2  = 0._pm_reel   ! utile a ckeptp et cowreg
   integrateur%rume2  = 0._pm_reel  ! utile a ckeptp et cowreg
   integrateur%corr(:,:) =  0._pm_reel  ! coeff de correction de la circularisation
   integrateur%omeg = 0._pm_reel                  ! pulsation du mouvement periodique principal
   integrateur%nn  = 0 ! ordre integration / 2
   integrateur%nd  = 0 ! 9-nn
   integrateur%nf  = 0 ! 7+nn
   integrateur%nnd  = 0 ! nd+1
   integrateur%nnf  = 0 ! nf+2
   integrateur%nfp  = 0  ! nf+1
   integrateur%sec = 0._pm_reel ! secondes dans le jour
   integrateur%jj   = 0       ! jour julien date initiale
   integrateur%d = 0._pm_reel
   integrateur%dp = 0._pm_reel
   integrateur%alpha(:,:) =  0._pm_reel
   integrateur%beta(:,:) =  0._pm_reel
   integrateur%spys(:,:) =  0._pm_reel
   integrateur%tm = 0._pm_reel
   integrateur%tm_old = 0._pm_reel
   integrateur%iter = 0
   integrateur%ihyp = 0
   integrateur%cys(:,:) =  0._pm_reel
   integrateur%cyps(:,:) =  0._pm_reel
   integrateur%cypsi(:,:) =  0._pm_reel
   integrateur%cysi(:,:) =  0._pm_reel
   integrateur%s   = 0._pm_reel    ! variable independante
   integrateur%dels(:,:) =  0._pm_reel
   integrateur%nn = 0
   integrateur%nd = 0 
   integrateur%nf = 0 
   integrateur%nnd = 0 
   integrateur%nnf = 0 
   integrateur%nfp = 0 
endif

! initialisations code retour
! ===========================

6000 continue

code_retour%routine = pm_num_mu_creer_integrateur
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mu_creer_integrateur_pas_fixe

subroutine mu_creer_integrateur_pas_var (type_integrateur, n, pas_min, pas_max, &
          tol_rel, tol_abs, integrateur, code_retour)

! (C) Copyright CNES - MSPRO - 2005

!************************************************************************
!
! But:  Creation d'un integrateur a pas variable
! ===
!
! Note d'utilisation:  
! ==================
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 5.2 : creation
!                         (Date: 01/2005 - Realisation: Bruno Revelin)
!
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======
use mslib

use int_util_internes_mspro, only : mui_integ_butcher

use type_mspro
use parametre_interne_mspro
use parametre_mspro

use valeur_code_retour_mspro
use numero_routine_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
integer,                              intent(in)     :: type_integrateur! type de l'integrateur
integer,                              intent(in)     :: n               ! dimension du vecteur d'etat
real(pm_reel),                        intent(in)     :: pas_min         ! pas d'integration minimum (pas variable)
real(pm_reel),                        intent(in)     :: pas_max         ! pas d'integration maximum (pas variable)
real(pm_reel),dimension(n),           intent(in)     :: tol_rel         ! tableau des tolerances relatives (pas variable)
real(pm_reel),dimension(n),           intent(in)     :: tol_abs         ! tableau des tolerances absolues (pas variable)
type(tm_integrateur),                 intent(out)    :: integrateur     ! integrateur cree
type(tm_code_retour), intent(out)                    :: code_retour
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

integer :: alloc_ok    ! retour de l'allocation
integer :: retour_local

intrinsic abs

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mu_creer_integrateur.f90: derniere modification V5.15 >'

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour

code_retour%valeur = pm_OK

! Verifications des parametres
! =======================

!  Dimension du vecteur d etat

if (n < 1) then
   code_retour%valeur = pm_err_val_para
   go to 6000
end if

! Pas d integration

if (abs(pas_min)>abs(pas_max)) then
   code_retour%valeur = pm_err_val_para
   go to 6000
end if

! type de l integrateur

if (type_integrateur /= pm_DOP853) then
   code_retour%valeur = pm_err_type_integ
   go to 6000
end if

! Initialisation des parametres integrateur
! =========================================

integrateur%type = type_integrateur
integrateur%pas_variable = pm_i_oui
integrateur%nb_etapes = pm_i_nb_etapes(type_integrateur)
integrateur%ordre = pm_i_ordre(type_integrateur)
integrateur%n = n
integrateur%pas_min = abs(pas_min)
integrateur%pas_max = abs(pas_max)

! Allocations memoire et initialisation du tableau de Butcher
! ===========================================================

allocate(integrateur%tol_rel(n),stat=alloc_ok)
if (alloc_ok /= pm_OK) then ! l'allocation dynamique a echoue
   code_retour%valeur = pm_err_allocate
   go to 6000
end if
integrateur%tol_rel(:) = tol_rel(:)

allocate(integrateur%tol_abs(n),stat=alloc_ok)
if (alloc_ok /= pm_OK) then ! l'allocation dynamique a echoue
   code_retour%valeur = pm_err_allocate
   go to 6000
end if
integrateur%tol_abs(:) = tol_abs(:)

allocate(integrateur%a(integrateur%nb_etapes-1,integrateur%nb_etapes-1),stat=alloc_ok)
if (alloc_ok /= pm_OK) then ! l'allocation dynamique a echoue
   code_retour%valeur = pm_err_allocate
   go to 6000
end if
allocate(integrateur%b(integrateur%nb_etapes),stat=alloc_ok)
if (alloc_ok /= pm_OK) then ! l'allocation dynamique a echoue
   code_retour%valeur = pm_err_allocate
   go to 6000
end if
allocate(integrateur%c(integrateur%nb_etapes-1),stat=alloc_ok)
if (alloc_ok /= pm_OK) then ! l'allocation dynamique a echoue
   code_retour%valeur = pm_err_allocate
   go to 6000
end if

call mui_integ_butcher (integrateur,retour_local)  ! recuperation des valeurs du tableau de Butcher

if (retour_local /= pm_OK) then 
   code_retour%valeur = retour_local
   go to 6000
end if

! Initialisation des parametres integrateur
! =========================================

integrateur%nb_commut = 0
integrateur%size_commut = 0
integrateur%adr_gest_pas = 0
allocate(integrateur%yDotK(integrateur%nb_etapes,n),stat=alloc_ok)
if (alloc_ok /= pm_OK) then ! l'allocation dynamique a echoue
   code_retour%valeur = pm_err_allocate
   go to 6000
end if
integrateur%t_deb = 0._pm_reel
integrateur%t_fin = 0._pm_reel
allocate(integrateur%y_fin(n),stat=alloc_ok)
if (alloc_ok /= pm_OK) then ! l'allocation dynamique a echoue
   code_retour%valeur = pm_err_allocate
   go to 6000
end if

! specifique aux integrateurs a pas variable
integrateur%v_initialise = pm_i_non
allocate(integrateur%v(pm_i_taille_v(type_integrateur),n),stat=alloc_ok)
if (alloc_ok /= pm_OK) then ! l'allocation dynamique a echoue
   code_retour%valeur = pm_err_allocate
   go to 6000
end if

! initialisations code retour
! ===========================

6000 continue

code_retour%routine = pm_num_mu_creer_integrateur
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mu_creer_integrateur_pas_var
