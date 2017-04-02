subroutine mui_integ_cowell ( fsub_cowell, integrateur, t0, y0, t, y, retour_fsub, pb_fsub, retour)

! (C) Copyright CNES - MSPRO - 2005

!************************************************************************
!
! But:  Integration a pas fixe de type Cowell
! ===
! integration fine de systemes differentiels de 1er et 2eme ordre
! par la methode de cowell
!
! Note d'utilisation:  
! ==================
!  Integration numerique de Cowell issu de AQUARELS
!
!  paramètre d'entrée/sortie
!  -------------------------
!*par type(tm_integrateur) :: integrateur  : integrateur utilisé
!  
!  paramètre d'entrée
!  ------------------
!*par fonction          :: fsub_cowell     : fonction de calcul du second membre de l'équation à intégrer
!*par type(tm_jour_sec) ::  t0             : date initiale de l'intégration. /!\ Cette date reste constante
!                                            même lorsque l'on intègre de tn à tn+1
!*par real(pm_reel),dim ::  y0             : valeur de la fonction a t (valeur de la fonction à t0 lors du 1er appel)
!*par type(tm_jour_sec) ::  t              : abscisse du point demandé
!
!  paramètres de sortie
!  --------------------
!*par real(pm_reel),dimension(integrateur%n) ::  y            :  valeur de la fonction en t       
!*par integer,                               ::  retour_fsub  :  code retour de fsub              
!*par type(tm_jour_sec)                      ::  pb_fsub      :  abscisse posant pb a fsub  
!*par integer,                               ::  retour       :  code retour                      
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!     + Version 5.3 : creation 
!                     DM-ID 408 : mettre un Cowell dans les integrateurs MSPRO
!                     (Date: 10/2005 - Realisation: Equipe Patrimoine Mecanique Spatiale - Atos Origin)
!     + Version 5.4 : modification
!                     DM-ID 471 : Integration à rebours
!                     (Date: 02/2006 - Realisation: Atos Origin)
!                     FA-ID 439 : remarques qualite
!                     DM-ID 492 : optimisation de l'utilisation du critere de convergence de
!                     l'integrateur de Cowell
!                     (Date: 02/2006 - Realisation: Atos Origin)
!     + Version 5.5 : modification
!                     DM-ID 476 : Matrices de transition
!                     (Date: 04/2006 - Realisation: Atos Origin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!                   FA-ID 622 : Non initialisation d'une variable interne
!   + Version 5.7 : DM_ID 738 : Evolution du Cowell
!                   (Date:06/2007 - Realisation: Sandrine Avril- Atos origin)
!   + Version 5.8 : FA-ID 829 : Suppression des variables inutilisées
!                   (Date : 10/2007 - Réalisation: Atos origin)
!   + Version 5.9 : DM-ID 964 : Permettre de changer de sens d'intégration sans ré-initialiser
!                   l'intégrateur.
!                   FA-ID 961 : Mise à jour des cartouches du code du Cowell
!                   (Date : 03/2008 - Réalisation: Y.TANGUY - Atos Origin)
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
     
use parametre_interne_mspro
use parametre_mspro
use type_mspro
use valeur_code_retour_mspro

use int_util_internes_mspro, only : mui_cowell_cnicow
use int_util_internes_mspro, only : mui_cowell_cowcir
use int_util_internes_mspro, only : mui_cowell_cowreg
use int_util_internes_mspro, only : mui_cowell_normal
use int_util_internes_mspro, only : mui_cowell_chavar
use int_util_internes_mspro, only : mui_cowell_inicow
use int_util_internes_mspro, only : mui_cowell_cowitg
use int_util_internes_mspro, only : mui_cowell_ckeptp
use int_util_internes_mspro, only : mui_cowell_decala
use int_util_internes_mspro, only : mui_cowell_cowint

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

interface
   subroutine fsub_cowell(t,y,ydot,retour)     ! equation differentielle

   use mslib
   
   type(tm_jour_sec),intent(in)                    ::  t     ! abscisse
   real(pm_reel),dimension(:),intent(in)           ::  y     ! vecteur d'etat
   real(pm_reel),dimension(:),intent(out)          ::  ydot  ! derivee en t
   integer,                   intent(out)          ::  retour
   
   end subroutine fsub_cowell
end interface
   
type(tm_integrateur),                  intent(inout)                  ::  integrateur  ! integrateur utilise
type(tm_jour_sec),                     intent(in)                     ::  t0           ! valeur initiale de t
real(pm_reel),dimension(integrateur%n),intent(in)                     ::  y0           ! valeur de la fonction a t0
type(tm_jour_sec),                     intent(in)                     ::  t            ! abscisse du point demande
real(pm_reel),dimension(integrateur%n),intent(out)                    ::  y            ! valeur de la fonction en t
integer,                               intent(out)                    ::  retour_fsub  ! code retour de fsub
type(tm_jour_sec),  intent(out) ::  pb_fsub      ! abscisse posant pb a fsub
integer,                               intent(out)                    ::  retour       ! code retour                      

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------
integer :: i     ! indicateurs de boucles

integer :: n1     ! nombre total d'equations differentielles
integer :: n2     ! nombre d'equations du 2nd ordre
integer :: n4     ! nbre total d eq. diff

integer :: iboucl

real (pm_reel) :: eps    ! critere d'arret de stabili. des derivees n-ieme (phase de progression)
real (pm_reel) :: epsini ! critere d'arret de stabili. a la premiere iteration
real (pm_reel) :: eps_var_integ    ! critere d'arret de stabili. de la variable d'intégration (phase de progression)
real (pm_reel) :: epsini_var_integ ! critere d'arret de stabili. de la variable d'intégration (initialisation)
real (pm_reel) :: per    ! definition de la pseudo-periode
real (pm_reel) :: delta_t ! intervalle de temps
real (pm_reel) :: ta     ! temps compte a partir de l'integration
real (pm_reel) :: tk     ! partie keplerienne du temps
real (pm_reel) :: sig
real (pm_reel) :: tps
real (pm_reel) :: seed   ! variable independante 

real (pm_reel), dimension(integrateur%n) :: y_init
real (pm_reel), dimension(integrateur%n) :: yp_init
real (pm_reel), dimension(integrateur%n) :: yn
real (pm_reel), dimension(integrateur%n) :: ypn
real (pm_reel), dimension(integrateur%n) :: yps
real (pm_reel), dimension(integrateur%n) :: ys
real (pm_reel), dimension(integrateur%n) :: ypsig
real (pm_reel), dimension(integrateur%n) :: ysig
real (pm_reel), dimension(integrateur%n) :: ynsig
real (pm_reel), dimension(integrateur%n) :: ypnsig
real (pm_reel), dimension(integrateur%n) :: ypssig
real (pm_reel), dimension(integrateur%n) :: yssig
real (pm_reel), dimension(integrateur%n,pm_i_dim17) :: dels
real (pm_reel), dimension(6,10) :: phinit ! matrice pour l'initialisation 
                                          ! de la matrice de transition initiale
                                          ! de dimension (6,nombre de paramètres du modèle)
real (pm_reel), dimension(integrateur%n,pm_i_dim17) :: spys_tmp
real (pm_reel), dimension(integrateur%n,pm_i_dim17) :: cys_tmp
real (pm_reel), dimension(integrateur%n,pm_i_dim17) :: cyps_tmp
real (pm_reel), dimension(integrateur%n,pm_i_dim17) :: cypsi_tmp
real (pm_reel), dimension(integrateur%n,pm_i_dim17) :: cysi_tmp
real (pm_reel), dimension(integrateur%n,pm_i_dim17) :: dels_tmp



real(pm_reel) :: jourfrac0, jourfrac 
type(tm_code_retour) :: code_retour_local
integer :: retour_sub
real(pm_reel) :: eps100              ! variable epsilon machine * 100 
logical :: calcul_mat                ! =pm_i_oui si calcul de la matrice de transition demande
integer :: dim_mat, dim_matsur2=0    ! dimension de la matrice de transition
integer :: j, irang, k

logical :: sauve_tm_old

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mui_integ_cowell.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

! autres initialisations
eps100 = 100._pm_reel * epsilon(1._pm_reel)

! initialisation des variables locales
y_init(:) = 0._pm_reel
yp_init(:) = 0._pm_reel
yn(:) = 0._pm_reel
ypn(:) = 0._pm_reel
yps(:) = 0._pm_reel
ys(:) = 0._pm_reel
ypsig(:) = 0._pm_reel
ysig(:) = 0._pm_reel
ynsig(:) = 0._pm_reel
ypnsig(:) = 0._pm_reel
ypssig(:) = 0._pm_reel
yssig(:) = 0._pm_reel
dels(:,:) = 0._pm_reel
calcul_mat = .false.
spys_tmp(:,:) = 0._pm_reel
cys_tmp(:,:) = 0._pm_reel
cyps_tmp(:,:) = 0._pm_reel
cypsi_tmp(:,:) = 0._pm_reel
cysi_tmp(:,:) = 0._pm_reel
dels_tmp(:,:) = 0._pm_reel

!***********************************************************************
! initialisation parametres d integration
!***********************************************************************
!
!n1 = 4
!n2 = 3
!n4=n1+1
if (integrateur%calcul_masse) then
   n2 = (integrateur%n - 1)/2
   n1 = n2+1
else
   n2 = integrateur%n/2
   n1 = n2
endif
n4 = n1+1

! n2 : nombre d'équations à intégrer au 2nd ordre
! Si n2 > 3, un calcul de matrice de transition est demande
if (n2> 3) then
   calcul_mat = pm_i_oui
   dim_mat = 6*(integrateur%nb_param_lib + 4)
   dim_matsur2 = dim_mat/2
endif

! eps    : critere d'arret de stabili. des derivees n-ieme
eps = integrateur%eps_prog
! eps_prog : critère d'arret de stabilité pour la variable d'intégration (en phase de progression)
eps_var_integ = integrateur%eps_prog_var_integ

! epsini : critere d arret de stabili. a la premiere iteration
epsini = integrateur%eps_init
! epsini_var_integ : critère d'arret de stabiltié pour la variable d'intégration (en phase d'initialisation)
epsini_var_integ = integrateur%eps_init_var_integ

! si calcul_mat, initialisation de la matrice de transition
! on definit phinit de dimension(6, nb_param_modele) initialise a 1 sur la diagonale
irang = 1
if (calcul_mat) then
   ! initialisation de phinit à la matrice nulle
   phinit(:,:) = 0._pm_reel
   ! positionnement de 1 sur la diagonale
   do i=1,6
      phinit(i,i) = 1.0_pm_reel
   enddo
   do i = 1,10 !! initialisation des vecteurs y_init et yp_init
      if ((i <= 6 .and. integrateur%liber_param(i) /= 0) .or. (i >= 7)) then
         do j = 1,3
            y_init(3*irang+j)  = phinit(j,i)
            yp_init(3*irang+j) = phinit(j+3,i)
         enddo
         
         irang = irang + 1
      endif
   enddo
   
endif

!! prise en compte de l'équation de masse
if (integrateur%calcul_masse) then
   y_init(3*irang + 1) = y0(integrateur%n)
   yp_init(3*irang + 1) = 0
endif

!***********************************************************************
! 1ere iteration : initialisation du cowell
!***********************************************************************
if (integrateur%iter == 0) then

!**********************************************************************
! Cas d une integration à rebours
! Remarque : si l'intégration à rebours se produit après initalisation,
! le changement de sens est automatique
! 
!**********************************************************************
call md_joursec_jourfrac (t0,jourfrac0,code_retour_local)  ! pas d'erreur possible
call md_joursec_jourfrac (t,jourfrac,code_retour_local)  ! pas d'erreur possible
if ((jourfrac-jourfrac0) < eps100) then
   integrateur%pas = - abs(integrateur%pas)
endif


!***********************************************************************
! initialisation parametres d integration
!***********************************************************************
!
   integrateur%nn=integrateur%ordre/2
   integrateur%nd=9-integrateur%nn
   integrateur%nf=7+integrateur%nn
   integrateur%nnd=integrateur%nd+1
   integrateur%nnf=integrateur%nf+2
   integrateur%nfp=integrateur%nf+1
      
!***********************************************************************
!   dans le cas ireg=2 et icircl=1 on force ireg=3
!   (changement des variables a integrer x=x/r)
!***********************************************************************
!
   if((integrateur%ireg == 2).and.(integrateur%icirc /= 0)) integrateur%ireg=3
    
! y      : positions initiales (m)
   y_init(1:3) = y0(1:3)
    
! yp     : vitesses initiales (m/s)
   yp_init(1:3) = y0(4:6)

! definition de la pseudo-periode =0 => calculee par cowell si icircl <> 0
   per=0.0_pm_reel
!
!***********************************************************************
!* initialisation parametres date initiale
!***********************************************************************
!
   integrateur%jj = t0%jour
   integrateur%sec = t0%sec
!
!*******************************************************************
!* chargement Parametres d integration
!*******************************************************************
!
   call mui_cowell_cnicow(integrateur,retour)
   if (retour /= pm_OK) go to 6000

!
!*******************************************************************
!* chargement Parametres de circularisation
!*******************************************************************
!
   call mui_cowell_cowcir(integrateur, integrateur%icirc,per,y_init,yp_init,retour)
   if (retour < pm_OK) go to 6000
!
!*******************************************************************
!* chargement du common coreg
!*******************************************************************
!
   call mui_cowell_cowreg(integrateur,y_init,yp_init,per,integrateur%icirc,retour)
   if (retour < pm_OK) go to 6000
!
!******************************************************************
!* constitution de tableaux de longueur n4
!******************************************************************
!
   tps = pm_i_zero
   call mui_cowell_normal(integrateur,yn,ypn,y_init,yp_init,tps,pm_i_nze,n1,n2,n4,retour)
   if (retour /= pm_OK) go to 6000
!
!*******************************************************************
!* passage a la variable independante seed
!*******************************************************************
!
   integrateur%s=0.0_pm_reel
   seed=0.0_pm_reel
   call mui_cowell_chavar(integrateur,yn,ys,ypn,yps,seed,pm_i_nun,n1,n2,n4,retour)
   if (retour /= pm_OK) go to 6000
   integrateur%s=seed

!
!******************************************************************
!* conditions initiales sur les tableaux cxx,cxxs
!******************************************************************
!
   do  i=1,n4
      integrateur%cys(i,9)=ys(i)
      integrateur%cyps(i,9)=yps(i)
   end do
!
!*******************************************************************
!* initialisation du tableau dels (derivees secondes)
!*******************************************************************
!
   call mui_cowell_inicow(fsub_cowell,integrateur,ys,yps,integrateur%icirc,  &
        n1,n2,n4,dels,retour_fsub, pb_fsub,retour_sub)
   if (retour_sub /= pm_OK) then
      retour = retour_sub
      if (retour_sub < pm_OK) go to 6000
   end if
   integrateur%dels(:,:) = dels(:,:)
!
!******************************************************************
!* initialisation des tableaux cxx,cxxs de cowell : premier
!* appel a cowitg. normalement , on devrait seulement arreter
!* l'integration en cas de divergence apres l'initialisation
!* en fait , on arrete meme en cas de divergence au demarrage
!******************************************************************
!
   integrateur%iter=1
!
   call mui_cowell_cowitg(fsub_cowell,integrateur,integrateur%cys,integrateur%cyps,  &
        integrateur%s,integrateur%dels,integrateur%iter,epsini, epsini_var_integ, &
        n1,n2,n4,iboucl,retour_fsub,pb_fsub,retour)
   if (retour /= pm_OK) then
      if (retour < pm_OK) go to 6000
   end if

!
   call mui_cowell_ckeptp(integrateur,integrateur%s-integrateur%pas*integrateur%nn,&
        integrateur%ireg,tk,retour)
   if (retour /= pm_OK) go to 6000
!
   call mui_cowell_decala(integrateur,integrateur%cys,integrateur%cyps,&
        integrateur%ordre,n4,integrateur%cysi,integrateur%cypsi,retour)
   if (retour /= pm_OK) go to 6000
!

   ! Passage de integrateur%s à integrateur%tm
   ! remarque: theta0 est la partie constante de l'équation du temps.
   if (integrateur%ihyp == 0) then
! ** cas elliptique: cypsi contient la partie periodique du temps
! dans le cas de changement t -> s, sinon 0
!
      integrateur%tm=integrateur%theta0+integrateur%s-integrateur%pas*integrateur%nn+&
           integrateur%cypsi(n4,9)+tk
   else
! ** cas hyperbolique: cypsi contient directement la valeur de t
! dans le cas de changement t -> s, sinon 0
!
      if (integrateur%ireg == 0) then
         integrateur%tm=integrateur%s-integrateur%pas*integrateur%nn
      else
! attention trop de decalages avec : tm=cypsi(n4,9)-pas*nn
!
         integrateur%tm=integrateur%cypsi(n4,9)
      endif
   endif
   integrateur%iter=integrateur%iter+1

!
   integrateur%tm_old = integrateur%tm

endif

! Si l integrateur de Cowell a deja ete initialise
if (integrateur%iter > 0) then

! Date de debut d'integration en jour/sec : t0
! Date de fin d'integration en jour/sec : t

   integrateur%jj = t0%jour
   integrateur%sec = t0%sec

! Duree entre le debut et la fin de l integration

   delta_t = real(t%jour-integrateur%jj,kind=pm_reel)
   delta_t=delta_t*86400.0_pm_reel + (t%sec-integrateur%sec)
!---------------------------------------------------------
! Détection de changement de sens lors de l'intégration. !
!---------------------------------------------------------
! Cela se produit lorsqu'un appel à mu_integrer (cowell) est réalisé avec une date antérieure
! à la date du précédent appel (cas d'une intégration posigrade au précédent appel).
! 
! Selon la date d'appel à l'intégrateur, le traitement sera différent.
! Le schéma ci-dessous illustre le cas d'un pas initial positif (on intégre "en avant")
! 
! ------------------------------|-----------||------------|--------------------->
!                               < support interpolation >                 temps-> 
!               t1                               t2      tm        t3
! Si la date d'appel t est égale à :
! - t3 : cas nominal -> on intègre vers une date ultérieure à la date précédente (tm). On sauvegardera la date t3 (tm=t3)
! - t2 : on intégre vers une date antérieure, mais qui se situe dans la partie "avant" du support d'interpolation
!        -> dans ce cas, le pas ne change pas de sens, mais on ne sauvegarde pas la date tm (ie : le support d'interpolation
!           n'avance pas non plus). On se contente d'interpoler la sortie
! - t1 : on intégre vers une date antérieure, qui se situe en dehors (avant) le support d'interpolation
!        -> dans ce cas, on permute les tableaux, on inverse le pas
!           et on rentre dans une phase de progression (cowitg) à l'issue de laquelle la date tm sera sauvegardée 
!           (pas = - pas ; permutation des tableaux ; progression ; tm=t2 ; interpolation de la sortie à t2)
!
! /!\ la limite de permutation des tableaux est définie par tm-ordre/2*pas ( nn = ordre/2 )
! Ceci permet d'assurer que l'on interpolera uniquement sur la partie "sûre" du tableau d'interpolation.
!-----------------------------------------------------------------------------------------------------------
   if (( (delta_t < integrateur%tm_old-(integrateur%nn)*abs(integrateur%pas)).and. ( integrateur%pas > 0))  &
        .or. ((delta_t > integrateur%tm_old+(integrateur%nn)*abs(integrateur%pas)).and.( integrateur%pas < 0))) then
      
      
      !      ! le pas change de sens !
      integrateur%pas = - integrateur%pas
     
      !      ! les tableaux de support sont permutes
      do i = 1, integrateur%n
         do j = 1, pm_i_dim17
            ! tab_tmp(j) = tab(17 - (j -1))
            ! ex : tab_tmp(1) = tab(17)
            !      tab_tmp(2) = tab(16)
            ! ..
            k = pm_i_dim17 - (j - 1) 

            spys_tmp(i,j) = integrateur%spys(i,k)
            cys_tmp(i,j) = integrateur%cys(i,k)
            cyps_tmp(i,j) = integrateur%cyps(i,k)
            cypsi_tmp(i,j) = integrateur%cypsi(i,k)
            cysi_tmp(i,j) = integrateur%cysi(i,k)
            dels_tmp(i,j) = integrateur%dels(i,k)
        

         enddo
      enddo
      ! Affectation des tableaux de l'intégrateur avec les valeurs
      ! des tableaux temporaires qui viennent d'être permutés
      integrateur%spys(:,:)= spys_tmp(:,:)
      integrateur%cys(:,:)= cys_tmp(:,:)
      integrateur%cyps(:,:)= cyps_tmp(:,:)
      integrateur%cypsi(:,:)= cypsi_tmp(:,:)
      integrateur%cysi(:,:)= cysi_tmp(:,:)
      integrateur%dels(:,:)= dels_tmp(:,:)

      ! On a changé de sens, sans ré-initialiser (simple permutation des tableaux)
      ! -> on sauvegarde de nouveau la position du tableau d'interpolation
      
      sauve_tm_old = .true.

   else if (( (integrateur%tm_old-(integrateur%nn)*abs(integrateur%pas) <= delta_t .and. &
        delta_t < integrateur%tm_old).and. ( integrateur%pas > 0)) .or. &
        ((delta_t <= integrateur%tm_old+(integrateur%nn)*abs(integrateur%pas) .and. &
        integrateur%tm_old < delta_t).and.( integrateur%pas < 0))) then
      
      ! On a changé de sens, mais en restant "dans" l'intervalle d'interpolation,
      ! qui est délimité par tm-nn*pas et tm (nn = ordre/2 ; cas d'un pas positif)
      ! -> on ne sauvegarde pas tm, afin de permuter la prochaine fois, si jamais
      ! la date demandée sort du tableau d'interpolation
      sauve_tm_old = .false.
            
   else
      
      ! On reste dans le même sens que précédemment, on sauvegarde donc la variable tm dans tm_old,
      ! afin de connaître la borne max de l'intervalle d'interpolation
      sauve_tm_old = .true.
      
   end if

!****************************************************************
!* iterations suivantes,recherche de la 1ere date de sortie
!****************************************************************
!
!  Test sur la date courante : 
!             - si celle-ci est plus petite que la durée avec une
!               intégration à pas > 0,
!        ou
!             - si celle-ci est plus grande que la durée avec une
!               intégration à pas < 0 : 
!        Progression du COWELL d'un pas
!
!  sinon, on a dépassé la date de l'événement à traiter
!         on peut donc interpoler le résultat

   do while (((integrateur%tm < delta_t).and.( integrateur%pas > 0))  &
        .or. ((delta_t < integrateur%tm).and.( integrateur%pas < 0)) )

      ! Phase de progression
      call mui_cowell_cowitg(fsub_cowell,integrateur,integrateur%cys,integrateur%cyps,  &
           integrateur%s,integrateur%dels,integrateur%iter,eps, eps_var_integ, &
           n1,n2,n4,iboucl,retour_fsub,pb_fsub,retour,mode_iteratif=integrateur%iteratif)
      if (retour /= pm_OK) then
         if (retour < pm_OK) go to 6000
      end if
!
!   arret de l'integration en cas de divergence en routine
!
      call mui_cowell_ckeptp(integrateur,integrateur%s-integrateur%pas*integrateur%nn,&
           integrateur%ireg,tk,retour)
      if (retour /= pm_OK) go to 6000
!
      call mui_cowell_decala(integrateur,integrateur%cys,integrateur%cyps,&
           integrateur%ordre,n4,integrateur%cysi,integrateur%cypsi,retour)
      if (retour /= pm_OK) go to 6000
!
!        Calcul de la date courante à l'aide du tableau des fonctions
!        intégrées au point correspondant à l'équation du temps, 
!        au milieu du tableau de COWELL

     if (integrateur%ihyp == 0) then
! ** cas elliptique: cypsi contient la partie periodique du temps
! dans le cas de changement t -> s, sinon 0
!
         integrateur%tm=integrateur%theta0+integrateur%s-integrateur%pas*integrateur%nn+&
              integrateur%cypsi(n4,9)+tk
      else
! ** cas hyperbolique: cypsi contient directement la valeur de t
! dans le cas de changement t -> s, sinon 0
!
         if (integrateur%ireg == 0) then
            integrateur%tm=integrateur%s-integrateur%pas*integrateur%nn
         else
! attention trop de decalages avec : tm=cypsi(n4,9)-hpas*nn
!
            integrateur%tm=integrateur%cypsi(n4,9)
         endif
      endif
      integrateur%iter=integrateur%iter+1
!
! Sauvegarde de la date tm de l'integration
      if (sauve_tm_old) then
         integrateur%tm_old = integrateur%tm
      end if
   enddo

!
!******************************************************************
!* la date est encadree,interpolation des resultats
!*****************************************************************
!
   call mui_cowell_cowint(integrateur,integrateur%cysi,integrateur%cypsi,&
        integrateur%s-integrateur%pas*real(integrateur%nn,kind=pm_reel),delta_t, &
        integrateur%pas,integrateur%ordre,n1,n2,n4,integrateur%ireg,&
        integrateur%ihyp,yssig,ypssig,sig,retour)
   if (retour /= pm_OK) go to 6000

!
!******************************************************************
!* restitution de la variable temps
!******************************************************************
!
   call mui_cowell_chavar(integrateur,ynsig,yssig,ypnsig,ypssig,sig,pm_i_nze, &
        n1,n2,n4,retour)
   if (retour /= pm_OK) go to 6000
!
!*******************************************************************
!* retour a la taille originale des tableaux
!******************************************************************
!
   call mui_cowell_normal(integrateur,ynsig,ypnsig,ysig,ypsig,ta,pm_i_nun,n1,n2,n4,retour)
   if (retour /= pm_OK) go to 6000

!
! y      : positions et vitesses finales

   y(1:3) = ysig(1:3)
    
   y(4:6) = ypsig(1:3)

   
   if (calcul_mat) then
      y(7:dim_matsur2+7-1) = ysig(4:dim_matsur2 + 4 -1)
      if (integrateur%calcul_masse) then
         y(dim_matsur2+7:integrateur%n-1) = ypsig(4:dim_matsur2 + 4 -1)
         y(integrateur%n) = ysig(dim_matsur2 + 4)
      else
         y(dim_matsur2+7:integrateur%n) = ypsig(4:dim_matsur2 + 4 -1)
      endif
   else
      if (integrateur%calcul_masse) y(integrateur%n) = ysig(4)
   endif

   integrateur%joursec_fin = t

endif
!

6000 continue

end subroutine mui_integ_cowell
