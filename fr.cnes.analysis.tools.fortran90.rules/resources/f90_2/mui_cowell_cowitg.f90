subroutine mui_cowell_cowitg(fsub_cowell,integrateur,cxs,cxps,s,dels,iter,eps,eps_var_integ, &
     n1,n2,n4,iboucl,retour_fsub,pb_fsub,retour,mode_iteratif)

! (C) Copyright CNES - MSPRO - 2005

!************************************************************************
!
! But:  
! ===     
!*rol execution de l'integration numerique :
!*rol   progression des tableaux x et xps d'un cran
!
! Note d'utilisation: 
! ================== 
!  paramètre d'entrée/sortie
!  -------------------------
!*par integrateur  : integrateur utilisé
!*par cxs          : tableau de cowell de la fonction a integrer     
!*par cxps         : tableau de cowell des derivees de cette fonction
!*par s            : variable d'intégration
!*par dels         : dérivées secondes
!*par iter         : nb d'itérations effectuées (entre 0 et ordre/2) 
!                    une fois ordre/2 atteint, on est en phase d'intégration convergée.
!
!  
!  paramètre d'entrée
!  ------------------
!*par subroutine fsub_cowell : routine de calcul du second membre de l'équation de l'accélération
!*par eps                    : epsilon pour les variables du mouvement (positions / vitesses )                         
!*par eps_var_integ          : epsilon pour la variable d'intégration (temps, anomalie vraie, anomalie excentrique)    
!*par n1                     : indice de l'équation du 1er ordre (masse) ou nombre d'équations du second ordre         
!*par n2                     : nombre d'equations du second ordre                                                      
!*par n4                     : nombre total d'equations differentielles (2nd ordre, 1er ordre + variable d'intégration)
!*par mode_iteratif          : utilisation du mode itératif
! 
!  paramètres de sortie
!  --------------------
!*par iboucl         :  nombre de boucles de calculs effectuées avant convergence
!*par retour_fsub    :  code retour de fsub                                      
!*par pb_fsub        :  abscisse posant pb a fsub                                
!*par retour         :  code retour                                              
!
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
!                     FA-ID 439 : remarques qualite
!                     (Date: 02/2006 - Realisation: Atos Origin)
!                     DM-ID 492 : optimisation de l'utilisation du critere de convergence de
!                     l'integrateur de Cowell
!                     (Date: 02/2006 - Realisation: Atos Origin)
!                     DM-ID 492 : optimisation de l'utilisation du critere de convergence de
!                     l'integrateur de Cowell
!                     (Date: 02/2006 - Realisation: Atos Origin)
!     + Version 5.5 : modification suite refus DV6
!                     Suppression du goto 200
!                     (Date: 05/2006 - Realisation: Atos Origin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!
!   + Version 5.7 : DM-ID 738 :  Evolution du Cowell
!                   (Date:06/2007 - Realisation: Sandrine Avril - Atos origin) 
!   + Version 5.9 : DM-ID 983 :  Nouvelle valeur d'epsilon pour la convergence 
!                                de la variable d'intégration
!                   FA-ID 961 : maj des cartouches
!                   FA-ID 971 : nb max d'équations du Cowell défini par une constante
!                         (Date: 03/2008 - Réalisation Y.TANGUY Atos Origin)
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
use type_mspro
use parametre_interne_mspro ! acces aux parametres internes de la librairie MSPRO
use valeur_code_retour_mspro

use int_util_internes_mspro, only : mui_cowell_cowsec
use int_util_internes_mspro, only : mui_cowell_ckeptp

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
   real(pm_reel), dimension(integrateur%n, pm_i_dim17), intent(inout)    :: cxs           ! tableau de cowell de la fonction a integrer     
   real(pm_reel), dimension(integrateur%n, pm_i_dim17), intent(inout)    :: cxps          ! tableau de cowell des derivees de cette fonction
   real(pm_reel), intent(inout)                                          :: s             ! variable d'intégration //!
   real(pm_reel), dimension(integrateur%n, pm_i_dim17), intent(inout)    :: dels          ! dérivées secondes
   integer, intent(inout)                                                :: iter          ! nb d'itérations effectuées (entre 0 et ordre/2) : une fois ordre/2 atteint, on est en phase d'intégration convergée.

   real(pm_reel), intent(in) :: eps            ! epsilon pour les variables du mouvement (positions / vitesses )
   real(pm_reel), intent(in) :: eps_var_integ  ! epsilon pour la variable d'intégration (temps, anomalie vraie, anomalie excentrique)
   integer, intent(in)       :: n1             ! indice de l'équation du 1er ordre (masse) ou nombre d'équations du second ordre
   integer, intent(in)       :: n2             ! nombre d'equations du second ordre      
   integer, intent(in)       :: n4             ! nombre total d'equations differentielles (2nd ordre, 1er ordre + variable d'intégration)

   integer, intent(out)            :: iboucl        ! nombre de boucles de calculs effectuées avant convergence
   integer, intent(out)            :: retour_fsub   ! code retour de fsub
   type(tm_jour_sec),  intent(out) ::  pb_fsub      ! abscisse posant pb a fsub
   integer, intent(out)            :: retour        ! code retour                                              
   logical, intent(in), optional   :: mode_iteratif ! utilisation du mode itératif (.true. / .false.)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
   
   integer :: i, j, n
   integer :: iif,jm9,k,mnp9

   ! Remarque : 
   ! - les tableaux del et delp sont recalculés à chaque appel,
   ! en commençant par le point milieu (9è colonne) et de proche en proche
   ! autour de ce point.
   ! - ces tableaux sont initialisés et utilisés progressivement, à partir du point milieu
   ! /!\ il n'y a pas d'initialisation à 0 en début de calcul (gain de temps de calcul)
   real(pm_reel), dimension(pm_i_NBMAX_EQU_COWELL, pm_i_dim17)  :: del,delp
   real(pm_reel) :: dxp,h2,ss,tk
   real(pm_reel), dimension(integrateur%n) :: xpp,xpm
   real(pm_reel) :: z,zp

   logical :: poursuivre200, poursuivreIter
   logical :: iteratif 

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mui_cowell_cowitg.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
   retour = pm_OK

! autres initialisations

   xpp(:) =  0._pm_reel
   xpm(:) =  0._pm_reel

   iboucl=0
   poursuivre200=.true.
   poursuivreIter=.true.

!iterations
   if (.not. present(mode_iteratif)) then
      iteratif = pm_i_oui
   else
      if (.not.mode_iteratif) then
         iteratif = pm_i_non
      else
         iteratif = pm_i_oui
      endif
   endif



!***********************************************************************
!     On limite iter à la valeur ORDRE / 2
!     Lorsque iter a atteint cette valeur, on passe en phase
!     d'intégration convergée
!-----------------------------------------------------------------------
   if(iter > integrateur%nn)  iter=integrateur%nn
!
   if(iter == 1) then
      do  i=1,n4
         cxps(i,integrateur%nnf-1)=0.0_pm_reel
         cxps(i,integrateur%nd)=0.0_pm_reel
         cxs(i,integrateur%nd)=0.0_pm_reel
      end do
   endif
!
! modif : on stabilise l'avant dernier point du support
!         c'est a dire toujours le point nnf-1
! pour iter a 1 :   cxps au point nnf-1 vaut 0
!------------------------------------------
!
   do  i=1,n4
      xpm(i)=cxps(i,integrateur%nnf-1)
   end do
!---
!     Calcul du pas au carré (utile pour le calcul de X)
!
   h2=integrateur%pas*integrateur%pas
!
!200 continue
   do while (poursuivre200)
      poursuivre200=.false.
      poursuivreIter=.true.

!
!***********************************************************************
!*fon calcul de del,delp au point central (en 9)
!***********************************************************************
!
!     Boucle sur tous les paramètres à integrer

      do  i=1,n4
!
         zp=integrateur%corr(2,9)*dels(i,9)
         z=integrateur%corr(1,9)*dels(i,9)
!
         do  j=integrateur%nd,integrateur%nf
            zp=zp+integrateur%beta(8,j)*dels(i,j+1)
            z=z+integrateur%alpha(8,j)*dels(i,j+1)
         end do
!
!        Calcul de DEL et DELP au point 9 pour toutes les variables 
         delp(i,9)=(1.+integrateur%corr(4,9))*cxps(i,9)/integrateur%pas-zp/integrateur%dp
         del(i,9)=(cxs(i,9)+integrateur%corr(3,9)*cxps(i,9))/h2-z/integrateur%d
!
!***********************************************************************
!*fon calcul de del,delp sur tout le segment utile sauf en 9
!***********************************************************************
!
         do  j=10,integrateur%nnf
            delp(i,j)=delp(i,j-1)+dels(i,j-1)
            del(i,j)=del(i,j-1)+delp(i,j)
         end do
!
!        Si le nombre de progressions du COWELL est inférieur à l'ordre,
!        alors on est donc toujours en phase de convergence pour 
!        laquelle il faut recalculer les colonnes inférieures

         if(iter /= integrateur%nn) then
!
            mnp9=9-integrateur%nnd
!
            do  k=1,mnp9
               j=9-k
               delp(i,j)=delp(i,j+1)-dels(i,j)
               del(i,j)=del(i,j+1)-delp(i,j+1)
            end do
!
         endif
!
!***********************************************************************
!*fon calcul de x,xps au pt extreme (17 pour iord =16)
!***********************************************************************
!
         zp=integrateur%corr(2,integrateur%nnf)*dels(i,integrateur%nnf-1)
         z=integrateur%corr(1,integrateur%nnf)*dels(i,integrateur%nnf-1)
!
         do  j=integrateur%nd,integrateur%nf
            zp=zp+integrateur%beta(integrateur%nnf-1,j)*dels(i,j+1)
            z=z+integrateur%alpha(integrateur%nnf-1,j)*dels(i,j+1)
         end do
!
         cxps(i,integrateur%nnf)=integrateur%pas*(delp(i,integrateur%nnf)+zp/integrateur%dp)/ &
              (1.+integrateur%corr(4,integrateur%nnf))
         cxs(i,integrateur%nnf)=h2*(del(i,integrateur%nnf)+z/integrateur%d)-  &
              integrateur%corr(3,integrateur%nnf)*cxps(i,integrateur%nnf)
!
!***********************************************************************
!*fon initialisation de xpp pour le critere de convergence
!***********************************************************************
!
! modif : on stabilise l'avant dernier point du support
!         c'est a dire toujours le point nnf-1
! pour iter a 1 :   cxps au point nnf-1 vaut 0
!------------------------------------------
!
!        Stockage de la valeur des dérivées des paramètres
!        à intégrer, au point supérieur du tableau de COWELL
!        (= 12 pour un COWELL d'ordre 8)
!        Ces valeurs serviront lors de l'évaluation de la convergence

         xpp(i)=cxps(i,integrateur%nnf-1)
!---------
!
      end do
!
!***********************************************************************
!*fon calcul de dels (extreme) - seul dels inconnu
!***********************************************************************
!
      ss=s + real(integrateur%nn,kind=pm_reel)*integrateur%pas
!
      call mui_cowell_cowsec(fsub_cowell,integrateur,cxs(1:integrateur%n,integrateur%nnf), &
           cxps(1:integrateur%n,integrateur%nnf),ss, &
           n1,n2,n4,integrateur%nnf,pm_i_nun,dels(1:integrateur%n,integrateur%nnf), &
           retour_fsub, pb_fsub, retour,korrec=0)
      if (retour < pm_OK) go to 6000
    
      
!***********************************************************************
!*fon correction de x,xps aux pts inconnus y compris au pt extreme
!***********************************************************************
!
!     Bouclage sur tous les paramètres
      do  i=1,n4
!
!        Calcul des tableaux X et XPS sur la portion supérieure,
!        ATTENTION, avec des formules centrées sur 
!        T0 + PAS (=> BETA(J-2,..) et ALPHA(J-2,..))

         do  j=10,integrateur%nnf
!
            zp=integrateur%corr(2,j-1)*dels(i,j)
            z=integrateur%corr(1,j-1)*dels(i,j)
!
            do  k=integrateur%nd,integrateur%nf
               zp=zp+integrateur%beta(j-2,k)*dels(i,k+2)
               z=z+integrateur%alpha(j-2,k)*dels(i,k+2)
            end do
!
            cxps(i,j)=integrateur%pas*(delp(i,j)+zp/integrateur%dp)/(1.+integrateur%corr(4,j-1))
            cxs(i,j)=h2*(del(i,j)+z/integrateur%d)-integrateur%corr(3,j-1)*cxps(i,j)
!
         end do
!
!        Calcul des tableaux X et XPS sur la portion inférieure,
!        ATTENTION, avec des formules centrées sur 
!        T0 (=> BETA(J-1,..) et ALPHA(J-1,..))

         if(iter.ne.integrateur%nn) then
!
!           Le calcul n'est effectué que sur la portion qui n'a jamais
!           été un point central lors des précédentes itérations

            iif=9-iter
!
            do  j=integrateur%nnd,iif
!
               zp=integrateur%corr(2,j)*dels(i,j)
               z=integrateur%corr(1,j)*dels(i,j)
!
               do  k=integrateur%nd,integrateur%nf
                  zp=zp+integrateur%beta(j-1,k)*dels(i,k+1)
                  z=z+integrateur%alpha(j-1,k)*dels(i,k+1)
               end do
!
               cxps(i,j)=integrateur%pas*(delp(i,j)+zp/integrateur%dp)/(1.+integrateur%corr(4,j))
               cxs(i,j)=h2*(del(i,j)+z/integrateur%d)-integrateur%corr(3,j)*cxps(i,j)
!
            end do
!
         endif
!
      end do
!
!***********************************************************************
!*fon re-calcul de dels aux pts de correction de x xps
!***********************************************************************
!
!    En phase de démarrage, on recalcule toute la portion supérieure
!    avec un second membre complet
      if(iter /= integrateur%nn) then
!
         do  j=10,integrateur%nnf
            jm9=j-9
            ss=s+integrateur%pas*jm9
            call mui_cowell_cowsec(fsub_cowell,integrateur,cxs(1:integrateur%n,j),  &
                 cxps(1:integrateur%n,j),ss, &
                 n1,n2,n4,j,pm_i_nun,dels(1:integrateur%n,j),retour_fsub, pb_fsub, retour,&
                 korrec = 0)
            if (retour < pm_OK) go to 6000

!
            
         end do
        
!       En phase de démarrage, on ne recalcule que la portion inférieure
!       qui n'a jamais été un point central, mais avec un second membre
!       complet

         iif=9-iter
!
         do  j=integrateur%nnd,iif
            jm9=j-9
            ss=s+integrateur%pas*jm9
            call mui_cowell_cowsec(fsub_cowell,integrateur,cxs(1:integrateur%n,j),  &
                 cxps(1:integrateur%n,j),ss, &
                 n1,n2,n4,j,pm_i_nun,dels(1:integrateur%n,j),retour_fsub, pb_fsub, retour,&
                 korrec = 0)
            if (retour < pm_OK) go to 6000
            
            
         end do

!        En phase d'intégration convergée, on ne recalcule que la
!        portion supérieure, avec un modèle de force complet ou 
!        simplifié suivant la clé 
      else
!
         do  j=10,integrateur%nnf
            jm9=j-9
            ss=s+integrateur%pas*jm9
            call mui_cowell_cowsec(fsub_cowell,integrateur,cxs(1:integrateur%n,j),  &
                 cxps(1:integrateur%n,j),ss, &
                 n1,n2,n4,j,pm_i_nun,dels(1:integrateur%n,j),retour_fsub, pb_fsub, retour, &
                 korrec = 1)
            if (retour < pm_OK) go to 6000
            

         end do
!
         
      endif
!
!********************
! DEBUT mode iteratif
!********************
      if (iteratif) then
!***********************************************************************
!*fon test de convergence
!***********************************************************************
!
!        Pour chaque paramètre à intégrer, on forme les différences 
!        entre XPP et le max (dérivées au point maximal du tableau de
!        COWELL et la valeur sauvegardée DELP(...,ND))

         do  i=1,n1

!!!!!!!!!!!!!
            if (poursuivreIter)then 
!
! modif : on stabilise l'avant dernier point du support
!         c'est a dire toujours le point nnf-1
!------------------------------------------
!
               dxp=cxps(i,integrateur%nnf-1)-xpp(i)
!
               if(abs(cxps(i,integrateur%nnf-1))  > abs(xpm(i))) then
                  xpm(i)=cxps(i,integrateur%nnf-1)
               endif
!
! a ce niveau, xpm est toujours, en valeur abolue, le max des cxps, au 
! point nnf-1, successifs
!
               ! Test de la convergence des variables à intégrer
               if(abs(dxp) > (abs(xpm(i))*eps)) then
!***********************************************************************
!*fon   cas de non convergence du paramètre i
!***********************************************************************

!              Il faut donc une boucle de calcul supplémentaire

                  iboucl=iboucl+1
                  if(integrateur%nn == iter) then

!                 En phase d'intégration on fixe à 30 le nombre
!                 maximal de boucles que l'on peut effectuer,
!                 Si IBOUCL < 30, alors on repart dans les calculs
                     if(iboucl<=30) then
!!!!!!!!           goto 200
                        poursuivreIter=.false.
                        poursuivre200=.true.
                     else
                        retour = pm_err_conv_cowell
                        goto 6000
                     endif
                  else
!              Test sur le nombre de boucle déjà effectuées :
!
!              En phase de démarrage on fixe à 50 le nombre maximal 
!              de boucles que l'on peut effectuer
!              Si IBOUCL < 50, alors on repart dans les calculs
                     if(iboucl<=50) then
!!!!!!!!!!!                  goto 200
                        poursuivreIter=.false.
                        poursuivre200=.true.
                     else
                        retour = pm_err_conv_cowell
                        goto 6000
                     endif
                  endif
               else
                  poursuivreIter=.true.
               endif
!
!!!!!!!!!!
            endif
         end do
!
! test de l equation du temps : partie keplerienne + non keplerienne
!
!!!!!!!!!!
         if (poursuivreIter) then
            dxp = cxps(n4,integrateur%nnf-1)-xpp(n4)
!
            if(abs(cxps(n4,integrateur%nnf-1)) > abs(xpm(n4))) then
               xpm(n4)=cxps(n4,integrateur%nnf-1)
            endif
!
! a ce niveau, xpm est toujours, en valeur abolue, le max des cxps, au
! point nnf-1, successifs
!
! les 2 instructions qui suivent ne sont utiles que ds le cas ou
! ireg = 3, ds le cas contraire tk prendra tjs la valeur 0
            ss=s+(integrateur%nn-1)*integrateur%pas
            call mui_cowell_ckeptp(integrateur,ss,integrateur%ireg,tk,retour)
            if (retour /= pm_OK) go to 6000

            ! Test de la convergence de la variable d'intégration
            if(abs(dxp) > (abs(xpm(n4)+tk)*eps_var_integ)) then
!***********************************************************************
!*fon   cas de non convergence
!***********************************************************************
               iboucl=iboucl+1
               if(integrateur%nn == iter) then
                  if(iboucl<=30) then
                     poursuivre200=.true.
!               goto 200
                  else
                     retour = pm_err_conv_cowell
                     goto 6000
                  endif
               else
                  if(iboucl<=50) then
                     poursuivre200=.true.
!               goto 200
                  else
                     retour = pm_err_conv_cowell
                     goto 6000
                  endif
               endif
               
            else
               poursuivre200=.false.
            endif

         endif !poursuivreIter

!*******************
!  FIN mode iteratif
!*******************
      endif

   enddo !while poursuivre200

!-----
!
!***********************************************************************
!*fon   cas de la convergence
!***********************************************************************
!
! modif : on stabilise l'avant dernier point du support
!         c'est a dire toujours le point nnf-1
!------------------------------------------
!      do 18 i=1,n4
!         xpp(i)=cxps(i,nnf-1)
!  18 continue
! on ne voit pas l'utilite de cette boucle 
!-----------------------------------------
!
!     Incrémentation de la variable d'intégration d'un pas
   s=s+integrateur%pas

!    Progression du tableau SPYS dans la routine cowsec
   call mui_cowell_cowsec(fsub_cowell,integrateur,cxs(1:integrateur%n,integrateur%nd),  &
        cxps(1:integrateur%n,integrateur%nd),s, &
        n1,n2,n4,pm_i_nun,pm_i_nze,dels(1:integrateur%n,integrateur%nd),retour_fsub, pb_fsub, retour,&
        korrec = 0)
   if (retour < pm_OK) go to 6000
  
!     Progression des tableaux d'un cran vers la droite

   do  i=integrateur%nd,integrateur%nfp
      !
      do  n=1,n4
         cxps(n,i)=cxps(n,i+1)
         delp(n,i)=delp(n,i+1)
         cxs(n,i)=cxs(n,i+1)
         del(n,i)=del(n,i+1)
         dels(n,i)=dels(n,i+1)
      end do
!
   end do
!

6000 continue

 end subroutine  mui_cowell_cowitg

