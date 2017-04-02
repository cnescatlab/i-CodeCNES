subroutine mui_cowell_calcoe(integrateur,ideb,ifin,ic,lcf,kini0,kini,koeff,retour)

! (C) Copyright CNES - MSPRO - 2005

!************************************************************************
!
! But:  
! ===     
!*rol calcul des elements du tableau koeff utilise par
!*rol mui_cowell_calcab pour le calcul des alpha et beta
!
! Note d'utilisation: 
! ================== 
!              parametres d'entree
!
!*par ideb   : adresse extreme inferieure des alpha et beta
!*par ifin   : adresse extreme superieure des alpha et beta
!*par ic     : point central ( = 8 )
!*par lcf    : dimension de kini ( = iord-1 )
!*par kini0  :
!
!              parametres de sortie
!
!*par kini   : vecteur auxiliaire
!*par koeff  : tableau resultat
!*par retour : code retour
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
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 5.10: DM-ID 1058 : Correction des warnings levés par g95
!                   (Date: 8/2008 - Realisation: Atos origin)
!
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************ 
!
! Modules
! =======
use mslib
use type_mspro
use parametre_interne_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   type(tm_integrateur),                  intent(inout)                  ::  integrateur  ! integrateur utilise
   integer , intent(in) :: ideb  ! adresse extreme inferieure des alpha et beta
   integer , intent(in) :: ifin  ! adresse extreme superieure des alpha et beta
   integer , intent(in) :: ic    ! point central ( = 8 )
   integer , intent(in) :: lcf   ! dimension de kini ( = iord-1 )
   integer , intent(in) :: kini0 ! 
   integer, dimension(pm_i_dim15) , intent(inout) :: kini ! vecteur auxiliaire
   integer, dimension(pm_i_dim17,pm_i_dim15) , intent(out) :: koeff ! tableau resultat
   integer , intent(out) :: retour ! code retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
   
   integer :: i,i1
   integer :: idec,ifi1,is
   integer :: l1,lc

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mui_cowell_calcoe.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

!***********************************************************************
!*fon initialisation du vecteur auxiliaire kini
!***********************************************************************
!
   do  lc=2,lcf
      kini(lc-1)=kini(lc)
   end do
!
   kini(lcf)=kini0
!
   do  l1=2,lcf
      lc=lcf+1-l1
      kini(lc)=kini(lc)-kini(lc+1)
   end do
!
!***********************************************************************
!*fon 1ere colonne du tableau koeff
!***********************************************************************
!
   ifi1=ifin+1
!
   do  i=ideb,ifi1
      koeff(i,1)=kini(1)
   end do
!
!***********************************************************************
!*fon autres colonnes du tableau koeff
!***********************************************************************
!
   do  lc = 2,lcf
!
      idec=mod(lc,2)
      koeff(ic-(lc-1)/2,lc)=kini(lc)
!
      if( (ic-(lc-1)/2) > ideb) then
!
         i=ic-(lc-1)/2
         
         i1=i-1
         is=i1+idec
         koeff(i1,lc)=koeff(i,lc)-koeff(is,lc-1)
         i=i1
         !
         do while (i>ideb)
            i1=i-1
            is=i1+idec
            koeff(i1,lc)=koeff(i,lc)-koeff(is,lc-1)
            i=i1
         enddo
      endif
!
      if( (ic-(lc-1)/2) < ifi1) then
!
         i=ic-(lc-1)/2
         
         i1=i+1
         is=i +idec
         koeff(i1,lc)=koeff(i,lc)+koeff(is,lc-1)
         i=i1
!
         do while (i < ifi1)
            i1=i+1
            is=i +idec
            koeff(i1,lc)=koeff(i,lc)+koeff(is,lc-1)
            i=i1
         enddo
      endif
   enddo
      
 end subroutine  mui_cowell_calcoe
