module cps_math_tchcal_mod
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Resume
!  module de la routine cps_math_tchcal (ex am_math_tchcal de l'AMLIB)
!
!$Version
!  $Id: cps_math_tchcal.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: cps_math_tchcal.F90,v $
!  Revision 1.5  2010/10/21 13:46:20  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.4  2008/10/14 07:55:23  cml
!  DM-ID 1058 : Suppression d une variable inutilisee
!
!  Revision 1.3  2007/11/15 13:02:44  sbd
!  FA-ID 807 suppression include AM_trigo.h et remplacement PIS2 par pm_pi_sur2
!
!  Revision 1.2  2006/05/30 08:24:37  vivaresf
!  DM-ID 387 : prefixe COMPAS
!
!
!$FinHistorique
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Routine en provenance de l'AMLIB
  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: cps_math_tchcal.F90 69 2012-09-11 08:33:34Z ffsm $'


contains

      subroutine cps_math_tchcal (tdeb,tfin,rtab,ndeg,nparam, &
        fct,ncmax,ctch,ier)
!*******************************************************************************
!$<AM-V1.0>
!
!$Nom
!	SUBR cps_math_tchcal
!
!$Resume		
!	Calcul des coefficients de Tchebycheff  
!
!$Description
!	Calcul des coefficients de Tchebycheff approximant une
!       fonction de plusieurs paramètres à 1 variable
!       (Le nombre de paramètres est arbitrairement limité à 20) 
!	(La méthode de calcul est décrite dans la note CNES CT/DTI/MS/AE/262 du \
!       29/08/85-J.Bernard-O.Zarrouati)
!	
!$Version
! 
!$Auteur
! 	Equipe interplanétaire
!
!$Historique
!       7/10/1999 Ph BREMARD - Correction de la valeur ntrav (=45)
!       pour avoir jusqu'a 15 planetes.
!	Janvier 1993  J.Bernard (ucotch de la LIBIP)
!	15/03/95   modification normes AMLIB 
!
!$FinHistorique
!
!$Mots-cles
!	mathematique approximation AMLIB
!
!$Usage			
!	subroutine cps_math_tchcal(tdeb,tfin,rtab,ndeg,nparam,fct,ncmax,ctch,ier)
!	double precision tdeb,tfin,rtab(*),ctch(*)
!       integer ndeg,nparam,ncmax,ier
!       external fct
!
!$Arguments		
!>E	tdeb	: Début de l'intervalle 
!>E	tfin	: Fin   de l'intervalle
!>E	rtab	: Tableau de paramètres spécifiques au calcul des paramètres\
!                 utilisés dans la fonction fct
!>E	ndeg	: degré des polynômes d'approximation
!>E	nparam	: Nombre de paramètres ( nparam <= 20 ) à approximer
!>E	fct	: Subroutine de calcul des paramètres à approximer 
!>E	ncmax	: Dimension maximum du tableau ctch ( ncmax >= (ndeg+1)*nparam )
!>S	ctch	: Coefficients de Tchebycheff
!                 ctch(1) à ctch(ndeg+1) = coefficient pour parametre 1
!                 ctch(ndeg+2) à ctch(2*ndeg+2) = coefficient pour parametre 2
!                 ....
!                 ctch(ndeg+nparam) à ctch(nparam*ndeg+nparam) = \
!                 coefficient pour paramètre nparam
!>S     ier       : Code d'erreur
!	                 = 0:Pas d'erreur 
!                        < 0:PB 
!                        =-1:Mauvais dimensionnement du tableau ctch
!                        =-2:Erreur nombre de paramètres ( nparam <= 20 )
!                        =-3:Erreur appel fonction fct
!
!$Erreurs
!*
!$Include
!	
!
!$Global
!
!$Donnees
!
!$Acces			
!	PUBLIQUE
!
!$Ref-Externes
!	AM_err_setmsg (Librarie  AMIGAU)			
!
!$Fichiers	
!	
!$Hypotheses
!
!$Méthode
!       subroutine fct :
!       declarée en external dans le programme appelant
!       exemple:
!       subroutine fct (t,rtab,nparam,f,ier)
!       implicit double precision
!       dimension f(nparam),rtab(*)
!       ier = 0
!
!       éventuelle recupération de coefficients spécifiques
!       à la fonction fct
!
!       p1 = rtab(1)
!       p2 = rtab(2)
!       ...
!       pn = rtab(n)
!       calcul des nparam f (avec  t et p1,p2....pn))
!       return
!       end
!
!$Exemples
!
!$Documentation	
!
!$Bibliographie
!
!$Remarques
!	Attention à déclarer fct en external
!
!$Voir-Aussi
!
!$<>
!*******************************************************************************

        use mslib
        use msp_gestion_erreur

        implicit none
      
!-------------------------------------------------------------------------------
!	INCLUDE
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!	DECLARATION DES ARGUMENTS
!-------------------------------------------------------------------------------

      real(kind=pm_reel) :: tdeb,tfin,rtab(*),ctch(*)

      integer ::  ndeg,nparam,ncmax,ier
!
      external  fct
!
!-------------------------------------------------------------------------------
!	DECLARATION DES VARIABLES LOCALES
!-------------------------------------------------------------------------------
! 
!
      integer, parameter :: ntrav = 45
      real(kind=pm_reel) :: x,fx(ntrav)
      real(kind=pm_reel) :: amuli,amulj,dels2,tamp
      real(kind=pm_reel) :: cik,sik,c1k,s1k
      integer :: ik
      integer :: i,ind,ku
      integer :: ntot,nn
      integer :: ierr
      character :: msg*(80)                                            

!
!-------------------------------------------------------------------------------
!	CORPS DU MODULE math_tchcal
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!     Controle des paramètres d'entrée
!-------------------------------------------------------------------------------

       ier = 0

        nn = ndeg + 1
        ntot = nparam*nn
        if(ncmax.lt.ntot)then
	   ier=-1
	   call MSP_signaler_message(cle_mes='ERREUR_PARAM', &
          routine = 'math_tchcal',partie_variable ='dimensionnement vecteur ctch')
	   go to 9999
        endif

       if(nparam.gt.ntrav)then
	       ier =-2
               call MSP_signaler_message(cle_mes='ERREUR_PARAM', &
              routine='math_tchcal',partie_variable='nombre de parametres nparam > 20')
                go to 9999
        endif

!-------------------------------------------------------------------------------
!      Initialisation des coefficients de Tchebycheff
!-------------------------------------------------------------------------------                                     
       do i =1,ntot
          ctch(i) = 0.d0
       end do

       c1k = cos(pm_pi_sur2/nn)
       s1k = sin(pm_pi_sur2/nn)
       amuli = (c1k+s1k)*(c1k-s1k)
       amulj = 2.d0*c1k*s1k
       dels2 = (tfin - tdeb)*0.5d0

!-------------------------------------------------------------------------------
!      Calcul des coefficients de Tchebycheff
!-------------------------------------------------------------------------------

	do ik =1,nn
	   x = tdeb +(c1k+1.d0)*dels2
	   call fct(x,rtab,nparam,fx,ierr)
	   if(ierr.ne.0)then
	      ier=-3
	      write(msg,*)'erreur dans l appel a fct(',ierr,')'
	      call MSP_signaler_message(cle_mes='CREA_COEF_TCHE', routine = 'math_tchcal', &
                   partie_variable='erreur dans le calcul des coefficients de tchebycheff')
	      go to 9999
	   endif

	   ku = 1

	   do ind=1,ntot,nn
	     ctch(ind) = ctch(ind) + fx(ku)
	     ku = ku + 1
	   end do

	   cik = c1k
           sik = s1k

	   do i=2,nn
	      ku = 1

	      do ind = i,ntot,nn
	         ctch(ind) = ctch(ind) + fx(ku)*cik
	         ku = ku + 1
	      end do

              tamp = cik*c1k - sik*s1k
	      sik = sik*c1k + cik*s1k
	      cik = tamp
	   end do

	   tamp = c1k*amuli - s1k*amulj
	   s1k = c1k*amulj + s1k*amuli
           c1k = tamp

	end do

!-------------------------------------------------------------------------------
!      Réduction
!-------------------------------------------------------------------------------

	do i =1,ntot
	   ctch(i) = 2.d0/nn*ctch(i)
	enddo

	do  i=1,ntot,nn
	   ctch(i) = ctch(i)*0.5d0
        enddo

9999  continue

!-------------------------------------------------------------------------------
!	FIN DU MODULE math_tchcal
!-------------------------------------------------------------------------------

      end subroutine cps_math_tchcal
      
    end module cps_math_tchcal_mod
