      subroutine osci_recherche_deb_plan_grp (id_deb, code_plan, &
     & new_id_deb, new_tc_deb, duedate_tc, id_deb_grp, ier)

!***********************************************************************
!$<DOC>
!$Nom
!	SUBR osci_recherche_deb_plan_grp
!$Auteur
!	V. MORTIAUX (THALES-IS)
!$Resume
!   Recuperation du premier id pour un plan donne passe en parametre dans le groupe
!       auquel appartient l'id_deb passe en parametre
!$Description
!	Cette routine est surchargee au niveau ELI pour pouvoire prendre en compte les
!     due dates comme reference de positionnement (a la place de la date interne).
!   On recupere ici la duedate minimale et la premiere id du groupe (independemment 
!     du plan considere) auquel appartient la TC d'ID id_deb, ainsi que la premiere
!     ID du plan code_plan dont la date interne est superieure a cette due date.
!$Mots-cles
!
!$Acces
!       PUBLIQUE
!$Usage
!>	subroutine osci_recherche_deb_plan_grp (id_deb, code_plan, 
!>	     & new_id_deb, new_tc_deb, duedate_tc, id_deb_grp, ier)           
!>		integer id_deb                                            
!>		character code_plan                                       
!>		integer new_id_deb                                        
!>		character new_tc_deb                                      
!>		double precision duedate_tc                              
!>		integer id_deb_grp                                        
!>		integer ier                                               
!$Arguments
!>E	id_deb     	    : <-> Id de la TC 
!>E	code_plan  	    : <-> Plan auquel appartient la TC
!>S	new_id_deb 	    : <-> Id de la premiere TC du plan appartenant au groupe considere
!>S	new_tc_deb 	    : <-> Nom de la premiere TC du plan appartenant au groupe considere
!>S	duedate_tc    	: <-> Due date minimale du groupe consid?r? (du plan code_plan)
!>S	id_deb_grp 	    : <-> Plus petit id du groupe (tout plan confondu)
!>S	ier        	    : <-> Code retour 
!$Retour
!
!$Erreurs
!>	ier	:   0	Pas d'erreur
!>	ier	:  -1	L'id # n'appartient a aucun groupe de la cinematique generique
!>	ier	:  -6	Erreur a l'appel de osci_recherche_tcs_duedate
!$Include
!>	acces_F.h                   
!>	ST_osci_cine_scao.h         
!>	osci.fct                    
!>	osme_gest_err.fct           
!>	obmc_zlc00m.fin             
!>	ostc_scao.fct               
!>	ST_osci_cine_generique.h    
!>	o_dates.fct                 
!>	obmc_const.fct              
!>      octc_constantes_elisa.fct
!>	wst_osci_cine_generique.fin
!>	WIoctc_bdeli.fin
!$Global 
!>L	EPSDP100               
!>L	WST_osci_cine_generique
!>L	WIoctc_bdeli_delai_chevauch
!$Ref-Externes
!>	osme_signaler_erreur 
!>	osme_propager_erreur 
!>	osci_recherche_tcs_duedate 
!$Fichiers
!
!$Hypotheses
!
!$Methode
!
!$Exemples
!
!$Documentation
!
!$Bibliographie
!
!$Voir-aussi
!
!$Remarques
!
!$Historique
! HISTORIQUE
! VERSION : ELI_2.0 : DE : 7451 : 19/03/2008 : Mise a niveau du cartouche
! VERSION : ELI_2.0 : : : 02/04/2008 : Developpement ELISA (Atos-CS)
!              - Prise en compte de la due date comme reference de 
!                positionnement de la TC d'annulation
! VERSION : ELI_2.0 : DE : 7257 : 15/07/2008 : Suppression include ocev_evenem.fct
! VERSION : ELI_3.0 : FA : 8053 : 05/12/2008 : Recherche de l'id de la premiere TC du groupe (retour a l'existant MYRIADE)
! VERSION : ELI_3.0 : DE : 8019 : 25/03/2009 : Performance du simulateur
! VERSION : ELI_4.0 : FA : 8463 : 03/12/2009 : Modification cartouche
!           remplacement Fortran77 par Fortran90
! VERSION : ELI_5.2 : FA : 9349 : 14/05/2012 : Sortie en erreur sur chevauchement de TC CANCEL (Y. Ducassou - CS)
!           - Suppression de la variable date_rech
!           - Passage en argument de sortie de la variable interne new_tc_deb
! VERSION : ELI_6.0.0 : DE : 9356 : 27/06/2013 : Evolution du repositionnement de la TC CANCEL en cas de chevauchement
!           - Repositionnement de la date de debut d'annulation pour la recherche de due dates
! FIN-HISTORIQUE
! 
!$Copyreg
! Ce fichier contient des informations confidentielles propriete du 
! CNES. Conformement aux dispositions de l'accord ecrit entre le CNES
! et la societe utilisatrice, ce fichier et son contenu : 
! 
! a) doivent etre proteges et gardes strictement confidentiels par la
!    societe utilisatrice et traites avec le meme degre de precaution
!    et de protection que le CNES ;
! b) ne soient divulgues de maniere interne qu'aux seuls personnels de
!    la societe ayant a en connaitre, dument informes du caractere 
!    strictement confidentiel de ces informations, et ne soient 
!    utilises par ces derniers que dans les conditions definies par 
!    l'accord ;
! c) ne soient pas utilises, totalement ou partiellement, dans un autre 
!    but que celui defini dans l'accord, sans le consentement prealable 
!    et ecrit du CNES;
! d) ne soient divulgues, ni susceptibles d'etre divulgues, soit 
!    directement, soit indirectement, a tous tiers, y compris toute
!    filiale ou participation, ou a toutes personnes autres que celles
!    mentionnees a l'alinea b) ci-dessus;
! e) ne soient ni copies, ni reproduits, ni dupliques totalement ou 
!    partiellement sans l'autorisation prealable et ecrite du CNES 
!$<FDOC>
!***********************************************************************

      implicit none

!------------------------------
!	CONSTANTES
!------------------------------
#include "acces_F.h"
#include "ST_osci_cine_scao.h"
#include "osci.fct"
#include "osme_gest_err.fct"
#include "obmc_zlc00m.fin"
#include "ostc_scao.fct"
#include "ST_osci_cine_generique.h"
#include "o_dates.fct"
#include "octc_param.fct"
#include "o_vecteur.fct"
#include "o_vecteur_myr.fct"
#include "o_satellite.fct"
#include "o_satellite_myr.fct"
#include "o_manoeuvres.fct"
#include "o_station.fct"
! pour la precision de comparaison de dates en jj
#include "obmc_const.fct"
#include "octc_constantes_elisa.fct"

!------------------------------
!	PARAMETRES
!------------------------------
      integer id_deb
      character*(*) code_plan
      integer new_id_deb
      character*(10) new_tc_deb
      double precision duedate_tc
      integer id_deb_grp
      integer ier

!-------------------------------
!	SOUS-PROGRAMMES UTILISES
!-------------------------------
! Declaration manuelle des external

! Fin de la declaration manuelle
!-------------------------------
! Declaration automatique des external
!
      external osme_signaler_erreur, osme_propager_erreur
      external osci_recherche_tcs_duedate
! Fin de la declaration automatique

!-------------------------------

!-------------------------------
!	COMMONS
!-------------------------------
#include "wst_osci_cine_generique.fin"
#include "WIoctc_bdeli.fin"

!-------------------------------
!	VARIABLES LOCALES
!-------------------------------
      integer ier_app
      character*(PLGVARMESS) cvarerreur
!
      integer i
      integer ityp
      integer id_tc
      logical flag_groupe_trouve
      logical flag_end
      double precision duedate_min_grp
      
      integer inb_sauv
      
      character*(10) c_code_grp
      character*(10) c_grp_s
      character*(10) c_type_tc
      character*(10) code_plan_tc
      character*(10) c_plan_s

!
!-------------------------------
!	INITIALISATION
!-------------------------------
      ier = 0
      ier_app = 0
      cvarerreur = ' '

      id_deb_grp = 0
      id_tc = 0
      flag_groupe_trouve = .false.
      flag_end = .false.
      duedate_tc = 0.d0
      new_id_deb = 0
      duedate_min_grp = 0.d0
      c_code_grp = ' '
      c_grp_s = ' '
      c_type_tc = ' '
      new_tc_deb = ' '
      code_plan_tc = ' '
      c_plan_s = ' '

!------------------------
!------------------------
! CORPS DU SOUS-PROGRAMME 
!------------------------
!------------------------
!
!-----------------------------------------------------------------------------
!     Recuperation des premier et dernier id du groupe auquel appartient la TC
!-----------------------------------------------------------------------------
!
!     Sauvegarde du nombre de groupes
!
      inb_sauv = wst_osci_cine_generique%ST_liste_groupes_gene% &
     &          inb_ST_groupes_gene

      i = 0
      do while (( .not. flag_end ) .and. ( .not. flag_groupe_trouve))
          i = i + 1
          if (((wst_osci_cine_generique%ST_liste_groupes_gene% &
     &         ST_groupes_gene(i)%id_first_tc-id_deb)  <=  EPSDP100) &
     &       .AND.((id_deb-wst_osci_cine_generique% &
     &        ST_liste_groupes_gene%ST_groupes_gene(i)%id_last_tc) &
     &         <=  EPSDP100)) then 

            id_deb_grp = wst_osci_cine_generique%ST_liste_groupes_gene% &
     &                    ST_groupes_gene(i)%id_first_tc
            duedate_min_grp = wst_osci_cine_generique% &
     &                    ST_liste_groupes_gene%ST_groupes_gene(i)% &
     &                    due_date_first_tc - WIoctc_bdeli_delai_chevauch / PDNB_SEC_PAR_JOUR
             flag_groupe_trouve = .TRUE.
        
          endif  
          if (i == inb_sauv) then
            flag_end = .true.
          endif               
      enddo

      if(.NOT. flag_groupe_trouve) then
!#ID        osci_recherche_deb_plan_grp_mess1
!#LIB       'L'id # n'appartient a aucun groupe de la cinematique generique'
!#DES       L'id # n'appartient a aucun groupe de la cinematique generique
            write(cvarerreur,1000)id_deb
!
            call osme_signaler_erreur( &
     &         'osci_recherche_deb_plan_grp_mess1', &
     &          cvarerreur, ier_app)
            if (ier_app == PIERR_FATALE) then
               ier = -1
               goto 999
            else
!              (ier_app = PIERR_GRAVE ou PIERR_OK)
               continue
            endif      
      endif

!-----------------------------------------------------------------------------------
!     Recuperation de l'id de la premiere TC du plan appartenant au groupe determine
!-----------------------------------------------------------------------------------
      
      ityp = 1
      call osci_recherche_tcs_duedate( &
     &              duedate_min_grp, ityp, code_plan, &
     &              id_tc, c_type_tc, c_code_grp, &
     &              code_plan_tc, duedate_tc, &
     &              new_id_deb, new_tc_deb, c_grp_s, c_plan_s, ier)
      if (ier_app < 0) then
!     Cas d'erreur grave ou fatale : il faut propager
         call osme_propager_erreur('osci_recherche_deb_plan_grp', &
     &        'osci_recherche_tcs', ier_app)
         ier = -6
         goto 999
      endif

!-------------------------------
!	Gestion des erreurs
!-------------------------------
 999  continue

!-------------------------------
!	FORMATS
!-------------------------------
 1000 format(i9,'##')

      end
      
      
      
      
      
!-----------------------------------------------------------------------------------------------------------------



subroutine ostc_lecdon (cficdon,ier)

!***********************************************************************
!$<DOC>
!$Nom
!        SUBR ostc_lecdon
!$Auteur
!        D. CLAUDE (IXI)
!$Resume
!
!$Description
!        Lecture du fichier de donnees (.don) contenant toutes les donnees
!        necessaires et suffisantes a la fonction "Generation TCs SCAO"
!$Mots-cles
!        TCs SCAO , Initialisation commons
!$Acces
!       PRIVE
!*************************************************************************


!------------------------------
!        PARAMETRES
!------------------------------
      character*(*) cficdon
      integer ier
      
      
!-------------------------------
!        VARIABLES LOCALES
!-------------------------------
!     Variables utilisees pour les acces MADONA
      integer iacces, mode
      integer i, j, k, m
      integer i_tc, j_tc, k_tc
      integer imode_gen
      logical tc_inhibee
      type(st_type_groupe)  type_grp
      type(st_groupe)  grp
      type(st_tc_groupe)  tc
      integer ilg

!     Code retour des routines appelees
      integer ier_app
!     Variables pour la gestion des messages
      integer igravite
      character*(PLGVARMESS) cvarerreur
!     Numero du jeu de biais utilise et nombre de jeux de biais disponible
      integer num_jeu, nb_jeux

!     Nombre de tuyeres
      integer nb_propu
      
      
!-------------------------------
!        INITIALISATIONS
!-------------------------------

!     Parametres de sortie
      ier = 0

!     Variables locales
      iacces = 0
      mode = 0
      i = 0
      j = 0
      k = 0
      i_tc = 0
      j_tc = 0
      k_tc = 0
      ilg = 0
      igravite = 0
      cvarerreur = ' '
      tc_inhibee = .FALSE.
      num_jeu =  0
      nb_jeux = 0
      nb_propu = 0
      
!     ============================================
!     Initialisation d'une partie du common
!     WEostc_param_tc_group (Partie generation)
!     ============================================

!     Ligne a ne pas supprimer (necessaire pour l'affectation
!     de la strategie GS faite plus bas) : on met initialement
!     la strategie GS a "GS BLOQUE"
      WEoctc_param_tc_strategie_gs = PNSTRATEGIE_GS_BLOQUES

      call ostc_interfaceGene(imode_gen,ier_app)
      if (ier_app /= 0) then
!        Cas d'erreur grave ou fatale : il faut propager
         call osme_propager_erreur('ostc_lecdon','ostc_interfaceGene', ier_app)
         if (ier_app < 0) then
!           Erreur fatale : on arrete le traitement
            ier = -1
            goto 999
         else
            continue
         endif
         
         where(ier>0.0)
		    ier_app = nb_prou
		 elsewhere
		    ier_app = 0.0
		 end where
		 
		 do while (resid >= 5.0D-10)
		   resid = abs(x(i))
		   write (*,*) ' Continue execution'
		   i = i+1
		 end do
		 
		 while (resid >= 5.0D-10) do
		   resid = abs(x(i))
		   write (*,*) ' Continue execution'
		   i = i+1
		 end do
      endif
      
   end
