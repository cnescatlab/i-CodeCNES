module MSP_INTEGRATOR_DEF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  MSP_INTEGRATOR_DEF
!
!$Resume
!  Structure MECASPA permettant de stocker le paramétrage d'un intégrateur MSPRO
!
!$Description
!  Structure MECASPA permettant de stocker le paramétrage d'un intégrateur MSPRO.
!  Cette structure ne stocke pas le type d'intégrateur, et couvre tous les paramétrages
!  utilisateurs d'un intégrateur MSPRO (pour Cowell, DOP853 et Gill).
!  Cette structure est faite pour être initialisée à travers l'objet GSLIB "GS_MS_INTEGRATOR".
!
!$Auteur
!  Y. TANGUY (ATOS)
!
!$Version
!  $Id: MSP_INTEGRATOR_DEF.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: MSP_INTEGRATOR_DEF.F90,v $
!  Revision 1.9  2010/10/15 11:20:59  ogarat
!  VERSION::FA-ID:1331:15/10/2010:Ajout du FinHistorique
!
!  Revision 1.8  2009/03/17 08:51:48  tanguyy
!  AQ / FA-ID 1214 : maj des cartouches
!
!  Revision 1.7  2009/03/16 17:06:54  tanguyy
!  FA-ID 1214 : constantes pour les valeurs par defaut du Cowell
!  Revision 1.6  2008/09/19 12:31:36  mercadig
!  FA-ID 1103 : Modification des valeurs par defaut dans msp_creer_integrator
!  Revision 1.5  2008/04/11 08:23:19  tanguyy
!  AQ : MAJ des cartouches pour DM-ID 983
!  Revision 1.4  2008/03/19 15:25:14  ttn
!  DM-ID 983 : Second membre simplifie
!  Revision 1.3  2007/11/22 16:53:46  tanguyy
!  Rajout du numero d'unite logique sur lequel afficher
!  Revision 1.2  2007/06/25 06:35:15  tanguyy
!  Utilisation d'un type sequence, pour compatibilité avec les "common" PSIMU
!  Revision 1.1  2007/06/04 09:46:44  tanguyy
!  DM-ID 702 : version initiale du module dans la GSLIB
!
!$FinHistorique
!
!$Usage
!  use MSP_INTEGRATOR_DEF
!
!$Structure
!
!: MSP_INTEGRATOR : 
!>     sequence             : <>                 
!>     type_integrateur     : <integer>          3 valeurs possibles : pm_cowell, pm_dop853, pm_gill                     
!>     ordre                : <integer>          Ordre de l'intégrateur (Cowell)                                         
!>     pas                  : <pm_reel>          Pas d'intégration (Cowell et Gill, = pas_min pour le DOP)               
!>     pas_max              : <pm_reel>          Pas d'intégration maximum (DOP uniquement)                              
!>     circularisation      : <integer>          Circularisation (Cowell uniquement)                                     
!>     second_membre_simp   : <integer>          Utilisation du second membre simplifié (Cowell)                         
!>     convergence          : <integer>          Utilisation du critère de convergence (Cowell uniquement)               
!>     epsilon_rel          : <pm_reel>          Tolérance relative (DOP)                                                
!>     epsilon_abs          : <pm_reel>          Tolérance absolue (DOP)                                                 
!>     epsilon_init         : <pm_reel>          Seuil de convergence initial (Cowell)                                   
!>     epsilon_prog         : <pm_reel>          Seuil de convergence de progression (Cowell)                            
!>     indic_lib            : <integer,DIM=(6)>  Indicateurs (MPS_ENUM_OUI/NON) de libération indépendante des paramètres
!
!$Global
!
!>  MSP_ENUM_OUI                    : <integer,parameter>  
!>  MSP_ENUM_NON                    : <integer,parameter>  
!>  MSP_circularisation_defaut      : <integer,parameter>  Valeur par défaut pour la circularisation (activée)
!>  MSP_second_membre_simp_defaut   : <integer,parameter>  Valeur par défaut pour le 2nd membre simplifié (activé)
!>  MSP_convergence_defaut          : <integer,parameter>  Valeur par défaut pour le critère de convergence (désactivé)
!>  MSP_ordre_cowell_defaut         : <integer,parameter>  Ordre par défaut du Cowell
!>  MSP_epsilon_init_defaut         : <pm_reel,parameter>  Valeur par défaut pour l'epsilon d'initialisation (1e-13)
!>  MSP_epsilon_prog_defaut         : <pm_reel,parameter>  Valeur par défaut pour l'epsilon de progression (1e-9)
!$Common
!
!$Routines
!- MSP_consulter_integrator
!- MSP_modifier_integrator
!- MSP_afficher_integrator
!
!$Fonctions
!- MSP_creer_integrator
!
!$Include
!
!$Module
!#V
!- MSPRO
!#
!
!$Interface
!#V
!#
!
!$Remarques
!  /!\ Cette structure n'est pas POUR LE MOMENT dans la MECASPA, en attendant que la dépendance GSLIB/MECASPA
!  soit inversée...
!
!$Mots-cles
!
!$Voir-Aussi
!.  MSP_creer_integrator MSP_consulter_integrator MSP_modifier_integrator MSP_afficher_integrator
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use MSPRO

  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: MSP_INTEGRATOR_DEF.F90 69 2012-09-11 08:33:34Z ffsm $'

  


! DEFINITIONS DE TYPES:
  type MSP_INTEGRATOR
     sequence

     integer :: type_integrateur         ! 3 valeurs possibles : pm_cowell, pm_dop853, pm_gill
     integer :: ordre                    ! Ordre de l'intégrateur (Cowell)
     real(kind=pm_reel) :: pas           ! Pas d'intégration (Cowell et Gill, = pas_min pour le DOP)
     real(kind=pm_reel) :: pas_max       ! Pas d'intégration maximum (DOP uniquement)
     integer :: circularisation          ! Circularisation (Cowell uniquement)
     integer :: second_membre_simp       ! Utilisation du second membre simplifié (Cowell)
     integer :: convergence              ! Utilisation du critère de convergence (Cowell uniquement)
     real(kind=pm_reel) :: epsilon_rel   ! Tolérance relative (DOP)
     real(kind=pm_reel) :: epsilon_abs   ! Tolérance absolue (DOP)
     real(kind=pm_reel) :: epsilon_init  ! Seuil de convergence initial (Cowell)
     real(kind=pm_reel) :: epsilon_prog  ! Seuil de convergence de progression (Cowell)
     integer, dimension(6) :: indic_lib  ! Indicateurs (MPS_ENUM_OUI/NON) de libération indépendante des paramètres

  end type MSP_INTEGRATOR

! Type paramétré (temporaire : quand on introduira ce module dans la MECAPSA, il faudra supprimer ces deux déclarations)
  integer, parameter :: MSP_ENUM_OUI = 1
  integer, parameter :: MSP_ENUM_NON = 0

  ! Valeurs par défaut pour le Cowell
  integer, parameter :: MSP_circularisation_defaut = 1
  integer, parameter :: MSP_second_membre_simp_defaut = 1
  integer, parameter :: MSP_convergence_defaut = 0
  
  integer, parameter :: MSP_ordre_cowell_defaut = 8
  real(kind=pm_reel), parameter :: MSP_epsilon_init_defaut = 1.0e-13_pm_reel  ! Seuil de convergence initial (Cowell)
  real(kind=pm_reel), parameter :: MSP_epsilon_prog_defaut = 1.0e-9_pm_reel  ! Seuil de convergence de progression (Cowell)


contains

  function MSP_creer_integrator(type_integrateur,pas,ordre,pas_max,circularisation,second_membre_simp,&
       convergence,epsilon_rel,epsilon_abs,epsilon_init,epsilon_prog,indic_lib) result(integrateur)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_creer_integrator
!
!$Resume
!  Création d'une structure MSP_integrator
!
!$Description
!  Création d'une structure MSP_integrator
!
!$Auteur
!  Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  integrateur = MSP_creer_integrator(type_integrateur,[pas],[ordre],[pas_max],[circularisation],[second_membre_simp],&
!.           [convergence],[epsilon_rel],[epsilon_abs],[epsilon_init],[epsilon_prog],[indic_lib])
!.    type(MSP_INTEGRATOR) :: integrateur
!.    integer :: type_integrateur 
!.    real(kind=pm_reel) :: pas 
!.    integer :: ordre 
!.    real(kind=pm_reel) :: pas_max 
!.    integer :: circularisation 
!.    integer :: second_membre_simp
!.    integer :: convergence 
!.    real(kind=pm_reel) :: epsilon_rel 
!.    real(kind=pm_reel) :: epsilon_abs 
!.    real(kind=pm_reel) :: epsilon_init 
!.    real(kind=pm_reel) :: epsilon_prog 
!.    integer, dimension(6) :: indic_lib 
!
!$Arguments
!>E     type_integrateur    :<integer>           Type d'intégrateur MSPRO (pm_dop853, pm_gill, pm_cowell)
!>[E]   pas                 :<pm_reel>           Pas d'intégration (Cowell/Gill) ou pas minimum pour le DOP853
!>[E]   ordre               :<integer>           Ordre de l'intégrateur (Cowell seulement)
!>[E]   pas_max             :<pm_reel>           Pas max pour le DOP853
!>[E]   circularisation     :<integer>           Utilisation de la circularisation (Cowell)
!>[E]   second_membre_simp  :<integer>           Utilisation du second membre simplifié (Cowell)
!>[E]   convergence         :<integer>           Utilisation de l'algorithme de convergence (Cowell)
!>[E]   epsilon_rel         :<pm_reel>           Valeur seuil pour la tolérance relative (DOP853)
!>[E]   epsilon_abs         :<pm_reel>           Valeur seuil pour la tolérance absolue (DOP853)
!>[E]   epsilon_init        :<pm_reel>           Valeur d'epsilon initial pour le Cowell
!>[E]   epsilon_prog        :<pm_reel>           Valeur d'epsilon lors de la phase de progression (Cowell)
!>[E]   indic_lib           :<integer,DIM=(6)>   Indicateur de libération des paramètres 
!>                                             pour calculer la matrice de transition (Cowell)               
!>S     integrateur         :<MSP_INTEGRATOR>    Structure MSP_INTEGRATOR instanciée
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


    ! Déclaration des arguments
    !==========================
    ! Argument de sortie de la fonction
    type(MSP_INTEGRATOR)          :: integrateur

    ! Argument d'entrée obligatoire
    integer,           intent(in)            :: type_integrateur        
    ! Arguments optionnels 
    real(kind=pm_reel),optional, intent(in)  :: pas                     
    integer ,optional, intent(in)            :: ordre                   
    real(kind=pm_reel) ,optional, intent(in) :: pas_max                 
    integer ,optional, intent(in)            :: circularisation         
    integer ,optional, intent(in)            :: second_membre_simp
    integer ,optional, intent(in)            :: convergence             
    real(kind=pm_reel) ,optional, intent(in) :: epsilon_rel             
    real(kind=pm_reel) ,optional, intent(in) :: epsilon_abs             
    real(kind=pm_reel) ,optional, intent(in) :: epsilon_init            
    real(kind=pm_reel) ,optional, intent(in) :: epsilon_prog            
    integer, dimension(6) ,optional, intent(in) :: indic_lib            

    
    ! Début du code
    !==============

    ! Initialisations par défaut de toutes les variables optionnelles

    integrateur%pas               = 0._pm_reel
    integrateur%ordre             = MSP_ORDRE_COWELL_DEFAUT
    integrateur%circularisation   = MSP_CIRCULARISATION_DEFAUT
    integrateur%second_membre_simp= MSP_SECOND_MEMBRE_SIMP_DEFAUT
    integrateur%convergence       = MSP_CONVERGENCE_DEFAUT
    integrateur%epsilon_init      = MSP_epsilon_init_defaut
    integrateur%epsilon_prog      = MSP_epsilon_prog_defaut
    integrateur%indic_lib(:)      = -1
    integrateur%pas_max           = 0._pm_reel
    integrateur%epsilon_rel       = 0._pm_reel   
    integrateur%epsilon_abs       = 0._pm_reel   


    ! Type de l'intégrateur : paramètre obligatoire
    integrateur%type_integrateur = type_integrateur

    ! Pas de l'intégrateur -> nécessaire pour les trois intégrateurs
    if(present(pas             )) integrateur%pas               = pas            
    
    ! Paramètres spécifiques au Cowell
    if(present(ordre           )) integrateur%ordre             = ordre          
    if(present(circularisation )) integrateur%circularisation   = circularisation
    if(present(second_membre_simp )) integrateur%second_membre_simp  = second_membre_simp
    if(present(convergence     )) integrateur%convergence       = convergence    
    if(present(epsilon_init    )) integrateur%epsilon_init      = epsilon_init   
    if(present(epsilon_prog    )) integrateur%epsilon_prog      = epsilon_prog   
    if(present(indic_lib       )) integrateur%indic_lib         = indic_lib

    ! Paramètres spécifiques au DOP
    if(present(pas_max         )) integrateur%pas_max           = pas_max        
    if(present(epsilon_rel     )) integrateur%epsilon_rel       = epsilon_rel    
    if(present(epsilon_abs     )) integrateur%epsilon_abs       = epsilon_abs    

    return 
    
  end function MSP_creer_integrator
  
  subroutine MSP_consulter_integrator(integrateur,type_integrateur,ordre,pas,pas_max,circularisation,second_membre_simp,&
       convergence,epsilon_rel,epsilon_abs,epsilon_init,epsilon_prog,indic_lib)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consulter_integrator
!
!$Resume
!  Routine de consultation d'une structure MSP_integrator
!
!$Description
!  Routine de consultation d'une structure MSP_integrator
!
!$Auteur
!  Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_consulter_integrator(integrateur,[type_integrateur],[ordre],[pas],[pas_max],[circularisation],[second_membre_simp],&
!.           [convergence],[epsilon_rel],[epsilon_abs],[epsilon_init],[epsilon_prog],[indic_lib])
!.    type(MSP_INTEGRATOR) :: integrateur
!.    integer :: type_integrateur 
!.    integer :: ordre 
!.    real(kind=pm_reel) :: pas 
!.    real(kind=pm_reel) :: pas_max 
!.    integer :: circularisation 
!.    integer :: second_membre_simp 
!.    integer :: convergence 
!.    real(kind=pm_reel) :: epsilon_rel 
!.    real(kind=pm_reel) :: epsilon_abs 
!.    real(kind=pm_reel) :: epsilon_init 
!.    real(kind=pm_reel) :: epsilon_prog 
!.    integer, dimension(6) :: indic_lib 
!
!$Arguments
!>E     integrateur         :<MSP_INTEGRATOR>    Structure MSP_INTEGRATOR à consulter
!>[S]   type_integrateur    :<integer>           Type d'intégrateur MSPRO (pm_dop853, pm_gill, pm_cowell)      
!>[S]   ordre               :<integer>           Ordre de l'intégrateur (Cowell seulement)                     
!>[S]   pas                 :<pm_reel>           Pas d'intégration (Cowell/Gill) ou pas minimum pour le DOP853 
!>[S]   pas_max             :<pm_reel>           Pas max pour le DOP853                                        
!>[S]   circularisation     :<integer>           Utilisation de la circularisation (Cowell)                    
!>[S]   second_membre_simp  :<integer>           Utilisation du second membre simplifié (Cowell)
!>[S]   convergence         :<integer>           Utilisation de l'algorithme de convergence (Cowell)           
!>[S]   epsilon_rel         :<pm_reel>           Valeur seuil pour la tolérance relative (DOP853)              
!>[S]   epsilon_abs         :<pm_reel>           Valeur seuil pour la tolérance absolue (DOP853)               
!>[S]   epsilon_init        :<pm_reel>           Valeur d'epsilon initial pour le Cowell                       
!>[S]   epsilon_prog        :<pm_reel>           Valeur d'epsilon lors de la phase de progression (Cowell)     
!>[S]   indic_lib           :<integer,DIM=(6)>   Indicateur de libération des paramètres                       
!                                              pour calculer la matrice de transition (Cowell)               
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

    ! Déclaration des arguments
    !==========================
    type(MSP_INTEGRATOR), intent(in)          :: integrateur
    integer ,optional, intent(out)            :: type_integrateur         
    integer ,optional, intent(out)            :: ordre                    
    real(kind=pm_reel) ,optional, intent(out) :: pas                      
    real(kind=pm_reel) ,optional, intent(out) :: pas_max                  
    integer ,optional, intent(out)            :: circularisation          
    integer ,optional, intent(out)            :: second_membre_simp 
    integer ,optional, intent(out)            :: convergence              
    real(kind=pm_reel) ,optional, intent(out) :: epsilon_rel              
    real(kind=pm_reel) ,optional, intent(out) :: epsilon_abs              
    real(kind=pm_reel) ,optional, intent(out) :: epsilon_init             
    real(kind=pm_reel) ,optional, intent(out) :: epsilon_prog             
    integer, dimension(6) ,optional, intent(out) :: indic_lib             

    
    ! Début du code
    !==============

    if(present(type_integrateur)) type_integrateur  = integrateur%type_integrateur 
    if(present(ordre           )) ordre             = integrateur%ordre           
    if(present(pas             )) pas               = integrateur%pas             
    if(present(pas_max         )) pas_max           = integrateur%pas_max         
    if(present(circularisation )) circularisation   = integrateur%circularisation 
    if(present(second_membre_simp )) second_membre_simp   = integrateur%second_membre_simp 
    if(present(convergence     )) convergence       = integrateur%convergence     
    if(present(epsilon_rel     )) epsilon_rel       = integrateur%epsilon_rel     
    if(present(epsilon_abs     )) epsilon_abs       = integrateur%epsilon_abs     
    if(present(epsilon_init    )) epsilon_init      = integrateur%epsilon_init    
    if(present(epsilon_prog    )) epsilon_prog      = integrateur%epsilon_prog    
    if(present(indic_lib       )) indic_lib         = integrateur%indic_lib


  end subroutine MSP_consulter_integrator

  subroutine MSP_modifier_integrator(integrateur,type_integrateur,ordre,pas,pas_max,circularisation,second_membre_simp,&
       convergence,epsilon_rel,epsilon_abs,epsilon_init,epsilon_prog,indic_lib)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_modifier_integrator
!
!$Resume
!  Routine de modification d'une structure MSP_INTEGRATOR
!
!$Description
!  Routine de modification d'une structure MSP_INTEGRATOR
!
!$Auteur
!  Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_modifier_integrator(integrateur,[type_integrateur],[ordre],[pas],[pas_max],[circularisation],[second_membre_simp],&
!.           [convergence],[epsilon_rel],[epsilon_abs],[epsilon_init],[epsilon_prog],[indic_lib])
!.    type(MSP_INTEGRATOR) :: integrateur
!.    integer :: type_integrateur 
!.    integer :: ordre 
!.    real(kind=pm_reel) :: pas 
!.    real(kind=pm_reel) :: pas_max 
!.    integer :: circularisation 
!.    integer :: second_membre_simp
!.    integer :: convergence 
!.    real(kind=pm_reel) :: epsilon_rel 
!.    real(kind=pm_reel) :: epsilon_abs 
!.    real(kind=pm_reel) :: epsilon_init 
!.    real(kind=pm_reel) :: epsilon_prog 
!.    integer, dimension(6) :: indic_lib 
!
!$Arguments
!>E/S   integrateur         :<MSP_INTEGRATOR>    Structure MSP_INTEGRATOR à modifier
!>[E]   type_integrateur    :<integer>           Type d'intégrateur MSPRO (pm_dop853, pm_gill, pm_cowell)      
!>[E]   ordre               :<integer>           Pas d'intégration (Cowell/Gill) ou pas minimum pour le DOP853 
!>[E]   pas                 :<pm_reel>           Ordre de l'intégrateur (Cowell seulement)                     
!>[E]   pas_max             :<pm_reel>           Pas max pour le DOP853                                        
!>[E]   circularisation     :<integer>           Utilisation de la circularisation (Cowell)                    
!>[E]   second_membre_simp  :<integer>           Utilisation du second membre simplifié (Cowell)
!>[E]   convergence         :<integer>           Utilisation de l'algorithme de convergence (Cowell)           
!>[E]   epsilon_rel         :<pm_reel>           Valeur seuil pour la tolérance relative (DOP853)              
!>[E]   epsilon_abs         :<pm_reel>           Valeur seuil pour la tolérance absolue (DOP853)               
!>[E]   epsilon_init        :<pm_reel>           Valeur d'epsilon initial pour le Cowell                       
!>[E]   epsilon_prog        :<pm_reel>           Valeur d'epsilon lors de la phase de progression (Cowell)     
!>[E]   indic_lib           :<integer,DIM=(6)>   Indicateur de libération des paramètres                       
!                                              pour calculer la matrice de transition (Cowell)               
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

    ! Déclaration des arguments
    !==========================
    type(MSP_INTEGRATOR), intent(inout)           :: integrateur
    integer ,optional, intent(in)            :: type_integrateur         
    integer ,optional, intent(in)            :: ordre                    
    real(kind=pm_reel) ,optional, intent(in) :: pas                      
    real(kind=pm_reel) ,optional, intent(in) :: pas_max                  
    integer ,optional, intent(in)            :: circularisation          
    integer ,optional, intent(in)            :: second_membre_simp
    integer ,optional, intent(in)            :: convergence              
    real(kind=pm_reel) ,optional, intent(in) :: epsilon_rel              
    real(kind=pm_reel) ,optional, intent(in) :: epsilon_abs              
    real(kind=pm_reel) ,optional, intent(in) :: epsilon_init             
    real(kind=pm_reel) ,optional, intent(in) :: epsilon_prog             
    integer, dimension(6) ,optional, intent(in) :: indic_lib             


    
    ! Début du code
    !==============

    if(present(type_integrateur)) integrateur%type_integrateur  = type_integrateur
    if(present(ordre           )) integrateur%ordre             = ordre          
    if(present(pas             )) integrateur%pas               = pas            
    if(present(pas_max         )) integrateur%pas_max           = pas_max        
    if(present(circularisation )) integrateur%circularisation   = circularisation
    if(present(second_membre_simp )) integrateur%second_membre_simp   = second_membre_simp
    if(present(convergence     )) integrateur%convergence       = convergence    
    if(present(epsilon_rel     )) integrateur%epsilon_rel       = epsilon_rel    
    if(present(epsilon_abs     )) integrateur%epsilon_abs       = epsilon_abs    
    if(present(epsilon_init    )) integrateur%epsilon_init      = epsilon_init   
    if(present(epsilon_prog    )) integrateur%epsilon_prog      = epsilon_prog   
    if(present(indic_lib       )) integrateur%indic_lib         = indic_lib

  end subroutine MSP_modifier_integrator
  


  subroutine MSP_afficher_integrator(integrateur,lfn)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_afficher_integrator
!
!$Resume
!  Affichage d'une structure MSP_INTEGRATOR, selon le type d'intégrateur choisi
!
!$Description
!  Affichage d'une structure MSP_INTEGRATOR, selon le type d'intégrateur choisi
!
!$Auteur
!  Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_afficher_integrator(integrateur,[lfn])
!.    type(Msp_integrator) :: integrateur
!.    integer :: lfn
!
!$Arguments
!>E     integrateur  :<Msp_integrator>   Structure MSP_integrator
!>[E]   lfn          :<integer>          Logical file number (n° de fichier ou de canal pour l'affichage)
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

    ! Argument
    !=========
    type(Msp_integrator), intent(in) :: integrateur
    integer, optional, intent(in)    ::  lfn

    ! Variables locales
    !==================
    integer :: ii, lfn_local

    ! Début du code
    !==============
    
    if (present(lfn)) then
       lfn_local = lfn
    else 
       lfn_local = 0
    end if

    write (lfn_local,100)
        
    select case(integrateur%type_integrateur)
    case(pm_cowell)
       write (lfn_local,103) "Type d'intégrateur","Cowell"
       write (lfn_local,101) "Ordre",integrateur%ordre
       write (lfn_local,102) "Pas d'intégration",integrateur%pas
       write (lfn_local,101) "Circularisation",integrateur%circularisation
       write (lfn_local,101) "Second membre simplifié",integrateur%second_membre_simp
       write (lfn_local,101) "Convergence",integrateur%convergence
       write (lfn_local,102) "Seuil de convergence initial",integrateur%epsilon_init
       write (lfn_local,102) "Seuil de convergence de progression",integrateur%epsilon_prog
       do ii=1,6
          write (lfn_local,104) "Libération variable",ii,integrateur%indic_lib(ii)
       end do
    case(pm_dop853)
       write (lfn_local,103) "Type d'intégrateur","DOP 853"
       write (lfn_local,102) "Pas d'intégration min",integrateur%pas
       write (lfn_local,102) "Pas d'intégration max",integrateur%pas_max
       write (lfn_local,102) "Tolérance relative",integrateur%epsilon_rel
       write (lfn_local,102) "Tolérance absolue",integrateur%epsilon_abs
    case(pm_gill)
       write (lfn_local,103) "Type d'intégrateur","Gill"
       write (lfn_local,102) "Pas d'intégration",integrateur%pas

    end select
         
    write (lfn_local,100)    



    ! Déclaration de formats d'affichage 
    !===================================
    
    ! ligne de séparation
100  format('-----------------')
    ! affichage d'une chaine de caracteres,d'un entier, ou d'un réel
101  format(a,(' : '),i4)    
102  format(a,(' : '),e20.6)    
103  format(a,(' : '),a)
104  format(a,i4,(' : '),i4)   

  end subroutine MSP_afficher_integrator



end module MSP_INTEGRATOR_DEF
