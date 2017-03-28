module parametre_interne_mspro

! (C) Copyright CNES - MSPRO - 2002 - 2005

!************************************************************************
!
! But:  Definition des parametres internes MSPRO (hors numeros de routines et de codes retour)
! ===   
!
! Note d'utilisation: Ce module n'est accessible qu'en interne a la MSPRO 
! ==================  
!
!$Historique
! ==========
!   + Version 3.0 : creation
!                         (Date: 11/2002 - Realisation: Michel Lagreca/Bruno Revelin)
!   + Version 5.2 (DE 1): Ajout des parametres pour les integrateurs
!                         (Date: 01/2005 - Realisation: Bruno Revelin)
!   + Version 5.3 :  DM-ID 408 : mettre un Cowell dans les integrateurs MSPRO
!                     (Date: 10/2005 - Realisation: Equipe Patrimoine Mecanique Spatiale - Atos Origin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 5.9 : FA-ID 971 : constante pour désigner le nb max d'équations du Cowell
!                    (Date: 03/2008 - Réalisation Y.TANGUY Atos Origin)
!
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

use mslib 

implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: parametre_interne_mspro.f90 69 2012-09-11 08:33:34Z ffsm $'


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! General a tous les themes en interne

! Logiques OUI et NON   
logical, parameter :: pm_i_oui = .true.
logical, parameter :: pm_i_non = .false.

! pour le changement d'echelle de temps
integer, parameter :: pm_i_te_tai = 12
integer, parameter :: pm_i_tai_tuc = 23
integer, parameter :: pm_i_tuc_tai = 32
integer, parameter :: pm_i_tai_te = 21

! valeur de racines
real(pm_reel), parameter :: pm_i_rac2 = 1.4142135623730951_pm_reel
real(pm_reel), parameter :: pm_i_rac6 = 2.4494897427831779_pm_reel

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Theme U: integrateurs

! Entier nul
integer, parameter :: pm_i_nze = 0

! Reel nul
real(pm_reel), parameter :: pm_i_zero = 0._pm_reel

! Entier 1
integer, parameter :: pm_i_nun = 1
integer, parameter :: pm_i_nne = 9

! Pour les tableaux du Cowell
integer, parameter :: pm_i_dim15 = 15
integer, parameter :: pm_i_dim16 = 16
integer, parameter :: pm_i_dim17 = 17
integer, parameter :: pm_i_NBMAX_EQU_COWELL = 221 ! nb max d'équations (avec matrices de transition)

! Nb max d'integrateurs
integer, parameter :: pm_i_NBMAX_INTEG = 3

! Taille initiale du tableau de fonctions de commutations
integer, parameter :: pm_i_SIZE_COMMUT = 5

! Nb max d'iterations pour la recherche de racines dans l'integrateur
integer, parameter :: pm_i_NB_MAX_ITER = 1000

! nb d'etapes de chaque integrateur (cf ordre dans parametre_mspro)
integer,dimension(pm_i_NBMAX_INTEG), parameter :: pm_i_nb_etapes = (/4,13,0/)

! ordre de chaque integrateur 
integer,dimension(pm_i_NBMAX_INTEG), parameter :: pm_i_ordre = (/4,8,16/)

! taille du vecteur d'interpolation (pour certains integrateurs a pas variable)
integer,dimension(pm_i_NBMAX_INTEG), parameter :: pm_i_taille_v = (/0,7,0/)

! parametres RKF pour le pas variable
real(pm_reel),parameter :: pm_i_RKF_securite = 0.9_pm_reel
real(pm_reel),parameter :: pm_i_RKF_min_reduc = 0.2_pm_reel
real(pm_reel),parameter :: pm_i_RKF_max_augm = 10._pm_reel

! tableau de Butcher et erreur pour chaque integrateur

! Gill
real(pm_reel),dimension(6),  parameter :: pm_i_Gill_a = (/ &
     0.5_pm_reel       , &
     (pm_i_rac2-1._pm_reel)/2._pm_reel , (2._pm_reel-pm_i_rac2)/2._pm_reel, &
     0._pm_reel        ,  -pm_i_rac2/2._pm_reel, (2._pm_reel+pm_i_rac2)/2._pm_reel /)
real(pm_reel),dimension(4),  parameter :: pm_i_Gill_b = (/ &
     1._pm_reel/6._pm_reel, (2._pm_reel-pm_i_rac2)/6._pm_reel, &
     (2._pm_reel+pm_i_rac2)/6._pm_reel,  1._pm_reel/6._pm_reel/)
real(pm_reel),dimension(3),  parameter :: pm_i_Gill_c = (/ 0.5_pm_reel, 0.5_pm_reel, 1._pm_reel/)
logical,parameter              :: pm_i_Gill_report_fin_deb = pm_i_non

! DormandPrince853
real(pm_reel),dimension(78),  parameter :: pm_i_DOP853_a = (/ &
! k2
     (12._pm_reel - 2._pm_reel * pm_i_rac6) / 135._pm_reel,  &
! k3
     (6._pm_reel - pm_i_rac6) / 180._pm_reel, (6._pm_reel - pm_i_rac6) / 60._pm_reel,  &
! k4
     (6._pm_reel - pm_i_rac6) / 120._pm_reel, 0._pm_reel, (6._pm_reel - pm_i_rac6) / 40._pm_reel,  &
! k5
     (462._pm_reel + 107._pm_reel * pm_i_rac6) / 3000._pm_reel, 0._pm_reel,  &
     (-402._pm_reel - 197._pm_reel * pm_i_rac6) / 1000._pm_reel, &
     (168._pm_reel + 73._pm_reel * pm_i_rac6) / 375._pm_reel,  &
! k6
     1._pm_reel / 27._pm_reel, 0._pm_reel, 0._pm_reel, (16._pm_reel + pm_i_rac6) / 108._pm_reel, &
     (16._pm_reel - pm_i_rac6) / 108._pm_reel,  &
! k7
     19._pm_reel / 512._pm_reel, 0._pm_reel, 0._pm_reel, (118._pm_reel + 23._pm_reel * pm_i_rac6) / 1024._pm_reel, &
     (118._pm_reel - 23._pm_reel * pm_i_rac6) / 1024._pm_reel, -9._pm_reel / 512._pm_reel,  &
! k8
     13772._pm_reel / 371293._pm_reel, 0._pm_reel, 0._pm_reel,  &
     (51544._pm_reel + 4784._pm_reel * pm_i_rac6) / 371293._pm_reel, &
     (51544._pm_reel - 4784._pm_reel * pm_i_rac6) / 371293._pm_reel, &
     -5688._pm_reel / 371293._pm_reel, 3072._pm_reel / 371293._pm_reel,  &
! k9
     58656157643._pm_reel / 93983540625._pm_reel, 0._pm_reel, 0._pm_reel,  &
     (-1324889724104._pm_reel - 318801444819._pm_reel * pm_i_rac6) / 626556937500._pm_reel,  &
     (-1324889724104._pm_reel + 318801444819._pm_reel * pm_i_rac6) / 626556937500._pm_reel,  &
     96044563816._pm_reel / 3480871875._pm_reel, 5682451879168._pm_reel / 281950621875._pm_reel,  &
     -165125654._pm_reel / 3796875._pm_reel,  &
! k10
     8909899._pm_reel / 18653125._pm_reel, 0._pm_reel, 0._pm_reel,  &
     (-4521408._pm_reel - 1137963._pm_reel * pm_i_rac6) / 2937500._pm_reel,  &
     (-4521408._pm_reel + 1137963._pm_reel * pm_i_rac6) / 2937500._pm_reel,  &
     96663078._pm_reel / 4553125._pm_reel, 2107245056._pm_reel / 137915625._pm_reel,  &
     -4913652016._pm_reel / 147609375._pm_reel, -78894270._pm_reel / 3880452869._pm_reel,  &
! k11
     -20401265806._pm_reel / 21769653311._pm_reel, 0._pm_reel, 0._pm_reel,  &
     (354216._pm_reel + 94326._pm_reel * pm_i_rac6) / 112847._pm_reel,  &
     (354216._pm_reel - 94326._pm_reel * pm_i_rac6) / 112847._pm_reel,  &
     -43306765128._pm_reel / 5313852383._pm_reel, -20866708358144._pm_reel / 1126708119789._pm_reel,  &
     14886003438020._pm_reel / 654632330667._pm_reel, 35290686222309375._pm_reel / 14152473387134411._pm_reel,  &
     -1477884375._pm_reel / 485066827._pm_reel,  &
! k12
     39815761._pm_reel / 17514443._pm_reel, 0._pm_reel, 0._pm_reel,  &
     (-3457480._pm_reel - 960905._pm_reel * pm_i_rac6) / 551636._pm_reel,  &
     (-3457480._pm_reel + 960905._pm_reel * pm_i_rac6) / 551636._pm_reel,  &
     -844554132._pm_reel / 47026969._pm_reel, 8444996352._pm_reel / 302158619._pm_reel,  &
     -2509602342._pm_reel / 877790785._pm_reel, -28388795297996250._pm_reel / 3199510091356783._pm_reel,  &
     226716250._pm_reel / 18341897._pm_reel, 1371316744._pm_reel / 2131383595._pm_reel,  &
! k13 pour interpolation seulement a priori, l'etape etant la meme que la 1ere
!     du pas suivant.
     104257._pm_reel/1920240._pm_reel, 0._pm_reel, 0._pm_reel, 0._pm_reel, 0._pm_reel, 3399327._pm_reel/763840._pm_reel,  &
     66578432._pm_reel/35198415._pm_reel, -1674902723._pm_reel/288716400._pm_reel,  &
     54980371265625._pm_reel/176692375811392._pm_reel, -734375._pm_reel/4826304._pm_reel,  &
     171414593._pm_reel/851261400._pm_reel, 137909._pm_reel/3084480._pm_reel /)

real(pm_reel),dimension(13),  parameter :: pm_i_DOP853_b = (/ &
              104257._pm_reel/1920240._pm_reel,   0._pm_reel,   0._pm_reel,   0._pm_reel,   0._pm_reel, &
             3399327._pm_reel/763840._pm_reel, &
            66578432._pm_reel/35198415._pm_reel, &
         -1674902723._pm_reel/288716400._pm_reel, &
      54980371265625._pm_reel/176692375811392._pm_reel, &
             -734375._pm_reel/4826304._pm_reel, &
           171414593._pm_reel/851261400._pm_reel, &
              137909._pm_reel/3084480._pm_reel, &
                   0._pm_reel /)

real(pm_reel),dimension(12),  parameter :: pm_i_DOP853_c = (/ &
     (12._pm_reel - 2._pm_reel * pm_i_rac6) / 135._pm_reel, &
     (6._pm_reel - pm_i_rac6) / 45._pm_reel, &
     (6._pm_reel - pm_i_rac6) / 30._pm_reel, &
     (6._pm_reel + pm_i_rac6) / 30._pm_reel, &
     1._pm_reel/3._pm_reel ,   1._pm_reel/4._pm_reel, &
     4._pm_reel/13._pm_reel, 127._pm_reel/195._pm_reel, &
     3._pm_reel/5._pm_reel ,   6._pm_reel/7._pm_reel,   1._pm_reel, 1._pm_reel /)

logical,parameter              :: pm_i_DOP853_report_fin_deb = pm_i_oui

! erreur
real(pm_reel),dimension(12), parameter:: pm_i_DOP853_e1 = (/ &
           116092271._pm_reel / 8848465920._pm_reel, 0._pm_reel, 0._pm_reel, 0._pm_reel, 0._pm_reel, &
            -1871647._pm_reel / 1527680._pm_reel,  &
           -69799717._pm_reel / 140793660._pm_reel,  &
       1230164450203._pm_reel / 739113984000._pm_reel,  &
   -1980813971228885._pm_reel / 5654156025964544._pm_reel,  &
           464500805._pm_reel / 1389975552._pm_reel,  &
       1606764981773._pm_reel / 19613062656000._pm_reel,  &
             -137909._pm_reel / 6168960._pm_reel /)

real(pm_reel),dimension(12), parameter:: pm_i_DOP853_e2 = (/ &
             -364463._pm_reel / 1920240._pm_reel, 0._pm_reel, 0._pm_reel, 0._pm_reel, 0._pm_reel, &
             3399327._pm_reel / 763840._pm_reel,  &
            66578432._pm_reel / 35198415._pm_reel,  &
         -1674902723._pm_reel / 288716400._pm_reel,  &
     -74684743568175._pm_reel / 176692375811392._pm_reel,  &
             -734375._pm_reel / 4826304._pm_reel,  &
           171414593._pm_reel / 851261400._pm_reel,  &
               69869._pm_reel / 3084480._pm_reel /)

! interpolation
  ! k14
real(pm_reel), parameter:: pm_i_DOP853_c14 = 1._pm_reel / 10._pm_reel

real(pm_reel),dimension(13), parameter:: pm_i_DOP853_k14 = (/ &
       13481885573._pm_reel / 240030000000._pm_reel      - pm_i_DOP853_b(1),0._pm_reel ,0._pm_reel ,0._pm_reel ,0._pm_reel,  &
                 0._pm_reel                              - pm_i_DOP853_b(6),  &
      139418837528._pm_reel / 549975234375._pm_reel      - pm_i_DOP853_b(7),  &
   -11108320068443._pm_reel / 45111937500000._pm_reel    - pm_i_DOP853_b(8),  &
 -1769651421925959._pm_reel / 14249385146080000._pm_reel - pm_i_DOP853_b(9),  &
          57799439._pm_reel / 377055000._pm_reel         - pm_i_DOP853_b(10),  &
      793322643029._pm_reel / 96734250000000._pm_reel    - pm_i_DOP853_b(11),  &
        1458939311._pm_reel / 192780000000._pm_reel      - pm_i_DOP853_b(12),  &
             -4149._pm_reel / 500000._pm_reel /)

  ! k15
real(pm_reel), parameter:: pm_i_DOP853_c15    = 1._pm_reel / 5._pm_reel

real(pm_reel),dimension(14), parameter:: pm_i_DOP853_k15 = (/ &
     1595561272731._pm_reel / 50120273500000._pm_reel    - pm_i_DOP853_b(1), 0._pm_reel, 0._pm_reel, 0._pm_reel, 0._pm_reel, &
      975183916491._pm_reel / 34457688031250._pm_reel    - pm_i_DOP853_b(6),  &
    38492013932672._pm_reel / 718912673015625._pm_reel   - pm_i_DOP853_b(7),  &
 -1114881286517557._pm_reel / 20298710767500000._pm_reel - pm_i_DOP853_b(8),  &
                 0._pm_reel                              - pm_i_DOP853_b(9),  &
                 0._pm_reel                              - pm_i_DOP853_b(10),  &
    -2538710946863._pm_reel / 23431227861250000._pm_reel - pm_i_DOP853_b(11),  &
        8824659001._pm_reel / 23066716781250._pm_reel    - pm_i_DOP853_b(12),  &
      -11518334563._pm_reel / 33831184612500._pm_reel,  &
        1912306948._pm_reel / 13532473845._pm_reel  /)

  ! k16
real(pm_reel), parameter:: pm_i_DOP853_c16    = 7._pm_reel / 9._pm_reel

real(pm_reel),dimension(15), parameter:: pm_i_DOP853_k16 = (/  &
      -13613986967._pm_reel / 31741908048._pm_reel       - pm_i_DOP853_b(1),0._pm_reel, 0._pm_reel, 0._pm_reel, 0._pm_reel,  &
       -4755612631._pm_reel / 1012344804._pm_reel        - pm_i_DOP853_b(6),  &
    42939257944576._pm_reel / 5588559685701._pm_reel     - pm_i_DOP853_b(7),  &
    77881972900277._pm_reel / 19140370552944._pm_reel    - pm_i_DOP853_b(8),  &
    22719829234375._pm_reel / 63689648654052._pm_reel    - pm_i_DOP853_b(9),  &
                 0._pm_reel                              - pm_i_DOP853_b(10),  &
                 0._pm_reel                              - pm_i_DOP853_b(11),  &
                 0._pm_reel                              - pm_i_DOP853_b(12),  &
       -1199007803._pm_reel / 857031517296._pm_reel,  &
      157882067000._pm_reel / 53564469831._pm_reel,  &
     -290468882375._pm_reel / 31741908048._pm_reel   /)

  ! poids pour interpolation
  ! seuls les poids non nuls sont presents
real(pm_reel),dimension(48), parameter:: pm_i_DOP853_tmp_d = (/ &
             -17751989329._pm_reel / 2106076560._pm_reel,               4272954039._pm_reel / 7539864640._pm_reel, &
            -118476319744._pm_reel / 38604839385._pm_reel,            755123450731._pm_reel / 316657731600._pm_reel, &
      3692384461234828125._pm_reel / 1744130441634250432._pm_reel,     -4612609375._pm_reel / 5293382976._pm_reel, &
            2091772278379._pm_reel / 933644586600._pm_reel,             2136624137._pm_reel / 3382989120._pm_reel, &
                  -126493._pm_reel / 1421424._pm_reel,                    98350000._pm_reel / 5419179._pm_reel, &
                -18878125._pm_reel / 2053168._pm_reel,                 -1944542619._pm_reel / 438351368._pm_reel, &
!
              32941697297._pm_reel / 3159114840._pm_reel,             456696183123._pm_reel / 1884966160._pm_reel, &
           19132610714624._pm_reel / 115814518155._pm_reel,       -177904688592943._pm_reel / 474986597400._pm_reel, &
     -4821139941836765625._pm_reel / 218016305204281304._pm_reel,      30702015625._pm_reel / 3970037232._pm_reel, &
          -85916079474274._pm_reel / 2800933759800._pm_reel,           -5919468007._pm_reel / 634310460._pm_reel, &
                  2479159._pm_reel / 157936._pm_reel,                    -18750000._pm_reel / 602131._pm_reel, &
                -19203125._pm_reel / 2053168._pm_reel,                 15700361463._pm_reel / 438351368._pm_reel, &
!
              12627015655._pm_reel / 631822968._pm_reel,              -72955222965._pm_reel / 188496616._pm_reel, &
          -13145744952320._pm_reel / 69488710893._pm_reel,          30084216194513._pm_reel / 56998391688._pm_reel, &
      -296858761006640625._pm_reel / 25648977082856624._pm_reel,         569140625._pm_reel / 82709109._pm_reel, &
             -18684190637._pm_reel / 18672891732._pm_reel,                69644045._pm_reel / 89549712._pm_reel, &
                -11847025._pm_reel / 4264272._pm_reel,                  -978650000._pm_reel / 16257537._pm_reel, &
                519371875._pm_reel / 6159504._pm_reel,                  5256837225._pm_reel / 438351368._pm_reel, &
!
               -450944925._pm_reel / 17550638._pm_reel,               -14532122925._pm_reel / 94248308._pm_reel, &
            -595876966400._pm_reel / 2573655959._pm_reel,             188748653015._pm_reel / 527762886._pm_reel, &
      2545485458115234375._pm_reel / 27252038150535163._pm_reel,       -1376953125._pm_reel / 36759604._pm_reel, &
              53995596795._pm_reel / 518691437._pm_reel,                 210311225._pm_reel / 7047894._pm_reel, &
                 -1718875._pm_reel / 39484._pm_reel,                      58000000._pm_reel / 602131._pm_reel, &
                 -1546875._pm_reel / 39484._pm_reel,                   -1262172375._pm_reel / 8429834._pm_reel /)


integer,parameter:: pm_i_DOP853_l_ydotK_reste = 3  ! taille du tableau ydotK_reste

!...............................................................................................................

character(len=pm_longueur_info_utilisateur),private :: info_utilisateur = &
                  '@(#) Fichier MSPRO parametre_interne_mspro.f90: derniere modification V5.15 >'

 
       
end module parametre_interne_mspro

