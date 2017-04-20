module MSP_BULLETIN_DEF

!*******************************************************************************
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  MSP_BULLETIN_DEF
!
!$Resume
!  Module contenant les informations de base à un bulletin orbital.
!
!$Description
!  Module contenant les informations de base à un bulletin orbital.
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: MSP_BULLETIN_DEF.F90 365 2013-02-18 12:36:19Z aadt $
!
!$Historique
!  $Log: MSP_BULLETIN_DEF.F90,v $
!  Revision 365  2013/02/18 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 158 2012/11/27 Fran Simarro/FFSM - GMV
!  Fermeture de code source pour la livraison BIBMS 1.13 (dec 2012)
!
!  Revision 150 2012/11/23 ffsm
!  FA-ID 1524 : Modifié pour prendre en compte l'échelle temps de la date d'entrée
!               à l'heure d'ajouter un saut TaiTuc
!
!  Revision 1.76  2011/06/10 06:44:30  vivaresf
!  VERSION::FA-ID:1466:10/06/2011:idem pour les paramètres topocentriques
!
!  Revision 1.75  2011/06/09 08:38:27  vivaresf
!  VERSION::FA-ID:1466:09/06/2011:une seule lecture COMPAS pour limiter le temps de calcul
!
!  Revision 1.74  2011/06/03 10:30:51  vivaresf
!  VERSION::FA-ID:1466:03/06/2011:récupération des valeurs par défaut depuis COMPAS
!
!  Revision 1.73  2011/04/26 10:50:18  vivaresf
!  VERSION::FA-ID:1466:26/04/2011:appels à COMPAS pour des valeurs par défaut des constantes physiques du bulletin
!
!  Revision 1.72  2010/10/20 09:35:42  mercadig
!  VERSION::AQ::20/10/2010:Ajout du marqueur de fin historique dans le cartouche
!
!  Revision 1.71  2009/03/12 16:37:19  mercadig
!  FA-ID 1178: Creation de MSP_EPSILON_PRS pour le module de pression de radiation solaire
!
!  Revision 1.70  2009/03/11 13:36:13  mercadig
!  FA-ID 1178: Modification du seuil MSP_EPSILON_APLA
!
!  Revision 1.69  2008/11/24 14:15:33  huec
!  FA-ID 1156 : Ajout d\'une condition pour le declenchement d\'une erreur sur presence d\'arguments
!
!  Revision 1.68  2008/11/19 13:31:32  mercadig
!  DM-ID 733 : Mise a jour cartouche
!
!  Revision 1.67  2008/11/05 08:38:49  tanguyy
!  AQ : maj des cartouches
!  Revision 1.66  2008/08/08 14:17:48  gss
!  DM-ID 1058 : (portage g95) initialisation à NULL des pointeurs lors de leur
!  déclaration. Suppression de variables non utilisées. Forcage de typage réel
!  vers entiers des variables dir_gs et diffdate. Ajout d'une gestion d'erreur
!  sur les appels à la fonction acc_select.
!  Revision 1.65  2008/07/04 15:01:11  huec
!  DM-ID 1058 : Initialisation de variable
!  Revision 1.64  2008/04/14 09:41:22  huec
!  AQ : Amelioration d operations sur des entiers
!  Revision 1.63  2008/04/08 13:05:48  huec
!  FA-ID 865 : La variable modprec n existe plus dans la GSLIB
!  Revision 1.62  2008/02/26 09:15:39  huec
!  FA-ID 930 : Correction des tests sur la presence des arguments specifiques au repere STATION
!  Revision 1.61  2008/02/19 09:14:20  huec
!  DM-ID 11 : Suppression des variables inutilisees (saut du TUC)
!  Revision 1.60  2008/02/15 08:50:14  huec
!  DM-ID 11 : Utilisation de la base COMPAS pour le saut du TUC
!  Revision 1.59  2008/02/13 13:13:13  huec
!  DM-ID 11 : Modification de la routine MSP_calcul_ecart_echt qui appelle desormais la routine COMPAS cps_get_sautTAITUC
!  Revision 1.58  2007/11/27 14:55:18  huec
!  DM-ID 699 : Amelioration des moyens de tests MECASPA
!  Revision 1.57  2007/11/19 09:39:31  huec
!  DM-ID 820 : Scripts de generation automatique, utilisation de makemake V2.0
!  Revision 1.56  2007/10/23 15:01:28  huec
!  FA-ID 776 : Variables locales non utilisees dans la MECASPA
!  Revision 1.55  2007/10/16 12:41:33  huec
!  Correction de coquilles
!  Revision 1.54  2007/06/04 16:09:43  vivaresf
!  DM-ID 538 : validation
!  Revision 1.53  2007/05/30 12:13:49  couturis
!  DM-ID 538: modifications dues a l evolution de GS_BULLETIN_IP et GS_REPERE_IP
!  Revision 1.52  2007/05/21 16:22:17  vivaresf
!  DM-ID 538 : théorie planétaire dans les routines de lecture/ecriture du bulletin
!  Revision 1.51  2006/11/09 09:13:58  mle
!  DM-ID 487 : noms des parameter dans MECASPA
!  Revision 1.50  2006/06/02 11:21:53  vpg
!  DM-ID 232 : qualite. Nommage des arguments optionnels lors des appels de fonctions et de routines
!  Revision 1.49  2006/05/17 07:43:39  okd
!  FA-ID 540 : Cloture du FT (Warnings injustifiés sur conversion de bulletin)
!  Revision 1.48.2.1  2006/05/17 07:43:08  okd
!  Correction FA 540
!  Revision 1.48  2005/10/27 09:57:51  vivaresf
!  DM-ID 396 : GS_LIB avec option COMPAS
!  Revision 1.47  2005/10/27 07:34:34  vivaresf
!  DM-ID 396 : Cloture du FT (Mode COMPAS dans la GS_LIB)
!  Revision 1.46.2.1  2005/10/27 07:34:01  vivaresf
!  DM-ID 396 : Version initiale du FT (Mode COMPAS dans la GS_LIB)
!  Revision 1.46  2005/03/08 07:32:34  fabrec
!  DM-ID 111 : mise à jour des cartouches
!  Revision 1.45  2005/01/20 13:56:17  pauh
!  FA_332
!  Revision 1.44.2.1  2005/01/19 10:07:31  pauh
!  FA 332 : Appels de DEALLOCATE avec l'argument stat=MSP_iostat
!  Revision 1.44  2005/01/10 12:43:49  vivaresf
!  FA_245
!  Revision 1.43.2.1  2005/01/10 12:43:27  vivaresf
!  Presentation
!  Revision 1.43  2005/01/06 09:35:29  vivaresf
!  Presentation
!  Revision 1.42  2004/12/22 09:25:13  vivaresf
!  Entetes
!  Revision 1.41  2004/12/22 08:01:19  vivaresf
!  DM_110
!  Revision 1.40.2.1  2004/12/22 07:58:45  vivaresf
!  DM-ID 110 : cle repere dans MSP_convertir_bulletin
!  FA-ID 328 : traitement des parametres rep et lonref dans MSP_creer_typrep
!  FA-ID 328 : traitement du parametre rep dans MSP_consulter_typrep
!  Revision 1.40  2004/12/21 14:36:15  vivaresf
!  Entetes
!  Revision 1.39  2004/12/21 14:03:26  vivaresf
!  DM_109
!  Revision 1.38.2.1  2004/12/21 13:53:56  vivaresf
!  DM_ID 109 : routines MSP_creer_typrep_mad et MSP_ecrire_typrep
!  Revision 1.38  2004/11/19 11:25:32  vivaresf
!  FA-ID 245 : conversion des dates de references des reperes
!  a la lecture d'un fichier MADONA
!  Revision 1.37  2004/10/11 08:48:49  vivaresf
!  DM 82 : precision sur les dates des manoeuvres dans la GSLIB,
!  prise en compte du nouvel appel de read/write_GS_BULLETIN_IP
!  Revision 1.36  2004/07/12 17:54:35  vivaresf
!  DM 89 : intégration
!  Revision 1.34.2.3  2004/07/08 17:03:07  vivaresf
!  DM-ID 89 : gestion des saut du TUC pour dates reperes 1950
!  Revision 1.34.2.2  2004/07/07 10:28:00  vivaresf
!  DM-ID 89 : mise a jour des cartouches
!  Revision 1.34.2.1  2004/07/06 09:58:00  vivaresf
!  DM-ID 89 : MSLIB90 5, MSPRO 4.
!  - variables de poles par defaut MSP_poleu2000, MSP_polev2000
!  - variable de test pour la comparaison de reels (hors dates MSP_EPSILON_APLA)
!  - MSP_convertir_bulletin :
!    . gestion des dates de reference
!    . parametre vrot pour le repere de sortie
!  - Test d'egalite sur les dates simplifie
!  - MSP_conv_typrep : gestion des obliquite, dates de references, poles et modeles
!  - MSP_calcul_ecart_echt : 0 pour les ecarts TE-TUC avant le premier saut
!  - MSP_precession : affichage du mode en texte et non en code
!  Revision 1.34  2004/05/27 16:05:06  vivaresf
!  DM-ID 89 : use MSPRO sans restrictions
!  Revision 1.33  2004/05/06 09:22:09  vivaresf
!  DM-ID 83 : intégration
!  Revision 1.31.2.2  2004/05/03 14:47:23  vivaresf
!  DM-ID 83, dates en jour/secondes avec origine MJD1950 ou MJD2000
!  dans les routines d'acces les dates sont soit reelles, soit jour/secondes 
!  (option datbul_js, date_ref_js)
!  - 2 constantes : MSP_JJ2000 (J2000 des reperes a J2000), MSP_MJD2000 
!  - l'origine des dates est accessible (option origdat, defaut J50)
!  - fonctions internes mise en 'private',
!  - affichage des dates en jour/secondes,
!  - affichage de l'origine
!  Revision 1.31.2.1  2004/04/15 13:39:20  vivaresf
!  DM_83, version initiale, sans les modifs de la DM 89 (a reprendre plus tard)
!  Revision 1.31  2004/02/17 11:12:51  adm_ipsi
!  DM-ID 89, Utilisation MSPRO V4.0, pm_uai94 renommé en pm_uai1994
!  Revision 1.30  2004/01/14 16:40:51  adm_ipsi
!  *** empty log message ***
!  Revision 1.28  2003/11/06 09:36:00  adm_ipsi
!  *** empty log message ***
!  Revision 1.27  2003/10/29 17:46:19  adm_ipsi
!  FA-ID 70, prise en compte de dateref
!  Revision 1.26  2003/10/13 13:45:44  adm_ipsi
!  DM-ID 66, Retrait des messages de warning dans l'appel à mx_var, 
!  FA-ID 67, Utilisation du parametre planete pour l'appel a verif_typrep
!  Revision 1.25  2003/09/30 16:16:56  adm_ipsi
!  DM-ID 52, Utilisation de MSLIB V4.1 et MSPRO V3.1 
!               - Utilisation pm_hpha, 
!               - Suppression de MSP_conv_typbul, 
!               - Calcul de la Jacobienne par mx_var, 
!               - Suppression du parametre cle_mat de MSP_conv_typrep
!  Revision 1.24  2003/09/17 14:31:24  adm_ipsi
!   Suppression de l'option save pour les variables globales
!  Revision 1.23  2003/09/16 09:54:01  adm_ipsi
!  FA-ID 33, Passage au niveau module des variables contenant les donnés de saut du TUC
!  Revision 1.22  2003/08/01 16:56:56  adm_ipsi
!  DM-ID 43 : Utilisation nouvelle mslib 4.0 et mspro 3.0, nouvelle codification des constantes
!
!$FinHistorique
!
!$Usage
!  use MSP_BULLETIN_DEF
!
!$Structure
!
!: MSP_BULLETIN : 
!#V
!>     datbul          : <tm_jour_sec,private>      date du bulletin (JJ)
!>     ech_temps_bul   : <integer,private>          échelle de temps de la date du bulletin (pm_TUC, pm_TE)
!>     coord           : <MSP_TYPBUL,private>       type dérivé contenant les informations liées aux coordonnées
!>     repere          : <MSP_TYPREP,private>       type dérivé contenant les informations liées au repère
!>     modprec         : <integer,private>          mode de précession (pm_lieske_wahr_aoki, pm_uai1994)
!#
!
!: MSP_TYPBUL : 
!#V
!>     iorb            : <integer,private>          type de coordonnées (pm_kep, pm_cir,
!>                                         pm_equa, pm_cir_equa, pm_car,
!>                                         pm_hpha, pm_geoc, 
!>                                         MSP_ENUM_RENTREE_SPHERIQUE, pm_geod_meca_vol,
!>                                         pm_geod_gps_ard)
!>     param           : <pm_reel,DIM=(6),private>  coordonnées (cf. définition de iorb)
!>     mu              : <pm_reel,private>          terme central du potentiel [m^3/s^2]
!>     requa           : <pm_reel,private>          rayon terrestre à l'équateur [m]
!>     apla            : <pm_reel,private>          aplatissement terrestre
!#
!
!: MSP_TYPREP : 
!#V
!>     typrep          : <integer,private>          type de repère (pm_equa_vrai, pm_equa_moyen, pm_ecli_moyen,
!>                                               pm_veis, pm_planeto_ref, pm_planeto_ref_iner, pm_planeto_vrai ,
!>                                               pm_equa_uai, pm_topo)        
!>     cle_date        : <integer,private>          type de date de référence du repère :
!>                                              pm_1janvier1950_00h00   date du repère = 1/1/1950 à 0h00         
!>                                              pm_1janvier2000_12h00   date du repère = 1/1/2000 à 12h00 (18262.5 JJCNES)         
!>                                              MSP_ENUM_ECHD_DATE_REF  date du repère fixée par une date de référence         
!>                                              pm_autre_date           date du repère fixée par la date du bulletin          
!>     date_ref        : <tm_jour_sec,private>      date de référence [JJ]       
!>     origdat         : <integer,private>          origine des dates (J50, MJD2000)
!>     planete         : <integer,private>          planète fixant la base vectorielle (eph_mercure,eph_venus,eph_terre,eph_mars,
!>                                              eph_jupiter,eph_saturne,eph_uranus,eph_neptune,eph_pluton)
!>     corcen          : <integer,private>          corps central fixant l'origine du repère (eph_mercure,eph_venus,eph_terre,eph_mars,
!>                                          eph_jupiter,eph_saturne,eph_uranus,eph_neptune,eph_pluton,
!>                                          eph_soleil)
!>     ech_temps_rep   : <integer,private>          échelle de temps de la date de référence du repère  (pm_TUC, pm_TE)   
!>     obli            : <pm_reel,private>          obliquite du corps central [rad]    
!>     vrot            : <pm_reel,private>          vitesse de rotation [rad/s]
!>     lonref          : <pm_reel,private>          longitude de référence [rad]    
!>     pole            : <tm_pole_uv,private>       type dérivé contenant les coordonnées du pole vrai    
!>     station         : <tm_def_topo,private>      type dérivé contenant les coordonnées définissant une station    
!#
!
!$Global
!
!>  MSP_ENUM_KEPLERIEN               : <integer,parameter>                   keplerien (a,e,i,pom,gom,M) en (m,-,rad,rad,rad,rad)     
!>  MSP_ENUM_ADAPTE_CIRCULAIRE       : <integer,parameter>                   adapté circulaire (a,ex,ey,i,gom,pom+M) en (m,-,-,rad,rad,rad)
!>                                                         avec ex = e*cos(pom) et ey = e*sin(w)
!>  MSP_ENUM_ADAPTE_EQUATORIAL       : <integer,parameter>                   adapté équatorial (a,e,hx,hy,pom+gom,M) en (m,-,-,-,rad,rad)
!>                                                         avec hx = 2*sin(i/2)*cos(gom) et hy = 2*sin(i/2)*sin(gom)         
!>  MSP_ENUM_ADAPTE_CIRC_EQ          : <integer,parameter>                   adapté circulaire équatorial (a,ex,ey,hx,hy,pom+gom+M) en (m,-,-,-,-,rad)
!>                                                         avec ex = e*cos(pom+gom), ey = e*sin(pom+gom)
!>                                                         hx = 2*sin(i/2)*cos(gom) et hy = 2*sin(i/2)*sin(gom)
!>  MSP_ENUM_CARTESIENS              : <integer,parameter>                   cartésien (x,y,z,vx,vy,vz) en (m,m,m,m/s,m/s,m/s)
!>  MSP_ENUM_SPHERIQUE               : <integer,parameter>                   sphérique (site,gisement,distance,d(s)/dt,d(g)/dt,d(d)/dt)
!>                                                         en (rad,rad,m,rad/s,rad/s,m/s)         
!>  MSP_ENUM_RENTREE_ELLIPSOIDIQUE   : <integer,parameter>                   rentrée ellipsoidique (altv,latv,lon,vit,penv,aziv)
!>                                                         en (m,rad,rad,m/s,rad,rad)         
!>  MSP_ENUM_POINT_GPS               : <integer,parameter>                   
!>  MSP_ENUM_PERIGEE_APOGEE          : <integer,parameter>                   périgée/apogée (hp,ha,i,pom,gom,M) en (m,m,rad,rad,rad,rad)         
!>  MSP_ENUM_ADAPTE_CNES             : <integer,parameter>                   adapté circulaire équatorial CNES (a,ex,ey,hx,hy,pom+gom+M) en (m,-,-,-,-,rad)
!>                                                         avec ex = e*cos(pom+gom), ey = e*sin(pom+gom)
!>                                                         hx = sin(i)*cos(gom) et hy = sin(i)*sin(gom) 
!>  MSP_ENUM_RENTREE_SPHERIQUE       : <integer,parameter>                   rentrée sphérique (alts,lats,lon,vit,pens,azis)
!>                                                         en (m,rad,rad,m/s,rad,rad)       
!>  MSP_ENUM_GAMMA_VRAI              : <integer,parameter>                   type de repère Gamma vrai          
!>  MSP_ENUM_GAMMA_MOYEN             : <integer,parameter>                   type de repère Gamma moyen         
!>  MSP_ENUM_ECLIPTIQUE              : <integer,parameter>                   type de repère écliptique          
!>  MSP_ENUM_VEIS                    : <integer,parameter>                   type de repère VEIS          
!>  MSP_ENUM_PLANETO_VRAI            : <integer,parameter>                   type de repère Planétocentrique vrai
!>  MSP_ENUM_PLANETO_REF             : <integer,parameter>                   type de repère Planétocentrique de référence
!>  MSP_ENUM_PLANETO_REF_INER        : <integer,parameter>                   type de repère Planétocentrique inertiel
!>  MSP_ENUM_PQ                      : <integer,parameter>                   type de repère PQ          
!>  MSP_ENUM_STATION                 : <integer,parameter>                   type de repère station          
!>  MSP_ENUM_ECHT_TUC                : <integer,parameter>                   échelle de temps TUC        
!>  MSP_ENUM_ECHT_TE                 : <integer,parameter>                   échelle de temps TE        
!>  MSP_ENUM_ECHD_1950               : <integer,parameter>                   date du repère = 1/1/1950 à 0h00 90)
!>  MSP_ENUM_ECHD_2000               : <integer,parameter>                   date du repère = 1/1/2000 à 12h00 (18262.5 JJCNES)         
!>  MSP_ENUM_ECHD_DATE_BUL           : <integer,parameter>                   date du repère fixée par la date du bulletin       
!>  MSP_ENUM_ECHD_DATE_REF           : <integer,parameter>                   date du repère fixée par une date de référence         
!>  MSP_1950                         : <pm_reel,parameter>                   1/1/1950 0h
!>  MSP_JJ2000                       : <pm_reel,parameter>                   1/1/2000 12h
!>  MSP_MJD2000                      : <pm_reel,parameter>                   1/1/2000 0h
!>  MSP_ENUM_NORD                    : <integer,parameter>                   Code d'orientation monture du repère Station (Nord)
!>  MSP_ENUM_SUD                     : <integer,parameter>                   Code monture (Sud)
!>  MSP_ENUM_EST                     : <integer,parameter>                   Code monture (Est)
!>  MSP_ENUM_OUEST                   : <integer,parameter>                   Code monture (Ouest)
!>  MSP_NORD                         : <pm_reel,parameter>                   Orientation monture du repère Station (Nord)
!>  MSP_SUD                          : <pm_reel,parameter>                   Orientation monture du repère Station (Sud)
!>  MSP_EST                          : <pm_reel,parameter>                   Orientation monture du repère Station (Est)
!>  MSP_OUEST                        : <pm_reel,parameter>                   Orientation monture du repère Station (Ouest)
!>  MSP_ENUM_MODPREC_LIESKE          : <integer,parameter>                   modèle de mode de précession LIESKE
!>  MSP_ENUM_MODPREC_UAI94           : <integer,parameter>                   modèle de mode de précession UAI 1994
!>  MSP_ENUM_MODPREC_UAI2000         : <integer,parameter>                   modèle de mode de précession UAI 2000
!>  MSP_ENUM_BARYSS                  : <integer,parameter>                   Barycentre Système Solaire 
!>  MSP_ENUM_BARY_MERCURE            : <integer,parameter>                   Barycentre système Mercure (inutilisé)
!>  MSP_ENUM_BARY_VENUS              : <integer,parameter>                   Barycentre système Venus (inutilisé)
!>  MSP_ENUM_BARY_TERRE              : <integer,parameter>                   Barycentre Terre/Lune
!>  MSP_ENUM_BARY_MARS               : <integer,parameter>                   Barycentre système Mars
!>  MSP_ENUM_BARY_JUPITER            : <integer,parameter>                   Barycentre système Jupiter
!>  MSP_ENUM_BARY_SATURNE            : <integer,parameter>                   Barycentre système Saturne
!>  MSP_ENUM_BARY_URANUS             : <integer,parameter>                   Barycentre système Uranus
!>  MSP_ENUM_BARY_NEPTUNE            : <integer,parameter>                   Barycentre système Neptune
!>  MSP_ENUM_BARY_PLUTON             : <integer,parameter>                   Barycentre système Pluton
!>  MSP_ENUM_SOLEIL                  : <integer,parameter>                   SOLEIL          
!>  MSP_ENUM_MERCURE                 : <integer,parameter>                   planète MERCURE         
!>  MSP_ENUM_VENUS                   : <integer,parameter>                   planète VENUS         
!>  MSP_ENUM_TERRE                   : <integer,parameter>                   planète TERRE         
!>  MSP_ENUM_MARS                    : <integer,parameter>                   planète MARS         
!>  MSP_ENUM_JUPITER                 : <integer,parameter>                   planète JUPITER         
!>  MSP_ENUM_SATURNE                 : <integer,parameter>                   planète SATURNE         
!>  MSP_ENUM_URANUS                  : <integer,parameter>                   planète URANUS         
!>  MSP_ENUM_NEPTUNE                 : <integer,parameter>                   planète NEPTUNE         
!>  MSP_ENUM_PLUTON                  : <integer,parameter>                   planète PLUTON 
!>  MSP_ENUM_LUNE                    : <integer,parameter>                   LUNE 
!>  MSP_ENUM_PHOBOS                  : <integer,parameter>                   PHOBOS (satellite de Mars)
!>  MSP_ENUM_DEIMOS                  : <integer,parameter>                   DEIMOS (satellite de Mars)
!>  MSP_ENUM_IO                      : <integer,parameter>                   IO (satellite de Jupiter)
!>  MSP_ENUM_EUROPA                  : <integer,parameter>                   EUROPA (satellite de Jupiter)
!>  MSP_ENUM_GANYMEDE                : <integer,parameter>                   GANYMEDE (satellite de Jupiter)
!>  MSP_ENUM_CALLISTO                : <integer,parameter>                   CALLISTO (satellite de Jupiter)
!>  MSP_ENUM_MIMAS                   : <integer,parameter>                   MIMAS (satellite de Saturne)
!>  MSP_ENUM_ENCELADUS               : <integer,parameter>                   ENCELADUS (satellite de Saturne)
!>  MSP_ENUM_THETYS                  : <integer,parameter>                   THETYS (satellite de Saturne)
!>  MSP_ENUM_DIONE                   : <integer,parameter>                   DIONE (satellite de Saturne)
!>  MSP_ENUM_RHEA                    : <integer,parameter>                   RHEA (satellite de Saturne)
!>  MSP_ENUM_TITAN                   : <integer,parameter>                   TITAN (satellite de Saturne)
!>  MSP_ENUM_HYPERION                : <integer,parameter>                   HYPERION (satellite de Saturne)
!>  MSP_ENUM_IAPETUS                 : <integer,parameter>                   IAPETUS (satellite de Saturne)
!>  MSP_ENUM_WIRTANEN                : <integer,parameter>                   WIRTANEN (comète)
!>  MSP_ENUM_HALEBOPP                : <integer,parameter>                   HALE-BOPP (comète)
!>  MSP_ENUM_TEMPL1                  : <integer,parameter>                   TEMPLE-1 (comète)
!>  MSP_ENUM_CERES                   : <integer,parameter>                   CERES (Astéroïde)
!>  MSP_ENUM_PALLES                  : <integer,parameter>                   PALLAS (Astéroïde)
!>  MSP_ENUM_JUNON                   : <integer,parameter>                   JUNON (Astéroïde)
!>  MSP_ENUM_VESTA                   : <integer,parameter>                   VESTA (Astéroïde)
!>  MSP_ENUM_CHALDAEA                : <integer,parameter>                   Chaldaea (Astéroïde)
!>  MSP_ENUM_ORPHEUS                 : <integer,parameter>                   Orphéus (Astéroïde)
!>  MSP_EPSILON_DATE                 : <pm_reel,parameter>                   Epsilon pour la comparaison des dates
!>  MSP_EPSILON_APLA                 : <pm_reel,parameter>                   Epsilon pour les comparaison des réels
!>  MSP_OBLIQUITE                    : <pm_reel,parameter>                   Obliquite à J2000 (rad)
!#V
!>  MSP_1950jour                     : <integer,parameter,private>           1/1/1950
!>  MSP_1950sec                      : <pm_reel,parameter,private>           0h
!>  MSP_2000jour                     : <integer,parameter,private>           1/1/2000
!>  MSP_2000sec                      : <pm_reel,parameter,private>           12h
!>  MSP_VITROT                       : <pm_reel,DIM=(10),parameter,private>  tableau contenant les vitesses de rotation
!>                                                                             des 9 planètes et du Soleil
!>  MSP_APLA_TERRE                   : <pm_reel,parameter,private>           applatissement de la terre
!>  MSP_REQUA_TERRE                  : <pm_reel,parameter,private>           rayon equatorial terrestre
!>  MSP_poleu2000                    : <pm_reel,DIM=(9),parameter,private>   tableau contenant l'angle u du pôle à J2000
!>                                                                             des 9 planètes (rad)
!>  MSP_polev2000                    : <pm_reel,DIM=(9),parameter,private>   tableau contenant l'angle v du pôle à J2000
!>                                                                             des 9 planètes (rad)
!#
!$Common
!
!$Routines
!- MSP_creer_bulletin
!- MSP_create_orbit
!- MSP_creer_typrep
!- MSP_create_reference_frame
!- MSP_create_coord_data
!- MSP_get_orbit_data
!- MSP_set_orbit_data
!- MSP_get_reference_frame_data
!- MSP_set_reference_frame_data
!- MSP_get_coord_data
!- MSP_set_coord_data
!- MSP_convert_orbit_data
!- MSP_get_frame_type
!- MSP_get_orbit_type
!- MSP_get_date_origin_type
!- MSP_get_time_scale_type
!- MSP_display_orbit_data
!- MSP_write_orbit
!- MSP_write_frame
!- MSP_conv_typrep
!- MSP_date_etutc
!- MSP_modifier_bulletin
!- MSP_modifier_typbul
!- MSP_modifier_typrep
!- MSP_calcul_ecart_echt
!- MSP_consulter_bulletin
!- MSP_consulter_typbul
!- MSP_consulter_typrep
!- MSP_afficher_bulletin
!- MSP_ecrire_bulletin
!- MSP_ecrire_typrep
!- MSP_init_structure_topo
!#V
!- MSP_date_etutc_frac
!- MSP_date_etutc_jjsec
!- MSP_conv_typrep_jjsec
!- MSP_conv_typrep_frac
!#
!
!$Fonctions
!- MSP_creer_typrep_param
!- MSP_creer_typrep_mad
!- MSP_creer_typbul
!- MSP_creer_bulletin_param
!- MSP_creer_bulletin_madona
!- MSP_convertir_bulletin
!- MSP_type_repere
!- MSP_type_bulletin
!- MSP_precession
!- MSP_type_echelle_date
!- MSP_type_echelle_temps
!- MSP_argument_latitude
!#V
!- vitesse_rotation
!- planete_mspro
!- direction_mspro
!- direction_gslib
!- verif_typrep_planete
!- verif_typrep_date
!- comparer_repere
!- comparer_coord
!- comparer_bulletin
!#
!
!$Include
!
!$Module
!#V
!- MSLIB
!- MSPRO
!- MSP_GESTION_ERREUR
!- MSP_ACCES
!- MSP_MATH
!- MSP_MECASPA_DEF
!- eph_constantes
!#
!
!$Interface
!> msp_date_etutc :                MSP_date_etutc_frac, MSP_date_etutc_jjsec
!> msp_set_coord_data :            MSP_modifier_typbul
!> msp_set_reference_frame_data :  MSP_modifier_typrep
!> msp_get_frame_type :            MSP_type_repere
!> msp_get_reference_frame_data :  MSP_consulter_typrep
!> msp_get_time_scale_type :       MSP_type_echelle_temps
!> msp_set_orbit_data :            MSP_modifier_bulletin
!> msp_display_orbit_data :        MSP_afficher_bulletin
!> msp_get_orbit_type :            MSP_type_bulletin
!> msp_create_orbit :              MSP_creer_bulletin_param, 
!                                  MSP_creer_bulletin_madona
!> msp_get_orbit_data :            MSP_consulter_bulletin
!> msp_creer_bulletin :            MSP_creer_bulletin_param, 
!                                  MSP_creer_bulletin_madona
!> msp_write_frame :               MSP_ecrire_typrep
!> msp_creer_typrep :              MSP_creer_typrep_param, MSP_creer_typrep_mad
!> msp_create_reference_frame :    MSP_creer_typrep_param, MSP_creer_typrep_mad
!> msp_conv_typrep :               MSP_conv_typrep_frac, MSP_conv_typrep_jjsec
!> msp_get_date_origin_type :      MSP_type_echelle_date
!> msp_convert_orbit_data :        MSP_convertir_bulletin
!> msp_get_coord_data :            MSP_consulter_typbul
!> msp_create_coord_data :         MSP_creer_typbul
!> msp_write_orbit :               MSP_ecrire_bulletin
!#V
!#
!
!$Remarques
!
!$Mots-cles
!  BULLETIN
!
!$Voir-Aussi
!#V
!.  vitesse_rotation planete_mspro direction_mspro direction_gslib verif_typrep_planete verif_typrep_date
!.  MSP_creer_bulletin_param MSP_creer_bulletin_madona comparer_repere comparer_coord comparer_bulletin
!.  MSP_date_etutc_frac MSP_date_etutc_jjsec MSP_conv_typrep_jjsec MSP_conv_typrep_frac
!#
!.  MSP_creer_typrep_param MSP_creer_typrep_mad MSP_creer_typbul MSP_convertir_bulletin MSP_type_repere
!.  MSP_type_bulletin MSP_precession MSP_type_echelle_date MSP_type_echelle_temps MSP_argument_latitude
!.  MSP_creer_bulletin MSP_create_orbit MSP_creer_typrep MSP_create_reference_frame MSP_create_coord_data
!.  MSP_get_orbit_data MSP_set_orbit_data MSP_get_reference_frame_data MSP_set_reference_frame_data
!.  MSP_get_coord_data MSP_set_coord_data MSP_convert_orbit_data MSP_get_frame_type MSP_get_orbit_type
!.  MSP_get_date_origin_type MSP_get_time_scale_type MSP_display_orbit_data MSP_write_orbit
!.  MSP_write_frame MSP_conv_typrep MSP_date_etutc MSP_modifier_bulletin MSP_modifier_typbul
!.  MSP_modifier_typrep MSP_calcul_ecart_echt MSP_consulter_bulletin MSP_consulter_typbul
!.  MSP_consulter_typrep MSP_afficher_bulletin MSP_ecrire_bulletin MSP_ecrire_typrep MSP_init_structure_topo
!
!$<>
!******************************************************************************

   use MSLIB
   use MSPRO
   use MSP_GESTION_ERREUR
   use MSP_ACCES
   use MSP_MATH
   use MSP_MECASPA_DEF
   use eph_constantes

   implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: MSP_BULLETIN_DEF.F90 365 2013-02-18 12:36:19Z aadt $'


   type MSP_TYPREP

      private
      integer            :: typrep
      integer            :: cle_date
      type(tm_jour_sec)  :: date_ref
      integer            :: origdat
      integer            :: planete
      integer            :: corcen
      integer            :: ech_temps_rep
      real(kind=pm_reel) :: obli
      real(kind=pm_reel) :: vrot
      real(kind=pm_reel) :: lonref
      type(tm_pole_uv)   :: pole
      type(tm_def_topo)  :: station

   end type MSP_TYPREP

   type MSP_TYPBUL

      private
      integer            :: iorb
      real(kind=pm_reel) :: param(6)      
      real(kind=pm_reel) :: mu      
      real(kind=pm_reel) :: requa      
      real(kind=pm_reel) :: apla      

   end type MSP_TYPBUL

   type MSP_BULLETIN

      private
      type(tm_jour_sec)  :: datbul
      integer            :: ech_temps_bul
      type(MSP_TYPBUL)   :: coord
      type(MSP_TYPREP)   :: repere
      integer            :: modprec

   end type MSP_BULLETIN


   ! VARIABLES GLOBALES:

   ! Type de paramètres du bulletin
   ! ces constantes sont OBSOLETES, elles sont remplacées par les codes MSPRO
   ! on les garde pour la compabilité ascendante
   integer, parameter :: MSP_ENUM_KEPLERIEN             = pm_kep
   integer, parameter :: MSP_ENUM_ADAPTE_CIRCULAIRE     = pm_cir
   integer, parameter :: MSP_ENUM_ADAPTE_EQUATORIAL     = pm_equa
   integer, parameter :: MSP_ENUM_ADAPTE_CIRC_EQ        = pm_cir_equa
   integer, parameter :: MSP_ENUM_CARTESIENS            = pm_car
   integer, parameter :: MSP_ENUM_SPHERIQUE             = pm_geoc
   integer, parameter :: MSP_ENUM_RENTREE_ELLIPSOIDIQUE = pm_geod_meca_vol
   integer, parameter :: MSP_ENUM_POINT_GPS             = pm_geod_gps_ard
   integer, parameter :: MSP_ENUM_PERIGEE_APOGEE        = pm_hpha
   integer, parameter :: MSP_ENUM_ADAPTE_CNES           = pm_car + 1
   integer, parameter :: MSP_ENUM_RENTREE_SPHERIQUE     = MSP_ENUM_ADAPTE_CNES + 1
         
   ! Type de repère
   ! ces constantes sont OBSOLETES, elles sont remplacées par les codes MSPRO
   ! on les garde pour la compabilité ascendante
   integer, parameter :: MSP_ENUM_GAMMA_VRAI       = pm_equa_vrai
   integer, parameter :: MSP_ENUM_GAMMA_MOYEN      = pm_equa_moy
   integer, parameter :: MSP_ENUM_ECLIPTIQUE       = pm_ecli_moy
   integer, parameter :: MSP_ENUM_VEIS             = pm_veis
   integer, parameter :: MSP_ENUM_PLANETO_VRAI     = pm_planeto_vrai
   integer, parameter :: MSP_ENUM_PLANETO_REF      = pm_planeto_ref
   integer, parameter :: MSP_ENUM_PLANETO_REF_INER = pm_planeto_ref_iner
   integer, parameter :: MSP_ENUM_PQ               = pm_equa_uai
   integer, parameter :: MSP_ENUM_STATION          = pm_topo

   ! Echelle de temps
   ! ces constantes sont OBSOLETES, elles sont remplacées par les codes MSPRO
   ! on les garde pour la compabilité ascendante
   integer, parameter :: MSP_ENUM_ECHT_TUC = pm_TUC
   integer, parameter :: MSP_ENUM_ECHT_TE  = pm_TE

   ! Clé pour la date de référence
   ! ces constantes sont OBSOLETES, elles sont remplacées par les codes MSPRO
   ! on les garde pour la compabilité ascendante
   integer, parameter :: MSP_ENUM_ECHD_1950           = pm_1janvier1950_00h00
   integer, parameter :: MSP_ENUM_ECHD_2000           = pm_1janvier2000_12h00    
   integer, parameter :: MSP_ENUM_ECHD_DATE_BUL       = pm_autre_date
   integer, parameter :: MSP_ENUM_ECHD_DATE_REF       = pm_autre_date + 1

   ! Valeurs pour les dates de référence des repères 
   real(kind=pm_reel), parameter :: MSP_1950 = 0._pm_reel
   real(kind=pm_reel), parameter :: MSP_JJ2000  = 18262.5_pm_reel
   
   ! Date de référence J2000 (MJD2000)
   real(kind=pm_reel), parameter :: MSP_MJD2000 = 18262._pm_reel

   ! Conversion J50 / MJD2000
   integer, parameter,private :: MSP_1950jour = 0 
   real(kind=pm_reel), parameter,private :: MSP_1950sec = 0.
   integer, parameter,private :: MSP_2000jour   = 18262
   real(kind=pm_reel), parameter,private :: MSP_2000sec = 0.

   ! code GS_LIB de direction du repere station
   integer, parameter :: MSP_ENUM_NORD = 1
   integer, parameter :: MSP_ENUM_SUD = 2
   integer, parameter :: MSP_ENUM_EST = 3
   integer, parameter :: MSP_ENUM_OUEST = 4

   ! Valeur mspro de direction du repere station
   real(kind=pm_reel), parameter :: MSP_NORD  = 0._pm_reel
   real(kind=pm_reel), parameter :: MSP_SUD   = pm_pi
   real(kind=pm_reel), parameter :: MSP_EST   =  pm_pi_sur2
   real(kind=pm_reel), parameter :: MSP_OUEST = -pm_pi_sur2

   ! Mode de precession
   ! ces constantes sont OBSOLETES, elles sont remplacées par les codes MSPRO
   ! on les garde pour la compabilité ascendante
   integer, parameter :: MSP_ENUM_MODPREC_LIESKE  = pm_lieske_wahr_aoki
   integer, parameter :: MSP_ENUM_MODPREC_UAI94   = pm_uai1994
   integer, parameter :: MSP_ENUM_MODPREC_UAI2000 = pm_uai2000

   ! Corps celestes (codification LIBEPHEM)
   ! ces constantes sont OBSOLETES, elles sont remplacées par les codes COMPAS
   ! on les garde pour la compabilité ascendante
   integer, parameter :: MSP_ENUM_BARYSS    = 0
   integer, parameter :: MSP_ENUM_BARY_MERCURE  = 1
   integer, parameter :: MSP_ENUM_BARY_VENUS    = 2
   integer, parameter :: MSP_ENUM_BARY_TERRE    = 3
   integer, parameter :: MSP_ENUM_BARY_MARS     = 4
   integer, parameter :: MSP_ENUM_BARY_JUPITER  = 5
   integer, parameter :: MSP_ENUM_BARY_SATURNE  = 6
   integer, parameter :: MSP_ENUM_BARY_URANUS   = 7
   integer, parameter :: MSP_ENUM_BARY_NEPTUNE  = 8
   integer, parameter :: MSP_ENUM_BARY_PLUTON   = 9
   integer, parameter :: MSP_ENUM_SOLEIL        = 10

   integer, parameter :: MSP_ENUM_MERCURE   = 199
   integer, parameter :: MSP_ENUM_VENUS     = 299
   integer, parameter :: MSP_ENUM_TERRE     = 399
   integer, parameter :: MSP_ENUM_MARS      = 499
   integer, parameter :: MSP_ENUM_JUPITER   = 599
   integer, parameter :: MSP_ENUM_SATURNE   = 699
   integer, parameter :: MSP_ENUM_URANUS    = 799
   integer, parameter :: MSP_ENUM_NEPTUNE   = 899
   integer, parameter :: MSP_ENUM_PLUTON    = 999

   ! satellite de la Terre
   integer, parameter :: MSP_ENUM_LUNE      = 301

   ! satellite de Mars
   integer, parameter :: MSP_ENUM_PHOBOS    = 401
   integer, parameter :: MSP_ENUM_DEIMOS    = 402
   
   ! satellites de Jupiter
   integer,parameter:: MSP_ENUM_IO         = 501
   integer,parameter:: MSP_ENUM_EUROPA     = 502
   integer,parameter:: MSP_ENUM_GANYMEDE   = 503
   integer,parameter:: MSP_ENUM_CALLISTO   = 504
   
   ! satellites de Saturne
   integer,parameter::MSP_ENUM_MIMAS      = 601
   integer,parameter::MSP_ENUM_ENCELADUS  = 603
   integer,parameter::MSP_ENUM_THETYS     = 603
   integer,parameter::MSP_ENUM_DIONE      = 604
   integer,parameter::MSP_ENUM_RHEA       = 605
   integer,parameter::MSP_ENUM_TITAN      = 606
   integer,parameter::MSP_ENUM_HYPERION   = 607
   integer,parameter::MSP_ENUM_IAPETUS    = 608
   
   ! comètes
   integer,parameter::MSP_ENUM_WIRTANEN   = 1000109
   integer,parameter::MSP_ENUM_HALEBOPP   = 1000132
   integer,parameter::MSP_ENUM_TEMPL1     = 1000093
   
   ! astéroïdes
   integer,parameter::MSP_ENUM_CERES      = 2000001
   integer,parameter::MSP_ENUM_PALLES     = 2000002
   integer,parameter::MSP_ENUM_JUNON      = 2000003
   integer,parameter::MSP_ENUM_VESTA      = 2000004
   integer,parameter::MSP_ENUM_CHALDAEA   = 2000313
   integer,parameter::MSP_ENUM_ORPHEUS    = 2003316
   
   
   real(kind=pm_reel), parameter, dimension(10), private :: MSP_VITROT = &
      (/ .00000124001249730207_pm_reel, &
        -.00000029924494208699_pm_reel, &
         .00007292115373193760_pm_reel, &
         .00007088218081252046_pm_reel, &
         .00017585323445764879_pm_reel, &
         .00016378499018487370_pm_reel, &
        -.00010123719558981527_pm_reel, &
         .00010833825276957787_pm_reel, &
        -.00001138550983027745_pm_reel, &
         .00000286532965763744_pm_reel /)

   ! On garde ces constantes qui peuvent être exploitées par les utilisateurs
   real(kind=pm_reel), parameter, private :: MSP_APLA_TERRE = 1/298.257_pm_reel
   real(kind=pm_reel), parameter, private :: MSP_REQUA_TERRE = 6378.137e+03_pm_reel  ! (m)

   ! valeurs COMPAS (lues une seule fois)
   real(kind=pm_reel), private, save :: MSP_cps_APLA_TERRE = 0_PM_REEL
   real(kind=pm_reel), private, save :: MSP_cps_REQUA_TERRE = 0_PM_REEL ! (m)
   real(kind=pm_reel), private, save :: MSP_cps_MU_TERRE = 0_PM_REEL ! (m)
   logical, save , private :: msp_cps_apla_lu = .false.
   logical, save , private :: msp_cps_requa_lu = .false.
   logical, save , private :: msp_cps_mu_lu = .false.

   ! Poles a J2000 pour les planetes, modele UAI 1994
   real(kind=pm_reel), parameter, dimension(9), private :: MSP_poleu2000 = &
      (/ 4.904549731029265_PM_REEL, &
         4.760560067739733_PM_REEL, &
         0._PM_REEL, &
         5.544579421028105_PM_REEL,  &
         4.678355059970801_PM_REEL, &
         0.7084116900919784_PM_REEL, &
         4.4909241515991284_PM_REEL, &
         5.224359307135805_PM_REEL,  &
         5.46322962459265_PM_REEL    /)

   real(kind=pm_reel), parameter, dimension(9), private :: MSP_polev2000 = &
      (/ 1.0725048253505156_PM_REEL,  &
         1.1721631256393916_PM_REEL, &
         0._PM_REEL, &
         0.9230348282097212_PM_REEL, &
         1.125562834611143_PM_REEL, &
         1.457995697238503_PM_REEL, &
        -0.2648537139901395_PM_REEL, &
         0.7496251798448437_PM_REEL, &
         0.15865042900628456_PM_REEL /)

   ! Epsilon utilisé pour la comparaison de deux dates (en s)
   real(kind=pm_reel), parameter :: MSP_EPSILON_DATE = 1.e-6_pm_reel    ! (s)

   ! Epsilon utilisé pour la comparaison de deux PM_REEL (en general)
   real(kind=pm_reel), parameter :: MSP_EPSILON_APLA = 1.e-20_pm_reel
   
   ! Epsilon utilisé pour la comparaison de deux PM_REEL (uniquement pour pression rad solaire)
   real(kind=pm_reel), parameter :: MSP_EPSILON_PRS = 1.e-15_pm_reel

   ! A remplacer par pm_obliquite quand disponible dans la MSPRO
   real(kind=pm_reel), parameter :: MSP_OBLIQUITE   = 0.40909261307192995_pm_reel ! (rad/s)

   interface MSP_creer_bulletin

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_creer_bulletin
!
!$Resume
!  Cette routine permet de créer un bulletin 
!
!$Description
!  Cette routine permet de créer un bulletin, 2 cas d'utilisation,
!  en donnant les différents arguments ou en donnant un accès
!  MADONA dans lequel lire une structure (nom de la structure donné en paramètre)
!
!$Acces
!  PUBLIC
!
!$Usage
!  bulletin = MSP_creer_bulletin(datbul,[iorb],[param],[typrep],[repere],[coord],&
!.            ech_temps_bul,datbul_js,lonref,date_ref,date_refjs,ech_temps_rep,lat,lon,&
!.            alt,direction,planete,corcen,cle_date,modprec,mu,requa,apla,requa_r,apla_r,&
!.            vrot,rep,pole_u,pole_v,obli,origdat)
!.    real(kind=pm_reel) :: datbul
!.    type(MSP_TYPREP) :: repere
!.    type(MSP_TYPBUL) :: coord
!.    integer :: iorb
!.    real(kind=pm_reel) :: param(6)
!.    integer :: typrep
!.    integer :: ech_temps_bul
!.    real(kind=pm_reel) :: lonref,date_ref,lat,lon,alt
!.    real(kind=pm_reel) :: direction
!.    integer :: planete,corcen,cle_date,modprec,ech_temps_rep
!.    real(kind=pm_reel) :: mu,requa,apla,requa_r,apla_r,vrot
!.    real(kind=pm_reel) :: pole_u, pole_v, obli
!.    real(kind=pm_reel), dimension(10) :: rep
!.    type(tm_jour_sec) :: datbul_js, date_refjs
!.    integer :: origdat
!.    type(MSP_BULLETIN) :: bulletin
!
!  bulletin = MSP_creer_bulletin(subr_lect, nom_dom, [nacc], [section], [type_section])
!.    type(MSP_BULLETIN) :: bulletin
!.    external subr_lect
!.    character(LEN=*) :: nom_dom
!.    integer :: nacc
!.    integer, dimension(:) :: type_section
!.    character(LEN=MSP_LONG_CHAINE), dimension(:) :: section
!
!$Arguments
!>E     datbul         :<pm_reel>            date du bulletin [JJ]
!>[E]   iorb           :<integer>            type de coordonnées (pm_kep, pm_cir,
!>                                        pm_equa, pm_cir_equa, pm_car,
!>                                        pm_hpha, pm_geoc, MSP_ENUM_TWOLINES,
!>                                        MSP_ENUM_ADAPTE_CNES, MSP_ENUM_RENTREE_SPHERIQUE, pm_geod_meca_vol,
!>                                        pm_geod_gps_ard)
!>[E]   param          :<pm_reel,DIM=(6)>    coordonnées (cf. définition de iorb)
!>[E]   typrep         :<integer>            type de repère (pm_equa_vrai, pm_equa_moyen, pm_ecli_moy,
!>                                        pm_veis, pm_planeto_ref, pm_planeto_ref_iner, pm_planeto_vrai,
!>                                        pm_equa_uai, pm_topo)
!>[E]   repere         :<MSP_TYPREP>         Structure repère (prioritaire sur les autres arguements de cette sous-structure, voir MSP_creer_typrep)   
!>[E]   coord          :<MSP_TYPBUL>         Structure type de bulletin (prioritaire sur les autres arguements de cette sous-structure, voir MSP_creer_typbul)   
!>[E]   ech_temps_bul  :<integer>            échelle de temps de la date du bulletin (pm_TUC, pm_TE)
!>[E]   datbul_js      :<tm_jour_sec>        date du bulletin jours/secondes 
!>[E]   lonref         :<pm_reel>            longitude de référence
!>[E]   date_ref       :<pm_reel>            date de référence [JJ]
!>[E]   date_refjs     :<tm_jour_sec>        date de référence jours/secondes
!>[E]   ech_temps_rep  :<integer>            échelle de temps de la date de référence du repère dans le cas où une date de
!>                                           référence est nécessaire cad
!>.                                          si typrep=pm_planeto_ref, pm_planeto_ref_iner, pm_planeto_vrai et cle_date=(pm_autre_date+1)
!>[E]   lat            :<pm_reel>            latitude station [rad]
!>[E]   lon            :<pm_reel>            longitude station  [rad]
!>[E]   alt            :<pm_reel>            altitude station  [m]
!>[E]   direction      :<pm_reel>            Direction de la monture de la station:
!>.                                       MSP_ENUM_NORD  -> monture au Nord
!>.                                       MSP_ENUM_SUD   -> monture au Sud
!>.                                       MSP_ENUM_EST   -> monture  Est
!>.                                       MSP_ENUM_OUEST -> monture  Ouest
!>[E]   planete        :<integer>            planète (eph_mercure,eph_venus,eph_terre,eph_mars,
!>                                     eph_jupiter,eph_saturne,eph_uranus,eph_neptune,eph_pluton)
!>                                     [par défaut eph_terre]
!>[E]   corcen         :<integer>            corps central (eph_mercure,eph_venus,eph_terre,eph_mars,
!>                                     eph_jupiter,eph_saturne,eph_uranus,eph_neptune,eph_pluton,
!>                                     eph_soleil) [par défaut eph_terre]
!>[E]   cle_date       :<integer>            type de date utilisée dans pour l'origine des dates (pm_1janvier1950_00h00, pm_1janvier2000_12h00,
!>                                      pm_autre_date, MSP_ENUM_ECHD_DATE_REF)
!>                                      dans le cas des repères PQ et STATION la
!>                                     valeur par défaut est pm_autre_date
!>[E]   modprec        :<integer>            mode de précession (pm_lieske_wahr_aoki, pm_uai1994)
!>                                           [par défaut pm_lieske_wahr_aoki_]
!>[E]   mu             :<pm_reel>            terme central du potentiel (m^3/s^2) [par défaut valeur lue dans COMPAS]
!>[E]   requa          :<pm_reel>            rayon terrestre à l'équateur [m] [par défaut valeur lue dans COMPAS]
!>[E]   apla           :<pm_reel>            aplatissement terrestre [par défaut valeur lue dans COMPAS]
!>[E]   requa_r        :<pm_reel>            rayon terrestre à l'équateur pour les coordonnées station  [m]
!>[E]   apla_r         :<pm_reel>            aplatissement terrestre pour les coordonnées station 
!>[E]   vrot           :<pm_reel>            vitesse de rotation [rad/s] [par défaut MSP_VITROT(eph_terre)]
!>[E]   rep            :<pm_reel,DIM=(10)>   tableau utilisé pour psimu
!>[E]   pole_u         :<pm_reel>            angle u du pole vrai
!>[E]   pole_v         :<pm_reel>            angle v du pole vrai
!>[E]   obli           :<pm_reel>            obliquité
!>[E]   origdat        :<integer>            origine des dates (0=J50, 1=MJD2000)
!>S     bulletin       :<MSP_BULLETIN>       type dérivé contenant les informations liées au bulletin
!.
!.
!>E/S   subr_lect     :<external>                      Routine externe servant a lire dans le moyen d'acces nacc
!                                          Cette routine peut etre read_GS_BULLETIN_IP si le bulletin 
!                                          contenu dans le moyen d'acces possede un format compatible avec l'objet GS_LIB.
!>E     nom_dom       :<LEN=*>                         nom du domaine de traduction MADONA
!>[E]   nacc          :<integer>                       Numero du moyen d'acces MADONA où lire les informations de bulletin
!>[E]   section       :<LEN=MSP_LONG_CHAINE,DIM=(:)>   Sections à sélectionner pour atteindre la structure bulletin
!>[E]   type_section  :<integer,DIM=(:)>               Type des sections a selectionner 
!>S     bulletin      :<MSP_BULLETIN>                  Structure bulletin telle que définie dans MSP_BULLETIN_DEF.F90
!
!$Procedures
!#V
!- MSP_creer_bulletin_param
!- MSP_creer_bulletin_madona
!#
!
!$Remarques
!
!$Mots-cles
! BULLETIN CREER MADONA
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_creer_bulletin_param, MSP_creer_bulletin_madona
   end interface


   interface MSP_create_orbit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_create_orbit
!
!$Resume
!  Creation of an orbit structure
!
!$Description
!  Creation of an orbit structure
!
!$Acces
!  PUBLIC
!
!$Usage
!  bulletin = MSP_create_orbit(datbul,[iorb],[param],[typrep],[repere],[coord],&
!.            ech_temps_bul,datbul_js,lonref,date_ref,date_refjs,ech_temps_rep,lat,lon,&
!.            alt,direction,planete,corcen,cle_date,modprec,mu,requa,apla,requa_r,apla_r,&
!.            vrot,rep,pole_u,pole_v,obli,origdat)
!.    real(kind=pm_reel) :: datbul
!.    type(MSP_TYPREP) :: repere
!.    type(MSP_TYPBUL) :: coord
!.    integer :: iorb
!.    real(kind=pm_reel) :: param(6)
!.    integer :: typrep
!.    integer :: ech_temps_bul
!.    real(kind=pm_reel) :: lonref,date_ref,lat,lon,alt
!.    real(kind=pm_reel) :: direction
!.    integer :: planete,corcen,cle_date,modprec,ech_temps_rep
!.    real(kind=pm_reel) :: mu,requa,apla,requa_r,apla_r,vrot
!.    real(kind=pm_reel) :: pole_u, pole_v, obli
!.    real(kind=pm_reel), dimension(10) :: rep
!.    type(tm_jour_sec) :: datbul_js, date_refjs
!.    integer :: origdat
!.    type(MSP_BULLETIN) :: bulletin
!
!  bulletin = MSP_create_orbit(subr_lect, nom_dom, [nacc], [section], [type_section])
!.    type(MSP_BULLETIN) :: bulletin
!.    external subr_lect
!.    character(LEN=*) :: nom_dom
!.    integer :: nacc
!.    integer, dimension(:) :: type_section
!.    character(LEN=MSP_LONG_CHAINE), dimension(:) :: section
!
!$Procedures
!#V
!- MSP_creer_bulletin_param
!- MSP_creer_bulletin_madona
!#
!
!$Remarques
!
!$Mots-cles
! BULLETIN CREER MADONA
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_creer_bulletin_param, MSP_creer_bulletin_madona
   end interface

   interface MSP_creer_typrep

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_creer_typrep
!
!$Resume
!  Cette routine permet de créer une structure repère
!
!$Description
!  Cette routine permet de créer une structure repère
!
!$Acces
!  PUBLIC
!
!$Usage
!  repere = MSP_creer_typrep([typrep], [cle_date], [planete], [date_ref],[date_refjs], &
!.            ech_temps_rep,lonref,lat, lon,alt,direction, corcen,requa_r,apla_r,vrot, &
!.            rep, pole_u, pole_v, obli,origdat)
!.    type (MSP_TYPREP) :: repere
!.    integer :: typrep
!.    integer :: cle_date, planete, corcen, ech_temps_rep
!.    real(kind=pm_reel) :: date_ref, lonref, lat,lon,alt,direction
!.    real(kind=pm_reel) :: requa_r,apla_r,vrot, pole_u,pole_v,obli
!.    type(tm_jour_sec) :: date_refjs
!.    real(kind=pm_reel), dimension(10) :: rep
!.    integer :: origdat
!
!  repere = MSP_creer_typrep(subr_lect, nom_dom, [nacc], [section], [type_section])
!.    type (MSP_TYPREP) :: repere
!.    external subr_lect
!.    character(LEN=*) :: nom_dom
!.    integer :: nacc
!.    integer, dimension(:) :: type_section
!.    character(LEN=MSP_LONG_CHAINE), dimension(:) :: section
!
!$Procedures
!- MSP_creer_typrep_param
!- MSP_creer_typrep_mad
!
!$Remarques
!
!$Mots-cles
! REPERE CREER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_creer_typrep_param, MSP_creer_typrep_mad
   end interface

   interface MSP_create_reference_frame

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_create_reference_frame
!
!$Resume
!  Creation of a reference frame data structure
!
!$Description
!  Creation of a reference frame data structure
!
!$Acces
!  PUBLIC
!
!$Usage
!  repere = MSP_create_reference_frame([typrep], [cle_date], [planete], [date_ref],[date_refjs], &
!.            ech_temps_rep,lonref,lat, lon,alt,direction, corcen,requa_r,apla_r,vrot, &
!.            rep, pole_u, pole_v, obli,origdat)
!.    type (MSP_TYPREP) :: repere
!.    integer :: typrep
!.    integer :: cle_date, planete, corcen, ech_temps_rep
!.    real(kind=pm_reel) :: date_ref, lonref, lat,lon,alt,direction
!.    real(kind=pm_reel) :: requa_r,apla_r,vrot, pole_u,pole_v,obli
!.    type(tm_jour_sec) :: date_refjs
!.    real(kind=pm_reel), dimension(10) :: rep
!.    integer :: origdat
!
!  repere = MSP_create_reference_frame(subr_lect, nom_dom, [nacc], [section], [type_section])
!.    type (MSP_TYPREP) :: repere
!.    external subr_lect
!.    character(LEN=*) :: nom_dom
!.    integer :: nacc
!.    integer, dimension(:) :: type_section
!.    character(LEN=MSP_LONG_CHAINE), dimension(:) :: section
!
!$Procedures
!- MSP_creer_typrep_param
!- MSP_creer_typrep_mad
!
!$Remarques
!
!$Mots-cles
! REPERE CREER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_creer_typrep_param, MSP_creer_typrep_mad
   end interface

   interface MSP_create_coord_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_create_coord_data
!
!$Resume
!  Creation of a coordinates data structure in relation with an orbit
!
!$Description
!  Creation of a coordinates data structure in relation with an orbit
!
!$Acces
!  PUBLIC
!
!$Usage
!  coord = MSP_create_coord_data (param, iorb, [mu], [requa], [apla])
!.    type(MSP_TYPBUL) :: coord
!.    integer :: iorb
!.    real(kind=pm_reel) :: param(6)
!.    real(kind=pm_reel) :: mu,requa,apla
!
!$Procedures
!- MSP_creer_typbul
!
!$Remarques
!
!$Mots-cles
! COORDONNEES CREER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_creer_typbul
   end interface


   interface MSP_get_orbit_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_get_orbit_data
!
!$Resume
!  Get information on an orbit structure
!
!$Description
!  Get information on an orbit structure
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_get_orbit_data(bulletin, [repere], [coord], &
!.           datbul, datbul_js, iorb, param,  typrep, rep, &
!.           ech_temps_bul, lonref, date_ref, date_refjs, lat, lon, alt, direction,   &
!.           planete, corcen, cle_date, ech_temps_rep, modprec, &
!.           mu, requa, apla, requa_r, apla_r, vitrot, pole_u, pole_v, obli,origdat)
!.    type(MSP_BULLETIN) :: bulletin
!.    type(MSP_TYPREP) :: repere
!.    type(MSP_TYPBUL) :: coord
!.    real(KIND=PM_REEL) :: datbul
!.    integer :: iorb 
!.    real(KIND=PM_REEL), dimension(6) :: param
!.    integer :: typrep
!.    real(KIND=PM_REEL), dimension(10) :: rep
!.    real(KIND=PM_REEL) :: lonref, date_ref, lat, lon, alt, direction
!.    integer :: planete, corcen, cle_date, ech_temps_bul, modprec, ech_temps_rep
!.    real(KIND=PM_REEL) :: mu, requa, apla, requa_r, apla_r, vitrot
!.    real(KIND=PM_REEL) :: pole_u, pole_v, obli
!.    type(tm_jour_sec) :: date_refjs, datbul_js
!.    integer :: origdat
!
!$Procedures
!- MSP_consulter_bulletin
!
!$Remarques
!
!$Mots-cles
! BULLETIN CONSULTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_consulter_bulletin
   end interface

   interface MSP_set_orbit_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_set_orbit_data
!
!$Resume
!  Modify attributes of an orbit structure
!
!$Description
!  Modify attributes of an orbit structure
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_set_orbit_data(bulletin, [coord], [repere], &
!.           datbul, datbul_js, iorb, param, typrep, rep, &
!.           ech_temps_bul, lonref, date_ref, date_refjs, lat, lon, alt, direction,  &
!.           planete, corcen, cle_date, ech_temps_rep, modprec, mu, requa_r, apla_r, requa, apla, vitrot, &
!.           pole_u, pole_v, obli,origdat)
!.    type(MSP_BULLETIN) :: bulletin
!.    type(MSP_TYPREP) :: repere
!.    type(MSP_TYPBUL) :: coord
!.    real(KIND=PM_REEL) :: datbul
!.    integer :: iorb 
!.    real(KIND=PM_REEL), dimension(6) :: param
!.    integer :: typrep
!.    real(KIND=PM_REEL), dimension(10) :: rep
!.    real(KIND=PM_REEL) :: lonref, date_ref, lat, lon, alt, direction
!.    integer :: planete, corcen, cle_date, ech_temps_bul, modprec, ech_temps_rep
!.    real(KIND=PM_REEL) :: mu, requa_r, apla_r
!.    real(KIND=PM_REEL) :: requa, apla, vitrot
!.    real(KIND=PM_REEL) :: pole_u, pole_v, obli
!.    type(tm_jour_sec) :: datbul_js, date_refjs
!.    integer :: origdat
!
!$Procedures
!- MSP_modifier_bulletin
!
!$Remarques
!
!$Mots-cles
! BULLETIN MODIFIER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_modifier_bulletin
   end interface

   interface MSP_get_reference_frame_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_get_reference_frame_data
!
!$Resume
!  Get attributes value of a reference frame structure
!
!$Description
!  Get attributes value of a reference frame structure
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_get_reference_frame_data(repere,  [typrep], [rep], [ech_temps_rep], [lonref], &
!.           date_ref, date_refjs, lat, lon, alt, direction, planete, corcen, &
!.           cle_date, requa_r, apla_r, vitrot, pole_u, pole_v, obli, origdat)
!.    type(MSP_TYPREP) :: repere
!.    integer :: typrep
!.    real(KIND=PM_REEL), dimension(10) :: rep
!.    real(KIND=PM_REEL) :: lonref, date_ref, lat, lon, alt
!.    real(KIND=PM_REEL) :: direction
!.    integer :: planete, corcen, cle_date, ech_temps_rep
!.    real(KIND=PM_REEL) :: requa_r, apla_r, vitrot
!.    real(KIND=PM_REEL) :: pole_u, pole_v, obli
!.    type(tm_jour_sec) :: date_refjs
!.    integer :: origdat
!
!$Procedures
!- MSP_consulter_typrep
!
!$Remarques
!
!$Mots-cles
! REPERE CONSULTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_consulter_typrep
   end interface

   interface MSP_set_reference_frame_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_set_reference_frame_data
!
!$Resume
!  Modify attributes value of a reference frame structure
!
!$Description
!  Modify attributes value of a reference frame structure
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_set_reference_frame_data(repere, [typrep], [cle_date], [planete], &
!.            date_ref,date_refjs, ech_temps_rep,lonref,lat, lon,alt,direction,&
!.            corcen,requa_r,apla_r,vrot, rep, pole_u, pole_v, obli,origdat)
!.    type(MSP_TYPREP) :: repere
!.    integer :: typrep
!.    integer :: cle_date, planete, corcen, ech_temps_rep
!.    real(kind=pm_reel) :: date_ref, lonref, lat,lon,alt,direction
!.    real(kind=pm_reel) :: requa_r,apla_r,vrot, pole_u,pole_v,obli
!.    real(kind=pm_reel), dimension(10) :: rep
!.    type(tm_jour_sec) :: date_refjs
!.    integer :: origdat
!
!$Procedures
!- MSP_modifier_typrep
!
!$Remarques
!
!$Mots-cles
! REPERE MODIFIER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_modifier_typrep
   end interface

   interface MSP_get_coord_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_get_coord_data
!
!$Resume
!  Get attributes value of a coordinates structure
!
!$Description
!  Get attributes value of a coordinates structure
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_get_coord_data(coord, [mu], [apla], [requa], [iorb], [param])
!.    type(MSP_TYPBUL) :: coord
!.    integer :: iorb 
!.    real(KIND=PM_REEL), dimension(6) :: param
!.    real(KIND=PM_REEL) :: mu, requa, apla
!
!$Procedures
!- MSP_consulter_typbul
!
!$Remarques
!
!$Mots-cles
! COORDONNEES CONSULTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_consulter_typbul
   end interface

   interface MSP_set_coord_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_set_coord_data
!
!$Resume
!  Modify attributes value of a coordinates structure
!
!$Description
!  Modify attributes value of a coordinates structure
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_set_coord_data(coord, [iorb], [param], [mu], [apla], [requa])
!.    type(MSP_TYPBUL) :: coord
!.    integer :: iorb 
!.    real(KIND=PM_REEL), dimension(6) :: param
!.    real(KIND=PM_REEL) :: mu, requa, apla
!
!$Procedures
!- MSP_modifier_typbul
!
!$Remarques
!
!$Mots-cles
! COORDONNEES MODIFIER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_modifier_typbul
   end interface

   interface MSP_convert_orbit_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_convert_orbit_data
!
!$Resume
!  Convert an orbit w.r.t reference frame and coordinate type
!
!$Description
!  Convert an orbit w.r.t reference frame and coordinate type
!
!$Acces
!  PUBLIC
!
!$Usage
!  bulletin_out = MSP_convert_orbit_data(bulletin_in, [iorb], [ech_temps_bul], &
!.           modprec,mu,requa, apla, typrep, &
!.           cle_date, date_ref, date_refjs, ech_temps_rep, &
!.           planete,lonref,lat,lon,alt, direction, requa_r, apla_r, obli, &
!.           pole_u, pole_v, origdat, vrot, repere)
!.    type(MSP_BULLETIN) :: bulletin_in
!.    integer :: iorb, typrep, ech_temps_bul
!.    integer :: planete, cle_date, ech_temps_rep, modprec
!.    real(KIND=PM_REEL) :: lonref, date_ref
!.    real(KIND=PM_REEL) :: lat, lon, alt, direction
!.    real(KIND=PM_REEL) :: requa, apla, requa_r, apla_r, mu
!.    real(KIND=PM_REEL) :: pole_u, pole_v, vrot, obli
!.    type(tm_jour_sec) :: date_refjs
!.    integer :: origdat
!.    type(MSP_TYPREP) :: repere
!.    type(MSP_BULLETIN) :: bulletin_out
!
!$Procedures
!- MSP_convertir_bulletin
!
!$Remarques
!
!$Mots-cles
! BULLETIN CONVERTIR
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_convertir_bulletin
   end interface

   interface MSP_get_frame_type

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_get_frame_type
!
!$Resume
!  Convert a frame type into character a string
!
!$Description
!  Convert a frame type into character a string
!
!$Acces
!  PUBLIC
!
!$Usage
!  string = MSP_get_frame_type(type)
!.    integer :: type
!
!$Procedures
!- MSP_type_repere
!
!$Remarques
!
!$Mots-cles
! REPERE TYPE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_type_repere
   end interface

   interface MSP_get_orbit_type

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_get_orbit_type
!
!$Resume
!  Convert an orbit type into a character string
!
!$Description
!  Convert an orbit type into a character string
!
!$Acces
!  PUBLIC
!
!$Usage
!  string = MSP_get_orbit_type(type)
!.    integer :: type
!
!$Procedures
!- MSP_type_bulletin
!
!$Remarques
!
!$Mots-cles
! BULLETIN TYPE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_type_bulletin
   end interface

   interface MSP_get_date_origin_type

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_get_date_origin_type
!
!$Resume
!  Convert a date origin type into a character string
!
!$Description
!  Convert a date origin type into a character string
!
!$Acces
!  PUBLIC
!
!$Usage
!  string = MSP_get_date_origin_type(type)
!.    integer :: type
!
!$Procedures
!- MSP_type_echelle_date
!
!$Remarques
!
!$Mots-cles
! BULLETIN TYPE ECHELLE DATE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_type_echelle_date
   end interface

   interface MSP_get_time_scale_type

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_get_time_scale_type
!
!$Resume
!  Convert a time scale type into a character string
!
!$Description
!  Convert a time scale type into a character string
!
!$Acces
!  PUBLIC
!
!$Usage
!  string = MSP_get_time_scale_type(type)
!.    integer :: type
!
!$Procedures
!- MSP_type_echelle_temps
!
!$Remarques
!
!$Mots-cles
! BULLETIN TYPE ECHELLE TEMPS
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_type_echelle_temps
   end interface

   interface MSP_display_orbit_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_display_orbit_data
!
!$Resume
!  Display orbit characteristics in a logical unit
!
!$Description
!  Display orbit characteristics in a logical unit
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_display_orbit_data(bulletin, lu)
!.    integer :: lu
!.    type(MSP_BULLETIN) :: bulletin
!
!$Procedures
!- MSP_afficher_bulletin
!
!$Remarques
!
!$Mots-cles
! BULLETIN AFFICHER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_afficher_bulletin
   end interface

   interface MSP_write_orbit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_write_orbit
!
!$Resume
!  Write an orbit into a file 
!
!$Description
!  Write an orbit into a file 
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_write_orbit(bulletin, subr_write, nom_dom, [nacc], [section], [type_section])
!.    type(MSP_BULLETIN) :: bulletin
!.    external subr_write
!.    character(LEN=*) :: nom_dom
!.    integer :: nacc
!.    integer, dimension(:) :: type_section
!.    character(LEN=*), dimension(:) :: section
!
!$Procedures
!- MSP_ecrire_bulletin
!
!$Remarques
!
!$Mots-cles
! BULLETIN ECRIRE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_ecrire_bulletin
   end interface

   interface MSP_write_frame

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_write_frame
!
!$Resume
!  Write a frame into a file 
!
!$Description
!  Write a frame into a file 
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_write_frame(repere,subr_write,nom_dom,[nacc],[section],[type_section])
!.    type(MSP_TYPREP) :: repere
!.    external subr_write
!.    character(LEN=*) :: nom_dom
!.    integer :: nacc
!.    integer, dimension(:) :: type_section
!.    character(LEN=*), dimension(:) :: section
!
!$Procedures
!- MSP_ecrire_typrep
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_ecrire_typrep
   end interface

   interface MSP_conv_typrep

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_conv_typrep
!
!$Resume
!  Changement de repère des paramètres d'un bulletin
!
!$Description
!  Changement de repère des paramètres d'un bulletin
!  Les dates sont, soit des réels, soit des jour/secondes
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_conv_typrep(param_in, typrep_e, cle_date_e, &
!.                  planete_e, dateref_e, ech_temps_e, &
!.                  lonref_e,  strae, requa_e, apla_e, &
!.                  typrep_s , cle_date_s, &
!.                  planete_s, dateref_s, ech_temps_s, &
!.                  lonref_s,  stras,  requa_s, apla_s, &
!.                  vrot, param_out, mat_jacob, obli, &
!.                  origdat, pole_in, pole_out, &
!.                  modprec_in, modprec_out, obli_out, vrot_out)
!.    integer :: typrep_e, ech_temps_e
!.    integer :: planete_e, cle_date_e
!.    real(KIND=PM_REEL) :: dateref_e ,lonref_e, requa_e, apla_e, vrot
!.    integer :: typrep_s,ech_temps_s
!.    integer :: planete_s, cle_date_s
!.    real(KIND=PM_REEL) :: dateref_s ,lonref_s, requa_s, apla_s
!.    real(KIND=PM_REEL), dimension(6) :: param_in
!.    real(KIND=PM_REEL), dimension(9) :: strae, stras
!.    real(KIND=PM_REEL) :: obli, obli_out, vrot_out
!.    type(tm_pole_uv) :: pole_in, pole_out
!.    real(KIND=PM_REEL), dimension(6) :: param_out
!.    real(KIND=PM_REEL), dimension(6, 6) :: mat_jacob
!.    integer :: origdat
!.    integer :: modprec_in,modprec_out
!
!  call MSP_conv_typrep(param_in, typrep_e, cle_date_e, &
!.                  planete_e, dateref_e, ech_temps_e, &
!.                  lonref_e,  strae, requa_e, apla_e, &
!.                  typrep_s , cle_date_s, &
!.                  planete_s, dateref_s, ech_temps_s, &
!.                  lonref_s,  stras,  requa_s, apla_s, &
!.                  vrot, param_out, mat_jacob, obli, &
!.                  origdat, pole_in, pole_out, &
!.                  modprec_in, modprec_out, obli_out, vrot_out)
!.    integer :: typrep_e, ech_temps_e
!.    integer :: planete_e, cle_date_e
!.    real(KIND=PM_REEL) :: lonref_e, requa_e, apla_e, vrot
!.    type(tm_jour_sec) :: dateref_e, dateref_s
!.    integer :: typrep_s,ech_temps_s
!.    integer :: planete_s, cle_date_s
!.    real(KIND=PM_REEL) :: lonref_s, requa_s, apla_s
!.    real(KIND=PM_REEL), dimension(6) :: param_in
!.    real(KIND=PM_REEL), dimension(9) :: strae, stras
!.    real(KIND=PM_REEL) :: obli,obli_out
!.    type(tm_pole_uv) :: pole_in, pole_out
!.    real(KIND=PM_REEL) :: vrot_out
!.    real(KIND=PM_REEL), dimension(6) :: param_out
!.    real(KIND=PM_REEL), dimension(6, 6) :: mat_jacob
!.    integer :: origdat
!.    integer :: modprec_in, modprec_out
!
!$Procedures
!#V
!- MSP_conv_typrep_frac
!- MSP_conv_typrep_jjsec
!#
!
!$Remarques
!
!$Mots-cles
! BULLETIN CONVERTIR
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_conv_typrep_frac, MSP_conv_typrep_jjsec
   end interface

   interface MSP_date_etutc

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_date_etutc
!
!$Resume
!  Conversions entre dates TUC et TE
!
!$Description
!  Conversions entre dates TUC et TE
!  Les dates sont, soit des réels, soit des jour/secondes
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_date_etutc(date_in,sens,date_out,[origdat])
!.    real(kind=pm_reel) :: date_in 
!.    integer :: sens
!.    real(kind=pm_reel) :: date_out 
!.    integer :: origdat
!
!  call MSP_date_etutc(date_in,sens,date_out,[origdat])
!.    type(tm_jour_sec) :: date_in
!.    type(tm_jour_sec) :: date_out
!.    integer :: sens
!.    integer :: origdat
!
!$Procedures
!#V
!- MSP_date_etutc_frac
!- MSP_date_etutc_jjsec
!#
!
!$Remarques
!
!$Mots-cles
! BULLETIN CONVERTIR
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_date_etutc_frac,MSP_date_etutc_jjsec
   end interface

   private MSP_creer_bulletin_param, MSP_creer_bulletin_madona
   private MSP_conv_typrep_frac, MSP_conv_typrep_jjsec
   private MSP_date_etutc_frac, MSP_date_etutc_jjsec


! Fonctions internes
   private comparer_repere, comparer_coord, comparer_bulletin
   private planete_mspro, direction_gslib, direction_mspro,vitesse_rotation
   private verif_typrep_planete, verif_typrep_date

 contains


   function MSP_creer_typrep_param(typrep, cle_date, planete, date_ref,date_refjs, &
        ech_temps_rep,lonref,lat, lon,alt,direction, corcen,requa_r,apla_r,vrot, &
        rep, pole_u, pole_v, obli,origdat) result (repere)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_creer_typrep_param
!
!$Resume
! Cette routine permet de créer une structure contenant les caractéristiques d'un repère
!
!$Description
! Cette routine permet de créer une structure contenant les caractéristiques d'un repère
!
!$Auteur
!  Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  repere = MSP_creer_typrep_param([typrep], [cle_date], [planete], [date_ref],[date_refjs], &
!.            [ech_temps_rep],[lonref],[lat], [lon],[alt],[direction], [corcen],[requa_r],[apla_r],[vrot], &
!.            [rep], [pole_u], [pole_v], [obli],[origdat])
!.    type (MSP_TYPREP) :: repere
!.    integer :: typrep
!.    integer :: cle_date, planete, corcen, ech_temps_rep
!.    real(kind=pm_reel) :: date_ref, lonref, lat,lon,alt,direction
!.    real(kind=pm_reel) :: requa_r,apla_r,vrot, pole_u,pole_v,obli
!.    type(tm_jour_sec) :: date_refjs
!.    real(kind=pm_reel), dimension(10) :: rep
!.    integer :: origdat
!
!$Arguments
!>[E]   typrep         :<integer>            type de repère (pm_equa_vrai, pm_equa_moyen, pm_ecli_moy,
!>                                        pm_veis, pm_planeto_ref, pm_planeto_ref_iner, pm_planeto_vrai ,
!>                                        pm_equa_uai, pm_topo)
!>[E]   cle_date       :<integer>            Date de référence du repère :
!>                                              pm_1janvier1950_00h00  date du repère = 1/1/1950 à 0h00         
!>                                              pm_1janvier2000_12h00  date du repère = 1/1/2000 à 12h00 (18262.5 JJCNES)         
!>                                              MSP_ENUM_ECHD_DATE_REF date du repère fixée par une date de référence         
!>                                              pm_autre_date          date du repère fixée par la date du bulletin    
!>[E]   planete        :<integer>            planète (eph_mercure,eph_venus,eph_terre,eph_mars,
!>                                     eph_jupiter,eph_saturne,eph_uranus,eph_neptune,eph_pluton)
!>                                     [par défaut eph_terre]
!>[E]   date_ref       :<pm_reel>            date de référence [JJ]
!>[E]   date_refjs     :<tm_jour_sec>        date de reference (Jours / secondes 1950), si présent
!                                      remplace date_ref
!>[E]   ech_temps_rep  :<integer>            échelle de temps de la date de référence du repère dans le cas où une date de
!>                                 référence est nécessaire cad
!>.                                       si typrep=pm_planeto_ref, pm_planeto_ref_iner, pm_planeto_vrai et ech_date=(pm_autre_date+1)
!>.                                       si typrep=pm_topo
!>[E]   lonref         :<pm_reel>            longitude de référence
!>[E]   lat            :<pm_reel>            latitude station [rad]
!>[E]   lon            :<pm_reel>            longitude station  [rad]
!>[E]   alt            :<pm_reel>            altitude station [m]
!>[E]   direction      :<pm_reel>            Direction de la monture de la station:
!>.                                       MSP_NORD  -> monture au Nord
!>.                                       MSP_SUD   -> monture au Sud
!>.                                       MSP_EST   -> monture  Est
!>.                                       MSP_OUEST -> monture  Ouest
!>[E]   corcen         :<integer>            corps central (eph_mercure,eph_venus,eph_terre,eph_mars,
!>                                     eph_jupiter,eph_saturne,eph_uranus,eph_neptune,eph_pluton,
!>                                     eph_soleil) [par défaut eph_terre]
!>[E]   requa_r        :<pm_reel>            rayon terrestre à l'équateur pour les coordonnées station  [m]
!>[E]   apla_r         :<pm_reel>            aplatissement terrestre pour les coordonnées station 
!>[E]   vrot           :<pm_reel>            vitesse de rotation [rad/s] [par défaut MSP_VITROT(eph_terre)]
!>[E]   rep            :<pm_reel,DIM=(10)>   
!>[E]   pole_u         :<pm_reel>            angle u du pole vrai [rad]
!>[E]   pole_v         :<pm_reel>            angle v du pole vrai [rad]
!>[E]   obli           :<pm_reel>            obliquite du corps central [rad]
!>[E]   origdat        :<integer>            origine des dates (0=J50, 1=MJD2000)
!>S     repere         :<MSP_TYPREP>         Repère à créer
!
!$Common
!
!$Routines
!- MSP_init_structure_topo
!- md_jourfrac_joursec
!- MSP_signaler_message
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
! REPERE CREER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     implicit none
     
     type (MSP_TYPREP) :: repere

     integer, intent(IN), optional :: typrep
     integer, intent(IN), optional :: cle_date, planete, corcen, ech_temps_rep
     real(kind=pm_reel), intent(IN), optional :: date_ref, lonref, lat,lon,alt,direction
     real(kind=pm_reel), intent(IN), optional :: requa_r,apla_r,vrot, pole_u,pole_v,obli
     type(tm_jour_sec) , intent(IN), optional :: date_refjs
     real(kind=pm_reel), dimension(10),intent(IN), optional :: rep
     integer, intent(IN), optional :: origdat
   
     ! variables locales
     type(tm_def_topo)         :: topo
     type(tm_pole_uv)          :: pole
     type(tm_code_retour)      :: code_retour
     character(LEN=10)         :: cier


     ! Initialisation de la  structure topo
     call MSP_init_structure_topo(topo)
              
     ! Initialisations des valeurs par défaut : valeurs de la TERRE
     repere%lonref    = 0._pm_reel
     repere%corcen    = eph_terre
     repere%planete   = eph_terre
     repere%vrot      = MSP_VITROT((eph_terre-99)/100)
     repere%obli      = MSP_OBLIQUITE
     repere%ech_temps_rep = pm_TE
     repere%origdat    = 0


     ! -- Mise en conformité des champs cle_date / date_ref
     if (present(cle_date)) then
        repere%cle_date=cle_date
        select case(cle_date)
        case(pm_1janvier1950_00h00)
           repere%date_ref%jour = MSP_1950jour
           repere%date_ref%sec  = MSP_1950sec
        case(pm_1janvier2000_12h00)
           repere%date_ref%jour = int(MSP_JJ2000)
           repere%date_ref%sec  = (MSP_JJ2000 - repere%date_ref%jour) * 86400._PM_REEL
        case(MSP_ENUM_ECHD_DATE_REF)
           if(present(date_refjs)) then
              repere%date_ref=date_refjs
           elseif(present(date_ref)) then
              call md_jourfrac_joursec(date_ref, repere%date_ref,code_retour)
           else
              ! Date de reference obligatoire
              call MSP_signaler_message (cle_mes="MSP_creer_bulletin_rep_007")
              return
           end if

        case(pm_autre_date)
           ! - La date de référence sera celle du bulletin
        case default
           ! -- Code inconnu
           write (cier,'(i6.6)') cle_date
           call MSP_signaler_message (cle_mes="MSP_creer_bulletin_rep_008",&
                partie_variable=cier)
           return
        end select

     else
        ! Si cle_date est absent mais une date_ref est fournie
        if(present(date_refjs)) then
           repere%cle_date = (MSP_ENUM_ECHD_DATE_REF)
           repere%date_ref = date_refjs
        elseif (present(date_ref)) then
           repere%cle_date = MSP_ENUM_ECHD_DATE_REF
           call md_jourfrac_joursec(date_ref, repere%date_ref,code_retour)
        endif
     endif


     ! - Appel par PSIMU : les caractéristiques du repères sont dans le tableau rep(10)
     if (present(rep)) then
        repere%typrep  = int(rep(1))
        select case(repere%typrep)
        case(pm_planeto_ref, pm_planeto_ref_iner, pm_planeto_vrai)
           repere%lonref = rep(3)
        case(pm_topo)
           topo%geod%lat  = rep(4)
           topo%geod%long = rep(5)
           topo%geod%haut = rep(6)
           topo%axe_x     = rep(7)
        end select
         
        if (repere%cle_date == MSP_ENUM_ECHD_DATE_REF) then
           repere%date_ref%jour = int(rep(4))
           repere%date_ref%sec = rep(5)
           repere%ech_temps_rep = int(rep(2))
        endif
     endif

     ! Type de repère
 
     if (present(typrep)) then
        repere%typrep = typrep
     else if (.not.present(rep)) then
        ! Présence de typrep obligatoire
        call MSP_signaler_message (cle_mes="MSP_creer_bulletin_rep_003")
        return
     endif

     ! Origine des dates (J50 ou MJD2000)
     if ( present(origdat) ) repere%origdat = origdat

     ! Corps central
     if ( present(corcen) ) repere%corcen = corcen

     ! Obliquite
     if ( present(obli) ) repere%obli = obli

     ! Longitude de reference
     if ( present(lonref) ) repere%lonref = lonref

     ! Vérification de la compatibilité entre le type de repère et la date de référence
     if (.not.verif_typrep_date(repere%typrep,repere%cle_date)) return


     ! Positionnement de l'echelle de temps pour la date de référence
     if (present(ech_temps_rep)) then
        if (ech_temps_rep /= 0) repere%ech_temps_rep = ech_temps_rep
     endif

     if (present(planete)) then
        ! Vérification de la compatibilité entre le type de repère et la planete
        if (.not.verif_typrep_planete(repere%typrep,planete)) then
           call MSP_signaler_message (cle_mes="MSP_creer_bulletin_rep_004", routine="MSP_creer_typrep")
           return
        else
           repere%planete = planete
        endif
     endif


     ! Initialisation de la structure pole
     pole%u = MSP_poleu2000((repere%planete-99)/100)
     pole%v = MSP_poleu2000((repere%planete-99)/100)
 
     ! -- Affectation des coordonnées du pole vrai
     if (present(pole_u)) pole%u = pole_u
     if (present(pole_v)) pole%v = pole_v
     repere%pole=pole

     ! Vitesse de rotation
     if ( present(vrot) ) then
        repere%vrot = vrot
     else if ( present(corcen) ) then
        repere%vrot = vitesse_rotation(corcen)
     endif


     ! Informations topocentriques 
     if (present(requa_r )) topo%ellips%r_equa = requa_r
     if (present(apla_r )) topo%ellips%apla =   apla_r  

     if ( (repere%typrep == pm_topo).and. .not.present(rep) ) then
        if ( .not.present(lat) .or. .not.present(lon) .or. .not.present(alt) .or. &
             .not.present(direction) .or. &
             .not.present(requa_r) .or. .not.present(apla_r) ) then
           call MSP_signaler_message (cle_mes="MSP_creer_bulletin_rep_001")
           return
        else
           topo%geod%lat  = lat     
           topo%geod%long = lon   
           topo%geod%haut = alt
           topo%axe_x = direction    
        endif
        if ( present(ech_temps_rep) ) &
             call MSP_signaler_message (cle_mes="MSP_creer_bulletin_ech_temps_001")
     endif

     repere%station = topo

     ! -- Message de Warning : informations inutiles
     if ( (repere%typrep /= pm_topo).and. (present(lat).or.&
          present(lon).or.present(alt).or. &
          present(direction).or.&
          present(requa_r).or.present(apla_r)) ) then
        call MSP_signaler_message (cle_mes="MSP_creer_bulletin_rep_006", &
             partie_variable=MSP_type_repere(repere%typrep))
     end if


     if ( (repere%typrep /= pm_planeto_ref).and.&
          (repere%typrep /= pm_planeto_ref_iner).and. &
          (repere%typrep /= pm_planeto_vrai).and. present(lonref) ) then
        call MSP_signaler_message (cle_mes="MSP_creer_bulletin_rep_009", &
             partie_variable=MSP_type_repere(repere%typrep))
     end if


   end function MSP_creer_typrep_param


    FUNCTION MSP_creer_typrep_mad(subr_lect, nom_dom, nacc, section, type_section) result(repere)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_creer_typrep_mad
!
!$Resume
!  Fonction créant une structure repère à partir d'un fichier
!
!$Description
!  Fonction créant une structure repère à partir d'un fichier ou d'un moyen d'acces
!  MADONA
!
!$Auteur
!  Florence VIVARES (ATOS origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  repere = MSP_creer_typrep_mad(subr_lect, nom_dom, [nacc], [section], [type_section])
!.    type (MSP_TYPREP) :: repere
!.    external subr_lect
!.    character(LEN=*) :: nom_dom
!.    integer :: nacc
!.    integer, dimension(:) :: type_section
!.    character(LEN=MSP_LONG_CHAINE), dimension(:) :: section
!
!$Arguments
!>E/S   subr_lect     :<external>                      sous-programme de lecture (doit avoir la syntaxe d'appel de read_gs_repere_ip)
!>E     nom_dom       :<LEN=*>                         domaine de traduction
!>[E]   nacc          :<integer>                       moyen d'acces MADONA
!>[E]   section       :<LEN=MSP_LONG_CHAINE,DIM=(:)>   tableau de sections à
!                      l'interieur desquels lire la structure contenant le repère
!>[E]   type_section  :<integer,DIM=(:)>               tableau de types
!                      (ACC_STRUCT ou ACC_TABL) de même taille que section.
!>S     repere        :<MSP_TYPREP>                    
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- md_jourfrac_joursec
!- subr_lect
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

      ! retour de la fonction
      type (MSP_TYPREP) :: repere

      ! Parametres
      ! fonction de lecture externe
      external subr_lect

      ! Reference du moyen d'acces MADONA 
      character(LEN=*), intent(IN) :: nom_dom
      integer, intent(IN), optional :: nacc
      ! Emplacement dans le moyen d'acces (section a selectionner)
      integer, dimension(:), intent(IN), optional :: type_section
      character(LEN=MSP_LONG_CHAINE), dimension(:), intent(IN), optional :: section

 
      ! Variables locales
      integer :: ntab, ier, caster, ii, MSP_iostat
      integer, dimension(:), pointer :: type_section_loc => NULL()
      character(LEN=MSP_LONG_CHAINE), dimension(:), allocatable :: section_loc
      real(kind=pm_reel)   :: dir_pm   ! conversion en valeur mspro de la direction d'un repère Station
      type(tm_code_retour) :: code_retour
      character(LEN=3) :: chaine_nulle, chaine_nulle2

      ! - Dansle format GS_LIB, les données station sont dans un tableau
      real(kind=pm_reel), dimension(10)  :: rep


      ! Initialisations
      
      MSP_iostat = 0
      ! Pour respecter le format d'appel de GS_REPERE_IP
      chaine_nulle = "."
      chaine_nulle2 = ""


      ! Un argument type_section ou section est passé
      if ( PRESENT(section).or.PRESENT(type_section) ) then
         
         if (.not.PRESENT(nacc)) then 
            call MSP_signaler_message (cle_mes="MSP_ERREUR_ARGUMENTS_001", &
                 routine="MSP_creer_typrep_mad", type=MSP_ENUM_ERREUR, &
                 partie_variable="section, type_section et nacc")
            return

            ! L'argument nacc est également présent
         else
          
            ! Les deux arguments section et type_section sont présents
            if (PRESENT(section).and.PRESENT(type_section)) then 
               
               ntab = size(section)
               allocate(section_loc(ntab))
               do ii=1,ntab
                  section_loc(ii) = trim(section(ii))
               enddo
               ier = MSP_acc_select(nacc, section, type_section, ntab)
               ! DM1058 - Gestion d'erreur
               if (ier < 0) then
                  call MSP_signaler_message (cle_mes="MSP_probleme_select_2", &
                       routine="MSP_creer_typrep_mad")
               endif
               deallocate(section_loc,stat=MSP_iostat)
               if (MSP_gen_messages("MSP_creer_typrep_mad")) return

               
            !  Type_section est présent mais pas section
            else if ( .not.PRESENT(section).and.PRESENT(type_section) ) then
               call MSP_signaler_message (cle_mes="MSP_creer_bulletin_arguments", &
                    routine="MSP_creer_typrep_mad", partie_variable="section")
               return
               
            ! section n'est pas présent, mais type_section l'est 
            else if ( (.not.PRESENT(type_section).and.PRESENT(section)) ) then 
               ntab = size(section)
               ALLOCATE(type_section_loc(ntab))
               type_section_loc(:) = ACC_STRUCT
               allocate(section_loc(ntab))
               do ii=1,ntab
                  section_loc(ii) = trim(section(ii))
               enddo
               ier = MSP_acc_select(nacc, section, type_section_loc, ntab)
               ! DM1058 - Gestion d'erreur
               if (ier < 0) then
                  call MSP_signaler_message (cle_mes="MSP_probleme_select_2", &
                       routine="MSP_creer_typrep_mad")
               endif
               DEALLOCATE(type_section_loc,stat=MSP_iostat)
               deallocate(section_loc,stat=MSP_iostat)
               if (MSP_gen_messages("MSP_creer_typrep_mad")) return
               
          end if

       end if

       ! Pas d'argument section ni type_section
    else
       
       ! Dans ce cas nacc est inutile
       if (PRESENT(nacc)) then 
          call MSP_signaler_message (cle_mes="MSP_ERREUR_ARGUMENTS_008", &
               routine="MSP_creer_typrep_mad", type=MSP_ENUM_WARNING, &
               partie_variable="nacc")
       end if

    end if

    ! Initialisations
    repere%station%geod%lat      = 0._pm_reel    
    repere%station%geod%long     = 0._pm_reel   
    repere%station%geod%haut     = 0._pm_reel     

    ! Valeurs écrasée par la lecture
    repere%station%ellips%r_equa = MSP_REQUA_TERRE  
    repere%station%ellips%apla   = 1./MSP_APLA_TERRE ! Valeur inversée après la lecture

    repere%station%axe_x         = 0._pm_reel 
    repere%pole%u                = 0._pm_reel 
    repere%pole%v                = 0._pm_reel
    repere%lonref                = 0._pm_reel 
    call md_jourfrac_joursec(0._pm_reel, repere%date_ref,code_retour)
    repere%obli                  = MSP_OBLIQUITE
    repere%origdat               = 0
    repere%ech_temps_rep         = pm_TE


    caster = 0   ! Pour tous les corps centraux
    call subr_lect(rep(:), repere%station%ellips%r_equa, repere%station%ellips%apla, &
         repere%planete,   repere%corcen, repere%cle_date, &
         repere%vrot, repere%ech_temps_rep, caster, nom_dom, repere%obli, &
         repere%pole%u, repere%pole%v, 0, chaine_nulle, chaine_nulle2)

    ! - Décomposition du tableau rep
        repere%typrep  = int(rep(1))
        select case(repere%typrep)
           case(pm_planeto_ref, pm_planeto_ref_iner, pm_planeto_vrai)
              repere%lonref = rep(3)
           case(pm_topo)
              repere%station%geod%lat  = rep(4)
              repere%station%geod%long = rep(5)
              repere%station%geod%haut = rep(6)
              ! - La direction GS_LIB vaut 1 / 2 / 3 /4
              dir_pm = direction_mspro(int(rep(7)))
              if (MSP_gen_messages("MSP_creer_typrep_mad - direction")) return
              repere%station%axe_x =  dir_pm
         end select
         
        select case(repere%cle_date)
         case (MSP_ENUM_ECHD_DATE_REF)
            repere%date_ref%jour = int(rep(4))
            repere%date_ref%sec  = rep(5)
            repere%ech_temps_rep = int(rep(2))
         case (pm_1janvier2000_12h00)
            repere%date_ref%jour = int(MSP_JJ2000)
            repere%date_ref%sec  = (MSP_JJ2000 - repere%date_ref%jour)*86400._PM_REEL
         case default
            repere%date_ref%jour = MSP_1950jour
            repere%date_ref%sec  = MSP_1950sec
         end select
         
         ! - L'aplatissement mspro est inversée
         if (abs(repere%station%ellips%apla) > MSP_epsilon_apla) then
            repere%station%ellips%apla = 1. / repere%station%ellips%apla
         else
            repere%station%ellips%apla = 999.E10
         endif
  

    END FUNCTION MSP_creer_typrep_mad


    real(kind=pm_reel) function vitesse_rotation(planete) result(vrot)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  vitesse_rotation
!
!$Resume
!  Vitesse de rotation d'un corps
!
!$Description
!  Retourne la vitesse de rotation d'une planete ou du soleil
!
!$Auteur
!   Didier Semeux (ATOS ORIGIN)
!
!$Acces
!  PRIVE
!
!$Usage
!  vrot = vitesse_rotation(planete)
!.    integer :: planete
!
!$Arguments
!>E     planete  :<integer>   Code planète MECASPA
!>S     vrot     :<pm_reel>   Vitesse de rotation rad/s
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
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      integer, intent(IN)  :: planete
      integer              :: ipla
      character(LEN=10)         :: cier

      vrot = 0._pm_reel

      ipla = (planete-99)/100

      if (planete == 10) then
         ! Soleil 
         vrot = MSP_VITROT(planete)
      else if ((ipla >=1).and.(ipla<=9)) then
         vrot = MSP_VITROT(ipla)
      else
         ! -- Code inconnu
         write (cier,'(i6.6)') planete
         call MSP_signaler_message (cle_mes="vitesse_rotation_001",&
              partie_variable=cier)
      endif
      
    end function vitesse_rotation

    integer function planete_mspro(planete) result(pm_planete)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  planete_mspro
!
!$Resume
!  Code planète MSPRO 
!
!$Description
!  Retourne le code planète MSPRO 
!
!$Auteur
!   Didier Semeux (ATOS ORIGIN)
!
!$Acces
!  PRIVE
!
!$Usage
!  pm_planete = planete_mspro(planete)
!.    integer :: planete
!
!$Arguments
!>E     planete     :<integer>   Code planète MECASPA
!>S     pm_planete  :<integer>   Code planète MSPRO
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
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      integer, intent(IN)  :: planete
      character(LEN=10)         :: cier

      select case(planete)

      case( eph_mercure)
         pm_planete = pm_pla_mercure
      case( eph_venus)
         pm_planete = pm_pla_venus
      case( eph_terre)
         pm_planete = pm_pla_terre
      case( eph_mars)
        pm_planete = pm_pla_mars
      case( eph_jupiter)
        pm_planete = pm_pla_jupiter
      case( eph_saturne)
         pm_planete = pm_pla_saturne
      case( eph_uranus)
         pm_planete = pm_pla_uranus
      case( eph_neptune)
        pm_planete = pm_pla_neptune
      case( eph_pluton)
         pm_planete = pm_pla_pluton
      case default
         ! -- Code inconnu
         pm_planete = 0
         write (cier,'(i6.6)') planete
         call MSP_signaler_message (cle_mes="planete_mspro_001",partie_variable=cier)
      end select
      
    end function planete_mspro


    real(kind=pm_reel) function direction_mspro(direction) result(pm_direction)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  direction_mspro
!
!$Resume
!  Code direction MSPRO 
!
!$Description
!  Retourne le code direction MSPRO 
!
!$Auteur
!   Didier Semeux (ATOS ORIGIN)
!
!$Acces
!  PRIVE
!
!$Usage
!  pm_direction = direction_mspro(direction)
!.    integer :: direction
!
!$Arguments
!>E     direction     :<integer>   Code direction MECASPA
!>S     pm_direction  :<pm_reel>   Code direction MSPRO
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
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      integer, intent(IN)  :: direction
      character(LEN=10)         :: cier

      select case(direction)

      case( MSP_ENUM_NORD )
         pm_direction = MSP_NORD
      case( MSP_ENUM_SUD )
         pm_direction = MSP_SUD
      case( MSP_ENUM_EST )
         pm_direction = MSP_EST
      case( MSP_ENUM_OUEST )
         pm_direction = MSP_OUEST
      case default
         ! -- Code inconnu
         pm_direction = 0
         write (cier,'(i6.6)') direction
         call MSP_signaler_message (cle_mes="direction_mspro_001",partie_variable=cier)
      end select
      
    end function direction_mspro


    real(kind=pm_reel) function direction_gslib(direction) result(gs_direction)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  direction_gslib
!
!$Resume
!  Code direction GSLIB
!
!$Description
!  Retourne le code direction GSLIB
!
!$Auteur
!   Didier Semeux (ATOS ORIGIN)
!
!$Acces
!  PRIVE
!
!$Usage
!  gs_direction = direction_gslib(direction)
!.    real(kind=pm_reel) :: direction
!
!$Arguments
!>E     direction     :<pm_reel>   Code direction MECASPA
!>S     gs_direction  :<pm_reel>   Code direction GSLIB
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
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      real(kind=pm_reel), intent(IN)  :: direction
      character(LEN=10)         :: cier


      if ( direction.egal.MSP_NORD ) then
         gs_direction = real(MSP_ENUM_NORD, kind=pm_reel)
      else if (direction.egal.MSP_SUD ) then
         gs_direction = real( MSP_ENUM_SUD, kind=pm_reel)
      else if (direction.egal.MSP_EST )  then
         gs_direction =real( MSP_ENUM_EST, kind=pm_reel)
      else if ( direction.egal.MSP_OUEST )  then
         gs_direction = real(MSP_ENUM_OUEST, kind=pm_reel)
      else
         ! -- Code inconnu
         gs_direction = 0
         write (cier,'(f10.6)') direction
         call MSP_signaler_message (cle_mes="direction_gslib_001",partie_variable=cier)
      endif
      
    end function direction_gslib



    logical function verif_typrep_planete(typrep,planete) result (retour) 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  verif_typrep_planete
!
!$Resume
!      Réalise les controle de cohérence entre le type de repere et la planete
!
!$Description
!       Réalise les controle de cohérence entre le type de repere et la planete.
!       Retourne .true. si les controles sont positifs, et .false. dans le cas contraire
!
!$Auteur
!!       JF Goester / Externalisé par D. Semeux (SchlumbergerSema)
!
!$Acces
!  PRIVE
!
!$Usage
!  retour = verif_typrep_planete(typrep,[planete])
!.    integer :: typrep
!.    integer :: planete
!
!$Arguments
!>E     typrep   :<integer>   
!>[E]   planete  :<integer>   
!>S     retour   :<logical>   
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
!  REPERE COHERENCE PLANETE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      integer, intent(IN)           :: typrep
      integer, optional, intent(IN) :: planete

      retour = .true.

      ! Test sur la présence de la planète
      select case (typrep)
         ! Planète non nécessaire:
         case (pm_veis,pm_topo)
         if ( present(planete) ) then
            call MSP_signaler_message (cle_mes="MSP_creer_bulletin_planete_001")
         endif
         ! Toutes planètes:
         case (pm_equa_moy,pm_planeto_ref, pm_planeto_ref_iner, &
                pm_planeto_vrai, pm_equa_uai)
         if ( .not.present(planete) ) then
            call MSP_signaler_message (cle_mes="MSP_creer_bulletin_planete_002")
         endif
         ! Planète terre uniquement:
         case (pm_equa_vrai,pm_ecli_moy)
         if ( present (planete) ) then
            if ( planete /= eph_terre ) then
               call MSP_signaler_message (cle_mes="MSP_creer_bulletin_planete_003")
            endif
         endif

         case default
            call MSP_signaler_message (cle_mes="MSP_creer_bulletin_rep_005")
            return
      end select


    end function verif_typrep_planete


    logical function verif_typrep_date (typrep,cle_date) result (retour) 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  verif_typrep_date
!
!$Resume
!       Réalise les contrôles de cohérence entre le type de repere et la cle_date
!
!$Description
!       Réalise les contrôles de cohérence entre le type de repere et la cle_date
!       Retourne .true. si les contrôles sont positifs, et .false. dans le cas contraire
!
!$Auteur
!       JF Goester / Externalisé par D. Semeux (SchlumbergerSema)
!
!$Acces
!  PRIVE
!
!$Usage
!  retour = verif_typrep_date (typrep,[cle_date])
!.    integer :: typrep
!.    integer :: cle_date
!
!$Arguments
!>E     typrep    :<integer>   
!>[E]   cle_date  :<integer>   
!>S     retour    :<logical>   
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
!  REPERE COHERENCE DATE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      integer, intent(IN)           :: typrep
      integer, optional, intent(IN) :: cle_date

      retour = .true.


      ! Test sur la présence du type de date utilisé pour l'origine des dates
      select case (typrep)

         ! Echelle de date non nécessaire:
         case (pm_equa_uai)
            !repere%cle_date = pm_1janvier1950_00h00
            if ( present(cle_date) ) call MSP_signaler_message (cle_mes="MSP_creer_bulletin_echd_001")

         ! Echelle de date obligatoire parmi toutes sauf "date de reference"
         case (pm_equa_vrai,pm_equa_moy,pm_ecli_moy)
            if ( .not. present(cle_date) ) then
               call MSP_signaler_message (cle_mes="MSP_creer_bulletin_echd_002")
               retour = .false.
               return
            else if (cle_date == MSP_ENUM_ECHD_DATE_REF) then 
               call MSP_signaler_message (cle_mes="MSP_creer_bulletin_echd_003", &
                                          partie_variable='"DATE_BUL, 1950 ou 2000"')
               retour = .false.
               return
            endif

         ! Echelle de date obligatoire parmi date courante et 1950
         case (pm_veis)
            if ( .not. present(cle_date) ) then
               call MSP_signaler_message (cle_mes="MSP_creer_bulletin_echd_003", &
                                          partie_variable='"DATE_BUL ou 1950"')
               retour = .false.
               return
            else if ( (cle_date /= pm_autre_date) .and. (cle_date /= pm_1janvier1950_00h00) ) then
               call MSP_signaler_message (cle_mes="MSP_creer_bulletin_echd_003", &
                                          partie_variable='"DATE_BUL ou 1950"')
               retour = .false.
               return
            endif

         ! Echelle de date obligatoire parmi date courante et date de référence
         case (pm_planeto_ref, pm_planeto_ref_iner, pm_planeto_vrai  )
            if ( .not. present(cle_date) ) then
               call MSP_signaler_message (cle_mes="MSP_creer_bulletin_echd_003", &
                                          partie_variable='"DATE_BUL ou DATE_REFERENCE"')
               retour = .false.
               return
            else if ( (cle_date /= pm_autre_date) .and. (cle_date /= MSP_ENUM_ECHD_DATE_REF) ) then
               call MSP_signaler_message (cle_mes="MSP_creer_bulletin_echd_003", &
                                          partie_variable='"DATE_BUL ou DATE_REFERENCE"')
               retour = .false.
               return
            endif

         ! Echelle de date obligatoirement DATE_BUL
         case(pm_topo)
              if ( .not. present(cle_date) ) then
                 call MSP_signaler_message (cle_mes="MSP_creer_bulletin_echd_003", &
                                          partie_variable='" MSP_ENUM_ECHD_DATE_BUL"')
                 retour = .false.
                 return
                 else if (cle_date /= pm_autre_date) then
                    call MSP_signaler_message (cle_mes="MSP_creer_bulletin_echd_003", &
                                          partie_variable='" MSP_ENUM_ECHD_DATE_BUL"')
                    retour = .false.
                    return
              endif
         case default
            call MSP_signaler_message (cle_mes="MSP_creer_bulletin_rep_005")
            retour = .false.
            return
      end select

    end function verif_typrep_date


    function MSP_creer_typbul (param, iorb, mu, requa, apla) result(coord)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_creer_typbul
!
!$Resume
! Création d' une structure contenant les coordonnées relatives à un bulletin
!
!$Description
! Création d' une structure contenant les coordonnées relatives à un bulletin
!
!$Auteur
! Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  coord = MSP_creer_typbul (param, iorb, [mu], [requa], [apla])
!.    type(MSP_TYPBUL) :: coord
!.    integer :: iorb
!.    real(kind=pm_reel) :: param(6)
!.    real(kind=pm_reel) :: mu,requa,apla
!
!$Arguments
!>E     param  :<pm_reel,DIM=(6)>   coordonnées (cf. définition de iorb)
!>E     iorb   :<integer>           type de coordonnées (pm_kep, pm_cir,
!>                                        pm_equa, pm_cir_equa, pm_car,
!>                                        pm_hpha, pm_geoc,
!>                                        pm_geod_meca_vol,
!>                                        pm_geod_gps_ard)
!>[E]   mu     :<pm_reel>           terme central du potentiel (m^3/s^2) [par défaut, valeur issue de COMPAS]
!>[E]   requa  :<pm_reel>           rayon terrestre à l'équateur [m] [par défaut valeur issue de COMPAS]
!>[E]   apla   :<pm_reel>           aplatissement terrestre [par défaut valeur issue de COMPAS]
!>S     coord  :<MSP_TYPBUL>        Structure coordonnées créée
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
! COORDONNEES CREER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      type(MSP_TYPBUL) :: coord
      integer, intent(IN)            :: iorb
      real(kind=pm_reel), intent(IN) :: param(6)
      real(kind=pm_reel), intent(IN), optional :: mu,requa,apla
      
      ! Variables locales
      integer :: trouve
      character(LEN=32) :: unite
      real(kind=pm_reel) :: apla_loc

      coord%iorb     = iorb
      coord%param(:) = param(:)

      if ( present(mu) ) then
         coord%mu = mu
      else
         if (.not.msp_cps_mu_lu) then
            trouve = cps_getCsteThCourante(eph_terre, "mu", MSP_cps_MU_TERRE, unite)
            msp_cps_mu_lu =.true.
         endif
         coord%mu = MSP_cps_MU_TERRE
!         coord%mu = 398600.47e+09_pm_reel
      endif

      if ( present(requa) ) then
         coord%requa = requa
      else
         if (.not.msp_cps_requa_lu) then
            trouve = cps_getCsteThCourante(eph_terre, "requa", MSP_cps_REQUA_TERRE, unite)
            msp_cps_requa_lu =.true.
         endif
         coord%requa = MSP_cps_REQUA_TERRE
      endif

      if ( present(apla) ) then
         coord%apla = apla
      else
         if (.not.msp_cps_apla_lu) then
            trouve = cps_getCsteThCourante(eph_terre, "apla", MSP_cps_APLA_TERRE, unite)
            msp_cps_apla_lu =.true.
         endif
         apla_loc = MSP_cps_APLA_TERRE
         coord%apla = apla_loc
         if (apla_loc > 1._pm_reel) coord%apla = 1._pm_reel/apla_loc
      endif

      ! Le mode MSP_ENUM_RENTREE_SPHERIQUE est ramené à MSP_ENUM_RENTREE_ELLIPSOIDIQUE avec applatissement nul
      if (coord%iorb==MSP_ENUM_RENTREE_SPHERIQUE) then
         coord%iorb = pm_geod_meca_vol
         coord%apla = 0._pm_reel
      endif

    end function MSP_creer_typbul


   function MSP_creer_bulletin_param(datbul,iorb,param,typrep,repere,coord,&
        ech_temps_bul,datbul_js,lonref,date_ref,date_refjs,ech_temps_rep,lat,lon,&
        alt,direction,planete,corcen,cle_date,modprec,mu,requa,apla,requa_r,apla_r,&
        vrot,rep,pole_u,pole_v,obli,origdat) result (bulletin)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_creer_bulletin_param
!
!$Resume
!  Fonction créant un bulletin orbital à partir de paramètres
!
!$Description
!  Fonction créant un bulletin orbital à partir de paramètres
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PRIVE
!
!$Usage
!  bulletin = MSP_creer_bulletin_param(datbul,[iorb],[param],[typrep],[repere],[coord],&
!.            [ech_temps_bul],[datbul_js],[lonref],[date_ref],[date_refjs],[ech_temps_rep],[lat],[lon],&
!.            [alt],[direction],[planete],[corcen],[cle_date],[modprec],[mu],[requa],[apla],[requa_r],[apla_r],&
!.            [vrot],[rep],[pole_u],[pole_v],[obli],[origdat])
!.    real(kind=pm_reel) :: datbul
!.    type(MSP_TYPREP) :: repere
!.    type(MSP_TYPBUL) :: coord
!.    integer :: iorb
!.    real(kind=pm_reel) :: param(6)
!.    integer :: typrep
!.    integer :: ech_temps_bul
!.    real(kind=pm_reel) :: lonref,date_ref,lat,lon,alt
!.    real(kind=pm_reel) :: direction
!.    integer :: planete,corcen,cle_date,modprec,ech_temps_rep
!.    real(kind=pm_reel) :: mu,requa,apla,requa_r,apla_r,vrot
!.    real(kind=pm_reel) :: pole_u, pole_v, obli
!.    real(kind=pm_reel), dimension(10) :: rep
!.    type(tm_jour_sec) :: datbul_js, date_refjs
!.    integer :: origdat
!.    type(MSP_BULLETIN) :: bulletin
!
!$Arguments
!>E     datbul         :<pm_reel>            date du bulletin [JJ]
!>[E]   iorb           :<integer>            type de coordonnées (pm_kep, pm_cir,
!>                                        pm_equa, pm_cir_equa, pm_car,
!>                                        pm_hpha, pm_geoc, MSP_ENUM_TWOLINES,
!>                                        MSP_ENUM_ADAPTE_CNES, MSP_ENUM_RENTREE_SPHERIQUE, pm_geod_meca_vol,
!>                                        pm_geod_gps_ard)
!>[E]   param          :<pm_reel,DIM=(6)>    coordonnées (cf. définition de iorb)
!>[E]   typrep         :<integer>            type de repère (pm_equa_vrai, pm_equa_moyen, pm_ecli_moy,
!>                                        pm_veis, pm_planeto_ref, pm_planeto_ref_iner, pm_planeto_vrai,
!>                                        pm_equa_uai, pm_topo)
!>[E]   repere         :<MSP_TYPREP>         Structure repère (prioritaire sur les autres arguements de cette sous-structure, voir MSP_creer_typrep)   
!>[E]   coord          :<MSP_TYPBUL>         Structure type de bulletin (prioritaire sur les autres arguements de cette sous-structure, voir MSP_creer_typbul)   
!>[E]   ech_temps_bul  :<integer>            échelle de temps de la date du bulletin (pm_TUC, pm_TE)
!>[E]   datbul_js      :<tm_jour_sec>        date du bulletin jours/secondes 
!>[E]   lonref         :<pm_reel>            longitude de référence
!>[E]   date_ref       :<pm_reel>            date de référence [JJ]
!>[E]   date_refjs     :<tm_jour_sec>        date de référence jours/secondes
!>[E]   ech_temps_rep  :<integer>            échelle de temps de la date de référence du repère dans le cas où une date de
!>                                      référence est nécessaire cad
!>.                                       si typrep=pm_planeto_ref, pm_planeto_ref_iner, pm_planeto_vrai et cle_date=(pm_autre_date+1)
!>[E]   lat            :<pm_reel>            latitude station [rad]
!>[E]   lon            :<pm_reel>            longitude station  [rad]
!>[E]   alt            :<pm_reel>            altitude station  [m]
!>[E]   direction      :<pm_reel>            Direction de la monture de la station:
!>.                                       MSP_ENUM_NORD  -> monture au Nord
!>.                                       MSP_ENUM_SUD   -> monture au Sud
!>.                                       MSP_ENUM_EST   -> monture  Est
!>.                                       MSP_ENUM_OUEST -> monture  Ouest
!>[E]   planete        :<integer>            planète (eph_mercure,eph_venus,eph_terre,eph_mars,
!>                                     eph_jupiter,eph_saturne,eph_uranus,eph_neptune,eph_pluton)
!>                                     [par défaut eph_terre]
!>[E]   corcen         :<integer>            corps central (eph_mercure,eph_venus,eph_terre,eph_mars,
!>                                     eph_jupiter,eph_saturne,eph_uranus,eph_neptune,eph_pluton,
!>                                     eph_soleil) [par défaut eph_terre]
!>[E]   cle_date       :<integer>            type de date utilisée dans pour l'origine des dates (pm_1janvier1950_00h00, pm_1janvier2000_12h00,
!>                                      pm_autre_date, MSP_ENUM_ECHD_DATE_REF)
!>                                      dans le cas des repères PQ et STATION la
!>                                     valeur par défaut est pm_autre_date
!>[E]   modprec        :<integer>            mode de précession (pm_lieske_wahr_aoki, pm_uai1994)
!>                                           [par défaut pm_lieske_wahr_aoki_]
!>[E]   mu             :<pm_reel>            terme central du potentiel (m^3/s^2) [par défaut valeur lue dans COMPAS]
!>[E]   requa          :<pm_reel>            rayon terrestre à l'équateur [m] [par défaut valeur lue dans COMPAS]
!>[E]   apla           :<pm_reel>            aplatissement terrestre [par défaut valeur lue dans COMPAS]
!>[E]   requa_r        :<pm_reel>            rayon terrestre à l'équateur pour les coordonnées station  [m]
!>[E]   apla_r         :<pm_reel>            aplatissement terrestre pour les coordonnées station 
!>[E]   vrot           :<pm_reel>            vitesse de rotation [rad/s] [par défaut MSP_VITROT(eph_terre)]
!>[E]   rep            :<pm_reel,DIM=(10)>   tableau utilisé pour psimu
!>[E]   pole_u         :<pm_reel>            angle u du pole vrai
!>[E]   pole_v         :<pm_reel>            angle v du pole vrai
!>[E]   obli           :<pm_reel>            obliquité
!>[E]   origdat        :<integer>            origine des dates (0=J50, 1=MJD2000)
!>S     bulletin       :<MSP_BULLETIN>       type dérivé contenant les informations liées au bulletin
!
!$Common
!
!$Routines
!- md_jourfrac_joursec
!- MSP_signaler_message
!
!$Include
!
!$Module
!
!$Remarques
!   Certains paramètres sont optionels mais deviennent obligatoires en fonction du type de repère choisi
!   (par exemple la direction pour un repère de type station).
!
!$Mots-cles
!  BULLETIN CREER 
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      real(kind=pm_reel), intent(IN) :: datbul

      type(MSP_TYPREP), intent(IN), optional :: repere
      type(MSP_TYPBUL), intent(IN), optional :: coord

      integer, intent(IN), optional            :: iorb
      real(kind=pm_reel), intent(IN), optional :: param(6)
      integer, intent(IN), optional            :: typrep

      integer, intent(IN), optional            :: ech_temps_bul
      real(kind=pm_reel), intent(IN), optional :: lonref,date_ref,lat,lon,alt
      real(kind=pm_reel), intent(IN), optional :: direction
      integer, intent(IN), optional            :: planete,corcen,cle_date,modprec,ech_temps_rep
      real(kind=pm_reel), intent(IN), optional :: mu,requa,apla,requa_r,apla_r,vrot
      real(kind=pm_reel), intent(IN), optional :: pole_u, pole_v, obli
      real(kind=pm_reel), dimension(10), intent(IN), optional :: rep
      type(tm_jour_sec), intent(in), optional  :: datbul_js, date_refjs
      integer, intent(IN), optional :: origdat

      ! Variables locales
      type(MSP_BULLETIN)   :: bulletin
      type(tm_code_retour) :: code_retour

      type(tm_jour_sec)    :: date_ref_tmp
      integer              :: ech_temps_tmp

      ! Initialisations (pour eviter que ech_temps_tmp garde l'ancienne valeur)
      ech_temps_tmp=pm_TUC
      date_ref_tmp%jour=0
      date_ref_tmp%sec=0.0_PM_REEL

      if (present(datbul_js)) then 
         bulletin%datbul=datbul_js
      else
         call md_jourfrac_joursec(datbul, bulletin%datbul,code_retour)
      endif

      ! Paramètres obligatoires:
      if ( present(ech_temps_bul) ) then
         bulletin%ech_temps_bul = ech_temps_bul
      else
         bulletin%ech_temps_bul = pm_TUC
      endif

      if (PRESENT(coord)) then 
         bulletin%coord = coord
      else
         if ( (.not.present(iorb)).or.(.not.present(param)) ) then
            call MSP_signaler_message (cle_mes="MSP_creer_bulletin_arguments", &
                 partie_variable='"iorb" et "param"', &
                 routine="MSP_creer_bulletin_param")
            return
         end if
         bulletin%coord = MSP_creer_typbul(param, iorb, mu=mu, apla=apla, requa=requa)
         if ( MSP_gen_messages ("MSP_creer_bulletin_param") ) return
      end if

      if (PRESENT(repere)) then 
         bulletin%repere = repere
      else
         ! Si le repere est défini à la date du bulletin, affectation de la date du bulletin
         ! à la date de définition du repère
         if (present(date_ref))     call md_jourfrac_joursec(date_ref, date_ref_tmp,code_retour)
         if (present(date_refjs))   date_ref_tmp = date_refjs
         if (present(ech_temps_rep)) ech_temps_tmp = ech_temps_rep

         bulletin%repere = MSP_creer_typrep(typrep=typrep,  &
              ech_temps_rep=ech_temps_tmp, lonref=lonref, &
              date_refjs=date_ref_tmp, lat=lat, lon=lon, alt=alt, &
              direction=direction, planete=planete, &
              corcen=corcen, cle_date=cle_date, &
              requa_r=requa_r, apla_r=apla_r, vrot=vrot, rep=rep , &
              pole_u=pole_u,pole_v=pole_v,obli=obli,origdat=origdat)
         if ( MSP_gen_messages ("MSP_creer_bulletin_param") ) return
      end if

      

      ! - Si la date du repère est la date du bulletin positionnement de cette date
      if (bulletin%repere%cle_date == pm_autre_date) then
         bulletin%repere%date_ref = bulletin%datbul
         bulletin%repere%ech_temps_rep = bulletin%ech_temps_bul
      endif

      ! Test sur la présence du mode de précession:
      if ( present(modprec) ) then
         bulletin%modprec = modprec
      else if (bulletin%repere%planete == eph_terre) then
         call MSP_signaler_message (cle_mes="MSP_creer_bulletin_modprec_001", &
              partie_variable='"LIESKE"')
         bulletin%modprec = pm_lieske_wahr_aoki
      else
         call MSP_signaler_message (cle_mes="MSP_creer_bulletin_modprec_001", &
              partie_variable='"UAI1994"')
         bulletin%modprec = pm_uai1994
      endif


    end function MSP_creer_bulletin_param



  FUNCTION MSP_creer_bulletin_madona(subr_lect, nom_dom, nacc, section, type_section) result (bulletin)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_creer_bulletin_madona
!
!$Resume
!  Fonction créant un bulletin orbital à partir d'un fichier ou d'un moyen d'acces
!
!$Description
!  Fonction créant un bulletin orbital à partir d'un fichier ou d'un moyen d'acces
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PRIVE
!
!$Usage
!  bulletin = MSP_creer_bulletin_madona(subr_lect, nom_dom, [nacc], [section], [type_section])
!.    type(MSP_BULLETIN) :: bulletin
!.    external subr_lect
!.    character(LEN=*) :: nom_dom
!.    integer :: nacc
!.    integer, dimension(:) :: type_section
!.    character(LEN=MSP_LONG_CHAINE), dimension(:) :: section
!
!$Arguments
!>E/S   subr_lect     :<external>                      Routine externe servant a lire dans le moyen d'acces nacc
!                                          Cette routine peut etre read_GS_BULLETIN_IP si le bulletin 
!                                          contenu dans le moyen d'acces possede un format compatible avec l'objet GS_LIB.
!>E     nom_dom       :<LEN=*>                         nom du domaine de traduction MADONA
!>[E]   nacc          :<integer>                       Numero du moyen d'acces MADONA où lire les informations de bulletin
!>[E]   section       :<LEN=MSP_LONG_CHAINE,DIM=(:)>   Sections à sélectionner pour atteindre la structure bulletin
!>[E]   type_section  :<integer,DIM=(:)>               Type des sections a selectionner 
!>S     bulletin      :<MSP_BULLETIN>                  Structure bulletin telle que définie dans MSP_BULLETIN_DEF.F90
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- md_jourfrac_joursec
!- subr_lect
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  BULLETIN CREER MADONA
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    type(MSP_BULLETIN) :: bulletin
    external subr_lect

    character(LEN=*), intent(IN) :: nom_dom
    integer, intent(IN), optional :: nacc
    integer, dimension(:), intent(IN), optional :: type_section
    character(LEN=MSP_LONG_CHAINE), dimension(:), intent(IN), optional :: section

    ! Variables locales
    integer :: ntab, ier, caster, MSP_iostat
    integer, dimension(:), pointer :: type_section_loc => NULL()
    real(kind=pm_reel)   :: dir_pm   ! conversion en valeur mspro de la direction d'un repère Station
    type(tm_code_retour) :: code_retour
    character(LEN=3) :: chaine_nulle1, chaine_nulle2

    ! - Dansle format GS_LIB, les données station sont dans un tableau
    real(kind=pm_reel), dimension(10)  :: rep

    ! Initialisations
    MSP_iostat = 0
    ! Pour respecter le format d'appel a read_GS_BULLETIN_IP
    chaine_nulle1 = "."
    chaine_nulle2 = ""


    ! Un argument type_section ou section est passé
    if ( PRESENT(section).or.PRESENT(type_section) ) then
       
       if (.not.PRESENT(nacc)) then 
          call MSP_signaler_message (cle_mes="MSP_ERREUR_ARGUMENTS_001", &
               routine="MSP_creer_bulletin_madona", type=MSP_ENUM_ERREUR, &
               partie_variable="section, type_section et nacc")
          return

       ! L'argument nacc est également présent
       else
          
          ! Les deux arguments section et type_section sont présents
          if (PRESENT(section).and.PRESENT(type_section)) then 
                
             ntab = size(section)
             ier = MSP_acc_select(nacc, section, type_section, ntab)
             ! DM1058 - Gestion d'erreur
             if (ier < 0) then
                call MSP_signaler_message (cle_mes="MSP_probleme_select_2", &
                     routine="MSP_creer_bulletin_madona")
             endif
             if (MSP_gen_messages("MSP_creer_bulletin")) return

             
          !  Type_section est présent mais pas section
          else if ( .not.PRESENT(section).and.PRESENT(type_section) ) then
             call MSP_signaler_message (cle_mes="MSP_poussee_cont_007", &
                  routine="section", type=MSP_ENUM_PROPAGATION)
             return
                
          ! section n'est pas présent, mais type_section l'est 
          else if ( (.not.PRESENT(type_section).and.PRESENT(section)) ) then 
             ntab = size(section)
             ALLOCATE(type_section_loc(ntab))
             type_section_loc(:) = ACC_STRUCT
             ier = MSP_acc_select(nacc, section, type_section_loc, ntab)
             ! DM1058 - Gestion d'erreur
             if (ier < 0) then
                call MSP_signaler_message (cle_mes="MSP_probleme_select_2", &
                     routine="MSP_creer_bulletin_madona")
             endif
             DEALLOCATE(type_section_loc,stat=MSP_iostat)
             if (MSP_gen_messages("MSP_creer_bulletin")) return

          end if

       end if

       ! Pas d'argument section ni type_section
    else
       
       ! Dans ce cas nacc est inutile
       if (PRESENT(nacc)) then 
          call MSP_signaler_message (cle_mes="MSP_ERREUR_ARGUMENTS_008", &
               routine="MSP_creer_bulletin_madona", type=MSP_ENUM_WARNING, &
               partie_variable="nacc")
       end if

    end if

    ! Initialisations
    bulletin%repere%station%geod%lat      = 0._pm_reel    
    bulletin%repere%station%geod%long     = 0._pm_reel   
    bulletin%repere%station%geod%haut     = 0._pm_reel     

    ! Valeurs écrasées par la lecture
    bulletin%repere%station%ellips%r_equa = MSP_REQUA_TERRE  
    bulletin%repere%station%ellips%apla   = 1./MSP_APLA_TERRE

    bulletin%repere%station%axe_x         = 0._pm_reel 
    bulletin%repere%pole%u                = 0._pm_reel 
    bulletin%repere%pole%v                = 0._pm_reel
    bulletin%repere%lonref                = 0._pm_reel 
    call md_jourfrac_joursec(0._pm_reel, bulletin%repere%date_ref,code_retour)
    bulletin%repere%obli                  = MSP_OBLIQUITE
    bulletin%repere%origdat               = 0


    caster = 0   ! Pour tous les corps centraux

    call subr_lect(bulletin%coord%iorb,rep(:),bulletin%datbul%jour,bulletin%datbul%sec, &
         bulletin%coord%param, bulletin%coord%mu, bulletin%coord%requa, &
         bulletin%coord%apla, bulletin%repere%station%ellips%r_equa, bulletin%repere%station%ellips%apla , &
         bulletin%repere%planete,  bulletin%repere%corcen, bulletin%repere%cle_date, &
         bulletin%repere%vrot, bulletin%ech_temps_bul, bulletin%repere%obli, &
         bulletin%repere%pole%u, bulletin%repere%pole%v, caster, nom_dom, 0, 1994, chaine_nulle1, chaine_nulle2)
    
    if (bulletin%repere%planete .eq. eph_terre) then
       bulletin%modprec = pm_lieske_wahr_aoki
    else
       bulletin%modprec = pm_uai1994
    end if
!    call md_jourfrac_joursec(datbul, bulletin%datbul,code_retour)

    ! - Décomposition du tableau rep
        bulletin%repere%typrep  = int(rep(1))
        select case(bulletin%repere%typrep)
           case(pm_planeto_ref, pm_planeto_ref_iner, pm_planeto_vrai) 
              bulletin%repere%lonref = rep(3)
           case(pm_topo)
              bulletin%repere%station%geod%lat  = rep(4)
              bulletin%repere%station%geod%long = rep(5)
              bulletin%repere%station%geod%haut = rep(6)
              ! - La direction GS_LIB vaut 1 / 2 / 3 /4
              dir_pm = direction_mspro(int(rep(7)))
              if (MSP_gen_messages("MSP_creer_bulletin_madona - direction")) return
              bulletin%repere%station%axe_x =  dir_pm
         end select
         
         if ( bulletin%repere%cle_date == MSP_ENUM_ECHD_DATE_REF) then
            bulletin%repere%date_ref%jour = int(rep(4))
            bulletin%repere%date_ref%sec  = rep(5)
            bulletin%repere%ech_temps_rep = int(rep(2))
         endif
         
         ! - L'aplatissement mspro est inversée
         if (abs(bulletin%coord%apla) > MSP_epsilon_apla ) then
            bulletin%coord%apla = 1. / bulletin%coord%apla
         else
            bulletin%coord%apla = 999.E10
         endif

         if (abs(bulletin%repere%station%ellips%apla) > MSP_epsilon_apla) then
            bulletin%repere%station%ellips%apla = 1. / bulletin%repere%station%ellips%apla
         else
            bulletin%repere%station%ellips%apla = 999.E10
         endif

         
  end FUNCTION MSP_creer_bulletin_madona
       


  SUBROUTINE  MSP_modifier_bulletin(bulletin, coord, repere, &
       datbul, datbul_js, iorb, param, typrep, rep, &
       ech_temps_bul, lonref, date_ref, date_refjs, lat, lon, alt, direction,  &
       planete, corcen, cle_date, ech_temps_rep, modprec, mu, requa_r, apla_r, requa, apla, vitrot, &
       pole_u, pole_v, obli,origdat)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_modifier_bulletin
!
!$Resume
!  Cette routine permet de modifier une structure bulletin
!
!$Description
!  Cette routine permet de modifier une structure bulletin
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_modifier_bulletin(bulletin, [coord], [repere], &
!.           [datbul], [datbul_js], [iorb], [param], [typrep], [rep], &
!.           [ech_temps_bul], [lonref], [date_ref], [date_refjs], [lat], [lon], [alt], [direction],  &
!.           [planete], [corcen], [cle_date], [ech_temps_rep], [modprec], [mu], [requa_r], [apla_r], [requa], [apla], [vitrot], &
!.           [pole_u], [pole_v], [obli],[origdat])
!.    type(MSP_BULLETIN) :: bulletin
!.    type(MSP_TYPREP) :: repere
!.    type(MSP_TYPBUL) :: coord
!.    real(KIND=PM_REEL) :: datbul
!.    integer :: iorb 
!.    real(KIND=PM_REEL), dimension(6) :: param
!.    integer :: typrep
!.    real(KIND=PM_REEL), dimension(10) :: rep
!.    real(KIND=PM_REEL) :: lonref, date_ref, lat, lon, alt, direction
!.    integer :: planete, corcen, cle_date, ech_temps_bul, modprec, ech_temps_rep
!.    real(KIND=PM_REEL) :: mu, requa_r, apla_r
!.    real(KIND=PM_REEL) :: requa, apla, vitrot
!.    real(KIND=PM_REEL) :: pole_u, pole_v, obli
!.    type(tm_jour_sec) :: datbul_js, date_refjs
!.    integer :: origdat
!
!$Arguments
!>E/S   bulletin       :<MSP_BULLETIN>       Structure bulletin à modifier
!>[E]   coord          :<MSP_TYPBUL>         Structure type bulletin (prioritaire sur les autres arguements de cette sous-structure, voir MSP_modifier_typbul) 
!>[E]   repere         :<MSP_TYPREP>         Structure repère (prioritaire sur les autres arguements de cette sous-structure, voir MSP_modifier_typrep) 
!>[E]   datbul         :<PM_REEL>            date du bulletin [JJ]
!>[E]   datbul_js      :<tm_jour_sec>        date du bulletin jour/secondes 1950
!>[E]   iorb           :<integer>            type de coordonnées (pm_kep, pm_cir,
!>                                        pm_equa, pm_cir_equa, pm_car,
!>                                        pm_hpha, pm_geoc, 
!>                                        MSP_ENUM_RENTREE_SPHERIQUE, pm_geod_meca_vol,
!>                                        pm_geod_gps_ard)
!>[E]   param          :<PM_REEL,DIM=(6)>    coordonnées (cf. définition de iorb)
!>[E]   typrep         :<integer>            type de repère (pm_equa_vrai, pm_equa_moyen, pm_ecli_moy,
!>                                         pm_veis, pm_planeto_ref, pm_planeto_ref_iner, pm_planeto_vrai,
!>                                         pm_equa_uai, pm_topo)
!>[E]   rep            :<PM_REEL,DIM=(10)>   Tableau contenant des infos sur le repère. A utiliser uniquement par PSIMU!
!>[E]   ech_temps_bul  :<integer>            échelle de temps de la date du bulletin (pm_TUC, pm_TE)
!>[E]   lonref         :<PM_REEL>            longitude de référence
!>[E]   date_ref       :<PM_REEL>            date de référence du repère [JJ]
!>[E]   date_refjs     :<tm_jour_sec>        date de référence du repère jour/secondes 1950
!>[E]   lat            :<PM_REEL>            latitude station [rad]
!>[E]   lon            :<PM_REEL>            longitude station[rad]
!>[E]   alt            :<PM_REEL>            altitude station [m]
!>[E]   direction      :<PM_REEL>            direction de la monture de la station:
!>.                                       MSP_ENUM_NORD  -> monture au Nord
!>.                                       MSP_ENUM_SUD   -> monture au Sud
!>.                                       MSP_ENUM_EST   -> monture  Est
!>.                                       MSP_ENUM_OUEST -> monture  Ouest
!>[E]   planete        :<integer>            planète (eph_mercure,eph_venus,eph_terre,eph_mars,
!>                                               eph_jupiter,eph_saturne,eph_uranus,
!>                                               eph_neptune,eph_pluton)
!>[E]   corcen         :<integer>            corps central (eph_mercure,eph_venus,eph_terre,eph_mars,
!>                                          eph_jupiter,eph_saturne,eph_uranus,eph_neptune,eph_pluton,
!>                                          eph_soleil)
!>[E]   cle_date       :<integer>            type de date utilisé pour l'origine des dates (pm_1janvier1950_00h00, pm_1janvier2000_12h00,
!>                                     pm_autre_date, MSP_ENUM_ECHD_DATE_REF)
!>[E]   ech_temps_rep  :<integer>            echelle de temps de la date de référence du repère
!>[E]   modprec        :<integer>            mode de précession (pm_lieske_wahr_aoki, pm_uai1994)
!>[E]   mu             :<PM_REEL>            terme central du potentiel (m^3/s^2)
!>[E]   requa_r        :<PM_REEL>            rayon terrestre à l'équateur pour les coordonnées station  [m]
!>[E]   apla_r         :<PM_REEL>            aplatissement terrestre pour les coordonnées station 
!>[E]   requa          :<PM_REEL>            rayon terrestre à l'équateur [m]
!>[E]   apla           :<PM_REEL>            aplatissement terrestre
!>[E]   vitrot         :<PM_REEL>            vitesse de rotation [rad/s]
!>[E]   pole_u         :<PM_REEL>            angle u du pole vrai
!>[E]   pole_v         :<PM_REEL>            angle v du pole vrai
!>[E]   obli           :<PM_REEL>            obliquite
!>[E]   origdat        :<integer>            origine des dates (0=J50, 1=MJD2000)
!
!$Common
!
!$Routines
!- md_jourfrac_joursec
!- MSP_modifier_typrep
!- MSP_modifier_typbul
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  BULLETIN MODIFIER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


    implicit none

    type(MSP_BULLETIN), intent(INOUT) :: bulletin
    type(MSP_TYPREP), intent(IN), optional :: repere
    type(MSP_TYPBUL), intent(IN), optional :: coord
    real(KIND=PM_REEL), intent(IN), optional :: datbul
    integer, intent(IN), optional            :: iorb         
    real(KIND=PM_REEL), dimension(6), intent(IN), optional :: param
    integer, intent(IN), optional            :: typrep
    real(KIND=PM_REEL), dimension(10), intent(IN), optional :: rep

    real(KIND=PM_REEL), intent(IN), optional :: lonref, date_ref, lat, lon, alt, direction
    integer, intent(IN), optional            :: planete, corcen, cle_date, ech_temps_bul, modprec, ech_temps_rep
    real(KIND=PM_REEL), intent(IN), optional :: mu, requa_r, apla_r
    real(KIND=PM_REEL), intent(IN), optional :: requa, apla, vitrot
    real(KIND=PM_REEL), intent(IN), optional :: pole_u, pole_v, obli
    type(tm_jour_sec), intent(IN), optional  :: datbul_js, date_refjs
    integer, intent(IN), optional :: origdat

    ! variables locales
    type(tm_code_retour)      :: code_retour

    if ( PRESENT(datbul) ) call md_jourfrac_joursec(datbul,bulletin%datbul,code_retour)
    if ( PRESENT(datbul_js) ) bulletin%datbul = datbul_js
    if ( PRESENT(ech_temps_bul) ) bulletin%ech_temps_bul = ech_temps_bul
    if ( PRESENT(modprec) )   bulletin%modprec   = modprec

    if ( PRESENT(repere) ) then 
       bulletin%repere = repere
    else
       call MSP_modifier_typrep(bulletin%repere,  typrep=typrep, &
            ech_temps_rep=ech_temps_rep, lonref=lonref, date_ref=date_ref, &
            date_refjs=date_refjs, lat=lat, lon=lon, alt=alt, &
            direction=direction,  &
            planete=planete, corcen=corcen, cle_date=cle_date, &
            requa_r=requa_r, apla_r=apla_r, vrot=vitrot, rep=rep, &
            pole_u=pole_u, pole_v=pole_v, obli=obli,origdat=origdat)
       if ( MSP_gen_messages ("MSP_modifier_bulletin") ) return
    end if

    ! -- Mise à jour de la date du repère si c'est la date du bulletin
    if (bulletin%repere%cle_date == pm_autre_date) then
       bulletin%repere%date_ref    = bulletin%datbul
       if ( PRESENT(ech_temps_bul) ) bulletin%repere%ech_temps_rep = ech_temps_bul
    endif
    
    if ( PRESENT(coord) ) then 
       bulletin%coord = coord
    else
       call MSP_modifier_typbul(bulletin%coord, iorb=iorb, param=param, mu=mu, apla=apla, requa=requa)
       if ( MSP_gen_messages ("MSP_modifier_bulletin") ) return
    end if



  end SUBROUTINE MSP_modifier_bulletin


  SUBROUTINE MSP_modifier_typbul(coord, iorb, param, mu, apla, requa)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_modifier_typbul
!
!$Resume
!  Cette routine modifie le contenu d'une structure coordonnées
!
!$Description
!  Cette routine modifie le contenu d'une structure coordonnées
!
!$Auteur
!  Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_modifier_typbul(coord, [iorb], [param], [mu], [apla], [requa])
!.    type(MSP_TYPBUL) :: coord
!.    integer :: iorb 
!.    real(KIND=PM_REEL), dimension(6) :: param
!.    real(KIND=PM_REEL) :: mu, requa, apla
!
!$Arguments
!>E/S   coord  :<MSP_TYPBUL>        Coordonnées à modifier
!>[E]   iorb   :<integer>           type de coordonnées (pm_kep, pm_cir,
!>                                   pm_equa, pm_cir_equa, pm_car,
!>                                   pm_hpha, pm_geoc,
!>                                   MSP_ENUM_RENTREE_SPHERIQUE, pm_geod_meca_vol,
!>                                   pm_geod_gps_ard)
!>[E]   param  :<PM_REEL,DIM=(6)>   coordonnées (cf. définition de iorb)
!>[E]   mu     :<PM_REEL>           terme central du potentiel (m^3/s^2)    
!>[E]   apla   :<PM_REEL>           rayon terrestre à l'équateur [m]   
!>[E]   requa  :<PM_REEL>           aplatissement terrestre
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
! COORDONNEES MODIFIER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    type(MSP_TYPBUL), intent(INOUT) :: coord
    integer, intent(IN), optional            :: iorb         
    real(KIND=PM_REEL), dimension(6), intent(IN), optional :: param
    real(KIND=PM_REEL), intent(IN), optional :: mu, requa, apla

    if (present(iorb))  coord%iorb  = iorb
    if (present(param)) coord%param = param
    if (present(mu))    coord%mu    = mu
    if (present(apla))  coord%apla  = apla
    if (present(requa)) coord%requa = requa

    ! Le mode MSP_ENUM_RENTREE_SPHERIQUE est ramené à MSP_ENUM_RENTREE_ELLIPSOIDIQUE avec applatissement nul
      if (coord%iorb==MSP_ENUM_RENTREE_SPHERIQUE) then
         coord%iorb = pm_geod_meca_vol
         coord%apla = 0._pm_reel
      endif

  end SUBROUTINE MSP_modifier_typbul




  SUBROUTINE MSP_modifier_typrep(repere, typrep, cle_date, planete, &
        date_ref,date_refjs, ech_temps_rep,lonref,lat, lon,alt,direction,&
        corcen,requa_r,apla_r,vrot, rep, pole_u, pole_v, obli,origdat)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_modifier_typrep
!
!$Resume
!  Cette routine permet de modifier une structure repère
!
!$Description
!  Cette routine permet de modifier une structure repère
!
!$Auteur
!  Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_modifier_typrep(repere, [typrep], [cle_date], [planete], &
!.            [date_ref],[date_refjs], [ech_temps_rep],[lonref],[lat], [lon],[alt],[direction],&
!.            [corcen],[requa_r],[apla_r],[vrot], [rep], [pole_u], [pole_v], [obli],[origdat])
!.    type(MSP_TYPREP) :: repere
!.    integer :: typrep
!.    integer :: cle_date, planete, corcen, ech_temps_rep
!.    real(kind=pm_reel) :: date_ref, lonref, lat,lon,alt,direction
!.    real(kind=pm_reel) :: requa_r,apla_r,vrot, pole_u,pole_v,obli
!.    real(kind=pm_reel), dimension(10) :: rep
!.    type(tm_jour_sec) :: date_refjs
!.    integer :: origdat
!
!$Arguments
!>E/S   repere         :<MSP_TYPREP>         Structure repère à modifier
!>[E]   typrep         :<integer>            type de repère (pm_equa_vrai, pm_equa_moyen, pm_ecli_moy,
!>                                         pm_veis, pm_planeto_ref, pm_planeto_ref_iner, pm_planeto_vrai,
!>                                         pm_equa_uai,  pm_topo)
!>[E]   cle_date       :<integer>            type de date utilisé pour l'origine des dates (pm_1janvier1950_00h00, pm_1janvier2000_12h00,
!>                                     pm_autre_date, MSP_ENUM_ECHD_DATE_REF)
!>[E]   planete        :<integer>            planète (eph_mercure,eph_venus,eph_terre,eph_mars,
!>                                               eph_jupiter,eph_saturne,eph_uranus,
!>                                               eph_neptune,eph_pluton)
!>[E]   date_ref       :<pm_reel>            date de référence [JJ]
!>[E]   date_refjs     :<tm_jour_sec>        date de référence en jour/sec jj50
!>[E]   ech_temps_rep  :<integer>            echelle de temps de la date de référence du repère
!>[E]   lonref         :<pm_reel>            longitude de référence
!>[E]   lat            :<pm_reel>            latitude station  [rad]
!>[E]   lon            :<pm_reel>            longitude station [rad]
!>[E]   alt            :<pm_reel>            altitude station  [m]
!>[E]   direction      :<pm_reel>            direction de la monture de la station:
!>.                                       MSP_NORD  -> monture au Nord
!>.                                       MSP_SUD   -> monture au Sud
!>.                                       MSP_EST   -> monture  Est
!>.                                       MSP_OUEST -> monture  Ouest
!>[E]   corcen         :<integer>            corps central (eph_mercure,eph_venus,eph_terre,eph_mars,
!>                                          eph_jupiter,eph_saturne,eph_uranus,eph_neptune,eph_pluton,
!>                                          eph_soleil)
!>[E]   requa_r        :<pm_reel>            rayon terrestre à l'équateur pour les coordonnées station  [m]
!>[E]   apla_r         :<pm_reel>            aplatissement terrestre pour les coordonnées station 
!>[E]   vrot           :<pm_reel>            Vitesse de rotation du corps central
!>[E]   rep            :<pm_reel,DIM=(10)>   Tableau contenant des infos sur le repère. A utiliser uniquement par PSIMU!
!>[E]   pole_u         :<pm_reel>            angle u du pole vrai
!>[E]   pole_v         :<pm_reel>            angle v du pole vrai
!>[E]   obli           :<pm_reel>            obliquite
!>[E]   origdat        :<integer>            origine des dates (0=J50, 1=MJD2000)
!
!$Common
!
!$Routines
!- md_jourfrac_joursec
!- MSP_signaler_message
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
! REPERE MODIFIER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    type(MSP_TYPREP), intent(INOUT) :: repere

     integer, intent(IN) , optional         :: typrep
     integer, intent(IN), optional ::   cle_date, planete, corcen, ech_temps_rep
     real(kind=pm_reel), intent(IN), optional ::     date_ref, lonref, lat,lon,alt,direction
     real(kind=pm_reel), intent(IN), optional ::    requa_r,apla_r,vrot, pole_u,pole_v,obli
     real(kind=pm_reel), dimension(10),intent(IN), optional :: rep
     type(tm_jour_sec), intent(IN), optional :: date_refjs
     integer, intent(IN), optional :: origdat

     ! Variables locales
     character(LEN=10)         :: cier
     type(tm_code_retour)      :: code_retour


     if (present(typrep ))   repere%typrep              = typrep 
     if (present(cle_date )) repere%cle_date            = cle_date 
     if (present(planete ))  repere%planete             = planete 
     if (present(date_ref ))  call md_jourfrac_joursec(date_ref, repere%date_ref,code_retour)
     if (present(date_refjs )) repere%date_ref          = date_refjs
     if (present(ech_temps_rep)) repere%ech_temps_rep   = ech_temps_rep
     if (present(lonref ))   repere%lonref              = lonref
     if (present(vrot))      repere%vrot                = vrot
     if (present( corcen ))  repere%corcen              = corcen
     if (present(obli ))     repere%obli                = obli
     if (present(lat ))      repere%station%geod%lat    = lat
     if (present(lon ))      repere%station%geod%long   = lon
     if (present(alt ))      repere%station%geod%haut   = alt
     if (present(direction ))repere%station%axe_x       = direction
     if (present(requa_r ))  repere%station%ellips%r_equa= requa_r
     if (present(apla_r ))   repere%station%ellips%apla = apla_r
     if (present(pole_u ))   repere%pole%u = pole_u
     if (present(pole_v ))   repere%pole%v = pole_v
     if (present(origdat) )  repere%origdat             = origdat

     ! - Appel par PSIMU : les caractéristiques du repères sont dans le tableau rep(10)
     if (present(rep)) then
        repere%typrep  = int(rep(1))
        select case(repere%typrep)
           case(pm_planeto_ref, pm_planeto_ref_iner, pm_planeto_vrai) 
              repere%lonref = rep(3)
           case(pm_topo)
              repere%station%geod%lat  = rep(4)
              repere%station%geod%long = rep(5)
              repere%station%geod%haut = rep(6)
              repere%station%axe_x     = rep(7)
         end select
         
         if (repere%cle_date == MSP_ENUM_ECHD_DATE_REF) then
            repere%date_ref%jour = int(rep(4))
            repere%date_ref%sec  = rep(5)
            repere%ech_temps_rep = int(rep(2))
         endif
      endif


      ! -- Mise en conformité des champs cle_date / date_ref
      if (present(cle_date)) then
         repere%cle_date=cle_date

         select case(cle_date)

            case(pm_1janvier1950_00h00)
               call md_jourfrac_joursec(MSP_1950, repere%date_ref,code_retour)
            case(pm_1janvier2000_12h00)
               call md_jourfrac_joursec(MSP_JJ2000, repere%date_ref,code_retour)
            case(MSP_ENUM_ECHD_DATE_REF)
               ! Si rep etait present l erreur ne doit pas etre declenchee (FA 1156)
               if(.not.present(date_ref).and..not.present(date_refjs).and..not.present(rep)) then
               ! Date de reference obligatoire
                   call MSP_signaler_message (cle_mes="MSP_creer_bulletin_rep_007",routine="MSP_modifier_typrep")
                  return
               end if
            case(pm_autre_date)
                  
            case default
            ! -- Code inconnu
               write (cier,'(i6.6)') cle_date
               call MSP_signaler_message (cle_mes="MSP_creer_bulletin_rep_008",partie_variable=cier)
               return
         end select

      else
      ! Si cle_date est absent mais une date_ref est fournie
         if (present(date_ref))   repere%cle_date = MSP_ENUM_ECHD_DATE_REF
         if (present(date_refjs)) repere%cle_date = MSP_ENUM_ECHD_DATE_REF
      endif


     ! Vérification de la compatibilité entre le type de repère et la date de référence
     if (.not.verif_typrep_date(repere%typrep,cle_date=repere%cle_date)) return

     if (present(planete)) then
     ! Vérification de la compatibilité entre le type de repère et la planete
        if (.not.verif_typrep_planete(repere%typrep,planete=planete))   return
     endif


  end SUBROUTINE MSP_modifier_typrep


  function MSP_convertir_bulletin(bulletin_in, iorb, ech_temps_bul, &
       modprec,mu,requa, apla, typrep, &
       cle_date, date_ref, date_refjs, ech_temps_rep, &
       planete,lonref,lat,lon,alt, direction, requa_r, apla_r, obli, &
       pole_u, pole_v, origdat, vrot, repere) result(bulletin_out)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_convertir_bulletin
!
!$Resume
!  Cette routine permet de réaliser la conversion de bulletin (coordonnées et repère)
!
!$Description
!  Cette routine permet de réaliser la conversion de bulletin (coordonnées et repère)
!
!$Auteur
!  25/04/2000 - Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  bulletin_out = MSP_convertir_bulletin(bulletin_in, [iorb], [ech_temps_bul], &
!.           [modprec],[mu],[requa], [apla], [typrep], &
!.           [cle_date], [date_ref], [date_refjs], [ech_temps_rep], &
!.           [planete],[lonref],[lat],[lon],[alt], [direction], [requa_r], [apla_r], [obli], &
!.           [pole_u], [pole_v],[origdat], [vrot], [repere])
!.    type(MSP_BULLETIN) :: bulletin_in
!.    integer :: iorb, typrep, ech_temps_bul
!.    integer :: planete, cle_date, ech_temps_rep, modprec
!.    real(KIND=PM_REEL) :: lonref, date_ref
!.    real(KIND=PM_REEL) :: lat, lon, alt, direction
!.    real(KIND=PM_REEL) :: requa, apla, requa_r, apla_r, mu
!.    real(KIND=PM_REEL) :: pole_u, pole_v, vrot, obli
!.    type(tm_jour_sec) :: date_refjs
!.    integer :: origdat
!.    type(MSP_TYPREP) :: repere
!.    type(MSP_BULLETIN) :: bulletin_out
!
!$Arguments
!>E     bulletin_in    :<MSP_BULLETIN>   Structure bulletin de départ pour la conversion
!>[E]   iorb           :<integer>        type de coordonnées vers lesquelles on souhaite convertir le bulletin (pm_kep, 
!>                                        pm_cir, pm_equa, pm_cir_equa, pm_car,
!>                                        pm_hpha, pm_geoc, 
!>                                        MSP_ENUM_RENTREE_SPHERIQUE, pm_geod_meca_vol,
!>                                        pm_geod_gps_ard)
!>[E]   ech_temps_bul  :<integer>        échelle de temps (de la date du bulletin) dans laquelle on souhaite convertir le bulletin 
!>                                      (pm_TUC, pm_TE)
!>[E]   modprec        :<integer>        modele de precession 
!>[E]   mu             :<PM_REEL>        terme central du potentiel utilisé dans les calculs de conversion (m^3/s^2)
!>[E]   requa          :<PM_REEL>        rayon terrestre à l'équateur utilisé dans les calculs de conversion [m]
!>[E]   apla           :<PM_REEL>        aplatissement terrestre utilisé dans les calculs de conversion 
!>[E]   typrep         :<integer>        type de repère dans lequel on souhaite convertir le bulletin (pm_equa_vrai, pm_equa_moyen, 
!>                                         pm_ecli_moy, pm_veis, pm_planeto_ref, pm_planeto_ref_iner, pm_planeto_vrai,
!>                                         pm_equa_uai, pm_topo)
!>[E]   cle_date       :<integer>        type de date utilisé pour la définition du repère :
!.                                              pm_1janvier1950_00h00  date du repère = 1/1/1950 à 0h00         
!.                                              pm_1janvier2000_12h00  date du repère = 1/1/2000 à 12h00 (18262.5 JJCNES)         
!.                                              MSP_ENUM_ECHD_DATE_REF date du repère fixée par une date de référence         
!.                                              pm_autre_date          date du repère fixée par la date du bulletin  
!>[E]   date_ref       :<PM_REEL>        date de référence du repère dans lequel on souhaite convertir le bulletin [JJ]
!>[E]   date_refjs     :<tm_jour_sec>    date de référence du repère dans lequel on souhaite convertir le bulletin jour/secondes 1950
!>[E]   ech_temps_rep  :<integer>        echelle de temps de la date de référence du repère dans lequel on souhaite convertir le bulletin
!>[E]   planete        :<integer>        planète autour de laquelle on souhaite convertir le bulletin_out (eph_mercure,eph_venus,
!>                                               eph_terre,eph_mars,eph_jupiter,eph_saturne,eph_uranus,
!>                                               eph_neptune,eph_pluton)
!>[E]   lonref         :<PM_REEL>        longitude de référence du repère dans lequel on souhaite convertir le bulletin
!>[E]   lat            :<PM_REEL>        latitude station ou rampe dans laquelle on souhaite convertir le bulletin [rad]
!>[E]   lon            :<PM_REEL>        longitude station ou rampe dans laquelle on souhaite convertir le bulletin [rad]
!>[E]   alt            :<PM_REEL>        altitude station ou rampe dans laquelle on souhaite convertir le bulletin [m]
!>[E]   direction      :<PM_REEL>        direction de la monture de la station:
!>.                                       MSP_NORD  -> monture au Nord
!>.                                       MSP_SUD   -> monture au Sud
!>.                                       MSP_EST   -> monture au Est111
!>.                                       MSP_OUEST -> monture au Ouest
!>[E]   requa_r        :<PM_REEL>        rayon terrestre à l'équateur utilisé pour le calcul des coordonnées de la station 
!>                                   dans laquelle on souhaite convertir le bulletin [m]
!>[E]   apla_r         :<PM_REEL>        aplatissement terrestre  utilisé pour le calcul des coordonnées de la station
!>                                   dans laquelle on souhaite convertir le bulletin [m]
!>[E]   obli           :<PM_REEL>        valeur de l'obliquite [rad] 
!>[E]   pole_u         :<PM_REEL>        angle u du pole vrai [rad]
!>[E]   pole_v         :<PM_REEL>        angle v du pole vrai [rad]
!>[E]   origdat        :<integer>        origine des dates (0=J50, 1=MJD2000)
!>[E]   vrot           :<PM_REEL>        Vitesse de rotation de la planete de reference du repere
!>[E]   repere         :<MSP_TYPREP>     Structure repère (prioritaire sur les autres arguements de cette sous-structure) 
!>S     bulletin_out   :<MSP_BULLETIN>   Structure bulletin de sortie après conversion
!
!$Common
!
!$Routines
!- MSP_modifier_bulletin
!- mx_var
!- MSP_signaler_message
!#V
!- MSP_conv_typrep_jjsec
!#
!
!$Include
!
!$Module
!
!$Remarques
!. Cette fonction s'emploie en spécifiant les caractéristiques du bulletin en entrée que l'on souhaite changer, et
!  en indiquant leur nouvelle valeur.
!. Les modeles de precession doivent etre coherent avec la planete de reference du repere (LIESKE pour la Terre,
!  modele UAI pour les autres planetes)
!. Les angles u/v du pole vrai et la vitesse de rotation doivent etre mis a jour quand on change de planete de reference 
!
!$Mots-cles
!  BULLETIN CONVERTIR
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! parametres
    type(MSP_BULLETIN), intent(IN) :: bulletin_in
    integer, intent(IN), optional :: iorb, typrep, ech_temps_bul
    integer, intent(IN), optional :: planete, cle_date, ech_temps_rep, modprec
    real(KIND=PM_REEL), intent(IN), optional :: lonref, date_ref
    real(KIND=PM_REEL), intent(IN), optional :: lat, lon, alt, direction
    real(KIND=PM_REEL), intent(IN), optional :: requa, apla, requa_r, apla_r, mu
    real(KIND=PM_REEL), intent(IN), optional :: pole_u, pole_v, vrot, obli
    type(tm_jour_sec),  intent(IN), optional :: date_refjs
    integer, intent(IN), optional            :: origdat
    type(MSP_TYPREP), intent(IN), optional   :: repere

    type(MSP_BULLETIN)                       :: bulletin_out

    ! Variables locales
    integer :: iorbi
    real(KIND=PM_REEL), dimension(6) :: parami, paramo

    logical :: test_change, test_change_param, edateref, eplanete
    type(MSP_BULLETIN)   :: bulletin_tmp
    type(tm_code_retour) :: code_retour
    type(tm_ellipsoide)  :: ellips
    integer :: ech_temps_e, ech_temps_s
    real(KIND=PM_REEL)   :: lonref_e, lonref_s, dir_gslib,diffdate
    type(tm_jour_sec)    :: dateref_e, dateref_s

    ! -- Variables pour la conversion en paramètres MSPRO
    type(tm_pole_uv) :: pole_in, pole_out
    real(KIND=PM_REEL) :: obli_tmp
    real(KIND=PM_REEL), dimension(9) :: stra_e, stra_s

    test_change = .false.
    test_change_param = .false.

    ! - Initialisation du bulletin de sortie avec les valeurs du bulletin d'entrée
    bulletin_tmp = bulletin_in

    ! - Modification des paramètres de sortie avec les arguments de l'appel
    call MSP_modifier_bulletin(bulletin_tmp,iorb=iorb,ech_temps_bul=ech_temps_bul,&
         modprec=modprec, mu=mu, requa=requa, apla=apla, &
         typrep=typrep, cle_date=cle_date,date_ref=date_ref,date_refjs=date_refjs,&
         ech_temps_rep=ech_temps_rep, planete=planete, &
         lonref=lonref, lat=lat, lon=lon, alt=alt, direction=direction,  &
         requa_r=requa_r, apla_r=apla_r, obli=obli,pole_u=pole_u,pole_v=pole_v, &
         origdat=origdat, vitrot=vrot, repere=repere)


    ! -- Des paramètres ont-ils été modifiés ?
    test_change = .not.(comparer_bulletin(bulletin_in, bulletin_tmp))
    test_change = test_change .or. &
         .not.(comparer_repere(bulletin_in%repere, bulletin_tmp%repere))

    test_change_param = .not.comparer_coord(bulletin_in%coord  ,  bulletin_tmp%coord)


    if (present(obli)) then
       obli_tmp = obli
    else
       obli_tmp = bulletin_in%repere%obli
    endif

    if (test_change.or.test_change_param) then 
      
      if (test_change) then 

         ! >>>>>>>> PASSAGE EN MODE CARTESIENS
         if (bulletin_in%coord%iorb /= pm_car) then 
            iorbi = pm_car

            ellips%r_equa = bulletin_in%coord%requa
            ellips%apla   = bulletin_in%coord%apla

            call mx_var(bulletin_in%coord%iorb,bulletin_in%coord%param,iorbi,parami,&
                 code_retour, mu=bulletin_in%coord%mu,ellips=ellips)

            if (code_retour%valeur /= pm_OK) then
            ! Le message "trop de paramètres est filtré 
               if (code_retour%valeur /= pm_warn_para_opt_trop ) &
                    call MSP_signaler_message (ier_mslib=code_retour)
               if (MSP_ERREUR) then
                  call MSP_signaler_message (cle_mes="MSP_convertir_bulletin_007")
                  return
               endif
            endif

         else
            iorbi = bulletin_in%coord%iorb
            parami(:) = bulletin_in%coord%param(:)
         end if

         ! >>>>>>>> CHANGEMENT DE REPERE
         ! -- Repere d'entrée
         if (bulletin_in%repere%cle_date == pm_autre_date) then
            dateref_e = bulletin_in%datbul
            ech_temps_e = bulletin_in%ech_temps_bul
         else
            dateref_e = bulletin_in%repere%date_ref
            ech_temps_e = bulletin_in%repere%ech_temps_rep
         endif
         lonref_e = bulletin_in%repere%lonref

         ! - Repere de sortie
         if (bulletin_in%repere%cle_date == pm_autre_date) then
            dateref_s = bulletin_tmp%datbul
            ech_temps_s = bulletin_tmp%ech_temps_bul
         else
            dateref_s = bulletin_tmp%repere%date_ref
            ech_temps_s = bulletin_tmp%repere%ech_temps_rep
         endif
         lonref_s = bulletin_tmp%repere%lonref

        ! - Recomposition du tableau strae en entrée
        select case(bulletin_in%repere%typrep)
           case(pm_planeto_ref, pm_planeto_ref_iner, pm_planeto_vrai) 
               stra_e(2) = bulletin_in%repere%lonref
           case(pm_topo)
                stra_e(3) = bulletin_in%repere%station%geod%lat
                stra_e(4) = bulletin_in%repere%station%geod%long
                stra_e(5) = bulletin_in%repere%station%geod%haut
                dir_gslib = direction_gslib(bulletin_in%repere%station%axe_x)
                if (MSP_gen_messages("MSP_convertir_bulletin - direction en entrée -")) return
                stra_e(6) = dir_gslib
         end select       

        ! - Recomposition du tableau stras en sortie
        select case(bulletin_tmp%repere%typrep)
           case(pm_planeto_ref, pm_planeto_ref_iner, pm_planeto_vrai) 
               stra_s(2) = bulletin_tmp%repere%lonref
           case(pm_topo)
                stra_s(3) = bulletin_tmp%repere%station%geod%lat
                stra_s(4) = bulletin_tmp%repere%station%geod%long
                stra_s(5) = bulletin_tmp%repere%station%geod%haut
                dir_gslib = direction_gslib(bulletin_tmp%repere%station%axe_x)
                if (MSP_gen_messages("MSP_convertir_bulletin - direction en sortie -")) return
                stra_s(6) = dir_gslib
         end select

         ! si valeur de pole non definies prendre des valeurs par defaut
         pole_in  = bulletin_in%repere%pole
         if (pole_in%u == 0._PM_REEL.and.pole_in%v == 0._PM_REEL) then
            pole_in%u=MSP_poleu2000((bulletin_in%repere%planete-99)/100)
            pole_in%v=MSP_polev2000((bulletin_in%repere%planete-99)/100)
         endif

         pole_out = bulletin_tmp%repere%pole
         if (pole_out%u == 0._PM_REEL.and.pole_out%v == 0._PM_REEL) then
            pole_out%u=MSP_poleu2000((bulletin_tmp%repere%planete-99)/100)
            pole_out%v=MSP_polev2000((bulletin_tmp%repere%planete-99)/100)
         endif

         ! si les dates de references sont egales et les corps egaux
         ! prendre des poles egaux (valeurs de sortie) sinon mx_rep sort en
         ! en erreur
         edateref = (bulletin_in%repere%cle_date == bulletin_tmp%repere%cle_date)
         if (bulletin_in%repere%cle_date == MSP_ENUM_ECHD_DATE_REF.or.&
              bulletin_in%repere%cle_date == pm_autre_date) then
            edateref = (dateref_e%jour == dateref_s%jour)
            edateref = edateref.and.(dateref_e%sec == dateref_s%sec)
         endif
         eplanete = (bulletin_in%repere%planete == bulletin_tmp%repere%planete)
         if (edateref.and.eplanete) pole_in = pole_out

         call MSP_conv_typrep_jjsec(param_in = parami , &
              typrep_e = bulletin_in%repere%typrep , &
              cle_date_e =bulletin_in%repere%cle_date , &
              planete_e = bulletin_in%repere%planete , dateref_e =dateref_e , &
              ech_temps_e = ech_temps_e, &
              lonref_e = lonref_e ,  strae = stra_e , &
              requa_e = bulletin_in%repere%station%ellips%r_equa, &
              apla_e  = bulletin_in%repere%station%ellips%apla, &
              typrep_s = bulletin_tmp%repere%typrep , &
              cle_date_s = bulletin_tmp%repere%cle_date , &
              planete_s = bulletin_tmp%repere%planete, dateref_s = dateref_s, &
              ech_temps_s = ech_temps_s, &
              lonref_s = lonref_s ,  stras = stra_s , &
              requa_s =bulletin_tmp%repere%station%ellips%r_equa , &
              apla_s = bulletin_tmp%repere%station%ellips%apla , &
              vrot = bulletin_in%repere%vrot , &
              param_out = paramo , &
              obli = bulletin_in%repere%obli, obli_out = obli_tmp , &
              vrot_out = bulletin_tmp%repere%vrot, &
              pole_in = pole_in, pole_out = pole_out, & 
              origdat=bulletin_in%repere%origdat,&
              modprec_in=bulletin_in%modprec, &
              modprec_out=bulletin_tmp%modprec)

         if (MSP_gen_messages("MSP_convertir_bulletin")) return

      else
         paramo(:) = bulletin_in%coord%param(:)
         iorbi = bulletin_in%coord%iorb
      end if

      ! -- Le bulletin de sortie est exprimé avec le type de paramètre de sortie
      ellips%r_equa = bulletin_tmp%coord%requa
      ellips%apla   = bulletin_tmp%coord%apla


      call mx_var (iorbi, paramo,bulletin_tmp%coord%iorb,bulletin_tmp%coord%param, &
           code_retour, mu=bulletin_in%coord%mu,ellips=ellips)

      if (code_retour%valeur /= pm_OK) then
         ! Le message "trop de paramètres est filtré 
         if (code_retour%valeur /= pm_warn_para_opt_trop ) &
              call MSP_signaler_message (ier_mslib=code_retour)

         if (MSP_ERREUR) then
            call MSP_signaler_message (cle_mes="MSP_convertir_bulletin_007")
            return
         end if
      endif

   end if

   ! Changement d'origine des dates
   if (bulletin_tmp%repere%origdat /= bulletin_in%repere%origdat) then
      diffdate = (MSP_1950jour - MSP_2000jour) * &
           (bulletin_in%repere%origdat-bulletin_tmp%repere%origdat)
      bulletin_tmp%repere%date_ref%jour = bulletin_tmp%repere%date_ref%jour-nint(diffdate)
      bulletin_tmp%datbul%jour = bulletin_tmp%datbul%jour-nint(diffdate)
      diffdate = (MSP_1950sec - MSP_2000sec) * &
           (bulletin_in%repere%origdat-bulletin_tmp%repere%origdat)
      bulletin_tmp%repere%date_ref%sec = bulletin_tmp%repere%date_ref%sec-nint(diffdate)
      bulletin_tmp%datbul%sec = bulletin_tmp%datbul%sec-diffdate
   endif
   bulletin_out = bulletin_tmp

 end function MSP_convertir_bulletin

logical function comparer_repere(repere_1, repere_2) result(retour)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  comparer_repere
!
!$Resume
!         Compare les champs de deux structures MSP_TYPREP
!
!$Description
!         Compare tous les champs de deux structures MSP_TYPREP
!
!$Auteur
!   Didier Semeux (SchlumbergerSema)
!
!$Acces
!  PRIVE
!
!$Usage
!  retour = comparer_repere(repere_1, repere_2)
!.    type(MSP_TYPREP) :: repere_1, repere_2
!
!$Arguments
!>E     repere_1  :<MSP_TYPREP>   Repère MECASPA
!>E     repere_2  :<MSP_TYPREP>   Repère MECASPA
!>S     retour    :<logical>      vaut .true. si les deux structures sont identiques
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
  
  type(MSP_TYPREP), intent(IN) :: repere_1, repere_2

  ! Variables locales

  real (kind=PM_REEL) :: eps

  retour  = .true.

  eps = abs(repere_1%date_ref%sec - repere_2%date_ref%sec) 
  if (repere_1%date_ref%jour /= repere_2%date_ref%jour) retour= .false.
  if (eps > MSP_epsilon_date) retour= .false.

  if (repere_1%typrep /= repere_2%typrep ) retour= .false.
  if (repere_1%cle_date/= repere_2%cle_date) retour= .false.

  if (repere_1%origdat/= repere_2%origdat) retour= .false.

  if (repere_1%planete/= repere_2%planete) retour= .false.
  if (repere_1%corcen/= repere_2%corcen) retour= .false.
  if (repere_1%ech_temps_rep/= repere_2%ech_temps_rep) retour= .false.
  if (repere_1%obli/= repere_2%obli ) retour= .false.

  if (repere_1%vrot/= repere_2%vrot ) retour= .false.
  if (repere_1%lonref/= repere_2%lonref ) retour= .false.

  if (repere_1%pole%u/= repere_2%pole%u ) retour= .false.
  if (repere_1%pole%v/= repere_2%pole%v ) retour= .false.

  if (repere_1%station%geod%lat /= repere_2%station%geod%lat ) retour= .false.
  if (repere_1%station%geod%long /= repere_2%station%geod%long ) retour= .false.
  if (repere_1%station%geod%haut /= repere_2%station%geod%haut ) retour= .false.
  if (repere_1%station%ellips%r_equa /= repere_2%station%ellips%r_equa ) retour= .false.
  if (repere_1%station%ellips%apla /= repere_2%station%ellips%apla ) retour= .false.
  if (repere_1%station%axe_x /= repere_2%station%axe_x ) retour= .false.

end function comparer_repere

logical function comparer_coord(coord_1, coord_2) result(retour)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  comparer_coord
!
!$Resume
!      Compare les champs de deux structure MSP_TYPBUL
!
!$Description
!      Compare tous les champs de deux structure MSP_TYPBUL sauf le champ
!      param
!
!$Auteur
!      Didier Semeux (SchlumbergerSema)
!
!$Acces
!  PRIVE
!
!$Usage
!  retour = comparer_coord(coord_1, coord_2)
!.    type(MSP_TYPBUL) :: coord_1, coord_2
!
!$Arguments
!>E     coord_1  :<MSP_TYPBUL>   
!>E     coord_2  :<MSP_TYPBUL>   
!>S     retour   :<logical>      vaut .true. si les champs sont identiques
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
  
  type(MSP_TYPBUL), intent(IN) :: coord_1, coord_2

  retour = .true.

  if (coord_1%iorb /= coord_2%iorb)        retour = .false.
  if (coord_1%mu /= coord_2%mu)   retour = .false.
  if (coord_1%requa /= coord_2%requa)   retour = .false.
  if (coord_1%apla /= coord_2%apla)   retour = .false.
  !if (coord_1%param /= coord_2%param)   retour = .false.

end function comparer_coord


logical function comparer_bulletin(bulletin_1, bulletin_2) result(retour)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  comparer_bulletin
!
!$Resume
!           Compare les champs de deux bulletins
!
!$Description
!     Compare les champs de deux bulletins. Attention, les sous-structures ne sont pas
!     comparées afin de pouvoir déterminer si la différence entre deux bulletin vient
!     d'un changement de repère ou d'un changement de paramètres
!
!$Auteur
!          Didier Semeux (SchlumbergerSema)
!
!$Acces
!  PRIVE
!
!$Usage
!  retour = comparer_bulletin(bulletin_1, bulletin_2)
!.    type(MSP_BULLETIN) :: bulletin_1, bulletin_2
!
!$Arguments
!>E     bulletin_1  :<MSP_BULLETIN>   
!>E     bulletin_2  :<MSP_BULLETIN>   
!>S     retour      :<logical>        vaut .true. si les champs scalaires du bulletins sont identiques
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
  
  type(MSP_BULLETIN), intent(IN) :: bulletin_1, bulletin_2
  ! Variables locales
  logical :: var_tmp
  real (kind=PM_REEL) :: eps

  retour  = .true.
  var_tmp = .true.

  eps = abs(bulletin_1%datbul%sec - bulletin_2%datbul%sec) 

  if (bulletin_1%datbul%jour /= bulletin_2%datbul%jour) var_tmp= .false.
  if (eps > MSP_epsilon_date) var_tmp= .false.
  if (bulletin_1%ech_temps_bul /= bulletin_2%ech_temps_bul)  var_tmp = .false.
  if (bulletin_1%modprec /= bulletin_2%modprec)      var_tmp = .false.
  retour = var_tmp

end function comparer_bulletin



 subroutine MSP_date_etutc_frac(date_in,sens,date_out,origdat)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_date_etutc_frac
!
!$Resume
!  Cette routine réalise les conversions entre date TUC et TE en utilisant la mspro
!
!$Description
!  Cette routine réalise les conversions entre date TUC et TE
!  Les dates sont des dates réelles fractionnaires (jj50 CNES)
!
!$Auteur
!  20/04/2004 - Florence VIVARES (ATOS ORIGIN)
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_date_etutc_frac(date_in,sens,date_out,[origdat])
!.    real(kind=pm_reel) :: date_in 
!.    integer :: sens
!.    real(kind=pm_reel) :: date_out 
!.    integer :: origdat
!
!$Arguments
!>E     date_in   :<pm_reel>   Date à convertir, en jour julien
!>E     sens      :<integer>   Sens de conversion :
!                                  1  : passage de date TE à la date TUC
!                                  -1 : passage de date TUC à la date TE
!>S     date_out  :<pm_reel>   Date après conversion
!>[E]   origdat   :<integer>   origine des dates (0=J50, 1=MJD2000)
!
!$Common
!
!$Routines
!- md_jourfrac_joursec
!- MSP_signaler_message
!- md_joursec_jourfrac
!#V
!- MSP_date_etutc_jjsec
!#
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  BULLETIN CONVERTIR
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




   real(kind=pm_reel), intent(IN)           :: date_in 
   integer, intent(IN)                      ::  sens
   real(kind=pm_reel), intent(OUT)          :: date_out 
   integer, intent(IN), optional            :: origdat

   type(tm_jour_sec)    :: date_pm, date_pm_out
   type(tm_code_retour) :: code_retour

   call md_jourfrac_joursec(date_in,date_pm,code_retour)
   call MSP_signaler_message (ier_mslib=code_retour)
   if (MSP_gen_messages("MSP_date_etutc - conv. date en entrée - ")) return


   call MSP_date_etutc_jjsec(date_pm,sens,date_pm_out,origdat=origdat)
   if (MSP_ERREUR) return
   
   ! -- Conversion de la date en structure  à la date en réel
   call md_joursec_jourfrac(date_pm_out,date_out,code_retour)
   call MSP_signaler_message (ier_mslib=code_retour)
   if (MSP_gen_messages("MSP_date_etutc - conv. date en sortie - ")) return


 end  subroutine MSP_date_etutc_frac



 subroutine MSP_date_etutc_jjsec(date_in,sens,date_out,origdat)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_date_etutc_jjsec
!
!$Resume
!  Cette routine réalise les conversions entre date TUC et TE en utilisant la mspro
!
!$Description
!  Cette routine réalise les conversions entre date TUC et TE en utilisant la mspro
!
!$Auteur
!  22/01/2003 - Philippe Bremard / Didier Semeux (SchlumbergerSema)
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_date_etutc_jjsec(date_in,sens,date_out,[origdat])
!.    type(tm_jour_sec) :: date_in
!.    type(tm_jour_sec) :: date_out
!.    integer :: sens
!.    integer :: origdat
!
!$Arguments
!>E     date_in   :<tm_jour_sec>   Date à convertir, en jour jour/secondes
!>E     sens      :<integer>       Sens de conversion :
!                                  1  : passage de date TE à la date TUC
!                                  -1 : passage de date TUC à la date TE
!>S     date_out  :<tm_jour_sec>   Date convertie, en jour jour/secondes
!>[E]   origdat   :<integer>       origine des dates (0=J50, 1=MJD2000)
!
!$Common
!
!$Routines
!- cps_get_sautTAITUC
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  BULLETIN CONVERTIR
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




   type(tm_jour_sec), intent(IN)  :: date_in
   type(tm_jour_sec), intent(OUT) :: date_out
   integer, intent(IN)                      ::  sens
   integer, intent(IN), optional            :: origdat

   real(pm_reel), parameter :: delta_sec_TE_TAI = 32.184_pm_reel ! ecart en secondes entre les echelles TE et TAI
   integer :: delta_TAITUC, k
   integer              :: origdat_tmp
   type(tm_jour_sec)    :: date_in50, date_inter
   real(KIND=PM_REEL)   :: reste

   ! -- si origine des temps a MJD2000, convertir en J1950
   date_in50=date_in
   if (present(origdat)) then
      origdat_tmp = origdat
   else
      origdat_tmp = 0
   endif
   
   if (origdat_tmp == 1) then
      date_in50%jour = date_in%jour + MSP_2000jour
      date_in50%sec  = date_in%sec  + MSP_2000sec
   endif
   
   if (sens == -1) then
      ! -- Calcul du saut du TUC
      call cps_get_sautTAITUC(date_in50,delta_TAITUC, echel_tuc=.true.)
      if (MSP_gen_messages("GS_date_etutc_jjsec")) return

      ! -- Conversion de la date TUC vers TE
      ! passage TUC-> TAI
      ! Calcul de la date TAI
      date_inter%jour = date_in50%jour
      date_inter%sec  = date_in50%sec + real(delta_TAITUC, kind=pm_reel)
      ! calcul du nombre de jours lies aux secondes
      reste = date_inter%sec / 86400._pm_reel
      k     = floor(reste)
      ! calcul de la quantite normalisee, k vaut 0 ou 1
      date_out%jour = date_inter%jour + k
      date_out%sec  = date_inter%sec - real(k, kind=pm_reel) * 86400._pm_reel

      ! Passage TAI -> TE
      date_inter%jour = date_out%jour
      date_inter%sec  = date_out%sec + delta_sec_TE_TAI
      ! calcul du nombre de jours lies aux secondes
      reste = date_inter%sec / 86400._pm_reel
      k     = floor(reste)
      ! calcul de la quantite normalisee, k vaut 0 ou 1
      date_out%jour = date_inter%jour + k
      date_out%sec  = date_inter%sec - real(k, kind=pm_reel) * 86400._pm_reel
      
   else if (sens == 1) then
      ! -- Conversion de la date  TE vers TUC
      ! passage TE-> TAI
      ! Calcul de la date TAI
      date_inter%jour = date_in50%jour
      date_inter%sec  = date_in50%sec - delta_sec_TE_TAI
      ! calcul du nombre de jours lies aux secondes
      reste = date_inter%sec / 86400._pm_reel
      k     = floor(reste)
      ! calcul de la quantite normalisee, k vaut 0 ou 1
      date_out%jour = date_inter%jour + k
      date_out%sec  = date_inter%sec - real(k, kind=pm_reel) * 86400._pm_reel

      ! -- Calcul du saut du TUC
      call cps_get_sautTAITUC(date_out,delta_TAITUC, echel_tuc=.false.)
      if (MSP_gen_messages("GS_date_etutc_jjsec")) return

      ! Passage TAI -> TUC
      date_inter%jour = date_out%jour
      date_inter%sec  = date_out%sec - real(delta_TAITUC,kind=pm_reel)
      ! calcul du nombre de jours lies aux secondes
      reste = date_inter%sec / 86400._pm_reel
      k     = floor(reste)
      ! calcul de la quantite normalisee, k vaut 0 ou 1
      date_out%jour = date_inter%jour + k
      date_out%sec  = date_inter%sec - real(k, kind=pm_reel) * 86400._pm_reel

   end if
  
 
   ! -- si origine des temps a MJD2000, y revenir
   if (origdat_tmp == 1) then
      date_out%jour = date_out%jour - MSP_2000jour
      date_out%sec  = date_out%sec  - MSP_2000sec
   endif
   
 end  subroutine MSP_date_etutc_jjsec



  subroutine  MSP_conv_typrep_jjsec(param_in, typrep_e, cle_date_e, &
              planete_e, dateref_e, ech_temps_e, &
              lonref_e,  strae, requa_e, apla_e, &
              typrep_s , cle_date_s, &
              planete_s, dateref_s, ech_temps_s, &
              lonref_s,  stras,  requa_s, apla_s, &
              vrot, param_out, mat_jacob, obli, &
              origdat, pole_in, pole_out, &
              modprec_in, modprec_out, obli_out, vrot_out)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_conv_typrep_jjsec
!
!$Resume
!  Cette routine permet de réaliser le changement de repères 
!   d'un bulletin en utilisant la mspro
!
!$Description
!  Cette routine permet de réaliser le changement de repères 
!   d'un bulletin en utilisant la mspro
!
!$Auteur
!  24/01/2003 - Didier Semeux (SchlumbergerSema)
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_conv_typrep_jjsec(param_in, typrep_e, cle_date_e, &
!.                  planete_e, dateref_e, ech_temps_e, &
!.                  lonref_e,  strae, requa_e, apla_e, &
!.                  typrep_s , cle_date_s, &
!.                  planete_s, dateref_s, ech_temps_s, &
!.                  lonref_s,  stras,  requa_s, apla_s, &
!.                  vrot, param_out, [mat_jacob], [obli], &
!.                  [origdat], [pole_in], [pole_out], &
!.                  [modprec_in], [modprec_out], [obli_out], [vrot_out])
!.    integer :: typrep_e, ech_temps_e
!.    integer :: planete_e, cle_date_e
!.    real(KIND=PM_REEL) :: lonref_e, requa_e, apla_e, vrot
!.    type(tm_jour_sec) :: dateref_e, dateref_s
!.    integer :: typrep_s,ech_temps_s
!.    integer :: planete_s, cle_date_s
!.    real(KIND=PM_REEL) :: lonref_s, requa_s, apla_s
!.    real(KIND=PM_REEL), dimension(6) :: param_in
!.    real(KIND=PM_REEL), dimension(9) :: strae, stras
!.    real(KIND=PM_REEL) :: obli,obli_out
!.    type(tm_pole_uv) :: pole_in, pole_out
!.    real(KIND=PM_REEL) :: vrot_out
!.    real(KIND=PM_REEL), dimension(6) :: param_out
!.    real(KIND=PM_REEL), dimension(6, 6) :: mat_jacob
!.    integer :: origdat
!.    integer :: modprec_in, modprec_out
!
!$Arguments
!>E     param_in     :<PM_REEL,DIM=(6)>      paramètres à convertir, en cartésien
!>E     typrep_e     :<integer>              type de repère dans lequel est exprimé le bulletin en entrée (pm_equa_vrai, pm_equa_moyen, 
!>                                         pm_ecli_moy, pm_veis, pm_planeto_ref, pm_planeto_ref_iner, pm_planeto_vrai,
!>                                         pm_equa_uai, pm_topo)
!>E     cle_date_e   :<integer>              type de date utilisé pour l'origine des dates du repère en entrée:
!>                                              pm_1janvier1950_00h00           date du repère = 1/1/1950 à 0h00         
!>                                              pm_1janvier2000_12h00           date du repère = 1/1/2000 à 12h00 (18262.5 JJCNES)         
!>                                              MSP_ENUM_ECHD_DATE_REF       date du repère fixée par une date de référence 
!>                                              pm_autre_date       date du repère fixée par la date du bulletin 
!>E     planete_e    :<integer>              planète définissant le repère d'entrée (eph_mercure,eph_venus,
!>                                               eph_terre,eph_mars,eph_jupiter,eph_saturne,eph_uranus,
!>                                               eph_neptune,eph_pluton)     
!>E     dateref_e    :<tm_jour_sec>          date de référence du repère en entrée [JJ]   
!>E     ech_temps_e  :<integer>              echelle de temps de la date de référence du repère en entrée   
!>E     lonref_e     :<PM_REEL>              longitude de référence du repère en entrée 
!>E     strae        :<PM_REEL,DIM=(9)>      tableau contenant les coordonées de la station en entrée
!>E     requa_e      :<PM_REEL>              rayon terrestre à l'équateur utilisé pour le calcul des coordonnées de la station  
!>                                                 en entrée [m]
!>E     apla_e       :<PM_REEL>              aplatissement terrestre  utilisé pour le calcul des coordonnées de la station 
!>                                                 en entrée [m]
!>E     typrep_s     :<integer>              type de repère dans lequel on souhaite convertir le bulletin (pm_equa_vrai, pm_equa_moyen, 
!>                                         pm_ecli_moy, pm_veis, pm_planeto_ref, pm_planeto_ref_iner, pm_planeto_vrai,
!>                                         pm_equa_uai, pm_topo)  
!>E     cle_date_s   :<integer>              type de date utilisé pour l'origine des dates du repère en sortie :
!>                                              pm_1janvier1950_00h00  date du repère = 1/1/1950 à 0h00         
!>                                              pm_1janvier2000_12h00  date du repère = 1/1/2000 à 12h00 (18262.5 JJCNES)         
!>                                              MSP_ENUM_ECHD_DATE_REF date du repère fixée par une date de référence 
!>                                              pm_autre_date          date du repère fixée par la date du bulletin 
!>E     planete_s    :<integer>              planète définissant le repère de sortie (eph_mercure,eph_venus,
!>                                               eph_terre,eph_mars,eph_jupiter,eph_saturne,eph_uranus,
!>                                               eph_neptune,eph_pluton)         
!>E     dateref_s    :<tm_jour_sec>          date de référence du repère dans lequel on souhaite convertir le bulletin [JJ] 
!>E     ech_temps_s  :<integer>              echelle de temps de la date de référence du repère dans lequel on souhaite convertir le bulletin   
!>E     lonref_s     :<PM_REEL>              longitude de référence du repère dans lequel on souhaite convertir le bulletin 
!>E     stras        :<PM_REEL,DIM=(9)>      tableau contenant les coordonées de la station en sortie
!>E     requa_s      :<PM_REEL>              rayon terrestre à l'équateur utilisé pour le calcul des coordonnées de la station  
!>                                               dans laquelle on souhaite convertir le bulletin [m]    
!>E     apla_s       :<PM_REEL>              aplatissement terrestre  utilisé pour le calcul des coordonnées de la station 
!>                                               dans laquelle on souhaite convertir le bulletin [m]          
!>E     vrot         :<PM_REEL>              vitesse de rotation de la planete de reference du repere d'entrée [rad/s]
!>S     param_out    :<PM_REEL,DIM=(6)>      paramètres cartésiennes du bulletin de sortie
!>[S]   mat_jacob    :<PM_REEL,DIM=(6, 6)>   matrice de passage
!>[E]   obli         :<PM_REEL>              valeur de l'obliquite du corps central [rad/s] - Par défaut pm_obliquite
!>[E]   origdat      :<integer>              origine des dates (0=J50, 1=MJD2000)
!>[E]   pole_in      :<tm_pole_uv>           Structure contenant les angles de pole u/v de la planète de reference du repere d'entree [rad]  
!>[E]   pole_out     :<tm_pole_uv>           Structure contenant les angles de pole u/v de la planète de reference du repere de sortie [rad]  
!>[E]   modprec_in   :<integer>              Modele de precession pour le repere d'entree (pm_lieske_wahr_aoki,pm_uai1994,pm_uai2000)
!>[E]   modprec_out  :<integer>              Modele de precession pour le repere de sortie (pm_lieske_wahr_aoki,pm_uai1994,pm_uai2000)
!>[E]   obli_out     :<PM_REEL>              Obliquite pour le repere de sortie [rad] - Par défaut pm_obliquite
!>[E]   vrot_out     :<PM_REEL>              Vitesse de rotation de la planete de reference du repere de sortie [rad/s]  
!
!$Common
!
!$Routines
!- MSP_init_structure_topo
!- MSP_calcul_ecart_echt
!- mx_rep
!- MSP_signaler_message
!
!$Include
!
!$Module
!
!$Remarques
!  - Cette fonction s'emploie en spécifiant les caractéristiques du bulletin en
!    entrée que l'on souhaite changer, et en indiquant leur nouvelle valeur.
!  - Afin de ne pas surcharger la liste des messages, le warning 4811 :
!    (Trop de paramètres optionnels) est supprimé après les appels à mx_rep
!
!$Mots-cles
!  BULLETIN CONVERTIR
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none


    integer, intent(IN):: typrep_e,  ech_temps_e
    integer, intent(IN) :: planete_e, cle_date_e
    real(KIND=PM_REEL), intent(IN):: lonref_e, requa_e, apla_e, vrot
    type(tm_jour_sec) , intent(IN) :: dateref_e, dateref_s

    integer, intent(IN):: typrep_s,ech_temps_s
    integer, intent(IN) :: planete_s, cle_date_s
    real(KIND=PM_REEL), intent(IN):: lonref_s, requa_s, apla_s
    real(KIND=PM_REEL), dimension(6), intent(IN) :: param_in
    real(KIND=PM_REEL), dimension(9),  intent(IN)  :: strae, stras
    real(KIND=PM_REEL), intent(IN), optional :: obli,obli_out
    type(tm_pole_uv)  , intent(IN), optional :: pole_in, pole_out
    real(KIND=PM_REEL), intent(IN), optional :: vrot_out

    real(KIND=PM_REEL), dimension(6), intent(OUT) :: param_out
    real(KIND=PM_REEL), dimension(6, 6), optional, intent(OUT) :: mat_jacob
    integer, intent(IN), optional :: origdat
    integer, intent(IN), optional  ::  modprec_in, modprec_out

    ! Variables locales
    type(tm_jour_sec) :: dateref_in, dateref_out
    type(tm_code_retour) :: code_retour

    ! -- Variables pour la conversion en paramètres MSPRO
    integer  ::  bv_planete_in, prec_nuta_tsid_in
    integer  ::  bv_planete_out, prec_nuta_tsid_out
    real(kind=PM_REEL) :: delta_ech_tu1_in, delta_ech_tai_in,delta_ech_tu1_out,delta_ech_tai_out
    type(tm_def_topo) :: topo_in, topo_out
    real(KIND=PM_REEL) :: obli_tmp, dir_pm,obli_out_tmp
    integer  ::  cle_date_e_tmp, cle_date_s_tmp,origdat_tmp


    ! -- Obliquite, valeur par defaut pm_obliquite
    if (PRESENT(obli)) then
       obli_tmp = obli
    else
       obli_tmp = MSP_OBLIQUITE   !(en rad/s)
    end if
    if (PRESENT(obli_out)) then
       obli_out_tmp = obli_out
    else
       obli_out_tmp = MSP_OBLIQUITE   !(en rad/s)
    end if

    call MSP_init_structure_topo(topo_in)
    call MSP_init_structure_topo(topo_out)


    ! -- Calcul des paramètres MSPRO du repère origine :

    ! - Si la date du repère est la date du bulletin
    if (cle_date_e == pm_autre_date) then
       cle_date_e_tmp = MSP_ENUM_ECHD_DATE_REF
    else
       cle_date_e_tmp = cle_date_e
    endif
    

    ! Dates en J50
    dateref_in=dateref_e
    dateref_out=dateref_s

    origdat_tmp=0
    if(present(origdat)) origdat_tmp = origdat

    if (origdat_tmp == 1) then
       dateref_in%jour =dateref_e%jour + MSP_2000jour
       dateref_in%sec  =dateref_e%sec  + MSP_2000sec
       dateref_out%jour=dateref_s%jour + MSP_2000jour
       dateref_out%sec =dateref_s%sec  + MSP_2000sec
    endif

    if ( cle_date_e == pm_1janvier2000_12h00) then
       dateref_in%jour = MSP_2000jour
       dateref_in%sec  = MSP_2000sec
    elseif ( cle_date_e == pm_1janvier1950_00h00) then
       dateref_in%jour = 0
       dateref_in%sec  = 0._pm_reel
    endif
    
    !    ecarts d'echelle de date en entrée
    call MSP_calcul_ecart_echt(dateref_in, ech_temps_e, &
         ecart_tu1=delta_ech_tu1_in, ecart_tai=delta_ech_tai_in)

    if (MSP_gen_messages("MSP_conv_typrep - ecart TUC/TE en entree ")) return

    ! Mode de précession en entrée
    if(present(modprec_in)) then
       prec_nuta_tsid_in = modprec_in
    elseif (planete_e == eph_terre) then
       prec_nuta_tsid_in = pm_lieske_wahr_aoki
    else
       prec_nuta_tsid_in = pm_uai1994
    end if

    if(present(modprec_out)) then
       prec_nuta_tsid_out = modprec_out
    elseif (planete_s == eph_terre) then
       prec_nuta_tsid_out = pm_lieske_wahr_aoki
    else
       prec_nuta_tsid_out = pm_uai1994
    end if


    ! Planete en entrée
    bv_planete_in = planete_mspro(planete_e)
    if (MSP_gen_messages("MSP_conv_typrep - planete en entree -")) return
    
    ! Structure topo en entrée
    select case(typrep_e)
    case(pm_topo)
       dir_pm = direction_mspro(int(strae(6)))
       if (MSP_gen_messages("MSP_conv_typrep - direction en entree - ")) return
       topo_in%geod%lat  = strae(3)
       topo_in%geod%long = strae(4)
       topo_in%geod%haut = strae(5)
       topo_in%axe_x     = dir_pm
    end select
    
    topo_in%ellips%r_equa = requa_e
    topo_in%ellips%apla = apla_e


    ! - Si la date du repère est la date du bulletin
    if (cle_date_s == pm_autre_date) then
       cle_date_s_tmp = MSP_ENUM_ECHD_DATE_REF
    else
       cle_date_s_tmp = cle_date_s
    endif
    ! -- Calcul des paramètres MSPRO du repère d'arrivée :
    !    ecarts d'echelle de date en sortie
    call MSP_calcul_ecart_echt(dateref_out, ech_temps_s, &
         ecart_tu1=delta_ech_tu1_out, ecart_tai=delta_ech_tai_out)
    if (MSP_gen_messages("MSP_conv_typrep - ecart TUC/TE en sortie - ")) return
    
    
    ! Planete en sortie
    bv_planete_out = planete_mspro(planete_s)
    if (MSP_gen_messages("MSP_conv_typrep - planete en sortie -")) return
    
    ! Structure topo en sortie
    select case(typrep_s)
    case(pm_topo)
       dir_pm = direction_mspro(int(stras(6)))
       if (MSP_gen_messages("MSP_conv_typrep - direction en sortie -")) return
       topo_out%geod%lat  = stras(3)
       topo_out%geod%long = stras(4)
       topo_out%geod%haut = stras(5)
       topo_out%axe_x     = dir_pm
    end select
    

    topo_out%ellips%r_equa = requa_s
    topo_out%ellips%apla = apla_s


    ! -- Changement de repere par la mspro 
    ! comme on a pas les valeurs par defaut des parametres de poles
    ! appel differents si definis par l'utilisateur ou non
 
    call mx_rep(typrep_e ,bv_planete_in,cle_date_e_tmp,prec_nuta_tsid_in, &
         param_in(1:3),param_in(4:6), &
         typrep_s,bv_planete_out,cle_date_s_tmp,prec_nuta_tsid_out, &
         param_out(1:3),param_out(4:6), code_retour, &
         vit_rot_in=vrot, vit_rot_out=vrot_out, &
         obliquite_in = obli_tmp, obliquite_out = obli_out_tmp , &
         pole_in=pole_in, pole_out=pole_out, &
         long_ref_in=lonref_e, long_ref_out=lonref_s, &
         val_date_in = dateref_in , val_date_out= dateref_out,  &
         delta_tu1_in = delta_ech_tu1_in,  delta_tai_in = delta_ech_tai_in, &
         delta_tu1_out= delta_ech_tu1_out, delta_tai_out= delta_ech_tai_out, &
         eps_date=MSP_EPSILON_DATE, def_topo_in=topo_in, def_topo_out=topo_out, &
         jacob=mat_jacob)

    if (code_retour%valeur /= pm_warn_para_opt_trop.and. &
         code_retour%valeur /= pm_OK) then
       call MSP_signaler_message (ier_mslib=code_retour)
    endif

    if (MSP_gen_messages("MSP_conv_typrep - Appel mx_rep ")) return

  end subroutine MSP_conv_typrep_jjsec

  subroutine  MSP_conv_typrep_frac(param_in, typrep_e, cle_date_e, &
              planete_e, dateref_e, ech_temps_e, &
              lonref_e,  strae, requa_e, apla_e, &
              typrep_s , cle_date_s, &
              planete_s, dateref_s, ech_temps_s, &
              lonref_s,  stras,  requa_s, apla_s, &
              vrot, param_out, mat_jacob, obli, &
              origdat, pole_in, pole_out, &
              modprec_in, modprec_out, obli_out, vrot_out)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_conv_typrep_frac
!
!$Resume
!  Cette routine permet de réaliser le changement de repères 
!   d'un bulletin en utilisant la mspro
!
!$Description
!  Cette routine permet de réaliser le changement de repères 
!   d'un bulletin en utilisant la mspro
!  Les dates sont des dates réelles fractionnaires (jj50 CNES)
!
!$Auteur
!  20/04/2004 - Florence VIVARES (ATOS ORIGIN)
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_conv_typrep_frac(param_in, typrep_e, cle_date_e, &
!.                  planete_e, dateref_e, ech_temps_e, &
!.                  lonref_e,  strae, requa_e, apla_e, &
!.                  typrep_s , cle_date_s, &
!.                  planete_s, dateref_s, ech_temps_s, &
!.                  lonref_s,  stras,  requa_s, apla_s, &
!.                  vrot, param_out, [mat_jacob], [obli], &
!.                  [origdat], [pole_in], [pole_out], &
!.                  [modprec_in], [modprec_out], [obli_out], [vrot_out])
!.    integer :: typrep_e, ech_temps_e
!.    integer :: planete_e, cle_date_e
!.    real(KIND=PM_REEL) :: dateref_e ,lonref_e, requa_e, apla_e, vrot
!.    integer :: typrep_s,ech_temps_s
!.    integer :: planete_s, cle_date_s
!.    real(KIND=PM_REEL) :: dateref_s ,lonref_s, requa_s, apla_s
!.    real(KIND=PM_REEL), dimension(6) :: param_in
!.    real(KIND=PM_REEL), dimension(9) :: strae, stras
!.    real(KIND=PM_REEL) :: obli, obli_out, vrot_out
!.    type(tm_pole_uv) :: pole_in, pole_out
!.    real(KIND=PM_REEL), dimension(6) :: param_out
!.    real(KIND=PM_REEL), dimension(6, 6) :: mat_jacob
!.    integer :: origdat
!.    integer :: modprec_in,modprec_out
!
!$Arguments
!>E     param_in     :<PM_REEL,DIM=(6)>      paramètres à convertir, en cartésien
!>E     typrep_e     :<integer>              type de repère dans lequel est exprimé le bulletin en entrée (pm_equa_vrai, pm_equa_moyen, 
!>                                         pm_ecli_moy, pm_veis, pm_planeto_ref, pm_planeto_ref_iner, pm_planeto_vrai,
!>                                         pm_equa_uai, pm_topo)
!>E     cle_date_e   :<integer>              type de date utilisé pour l'origine des dates du repère en entrée:
!>                                              pm_1janvier1950_00h00           date du repère = 1/1/1950 à 0h00         
!>                                              pm_1janvier2000_12h00           date du repère = 1/1/2000 à 12h00 (18262.5 JJCNES) 
!>                                              MSP_ENUM_ECHD_DATE_REF       date du repère fixée par une date de référence
!>                                              pm_autre_date       date du repère fixée par la date du bulletin   
!>E     planete_e    :<integer>              planète définissant le repère d'entrée (eph_mercure,eph_venus,
!>                                               eph_terre,eph_mars,eph_jupiter,eph_saturne,eph_uranus,
!>                                               eph_neptune,eph_pluton)
!>E     dateref_e    :<PM_REEL>              date de référence du repère en entrée [JJ]   
!>E     ech_temps_e  :<integer>              echelle de temps de la date de référence du repère en entrée   
!>E     lonref_e     :<PM_REEL>              longitude de référence du repère en entrée 
!>E     strae        :<PM_REEL,DIM=(9)>      tableau contenant les coordonées de la station en entrée
!>E     requa_e      :<PM_REEL>              rayon terrestre à l'équateur utilisé pour le calcul des coordonnées de la station 
!>                                                 en entrée [m]
!>E     apla_e       :<PM_REEL>              aplatissement terrestre  utilisé pour le calcul des coordonnées de la station 
!>                                                 en entrée [m]
!>E     typrep_s     :<integer>              type de repère dans lequel on souhaite convertir le bulletin (pm_equa_vrai, pm_equa_moyen, 
!>                                         pm_ecli_moy, pm_veis, pm_planeto_ref, pm_planeto_ref_iner, pm_planeto_vrai,
!>                                         pm_equa_uai, pm_topo)  
!>E     cle_date_s   :<integer>              type de date utilisé pour l'origine des dates du repère en sortie :
!>                                              pm_1janvier1950_00h00  date du repère = 1/1/1950 à 0h00         
!>                                              pm_1janvier2000_12h00  date du repère = 1/1/2000 à 12h00 (18262.5 JJCNES) 
!>                                              MSP_ENUM_ECHD_DATE_REF date du repère fixée par une date de référence 
!>                                              pm_autre_date          date du repère fixée par la date du bulletin 
!>E     planete_s    :<integer>              planète définissant le repère de sortie (eph_mercure,eph_venus,
!>                                               eph_terre,eph_mars,eph_jupiter,eph_saturne,eph_uranus,
!>                                               eph_neptune,eph_pluton)         
!>E     dateref_s    :<PM_REEL>              date de référence du repère dans lequel on souhaite convertir le bulletin [JJ]
!>E     ech_temps_s  :<integer>              echelle de temps de la date de référence du repère dans lequel on souhaite convertir le bulletin   
!>E     lonref_s     :<PM_REEL>              longitude de référence du repère dans lequel on souhaite convertir le bulletin 
!>E     stras        :<PM_REEL,DIM=(9)>      tableau contenant les coordonées de la sation en sortie
!>E     requa_s      :<PM_REEL>              rayon terrestre à l'équateur utilisé pour le calcul des coordonnées de la station  
!>                                               dans laquelle on souhaite convertir le bulletin [m]    
!>E     apla_s       :<PM_REEL>              aplatissement terrestre  utilisé pour le calcul des coordonnées de la station 
!>                                               dans laquelle on souhaite convertir le bulletin [m]          
!>E     vrot         :<PM_REEL>              vitesse de rotation de la planete de reference du repere d'entrée [rad/s]
!>S     param_out    :<PM_REEL,DIM=(6)>      paramètres cartésiennes du bulletin de sortie
!>[S]   mat_jacob    :<PM_REEL,DIM=(6, 6)>   matrice de passage
!>[E]   obli         :<PM_REEL>              valeur de l'obliquite du corps central [rad/s] - Par défaut pm_obliquite
!>[E]   origdat      :<integer>              origine des dates (0=J50, 1=MJD2000)
!>[E]   pole_in      :<tm_pole_uv>           Structure contenant les angles de pole u/v de la planète de reference du repere d'entree [rad]  
!>[E]   pole_out     :<tm_pole_uv>           Structure contenant les angles de pole u/v de la planète de reference du repere de sortie [rad]  
!>[E]   modprec_in   :<integer>              Modele de precession pour le repere d'entree (pm_lieske_wahr_aoki,pm_uai1994,pm_uai2000)
!>[E]   modprec_out  :<integer>              Modele de precession pour le repere de sortie (pm_lieske_wahr_aoki,pm_uai1994,pm_uai2000)
!>[E]   obli_out     :<PM_REEL>              Obliquite pour le repere de sortie [rad]
!>[E]   vrot_out     :<PM_REEL>              Vitesse de rotation de la planete de reference du repere de sortie [rad/s]  
!
!$Common
!
!$Routines
!- md_jourfrac_joursec
!#V
!- MSP_conv_typrep_jjsec
!#
!
!$Include
!
!$Module
!
!$Remarques
!  - Cette fonction s'emploie en spécifiant les caractéristiques du bulletin en entrée que l'on souhaite changer, et
!  en indiquant leur nouvelle valeur.
!  - Afin de ne pas surcharger la liste des messages, le warning 4811 - Trop de paramètres optionnels - est
!      supprimé après les appels à mx_rep
!
!$Mots-cles
!  BULLETIN CONVERTIR
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none


    integer, intent(IN):: typrep_e,  ech_temps_e
    integer, intent(IN) :: planete_e, cle_date_e
    real(KIND=PM_REEL), intent(IN):: dateref_e ,lonref_e, requa_e, apla_e, vrot

    integer, intent(IN):: typrep_s,ech_temps_s
    integer, intent(IN) :: planete_s, cle_date_s
    real(KIND=PM_REEL), intent(IN):: dateref_s ,lonref_s, requa_s, apla_s
    real(KIND=PM_REEL), dimension(6), intent(IN) :: param_in
    real(KIND=PM_REEL), dimension(9),  intent(IN)  :: strae, stras
    real(KIND=PM_REEL), intent(IN), optional :: obli, obli_out, vrot_out
    type(tm_pole_uv)  , intent(IN), optional :: pole_in, pole_out

    real(KIND=PM_REEL), dimension(6), intent(OUT) :: param_out
    real(KIND=PM_REEL), dimension(6, 6), optional, intent(OUT) :: mat_jacob
    integer, intent(IN), optional :: origdat
    integer, intent(IN), optional :: modprec_in,modprec_out


    ! dates en jour secondes
    type(tm_code_retour) :: code_retour
    type(tm_jour_sec)  :: dateref_pm_e, dateref_pm_s 

    
    call md_jourfrac_joursec(dateref_e, dateref_pm_e, code_retour)

    call md_jourfrac_joursec(dateref_s, dateref_pm_s, code_retour)

    ! fonction principale
    call MSP_conv_typrep_jjsec(param_in, typrep_e, cle_date_e, &
         planete_e, dateref_pm_e, ech_temps_e, &
         lonref_e,  strae, requa_e, apla_e, &
         typrep_s , cle_date_s, &
         planete_s, dateref_pm_s, ech_temps_s, &
         lonref_s,  stras,  requa_s, apla_s, &
         vrot,  param_out, mat_jacob=mat_jacob, &
         obli=obli, pole_in=pole_in, pole_out=pole_out, &
         origdat=origdat, obli_out=obli_out, vrot_out=vrot_out, &
         modprec_in=modprec_in, modprec_out=modprec_out)

    if (MSP_gen_messages("MSP_conv_typrep_frac ")) return

  end subroutine MSP_conv_typrep_frac


 subroutine MSP_calcul_ecart_echt(date,ech_temps,ecart_tu1, ecart_tai, &
      date_pm)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_calcul_ecart_echt
!
!$Resume
! Calcul des écart de date entre l'echelle ech_temps (pm_TE ou pm_TUC) 
! et les  echelles de temps TU1 et TAI à la date 'date'
!
!$Description
! Calcul des écart de date entre l'echelle ech_temps (pm_TE ou pm_TUC) 
! et les  echelles de temps TU1 et TAI à la date 'date'
!
!$Auteur
!  17/01/2003 - Philippe Bremard / Didier Semeux (SchlumbergerSema)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_calcul_ecart_echt(date,ech_temps,[ecart_tu1], [ecart_tai], &
!.          [date_pm])
!.    type(tm_jour_sec) :: date
!.    integer :: ech_temps
!.    real(kind=pm_reel) :: ecart_tu1,ecart_tai
!.    type(tm_jour_sec) :: date_pm
!
!$Arguments
!>E     date       :<tm_jour_sec>   Date à laquelle on souhaite connaitre l'ecart
!>E     ech_temps  :<integer>       Echelle de temps de référence (pm_TUC, pm_TE)
!                                   ascendante mais c est la base COMPAS qui est utilisee : 
!                                   ce parametre est obsolete
!>[S]   ecart_tu1  :<pm_reel>       Ecart entre l'echelle de temps 'ech-temps' et l'echelle TU1
!>[S]   ecart_tai  :<pm_reel>       Ecart entre l'echelle de temps 'ech-temps' et l'echelle TAI
!>[S]   date_pm    :<tm_jour_sec>   Conversion de la date d'entrée en date tm_jour_sec
!
!$Common
!
!$Routines
!- cps_get_sautTAITUC
!- md_ech_temps
!- MSP_signaler_message
!
!$Include
!
!$Module
!#V
!- MSPRO
!#
!
!$Remarques
! Si la date demandée se trouve avant le premier saut du TUC, ecart_tu1 vaut 0.
!
!$Mots-cles
!  ECART TAI TUC TU1
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use MSPRO

   ! Parametres
   type(tm_jour_sec), intent(IN)    :: date
   integer, intent(IN)              :: ech_temps
   real(kind=pm_reel), intent(OUT), optional  :: ecart_tu1,ecart_tai
   type(tm_jour_sec), intent(OUT),optional    :: date_pm

   ! Variables locales
   type(tm_jour_sec)   :: date_pm_loc, date_tmp
   real(KIND=PM_REEL)  ::  delta_te_tai
   integer :: delta_tai_tuc

   type(tm_code_retour) :: code_retour

   ! Initialisations

   delta_te_tai  = 0._PM_REEL
   delta_tai_tuc = 0

   date_pm_loc = date

   ! -- Calcul de l'ecart TE-TAI : delta_te_tai (toujours calculable)
   call md_ech_temps(pm_TAI,date_pm_loc,pm_TE,delta_te_tai,date_tmp,&
        code_retour=code_retour)
   call MSP_signaler_message (ier_mslib=code_retour)
   if (MSP_gen_messages("MSP_calcul_ecart_echt -  TE-TAI")) return


!        <  ecart TAI-TUC  ><    ecart TE-TAI >
!     ---+------------------+-----------------+----------->
!       TUC                TAI   (32,184 s)   TE
!       TU1



   ! -- Calcul de l'ecart TAI-TUC : delta_tai_tuc
   if (ech_temps == pm_TE)  then
     call cps_get_sautTAITUC(date_pm_loc - delta_te_tai, delta_tai_tuc, echel_tuc = .false.)
     if (MSP_gen_messages("MSP_calcul_ecart_echt - TAI-TUC")) return

   ! -- Ecarts avec le TE
      ecart_tu1 = - delta_te_tai - real(delta_tai_tuc, kind=pm_reel)    ! écart TU1 - TE 
      ecart_tai = - delta_te_tai                       ! écart TAI - TE 
   else 
     call cps_get_sautTAITUC(date_pm_loc, delta_tai_tuc, echel_tuc = .true.)
     if (MSP_gen_messages("MSP_calcul_ecart_echt - TAI-TUC")) return

   ! -- Ecarts avec le TUC
      ecart_tu1 = 0._PM_REEL                         ! écart TU1 - TUC
      ecart_tai = real(delta_tai_tuc,kind=pm_reel)      ! écart TAI - TUC 
   endif

   if (present(date_pm)) date_pm=date_pm_loc

 end subroutine MSP_calcul_ecart_echt



  SUBROUTINE  MSP_consulter_bulletin(bulletin, repere, coord, &
       datbul, datbul_js, iorb, param,  typrep, rep, &
       ech_temps_bul, lonref, date_ref, date_refjs, lat, lon, alt, direction,   &
       planete, corcen, cle_date, ech_temps_rep, modprec, &
       mu, requa, apla, requa_r, apla_r, vitrot, pole_u, pole_v, obli,origdat)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consulter_bulletin
!
!$Resume
!  Cette routine permet de consulter les champs d'une structure bulletin
!
!$Description
!  Cette routine permet de consulter les champs d'une structure bulletin
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_consulter_bulletin(bulletin, [repere], [coord], &
!.           [datbul], [datbul_js], [iorb], [param],  [typrep], [rep], &
!.           [ech_temps_bul], [lonref], [date_ref], [date_refjs], [lat], [lon], [alt], [direction],   &
!.           [planete], [corcen], [cle_date], [ech_temps_rep], [modprec], &
!.           [mu], [requa], [apla], [requa_r], [apla_r], [vitrot], [pole_u], [pole_v], [obli],[origdat])
!.    type(MSP_BULLETIN) :: bulletin
!.    type(MSP_TYPREP) :: repere
!.    type(MSP_TYPBUL) :: coord
!.    real(KIND=PM_REEL) :: datbul
!.    integer :: iorb 
!.    real(KIND=PM_REEL), dimension(6) :: param
!.    integer :: typrep
!.    real(KIND=PM_REEL), dimension(10) :: rep
!.    real(KIND=PM_REEL) :: lonref, date_ref, lat, lon, alt, direction
!.    integer :: planete, corcen, cle_date, ech_temps_bul, modprec, ech_temps_rep
!.    real(KIND=PM_REEL) :: mu, requa, apla, requa_r, apla_r, vitrot
!.    real(KIND=PM_REEL) :: pole_u, pole_v, obli
!.    type(tm_jour_sec) :: date_refjs, datbul_js
!.    integer :: origdat
!
!$Arguments
!>E     bulletin       :<MSP_BULLETIN>       Structure bulletin à lire
!>[S]   repere         :<MSP_TYPREP>         Sous structure contenant la description du repere
!>[S]   coord          :<MSP_TYPBUL>         Sous structure contenant la description des coordonnées
!>[S]   datbul         :<PM_REEL>            Date du bulletin en Jours Julien
!>[S]   datbul_js      :<tm_jour_sec>        Date du bulletin en Jours/secondes
!>[S]   iorb           :<integer>            type de coordonnées (pm_kep, pm_cir,
!                                        pm_equa, pm_cir_equa, pm_car,
!                                        pm_hpha, pm_geoc, 
!                                        MSP_ENUM_RENTREE_SPHERIQUE, pm_geod_meca_vol,
!                                        pm_geod_gps_ard)
!>[S]   param          :<PM_REEL,DIM=(6)>    coordonnées (cf. définition de iorb)
!>[S]   typrep         :<integer>            type de repère (pm_equa_vrai, pm_equa_moyen, pm_ecli_moy,
!>                                           pm_veis, pm_planeto_ref, pm_planeto_ref_iner, pm_planeto_vrai,
!>                                           pm_equa_uai,  pm_topo)
!>[S]   rep            :<PM_REEL,DIM=(10)>   Tableau information sur le repere (utilisé par PSIMU uniquement!)
!>[S]   ech_temps_bul  :<integer>            échelle de temps de la date du bulletin (pm_TUC, pm_TE)
!>[S]   lonref         :<PM_REEL>            longitude de référence
!>[S]   date_ref       :<PM_REEL>            date de référence du repere [JJ]
!>[S]   date_refjs     :<tm_jour_sec>        date de référence du repere jours/secondes 1950
!>[S]   lat            :<PM_REEL>            latitude station  [rad]
!>[S]   lon            :<PM_REEL>            longitude station [rad]
!>[S]   alt            :<PM_REEL>            altitude station  [m]
!>[S]   direction      :<PM_REEL>            direction de la station
!>[S]   planete        :<integer>            planète (eph_mercure,eph_venus,eph_terre,eph_mars,
!>                                               eph_jupiter,eph_saturne,eph_uranus,eph_neptune,eph_pluton)
!>[S]   corcen         :<integer>            corps central (eph_mercure,eph_venus,eph_terre,eph_mars,
!>                                          eph_jupiter,eph_saturne,eph_uranus,eph_neptune,eph_pluton,
!>                                          eph_soleil)
!>[S]   cle_date       :<integer>            type de date utilisé pour la définition du repère :
!.                                              pm_1janvier1950_00h00  date du repère = 1/1/1950 à 0h00         
!.                                              pm_1janvier2000_12h00  date du repère = 1/1/2000 à 12h00 (18262.5 JJCNES)         
!.                                              MSP_ENUM_ECHD_DATE_REF date du repère fixée par une date de référence         
!.                                              pm_autre_date          date du repère fixée par la date du bulletin 
!>[S]   ech_temps_rep  :<integer>            échelle de temps de la date de référence du repère
!>[S]   modprec        :<integer>            mode de précession (pm_lieske_wahr_aoki, pm_uai1994)
!>[S]   mu             :<PM_REEL>            terme central du potentiel (m^3/s^2)
!>[S]   requa          :<PM_REEL>            rayon terrestre à l'équateur [m]
!>[S]   apla           :<PM_REEL>            aplatissement terrestre
!>[S]   requa_r        :<PM_REEL>            rayon terrestre à l'équateur pour les coordonnées station  [m]
!>[S]   apla_r         :<PM_REEL>            aplatissement terrestre pour les coordonnées station 
!>[S]   vitrot         :<PM_REEL>            vitesse de rotation [rad/s]
!>[S]   pole_u         :<PM_REEL>            angle u du pole vrai
!>[S]   pole_v         :<PM_REEL>            angle v du pole vrai
!>[S]   obli           :<PM_REEL>            obliquite du corps central [rad]
!>[S]   origdat        :<integer>            origine des dates (0=J50, 1=MJD2000)
!
!$Common
!
!$Routines
!- md_joursec_jourfrac
!- MSP_consulter_typrep
!- MSP_consulter_typbul
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  BULLETIN CONSULTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



    implicit none

    type(MSP_BULLETIN), intent(IN) :: bulletin
    type(MSP_TYPREP), intent(OUT), optional :: repere
    type(MSP_TYPBUL), intent(OUT), optional :: coord

    real(KIND=PM_REEL), intent(OUT), optional :: datbul
    integer, intent(OUT), optional            :: iorb         
    real(KIND=PM_REEL), dimension(6), intent(OUT), optional :: param
    integer, intent(OUT), optional            :: typrep
    real(KIND=PM_REEL), dimension(10), intent(OUT), optional :: rep

    real(KIND=PM_REEL), intent(OUT), optional :: lonref, date_ref, lat, lon, alt, direction
    integer, intent(OUT), optional            :: planete, corcen, cle_date, ech_temps_bul, modprec, ech_temps_rep
    real(KIND=PM_REEL), intent(OUT), optional :: mu, requa, apla, requa_r, apla_r, vitrot
    real(KIND=PM_REEL), intent(OUT), optional :: pole_u, pole_v, obli
    type(tm_jour_sec) , intent(OUT), optional :: date_refjs, datbul_js
    integer, intent(OUT), optional            :: origdat


    ! variables locales
    type(tm_code_retour)      :: code_retour

    if ( PRESENT(datbul) ) call md_joursec_jourfrac(bulletin%datbul,datbul,code_retour)
    if ( PRESENT(datbul_js) ) datbul_js = bulletin%datbul
    if ( PRESENT(repere) )    repere    = bulletin%repere
    if ( PRESENT(coord) )     coord     = bulletin%coord
    if ( PRESENT(ech_temps_bul) ) ech_temps_bul = bulletin%ech_temps_bul
    if ( PRESENT(modprec) )   modprec   = bulletin%modprec

    call MSP_consulter_typrep(bulletin%repere, typrep=typrep, rep=rep, &
       ech_temps_rep=ech_temps_rep, lonref=lonref, date_ref=date_ref, date_refjs=date_refjs, &
       lat=lat, lon=lon, alt=alt, direction=direction, planete = planete, corcen=corcen, &
       cle_date = cle_date, origdat=origdat, &
       requa_r=requa_r, apla_r=apla_r, vitrot=vitrot, pole_u=pole_u, pole_v=pole_v,obli=obli)

    call MSP_consulter_typbul(bulletin%coord, param=param, &
         iorb=iorb, mu=mu, apla=apla, requa=requa)

  end SUBROUTINE MSP_consulter_bulletin


  SUBROUTINE MSP_consulter_typbul(coord, mu, apla, requa, iorb, param)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consulter_typbul
!
!$Resume
!  Cette routine permet de consulter le contenu d'une structure coordonnées
!
!$Description
!  Cette routine permet de consulter le contenu d'une structure coordonnées
!
!$Auteur
!  Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_consulter_typbul(coord, [mu], [apla], [requa], [iorb], [param])
!.    type(MSP_TYPBUL) :: coord
!.    integer :: iorb 
!.    real(KIND=PM_REEL), dimension(6) :: param
!.    real(KIND=PM_REEL) :: mu, requa, apla
!
!$Arguments
!>E     coord  :<MSP_TYPBUL>        Coordonnées à consulter
!>[S]   mu     :<PM_REEL>           terme central du potentiel (m^3/s^2)
!>[S]   apla   :<PM_REEL>           aplatissement terrestre
!>[S]   requa  :<PM_REEL>           rayon terrestre à l'équateur [m]
!>[S]   iorb   :<integer>           type de coordonnées (pm_kep, pm_cir,
!>                                        pm_equa, pm_cir_equa, pm_car,
!>                                        pm_hpha, pm_geoc, MSP_ENUM_TWOLINES,
!>                                        MSP_ENUM_ADAPTE_CNES, MSP_ENUM_RENTREE_SPHERIQUE, pm_geod_meca_vol,
!>                                        pm_geod_gps_ard)
!>[S]   param  :<PM_REEL,DIM=(6)>   coordonnées (cf. définition de iorb)
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
! COORDONNEES CONSULTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    type(MSP_TYPBUL), intent(IN) :: coord

    integer, intent(OUT), optional            :: iorb         
    real(KIND=PM_REEL), dimension(6), intent(OUT), optional :: param
    real(KIND=PM_REEL), intent(OUT), optional :: mu, requa, apla

    if ( PRESENT(mu) )        mu        = coord%mu
    if ( PRESENT(apla) )      apla      = coord%apla
    if ( PRESENT(requa) )     requa     = coord%requa
    if ( PRESENT(iorb) )      iorb      = coord%iorb
    if ( PRESENT(param) )     param(:)  = coord%param(:)

  end SUBROUTINE MSP_consulter_typbul


  SUBROUTINE MSP_consulter_typrep(repere,  typrep, rep, ech_temps_rep, lonref, &
       date_ref, date_refjs, lat, lon, alt, direction, planete, corcen, &
       cle_date, requa_r, apla_r, vitrot, pole_u, pole_v, obli, origdat)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consulter_typrep
!
!$Resume
! Cette routine permet de consulter le contenu d'une structure repère
!
!$Description
! Cette routine permet de consulter le contenu d'une structure repère
!
!$Auteur
! Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_consulter_typrep(repere,  [typrep], [rep], [ech_temps_rep], [lonref], &
!.           [date_ref], [date_refjs], [lat], [lon], [alt], [direction], [planete], [corcen], &
!.           [cle_date], [requa_r], [apla_r], [vitrot], [pole_u], [pole_v], [obli], [origdat])
!.    type(MSP_TYPREP) :: repere
!.    integer :: typrep
!.    real(KIND=PM_REEL), dimension(10) :: rep
!.    real(KIND=PM_REEL) :: lonref, date_ref, lat, lon, alt
!.    real(KIND=PM_REEL) :: direction
!.    integer :: planete, corcen, cle_date, ech_temps_rep
!.    real(KIND=PM_REEL) :: requa_r, apla_r, vitrot
!.    real(KIND=PM_REEL) :: pole_u, pole_v, obli
!.    type(tm_jour_sec) :: date_refjs
!.    integer :: origdat
!
!$Arguments
!>E     repere         :<MSP_TYPREP>         Structure repère à consulter
!>[S]   typrep         :<integer>            type de repère (pm_equa_vrai, pm_equa_moyen, pm_ecli_moy,
!>                                         pm_veis, pm_planeto_ref, pm_planeto_ref_iner, pm_planeto_vrai,
!>                                         pm_equa_uai, pm_topo)
!>[S]   rep            :<PM_REEL,DIM=(10)>   
!>[S]   ech_temps_rep  :<integer>            échelle de temps de la date de référence
!>[S]   lonref         :<PM_REEL>            longitude de référence
!>[S]   date_ref       :<PM_REEL>            date de référence [JJ]
!>[S]   date_refjs     :<tm_jour_sec>        date de référence jour/secondes 1950
!>[S]   lat            :<PM_REEL>            latitude station  [rad]
!>[S]   lon            :<PM_REEL>            longitude station [rad]
!>[S]   alt            :<PM_REEL>            altitude station  [m]
!>[S]   direction      :<PM_REEL>            direction station
!>[S]   planete        :<integer>            planète (eph_mercure,eph_venus,eph_terre,eph_mars,
!>                                               eph_jupiter,eph_saturne,eph_uranus,eph_neptune,eph_pluton)
!>[S]   corcen         :<integer>            corps central (eph_mercure,eph_venus,eph_terre,eph_mars,
!>                                          eph_jupiter,eph_saturne,eph_uranus,eph_neptune,eph_pluton,
!>                                          eph_soleil)
!>[S]   cle_date       :<integer>            type de date utilisé pour la définition du repère :
!>                                              pm_1janvier1950_00h00  date du repère = 1/1/1950 à 0h00         
!>                                              pm_1janvier2000_12h00  date du repère = 1/1/2000 à 12h00 (18262.5 JJCNES)         
!>                                              MSP_ENUM_ECHD_DATE_REF date du repère fixée par une date de référence         
!>                                              pm_autre_date          date du repère fixée par la date du bulletin 
!>[S]   requa_r        :<PM_REEL>            rayon terrestre à l'équateur pour les coordonnées station ou rampe [m]
!>[S]   apla_r         :<PM_REEL>            aplatissement terrestre pour les coordonnées station ou rampe
!>[S]   vitrot         :<PM_REEL>            vitesse de rotation [rad/s]
!>[S]   pole_u         :<PM_REEL>            coordonnée u du pole vrai [rad]
!>[S]   pole_v         :<PM_REEL>            coordonnée v du pole vrai [rad]
!>[S]   obli           :<PM_REEL>            obliquite du corps central [rad]
!>[S]   origdat        :<integer>            origine des dates (0=J50, 1=MJD2000)
!
!$Common
!
!$Routines
!- md_joursec_jourfrac
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
! REPERE CONSULTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none
 
    type(MSP_TYPREP), intent(IN) :: repere
    integer, intent(OUT), optional            :: typrep
    real(KIND=PM_REEL), dimension(10), intent(OUT), optional :: rep

    real(KIND=PM_REEL), intent(OUT), optional :: lonref, date_ref, lat, lon, alt
    real(KIND=PM_REEL), intent(OUT), optional :: direction
    integer, intent(OUT), optional            :: planete, corcen, cle_date,  ech_temps_rep
    real(KIND=PM_REEL), intent(OUT), optional :: requa_r, apla_r, vitrot
    real(KIND=PM_REEL), intent(OUT), optional :: pole_u, pole_v, obli
    type(tm_jour_sec) , intent(OUT), optional :: date_refjs
    integer, intent(OUT), optional            :: origdat

    ! variables locales
    type(tm_code_retour) :: code_retour

    if (present(typrep))   typrep   = repere%typrep
    if (present(origdat))  origdat  = repere%origdat
    if (present(corcen))   corcen   = repere%corcen
    if (present(requa_r))  requa_r  = repere%station%ellips%r_equa
    if (present(apla_r))   apla_r   = repere%station%ellips%apla
    if (present(vitrot))   vitrot   = repere%vrot

    if ( PRESENT(cle_date) ) cle_date= repere%cle_date
    if ( PRESENT(lonref) )   lonref   = repere%lonref
    if ( PRESENT(date_ref) ) call md_joursec_jourfrac(repere%date_ref,date_ref,code_retour)
    if ( PRESENT(date_refjs) ) date_refjs = repere%date_ref
    if ( PRESENT(ech_temps_rep) ) ech_temps_rep = repere%ech_temps_rep
    if ( PRESENT(lat) )      lat      = repere%station%geod%lat
    if ( PRESENT(lon) )      lon      = repere%station%geod%long
    if ( PRESENT(alt) )      alt      = repere%station%geod%haut
    if ( PRESENT(direction) )  direction  = repere%station%axe_x

    if ( PRESENT(planete) )  planete  = repere%planete
    if ( PRESENT(pole_u) )   pole_u  =  repere%pole%u
    if ( PRESENT(pole_v) )   pole_v  = repere%pole%v
    if ( PRESENT(obli) )     obli  = repere%obli


      ! - Appel par PSIMU : les caractéristiques du repères sont passée dans le tableau rep(10)
     if (present(rep)) then
        rep(:) = 0._pm_reel
        rep(1) =   real(repere%typrep,kind=pm_reel)
        select case(repere%typrep)
           case(pm_planeto_ref, pm_planeto_ref_iner, pm_planeto_vrai) 
              rep(3) =  repere%lonref 
           case(pm_topo)
              rep(4) = repere%station%geod%lat
              rep(5) = repere%station%geod%long
              rep(6) = repere%station%geod%haut
              rep(7) = repere%station%axe_x 
         end select
         
         if (repere%cle_date == MSP_ENUM_ECHD_DATE_REF) then
            rep(4) = dfloat(repere%date_ref%jour )
            rep(5) = repere%date_ref%sec
            rep(2) = real(repere%ech_temps_rep,kind=pm_reel)
         endif
      endif
      
    
  end SUBROUTINE MSP_consulter_typrep


  character(LEN= MSP_LONG_CHAINE) function MSP_type_repere(type) result(string)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_type_repere
!
!$Resume
!  Transforme le type de repere en une chaine de caractere
!
!$Description
!  Transforme le type de repere en une chaine de caractere
!
!$Auteur
!  02/09/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  string = MSP_type_repere(type)
!.    integer :: type
!
!$Arguments
!>E     type    :<integer>               Code du repère
!>S     string  :<LEN=MSP_LONG_CHAINE>   Chaine de caractère donnant le type du repère
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
!  REPERE TYPE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    integer, intent(IN) :: type

    SELECT CASE(type)

    CASE (pm_equa_vrai)
       string = "GAMMA VRAI DE LA DATE"
    CASE(pm_equa_moy)
       string = "GAMMA MOYEN"
    CASE(pm_ecli_moy)
       string = "ECLIPTIQUE"
    CASE(pm_veis)
       string = "DE VEIS"
    CASE(pm_planeto_ref)
       string = "PLANETO_REF"
    CASE(pm_planeto_ref_iner)
       string = "PLANETO_REF_INER"
    CASE(pm_planeto_vrai)
       string = "PLANETO_VRAI"
    CASE(pm_equa_uai)
       string = "PQ"
    CASE(pm_topo)
       string = "STATION"
    CASE DEFAULT 
       string = 'code inconnu' 
    end SELECT

  end function MSP_type_repere


  character(LEN= MSP_LONG_CHAINE) function MSP_type_bulletin(type) result(string)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_type_bulletin
!
!$Resume
!  Transforme le type de bulletin em une chaine de caractere
!
!$Description
!  Transforme le type de bulletin em une chaine de caractere
!
!$Auteur
!  02/09/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  string = MSP_type_bulletin(type)
!.    integer :: type
!
!$Arguments
!>E     type    :<integer>               Code du bulletin
!>S     string  :<LEN=MSP_LONG_CHAINE>   Chaine de caractère donnant le type du bulletin
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
!  BULLETIN TYPE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    integer, intent(IN) :: type

    SELECT CASE(type)

    CASE(pm_kep)
       string = "KEPLERIEN"
    CASE(pm_cir)
       string = "ADAPTE CIRCULAIRE"
    CASE(pm_equa)
       string = "ADAPTE EQUATORIAL"
    CASE(pm_cir_equa)
       string = "ADAPTE CIRCULAIRE ET EQUATORIAL"
    CASE(pm_car)
       string = "CARTESIENS"
    CASE(pm_hpha)
       string = "PERIGEE/APOGEE"
    CASE(pm_geoc)
       string = "SPHERIQUE"
    CASE(pm_geod_meca_vol)
       string = "RENTREE ELLIPSOIDIQUE"
    CASE(pm_geod_gps_ard)
       string = "POINT GPS"
    CASE DEFAULT 
       string = 'code inconnu' 
    end SELECT

  end function MSP_type_bulletin

  character(LEN= MSP_LONG_CHAINE) function MSP_precession(type) result(string)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_precession
!
!$Resume
!  Transforme le code precession en une chaine de caractere
!
!$Description
!  Transforme le code precession MSLIB90 en une chaine de caractere
!
!$Auteur
!  Florence VIVARES (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!  string = MSP_precession(type)
!.    integer :: type
!
!$Arguments
!>E     type    :<integer>               Code MSLIB90 precession
!>S     string  :<LEN=MSP_LONG_CHAINE>   Chaine de caractère donnant le nom en clair
!                                        du type de precession
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
!  BULLETIN PRECESSION
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    integer, intent(IN) :: type

    SELECT CASE(type)
    CASE(pm_lieske_wahr_aoki)
       string = "Lieske"
    CASE(pm_uai1994)
       string = "UAI 1994"
    CASE(pm_uai2000)
       string = "UAI 2000"
    CASE DEFAULT 
       string = 'code inconnu' 
    end SELECT

  end function MSP_precession


  character(LEN= MSP_LONG_CHAINE) function MSP_type_echelle_date(type) result(string)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_type_echelle_date
!
!$Resume
!  Transforme le type d'echelle de date en une chaine de caractere
!
!$Description
!  Transforme le type d'echelle de date en une chaine de caractere
!
!$Auteur
!  02/09/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  string = MSP_type_echelle_date(type)
!.    integer :: type
!
!$Arguments
!>E     type    :<integer>               Code de l'échelle de date
!>S     string  :<LEN=MSP_LONG_CHAINE>   Chaine de caractère avec nom en clair
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
!  BULLETIN TYPE ECHELLE DATE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    integer, intent(IN) :: type

    SELECT CASE(type)

    CASE(pm_1janvier1950_00h00)
       string = "ORIGINE 1950"
    CASE(pm_1janvier2000_12h00)
       string = "ORIGINE 2000"
    CASE(pm_autre_date)
       string = "DATE BULLETIN"
    CASE(MSP_ENUM_ECHD_DATE_REF)
       string = "AUTRE DATE"
    CASE DEFAULT 
       string = 'code inconnu' 
    end SELECT

  end function MSP_type_echelle_date

  character(LEN= MSP_LONG_CHAINE) function MSP_type_echelle_temps(type) result(string)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_type_echelle_temps
!
!$Resume
!  Transforme le type d'echelle de temps en une chaine de caractere
!
!$Description
!  Transforme le type d'echelle de temps en une chaine de caractere
!
!$Auteur
!  02/09/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  string = MSP_type_echelle_temps(type)
!.    integer :: type
!
!$Arguments
!>E     type    :<integer>               Code de l'échelle de temps
!>S     string  :<LEN=MSP_LONG_CHAINE>   Chaine de caractère avec nom en clair
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
!  BULLETIN TYPE ECHELLE TEMPS
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    integer, intent(IN) :: type

    SELECT CASE(type)

    CASE(pm_TUC)
       string = "TUC"
    CASE(pm_TE)
       string = "TE"
    case default
       string = 'code inconnu'
    end SELECT

  end function MSP_type_echelle_temps


  SUBROUTINE MSP_afficher_bulletin(bulletin, lu)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_afficher_bulletin
!
!$Resume
!  Cette routine permet d'afficher le contenu de la structure bulletin dans une unité logique
!
!$Description
!  Cette routine permet d'afficher le contenu de la structure bulletin dans une unité logique
!
!$Auteur
!  Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_afficher_bulletin(bulletin, lu)
!.    integer :: lu
!.    type(MSP_BULLETIN) :: bulletin
!
!$Arguments
!>E     bulletin  :<MSP_BULLETIN>   Bulletin MECASPA
!>E     lu        :<integer>        lfn du fichier de sauvegarde
!
!$Common
!
!$Routines
!- md_joursec_jourfrac
!- MSP_signaler_message
!- MSP_jj_to_jcd
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  BULLETIN AFFICHER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    integer, intent(in) :: lu
    type(MSP_BULLETIN), intent(in) :: bulletin

    real(kind=pm_reel) :: datejj50
    integer :: ian, imois, ijour, iheure, imin, isec, imils, ier    
    type(tm_code_retour) :: code_retour
    character(len=8) :: oo

    ! Initialisations
    datejj50=0.0_PM_REEL
    ian=0
    imois=0
    ijour=0
    iheure=0
    imin=0
    isec=0
    imils=0
    ier=0
    oo = 'MJD1950'
    if (bulletin%repere%origdat == 1) oo = 'MJD2000'

    ! Date du bulletin
    write(lu, 100) oo, bulletin%datbul%jour, bulletin%datbul%sec

    call md_joursec_jourfrac(bulletin%datbul,datejj50,code_retour)
    call MSP_signaler_message (ier_mslib=code_retour)

    call MSP_jj_to_jcd(datejj50, ian, imois, ijour, iheure, imin, isec, &
         imils, ier, jjsec=bulletin%datbul, origdat=bulletin%repere%origdat)

    write(lu, 101) ian, imois, ijour, iheure, imin, isec, imils

100 format(' DATE = ',A8,I8, ' - ', F10.3)
101 format('       ',I4.4,'/',I2.2,'/',I2.2,' ',I2.2,':',I2.2,' ',I2.2,'.',I3.3, 's')

    ! Echelles
    write(lu, '(a,a)') 'ECHELLE DE TEMPS : ', trim(MSP_type_echelle_temps(bulletin%ech_temps_bul))
    write(lu, '(a,a)') 'PRECESSION       : ', trim(MSP_precession(bulletin%modprec))
    write(lu, '(a)') ''

    ! Structure coord
    write(lu, '(a,a)') 'TYPE   = ', trim(MSP_type_bulletin(bulletin%coord%iorb))
    write(lu, '(a,g21.12)') 'MU     = ', bulletin%coord%mu
    write(lu, '(a,g21.12)') 'REQ    = ', bulletin%coord%requa
    write(lu, '(a,g21.12)') 'APLA   = ', bulletin%coord%apla
    write(lu, '(a,g21.12)') 'PARAM  = ', bulletin%coord%param(1)
    write(lu, '(a,g21.12)') '         ', bulletin%coord%param(2)
    write(lu, '(a,g21.12)') '         ', bulletin%coord%param(3)
    write(lu, '(a,g21.12)') '         ', bulletin%coord%param(4)
    write(lu, '(a,g21.12)') '         ', bulletin%coord%param(5)
    write(lu, '(a,g21.12)') '         ', bulletin%coord%param(6)
    write(lu, '(a)') ''

    ! Structure repere
    write(lu, '(a,a)') 'Type REPERE   = ', trim(MSP_type_repere(bulletin%repere%typrep))
    write(lu, '(a,a)') '  Echelle de date : ', trim(MSP_type_echelle_date(bulletin%repere%cle_date))

    write(lu, 100) oo, bulletin%repere%date_ref%jour, bulletin%repere%date_ref%sec

    call md_joursec_jourfrac(bulletin%repere%date_ref,datejj50,code_retour)
    call MSP_signaler_message (ier_mslib=code_retour)
    if (MSP_gen_messages("MSP_afficher_bulletin")) return
    call MSP_jj_to_jcd(datejj50, ian, imois, ijour, iheure, imin, isec, &
         imils, ier, jjsec=bulletin%repere%date_ref,origdat=bulletin%repere%origdat)
    write(lu, 101) ian, imois, ijour, iheure, imin, isec, imils
    write(lu, '(a,a)') 'Echelle de temps     = ', trim(MSP_type_echelle_temps(bulletin%repere%ech_temps_rep))

    write(lu, '(a,i9)') 'Planete : ' ,  bulletin%repere%planete
    write(lu, '(a,i9)') 'Corps central   = ', bulletin%repere%corcen
    write(lu, '(a,g21.12)') 'vitrot  = ', bulletin%repere%vrot
    write(lu, '(a,g21.12)') 'lonref  = ', bulletin%repere%lonref
    if (bulletin%repere%typrep == pm_topo) then
       write(lu, '(a,g21.12)') 'lat.  = ', bulletin%repere%station%geod%lat
       write(lu, '(a,g21.12)') 'long.  = ', bulletin%repere%station%geod%long
       write(lu, '(a,g21.12)') 'haut.   = ', bulletin%repere%station%geod%haut
       write(lu, '(a,g21.12)') 'direction  = ', bulletin%repere%station%axe_x
    endif
    write(lu, '(a,g21.12)') 'requa_r = ', bulletin%repere%station%ellips%r_equa
    write(lu, '(a,g21.12)') 'apla_r = ', bulletin%repere%station%ellips%apla

    write(lu, '(a)') ''
    if (MSP_gen_messages("MSP_afficher_bulletin" )) return

  end SUBROUTINE MSP_afficher_bulletin

  SUBROUTINE MSP_ecrire_bulletin(bulletin, subr_write, nom_dom, nacc, section, type_section)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_ecrire_bulletin
!
!$Resume
!  Cette routine permet d'écrire une structure bulletin dans un fichier
!
!$Description
!  Cette routine permet d'écrire une structure bulletin dans un fichier
!
!$Auteur
!  Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_ecrire_bulletin(bulletin, subr_write, nom_dom, [nacc], [section], [type_section])
!.    type(MSP_BULLETIN) :: bulletin
!.    external subr_write
!.    character(LEN=*) :: nom_dom
!.    integer :: nacc
!.    integer, dimension(:) :: type_section
!.    character(LEN=*), dimension(:) :: section
!
!$Arguments
!>E     bulletin      :<MSP_BULLETIN>      Structure bulletin à écrrire dans le fichier
!>E/S   subr_write    :<external>          Routine d'écriture externe
!>E     nom_dom       :<LEN=*>             Nom du domaine de traduction
!>[E]   nacc          :<integer>           Numéro d'accès MADONA
!>[E]   section       :<LEN=*,DIM=(:)>     Nom des sections MADONA dans lesquelles on souhaite voir apparaître le bulletin
!>[E]   type_section  :<integer,DIM=(:)>   Type des sections ci-dessus
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- md_joursec_jourfrac
!- subr_write
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
! BULLETIN ECRIRE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    type(MSP_BULLETIN), intent(IN) :: bulletin
    external subr_write
    
    character(LEN=*), intent(IN) :: nom_dom
    integer, intent(IN), optional :: nacc
    integer, dimension(:), intent(IN), optional :: type_section
    character(LEN=*), dimension(:), intent(IN), optional :: section

    integer :: ntab, ier

    ! - Variables utilisées pour reconstituer les paramètres GS_LIB
    integer :: dir_gs
    real(kind=pm_reel), dimension(10)  :: rep
    real(kind=pm_reel)  :: apla_gs, apla_r_gs, datebul
    type(tm_code_retour) :: code_retour

    ! Initialisations
    datebul=0.0_PM_REEL

    if (PRESENT(section).and.PRESENT(type_section).and.PRESENT(nacc)) then 

       ntab = size(section)
       ier = MSP_acc_create(nacc, section, type_section, ntab)
       ! DM1058 - Gestion d'erreur
       if (ier < 0) then
          call MSP_signaler_message (cle_mes="MSP_probleme_select_2", &
               routine="MSP_creer_bulletin_madona")
       endif
       if (MSP_gen_messages("MSP_ecrire_bulletin")) return


    else if ( ( (PRESENT(section).or.PRESENT(type_section)).or.PRESENT(nacc) ).and.&
         (.not.((PRESENT(section).and.PRESENT(type_section).and.PRESENT(nacc)))))then 

       call MSP_signaler_message (cle_mes="MSP_ERREUR_ARGUMENTS_001", &
            routine="MSP_ecrire_bulletin", type=MSP_ENUM_ERREUR, &
            partie_variable="section, type_section et nacc")
       return

    end if

    ! - Recomposition du tableau rep
    rep(1) = real(bulletin%repere%typrep,kind=pm_reel)
    select case(bulletin%repere%typrep)
    case(pm_planeto_ref, pm_planeto_ref_iner, pm_planeto_vrai) 
       rep(3) = bulletin%repere%lonref
    case(pm_topo)
       rep(4) = bulletin%repere%station%geod%lat
       rep(5) = bulletin%repere%station%geod%long
       rep(6) = bulletin%repere%station%geod%haut
       ! -- Conversion de la direction en valeur GS_LIB
       dir_gs = nint(direction_gslib(bulletin%repere%station%axe_x))
       if (MSP_gen_messages("MSP_ecrire_bulletin - direction")) return
       rep(7) = dir_gs
    end select
    
    if (bulletin%repere%cle_date == MSP_ENUM_ECHD_DATE_REF) then
       rep(4) = dfloat(bulletin%repere%date_ref%jour)
       rep(5) = bulletin%repere%date_ref%sec
       rep(2) = real(bulletin%repere%ech_temps_rep,kind=pm_reel)
    endif

    ! - L'aplatissement dans la GS_LIB est inversé
    if (abs(bulletin%coord%apla) > MSP_epsilon_apla) then
       apla_gs = 1. / bulletin%coord%apla
    else
       apla_gs = 999.E10_pm_reel
    endif

    if ( abs(bulletin%repere%station%ellips%apla) > MSP_epsilon_apla) then
       apla_r_gs = 1. /  bulletin%repere%station%ellips%apla
    else
       apla_r_gs = 999.E10_pm_reel
    endif
    

    call md_joursec_jourfrac(bulletin%datbul,datebul,code_retour)
    
    call subr_write(bulletin%coord%iorb, rep(:), bulletin%datbul%jour, &
         bulletin%datbul%sec, bulletin%coord%param, &
         bulletin%coord%mu, bulletin%coord%requa, apla_gs, &
         bulletin%repere%station%ellips%r_equa, apla_r_gs,bulletin%repere%planete, &
         bulletin%repere%corcen, bulletin%repere%cle_date, &
         bulletin%repere%vrot, bulletin%ech_temps_bul, bulletin%repere%obli, &
         bulletin%repere%pole%u, bulletin%repere%pole%v, nom_dom,0, 1994)

         
  end SUBROUTINE MSP_ecrire_bulletin

  SUBROUTINE MSP_ecrire_typrep(repere,subr_write,nom_dom,nacc,section,type_section)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_ecrire_typrep
!
!$Resume
!  Cette routine permet d'écrire une structure repère dans un fichier
!
!$Description
!  Cette routine permet d'écrire une structure repère dans un fichier
!
!$Auteur
!  Florence VIVARES (ATOS origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_ecrire_typrep(repere,subr_write,nom_dom,[nacc],[section],[type_section])
!.    type(MSP_TYPREP) :: repere
!.    external subr_write
!.    character(LEN=*) :: nom_dom
!.    integer :: nacc
!.    integer, dimension(:) :: type_section
!.    character(LEN=*), dimension(:) :: section
!
!$Arguments
!>E     repere        :<MSP_TYPREP>        repère à sauver
!>E/S   subr_write    :<external>          subroutine de sauvegarde (doit avoir la syntaxe d'appel de write_gs_repere_ip)
!>E     nom_dom       :<LEN=*>             domaine de tradutcion
!>[E]   nacc          :<integer>           moyen d'acces MADONA
!>[E]   section       :<LEN=*,DIM=(:)>     tableau de sections à
!                      l'interieur desquels lire la structure contenant le repère
!>[E]   type_section  :<integer,DIM=(:)>   tableau de types
!                      (ACC_STRUCT ou ACC_TABL) de même taille que section.
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- subr_write
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

    type(MSP_TYPREP), intent(IN) :: repere
    external subr_write
    
    character(LEN=*), intent(IN) :: nom_dom
    integer, intent(IN), optional :: nacc
    integer, dimension(:), intent(IN), optional :: type_section
    character(LEN=*), dimension(:), intent(IN), optional :: section

    ! - Variables utilisées pour reconstituer les paramètres GS_LIB
    integer :: dir_gs, ntab, ier
    real(kind=pm_reel), dimension(10)  :: rep
    real(kind=pm_reel)  :: apla_r_gs


    if (PRESENT(section).and.PRESENT(type_section).and.PRESENT(nacc)) then 

       ntab = size(section)
       ier = MSP_acc_create(nacc, section, type_section, ntab)
       ! DM1058 - Gestion d'erreur
       if (ier < 0) then
          call MSP_signaler_message (cle_mes="MSP_probleme_select_2", &
               routine="MSP_ecrire_typrep")
       endif
       if (MSP_gen_messages("MSP_ecrire_typrep")) return


    else if ( ( (PRESENT(section).or.PRESENT(type_section)).or.PRESENT(nacc) ).and.&
         (.not.((PRESENT(section).and.PRESENT(type_section).and.PRESENT(nacc)))))then 

       call MSP_signaler_message (cle_mes="MSP_ERREUR_ARGUMENTS_001", &
            routine="MSP_ecrire_typrep", type=MSP_ENUM_ERREUR, &
            partie_variable="section, type_section et nacc")
       return

    end if

    ! - Recomposition du tableau rep
    rep(1) = real(repere%typrep,kind=pm_reel)
    select case(repere%typrep)
    case(pm_planeto_ref, pm_planeto_ref_iner, pm_planeto_vrai) 
       rep(3) = repere%lonref
    case(pm_topo)
       rep(4) = repere%station%geod%lat
       rep(5) = repere%station%geod%long
       rep(6) = repere%station%geod%haut
       ! -- Conversion de la direction en valeur GS_LIB
       dir_gs = nint(direction_gslib(repere%station%axe_x))
       if (MSP_gen_messages("MSP_ecrire_typrep - direction")) return
       rep(7) = dir_gs
    end select
    
    if (repere%cle_date == MSP_ENUM_ECHD_DATE_REF) then
       rep(4) = dfloat(repere%date_ref%jour)
       rep(5) = repere%date_ref%sec
       rep(2) = real(repere%ech_temps_rep,kind=pm_reel)
    endif

    ! - L'aplatissement dans la GS_LIB est inversé
    if ( abs(repere%station%ellips%apla) > MSP_epsilon_apla) then
       apla_r_gs = 1. /  repere%station%ellips%apla
    else
       apla_r_gs = 999.E10_pm_reel
    endif

    call subr_write(rep(:), repere%station%ellips%r_equa , apla_r_gs , &
         repere%planete, repere%corcen, repere%cle_date, repere%vrot,  &
         repere%ech_temps_rep, nom_dom, repere%obli, repere%pole%u, &
         repere%pole%v,0)

 
  end SUBROUTINE MSP_ecrire_typrep

  real(KIND=PM_REEL) function MSP_argument_latitude(bulletin, bull_kepl) result(aol)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_argument_latitude
!
!$Resume
!  Cette routine calcule l'argument de latitude
!
!$Description
!  Cette routine calcule l'argument de latitude à partir d'une structure bulletin ou des paramètres képleriens
!
!$Auteur
!  Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  aol = MSP_argument_latitude([bulletin], [bull_kepl])
!.    type(MSP_BULLETIN) :: bulletin
!.    real(KIND=PM_REEL), dimension(6) :: bull_kepl
!
!$Arguments
!>[E/S] bulletin   :<MSP_BULLETIN>      Bulletin MECASPA
!>[E]   bull_kepl  :<PM_REEL,DIM=(6)>   Coordonnées képlèriennes
!>S     aol        :<PM_REEL>           Argument de latitude
!
!$Common
!
!$Routines
!- MSP_consulter_bulletin
!- MSP_signaler_message
!
!$Include
!
!$Module
!
!$Remarques
!  Si bulletin et bull_kepl sont donnés ensemble, le calcul est effectué sur bulleitn
!  Si aucun des 2 n'est donné, la sortie est aléatoire
!
!$Mots-cles
!  BULLETIN ARGUMENT LATITUDE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments en entrée
    type(MSP_BULLETIN), intent(INOUT), optional :: bulletin
    real(KIND=PM_REEL), dimension(6), intent(IN), optional :: bull_kepl

    ! Variables locales
    real(KIND=PM_REEL), dimension(6) :: bulletin_kepl, bulletin_circ
    type(MSP_BULLETIN) :: bulletin_tmp
    real(KIND=PM_REEL) :: v

    ! Initialisation
    aol = 0._pm_reel

    if (PRESENT(bull_kepl)) then 
       bulletin_kepl(:) = bull_kepl(:)
       v = MSP_moyenne_to_vraie(bulletin_kepl(6), bulletin_kepl(2))
       aol = bulletin_kepl(4) + v
    end if

    ! Recherche du bulletin keplerien
    if (PRESENT(bulletin)) then 

       bulletin_tmp = MSP_convertir_bulletin(bulletin, iorb=pm_kep, &
            typrep=pm_veis,cle_date=pm_autre_date)

       call MSP_consulter_bulletin(bulletin_tmp, param=bulletin_kepl)
       v = MSP_moyenne_to_vraie(bulletin_kepl(6), bulletin_kepl(2))
       aol = bulletin_kepl(4) + v

       ! Si l'orbite est proche de l'orbite circulaire
       if (bulletin_kepl(2) <= 1.E-5_PM_REEL) then 
          bulletin_tmp = MSP_convertir_bulletin(bulletin, &
               iorb=pm_cir, &
               typrep=pm_veis,cle_date=pm_autre_date)
          call MSP_consulter_bulletin(bulletin_tmp, param=bulletin_circ)
          aol = bulletin_circ(6)
       end if
    end if

    ! DM1058 - Si aucun argument n'est donné en entrée, on sort une erreur
    if ((.not.PRESENT(bull_kepl)).and.(.not.PRESENT(bulletin))) then
       call MSP_signaler_message (cle_mes="MSP_ERREUR_ARGUMENTS_009", &
            routine="MSP_argument_latitude")       
    endif


  end function MSP_argument_latitude
   
  subroutine MSP_init_structure_topo(topo)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_init_structure_topo
!
!$Resume
!  Initialisation par des valeurs par défaut d'une donnée de type tm_def_topo
!
!$Description
!  Le rayon équatorial et l'aplatissement sont initialisés avec les constates
!  terrestres définies dans le module
!  Sinon tout à zero.
!
!$Auteur
!  O KVERNELAND - Atos Origin
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_init_structure_topo(topo)
!.    type(tm_def_topo) :: topo
!
!$Arguments
!>E/S   topo  :<tm_def_topo>   la donnée à initialiser
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
    
    type(tm_def_topo), intent(INOUT) :: topo

    ! Variables locales
    integer :: trouve
    real(kind=pm_reel) :: apla_loc
    character(LEN=32) :: unite


    ! - Initialisation de la structure à zeo
    topo%geod%lat      = 0._pm_reel    
    topo%geod%long     = 0._pm_reel   
    topo%geod%haut     = 0._pm_reel     
    ! Par défaut les caractéristiques terre
    !topo%ellips%r_equa = MSP_REQUA_TERRE  
    if (.not.msp_cps_requa_lu) then
       trouve = cps_getCsteThCourante(eph_terre, "requa", MSP_cps_REQUA_TERRE, unite)
       msp_cps_requa_lu =.true.
    endif
    topo%ellips%r_equa = MSP_cps_REQUA_TERRE
    ! if (trouve == 1) topo%ellips%r_equa = apla_loc
    !topo%ellips%apla   = MSP_APLA_TERRE  
    if (.not.msp_cps_apla_lu) then
       trouve = cps_getCsteThCourante(eph_terre, "apla", MSP_cps_APLA_TERRE, unite)
       msp_cps_apla_lu =.true.
    endif
    apla_loc = MSP_cps_APLA_TERRE
    topo%ellips%apla = apla_loc
    ! Cas d'aplatissement défini en > 1, on inverse
    if (apla_loc > 1._pm_reel) topo%ellips%apla = 1._pm_reel/apla_loc
    topo%axe_x         = 0._pm_reel
    
  end subroutine MSP_init_structure_topo

end module MSP_BULLETIN_DEF
