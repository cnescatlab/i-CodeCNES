subroutine mp_atm_US76 (alt, tempe, pres, dens, code_retour)

! (C) Copyright CNES - MSLIB - 1998-2004

!************************************************************************
!
! But:  Modele d'ATMosphere US76
! ===
!
! Note d'utilisation:  1) Compte tenu de la precision des modeles d'atmosphere, on peut utilser la hauteur geodesique 
! ==================      pour l'altitude alt.
!                      2) L'altitude alt doit etre comprise entre 0 et 1000 km (limites de validite du modele)
!                      3) References : + U.S. Standard Atmosphere, 1976, Washington DC, 10/76.
!                                        [les numeros d'equations indiques correspondent a cette ref.]
!                                      + note explicative: Le modele d'atmosphere US 76; 
!                                        JF Goester;TE/IS/MS/AS/439; 17/12/90.  
!                  
!$Historique
! ==========
!   + Version 1.0 (SP 289 ed01 rev00): creation a partir de la routine MCUS76 de la MSLIB f77
!                         (Date: 11/1998 - Realisation: Veronique Lepine)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite (dont revision algorithme)
!                         (Date: 05/2003 - Realisation: Bruno Revelin, Veronique Lepine)
!   + Version 6.0 (DE 628 ed01 rev00): amelioration temps calcul
!                         (Date: 05/2004 - Realisation: Guylaine Prat)
!   + Version 6.3 (DM-ID 239) : Performances en temps de calcul
!                 (Date: 10/2005 - Realisation: ATOS ORIGIN) 
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!
!VERSION:V6.13:FA-ID:1410:30/09/2010:Ajout marqueur fin historique
!
!Revision 362 2013/02/15 bbjc
!DM-ID 1513: Suppression des warnings de compilation
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======
use int_util_internes, only : mui_dot_product6

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in)                   ::  alt   ! altitude
real(pm_reel), intent(out)                  ::  tempe ! temperature
real(pm_reel), intent(out)                  ::  pres  ! pression
real(pm_reel), intent(out)                  ::  dens  ! densite
type(tm_code_retour), intent(out)           ::  code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

integer          ::     i,j,ii,jj,kk,kmoins       ! compteurs et indices de tableaux
integer          ::     k_dicho, pas_dicho          ! variables interm pour recherche d'intervalle par dichotomie
real(pm_reel)    ::     tmol,rhpot,xmol,rn,zj,coeff ! intermediaires de calcul des parametres 
real(pm_reel)    ::     xi,rcar                     ! pour le calcul de la temperature cinetique
real(pm_reel), dimension(2:7) :: pen, rnn           ! interpolation masse molaire
real(pm_reel)    ::     temperature, pression, densite ! variables intermediaires du calcul
real(pm_reel)    ::     altitude ! variable intermediaire du calcul
real (pm_reel)   ::     retour

integer, parameter         ::     nmax = 430        ! dimensions de tableaux

!   * altitudes geopotentielles de debut et de fin des differents gradients de temperature
real(pm_reel), dimension(0:12), parameter :: &
     htab = (/ 0._pm_reel,11.e3_pm_reel,20.e3_pm_reel,32.e3_pm_reel,47.e3_pm_reel,51.e3_pm_reel, &
     71.e3_pm_reel,84.852e3_pm_reel,0._pm_reel,0._pm_reel,0._pm_reel,0._pm_reel,0._pm_reel /)

!   * altitudes geometriques 
real(pm_reel), dimension(0:12), parameter    ::&
     ztab = (/ 0._pm_reel,0._pm_reel,0._pm_reel,0._pm_reel,0._pm_reel,0._pm_reel,0._pm_reel,86.e3_pm_reel,91.e3_pm_reel, &
               110.e3_pm_reel,120.e3_pm_reel,500.e3_pm_reel,1000.e3_pm_reel /)

!   * coefficients correcteurs pour le calcul de la temperature  entre 80 et 86 km d'altitude
real(pm_reel), dimension(0:12), parameter    ::&
     xmm0tb = (/ 1._pm_reel,0.999996_pm_reel,0.999989_pm_reel,0.999971_pm_reel,0.999941_pm_reel, &
                 0.999909_pm_reel,0.999870_pm_reel,0.999829_pm_reel,0.999786_pm_reel, &
                 0.999741_pm_reel,0.999694_pm_reel,0.999641_pm_reel,0.999579_pm_reel /)

!   * derivees de la temperature par rapport a rhpot ou rzgeo
real(pm_reel), dimension(0:12), parameter    :: &
     dttab = (/ -6.5e-3_pm_reel,0._pm_reel,1.e-3_pm_reel,2.8e-3_pm_reel,0._pm_reel,-2.8e-3_pm_reel,-2.0e-3_pm_reel, &
                 0._pm_reel,0._pm_reel,12.e-3_pm_reel,0._pm_reel,0._pm_reel,0._pm_reel /)

!   * pressions pour htab
real(pm_reel), dimension(0:7), parameter     :: &
     ptab = (/ 1.01325e5_pm_reel,2.2632e4_pm_reel,5.4748e3_pm_reel,8.6801e2_pm_reel, &
               1.1090e2_pm_reel,6.6938e1_pm_reel,3.9564_pm_reel,3.7338e-1_pm_reel /)

!   * temperatures correspondant a htab ou ztab
real(pm_reel), dimension(0:12), parameter    ::&
     ttab = (/0._pm_reel,0._pm_reel,0._pm_reel,0._pm_reel,0._pm_reel,0._pm_reel,0._pm_reel,186.8673_pm_reel,186.8673_pm_reel,&
            240._pm_reel,360._pm_reel,999.24_pm_reel,1000._pm_reel /)

real(pm_reel), dimension(0:7), parameter     ::&
     tmtab = (/ 288.15_pm_reel,216.65_pm_reel,216.65_pm_reel,228.65_pm_reel,270.65_pm_reel, &
                270.65_pm_reel,214.65_pm_reel,186.946_pm_reel /)

!   * masses molaires des elements N2,O,O2,Ar,He et H
real(pm_reel), dimension(7), parameter     ::&
     rmoltb =(/ 0._pm_reel,28.0134_pm_reel,15.9994_pm_reel,31.9988_pm_reel,39.948_pm_reel, 4.0026_pm_reel,1.0080_pm_reel /)

real(pm_reel), parameter :: r0=6356.766e3_pm_reel  ! rayon de la Terre utilise pour le calcul des altitudes geopotentielles
real(pm_reel), parameter :: g0=9.80665_pm_reel     ! acceleration due a la gravite au niveau de la mer

real(pm_reel), parameter ::  xmol0=28.9644_pm_reel     !   masse moleculaire de l'atmosphere au sol (kg/kmol)
real(pm_reel), parameter ::  boltz=1.380622e-23_pm_reel!   constante de Boltzmann (n.m/k)
real(pm_reel), parameter ::  rstar=8.31432e3_pm_reel   !   constante des gaz parfaits (n.m/(kmol.k))

real(pm_reel), parameter ::  rappor=g0*xmol0/rstar

real(pm_reel), parameter ::  rlambd = 1.875e-05_pm_reel! = dttab(9)/(ttab(12)-ttab(10))   (constante) 
                                                       ! attention: valeur pour des altitudes en metres

real(pm_reel), parameter ::  epspre = 1.e-06_pm_reel ! epsilon de test du gradient de temperature pour le calcul de la pression

!   * constantes necessaires au calcul de la temperature entre  91 et 110 km
real(pm_reel), parameter ::  tc = 263.1905_pm_reel, gda = -76.3232_pm_reel, pta = -19.9429e3_pm_reel 

!   * composition de l'atmosphere en fonction des differents elements
!   * qui la composent ... N2,O,O2,Ar,He et H et en fonction de
!   * l'altitude a partir de 86 km

! le nombre de tableaux a initialiser depend de nmax, etant donne un nombre de cartes de continuations limite a 39 
! par le langage (ici on a pris 30 par commodite)

real(pm_reel), dimension (30*7), parameter  :: rntab01 = (/ &
86.0e3_pm_reel,1.130e20_pm_reel,8.600e16_pm_reel,3.031e19_pm_reel,1.351e18_pm_reel,7.582e14_pm_reel,0._pm_reel, &
86.5e3_pm_reel,1.034e20_pm_reel,9.939e16_pm_reel,2.772e19_pm_reel,1.236e18_pm_reel,6.976e14_pm_reel,0._pm_reel, &
87.0e3_pm_reel,9.456e19_pm_reel,1.147e17_pm_reel,2.535e19_pm_reel,1.130e18_pm_reel,6.422e14_pm_reel,0._pm_reel, &
87.5e3_pm_reel,8.651e19_pm_reel,1.320e17_pm_reel,2.319e19_pm_reel,1.033e18_pm_reel,5.915e14_pm_reel,0._pm_reel, &
88.0e3_pm_reel,7.915e19_pm_reel,1.513e17_pm_reel,2.120e19_pm_reel,9.437e17_pm_reel,5.453e14_pm_reel,0._pm_reel, &
88.5e3_pm_reel,7.242e19_pm_reel,1.724e17_pm_reel,1.938e19_pm_reel,8.624e17_pm_reel,5.031e14_pm_reel,0._pm_reel, &
89.0e3_pm_reel,6.626e19_pm_reel,1.952e17_pm_reel,1.772e19_pm_reel,7.880e17_pm_reel,4.647e14_pm_reel,0._pm_reel, &
89.5e3_pm_reel,6.062e19_pm_reel,2.193e17_pm_reel,1.619e19_pm_reel,7.198e17_pm_reel,4.296e14_pm_reel,0._pm_reel, &
90.0e3_pm_reel,5.547e19_pm_reel,2.443e17_pm_reel,1.479e19_pm_reel,6.574e17_pm_reel,3.976e14_pm_reel,0._pm_reel, &
90.5e3_pm_reel,5.075e19_pm_reel,2.699e17_pm_reel,1.351e19_pm_reel,6.002e17_pm_reel,3.685e14_pm_reel,0._pm_reel, &

91.0e3_pm_reel,4.643e19_pm_reel,2.953e17_pm_reel,1.234e19_pm_reel,5.478e17_pm_reel,3.419e14_pm_reel,0._pm_reel, &
91.5e3_pm_reel,4.248e19_pm_reel,3.200e17_pm_reel,1.126e19_pm_reel,4.998e17_pm_reel,3.177e14_pm_reel,0._pm_reel, &
92.0e3_pm_reel,3.886e19_pm_reel,3.434e17_pm_reel,1.027e19_pm_reel,4.557e17_pm_reel,2.956e14_pm_reel,0._pm_reel, &
92.5e3_pm_reel,3.553e19_pm_reel,3.651e17_pm_reel,9.361e18_pm_reel,4.152e17_pm_reel,2.753e14_pm_reel,0._pm_reel, &
93.0e3_pm_reel,3.249e19_pm_reel,3.846e17_pm_reel,8.527e18_pm_reel,3.781e17_pm_reel,2.568e14_pm_reel,0._pm_reel, &
93.5e3_pm_reel,2.970e19_pm_reel,4.016e17_pm_reel,7.761e18_pm_reel,3.441e17_pm_reel,2.399e14_pm_reel,0._pm_reel, &
94.0e3_pm_reel,2.715e19_pm_reel,4.159e17_pm_reel,7.060e18_pm_reel,3.129e17_pm_reel,2.244e14_pm_reel,0._pm_reel, &
94.5e3_pm_reel,2.481e19_pm_reel,4.275e17_pm_reel,6.418e18_pm_reel,2.844e17_pm_reel,2.103e14_pm_reel,0._pm_reel, &
95.0e3_pm_reel,2.268e19_pm_reel,4.365e17_pm_reel,5.830e18_pm_reel,2.583e17_pm_reel,1.973e14_pm_reel,0._pm_reel, &
95.5e3_pm_reel,2.072e19_pm_reel,4.429e17_pm_reel,5.293e18_pm_reel,2.345e17_pm_reel,1.854e14_pm_reel,0._pm_reel, &

96.0e3_pm_reel,1.894e19_pm_reel,4.471e17_pm_reel,4.801e18_pm_reel,2.127e17_pm_reel,1.745e14_pm_reel,0._pm_reel, &
96.5e3_pm_reel,1.730e19_pm_reel,4.493e17_pm_reel,4.353e18_pm_reel,1.928e17_pm_reel,1.645e14_pm_reel,0._pm_reel, &
97.0e3_pm_reel,1.581e19_pm_reel,4.500e17_pm_reel,3.943e18_pm_reel,1.746e17_pm_reel,1.553e14_pm_reel,0._pm_reel, &
97.5e3_pm_reel,1.445e19_pm_reel,4.494e17_pm_reel,3.570e18_pm_reel,1.581e17_pm_reel,1.468e14_pm_reel,0._pm_reel, &
98.0e3_pm_reel,1.320e19_pm_reel,4.476e17_pm_reel,3.230e18_pm_reel,1.430e17_pm_reel,1.390e14_pm_reel,0._pm_reel, &
98.5e3_pm_reel,1.206e19_pm_reel,4.447e17_pm_reel,2.920e18_pm_reel,1.292e17_pm_reel,1.317e14_pm_reel,0._pm_reel, &
99.0e3_pm_reel,1.102e19_pm_reel,4.408e17_pm_reel,2.639e18_pm_reel,1.167e17_pm_reel,1.251e14_pm_reel,0._pm_reel, &
99.5e3_pm_reel,1.008e19_pm_reel,4.358e17_pm_reel,2.383e18_pm_reel,1.053e17_pm_reel,1.190e14_pm_reel,0._pm_reel, &
100.0e3_pm_reel,9.210e18_pm_reel,4.298e17_pm_reel,2.151e18_pm_reel,9.501e16_pm_reel,1.133e14_pm_reel,0._pm_reel, &
101.0e3_pm_reel,7.740e18_pm_reel,4.168e17_pm_reel,1.756e18_pm_reel,7.735e16_pm_reel,1.034e14_pm_reel,0._pm_reel/)

real(pm_reel), dimension (30*7), parameter  :: rntab02 = (/ &
102.0e3_pm_reel,6.508e18_pm_reel,4.007e17_pm_reel,1.430e18_pm_reel,6.279e16_pm_reel,9.497e13_pm_reel,0._pm_reel, &
103.0e3_pm_reel,5.475e18_pm_reel,3.821e17_pm_reel,1.163e18_pm_reel,5.082e16_pm_reel,8.776e13_pm_reel,0._pm_reel, &
104.0e3_pm_reel,4.609e18_pm_reel,3.619e17_pm_reel,9.434e17_pm_reel,4.101e16_pm_reel,8.160e13_pm_reel,0._pm_reel, &
105.0e3_pm_reel,3.883e18_pm_reel,3.406e17_pm_reel,7.645e17_pm_reel,3.299e16_pm_reel,7.633e13_pm_reel,0._pm_reel, &
106.0e3_pm_reel,3.273e18_pm_reel,3.188e17_pm_reel,6.189e17_pm_reel,2.645e16_pm_reel,7.181e13_pm_reel,0._pm_reel, &
107.0e3_pm_reel,2.760e18_pm_reel,2.968e17_pm_reel,5.005e17_pm_reel,2.113e16_pm_reel,6.789e13_pm_reel,0._pm_reel, &
108.0e3_pm_reel,2.327e18_pm_reel,2.748e17_pm_reel,4.045e17_pm_reel,1.681e16_pm_reel,6.443e13_pm_reel,0._pm_reel, &
109.0e3_pm_reel,1.959e18_pm_reel,2.528e17_pm_reel,3.263e17_pm_reel,1.331e16_pm_reel,6.128e13_pm_reel,0._pm_reel, &
110.0e3_pm_reel,1.641e18_pm_reel,2.303e17_pm_reel,2.621e17_pm_reel,1.046e16_pm_reel,5.821e13_pm_reel,0._pm_reel, &
111.0e3_pm_reel,1.373e18_pm_reel,2.083e17_pm_reel,2.104e17_pm_reel,8.200e15_pm_reel,5.526e13_pm_reel,0._pm_reel, &

112.0e3_pm_reel,1.158e18_pm_reel,1.889e17_pm_reel,1.706e17_pm_reel,6.481e15_pm_reel,5.271e13_pm_reel,0._pm_reel, &
113.0e3_pm_reel,9.841e17_pm_reel,1.718e17_pm_reel,1.398e17_pm_reel,5.169e15_pm_reel,5.044e13_pm_reel,0._pm_reel, &
114.0e3_pm_reel,8.422e17_pm_reel,1.565e17_pm_reel,1.156e17_pm_reel,4.162e15_pm_reel,4.838e13_pm_reel,0._pm_reel, &
115.0e3_pm_reel,7.254e17_pm_reel,1.428e17_pm_reel,9.646e16_pm_reel,3.386e15_pm_reel,4.648e13_pm_reel,0._pm_reel, &
116.0e3_pm_reel,6.285e17_pm_reel,1.305e17_pm_reel,8.120e16_pm_reel,2.779e15_pm_reel,4.473e13_pm_reel,0._pm_reel, &
117.0e3_pm_reel,5.475e17_pm_reel,1.194e17_pm_reel,6.891e16_pm_reel,2.301e15_pm_reel,4.310e13_pm_reel,0._pm_reel, &
118.0e3_pm_reel,4.794e17_pm_reel,1.096e17_pm_reel,5.892e16_pm_reel,1.920e15_pm_reel,4.160e13_pm_reel,0._pm_reel, &
119.0e3_pm_reel,4.217e17_pm_reel,1.007e17_pm_reel,5.072e16_pm_reel,1.614e15_pm_reel,4.019e13_pm_reel,0._pm_reel, &
120.0e3_pm_reel,3.726e17_pm_reel,9.275e16_pm_reel,4.395e16_pm_reel,1.366e15_pm_reel,3.888e13_pm_reel,0._pm_reel, &
121.0e3_pm_reel,3.306e17_pm_reel,8.562e16_pm_reel,3.832e16_pm_reel,1.164e15_pm_reel,3.766e13_pm_reel,0._pm_reel, &

122.0e3_pm_reel,2.947e17_pm_reel,7.925e16_pm_reel,3.360e16_pm_reel,9.979e14_pm_reel,3.652e13_pm_reel,0._pm_reel, &
123.0e3_pm_reel,2.637e17_pm_reel,7.354e16_pm_reel,2.963e16_pm_reel,8.606e14_pm_reel,3.547e13_pm_reel,0._pm_reel, &
124.0e3_pm_reel,2.368e17_pm_reel,6.840e16_pm_reel,2.625e16_pm_reel,7.460e14_pm_reel,3.448e13_pm_reel,0._pm_reel, &
125.0e3_pm_reel,2.135e17_pm_reel,6.376e16_pm_reel,2.336e16_pm_reel,6.498e14_pm_reel,3.356e13_pm_reel,0._pm_reel, &
126.0e3_pm_reel,1.930e17_pm_reel,5.956e16_pm_reel,2.087e16_pm_reel,5.685e14_pm_reel,3.270e13_pm_reel,0._pm_reel, &
127.0e3_pm_reel,1.750e17_pm_reel,5.576e16_pm_reel,1.871e16_pm_reel,4.994e14_pm_reel,3.189e13_pm_reel,0._pm_reel, &
128.0e3_pm_reel,1.592e17_pm_reel,5.229e16_pm_reel,1.683e16_pm_reel,4.403e14_pm_reel,3.112e13_pm_reel,0._pm_reel, &
129.0e3_pm_reel,1.451e17_pm_reel,4.914e16_pm_reel,1.519e16_pm_reel,3.896e14_pm_reel,3.040e13_pm_reel,0._pm_reel, &
130.0e3_pm_reel,1.326e17_pm_reel,4.625e16_pm_reel,1.375e16_pm_reel,3.458e14_pm_reel,2.972e13_pm_reel,0._pm_reel, &
131.0e3_pm_reel,1.215e17_pm_reel,4.361e16_pm_reel,1.247e16_pm_reel,3.078e14_pm_reel,2.907e13_pm_reel,0._pm_reel/)

real(pm_reel), dimension (30*7), parameter  :: rntab03 = (/ &
132.0e3_pm_reel,1.116e17_pm_reel,4.118e16_pm_reel,1.134e16_pm_reel,2.748e14_pm_reel,2.846e13_pm_reel,0._pm_reel, &
133.0e3_pm_reel,1.026e17_pm_reel,3.894e16_pm_reel,1.034e16_pm_reel,2.460e14_pm_reel,2.787e13_pm_reel,0._pm_reel, &
134.0e3_pm_reel,9.460e16_pm_reel,3.688e16_pm_reel,9.444e15_pm_reel,2.207e14_pm_reel,2.732e13_pm_reel,0._pm_reel, &
135.0e3_pm_reel,8.735e16_pm_reel,3.497e16_pm_reel,8.645e15_pm_reel,1.985e14_pm_reel,2.679e13_pm_reel,0._pm_reel, &
136.0e3_pm_reel,8.080e16_pm_reel,3.320e16_pm_reel,7.927e15_pm_reel,1.789e14_pm_reel,2.629e13_pm_reel,0._pm_reel, &
137.0e3_pm_reel,7.487e16_pm_reel,3.156e16_pm_reel,7.283e15_pm_reel,1.616e14_pm_reel,2.581e13_pm_reel,0._pm_reel, &
138.0e3_pm_reel,6.947e16_pm_reel,3.004e16_pm_reel,6.702e15_pm_reel,1.463e14_pm_reel,2.535e13_pm_reel,0._pm_reel, &
139.0e3_pm_reel,6.456e16_pm_reel,2.862e16_pm_reel,6.177e15_pm_reel,1.326e14_pm_reel,2.491e13_pm_reel,0._pm_reel, &
140.0e3_pm_reel,6.009e16_pm_reel,2.729e16_pm_reel,5.702e15_pm_reel,1.205e14_pm_reel,2.449e13_pm_reel,0._pm_reel, &
141.0e3_pm_reel,5.600e16_pm_reel,2.605e16_pm_reel,5.272e15_pm_reel,1.096e14_pm_reel,2.408e13_pm_reel,0._pm_reel, &

142.0e3_pm_reel,5.225e16_pm_reel,2.489e16_pm_reel,4.881e15_pm_reel,9.989e13_pm_reel,2.369e13_pm_reel,0._pm_reel, &
143.0e3_pm_reel,4.881e16_pm_reel,2.380e16_pm_reel,4.524e15_pm_reel,9.118e13_pm_reel,2.332e13_pm_reel,0._pm_reel, &
144.0e3_pm_reel,4.565e16_pm_reel,2.278e16_pm_reel,4.199e15_pm_reel,8.335e13_pm_reel,2.296e13_pm_reel,0._pm_reel, &
145.0e3_pm_reel,4.275e16_pm_reel,2.183e16_pm_reel,3.903e15_pm_reel,7.630e13_pm_reel,2.261e13_pm_reel,0._pm_reel, &
146.0e3_pm_reel,4.007e16_pm_reel,2.092e16_pm_reel,3.631e15_pm_reel,6.994e13_pm_reel,2.228e13_pm_reel,0._pm_reel, &
147.0e3_pm_reel,3.760e16_pm_reel,2.007e16_pm_reel,3.382e15_pm_reel,6.420e13_pm_reel,2.196e13_pm_reel,0._pm_reel, &
148.0e3_pm_reel,3.531e16_pm_reel,1.927e16_pm_reel,3.153e15_pm_reel,5.900e13_pm_reel,2.165e13_pm_reel,0._pm_reel, &
149.0e3_pm_reel,3.320e16_pm_reel,1.852e16_pm_reel,2.943e15_pm_reel,5.428e13_pm_reel,2.135e13_pm_reel,0._pm_reel, &
150.e3_pm_reel,3.124e16_pm_reel,1.780e16_pm_reel,2.750e15_pm_reel,5.000e13_pm_reel,2.106e13_pm_reel,3.767e11_pm_reel, &
151.e3_pm_reel,2.942e16_pm_reel,1.712e16_pm_reel,2.572e15_pm_reel,4.611e13_pm_reel,2.078e13_pm_reel,3.659e11_pm_reel, &

152.e3_pm_reel,2.773e16_pm_reel,1.648e16_pm_reel,2.407e15_pm_reel,4.256e13_pm_reel,2.051e13_pm_reel,3.557e11_pm_reel, &
153.e3_pm_reel,2.616e16_pm_reel,1.587e16_pm_reel,2.255e15_pm_reel,3.933e13_pm_reel,2.024e13_pm_reel,3.461e11_pm_reel, &
154.e3_pm_reel,2.469e16_pm_reel,1.530e16_pm_reel,2.114e15_pm_reel,3.638e13_pm_reel,1.999e13_pm_reel,3.369e11_pm_reel, &
155.e3_pm_reel,2.333e16_pm_reel,1.475e16_pm_reel,1.984e15_pm_reel,3.368e13_pm_reel,1.974e13_pm_reel,3.283e11_pm_reel, &
156.e3_pm_reel,2.206e16_pm_reel,1.423e16_pm_reel,1.863e15_pm_reel,3.121e13_pm_reel,1.950e13_pm_reel,3.201e11_pm_reel, &
157.e3_pm_reel,2.087e16_pm_reel,1.373e16_pm_reel,1.751e15_pm_reel,2.895e13_pm_reel,1.927e13_pm_reel,3.123e11_pm_reel, &
158.e3_pm_reel,1.975e16_pm_reel,1.326e16_pm_reel,1.647e15_pm_reel,2.687e13_pm_reel,1.905e13_pm_reel,3.049e11_pm_reel, &
159.e3_pm_reel,1.871e16_pm_reel,1.281e16_pm_reel,1.550e15_pm_reel,2.496e13_pm_reel,1.883e13_pm_reel,2.978e11_pm_reel, &
160.e3_pm_reel,1.774e16_pm_reel,1.238e16_pm_reel,1.460e15_pm_reel,2.321e13_pm_reel,1.861e13_pm_reel,2.911e11_pm_reel, &
161.e3_pm_reel,1.682e16_pm_reel,1.197e16_pm_reel,1.376e15_pm_reel,2.159e13_pm_reel,1.841e13_pm_reel,2.847e11_pm_reel/)

real(pm_reel), dimension (30*7), parameter  :: rntab04 = (/ &
162.e3_pm_reel,1.596e16_pm_reel,1.158e16_pm_reel,1.297e15_pm_reel,2.011e13_pm_reel,1.820e13_pm_reel,2.786e11_pm_reel, &
163.e3_pm_reel,1.516e16_pm_reel,1.120e16_pm_reel,1.224e15_pm_reel,1.874e13_pm_reel,1.801e13_pm_reel,2.728e11_pm_reel, &
164.e3_pm_reel,1.440e16_pm_reel,1.085e16_pm_reel,1.156e15_pm_reel,1.747e13_pm_reel,1.782e13_pm_reel,2.672e11_pm_reel, &
165.e3_pm_reel,1.369e16_pm_reel,1.050e16_pm_reel,1.092e15_pm_reel,1.630e13_pm_reel,1.763e13_pm_reel,2.619e11_pm_reel, &
166.e3_pm_reel,1.302e16_pm_reel,1.018e16_pm_reel,1.032e15_pm_reel,1.522e13_pm_reel,1.745e13_pm_reel,2.568e11_pm_reel, &
167.e3_pm_reel,1.239e16_pm_reel,9.863e15_pm_reel,9.757e14_pm_reel,1.422e13_pm_reel,1.727e13_pm_reel,2.520e11_pm_reel, &
168.e3_pm_reel,1.179e16_pm_reel,9.562e15_pm_reel,9.232e14_pm_reel,1.329e13_pm_reel,1.710e13_pm_reel,2.473e11_pm_reel, &
169.e3_pm_reel,1.123e16_pm_reel,9.273e15_pm_reel,8.739e14_pm_reel,1.243e13_pm_reel,1.693e13_pm_reel,2.429e11_pm_reel, &
170.e3_pm_reel,1.070e16_pm_reel,8.996e15_pm_reel,8.277e14_pm_reel,1.163e13_pm_reel,1.676e13_pm_reel,2.386e11_pm_reel, &
171.e3_pm_reel,1.020e16_pm_reel,8.730e15_pm_reel,7.843e14_pm_reel,1.089e13_pm_reel,1.660e13_pm_reel,2.345e11_pm_reel, &

172.e3_pm_reel,9.724e15_pm_reel,8.474e15_pm_reel,7.435e14_pm_reel,1.020e13_pm_reel,1.644e13_pm_reel,2.306e11_pm_reel, &
173.e3_pm_reel,9.277e15_pm_reel,8.228e15_pm_reel,7.051e14_pm_reel,9.565e12_pm_reel,1.629e13_pm_reel,2.268e11_pm_reel, &
174.e3_pm_reel,8.853e15_pm_reel,7.992e15_pm_reel,6.690e14_pm_reel,8.970e12_pm_reel,1.614e13_pm_reel,2.232e11_pm_reel, &
175.e3_pm_reel,8.452e15_pm_reel,7.765e15_pm_reel,6.350e14_pm_reel,8.417e12_pm_reel,1.599e13_pm_reel,2.197e11_pm_reel, &
176.e3_pm_reel,8.072e15_pm_reel,7.546e15_pm_reel,6.030e14_pm_reel,7.901e12_pm_reel,1.585e13_pm_reel,2.163e11_pm_reel, &
177.e3_pm_reel,7.712e15_pm_reel,7.335e15_pm_reel,5.728e14_pm_reel,7.420e12_pm_reel,1.571e13_pm_reel,2.131e11_pm_reel, &
178.e3_pm_reel,7.371e15_pm_reel,7.132e15_pm_reel,5.443e14_pm_reel,6.971e12_pm_reel,1.557e13_pm_reel,2.100e11_pm_reel, &
179.e3_pm_reel,7.047e15_pm_reel,6.936e15_pm_reel,5.174e14_pm_reel,6.553e12_pm_reel,1.543e13_pm_reel,2.070e11_pm_reel, &
180.e3_pm_reel,6.740e15_pm_reel,6.747e15_pm_reel,4.921e14_pm_reel,6.162e12_pm_reel,1.530e13_pm_reel,2.041e11_pm_reel, &
181.e3_pm_reel,6.448e15_pm_reel,6.565e15_pm_reel,4.681e14_pm_reel,5.797e12_pm_reel,1.517e13_pm_reel,2.013e11_pm_reel, &

182.e3_pm_reel,6.170e15_pm_reel,6.389e15_pm_reel,4.455e14_pm_reel,5.456e12_pm_reel,1.504e13_pm_reel,1.987e11_pm_reel, &
183.e3_pm_reel,5.907e15_pm_reel,6.220e15_pm_reel,4.241e14_pm_reel,5.136e12_pm_reel,1.492e13_pm_reel,1.961e11_pm_reel, &
184.e3_pm_reel,5.656e15_pm_reel,6.056e15_pm_reel,4.039e14_pm_reel,4.838e12_pm_reel,1.479e13_pm_reel,1.936e11_pm_reel, &
185.e3_pm_reel,5.417e15_pm_reel,5.897e15_pm_reel,3.847e14_pm_reel,4.558e12_pm_reel,1.467e13_pm_reel,1.911e11_pm_reel, &
186.e3_pm_reel,5.190e15_pm_reel,5.744e15_pm_reel,3.666e14_pm_reel,4.296e12_pm_reel,1.456e13_pm_reel,1.888e11_pm_reel, &
187.e3_pm_reel,4.974e15_pm_reel,5.596e15_pm_reel,3.494e14_pm_reel,4.050e12_pm_reel,1.444e13_pm_reel,1.866e11_pm_reel, &
188.e3_pm_reel,4.768e15_pm_reel,5.453e15_pm_reel,3.331e14_pm_reel,3.820e12_pm_reel,1.433e13_pm_reel,1.844e11_pm_reel, &
189.e3_pm_reel,4.572e15_pm_reel,5.315e15_pm_reel,3.177e14_pm_reel,3.604e12_pm_reel,1.421e13_pm_reel,1.823e11_pm_reel, &
190.e3_pm_reel,4.385e15_pm_reel,5.181e15_pm_reel,3.031e14_pm_reel,3.401e12_pm_reel,1.410e13_pm_reel,1.802e11_pm_reel, &
191.e3_pm_reel,4.207e15_pm_reel,5.051e15_pm_reel,2.892e14_pm_reel,3.211e12_pm_reel,1.400e13_pm_reel,1.782e11_pm_reel/)

real(pm_reel), dimension (30*7), parameter  :: rntab05 = (/ &
192.e3_pm_reel,4.037e15_pm_reel,4.926e15_pm_reel,2.760e14_pm_reel,3.033e12_pm_reel,1.389e13_pm_reel,1.763e11_pm_reel, &
193.e3_pm_reel,3.875e15_pm_reel,4.804e15_pm_reel,2.635e14_pm_reel,2.865e12_pm_reel,1.379e13_pm_reel,1.745e11_pm_reel, &
194.e3_pm_reel,3.720e15_pm_reel,4.686e15_pm_reel,2.517e14_pm_reel,2.707e12_pm_reel,1.368e13_pm_reel,1.727e11_pm_reel, &
195.e3_pm_reel,3.572e15_pm_reel,4.572e15_pm_reel,2.404e14_pm_reel,2.558e12_pm_reel,1.358e13_pm_reel,1.709e11_pm_reel, &
196.e3_pm_reel,3.430e15_pm_reel,4.461e15_pm_reel,2.297e14_pm_reel,2.419e12_pm_reel,1.348e13_pm_reel,1.692e11_pm_reel, &
197.e3_pm_reel,3.295e15_pm_reel,4.354e15_pm_reel,2.195e14_pm_reel,2.288e12_pm_reel,1.339e13_pm_reel,1.676e11_pm_reel, &
198.e3_pm_reel,3.166e15_pm_reel,4.249e15_pm_reel,2.098e14_pm_reel,2.164e12_pm_reel,1.329e13_pm_reel,1.660e11_pm_reel, &
199.e3_pm_reel,3.043e15_pm_reel,4.148e15_pm_reel,2.006e14_pm_reel,2.047e12_pm_reel,1.319e13_pm_reel,1.645e11_pm_reel, &
200.e3_pm_reel,2.925e15_pm_reel,4.050e15_pm_reel,1.918e14_pm_reel,1.938e12_pm_reel,1.310e13_pm_reel,1.630e11_pm_reel, &
201.e3_pm_reel,2.812e15_pm_reel,3.955e15_pm_reel,1.834e14_pm_reel,1.834e12_pm_reel,1.301e13_pm_reel,1.615e11_pm_reel, &

202.e3_pm_reel,2.704e15_pm_reel,3.862e15_pm_reel,1.755e14_pm_reel,1.737e12_pm_reel,1.292e13_pm_reel,1.601e11_pm_reel, &
203.e3_pm_reel,2.601e15_pm_reel,3.773e15_pm_reel,1.679e14_pm_reel,1.645e12_pm_reel,1.283e13_pm_reel,1.587e11_pm_reel, &
204.e3_pm_reel,2.502e15_pm_reel,3.685e15_pm_reel,1.607e14_pm_reel,1.558e12_pm_reel,1.274e13_pm_reel,1.574e11_pm_reel, &
205.e3_pm_reel,2.407e15_pm_reel,3.600e15_pm_reel,1.538e14_pm_reel,1.477e12_pm_reel,1.266e13_pm_reel,1.561e11_pm_reel, &
206.e3_pm_reel,2.316e15_pm_reel,3.518e15_pm_reel,1.473e14_pm_reel,1.399e12_pm_reel,1.257e13_pm_reel,1.548e11_pm_reel, &
207.e3_pm_reel,2.229e15_pm_reel,3.438e15_pm_reel,1.410e14_pm_reel,1.327e12_pm_reel,1.249e13_pm_reel,1.536e11_pm_reel, &
208.e3_pm_reel,2.146e15_pm_reel,3.360e15_pm_reel,1.351e14_pm_reel,1.258e12_pm_reel,1.240e13_pm_reel,1.524e11_pm_reel, &
209.e3_pm_reel,2.066e15_pm_reel,3.284e15_pm_reel,1.294e14_pm_reel,1.193e12_pm_reel,1.232e13_pm_reel,1.512e11_pm_reel, &
210.e3_pm_reel,1.989e15_pm_reel,3.211e15_pm_reel,1.239e14_pm_reel,1.131e12_pm_reel,1.224e13_pm_reel,1.501e11_pm_reel, &
211.e3_pm_reel,1.915e15_pm_reel,3.139e15_pm_reel,1.188e14_pm_reel,1.073e12_pm_reel,1.216e13_pm_reel,1.490e11_pm_reel, &

212.e3_pm_reel,1.845e15_pm_reel,3.069e15_pm_reel,1.138e14_pm_reel,1.019e12_pm_reel,1.208e13_pm_reel,1.479e11_pm_reel, &
213.e3_pm_reel,1.777e15_pm_reel,3.001e15_pm_reel,1.091e14_pm_reel,9.666e11_pm_reel, 1.201e13_pm_reel,1.468e11_pm_reel, &
214.e3_pm_reel,1.712e15_pm_reel,2.935e15_pm_reel,1.046e14_pm_reel,9.176e11_pm_reel, 1.193e13_pm_reel,1.458e11_pm_reel, &
215.e3_pm_reel,1.650e15_pm_reel,2.871e15_pm_reel,1.003e14_pm_reel,8.711e11_pm_reel, 1.185e13_pm_reel,1.448e11_pm_reel, &
216.e3_pm_reel,1.590e15_pm_reel,2.808e15_pm_reel,9.617e13_pm_reel,8.272e11_pm_reel, 1.178e13_pm_reel,1.439e11_pm_reel, &
217.e3_pm_reel,1.533e15_pm_reel,2.747e15_pm_reel,9.224e13_pm_reel,7.856e11_pm_reel, 1.171e13_pm_reel,1.429e11_pm_reel, &
218.e3_pm_reel,1.477e15_pm_reel,2.688e15_pm_reel,8.848e13_pm_reel,7.463e11_pm_reel, 1.163e13_pm_reel,1.420e11_pm_reel, &
219.e3_pm_reel,1.424e15_pm_reel,2.630e15_pm_reel,8.489e13_pm_reel,7.090e11_pm_reel, 1.156e13_pm_reel,1.411e11_pm_reel, &
220.e3_pm_reel,1.373e15_pm_reel,2.573e15_pm_reel,8.145e13_pm_reel,6.737e11_pm_reel, 1.149e13_pm_reel,1.402e11_pm_reel, &
221.e3_pm_reel,1.324e15_pm_reel,2.518e15_pm_reel,7.816e13_pm_reel,6.402e11_pm_reel, 1.142e13_pm_reel,1.393e11_pm_reel/)

real(pm_reel), dimension (30*7), parameter  :: rntab06 = (/ &
222.e3_pm_reel,1.277e15_pm_reel,2.465e15_pm_reel,7.502e13_pm_reel,6.085e11_pm_reel, 1.135e13_pm_reel,1.385e11_pm_reel, &
223.e3_pm_reel,1.232e15_pm_reel,2.412e15_pm_reel,7.201e13_pm_reel,5.785e11_pm_reel, 1.128e13_pm_reel,1.377e11_pm_reel, &
224.e3_pm_reel,1.188e15_pm_reel,2.361e15_pm_reel,6.913e13_pm_reel,5.500e11_pm_reel, 1.122e13_pm_reel,1.369e11_pm_reel, &
225.e3_pm_reel,1.147e15_pm_reel,2.312e15_pm_reel,6.637e13_pm_reel,5.230e11_pm_reel, 1.115e13_pm_reel,1.361e11_pm_reel, &
226.e3_pm_reel,1.106e15_pm_reel,2.263e15_pm_reel,6.373e13_pm_reel,4.974e11_pm_reel, 1.108e13_pm_reel,1.353e11_pm_reel, &
227.e3_pm_reel,1.068e15_pm_reel,2.216e15_pm_reel,6.121e13_pm_reel,4.731e11_pm_reel, 1.102e13_pm_reel,1.345e11_pm_reel, &
228.e3_pm_reel,1.030e15_pm_reel,2.170e15_pm_reel,5.879e13_pm_reel,4.501e11_pm_reel, 1.095e13_pm_reel,1.338e11_pm_reel, &
229.e3_pm_reel,9.945e14_pm_reel,2.125e15_pm_reel,5.647e13_pm_reel,4.282e11_pm_reel, 1.089e13_pm_reel,1.331e11_pm_reel, &
230.e3_pm_reel,9.600e14_pm_reel,2.081e15_pm_reel,5.425e13_pm_reel,4.075e11_pm_reel, 1.083e13_pm_reel,1.324e11_pm_reel, &
231.e3_pm_reel,9.268e14_pm_reel,2.038e15_pm_reel,5.212e13_pm_reel,3.878e11_pm_reel, 1.076e13_pm_reel,1.317e11_pm_reel, &

232.e3_pm_reel,8.948e14_pm_reel,1.996e15_pm_reel,5.009e13_pm_reel,3.691e11_pm_reel, 1.070e13_pm_reel,1.310e11_pm_reel, &
233.e3_pm_reel,8.640e14_pm_reel,1.955e15_pm_reel,4.813e13_pm_reel,3.514e11_pm_reel, 1.064e13_pm_reel,1.304e11_pm_reel, &
234.e3_pm_reel,8.343e14_pm_reel,1.915e15_pm_reel,4.626e13_pm_reel,3.345e11_pm_reel, 1.058e13_pm_reel,1.297e11_pm_reel, &
235.e3_pm_reel,8.058e14_pm_reel,1.876e15_pm_reel,4.446e13_pm_reel,3.185e11_pm_reel, 1.052e13_pm_reel,1.291e11_pm_reel, &
236.e3_pm_reel,7.782e14_pm_reel,1.838e15_pm_reel,4.274e13_pm_reel,3.033e11_pm_reel, 1.046e13_pm_reel,1.285e11_pm_reel, &
237.e3_pm_reel,7.517e14_pm_reel,1.801e15_pm_reel,4.109e13_pm_reel,2.888e11_pm_reel, 1.040e13_pm_reel,1.279e11_pm_reel, &
238.e3_pm_reel,7.262e14_pm_reel,1.765e15_pm_reel,3.951e13_pm_reel,2.751e11_pm_reel, 1.034e13_pm_reel,1.273e11_pm_reel, &
239.e3_pm_reel,7.016e14_pm_reel,1.729e15_pm_reel,3.799e13_pm_reel,2.621e11_pm_reel, 1.029e13_pm_reel,1.267e11_pm_reel, &
240.e3_pm_reel,6.778e14_pm_reel,1.695e15_pm_reel,3.653e13_pm_reel,2.497e11_pm_reel, 1.023e13_pm_reel,1.261e11_pm_reel, &
241.e3_pm_reel,6.550e14_pm_reel,1.661e15_pm_reel,3.513e13_pm_reel,2.379e11_pm_reel, 1.017e13_pm_reel,1.256e11_pm_reel, &

242.e3_pm_reel,6.329e14_pm_reel,1.628e15_pm_reel,3.379e13_pm_reel,2.267e11_pm_reel, 1.012e13_pm_reel,1.250e11_pm_reel, &
243.e3_pm_reel,6.117e14_pm_reel,1.595e15_pm_reel,3.251e13_pm_reel,2.160e11_pm_reel, 1.006e13_pm_reel,1.245e11_pm_reel, &
244.e3_pm_reel,5.912e14_pm_reel,1.564e15_pm_reel,3.127e13_pm_reel,2.059e11_pm_reel, 1.001e13_pm_reel,1.240e11_pm_reel, &
245.e3_pm_reel,5.714e14_pm_reel,1.533e15_pm_reel,3.008e13_pm_reel,1.962e11_pm_reel, 9.953e12_pm_reel,1.234e11_pm_reel, &
246.e3_pm_reel,5.523e14_pm_reel,1.503e15_pm_reel,2.895e13_pm_reel,1.871e11_pm_reel, 9.899e12_pm_reel,1.229e11_pm_reel, &
247.e3_pm_reel,5.339e14_pm_reel,1.473e15_pm_reel,2.785e13_pm_reel,1.783e11_pm_reel, 9.846e12_pm_reel,1.224e11_pm_reel, &
248.e3_pm_reel,5.162e14_pm_reel,1.444e15_pm_reel,2.680e13_pm_reel,1.700e11_pm_reel, 9.794e12_pm_reel,1.219e11_pm_reel, &
249.e3_pm_reel,4.991e14_pm_reel,1.416e15_pm_reel,2.579e13_pm_reel,1.621e11_pm_reel, 9.741e12_pm_reel,1.215e11_pm_reel, &
250.e3_pm_reel,4.826e14_pm_reel,1.388e15_pm_reel,2.482e13_pm_reel,1.546e11_pm_reel, 9.690e12_pm_reel,1.210e11_pm_reel, &
251.e3_pm_reel,4.666e14_pm_reel,1.361e15_pm_reel,2.389e13_pm_reel,1.474e11_pm_reel, 9.638e12_pm_reel,1.205e11_pm_reel/)

real(pm_reel), dimension (30*7), parameter  :: rntab07 = (/ &
252.e3_pm_reel,4.512e14_pm_reel,1.335e15_pm_reel,2.300e13_pm_reel,1.406e11_pm_reel, 9.587e12_pm_reel,1.201e11_pm_reel, &
253.e3_pm_reel,4.364e14_pm_reel,1.309e15_pm_reel,2.214e13_pm_reel,1.341e11_pm_reel, 9.537e12_pm_reel,1.196e11_pm_reel, &
254.e3_pm_reel,4.221e14_pm_reel,1.284e15_pm_reel,2.132e13_pm_reel,1.280e11_pm_reel, 9.487e12_pm_reel,1.192e11_pm_reel, &
255.e3_pm_reel,4.082e14_pm_reel,1.259e15_pm_reel,2.052e13_pm_reel,1.221e11_pm_reel, 9.438e12_pm_reel,1.188e11_pm_reel, &
256.e3_pm_reel,3.949e14_pm_reel,1.235e15_pm_reel,1.976e13_pm_reel,1.165e11_pm_reel, 9.389e12_pm_reel,1.183e11_pm_reel, &
257.e3_pm_reel,3.820e14_pm_reel,1.211e15_pm_reel,1.903e13_pm_reel,1.111e11_pm_reel, 9.340e12_pm_reel,1.179e11_pm_reel, &
258.e3_pm_reel,3.695e14_pm_reel,1.188e15_pm_reel,1.832e13_pm_reel,1.060e11_pm_reel, 9.292e12_pm_reel,1.175e11_pm_reel, &
259.e3_pm_reel,3.575e14_pm_reel,1.165e15_pm_reel,1.765e13_pm_reel,1.012e11_pm_reel, 9.244e12_pm_reel,1.171e11_pm_reel, &
260.e3_pm_reel,3.459e14_pm_reel,1.143e15_pm_reel,1.700e13_pm_reel,9.658e10_pm_reel,9.196e12_pm_reel,1.167e11_pm_reel, &
261.e3_pm_reel,3.347e14_pm_reel,1.121e15_pm_reel,1.637e13_pm_reel,9.218e10_pm_reel,9.149e12_pm_reel,1.163e11_pm_reel, &

262.e3_pm_reel,3.238e14_pm_reel,1.100e15_pm_reel,1.577e13_pm_reel,8.799e10_pm_reel,9.103e12_pm_reel,1.159e11_pm_reel, &
263.e3_pm_reel,3.134e14_pm_reel,1.079e15_pm_reel,1.519e13_pm_reel,8.399e10_pm_reel,9.056e12_pm_reel,1.156e11_pm_reel, &
264.e3_pm_reel,3.033e14_pm_reel,1.059e15_pm_reel,1.463e13_pm_reel,8.019e10_pm_reel,9.010e12_pm_reel,1.152e11_pm_reel, &
265.e3_pm_reel,2.935e14_pm_reel,1.039e15_pm_reel,1.410e13_pm_reel,7.655e10_pm_reel,8.965e12_pm_reel,1.148e11_pm_reel, &
266.e3_pm_reel,2.841e14_pm_reel,1.019e15_pm_reel,1.358e13_pm_reel,7.309e10_pm_reel,8.920e12_pm_reel,1.145e11_pm_reel, &
267.e3_pm_reel,2.749e14_pm_reel,9.998e14_pm_reel,1.309e13_pm_reel,6.979e10_pm_reel,8.875e12_pm_reel,1.141e11_pm_reel, &
268.e3_pm_reel,2.661e14_pm_reel,9.811e14_pm_reel,1.261e13_pm_reel,6.665e10_pm_reel,8.830e12_pm_reel,1.138e11_pm_reel, &
269.e3_pm_reel,2.576e14_pm_reel,9.627e14_pm_reel,1.215e13_pm_reel,6.365e10_pm_reel,8.786e12_pm_reel,1.134e11_pm_reel, &
270.e3_pm_reel,2.494e14_pm_reel,9.447e14_pm_reel,1.171e13_pm_reel,6.078e10_pm_reel,8.743e12_pm_reel,1.131e11_pm_reel, &
271.e3_pm_reel,2.414e14_pm_reel,9.270e14_pm_reel,1.128e13_pm_reel,5.805e10_pm_reel,8.699e12_pm_reel,1.127e11_pm_reel, &

272.e3_pm_reel,2.337e14_pm_reel,9.097e14_pm_reel,1.088e13_pm_reel,5.545e10_pm_reel,8.656e12_pm_reel,1.124e11_pm_reel, &
273.e3_pm_reel,2.263e14_pm_reel,8.928e14_pm_reel,1.048e13_pm_reel,5.297e10_pm_reel,8.613e12_pm_reel,1.121e11_pm_reel, &
274.e3_pm_reel,2.191e14_pm_reel,8.762e14_pm_reel,1.010e13_pm_reel,5.060e10_pm_reel,8.571e12_pm_reel,1.118e11_pm_reel, &
275.e3_pm_reel,2.121e14_pm_reel,8.599e14_pm_reel,9.739e12_pm_reel,4.834e10_pm_reel,8.529e12_pm_reel,1.115e11_pm_reel, &
276.e3_pm_reel,2.054e14_pm_reel,8.440e14_pm_reel,9.388e12_pm_reel,4.618e10_pm_reel,8.487e12_pm_reel,1.112e11_pm_reel, &
277.e3_pm_reel,1.989e14_pm_reel,8.284e14_pm_reel,9.050e12_pm_reel,4.412e10_pm_reel,8.445e12_pm_reel,1.109e11_pm_reel, &
278.e3_pm_reel,1.926e14_pm_reel,8.131e14_pm_reel,8.725e12_pm_reel,4.216e10_pm_reel,8.404e12_pm_reel,1.106e11_pm_reel, &
279.e3_pm_reel,1.865e14_pm_reel,7.981e14_pm_reel,8.412e12_pm_reel,4.029e10_pm_reel,8.363e12_pm_reel,1.103e11_pm_reel, &
280.e3_pm_reel,1.806e14_pm_reel,7.834e14_pm_reel,8.110e12_pm_reel,3.850e10_pm_reel,8.322e12_pm_reel,1.100e11_pm_reel, &
281.e3_pm_reel,1.750e14_pm_reel,7.691e14_pm_reel,7.820e12_pm_reel,3.679e10_pm_reel,8.282e12_pm_reel,1.097e11_pm_reel/)

real(pm_reel), dimension (30*7), parameter  :: rntab08 = (/ &
282.e3_pm_reel,1.695e14_pm_reel,7.549e14_pm_reel,7.540e12_pm_reel,3.516e10_pm_reel,8.242e12_pm_reel,1.094e11_pm_reel, &
283.e3_pm_reel,1.641e14_pm_reel,7.411e14_pm_reel,7.271e12_pm_reel,3.360e10_pm_reel,8.202e12_pm_reel,1.091e11_pm_reel, &
284.e3_pm_reel,1.590e14_pm_reel,7.276e14_pm_reel,7.011e12_pm_reel,3.212e10_pm_reel,8.163e12_pm_reel,1.088e11_pm_reel, &
285.e3_pm_reel,1.540e14_pm_reel,7.143e14_pm_reel,6.761e12_pm_reel,3.070e10_pm_reel,8.124e12_pm_reel,1.086e11_pm_reel, &
286.e3_pm_reel,1.492e14_pm_reel,7.012e14_pm_reel,6.521e12_pm_reel,2.935e10_pm_reel,8.085e12_pm_reel,1.083e11_pm_reel, &
287.e3_pm_reel,1.445e14_pm_reel,6.885e14_pm_reel,6.289e12_pm_reel,2.805e10_pm_reel,8.046e12_pm_reel,1.080e11_pm_reel, &
288.e3_pm_reel,1.400e14_pm_reel,6.759e14_pm_reel,6.065e12_pm_reel,2.682e10_pm_reel,8.008e12_pm_reel,1.078e11_pm_reel, &
289.e3_pm_reel,1.356e14_pm_reel,6.637e14_pm_reel,5.850e12_pm_reel,2.564e10_pm_reel,7.969e12_pm_reel,1.075e11_pm_reel, &
290.e3_pm_reel,1.314e14_pm_reel,6.516e14_pm_reel,5.643e12_pm_reel,2.451e10_pm_reel,7.931e12_pm_reel,1.073e11_pm_reel, &
291.e3_pm_reel,1.273e14_pm_reel,6.398e14_pm_reel,5.443e12_pm_reel,2.344e10_pm_reel,7.894e12_pm_reel,1.070e11_pm_reel, &

292.e3_pm_reel,1.234e14_pm_reel,6.282e14_pm_reel,5.251e12_pm_reel,2.241e10_pm_reel,7.856e12_pm_reel,1.067e11_pm_reel, &
293.e3_pm_reel,1.195e14_pm_reel,6.169e14_pm_reel,5.065e12_pm_reel,2.143e10_pm_reel,7.819e12_pm_reel,1.065e11_pm_reel, &
294.e3_pm_reel,1.158e14_pm_reel,6.058e14_pm_reel,4.886e12_pm_reel,2.049e10_pm_reel,7.782e12_pm_reel,1.063e11_pm_reel, &
295.e3_pm_reel,1.122e14_pm_reel,5.948e14_pm_reel,4.714e12_pm_reel,1.960e10_pm_reel,7.746e12_pm_reel,1.060e11_pm_reel, &
296.e3_pm_reel,1.088e14_pm_reel,5.841e14_pm_reel,4.548e12_pm_reel,1.874e10_pm_reel,7.709e12_pm_reel,1.058e11_pm_reel, &
297.e3_pm_reel,1.054e14_pm_reel,5.736e14_pm_reel,4.388e12_pm_reel,1.792e10_pm_reel,7.673e12_pm_reel,1.055e11_pm_reel, &
298.e3_pm_reel,1.021e14_pm_reel,5.633e14_pm_reel,4.234e12_pm_reel,1.714e10_pm_reel,7.637e12_pm_reel,1.053e11_pm_reel, &
299.e3_pm_reel,9.898e13_pm_reel,5.532e14_pm_reel,4.085e12_pm_reel,1.639e10_pm_reel,7.602e12_pm_reel,1.051e11_pm_reel, &
300.e3_pm_reel,9.593e13_pm_reel,5.433e14_pm_reel,3.942e12_pm_reel,1.568e10_pm_reel,7.566e12_pm_reel,1.049e11_pm_reel, &
302.e3_pm_reel,9.011e13_pm_reel,5.241e14_pm_reel,3.670e12_pm_reel,1.435e10_pm_reel,7.496e12_pm_reel,1.044e11_pm_reel, &

304.e3_pm_reel,8.466e13_pm_reel,5.055e14_pm_reel,3.418e12_pm_reel,1.313e10_pm_reel,7.427e12_pm_reel,1.040e11_pm_reel, &
306.e3_pm_reel,7.954e13_pm_reel,4.877e14_pm_reel,3.184e12_pm_reel,1.202e10_pm_reel,7.358e12_pm_reel,1.035e11_pm_reel, &
308.e3_pm_reel,7.474e13_pm_reel,4.705e14_pm_reel,2.966e12_pm_reel,1.100e10_pm_reel,7.290e12_pm_reel,1.031e11_pm_reel, &
310.e3_pm_reel,7.024e13_pm_reel,4.540e14_pm_reel,2.763e12_pm_reel,1.007e10_pm_reel,7.224e12_pm_reel,1.027e11_pm_reel, &
312.e3_pm_reel,6.602e13_pm_reel,4.380e14_pm_reel,2.574e12_pm_reel,9.223e09_pm_reel,7.157e12_pm_reel,1.023e11_pm_reel, &
314.e3_pm_reel,6.206e13_pm_reel,4.227e14_pm_reel,2.399e12_pm_reel,8.447e09_pm_reel,7.092e12_pm_reel,1.019e11_pm_reel, &
316.e3_pm_reel,5.834e13_pm_reel,4.079e14_pm_reel,2.236e12_pm_reel,7.737e09_pm_reel,7.028e12_pm_reel,1.015e11_pm_reel, &
318.e3_pm_reel,5.485e13_pm_reel,3.937e14_pm_reel,2.084e12_pm_reel,7.087e09_pm_reel,6.964e12_pm_reel,1.012e11_pm_reel, &
320.e3_pm_reel,5.158e13_pm_reel,3.800e14_pm_reel,1.942e12_pm_reel,6.493e09_pm_reel,6.901e12_pm_reel,1.008e11_pm_reel, &
322.e3_pm_reel,4.850e13_pm_reel,3.668e14_pm_reel,1.811e12_pm_reel,5.950e09_pm_reel,6.839e12_pm_reel,1.004e11_pm_reel/)

real(pm_reel), dimension (30*7), parameter  :: rntab09 = (/ &
324.e3_pm_reel,4.561e13_pm_reel,3.541e14_pm_reel,1.688e12_pm_reel,5.452e09_pm_reel,6.777e12_pm_reel,1.001e11_pm_reel, &
326.e3_pm_reel,4.290e13_pm_reel,3.418e14_pm_reel,1.574e12_pm_reel,4.997e09_pm_reel,6.717e12_pm_reel,9.971e10_pm_reel, &
328.e3_pm_reel,4.035e13_pm_reel,3.300e14_pm_reel,1.468e12_pm_reel,4.580e09_pm_reel,6.657e12_pm_reel,9.937e10_pm_reel, &
330.e3_pm_reel,3.796e13_pm_reel,3.186e14_pm_reel,1.369e12_pm_reel,4.199e09_pm_reel,6.597e12_pm_reel,9.903e10_pm_reel, &
332.e3_pm_reel,3.571e13_pm_reel,3.076e14_pm_reel,1.277e12_pm_reel,3.850e09_pm_reel,6.538e12_pm_reel,9.869e10_pm_reel, &
334.e3_pm_reel,3.360e13_pm_reel,2.970e14_pm_reel,1.191e12_pm_reel,3.530e09_pm_reel,6.480e12_pm_reel,9.836e10_pm_reel, &
336.e3_pm_reel,3.162e13_pm_reel,2.868e14_pm_reel,1.111e12_pm_reel,3.237e09_pm_reel,6.423e12_pm_reel,9.804e10_pm_reel, &
338.e3_pm_reel,2.975e13_pm_reel,2.770e14_pm_reel,1.037e12_pm_reel,2.969e09_pm_reel,6.366e12_pm_reel,9.772e10_pm_reel, &
340.e3_pm_reel,2.800e13_pm_reel,2.675e14_pm_reel,9.674e11_pm_reel, 2.723e09_pm_reel,6.310e12_pm_reel,9.741e10_pm_reel, &
342.e3_pm_reel,2.635e13_pm_reel,2.583e14_pm_reel,9.027e11_pm_reel, 2.498e09_pm_reel,6.254e12_pm_reel,9.710e10_pm_reel, &

344.e3_pm_reel,2.480e13_pm_reel,2.495e14_pm_reel,8.424e11_pm_reel, 2.292e09_pm_reel,6.199e12_pm_reel,9.680e10_pm_reel, &
346.e3_pm_reel,2.335e13_pm_reel,2.410e14_pm_reel,7.862e11_pm_reel, 2.103e09_pm_reel,6.145e12_pm_reel,9.650e10_pm_reel, &
348.e3_pm_reel,2.198e13_pm_reel,2.328e14_pm_reel,7.338e11_pm_reel, 1.929e09_pm_reel,6.091e12_pm_reel,9.620e10_pm_reel, &
350.e3_pm_reel,2.069e13_pm_reel,2.249e14_pm_reel,6.850e11_pm_reel, 1.771e09_pm_reel,6.038e12_pm_reel,9.591e10_pm_reel, &
352.e3_pm_reel,1.948e13_pm_reel,2.172e14_pm_reel,6.394e11_pm_reel, 1.625e09_pm_reel,5.985e12_pm_reel,9.562e10_pm_reel, &
354.e3_pm_reel,1.834e13_pm_reel,2.099e14_pm_reel,5.969e11_pm_reel, 1.491e09_pm_reel,5.933e12_pm_reel,9.534e10_pm_reel, &
356.e3_pm_reel,1.727e13_pm_reel,2.027e14_pm_reel,5.573e11_pm_reel, 1.369e09_pm_reel,5.881e12_pm_reel,9.505e10_pm_reel, &
358.e3_pm_reel,1.627e13_pm_reel,1.959e14_pm_reel,5.204e11_pm_reel, 1.257e09_pm_reel,5.830e12_pm_reel,9.478e10_pm_reel, &
360.e3_pm_reel,1.532e13_pm_reel,1.893e14_pm_reel,4.859e11_pm_reel, 1.154e09_pm_reel,5.779e12_pm_reel,9.450e10_pm_reel, &
362.e3_pm_reel,1.443e13_pm_reel,1.829e14_pm_reel,4.538e11_pm_reel, 1.059e09_pm_reel,5.729e12_pm_reel,9.423e10_pm_reel, &

364.e3_pm_reel,1.359e13_pm_reel,1.767e14_pm_reel,4.238e11_pm_reel, 9.728e08_pm_reel,5.680e12_pm_reel,9.397e10_pm_reel, &
366.e3_pm_reel,1.280e13_pm_reel,1.707e14_pm_reel,3.958e11_pm_reel, 8.934e08_pm_reel,5.631e12_pm_reel,9.370e10_pm_reel, &
368.e3_pm_reel,1.206e13_pm_reel,1.650e14_pm_reel,3.697e11_pm_reel, 8.205e08_pm_reel,5.582e12_pm_reel,9.344e10_pm_reel, &
370.e3_pm_reel,1.136e13_pm_reel,1.594e14_pm_reel,3.454e11_pm_reel, 7.536e08_pm_reel,5.534e12_pm_reel,9.318e10_pm_reel, &
372.e3_pm_reel,1.070e13_pm_reel,1.541e14_pm_reel,3.226e11_pm_reel, 6.922e08_pm_reel,5.487e12_pm_reel,9.293e10_pm_reel, &
374.e3_pm_reel,1.008e13_pm_reel,1.489e14_pm_reel,3.014e11_pm_reel, 6.359e08_pm_reel,5.439e12_pm_reel,9.268e10_pm_reel, &
376.e3_pm_reel,9.498e12_pm_reel,1.439e14_pm_reel,2.816e11_pm_reel, 5.842e08_pm_reel,5.393e12_pm_reel,9.243e10_pm_reel, &
378.e3_pm_reel,8.950e12_pm_reel,1.391e14_pm_reel,2.631e11_pm_reel, 5.367e08_pm_reel,5.347e12_pm_reel,9.218e10_pm_reel, &
380.e3_pm_reel,8.434e12_pm_reel,1.344e14_pm_reel,2.459e11_pm_reel, 4.932e08_pm_reel,5.301e12_pm_reel,9.193e10_pm_reel, &
382.e3_pm_reel,7.948e12_pm_reel,1.300e14_pm_reel,2.297e11_pm_reel, 4.532e08_pm_reel,5.256e12_pm_reel,9.169e10_pm_reel/)

real(pm_reel), dimension (30*7), parameter :: rntab10 = (/ &
384.e3_pm_reel,7.490e12_pm_reel,1.256e14_pm_reel,2.147e11_pm_reel, 4.165e08_pm_reel,5.211e12_pm_reel,9.145e10_pm_reel, &
386.e3_pm_reel,7.059e12_pm_reel,1.214e14_pm_reel,2.006e11_pm_reel, 3.827e08_pm_reel,5.167e12_pm_reel,9.121e10_pm_reel, &
388.e3_pm_reel,6.653e12_pm_reel,1.174e14_pm_reel,1.875e11_pm_reel, 3.518e08_pm_reel,5.123e12_pm_reel,9.098e10_pm_reel, &
390.e3_pm_reel,6.271e12_pm_reel,1.135e14_pm_reel,1.753e11_pm_reel, 3.234e08_pm_reel,5.079e12_pm_reel,9.074e10_pm_reel, &
392.e3_pm_reel,5.911e12_pm_reel,1.097e14_pm_reel,1.638e11_pm_reel, 2.972e08_pm_reel,5.036e12_pm_reel,9.051e10_pm_reel, &
394.e3_pm_reel,5.572e12_pm_reel,1.061e14_pm_reel,1.532e11_pm_reel, 2.733e08_pm_reel,4.993e12_pm_reel,9.028e10_pm_reel, &
396.e3_pm_reel,5.253e12_pm_reel,1.025e14_pm_reel,1.432e11_pm_reel, 2.512e08_pm_reel,4.951e12_pm_reel,9.005e10_pm_reel, &
398.e3_pm_reel,4.952e12_pm_reel,9.913e13_pm_reel,1.339e11_pm_reel, 2.310e08_pm_reel,4.909e12_pm_reel,8.983e10_pm_reel, &
400.e3_pm_reel,4.669e12_pm_reel,9.584e13_pm_reel,1.252e11_pm_reel, 2.124e08_pm_reel,4.868e12_pm_reel,8.960e10_pm_reel, &
402.e3_pm_reel,4.402e12_pm_reel,9.267e13_pm_reel,1.170e11_pm_reel, 1.953e08_pm_reel,4.827e12_pm_reel,8.938e10_pm_reel, &

404.e3_pm_reel,4.151e12_pm_reel,8.960e13_pm_reel,1.094e11_pm_reel, 1.796e08_pm_reel,4.786e12_pm_reel,8.916e10_pm_reel, &
406.e3_pm_reel,3.914e12_pm_reel,8.664e13_pm_reel,1.023e11_pm_reel, 1.652e08_pm_reel,4.746e12_pm_reel,8.894e10_pm_reel, &
408.e3_pm_reel,3.691e12_pm_reel,8.378e13_pm_reel,9.568e10_pm_reel, 1.519e08_pm_reel,4.706e12_pm_reel,8.872e10_pm_reel, &
410.e3_pm_reel,3.480e12_pm_reel,8.101e13_pm_reel,8.948e10_pm_reel, 1.397e08_pm_reel,4.666e12_pm_reel,8.851e10_pm_reel, &
412.e3_pm_reel,3.282e12_pm_reel,7.834e13_pm_reel,8.369e10_pm_reel, 1.285e08_pm_reel,4.627e12_pm_reel,8.829e10_pm_reel, &
414.e3_pm_reel,3.095e12_pm_reel,7.576e13_pm_reel,7.827e10_pm_reel, 1.182e08_pm_reel,4.588e12_pm_reel,8.808e10_pm_reel, &
416.e3_pm_reel,2.919e12_pm_reel,7.327e13_pm_reel,7.321e10_pm_reel, 1.088e08_pm_reel,4.550e12_pm_reel,8.787e10_pm_reel, &
418.e3_pm_reel,2.754e12_pm_reel,7.086e13_pm_reel,6.848e10_pm_reel, 1.001e08_pm_reel,4.512e12_pm_reel,8.766e10_pm_reel, &
420.e3_pm_reel,2.597e12_pm_reel,6.853e13_pm_reel,6.406e10_pm_reel, 9.207e07_pm_reel,4.474e12_pm_reel,8.745e10_pm_reel, &
422.e3_pm_reel,2.450e12_pm_reel,6.628e13_pm_reel,5.993e10_pm_reel, 8.472e07_pm_reel,4.437e12_pm_reel,8.725e10_pm_reel, &

424.e3_pm_reel,2.311e12_pm_reel,6.410e13_pm_reel,5.606e10_pm_reel, 7.796e07_pm_reel,4.399e12_pm_reel,8.704e10_pm_reel, &
426.e3_pm_reel,2.180e12_pm_reel,6.200e13_pm_reel,5.245e10_pm_reel, 7.174e07_pm_reel,4.363e12_pm_reel,8.684e10_pm_reel, &
428.e3_pm_reel,2.057e12_pm_reel,5.997e13_pm_reel,4.907e10_pm_reel, 6.602e07_pm_reel,4.326e12_pm_reel,8.663e10_pm_reel, &
430.e3_pm_reel,1.940e12_pm_reel,5.800e13_pm_reel,4.592e10_pm_reel, 6.076e07_pm_reel,4.290e12_pm_reel,8.643e10_pm_reel, &
432.e3_pm_reel,1.831e12_pm_reel,5.611e13_pm_reel,4.297e10_pm_reel, 5.593e07_pm_reel,4.255e12_pm_reel,8.623e10_pm_reel, &
434.e3_pm_reel,1.727e12_pm_reel,5.427e13_pm_reel,4.020e10_pm_reel, 5.148e07_pm_reel,4.219e12_pm_reel,8.603e10_pm_reel, &
436.e3_pm_reel,1.630e12_pm_reel,5.250e13_pm_reel,3.762e10_pm_reel, 4.739e07_pm_reel,4.184e12_pm_reel,8.583e10_pm_reel, &
438.e3_pm_reel,1.538e12_pm_reel,5.079e13_pm_reel,3.521e10_pm_reel, 4.362e07_pm_reel,4.150e12_pm_reel,8.564e10_pm_reel, &
440.e3_pm_reel,1.451e12_pm_reel,4.913e13_pm_reel,3.295e10_pm_reel, 4.016e07_pm_reel,4.115e12_pm_reel,8.544e10_pm_reel, &
442.e3_pm_reel,1.369e12_pm_reel,4.753e13_pm_reel,3.084e10_pm_reel, 3.698e07_pm_reel,4.081e12_pm_reel,8.525e10_pm_reel/)

real(pm_reel), dimension (30*7), parameter  :: rntab11 = (/ &
444.e3_pm_reel,1.292e12_pm_reel,4.598e13_pm_reel,2.887e10_pm_reel, 3.404e07_pm_reel,4.047e12_pm_reel,8.505e10_pm_reel, &
446.e3_pm_reel,1.220e12_pm_reel,4.448e13_pm_reel,2.702e10_pm_reel, 3.135e07_pm_reel,4.014e12_pm_reel,8.486e10_pm_reel, &
448.e3_pm_reel,1.151e12_pm_reel,4.303e13_pm_reel,2.529e10_pm_reel, 2.887e07_pm_reel,3.981e12_pm_reel,8.467e10_pm_reel, &
450.e3_pm_reel,1.086e12_pm_reel,4.164e13_pm_reel,2.368e10_pm_reel, 2.658e07_pm_reel,3.948e12_pm_reel,8.448e10_pm_reel, &
452.e3_pm_reel,1.025e12_pm_reel,4.028e13_pm_reel,2.216e10_pm_reel, 2.448e07_pm_reel,3.915e12_pm_reel,8.429e10_pm_reel, &
454.e3_pm_reel,9.679e11_pm_reel, 3.898e13_pm_reel,2.075e10_pm_reel, 2.255e07_pm_reel,3.883e12_pm_reel,8.410e10_pm_reel, &
456.e3_pm_reel,9.136e11_pm_reel, 3.771e13_pm_reel,1.943e10_pm_reel, 2.077e07_pm_reel,3.851e12_pm_reel,8.391e10_pm_reel, &
458.e3_pm_reel,8.625e11_pm_reel, 3.649e13_pm_reel,1.819e10_pm_reel, 1.913e07_pm_reel,3.819e12_pm_reel,8.373e10_pm_reel, &
460.e3_pm_reel,8.142e11_pm_reel, 3.531e13_pm_reel,1.703e10_pm_reel, 1.762e07_pm_reel,3.788e12_pm_reel,8.354e10_pm_reel, &
462.e3_pm_reel,7.686e11_pm_reel, 3.416e13_pm_reel,1.595e10_pm_reel, 1.623e07_pm_reel,3.757e12_pm_reel,8.336e10_pm_reel, &

464.e3_pm_reel,7.256e11_pm_reel, 3.306e13_pm_reel,1.493e10_pm_reel, 1.495e07_pm_reel,3.726e12_pm_reel,8.317e10_pm_reel, &
466.e3_pm_reel,6.851e11_pm_reel, 3.199e13_pm_reel,1.398e10_pm_reel, 1.377e07_pm_reel,3.695e12_pm_reel,8.299e10_pm_reel, &
468.e3_pm_reel,6.468e11_pm_reel, 3.096e13_pm_reel,1.309e10_pm_reel, 1.269e07_pm_reel,3.665e12_pm_reel,8.281e10_pm_reel, &
470.e3_pm_reel,6.107e11_pm_reel, 2.996e13_pm_reel,1.226e10_pm_reel, 1.169e07_pm_reel,3.635e12_pm_reel,8.263e10_pm_reel, &
472.e3_pm_reel,5.766e11_pm_reel, 2.899e13_pm_reel,1.148e10_pm_reel, 1.077e07_pm_reel,3.605e12_pm_reel,8.245e10_pm_reel, &
474.e3_pm_reel,5.445e11_pm_reel, 2.806e13_pm_reel,1.076e10_pm_reel, 9.929e06_pm_reel,3.576e12_pm_reel,8.227e10_pm_reel, &
476.e3_pm_reel,5.142e11_pm_reel, 2.715e13_pm_reel,1.007e10_pm_reel, 9.149e06_pm_reel,3.547e12_pm_reel,8.209e10_pm_reel, &
478.e3_pm_reel,4.855e11_pm_reel, 2.628e13_pm_reel,9.436e09_pm_reel,8.432e06_pm_reel,3.518e12_pm_reel,8.191e10_pm_reel, &
480.e3_pm_reel,4.585e11_pm_reel, 2.543e13_pm_reel,8.839e09_pm_reel,7.771e06_pm_reel,3.489e12_pm_reel,8.173e10_pm_reel, &
482.e3_pm_reel,4.330e11_pm_reel, 2.461e13_pm_reel,8.280e09_pm_reel,7.162e06_pm_reel,3.461e12_pm_reel,8.155e10_pm_reel, &

484.e3_pm_reel,4.090e11_pm_reel, 2.382e13_pm_reel,7.757e09_pm_reel,6.602e06_pm_reel,3.432e12_pm_reel,8.138e10_pm_reel, &
486.e3_pm_reel,3.863e11_pm_reel, 2.306e13_pm_reel,7.267e09_pm_reel,6.085e06_pm_reel,3.404e12_pm_reel,8.120e10_pm_reel, &
488.e3_pm_reel,3.648e11_pm_reel, 2.232e13_pm_reel,6.808e09_pm_reel,5.609e06_pm_reel,3.377e12_pm_reel,8.103e10_pm_reel, &
490.e3_pm_reel,3.446e11_pm_reel, 2.160e13_pm_reel,6.378e09_pm_reel,5.171e06_pm_reel,3.349e12_pm_reel,8.085e10_pm_reel, &
492.e3_pm_reel,3.255e11_pm_reel, 2.091e13_pm_reel,5.976e09_pm_reel,4.767e06_pm_reel,3.322e12_pm_reel,8.068e10_pm_reel, &
494.e3_pm_reel,3.075e11_pm_reel, 2.024e13_pm_reel,5.599e09_pm_reel,4.395e06_pm_reel,3.295e12_pm_reel,8.051e10_pm_reel, &
496.e3_pm_reel,2.904e11_pm_reel, 1.959e13_pm_reel,5.247e09_pm_reel,4.052e06_pm_reel,3.268e12_pm_reel,8.034e10_pm_reel, &
498.e3_pm_reel,2.744e11_pm_reel, 1.896e13_pm_reel,4.917e09_pm_reel,3.737e06_pm_reel,3.242e12_pm_reel,8.017e10_pm_reel, &
500.e3_pm_reel,2.592e11_pm_reel, 1.836e13_pm_reel,4.607e09_pm_reel,3.445e06_pm_reel,3.215e12_pm_reel,8.000e10_pm_reel, &
505.e3_pm_reel,2.249e11_pm_reel, 1.693e13_pm_reel,3.917e09_pm_reel,2.814e06_pm_reel,3.151e12_pm_reel,7.959e10_pm_reel/)

real(pm_reel), dimension (30*7), parameter  :: rntab12 = (/ &
510.e3_pm_reel,1.951e11_pm_reel, 1.561e13_pm_reel,3.331e09_pm_reel,2.299e06_pm_reel,3.087e12_pm_reel,7.918e10_pm_reel, &
515.e3_pm_reel,1.694e11_pm_reel, 1.440e13_pm_reel,2.834e09_pm_reel,1.878e06_pm_reel,3.026e12_pm_reel,7.878e10_pm_reel, &
520.e3_pm_reel,1.470e11_pm_reel, 1.328e13_pm_reel,2.411e09_pm_reel,1.535e06_pm_reel,2.965e12_pm_reel,7.838e10_pm_reel, &
525.e3_pm_reel,1.277e11_pm_reel, 1.225e13_pm_reel,2.052e09_pm_reel,1.255e06_pm_reel,2.906e12_pm_reel,7.798e10_pm_reel, &
530.e3_pm_reel,1.109e11_pm_reel, 1.130e13_pm_reel,1.747e09_pm_reel,1.027e06_pm_reel,2.848e12_pm_reel,7.758e10_pm_reel, &
535.e3_pm_reel,9.633e10_pm_reel, 1.043e13_pm_reel,1.487e09_pm_reel,8.400e05_pm_reel,2.791e12_pm_reel,7.719e10_pm_reel, &
540.e3_pm_reel,8.370e10_pm_reel, 9.624e12_pm_reel,1.267e09_pm_reel,6.875e05_pm_reel,2.735e12_pm_reel,7.680e10_pm_reel, &
545.e3_pm_reel,7.274e10_pm_reel, 8.883e12_pm_reel,1.079e09_pm_reel,5.628e05_pm_reel,2.681e12_pm_reel,7.641e10_pm_reel, &
550.e3_pm_reel,6.323e10_pm_reel, 8.200e12_pm_reel,9.196e08_pm_reel,4.609e05_pm_reel,2.628e12_pm_reel,7.602e10_pm_reel, &
555.e3_pm_reel,5.497e10_pm_reel, 7.570e12_pm_reel,7.838e08_pm_reel,3.775e05_pm_reel,2.576e12_pm_reel,7.564e10_pm_reel, &

560.e3_pm_reel,4.781e10_pm_reel, 6.989e12_pm_reel,6.682e08_pm_reel,3.093e05_pm_reel,2.525e12_pm_reel,7.526e10_pm_reel, &
565.e3_pm_reel,4.158e10_pm_reel, 6.454e12_pm_reel,5.697e08_pm_reel,2.535e05_pm_reel,2.475e12_pm_reel,7.488e10_pm_reel, &
570.e3_pm_reel,3.617e10_pm_reel, 5.960e12_pm_reel,4.859e08_pm_reel,2.079e05_pm_reel,2.426e12_pm_reel,7.451e10_pm_reel, &
575.e3_pm_reel,3.148e10_pm_reel, 5.505e12_pm_reel,4.146e08_pm_reel,1.705e05_pm_reel,2.379e12_pm_reel,7.413e10_pm_reel, &
580.e3_pm_reel,2.740e10_pm_reel, 5.085e12_pm_reel,3.537e08_pm_reel,1.398e05_pm_reel,2.332e12_pm_reel,7.376e10_pm_reel, &
585.e3_pm_reel,2.385e10_pm_reel, 4.698e12_pm_reel,3.019e08_pm_reel,1.147e05_pm_reel,2.286e12_pm_reel,7.339e10_pm_reel, &
590.e3_pm_reel,2.076e10_pm_reel, 4.341e12_pm_reel,2.578e08_pm_reel,9.419e04_pm_reel,2.241e12_pm_reel,7.303e10_pm_reel, &
595.e3_pm_reel,1.808e10_pm_reel, 4.011e12_pm_reel,2.201e08_pm_reel,7.733e04_pm_reel,2.197e12_pm_reel,7.267e10_pm_reel, &
600.e3_pm_reel,1.575e10_pm_reel, 3.707e12_pm_reel,1.880e08_pm_reel,6.351e04_pm_reel,2.154e12_pm_reel,7.231e10_pm_reel, &
605.e3_pm_reel,1.372e10_pm_reel, 3.426e12_pm_reel,1.606e08_pm_reel,5.217e04_pm_reel,2.112e12_pm_reel,7.195e10_pm_reel, &

610.e3_pm_reel,1.196e10_pm_reel, 3.167e12_pm_reel,1.372e08_pm_reel,4.287e04_pm_reel,2.071e12_pm_reel,7.159e10_pm_reel, &
615.e3_pm_reel,1.042e10_pm_reel, 2.928e12_pm_reel,1.173e08_pm_reel,3.524e04_pm_reel,2.031e12_pm_reel,7.124e10_pm_reel, &
620.e3_pm_reel,9.085e09_pm_reel,2.707e12_pm_reel,1.003e08_pm_reel,2.898e04_pm_reel,1.991e12_pm_reel,7.089e10_pm_reel, &
625.e3_pm_reel,7.921e09_pm_reel,2.503e12_pm_reel,8.573e07_pm_reel,2.383e04_pm_reel,1.953e12_pm_reel,7.054e10_pm_reel, &
630.e3_pm_reel,6.908e09_pm_reel,2.315e12_pm_reel,7.332e07_pm_reel,1.961e04_pm_reel,1.915e12_pm_reel,7.019e10_pm_reel, &
635.e3_pm_reel,6.025e09_pm_reel,2.141e12_pm_reel,6.272e07_pm_reel,1.612e04_pm_reel,1.878e12_pm_reel,6.985e10_pm_reel, &
640.e3_pm_reel,5.257e09_pm_reel,1.981e12_pm_reel,5.367e07_pm_reel,1.328e04_pm_reel,1.842e12_pm_reel,6.950e10_pm_reel, &
645.e3_pm_reel,4.587e09_pm_reel,1.832e12_pm_reel,4.593e07_pm_reel,1.094e04_pm_reel,1.806e12_pm_reel,6.916e10_pm_reel, &
650.e3_pm_reel,4.003e09_pm_reel,1.695e12_pm_reel,3.932e07_pm_reel,9.006e03_pm_reel,1.771e12_pm_reel,6.883e10_pm_reel, &
655.e3_pm_reel,3.495e09_pm_reel,1.569e12_pm_reel,3.367e07_pm_reel,7.420e03_pm_reel,1.737e12_pm_reel,6.849e10_pm_reel /)

real(pm_reel), dimension (30*7), parameter  :: rntab13 = (/ &
660.e3_pm_reel,3.051e09_pm_reel,1.452e12_pm_reel,2.883e07_pm_reel,6.114e03_pm_reel,1.704e12_pm_reel,6.816e10_pm_reel, &
665.e3_pm_reel,2.665e09_pm_reel,1.344e12_pm_reel,2.470e07_pm_reel,5.040e03_pm_reel,1.671e12_pm_reel,6.782e10_pm_reel, &
670.e3_pm_reel,2.327e09_pm_reel,1.244e12_pm_reel,2.116e07_pm_reel,4.155e03_pm_reel,1.639e12_pm_reel,6.749e10_pm_reel, &
675.e3_pm_reel,2.033e09_pm_reel,1.151e12_pm_reel,1.813e07_pm_reel,3.427e03_pm_reel,1.608e12_pm_reel,6.717e10_pm_reel, &
680.e3_pm_reel,1.777e09_pm_reel,1.066e12_pm_reel,1.554e07_pm_reel,2.827e03_pm_reel,1.577e12_pm_reel,6.684e10_pm_reel, &
685.e3_pm_reel,1.553e09_pm_reel,9.870e11_pm_reel, 1.333e07_pm_reel,2.333e03_pm_reel,1.547e12_pm_reel,6.652e10_pm_reel, &
690.e3_pm_reel,1.357e09_pm_reel,9.140e11_pm_reel, 1.143e07_pm_reel,1.926e03_pm_reel,1.518e12_pm_reel,6.620e10_pm_reel, &
695.e3_pm_reel,1.187e09_pm_reel,8.465e11_pm_reel, 9.802e06_pm_reel,1.590e03_pm_reel,1.489e12_pm_reel,6.588e10_pm_reel, &
700.e3_pm_reel,1.038e09_pm_reel,7.840e11_pm_reel, 8.410e06_pm_reel,1.313e03_pm_reel,1.461e12_pm_reel,6.556e10_pm_reel, &
705.e3_pm_reel,9.075e08_pm_reel,7.263e11_pm_reel, 7.216e06_pm_reel,1.085e03_pm_reel,1.433e12_pm_reel,6.524e10_pm_reel, &

710.e3_pm_reel,7.939e08_pm_reel,6.728e11_pm_reel, 6.194e06_pm_reel,8.964e02_pm_reel,1.406e12_pm_reel,6.493e10_pm_reel, &
715.e3_pm_reel,6.946e08_pm_reel,6.234e11_pm_reel, 5.317e06_pm_reel,7.409e02_pm_reel,1.379e12_pm_reel,6.462e10_pm_reel, &
720.e3_pm_reel,6.078e08_pm_reel,5.777e11_pm_reel, 4.566e06_pm_reel,6.126e02_pm_reel,1.353e12_pm_reel,6.431e10_pm_reel, &
725.e3_pm_reel,5.320e08_pm_reel,5.354e11_pm_reel, 3.921e06_pm_reel,5.066e02_pm_reel,1.328e12_pm_reel,6.400e10_pm_reel, &
730.e3_pm_reel,4.658e08_pm_reel,4.962e11_pm_reel, 3.368e06_pm_reel,4.191e02_pm_reel,1.303e12_pm_reel,6.370e10_pm_reel, &
735.e3_pm_reel,4.078e08_pm_reel,4.599e11_pm_reel, 2.894e06_pm_reel,3.467e02_pm_reel,1.278e12_pm_reel,6.339e10_pm_reel, &
740.e3_pm_reel,3.572e08_pm_reel,4.264e11_pm_reel, 2.487e06_pm_reel,2.870e02_pm_reel,1.254e12_pm_reel,6.309e10_pm_reel, &
745.e3_pm_reel,3.129e08_pm_reel,3.953e11_pm_reel, 2.138e06_pm_reel,2.376e02_pm_reel,1.231e12_pm_reel,6.279e10_pm_reel, &
750.e3_pm_reel,2.741e08_pm_reel,3.666e11_pm_reel, 1.838e06_pm_reel,1.967e02_pm_reel,1.208e12_pm_reel,6.249e10_pm_reel, &
755.e3_pm_reel,2.402e08_pm_reel,3.399e11_pm_reel, 1.581e06_pm_reel,1.630e02_pm_reel,1.185e12_pm_reel,6.220e10_pm_reel, &

760.e3_pm_reel,2.105e08_pm_reel,3.153e11_pm_reel, 1.360e06_pm_reel,1.350e02_pm_reel,1.163e12_pm_reel,6.190e10_pm_reel, &
765.e3_pm_reel,1.845e08_pm_reel,2.924e11_pm_reel, 1.170e06_pm_reel,1.119e02_pm_reel,1.141e12_pm_reel,6.161e10_pm_reel, &
770.e3_pm_reel,1.618e08_pm_reel,2.712e11_pm_reel, 1.007e06_pm_reel,9.276e01_pm_reel,1.120e12_pm_reel,6.132e10_pm_reel, &
775.e3_pm_reel,1.419e08_pm_reel,2.516e11_pm_reel, 8.664e05_pm_reel,7.692e01_pm_reel,1.099e12_pm_reel,6.103e10_pm_reel, &
780.e3_pm_reel,1.244e08_pm_reel,2.335e11_pm_reel, 7.458e05_pm_reel,6.380e01_pm_reel,1.079e12_pm_reel,6.074e10_pm_reel, &
785.e3_pm_reel,1.092e08_pm_reel,2.166e11_pm_reel, 6.422e05_pm_reel,5.293e01_pm_reel,1.059e12_pm_reel,6.046e10_pm_reel, &
790.e3_pm_reel,9.577e07_pm_reel,2.011e11_pm_reel, 5.531e05_pm_reel,4.392e01_pm_reel,1.039e12_pm_reel,6.017e10_pm_reel, &
795.e3_pm_reel,8.404e07_pm_reel,1.866e11_pm_reel, 4.764e05_pm_reel,3.646e01_pm_reel,1.020e12_pm_reel,5.989e10_pm_reel, &
800.e3_pm_reel,7.377e07_pm_reel,1.732e11_pm_reel, 4.105e05_pm_reel,3.027e01_pm_reel,1.001e12_pm_reel,5.961e10_pm_reel, &
805.e3_pm_reel,6.476e07_pm_reel,1.608e11_pm_reel, 3.537e05_pm_reel,2.514e01_pm_reel,9.826e11_pm_reel, 5.933e10_pm_reel/)

real(pm_reel), dimension (30*7), parameter  :: rntab14 = (/ &
810.e3_pm_reel,5.686e07_pm_reel,1.493e11_pm_reel, 3.049e05_pm_reel,2.088e01_pm_reel,9.645e11_pm_reel, 5.905e10_pm_reel, &
815.e3_pm_reel,4.993e07_pm_reel,1.386e11_pm_reel, 2.628e05_pm_reel,1.735e01_pm_reel,9.468e11_pm_reel, 5.878e10_pm_reel, &
820.e3_pm_reel,4.386e07_pm_reel,1.287e11_pm_reel, 2.267e05_pm_reel,1.442e01_pm_reel,9.294e11_pm_reel, 5.851e10_pm_reel, &
825.e3_pm_reel,3.853e07_pm_reel,1.195e11_pm_reel, 1.955e05_pm_reel,1.199e01_pm_reel,9.124e11_pm_reel, 5.823e10_pm_reel, &
830.e3_pm_reel,3.386e07_pm_reel,1.110e11_pm_reel, 1.686e05_pm_reel,9.970e00_pm_reel,8.957e11_pm_reel, 5.796e10_pm_reel, &
835.e3_pm_reel,2.975e07_pm_reel,1.031e11_pm_reel, 1.455e05_pm_reel,8.293e00_pm_reel,8.793e11_pm_reel, 5.769e10_pm_reel, &
840.e3_pm_reel,2.615e07_pm_reel,9.580e10_pm_reel, 1.256e05_pm_reel,6.900e00_pm_reel,8.632e11_pm_reel, 5.743e10_pm_reel, &
845.e3_pm_reel,2.299e07_pm_reel,8.901e10_pm_reel, 1.084e05_pm_reel,5.742e00_pm_reel,8.475e11_pm_reel, 5.716e10_pm_reel, &
850.e3_pm_reel,2.022e07_pm_reel,8.270e10_pm_reel, 9.358e04_pm_reel,4.780e00_pm_reel,8.320e11_pm_reel, 5.690e10_pm_reel, &
855.e3_pm_reel,1.778e07_pm_reel,7.685e10_pm_reel, 8.081e04_pm_reel,3.980e00_pm_reel,8.169e11_pm_reel, 5.664e10_pm_reel, &

860.e3_pm_reel,1.564e07_pm_reel,7.142e10_pm_reel, 6.979e04_pm_reel,3.314e00_pm_reel,8.021e11_pm_reel, 5.637e10_pm_reel, &
865.e3_pm_reel,1.376e07_pm_reel,6.638e10_pm_reel, 6.029e04_pm_reel,2.761e00_pm_reel,7.875e11_pm_reel, 5.612e10_pm_reel, &
870.e3_pm_reel,1.211e07_pm_reel,6.171e10_pm_reel, 5.210e04_pm_reel,2.301e00_pm_reel,7.733e11_pm_reel, 5.586e10_pm_reel, &
875.e3_pm_reel,1.066e07_pm_reel,5.737e10_pm_reel, 4.503e04_pm_reel,1.918e00_pm_reel,7.593e11_pm_reel, 5.560e10_pm_reel, &
880.e3_pm_reel,9.380e06_pm_reel,5.334e10_pm_reel, 3.892e04_pm_reel,1.599e00_pm_reel,7.456e11_pm_reel, 5.535e10_pm_reel, &
885.e3_pm_reel,8.258e06_pm_reel,4.959e10_pm_reel, 3.365e04_pm_reel,1.333e00_pm_reel,7.321e11_pm_reel, 5.509e10_pm_reel, &
890.e3_pm_reel,7.271e06_pm_reel,4.612e10_pm_reel, 2.910e04_pm_reel,1.112e00_pm_reel,7.189e11_pm_reel, 5.484e10_pm_reel, &
895.e3_pm_reel,6.404e06_pm_reel,4.289e10_pm_reel, 2.517e04_pm_reel,9.277e-1_pm_reel,7.060e11_pm_reel, 5.459e10_pm_reel, &
900.e3_pm_reel,5.641e06_pm_reel,3.989e10_pm_reel, 2.177e04_pm_reel,7.742e-1_pm_reel,6.933e11_pm_reel, 5.434e10_pm_reel, &
905.e3_pm_reel,4.970e06_pm_reel,3.711e10_pm_reel, 1.884e04_pm_reel,6.462e-1_pm_reel,6.809e11_pm_reel, 5.410e10_pm_reel, &

910.e3_pm_reel,4.379e06_pm_reel,3.452e10_pm_reel, 1.631e04_pm_reel,5.396e-1_pm_reel,6.687e11_pm_reel, 5.385e10_pm_reel, &
915.e3_pm_reel,3.859e06_pm_reel,3.212e10_pm_reel, 1.411e04_pm_reel,4.506e-1_pm_reel,6.567e11_pm_reel, 5.361e10_pm_reel, &
920.e3_pm_reel,3.402e06_pm_reel,2.989e10_pm_reel, 1.222e04_pm_reel,3.764e-1_pm_reel,6.450e11_pm_reel, 5.336e10_pm_reel, &
925.e3_pm_reel,2.999e06_pm_reel,2.781e10_pm_reel, 1.058e04_pm_reel,3.145e-1_pm_reel,6.335e11_pm_reel, 5.312e10_pm_reel, &
930.e3_pm_reel,2.645e06_pm_reel,2.588e10_pm_reel, 9.165e03_pm_reel,2.629e-1_pm_reel,6.222e11_pm_reel, 5.288e10_pm_reel, &
935.e3_pm_reel,2.332e06_pm_reel,2.409e10_pm_reel, 7.940e03_pm_reel,2.197e-1_pm_reel,6.111e11_pm_reel, 5.264e10_pm_reel, &
940.e3_pm_reel,2.057e06_pm_reel,2.242e10_pm_reel, 6.880e03_pm_reel,1.837e-1_pm_reel,6.003e11_pm_reel, 5.241e10_pm_reel, &
945.e3_pm_reel,1.815e06_pm_reel,2.088e10_pm_reel, 5.962e03_pm_reel,1.537e-1_pm_reel,5.896e11_pm_reel, 5.217e10_pm_reel, &
950.e3_pm_reel,1.602e06_pm_reel,1.944e10_pm_reel, 5.168e03_pm_reel,1.286e-1_pm_reel,5.792e11_pm_reel, 5.194e10_pm_reel, &
955.e3_pm_reel,1.414e06_pm_reel,1.810e10_pm_reel, 4.481e03_pm_reel,1.076e-1_pm_reel,5.689e11_pm_reel, 5.170e10_pm_reel/)

real(pm_reel), dimension (10*7), parameter  :: rntab15 = (/ &
960.e3_pm_reel,1.248e06_pm_reel,1.685e10_pm_reel, 3.886e03_pm_reel,9.004e-2_pm_reel,5.589e11_pm_reel, 5.147e10_pm_reel, &
965.e3_pm_reel,1.102e06_pm_reel,1.569e10_pm_reel, 3.370e03_pm_reel,7.538e-2_pm_reel,5.490e11_pm_reel, 5.124e10_pm_reel, &
970.e3_pm_reel,9.726e05_pm_reel,1.462e10_pm_reel, 2.924e03_pm_reel,6.312e-2_pm_reel,5.393e11_pm_reel, 5.101e10_pm_reel, &
975.e3_pm_reel,8.590e05_pm_reel,1.362e10_pm_reel, 2.537e03_pm_reel,5.287e-2_pm_reel,5.298e11_pm_reel, 5.078e10_pm_reel, &
980.e3_pm_reel,7.587e05_pm_reel,1.268e10_pm_reel, 2.201e03_pm_reel,4.430e-2_pm_reel,5.205e11_pm_reel, 5.056e10_pm_reel, &
985.e3_pm_reel,6.703e05_pm_reel,1.182e10_pm_reel, 1.911e03_pm_reel,3.712e-2_pm_reel,5.114e11_pm_reel, 5.033e10_pm_reel, &
990.e3_pm_reel,5.922e05_pm_reel,1.101e10_pm_reel, 1.659e03_pm_reel,3.111e-2_pm_reel,5.024e11_pm_reel, 5.011e10_pm_reel, &
995.e3_pm_reel,5.234e05_pm_reel,1.026e10_pm_reel, 1.440e03_pm_reel,2.609e-2_pm_reel,4.936e11_pm_reel, 4.989e10_pm_reel, &
100.e4_pm_reel,4.626e05_pm_reel,9.562e09_pm_reel,1.251e03_pm_reel,2.188e-2_pm_reel,4.850e11_pm_reel, 4.967e10_pm_reel, &
100.e4_pm_reel,0._pm_reel    ,0._pm_reel    ,0._pm_reel    ,0._pm_reel     ,0._pm_reel    ,0._pm_reel/)

real(pm_reel), dimension (7*nmax), parameter   :: rntab0 = (/rntab01(:), rntab02(:), rntab03(:), rntab04(:), rntab05(:), &
                                                             rntab06(:), rntab07(:), rntab08(:), rntab09(:), rntab10(:), &
                                                             rntab11(:), rntab12(:), rntab13(:), rntab14(:), rntab15(:)/)

real(pm_reel),dimension(nmax,7)::rntab

intrinsic abs, exp, sqrt, sum, real

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSLIB mp_atm_US76.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mp_atm_US76.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************
! initialisation de la valeur du code retour
! ..........................................
 rntab = reshape(rntab0, (/nmax,7/), order= (/2,1/))
code_retour%valeur = pm_OK

altitude = alt

if (altitude > 1000.e3_pm_reel) then   ! altitude >1000km
   code_retour%valeur = pm_err_alt_sup1000km
   go to 6000
else if (altitude < 0._pm_reel) then   ! altitude < 0 km:
   altitude = 0._pm_reel
   code_retour%valeur = pm_warn_alt_negatif
end if

!calcul de temperature (t), pression (p) et xmol (m) pour alt < ou = 86 km
!==================================================================

if (altitude <= 86.e3_pm_reel) then

   rhpot=r0*altitude/(r0+altitude)    !        calcul de l'altitude geopotentielle h [equation 18]

   !  recherche de l'intervalle d'altitude encadrant rhpot tel que: htab(ii) <= rhpot < htab(ii+1)
   ii=6
   do  i=1,6
      if (rhpot < htab(i)) then
         ii=i-1
         exit
      end if
   end do

   tmol=tmtab(ii)+dttab(ii)*(rhpot-htab(ii))       !        calcul de la temperature moleculaire tm [equation 23]

   ! calcul de la temperature cinetique t et de la masse molaire m

   if (altitude < 80.e3_pm_reel) then  ! pour des altitudes z < 80 km    
      temperature=tmol
      xmol=xmol0

   else                                ! pour des altitudes z > ou = 80 km
      jj=11
      do  j=1,11   ! recherche de l'intervalle d'altitude encadrant alt tel que:80km + 500m*(j-1) < alt <= 80km + 500m*j

         zj=80000._pm_reel+real(j, kind=pm_reel)*500._pm_reel
         if (altitude <= zj) then
            jj=j-1
            exit
         end if
      end do

      ! interpolation (en fonction de l'altitude) du rapport m/m0:
      coeff=xmm0tb(jj)+(xmm0tb(jj+1)-xmm0tb(jj))/500._pm_reel*(altitude-zj+500._pm_reel)

      temperature=tmol*coeff            !  calcul de la temperature cinetique t (avec m/m0 interpole) [equation 22]
      xmol=xmol0*coeff                  !  calcul de la masse molaire m (interpolee)

   end if

   !  calcul de la pression p:
   !  -----------------------

   !  test sur le gradient de temperature
   if (abs(dttab(ii)) < epspre) then
      pression=ptab(ii)*exp(-rappor*(rhpot-htab(ii))/tmtab(ii))      !           [equation 33b]
   else
      pression=ptab(ii)*(tmtab(ii)/tmol)**(rappor/dttab(ii))         !           [equation 33a]
   end if

   !     calcul de temperature (t), pression (p) et xmol (m) pour alt > 86 km
   !     ====================================================================

else

   !        calcul de la temperature cinetique t:
   !        ------------------------------------

   if (altitude < ztab(8)) then
      temperature=ttab(7)                                            !           [equation 25]
   else if (altitude < ztab(9)) then
      rcar=sqrt(1._pm_reel-((altitude-ztab(8))/pta)**2)              !           [equation 27]
      temperature=tc+gda*rcar
   else if (altitude < ztab(10)) then
      temperature=ttab(9)+dttab(9)*(altitude-ztab(9))                !           [equation 29]
   else
      xi=(altitude-ztab(10))*(r0+ztab(10))/(r0+altitude)             !           [equation 31]
      temperature=ttab(12)-(ttab(12)-ttab(10))*exp(-rlambd*xi)
   end if

   !        calcul des number densities "Ni" pour chaque element N2,O,O2,Ar,He et H:
   
   !        recherche de l'intervalle d'altitude encadrant alt tel que
   !                      rntab(kk,1) < alt <= rntab(kk+1,1)
   !        On effectue cette recherche par dichotomie

   kmoins = 1
   k_dicho = nmax/2 
   pas_dicho = k_dicho/2
   do while (pas_dicho>0)
      if (altitude<=rntab(k_dicho,1)) then
         k_dicho = k_dicho - pas_dicho
      else
         kmoins = k_dicho
         k_dicho = k_dicho + pas_dicho
      end if
      pas_dicho = pas_dicho/2
   enddo
   
   kk= kmoins
   do while (rntab(kk+1,1)<altitude) 
      kk = kk+1
   enddo

   pen(2:7)=(rntab(kk+1,2:7)-rntab(kk,2:7)) / (rntab(kk+1,1)-rntab(kk,1))
   rnn(2:7)=rntab(kk,2:7)+pen(2:7)*(altitude-rntab(kk,1))   !          interpolation suivant l'altitude pour chaque "ni"

   call mui_dot_product6 ( rnn(2:7) , rmoltb(2:7) , xmol , retour )
   
   rn = sum(rnn(2:7))

   xmol=xmol/rn                            !        calcul de la masse molaire m [equation 20]

   pression=rn*boltz*temperature           !        calcul de la pression p [equation 33c]

end if

densite=(pression*xmol)/(rstar*temperature)!        calcul de la densite rho  [equation 42]

! Affectation des sorties
tempe = temperature
pres  = pression
dens  = densite

6000  continue

code_retour%routine = pm_num_mp_atm_US76
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mp_atm_US76
