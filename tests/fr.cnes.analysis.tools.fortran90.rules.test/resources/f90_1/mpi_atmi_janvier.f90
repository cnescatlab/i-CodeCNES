subroutine mpi_atmi_janvier (tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2, retour )

! (C) Copyright CNES - MSPRO - 2000

!************************************************************************
!
! But: Lecture du fichier ATMospherique pour l'Interplation correspondant au mois de JANVIER 
! ===
!
! Note d'utilisation:  Sans objet
! ==================
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 1.0  creation a partir d'une portion de la routine MCATMI 
!                         (Date: 12/2000 - Realisation: Veronique Lepine)
!   + Version 3.1 (DE globale 4) : Modifications suite aux remarques qualite ATV
!                         (Date: 07/2003 - Realisation: Bruno Revelin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 5.10: DM-ID 1058 : Correction des warnings levés par g95
!                   (Date: 8/2008 - Realisation: Atos origin)
!VERSION:5.15:FA-ID:1398:30/09/2010:Ajout du marqueur de fin historique
!
!$FinHistorique
!
!************************************************************************

  ! Modules
  ! =======

  use mslib

  use parametre_mspro

  ! Declarations
  ! ============
  implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), dimension(:,:), intent(out)    ::  tbar  ! temperatures 
real(pm_reel), dimension(:,:), intent(out)    ::  zbar  ! altitudes
real(pm_reel), dimension(:,:), intent(out)    ::  z1    ! amplitude de l'altitude de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  phi1  ! phase de l'altitude de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  t1    ! amplitude de la temperature de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  phit1 ! phase de la temperature de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  z2    ! amplitude de l'altitude de l'onde 2
real(pm_reel), dimension(:,:), intent(out)    ::  phi2  ! phase de l'altitude de l'onde 2
real(pm_reel), dimension(:,:), intent(out)    ::  t2    ! amplitude de la temperature de l'onde 2
real(pm_reel), dimension(:,:), intent(out)    ::  phit2 ! phase de la temperature de l'onde 2
integer, intent(out)                          ::  retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Autres declarations
  ! -------------------

  real(pm_reel), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_80 = (/ &   ! donnees latitude -80
       264.20_pm_r,   -65._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       248.90_pm_r,  3680._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       227.20_pm_r,  7155._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       228.00_pm_r, 10477._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       231.80_pm_r, 13833._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       233.60_pm_r, 17242._pm_r,   2._pm_r,222._pm_r,  .02_pm_r,288._pm_r,   1._pm_r,133._pm_r,  .07_pm_r,287._pm_r, & 
       234.80_pm_r, 20673._pm_r,   2._pm_r,223._pm_r,  .04_pm_r,  0._pm_r,   1._pm_r,140._pm_r,  .09_pm_r,276._pm_r, & 
       235.40_pm_r, 24116._pm_r,   2._pm_r,225._pm_r,  .09_pm_r, 27._pm_r,   0._pm_r,156._pm_r,  .12_pm_r,263._pm_r, & 
       238.10_pm_r, 27577._pm_r,   1._pm_r,227._pm_r,  .17_pm_r, 33._pm_r,   0._pm_r,182._pm_r,  .14_pm_r,249._pm_r, & 
       249.10_pm_r, 31139._pm_r,   1._pm_r,230._pm_r,  .24_pm_r, 34._pm_r,   1._pm_r,201._pm_r,  .16_pm_r,231._pm_r, & 
       261.70_pm_r, 34880._pm_r,   1._pm_r,238._pm_r,  .28_pm_r, 35._pm_r,   1._pm_r,208._pm_r,  .19_pm_r,216._pm_r, & 
       273.30_pm_r, 38802._pm_r,   0._pm_r,264._pm_r,  .29_pm_r, 34._pm_r,   1._pm_r,209._pm_r,  .21_pm_r,204._pm_r, & 
       283.00_pm_r, 42876._pm_r,   0._pm_r,332._pm_r,  .26_pm_r, 29._pm_r,   1._pm_r,206._pm_r,  .24_pm_r,195._pm_r, & 
       288.30_pm_r, 47067._pm_r,   1._pm_r,355._pm_r,  .14_pm_r, 12._pm_r,   2._pm_r,203._pm_r,  .29_pm_r,188._pm_r, & 
       287.20_pm_r, 51289._pm_r,   1._pm_r,354._pm_r,  .08_pm_r,284._pm_r,   2._pm_r,200._pm_r,  .40_pm_r,196._pm_r, & 
       279.10_pm_r, 55439._pm_r,   1._pm_r,339._pm_r,  .18_pm_r,232._pm_r,   3._pm_r,200._pm_r,  .48_pm_r,203._pm_r, & 
       267.30_pm_r, 59446._pm_r,   1._pm_r,312._pm_r,  .26_pm_r,212._pm_r,   4._pm_r,201._pm_r,  .50_pm_r,208._pm_r, & 
       253.40_pm_r, 63257._pm_r,   1._pm_r,280._pm_r,  .20_pm_r,212._pm_r,   4._pm_r,202._pm_r,  .30_pm_r,210._pm_r, & 
       238.50_pm_r, 66864._pm_r,   1._pm_r,266._pm_r,  .10_pm_r,253._pm_r,   5._pm_r,203._pm_r,  .08_pm_r,240._pm_r, & 
       222.50_pm_r, 70236._pm_r,   1._pm_r,271._pm_r,  .16_pm_r,318._pm_r,   4._pm_r,204._pm_r,  .16_pm_r,  2._pm_r, & 
       206.20_pm_r, 73381._pm_r,   1._pm_r,284._pm_r,  .27_pm_r,332._pm_r,   4._pm_r,205._pm_r,  .32_pm_r,  9._pm_r, & 
       189.90_pm_r, 76277._pm_r,   2._pm_r,298._pm_r,  .34_pm_r,336._pm_r,   4._pm_r,208._pm_r,  .40_pm_r,  9._pm_r, & 
       174.10_pm_r, 78945._pm_r,   2._pm_r,308._pm_r,  .35_pm_r,338._pm_r,   3._pm_r,211._pm_r,  .41_pm_r, 10._pm_r, & 
       160.80_pm_r, 81385._pm_r,   2._pm_r,314._pm_r,  .33_pm_r,338._pm_r,   3._pm_r,216._pm_r,  .37_pm_r,  9._pm_r, & 
       148.10_pm_r, 83647._pm_r,   3._pm_r,318._pm_r,  .28_pm_r,339._pm_r,   2._pm_r,223._pm_r,  .32_pm_r,  9._pm_r, & 
       138.50_pm_r, 85692._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       136.70_pm_r, 87670._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       140.30_pm_r, 89691._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       148.60_pm_r, 91808._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       162.10_pm_r, 94093._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       182.90_pm_r, 96640._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       214.30_pm_r, 99604._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       257.70_pm_r,103173._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       295.50_pm_r,107454._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       319.40_pm_r,112258._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       404.50_pm_r,117991._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_70 = (/ &      ! donnees latitude -70
       270.00_pm_r,  -112._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       251.50_pm_r,  3696._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       228.80_pm_r,  7203._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       226.60_pm_r, 10528._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       230.50_pm_r, 13866._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       232.50_pm_r, 17259._pm_r,   4._pm_r,215._pm_r,  .19_pm_r, 33._pm_r,   1._pm_r, 92._pm_r,  .06_pm_r,209._pm_r, & 
       233.30_pm_r, 20671._pm_r,   3._pm_r,215._pm_r,  .29_pm_r, 34._pm_r,   1._pm_r,101._pm_r,  .09_pm_r,208._pm_r, & 
       233.50_pm_r, 24087._pm_r,   3._pm_r,215._pm_r,  .38_pm_r, 33._pm_r,   1._pm_r,115._pm_r,  .10_pm_r,207._pm_r, & 
       237.20_pm_r, 27526._pm_r,   2._pm_r,216._pm_r,  .44_pm_r, 33._pm_r,   1._pm_r,130._pm_r,  .11_pm_r,205._pm_r, & 
       248.40_pm_r, 31077._pm_r,   2._pm_r,217._pm_r,  .44_pm_r, 33._pm_r,   1._pm_r,143._pm_r,  .09_pm_r,206._pm_r, & 
       260.30_pm_r, 34802._pm_r,   1._pm_r,219._pm_r,  .40_pm_r, 33._pm_r,   1._pm_r,152._pm_r,  .06_pm_r,209._pm_r, & 
       271.10_pm_r, 38698._pm_r,   1._pm_r,225._pm_r,  .32_pm_r, 34._pm_r,   1._pm_r,156._pm_r,  .03_pm_r,225._pm_r, & 
       280.40_pm_r, 42736._pm_r,   0._pm_r,253._pm_r,  .22_pm_r, 32._pm_r,   1._pm_r,158._pm_r,  .02_pm_r,326._pm_r, & 
       284.90_pm_r, 46884._pm_r,   0._pm_r,352._pm_r,  .17_pm_r, 28._pm_r,   1._pm_r,159._pm_r,  .07_pm_r,327._pm_r, & 
       283.20_pm_r, 51051._pm_r,   0._pm_r,  6._pm_r,  .10_pm_r,  6._pm_r,   1._pm_r,162._pm_r,  .13_pm_r,333._pm_r, & 
       275.60_pm_r, 55146._pm_r,   1._pm_r,  3._pm_r,  .03_pm_r,299._pm_r,   0._pm_r,161._pm_r,  .14_pm_r,359._pm_r, & 
       264.60_pm_r, 59108._pm_r,   0._pm_r,358._pm_r,  .09_pm_r,193._pm_r,   0._pm_r,117._pm_r,  .16_pm_r, 46._pm_r, & 
       251.50_pm_r, 62884._pm_r,   0._pm_r,353._pm_r,  .12_pm_r,179._pm_r,   0._pm_r, 82._pm_r,  .21_pm_r, 66._pm_r, & 
       237.50_pm_r, 66470._pm_r,   0._pm_r,345._pm_r,  .12_pm_r,175._pm_r,   1._pm_r, 75._pm_r,  .22_pm_r, 64._pm_r, & 
       222.90_pm_r, 69838._pm_r,   0._pm_r,198._pm_r,  .04_pm_r,180._pm_r,   1._pm_r, 69._pm_r,  .17_pm_r, 41._pm_r, & 
       207.50_pm_r, 72995._pm_r,   0._pm_r,215._pm_r,  .05_pm_r,328._pm_r,   1._pm_r, 61._pm_r,  .16_pm_r,  7._pm_r, & 
       191.90_pm_r, 75915._pm_r,   0._pm_r,310._pm_r,  .10_pm_r,335._pm_r,   1._pm_r, 50._pm_r,  .20_pm_r,343._pm_r, & 
       176.70_pm_r, 78617._pm_r,   0._pm_r,326._pm_r,  .13_pm_r,334._pm_r,   1._pm_r, 38._pm_r,  .21_pm_r,335._pm_r, & 
       163.80_pm_r, 81098._pm_r,   1._pm_r,330._pm_r,  .14_pm_r,335._pm_r,   2._pm_r, 27._pm_r,  .21_pm_r,328._pm_r, & 
       152.10_pm_r, 83407._pm_r,   1._pm_r,331._pm_r,  .13_pm_r,336._pm_r,   2._pm_r, 18._pm_r,  .19_pm_r,325._pm_r, & 
       143.80_pm_r, 85533._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       142.10_pm_r, 87597._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       144.90_pm_r, 89692._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       152.40_pm_r, 91871._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       164.80_pm_r, 94204._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       183.80_pm_r, 96781._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       212.50_pm_r, 99741._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       251.60_pm_r,103252._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       289.80_pm_r,107435._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       319.70_pm_r,112202._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       402.60_pm_r,117930._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_60 = (/ &      ! donnees latitude -60
       275.80_pm_r,  -109._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       255.60_pm_r,  3774._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       232.00_pm_r,  7337._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       225.10_pm_r, 10677._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       226.80_pm_r, 13979._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       228.30_pm_r, 17312._pm_r,   5._pm_r,212._pm_r,  .35_pm_r, 36._pm_r,   1._pm_r, 76._pm_r,  .14_pm_r,180._pm_r, & 
       229.90_pm_r, 20668._pm_r,   4._pm_r,212._pm_r,  .49_pm_r, 33._pm_r,   1._pm_r, 91._pm_r,  .19_pm_r,177._pm_r, & 
       231.50_pm_r, 24043._pm_r,   4._pm_r,212._pm_r,  .57_pm_r, 30._pm_r,   1._pm_r,109._pm_r,  .22_pm_r,177._pm_r, & 
       237.30_pm_r, 27469._pm_r,   3._pm_r,213._pm_r,  .58_pm_r, 27._pm_r,   1._pm_r,124._pm_r,  .21_pm_r,175._pm_r, & 
       246.90_pm_r, 31015._pm_r,   2._pm_r,217._pm_r,  .51_pm_r, 22._pm_r,   1._pm_r,134._pm_r,  .18_pm_r,171._pm_r, & 
       257.60_pm_r, 34706._pm_r,   1._pm_r,226._pm_r,  .39_pm_r, 15._pm_r,   2._pm_r,139._pm_r,  .13_pm_r,162._pm_r, & 
       268.10_pm_r, 38560._pm_r,   1._pm_r,242._pm_r,  .27_pm_r,  9._pm_r,   2._pm_r,140._pm_r,  .07_pm_r,146._pm_r, & 
       277.00_pm_r, 42551._pm_r,   1._pm_r,260._pm_r,  .15_pm_r,360._pm_r,   2._pm_r,140._pm_r,  .04_pm_r, 90._pm_r, & 
       280.90_pm_r, 46646._pm_r,   1._pm_r,272._pm_r,  .12_pm_r, 28._pm_r,   2._pm_r,137._pm_r,  .06_pm_r, 23._pm_r, & 
       278.70_pm_r, 50750._pm_r,   1._pm_r,284._pm_r,  .09_pm_r, 32._pm_r,   2._pm_r,134._pm_r,  .08_pm_r,  7._pm_r, & 
       271.80_pm_r, 54783._pm_r,   1._pm_r,290._pm_r,  .06_pm_r,315._pm_r,   2._pm_r,131._pm_r,  .06_pm_r, 24._pm_r, & 
       261.40_pm_r, 58694._pm_r,   1._pm_r,288._pm_r,  .19_pm_r,264._pm_r,   2._pm_r,128._pm_r,  .07_pm_r, 74._pm_r, & 
       248.80_pm_r, 62428._pm_r,   1._pm_r,280._pm_r,  .23_pm_r,250._pm_r,   2._pm_r,126._pm_r,  .13_pm_r, 99._pm_r, & 
       236.00_pm_r, 65981._pm_r,   2._pm_r,272._pm_r,  .22_pm_r,236._pm_r,   2._pm_r,123._pm_r,  .18_pm_r,103._pm_r, & 
       222.80_pm_r, 69338._pm_r,   2._pm_r,266._pm_r,  .17_pm_r,224._pm_r,   2._pm_r,121._pm_r,  .23_pm_r,109._pm_r, & 
       208.90_pm_r, 72504._pm_r,   2._pm_r,261._pm_r,  .12_pm_r,211._pm_r,   3._pm_r,119._pm_r,  .26_pm_r,106._pm_r, & 
       194.50_pm_r, 75454._pm_r,   2._pm_r,257._pm_r,  .08_pm_r,173._pm_r,   3._pm_r,118._pm_r,  .27_pm_r,110._pm_r, & 
       180.40_pm_r, 78202._pm_r,   2._pm_r,254._pm_r,  .06_pm_r,149._pm_r,   3._pm_r,117._pm_r,  .24_pm_r,109._pm_r, & 
       168.40_pm_r, 80746._pm_r,   2._pm_r,252._pm_r,  .07_pm_r,132._pm_r,   4._pm_r,116._pm_r,  .22_pm_r,111._pm_r, & 
       158.50_pm_r, 83128._pm_r,   2._pm_r,249._pm_r,  .07_pm_r,117._pm_r,   4._pm_r,116._pm_r,  .18_pm_r,112._pm_r, & 
       152.20_pm_r, 85381._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       150.50_pm_r, 87579._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       151.90_pm_r, 89787._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       158.00_pm_r, 92061._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       168.60_pm_r, 94465._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       185.00_pm_r, 97084._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       209.70_pm_r,100036._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       243.50_pm_r,103467._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       281.40_pm_r,107515._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       319.30_pm_r,112215._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       399.80_pm_r,117929._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_50 = (/ &      ! donnees latitude -50
       281.60_pm_r,   -55._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       262.10_pm_r,  3922._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       237.80_pm_r,  7578._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       223.50_pm_r, 10952._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       220.40_pm_r, 14198._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       220.90_pm_r, 17427._pm_r,   5._pm_r,207._pm_r,  .48_pm_r, 11._pm_r,   1._pm_r, 65._pm_r,  .14_pm_r,170._pm_r, & 
       223.50_pm_r, 20679._pm_r,   4._pm_r,210._pm_r,  .63_pm_r, 10._pm_r,   1._pm_r, 89._pm_r,  .19_pm_r,168._pm_r, & 
       227.50_pm_r, 23980._pm_r,   3._pm_r,216._pm_r,  .71_pm_r,  8._pm_r,   1._pm_r,114._pm_r,  .23_pm_r,168._pm_r, & 
       233.90_pm_r, 27354._pm_r,   3._pm_r,227._pm_r,  .67_pm_r,  6._pm_r,   1._pm_r,131._pm_r,  .23_pm_r,167._pm_r, & 
       243.50_pm_r, 30851._pm_r,   2._pm_r,246._pm_r,  .55_pm_r,  3._pm_r,   1._pm_r,139._pm_r,  .19_pm_r,167._pm_r, & 
       253.60_pm_r, 34488._pm_r,   2._pm_r,266._pm_r,  .36_pm_r,359._pm_r,   1._pm_r,144._pm_r,  .14_pm_r,163._pm_r, & 
       263.90_pm_r, 38281._pm_r,   2._pm_r,279._pm_r,  .18_pm_r,350._pm_r,   2._pm_r,145._pm_r,  .08_pm_r,159._pm_r, & 
       272.70_pm_r, 42211._pm_r,   2._pm_r,283._pm_r,  .04_pm_r,310._pm_r,   2._pm_r,146._pm_r,  .02_pm_r,135._pm_r, & 
       276.80_pm_r, 46241._pm_r,   2._pm_r,284._pm_r,  .06_pm_r, 60._pm_r,   2._pm_r,145._pm_r,  .04_pm_r,336._pm_r, & 
       274.80_pm_r, 50288._pm_r,   2._pm_r,289._pm_r,  .20_pm_r, 54._pm_r,   2._pm_r,145._pm_r,  .09_pm_r,315._pm_r, & 
       267.10_pm_r, 54257._pm_r,   2._pm_r,299._pm_r,  .25_pm_r, 55._pm_r,   1._pm_r,146._pm_r,  .08_pm_r,314._pm_r, & 
       257.00_pm_r, 58100._pm_r,   1._pm_r,309._pm_r,  .13_pm_r, 81._pm_r,   1._pm_r,146._pm_r,  .05_pm_r, 53._pm_r, & 
       245.10_pm_r, 61776._pm_r,   1._pm_r,305._pm_r,  .21_pm_r,189._pm_r,   1._pm_r,141._pm_r,  .14_pm_r, 81._pm_r, & 
       232.50_pm_r, 65275._pm_r,   1._pm_r,284._pm_r,  .43_pm_r,205._pm_r,   2._pm_r,132._pm_r,  .21_pm_r, 79._pm_r, & 
       221.40_pm_r, 68595._pm_r,   2._pm_r,257._pm_r,  .54_pm_r,209._pm_r,   2._pm_r,123._pm_r,  .22_pm_r, 68._pm_r, & 
       210.70_pm_r, 71763._pm_r,   2._pm_r,241._pm_r,  .58_pm_r,211._pm_r,   2._pm_r,115._pm_r,  .21_pm_r, 54._pm_r, & 
       199.80_pm_r, 74766._pm_r,   3._pm_r,233._pm_r,  .56_pm_r,211._pm_r,   2._pm_r,107._pm_r,  .20_pm_r, 42._pm_r, & 
       188.90_pm_r, 77615._pm_r,   4._pm_r,228._pm_r,  .53_pm_r,211._pm_r,   2._pm_r,100._pm_r,  .19_pm_r, 34._pm_r, & 
       179.10_pm_r, 80302._pm_r,   4._pm_r,226._pm_r,  .47_pm_r,213._pm_r,   2._pm_r, 94._pm_r,  .17_pm_r, 27._pm_r, & 
       170.50_pm_r, 82857._pm_r,   5._pm_r,224._pm_r,  .40_pm_r,213._pm_r,   2._pm_r, 89._pm_r,  .15_pm_r, 21._pm_r, & 
       164.00_pm_r, 85291._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       160.80_pm_r, 87654._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       160.50_pm_r, 90002._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       164.80_pm_r, 92391._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       173.10_pm_r, 94881._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       186.20_pm_r, 97547._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       206.50_pm_r,100488._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       234.80_pm_r,103832._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       271.30_pm_r,107728._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       317.00_pm_r,112333._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       396.40_pm_r,118015._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/) 
       

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_40 = (/ &      ! donnees latitude -40
       289.50_pm_r,   -13._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       269.20_pm_r,  4077._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       244.00_pm_r,  7833._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       222.90_pm_r, 11251._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       213.00_pm_r, 14442._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       212.10_pm_r, 17545._pm_r,   3._pm_r,196._pm_r,  .31_pm_r,325._pm_r,   0._pm_r,176._pm_r,  .05_pm_r,143._pm_r, & 
       216.70_pm_r, 20681._pm_r,   3._pm_r,203._pm_r,  .39_pm_r,324._pm_r,   0._pm_r,159._pm_r,  .06_pm_r,141._pm_r, & 
       223.30_pm_r, 23905._pm_r,   3._pm_r,214._pm_r,  .43_pm_r,325._pm_r,   0._pm_r,154._pm_r,  .07_pm_r,146._pm_r, & 
       230.00_pm_r, 27223._pm_r,   3._pm_r,227._pm_r,  .42_pm_r,326._pm_r,   0._pm_r,151._pm_r,  .07_pm_r,146._pm_r, & 
       238.20_pm_r, 30651._pm_r,   3._pm_r,240._pm_r,  .35_pm_r,325._pm_r,   1._pm_r,150._pm_r,  .07_pm_r,148._pm_r, & 
       248.80_pm_r, 34213._pm_r,   3._pm_r,249._pm_r,  .24_pm_r,327._pm_r,   1._pm_r,150._pm_r,  .05_pm_r,143._pm_r, & 
       259.40_pm_r, 37938._pm_r,   3._pm_r,254._pm_r,  .12_pm_r,328._pm_r,   1._pm_r,150._pm_r,  .04_pm_r,146._pm_r, & 
       268.80_pm_r, 41806._pm_r,   3._pm_r,256._pm_r,  .03_pm_r,342._pm_r,   1._pm_r,149._pm_r,  .03_pm_r,135._pm_r, & 
       273.30_pm_r, 45783._pm_r,   3._pm_r,258._pm_r,  .08_pm_r, 30._pm_r,   1._pm_r,152._pm_r,  .09_pm_r,183._pm_r, & 
       271.20_pm_r, 49779._pm_r,   3._pm_r,261._pm_r,  .20_pm_r, 27._pm_r,   1._pm_r,160._pm_r,  .17_pm_r,197._pm_r, & 
       263.10_pm_r, 53693._pm_r,   2._pm_r,267._pm_r,  .24_pm_r, 43._pm_r,   1._pm_r,167._pm_r,  .14_pm_r,199._pm_r, & 
       253.30_pm_r, 57479._pm_r,   2._pm_r,270._pm_r,  .23_pm_r, 92._pm_r,   1._pm_r,170._pm_r,  .04_pm_r,146._pm_r, & 
       241.70_pm_r, 61103._pm_r,   2._pm_r,263._pm_r,  .35_pm_r,144._pm_r,   1._pm_r,164._pm_r,  .16_pm_r, 60._pm_r, & 
       229.10_pm_r, 64553._pm_r,   2._pm_r,243._pm_r,  .47_pm_r,165._pm_r,   1._pm_r,149._pm_r,  .24_pm_r, 62._pm_r, & 
       218.90_pm_r, 67828._pm_r,   2._pm_r,224._pm_r,  .47_pm_r,178._pm_r,   1._pm_r,132._pm_r,  .26_pm_r, 76._pm_r, & 
       211.20_pm_r, 70978._pm_r,   3._pm_r,215._pm_r,  .44_pm_r,191._pm_r,   2._pm_r,121._pm_r,  .26_pm_r, 90._pm_r, & 
       205.10_pm_r, 74024._pm_r,   3._pm_r,211._pm_r,  .40_pm_r,203._pm_r,   2._pm_r,117._pm_r,  .26_pm_r,105._pm_r, & 
       198.50_pm_r, 76982._pm_r,   4._pm_r,211._pm_r,  .37_pm_r,213._pm_r,   2._pm_r,116._pm_r,  .26_pm_r,119._pm_r, & 
       191.60_pm_r, 79836._pm_r,   4._pm_r,212._pm_r,  .35_pm_r,221._pm_r,   3._pm_r,116._pm_r,  .27_pm_r,124._pm_r, & 
       184.10_pm_r, 82593._pm_r,   5._pm_r,213._pm_r,  .31_pm_r,228._pm_r,   3._pm_r,118._pm_r,  .25_pm_r,127._pm_r, & 
       176.40_pm_r, 85221._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       171.50_pm_r, 87753._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       169.60_pm_r, 90248._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       172.00_pm_r, 92758._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       177.70_pm_r, 95338._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       187.50_pm_r, 98052._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       203.20_pm_r,100980._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       226.40_pm_r,104239._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       260.60_pm_r,107986._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       312.50_pm_r,112469._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       392.40_pm_r,118097._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)      

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_30 = (/ &      ! donnees latitude -30
       296.00_pm_r,   -41._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       274.30_pm_r,  4137._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       249.30_pm_r,  7973._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       224.70_pm_r, 11446._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       207.00_pm_r, 14609._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       204.90_pm_r, 17605._pm_r,   3._pm_r,186._pm_r,  .25_pm_r,272._pm_r,   0._pm_r,331._pm_r,  .15_pm_r,100._pm_r, & 
       212.00_pm_r, 20655._pm_r,   3._pm_r,195._pm_r,  .31_pm_r,272._pm_r,   0._pm_r, 16._pm_r,  .20_pm_r,100._pm_r, & 
       219.90_pm_r, 23820._pm_r,   3._pm_r,204._pm_r,  .31_pm_r,272._pm_r,   1._pm_r, 59._pm_r,  .22_pm_r,100._pm_r, & 
       226.40_pm_r, 27087._pm_r,   3._pm_r,212._pm_r,  .27_pm_r,273._pm_r,   1._pm_r, 76._pm_r,  .23_pm_r,100._pm_r, & 
       233.60_pm_r, 30455._pm_r,   3._pm_r,218._pm_r,  .19_pm_r,275._pm_r,   1._pm_r, 84._pm_r,  .21_pm_r,100._pm_r, & 
       244.30_pm_r, 33950._pm_r,   3._pm_r,222._pm_r,  .10_pm_r,290._pm_r,   1._pm_r, 87._pm_r,  .16_pm_r, 99._pm_r, & 
       254.70_pm_r, 37608._pm_r,   3._pm_r,223._pm_r,  .06_pm_r,346._pm_r,   2._pm_r, 89._pm_r,  .11_pm_r, 99._pm_r, & 
       264.30_pm_r, 41407._pm_r,   3._pm_r,225._pm_r,  .10_pm_r, 21._pm_r,   2._pm_r, 89._pm_r,  .06_pm_r, 90._pm_r, & 
       269.90_pm_r, 45326._pm_r,   3._pm_r,226._pm_r,  .13_pm_r,  9._pm_r,   2._pm_r, 91._pm_r,  .08_pm_r,180._pm_r, & 
       269.10_pm_r, 49281._pm_r,   3._pm_r,229._pm_r,  .14_pm_r, 18._pm_r,   2._pm_r, 98._pm_r,  .21_pm_r,207._pm_r, & 
       261.90_pm_r, 53172._pm_r,   3._pm_r,229._pm_r,  .12_pm_r, 81._pm_r,   2._pm_r,111._pm_r,  .26_pm_r,214._pm_r, & 
       252.40_pm_r, 56942._pm_r,   3._pm_r,223._pm_r,  .32_pm_r,136._pm_r,   2._pm_r,124._pm_r,  .20_pm_r,216._pm_r, & 
       241.00_pm_r, 60555._pm_r,   3._pm_r,211._pm_r,  .41_pm_r,147._pm_r,   2._pm_r,130._pm_r,  .02_pm_r,148._pm_r, & 
       228.50_pm_r, 63995._pm_r,   3._pm_r,202._pm_r,  .35_pm_r,161._pm_r,   2._pm_r,126._pm_r,  .15_pm_r, 63._pm_r, & 
       218.40_pm_r, 67261._pm_r,   3._pm_r,198._pm_r,  .23_pm_r,186._pm_r,   2._pm_r,118._pm_r,  .23_pm_r, 65._pm_r, & 
       212.20_pm_r, 70411._pm_r,   4._pm_r,199._pm_r,  .18_pm_r,246._pm_r,   2._pm_r,109._pm_r,  .27_pm_r, 73._pm_r, & 
       208.80_pm_r, 73492._pm_r,   4._pm_r,204._pm_r,  .28_pm_r,281._pm_r,   2._pm_r,104._pm_r,  .29_pm_r, 79._pm_r, & 
       205.70_pm_r, 76529._pm_r,   4._pm_r,211._pm_r,  .39_pm_r,293._pm_r,   3._pm_r,100._pm_r,  .29_pm_r, 84._pm_r, & 
       201.80_pm_r, 79513._pm_r,   4._pm_r,221._pm_r,  .46_pm_r,299._pm_r,   3._pm_r, 98._pm_r,  .29_pm_r, 88._pm_r, & 
       195.50_pm_r, 82441._pm_r,   4._pm_r,230._pm_r,  .49_pm_r,302._pm_r,   4._pm_r, 97._pm_r,  .26_pm_r, 90._pm_r, & 
       186.80_pm_r, 85227._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       180.60_pm_r, 87900._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       177.70_pm_r, 90524._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       178.50_pm_r, 93146._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       181.90_pm_r, 95809._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       188.60_pm_r, 98565._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       200.10_pm_r,101481._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       218.90_pm_r,104661._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       250.30_pm_r,108270._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       305.90_pm_r,112616._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       387.90_pm_r,118167._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)   

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_20 = (/ &      ! donnees latitude -20
       299.20_pm_r,   -77._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       276.60_pm_r,  4145._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       252.60_pm_r,  8025._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       226.20_pm_r, 11535._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       203.00_pm_r, 14682._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       200.40_pm_r, 17607._pm_r,   3._pm_r,166._pm_r,  .31_pm_r,274._pm_r,   0._pm_r,339._pm_r,  .23_pm_r, 96._pm_r, & 
       209.30_pm_r, 20605._pm_r,   3._pm_r,176._pm_r,  .38_pm_r,273._pm_r,   0._pm_r, 41._pm_r,  .29_pm_r, 96._pm_r, & 
       216.90_pm_r, 23730._pm_r,   3._pm_r,186._pm_r,  .37_pm_r,273._pm_r,   1._pm_r, 70._pm_r,  .31_pm_r, 96._pm_r, & 
       223.00_pm_r, 26950._pm_r,   3._pm_r,196._pm_r,  .32_pm_r,274._pm_r,   1._pm_r, 80._pm_r,  .30_pm_r, 96._pm_r, & 
       230.20_pm_r, 30267._pm_r,   3._pm_r,203._pm_r,  .21_pm_r,276._pm_r,   2._pm_r, 84._pm_r,  .25_pm_r, 96._pm_r, & 
       241.10_pm_r, 33714._pm_r,   3._pm_r,207._pm_r,  .09_pm_r,291._pm_r,   2._pm_r, 86._pm_r,  .16_pm_r, 94._pm_r, & 
       250.60_pm_r, 37320._pm_r,   3._pm_r,208._pm_r,  .06_pm_r, 15._pm_r,   2._pm_r, 86._pm_r,  .09_pm_r, 87._pm_r, & 
       260.10_pm_r, 41055._pm_r,   3._pm_r,208._pm_r,  .11_pm_r, 34._pm_r,   2._pm_r, 86._pm_r,  .03_pm_r, 45._pm_r, & 
       267.40_pm_r, 44926._pm_r,   3._pm_r,208._pm_r,  .10_pm_r,354._pm_r,   2._pm_r, 85._pm_r,  .01_pm_r,360._pm_r, & 
       268.20_pm_r, 48855._pm_r,   3._pm_r,212._pm_r,  .22_pm_r,321._pm_r,   2._pm_r, 86._pm_r,  .06_pm_r,135._pm_r, & 
       262.70_pm_r, 52744._pm_r,   3._pm_r,220._pm_r,  .28_pm_r,310._pm_r,   2._pm_r, 90._pm_r,  .16_pm_r,176._pm_r, & 
       255.00_pm_r, 56538._pm_r,   3._pm_r,227._pm_r,  .19_pm_r,279._pm_r,   2._pm_r, 98._pm_r,  .32_pm_r,202._pm_r, & 
       244.90_pm_r, 60200._pm_r,   3._pm_r,228._pm_r,  .18_pm_r,207._pm_r,   2._pm_r,113._pm_r,  .38_pm_r,217._pm_r, & 
       232.40_pm_r, 63700._pm_r,   3._pm_r,224._pm_r,  .28_pm_r,182._pm_r,   2._pm_r,128._pm_r,  .34_pm_r,225._pm_r, & 
       221.20_pm_r, 67015._pm_r,   4._pm_r,219._pm_r,  .31_pm_r,178._pm_r,   2._pm_r,140._pm_r,  .18_pm_r,240._pm_r, & 
       214.00_pm_r, 70200._pm_r,   4._pm_r,215._pm_r,  .28_pm_r,184._pm_r,   2._pm_r,144._pm_r,  .07_pm_r,344._pm_r, & 
       210.80_pm_r, 73308._pm_r,   4._pm_r,213._pm_r,  .24_pm_r,191._pm_r,   2._pm_r,138._pm_r,  .23_pm_r, 28._pm_r, & 
       208.60_pm_r, 76379._pm_r,   5._pm_r,212._pm_r,  .22_pm_r,202._pm_r,   2._pm_r,122._pm_r,  .39_pm_r, 36._pm_r, & 
       206.30_pm_r, 79418._pm_r,   5._pm_r,211._pm_r,  .18_pm_r,207._pm_r,   2._pm_r,101._pm_r,  .49_pm_r, 37._pm_r, & 
       201.20_pm_r, 82424._pm_r,   5._pm_r,211._pm_r,  .16_pm_r,215._pm_r,   2._pm_r, 84._pm_r,  .52_pm_r, 40._pm_r, & 
       192.90_pm_r, 85298._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       186.60_pm_r, 88062._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       183.70_pm_r, 90777._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       183.50_pm_r, 93481._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       185.00_pm_r, 96208._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       189.20_pm_r, 98994._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       197.10_pm_r,101895._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       212.30_pm_r,105004._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       240.80_pm_r,108490._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       298.10_pm_r,112695._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       382.90_pm_r,118153._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)       

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_10 = (/ &      ! donnees latitude -10
       300.70_pm_r,   -77._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       276.70_pm_r,  4159._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       253.30_pm_r,  8046._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       226.00_pm_r, 11562._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       200.90_pm_r, 14693._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       197.80_pm_r, 17580._pm_r,   3._pm_r,168._pm_r,  .28_pm_r,296._pm_r,   1._pm_r, 20._pm_r,  .19_pm_r,103._pm_r, & 
       207.20_pm_r, 20545._pm_r,   3._pm_r,175._pm_r,  .34_pm_r,295._pm_r,   1._pm_r, 44._pm_r,  .23_pm_r,104._pm_r, & 
       215.50_pm_r, 23644._pm_r,   3._pm_r,185._pm_r,  .35_pm_r,293._pm_r,   1._pm_r, 62._pm_r,  .23_pm_r,104._pm_r, & 
       221.60_pm_r, 26846._pm_r,   3._pm_r,196._pm_r,  .32_pm_r,291._pm_r,   1._pm_r, 72._pm_r,  .20_pm_r,106._pm_r, & 
       228.70_pm_r, 30142._pm_r,   3._pm_r,205._pm_r,  .24_pm_r,289._pm_r,   2._pm_r, 77._pm_r,  .13_pm_r,108._pm_r, & 
       238.90_pm_r, 33563._pm_r,   3._pm_r,212._pm_r,  .15_pm_r,287._pm_r,   2._pm_r, 80._pm_r,  .04_pm_r,124._pm_r, & 
       248.80_pm_r, 37139._pm_r,   3._pm_r,215._pm_r,  .07_pm_r,290._pm_r,   2._pm_r, 80._pm_r,  .05_pm_r,273._pm_r, & 
       257.60_pm_r, 40845._pm_r,   3._pm_r,217._pm_r,  .03_pm_r,333._pm_r,   1._pm_r, 79._pm_r,  .12_pm_r,280._pm_r, & 
       264.10_pm_r, 44671._pm_r,   3._pm_r,216._pm_r,  .07_pm_r,142._pm_r,   1._pm_r, 74._pm_r,  .17_pm_r,289._pm_r, & 
       267.80_pm_r, 48570._pm_r,   3._pm_r,213._pm_r,  .15_pm_r,156._pm_r,   1._pm_r, 67._pm_r,  .20_pm_r,271._pm_r, & 
       265.70_pm_r, 52481._pm_r,   3._pm_r,210._pm_r,  .14_pm_r,206._pm_r,   1._pm_r, 69._pm_r,  .33_pm_r,226._pm_r, & 
       260.00_pm_r, 56336._pm_r,   3._pm_r,213._pm_r,  .31_pm_r,262._pm_r,   0._pm_r,141._pm_r,  .60_pm_r,203._pm_r, & 
       250.00_pm_r, 60074._pm_r,   3._pm_r,221._pm_r,  .42_pm_r,286._pm_r,   1._pm_r,180._pm_r,  .60_pm_r,191._pm_r, & 
       236.10_pm_r, 63639._pm_r,   4._pm_r,231._pm_r,  .40_pm_r,304._pm_r,   2._pm_r,181._pm_r,  .43_pm_r,172._pm_r, & 
       223.80_pm_r, 66999._pm_r,   4._pm_r,238._pm_r,  .28_pm_r,336._pm_r,   2._pm_r,175._pm_r,  .28_pm_r,114._pm_r, & 
       215.10_pm_r, 70212._pm_r,   4._pm_r,243._pm_r,  .30_pm_r, 35._pm_r,   2._pm_r,163._pm_r,  .50_pm_r, 61._pm_r, & 
       210.10_pm_r, 73322._pm_r,   3._pm_r,245._pm_r,  .48_pm_r, 62._pm_r,   2._pm_r,139._pm_r,  .82_pm_r, 46._pm_r, & 
       207.30_pm_r, 76378._pm_r,   2._pm_r,244._pm_r,  .67_pm_r, 73._pm_r,   3._pm_r,105._pm_r, 1.09_pm_r, 41._pm_r, & 
       205.90_pm_r, 79403._pm_r,   1._pm_r,232._pm_r,  .79_pm_r, 77._pm_r,   4._pm_r, 79._pm_r, 1.24_pm_r, 38._pm_r, & 
       202.10_pm_r, 82409._pm_r,   1._pm_r,140._pm_r,  .84_pm_r, 79._pm_r,   5._pm_r, 65._pm_r, 1.30_pm_r, 37._pm_r, & 
       195.00_pm_r, 85311._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       189.40_pm_r, 88115._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       186.80_pm_r, 90874._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       186.40_pm_r, 93625._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       186.90_pm_r, 96389._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       189.00_pm_r, 99189._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       194.30_pm_r,102069._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       206.70_pm_r,105116._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       232.70_pm_r,108496._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       290.10_pm_r,112571._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       377.70_pm_r,117929._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_0 = (/ &      ! donnees latitude 0
       300.80_pm_r,   -89._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       276.90_pm_r,  4149._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       253.60_pm_r,  8041._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       226.20_pm_r, 11560._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       200.40_pm_r, 14690._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       196.70_pm_r, 17568._pm_r,   2._pm_r,169._pm_r,  .14_pm_r,321._pm_r,   1._pm_r,358._pm_r,  .17_pm_r,100._pm_r, & 
       205.90_pm_r, 20518._pm_r,   2._pm_r,172._pm_r,  .16_pm_r,317._pm_r,   1._pm_r, 19._pm_r,  .21_pm_r,101._pm_r, & 
       214.80_pm_r, 23605._pm_r,   2._pm_r,178._pm_r,  .17_pm_r,311._pm_r,   1._pm_r, 39._pm_r,  .22_pm_r,100._pm_r, & 
       220.80_pm_r, 26797._pm_r,   2._pm_r,185._pm_r,  .15_pm_r,302._pm_r,   1._pm_r, 54._pm_r,  .20_pm_r,101._pm_r, & 
       227.50_pm_r, 30082._pm_r,   2._pm_r,193._pm_r,  .13_pm_r,284._pm_r,   1._pm_r, 62._pm_r,  .14_pm_r,107._pm_r, & 
       239.10_pm_r, 33495._pm_r,   2._pm_r,201._pm_r,  .10_pm_r,264._pm_r,   1._pm_r, 66._pm_r,  .06_pm_r,121._pm_r, & 
       249.50_pm_r, 37071._pm_r,   2._pm_r,206._pm_r,  .09_pm_r,257._pm_r,   1._pm_r, 68._pm_r,  .05_pm_r,229._pm_r, & 
       257.60_pm_r, 40780._pm_r,   2._pm_r,209._pm_r,  .06_pm_r,261._pm_r,   1._pm_r, 68._pm_r,  .10_pm_r,253._pm_r, & 
       263.60_pm_r, 44602._pm_r,   2._pm_r,210._pm_r,  .05_pm_r,248._pm_r,   1._pm_r, 67._pm_r,  .16_pm_r,232._pm_r, & 
       267.30_pm_r, 48495._pm_r,   2._pm_r,209._pm_r,  .07_pm_r,262._pm_r,   1._pm_r, 73._pm_r,  .32_pm_r,204._pm_r, & 
       267.40_pm_r, 52414._pm_r,   2._pm_r,208._pm_r,  .13_pm_r,261._pm_r,   0._pm_r,113._pm_r,  .49_pm_r,194._pm_r, & 
       261.70_pm_r, 56290._pm_r,   2._pm_r,210._pm_r,  .21_pm_r,247._pm_r,   1._pm_r,162._pm_r,  .52_pm_r,192._pm_r, & 
       250.70_pm_r, 60037._pm_r,   2._pm_r,215._pm_r,  .16_pm_r,261._pm_r,   1._pm_r,173._pm_r,  .30_pm_r,191._pm_r, & 
       237.90_pm_r, 63615._pm_r,   3._pm_r,222._pm_r,  .10_pm_r,336._pm_r,   2._pm_r,172._pm_r,  .10_pm_r,135._pm_r, & 
       225.00_pm_r, 66994._pm_r,   2._pm_r,231._pm_r,  .27_pm_r, 20._pm_r,   2._pm_r,163._pm_r,  .24_pm_r, 60._pm_r, & 
       215.30_pm_r, 70216._pm_r,   2._pm_r,246._pm_r,  .48_pm_r, 30._pm_r,   2._pm_r,148._pm_r,  .44_pm_r, 54._pm_r, & 
       210.40_pm_r, 73332._pm_r,   1._pm_r,277._pm_r,  .68_pm_r, 33._pm_r,   2._pm_r,127._pm_r,  .60_pm_r, 54._pm_r, & 
       207.70_pm_r, 76395._pm_r,   1._pm_r,328._pm_r,  .84_pm_r, 35._pm_r,   3._pm_r,109._pm_r,  .72_pm_r, 55._pm_r, & 
       206.00_pm_r, 79426._pm_r,   2._pm_r,358._pm_r,  .92_pm_r, 35._pm_r,   4._pm_r, 95._pm_r,  .79_pm_r, 55._pm_r, & 
       202.30_pm_r, 82434._pm_r,   4._pm_r, 10._pm_r,  .93_pm_r, 35._pm_r,   5._pm_r, 86._pm_r,  .80_pm_r, 55._pm_r, & 
       195.70_pm_r, 85347._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       190.10_pm_r, 88163._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       187.50_pm_r, 90932._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       187.30_pm_r, 93695._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       187.30_pm_r, 96470._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       188.30_pm_r, 99269._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       191.70_pm_r,102126._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       202.20_pm_r,105119._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       226.30_pm_r,108416._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       283.20_pm_r,112382._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       372.50_pm_r,117648._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_10 = (/ &      ! donnees latitude 10
       300.30_pm_r,   -75._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       277.00_pm_r,  4160._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       253.00_pm_r,  8047._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       225.70_pm_r, 11559._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       200.80_pm_r, 14687._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       197.30_pm_r, 17571._pm_r,   3._pm_r,163._pm_r,  .36_pm_r,344._pm_r,   1._pm_r,323._pm_r,  .18_pm_r, 88._pm_r, & 
       206.40_pm_r, 20525._pm_r,   3._pm_r,163._pm_r,  .47_pm_r,343._pm_r,   1._pm_r,338._pm_r,  .22_pm_r, 87._pm_r, & 
       215.20_pm_r, 23616._pm_r,   2._pm_r,164._pm_r,  .49_pm_r,341._pm_r,   1._pm_r,357._pm_r,  .19_pm_r, 90._pm_r, & 
       221.70_pm_r, 26815._pm_r,   1._pm_r,167._pm_r,  .44_pm_r,337._pm_r,   1._pm_r, 13._pm_r,  .13_pm_r, 97._pm_r, & 
       228.80_pm_r, 30113._pm_r,   1._pm_r,180._pm_r,  .34_pm_r,332._pm_r,   1._pm_r, 20._pm_r,  .05_pm_r,152._pm_r, & 
       238.70_pm_r, 33531._pm_r,   0._pm_r,227._pm_r,  .22_pm_r,322._pm_r,   1._pm_r, 19._pm_r,  .10_pm_r,231._pm_r, & 
       248.70_pm_r, 37103._pm_r,   0._pm_r,265._pm_r,  .12_pm_r,303._pm_r,   1._pm_r,  7._pm_r,  .18_pm_r,239._pm_r, & 
       257.90_pm_r, 40811._pm_r,   1._pm_r,272._pm_r,  .08_pm_r,278._pm_r,   1._pm_r,338._pm_r,  .23_pm_r,239._pm_r, & 
       264.50_pm_r, 44642._pm_r,   1._pm_r,267._pm_r,  .07_pm_r,192._pm_r,   1._pm_r,300._pm_r,  .24_pm_r,239._pm_r, & 
       268.00_pm_r, 48544._pm_r,   1._pm_r,252._pm_r,  .17_pm_r,137._pm_r,   1._pm_r,277._pm_r,  .25_pm_r,241._pm_r, & 
       266.00_pm_r, 52459._pm_r,   1._pm_r,219._pm_r,  .23_pm_r,121._pm_r,   1._pm_r,264._pm_r,  .24_pm_r,224._pm_r, & 
       258.90_pm_r, 56309._pm_r,   1._pm_r,182._pm_r,  .16_pm_r,110._pm_r,   1._pm_r,250._pm_r,  .30_pm_r,180._pm_r, & 
       248.00_pm_r, 60020._pm_r,   1._pm_r,171._pm_r,  .10_pm_r,356._pm_r,   1._pm_r,230._pm_r,  .37_pm_r,154._pm_r, & 
       235.50_pm_r, 63565._pm_r,   0._pm_r,187._pm_r,  .26_pm_r,329._pm_r,   2._pm_r,209._pm_r,  .39_pm_r,137._pm_r, & 
       223.80_pm_r, 66924._pm_r,   0._pm_r,299._pm_r,  .33_pm_r,330._pm_r,   2._pm_r,191._pm_r,  .34_pm_r,119._pm_r, & 
       215.50_pm_r, 70138._pm_r,   1._pm_r,320._pm_r,  .36_pm_r,334._pm_r,   2._pm_r,178._pm_r,  .27_pm_r, 94._pm_r, & 
       211.30_pm_r, 73259._pm_r,   1._pm_r,327._pm_r,  .38_pm_r,340._pm_r,   2._pm_r,166._pm_r,  .26_pm_r, 63._pm_r, & 
       208.70_pm_r, 76334._pm_r,   2._pm_r,332._pm_r,  .37_pm_r,344._pm_r,   2._pm_r,154._pm_r,  .31_pm_r, 40._pm_r, & 
       206.60_pm_r, 79375._pm_r,   2._pm_r,335._pm_r,  .37_pm_r,348._pm_r,   2._pm_r,138._pm_r,  .34_pm_r, 28._pm_r, & 
       202.90_pm_r, 82387._pm_r,   3._pm_r,337._pm_r,  .35_pm_r,349._pm_r,   1._pm_r,118._pm_r,  .36_pm_r, 22._pm_r, & 
       196.40_pm_r, 85316._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       190.40_pm_r, 88140._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       187.20_pm_r, 90905._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       187.00_pm_r, 93663._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       187.00_pm_r, 96433._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       187.40_pm_r, 99223._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       189.90_pm_r,102060._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       199.40_pm_r,105017._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       222.10_pm_r,108259._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       278.10_pm_r,112149._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       367.60_pm_r,117338._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_20 = (/ &      ! donnees latitude 20
       297.40_pm_r,   -76._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       274.50_pm_r,  4117._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       249.90_pm_r,  7961._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       224.70_pm_r, 11441._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       203.60_pm_r, 14581._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       200.70_pm_r, 17515._pm_r,   5._pm_r,164._pm_r,  .53_pm_r,344._pm_r,   2._pm_r,289._pm_r,  .45_pm_r, 76._pm_r, & 
       208.20_pm_r, 20507._pm_r,   4._pm_r,164._pm_r,  .81_pm_r,346._pm_r,   1._pm_r,308._pm_r,  .51_pm_r, 70._pm_r, & 
       215.80_pm_r, 23616._pm_r,   2._pm_r,161._pm_r, 1.01_pm_r,349._pm_r,   1._pm_r,341._pm_r,  .41_pm_r, 60._pm_r, & 
       221.90_pm_r, 26819._pm_r,   1._pm_r,146._pm_r, 1.15_pm_r,350._pm_r,   1._pm_r,359._pm_r,  .24_pm_r, 24._pm_r, & 
       228.90_pm_r, 30120._pm_r,   1._pm_r, 11._pm_r, 1.17_pm_r,351._pm_r,   2._pm_r,356._pm_r,  .27_pm_r,310._pm_r, & 
       238.20_pm_r, 33535._pm_r,   3._pm_r,359._pm_r, 1.06_pm_r,352._pm_r,   2._pm_r,342._pm_r,  .49_pm_r,278._pm_r, & 
       248.50_pm_r, 37102._pm_r,   4._pm_r,357._pm_r,  .83_pm_r,352._pm_r,   2._pm_r,322._pm_r,  .63_pm_r,262._pm_r, & 
       258.80_pm_r, 40814._pm_r,   5._pm_r,355._pm_r,  .55_pm_r,348._pm_r,   3._pm_r,304._pm_r,  .66_pm_r,248._pm_r, & 
       265.90_pm_r, 44662._pm_r,   6._pm_r,354._pm_r,  .26_pm_r,331._pm_r,   3._pm_r,290._pm_r,  .52_pm_r,226._pm_r, & 
       267.70_pm_r, 48575._pm_r,   6._pm_r,352._pm_r,  .14_pm_r,291._pm_r,   4._pm_r,279._pm_r,  .47_pm_r,205._pm_r, & 
       262.80_pm_r, 52465._pm_r,   6._pm_r,351._pm_r,  .10_pm_r,243._pm_r,   4._pm_r,269._pm_r,  .47_pm_r,187._pm_r, & 
       253.50_pm_r, 56251._pm_r,   6._pm_r,350._pm_r,  .11_pm_r,207._pm_r,   4._pm_r,258._pm_r,  .44_pm_r,174._pm_r, & 
       243.00_pm_r, 59883._pm_r,   6._pm_r,348._pm_r,  .18_pm_r,196._pm_r,   4._pm_r,248._pm_r,  .46_pm_r,155._pm_r, & 
       233.30_pm_r, 63374._pm_r,   5._pm_r,347._pm_r,  .18_pm_r,202._pm_r,   4._pm_r,238._pm_r,  .42_pm_r,138._pm_r, & 
       223.30_pm_r, 66714._pm_r,   5._pm_r,345._pm_r,  .13_pm_r,231._pm_r,   4._pm_r,230._pm_r,  .34_pm_r,118._pm_r, & 
       215.90_pm_r, 69929._pm_r,   5._pm_r,343._pm_r,  .13_pm_r,283._pm_r,   3._pm_r,223._pm_r,  .28_pm_r, 90._pm_r, & 
       212.70_pm_r, 73064._pm_r,   5._pm_r,341._pm_r,  .20_pm_r,311._pm_r,   3._pm_r,220._pm_r,  .27_pm_r, 61._pm_r, & 
       210.40_pm_r, 76162._pm_r,   6._pm_r,340._pm_r,  .27_pm_r,321._pm_r,   3._pm_r,218._pm_r,  .31_pm_r, 40._pm_r, & 
       207.60_pm_r, 79224._pm_r,   6._pm_r,339._pm_r,  .31_pm_r,325._pm_r,   2._pm_r,218._pm_r,  .35_pm_r, 31._pm_r, & 
       204.00_pm_r, 82245._pm_r,   7._pm_r,338._pm_r,  .33_pm_r,327._pm_r,   2._pm_r,221._pm_r,  .36_pm_r, 27._pm_r, & 
       198.30_pm_r, 85204._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       191.70_pm_r, 88056._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       187.10_pm_r, 90826._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       186.60_pm_r, 93580._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       186.60_pm_r, 96342._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       187.10_pm_r, 99126._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       189.60_pm_r,101956._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       198.60_pm_r,104904._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       220.40_pm_r,108126._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       275.00_pm_r,111978._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       363.20_pm_r,117109._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)     

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_30 = (/ &      ! donnees latitude 30
       291.70_pm_r,   -28._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       267.20_pm_r,  4067._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       242.40_pm_r,  7800._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       222.10_pm_r, 11204._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       209.40_pm_r, 14365._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       206.90_pm_r, 17399._pm_r,   6._pm_r,200._pm_r,  .61_pm_r,125._pm_r,   3._pm_r,290._pm_r,  .75_pm_r, 55._pm_r, & 
       210.90_pm_r, 20454._pm_r,   6._pm_r,191._pm_r,  .75_pm_r, 98._pm_r,   2._pm_r,316._pm_r,  .91_pm_r, 44._pm_r, & 
       217.00_pm_r, 23590._pm_r,   6._pm_r,179._pm_r, 1.04_pm_r, 61._pm_r,   3._pm_r,342._pm_r,  .89_pm_r, 25._pm_r, & 
       222.80_pm_r, 26807._pm_r,   5._pm_r,162._pm_r, 1.67_pm_r, 36._pm_r,   4._pm_r,351._pm_r,  .85_pm_r,353._pm_r, & 
       229.00_pm_r, 30117._pm_r,   3._pm_r,120._pm_r, 2.44_pm_r, 22._pm_r,   5._pm_r,347._pm_r, 1.00_pm_r,317._pm_r, & 
       236.10_pm_r, 33519._pm_r,   5._pm_r, 62._pm_r, 3.05_pm_r, 14._pm_r,   7._pm_r,337._pm_r, 1.28_pm_r,290._pm_r, & 
       245.90_pm_r, 37048._pm_r,   8._pm_r, 36._pm_r, 3.17_pm_r,  9._pm_r,   8._pm_r,324._pm_r, 1.45_pm_r,270._pm_r, & 
       256.90_pm_r, 40728._pm_r,  12._pm_r, 26._pm_r, 2.82_pm_r,  4._pm_r,   9._pm_r,312._pm_r, 1.50_pm_r,251._pm_r, & 
       264.30_pm_r, 44553._pm_r,  16._pm_r, 21._pm_r, 1.88_pm_r,359._pm_r,  10._pm_r,301._pm_r, 1.29_pm_r,232._pm_r, & 
       264.90_pm_r, 48436._pm_r,  18._pm_r, 18._pm_r,  .97_pm_r,348._pm_r,  10._pm_r,291._pm_r, 1.19_pm_r,213._pm_r, & 
       258.00_pm_r, 52269._pm_r,  18._pm_r, 16._pm_r,  .40_pm_r,276._pm_r,  11._pm_r,282._pm_r, 1.08_pm_r,195._pm_r, & 
       247.60_pm_r, 55976._pm_r,  17._pm_r, 14._pm_r,  .93_pm_r,222._pm_r,  11._pm_r,274._pm_r,  .90_pm_r,171._pm_r, & 
       238.30_pm_r, 59531._pm_r,  16._pm_r, 12._pm_r, 1.28_pm_r,217._pm_r,  10._pm_r,268._pm_r,  .92_pm_r,146._pm_r, & 
       230.50_pm_r, 62965._pm_r,  14._pm_r,  8._pm_r, 1.25_pm_r,220._pm_r,   9._pm_r,262._pm_r,  .87_pm_r,124._pm_r, & 
       224.00_pm_r, 66290._pm_r,  13._pm_r,  4._pm_r,  .97_pm_r,229._pm_r,   8._pm_r,257._pm_r,  .75_pm_r,100._pm_r, & 
       219.10_pm_r, 69534._pm_r,  12._pm_r,359._pm_r,  .67_pm_r,253._pm_r,   7._pm_r,256._pm_r,  .67_pm_r, 74._pm_r, & 
       216.50_pm_r, 72720._pm_r,  12._pm_r,355._pm_r,  .58_pm_r,290._pm_r,   6._pm_r,258._pm_r,  .67_pm_r, 50._pm_r, & 
       214.20_pm_r, 75875._pm_r,  13._pm_r,352._pm_r,  .70_pm_r,318._pm_r,   6._pm_r,265._pm_r,  .73_pm_r, 33._pm_r, & 
       211.70_pm_r, 78993._pm_r,  14._pm_r,350._pm_r,  .82_pm_r,331._pm_r,   5._pm_r,275._pm_r,  .78_pm_r, 23._pm_r, & 
       208.70_pm_r, 82076._pm_r,  15._pm_r,349._pm_r,  .87_pm_r,337._pm_r,   5._pm_r,289._pm_r,  .78_pm_r, 18._pm_r, & 
       203.00_pm_r, 85105._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       195.10_pm_r, 88018._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       188.50_pm_r, 90820._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       187.20_pm_r, 93586._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       187.00_pm_r, 96353._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       187.90_pm_r, 99144._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       191.00_pm_r,101988._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       200.30_pm_r,104958._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       221.50_pm_r,108200._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       274.30_pm_r,112059._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       359.30_pm_r,117146._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_40 = (/ &      ! donnees latitude 40
       284.40_pm_r,    -7._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       258.70_pm_r,  3969._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       234.00_pm_r,  7575._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       219.00_pm_r, 10891._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       215.10_pm_r, 14069._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       214.00_pm_r, 17210._pm_r,   7._pm_r,267._pm_r, 3.67_pm_r,167._pm_r,   4._pm_r, 13._pm_r,  .94_pm_r,349._pm_r, & 
       214.70_pm_r, 20346._pm_r,   8._pm_r,220._pm_r, 4.45_pm_r,161._pm_r,   5._pm_r,  4._pm_r, 1.42_pm_r,344._pm_r, & 
       217.90_pm_r, 23514._pm_r,  12._pm_r,192._pm_r, 4.11_pm_r,149._pm_r,   8._pm_r,357._pm_r, 1.84_pm_r,339._pm_r, & 
       222.20_pm_r, 26734._pm_r,  16._pm_r,177._pm_r, 3.16_pm_r,120._pm_r,  10._pm_r,351._pm_r, 2.10_pm_r,331._pm_r, & 
       227.80_pm_r, 30031._pm_r,  17._pm_r,162._pm_r, 3.38_pm_r, 69._pm_r,  13._pm_r,345._pm_r, 2.17_pm_r,320._pm_r, & 
       234.30_pm_r, 33411._pm_r,  16._pm_r,141._pm_r, 5.19_pm_r, 38._pm_r,  16._pm_r,339._pm_r, 2.03_pm_r,305._pm_r, & 
       242.50_pm_r, 36903._pm_r,  15._pm_r,108._pm_r, 6.70_pm_r, 25._pm_r,  18._pm_r,333._pm_r, 1.82_pm_r,283._pm_r, & 
       252.00_pm_r, 40520._pm_r,  19._pm_r, 75._pm_r, 7.10_pm_r, 17._pm_r,  19._pm_r,326._pm_r, 1.73_pm_r,256._pm_r, & 
       260.00_pm_r, 44277._pm_r,  25._pm_r, 56._pm_r, 5.28_pm_r, 12._pm_r,  20._pm_r,319._pm_r, 1.62_pm_r,230._pm_r, & 
       261.30_pm_r, 48104._pm_r,  29._pm_r, 47._pm_r, 3.21_pm_r,  4._pm_r,  20._pm_r,313._pm_r, 1.49_pm_r,211._pm_r, & 
       253.60_pm_r, 51879._pm_r,  31._pm_r, 42._pm_r, 1.34_pm_r,340._pm_r,  19._pm_r,307._pm_r, 1.20_pm_r,186._pm_r, & 
       242.00_pm_r, 55513._pm_r,  31._pm_r, 40._pm_r, 1.15_pm_r,253._pm_r,  18._pm_r,304._pm_r,  .99_pm_r,140._pm_r, & 
       233.40_pm_r, 58987._pm_r,  29._pm_r, 38._pm_r, 1.74_pm_r,238._pm_r,  16._pm_r,303._pm_r, 1.17_pm_r,122._pm_r, & 
       228.50_pm_r, 62368._pm_r,  27._pm_r, 36._pm_r, 1.86_pm_r,237._pm_r,  14._pm_r,304._pm_r, 1.08_pm_r,111._pm_r, & 
       224.70_pm_r, 65686._pm_r,  24._pm_r, 34._pm_r, 1.55_pm_r,243._pm_r,  13._pm_r,306._pm_r,  .80_pm_r,102._pm_r, & 
       221.30_pm_r, 68951._pm_r,  23._pm_r, 31._pm_r, 1.11_pm_r,259._pm_r,  12._pm_r,308._pm_r,  .44_pm_r, 84._pm_r, & 
       220.40_pm_r, 72184._pm_r,  22._pm_r, 28._pm_r,  .86_pm_r,289._pm_r,  12._pm_r,310._pm_r,  .21_pm_r, 28._pm_r, & 
       219.00_pm_r, 75404._pm_r,  22._pm_r, 25._pm_r,  .89_pm_r,320._pm_r,  12._pm_r,311._pm_r,  .34_pm_r,328._pm_r, & 
       216.70_pm_r, 78592._pm_r,  23._pm_r, 22._pm_r, 1.02_pm_r,336._pm_r,  13._pm_r,311._pm_r,  .49_pm_r,314._pm_r, & 
       214.10_pm_r, 81749._pm_r,  24._pm_r, 20._pm_r, 1.10_pm_r,345._pm_r,  14._pm_r,311._pm_r,  .58_pm_r,309._pm_r, & 
       208.90_pm_r, 84859._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       200.30_pm_r, 87862._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       192.00_pm_r, 90725._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       189.30_pm_r, 93529._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       188.70_pm_r, 96321._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       190.00_pm_r, 99138._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       194.20_pm_r,102019._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       204.00_pm_r,105040._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       225.60_pm_r,108339._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       274.70_pm_r,112235._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       355.70_pm_r,117281._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)       

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_50 = (/ &      ! donnees latitude 50
       278.10_pm_r,   -81._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       252.00_pm_r,  3796._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       228.60_pm_r,  7311._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       218.20_pm_r, 10579._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       217.90_pm_r, 13768._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       217.40_pm_r, 16957._pm_r,   6._pm_r,277._pm_r, 6.92_pm_r,182._pm_r,  13._pm_r, 40._pm_r, 2.42_pm_r,333._pm_r, & 
       216.80_pm_r, 20136._pm_r,  13._pm_r,207._pm_r, 9.06_pm_r,178._pm_r,  15._pm_r, 25._pm_r, 3.36_pm_r,330._pm_r, & 
       217.00_pm_r, 23309._pm_r,  25._pm_r,190._pm_r, 9.48_pm_r,171._pm_r,  18._pm_r, 11._pm_r, 3.85_pm_r,327._pm_r, & 
       220.40_pm_r, 26507._pm_r,  37._pm_r,182._pm_r, 8.08_pm_r,157._pm_r,  23._pm_r,  0._pm_r, 3.75_pm_r,321._pm_r, & 
       226.10_pm_r, 29778._pm_r,  46._pm_r,174._pm_r, 6.00_pm_r,127._pm_r,  26._pm_r,352._pm_r, 3.10_pm_r,309._pm_r, & 
       232.00_pm_r, 33130._pm_r,  49._pm_r,165._pm_r, 6.00_pm_r, 80._pm_r,  29._pm_r,346._pm_r, 2.30_pm_r,286._pm_r, & 
       239.20_pm_r, 36581._pm_r,  48._pm_r,153._pm_r, 8.02_pm_r, 49._pm_r,  30._pm_r,341._pm_r, 1.92_pm_r,249._pm_r, & 
       247.10_pm_r, 40141._pm_r,  45._pm_r,138._pm_r, 9.66_pm_r, 34._pm_r,  29._pm_r,335._pm_r, 2.17_pm_r,216._pm_r, & 
       253.40_pm_r, 43811._pm_r,  42._pm_r,120._pm_r, 8.74_pm_r, 23._pm_r,  27._pm_r,330._pm_r, 2.09_pm_r,211._pm_r, & 
       255.40_pm_r, 47543._pm_r,  41._pm_r,104._pm_r, 6.95_pm_r, 11._pm_r,  26._pm_r,324._pm_r, 2.02_pm_r,215._pm_r, & 
       250.70_pm_r, 51254._pm_r,  41._pm_r, 92._pm_r, 5.13_pm_r,351._pm_r,  25._pm_r,317._pm_r, 2.04_pm_r,213._pm_r, & 
       240.90_pm_r, 54860._pm_r,  38._pm_r, 83._pm_r, 4.07_pm_r,318._pm_r,  24._pm_r,311._pm_r, 2.10_pm_r,199._pm_r, & 
       232.80_pm_r, 58324._pm_r,  34._pm_r, 77._pm_r, 4.01_pm_r,292._pm_r,  23._pm_r,304._pm_r, 2.16_pm_r,177._pm_r, & 
       228.00_pm_r, 61696._pm_r,  29._pm_r, 72._pm_r, 4.03_pm_r,271._pm_r,  21._pm_r,298._pm_r, 2.03_pm_r,158._pm_r, & 
       226.00_pm_r, 65017._pm_r,  23._pm_r, 69._pm_r, 3.89_pm_r,254._pm_r,  18._pm_r,294._pm_r, 1.69_pm_r,142._pm_r, & 
       225.20_pm_r, 68321._pm_r,  18._pm_r, 70._pm_r, 3.62_pm_r,242._pm_r,  16._pm_r,291._pm_r, 1.33_pm_r,122._pm_r, & 
       224.60_pm_r, 71614._pm_r,  13._pm_r, 75._pm_r, 3.33_pm_r,231._pm_r,  15._pm_r,291._pm_r, 1.11_pm_r, 98._pm_r, & 
       223.90_pm_r, 74898._pm_r,   9._pm_r, 89._pm_r, 3.09_pm_r,223._pm_r,  13._pm_r,294._pm_r, 1.06_pm_r, 75._pm_r, & 
       222.50_pm_r, 78168._pm_r,   7._pm_r,118._pm_r, 2.84_pm_r,216._pm_r,  12._pm_r,300._pm_r, 1.10_pm_r, 59._pm_r, & 
       220.30_pm_r, 81417._pm_r,   8._pm_r,149._pm_r, 2.55_pm_r,211._pm_r,  12._pm_r,307._pm_r, 1.10_pm_r, 50._pm_r, & 
       215.10_pm_r, 84614._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       206.50_pm_r, 87718._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       197.20_pm_r, 90670._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       192.70_pm_r, 93534._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       191.70_pm_r, 96371._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       193.20_pm_r, 99230._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       198.60_pm_r,102167._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       209.30_pm_r,105259._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       230.90_pm_r,108638._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       275.00_pm_r,112581._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       352.00_pm_r,117578._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)    

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_60 = (/ &      ! donnees latitude 60
       267.00_pm_r,  -108._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       247.50_pm_r,  3652._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       225.20_pm_r,  7106._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       216.90_pm_r, 10336._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       217.00_pm_r, 13507._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       216.70_pm_r, 16684._pm_r,   2._pm_r,257._pm_r, 9.98_pm_r,188._pm_r,  21._pm_r, 40._pm_r, 3.63_pm_r,342._pm_r, & 
       215.60_pm_r, 19851._pm_r,  18._pm_r,193._pm_r,13.83_pm_r,184._pm_r,  25._pm_r, 27._pm_r, 4.90_pm_r,339._pm_r, & 
       213.20_pm_r, 22986._pm_r,  40._pm_r,187._pm_r,15.56_pm_r,178._pm_r,  30._pm_r, 16._pm_r, 5.29_pm_r,334._pm_r, & 
       215.60_pm_r, 26118._pm_r,  62._pm_r,182._pm_r,14.13_pm_r,169._pm_r,  35._pm_r,  7._pm_r, 4.50_pm_r,323._pm_r, & 
       223.30_pm_r, 29332._pm_r,  79._pm_r,178._pm_r,10.63_pm_r,153._pm_r,  39._pm_r,  1._pm_r, 3.11_pm_r,299._pm_r, & 
       230.80_pm_r, 32656._pm_r,  90._pm_r,173._pm_r, 7.70_pm_r,119._pm_r,  40._pm_r,355._pm_r, 2.52_pm_r,252._pm_r, & 
       238.50_pm_r, 36094._pm_r,  92._pm_r,166._pm_r, 7.77_pm_r, 75._pm_r,  37._pm_r,350._pm_r, 3.22_pm_r,213._pm_r, & 
       246.20_pm_r, 39642._pm_r,  90._pm_r,158._pm_r, 9.75_pm_r, 46._pm_r,  33._pm_r,345._pm_r, 3.93_pm_r,195._pm_r, & 
       250.80_pm_r, 43285._pm_r,  83._pm_r,150._pm_r,10.16_pm_r, 31._pm_r,  29._pm_r,340._pm_r, 3.16_pm_r,191._pm_r, & 
       253.80_pm_r, 46981._pm_r,  76._pm_r,141._pm_r, 9.45_pm_r, 17._pm_r,  25._pm_r,335._pm_r, 2.49_pm_r,199._pm_r, & 
       252.40_pm_r, 50695._pm_r,  68._pm_r,133._pm_r, 8.30_pm_r,  0._pm_r,  23._pm_r,328._pm_r, 2.45_pm_r,209._pm_r, & 
       244.80_pm_r, 54342._pm_r,  59._pm_r,126._pm_r, 7.30_pm_r,338._pm_r,  22._pm_r,319._pm_r, 2.85_pm_r,208._pm_r, & 
       237.90_pm_r, 57872._pm_r,  49._pm_r,121._pm_r, 7.01_pm_r,318._pm_r,  20._pm_r,309._pm_r, 2.68_pm_r,193._pm_r, & 
       232.60_pm_r, 61316._pm_r,  40._pm_r,120._pm_r, 6.61_pm_r,298._pm_r,  18._pm_r,299._pm_r, 2.30_pm_r,179._pm_r, & 
       229.50_pm_r, 64697._pm_r,  31._pm_r,123._pm_r, 5.96_pm_r,280._pm_r,  17._pm_r,291._pm_r, 1.77_pm_r,164._pm_r, & 
       228.10_pm_r, 68047._pm_r,  24._pm_r,133._pm_r, 5.31_pm_r,263._pm_r,  15._pm_r,285._pm_r, 1.28_pm_r,146._pm_r, & 
       227.10_pm_r, 71380._pm_r,  21._pm_r,150._pm_r, 4.83_pm_r,247._pm_r,  14._pm_r,282._pm_r,  .94_pm_r,121._pm_r, & 
       226.10_pm_r, 74699._pm_r,  22._pm_r,168._pm_r, 4.58_pm_r,233._pm_r,  12._pm_r,281._pm_r,  .85_pm_r, 93._pm_r, & 
       225.30_pm_r, 78006._pm_r,  26._pm_r,181._pm_r, 4.40_pm_r,223._pm_r,  11._pm_r,283._pm_r,  .88_pm_r, 74._pm_r, & 
       223.80_pm_r, 81299._pm_r,  31._pm_r,188._pm_r, 4.13_pm_r,215._pm_r,  10._pm_r,287._pm_r,  .92_pm_r, 62._pm_r, & 
       220.30_pm_r, 84555._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       213.00_pm_r, 87750._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       202.90_pm_r, 90797._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       197.20_pm_r, 93731._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       195.40_pm_r, 96625._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       197.30_pm_r, 99538._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       203.50_pm_r,102538._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       214.90_pm_r,105707._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       236.00_pm_r,109170._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       274.50_pm_r,113151._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       348.50_pm_r,118092._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)      

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_70 = (/ &      ! donnees latitude 70
       254.20_pm_r,  -106._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       243.80_pm_r,  3530._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       222.70_pm_r,  6936._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       215.20_pm_r, 10134._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       214.40_pm_r, 13271._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       214.10_pm_r, 16408._pm_r,   4._pm_r,  7._pm_r,10.52_pm_r,193._pm_r,  15._pm_r, 34._pm_r, 3.31_pm_r,  2._pm_r, & 
       212.50_pm_r, 19536._pm_r,  15._pm_r,193._pm_r,15.17_pm_r,189._pm_r,  20._pm_r, 25._pm_r, 4.44_pm_r,359._pm_r, & 
       207.20_pm_r, 22602._pm_r,  39._pm_r,189._pm_r,17.49_pm_r,184._pm_r,  26._pm_r, 18._pm_r, 4.55_pm_r,353._pm_r, & 
       208.80_pm_r, 25639._pm_r,  64._pm_r,185._pm_r,16.23_pm_r,175._pm_r,  31._pm_r, 12._pm_r, 3.47_pm_r,340._pm_r, & 
       218.00_pm_r, 28763._pm_r,  84._pm_r,181._pm_r,12.56_pm_r,162._pm_r,  34._pm_r,  7._pm_r, 2.09_pm_r,304._pm_r, & 
       228.10_pm_r, 32028._pm_r,  98._pm_r,177._pm_r, 8.86_pm_r,136._pm_r,  34._pm_r,  3._pm_r, 2.18_pm_r,244._pm_r, & 
       237.60_pm_r, 35442._pm_r, 104._pm_r,171._pm_r, 7.52_pm_r, 95._pm_r,  31._pm_r,358._pm_r, 3.17_pm_r,215._pm_r, & 
       245.40_pm_r, 38979._pm_r, 103._pm_r,165._pm_r, 8.74_pm_r, 61._pm_r,  27._pm_r,352._pm_r, 3.73_pm_r,202._pm_r, & 
       249.90_pm_r, 42610._pm_r,  98._pm_r,158._pm_r, 9.37_pm_r, 40._pm_r,  23._pm_r,347._pm_r, 2.82_pm_r,192._pm_r, & 
       253.40_pm_r, 46294._pm_r,  91._pm_r,151._pm_r, 9.15_pm_r, 22._pm_r,  19._pm_r,344._pm_r, 1.98_pm_r,178._pm_r, & 
       254.90_pm_r, 50020._pm_r,  82._pm_r,145._pm_r, 8.59_pm_r,  3._pm_r,  17._pm_r,343._pm_r, 1.65_pm_r,165._pm_r, & 
       251.80_pm_r, 53736._pm_r,  71._pm_r,141._pm_r, 8.29_pm_r,341._pm_r,  14._pm_r,343._pm_r, 1.70_pm_r,163._pm_r, & 
       246.30_pm_r, 57385._pm_r,  59._pm_r,138._pm_r, 8.25_pm_r,324._pm_r,  12._pm_r,343._pm_r, 1.48_pm_r,161._pm_r, & 
       239.10_pm_r, 60942._pm_r,  48._pm_r,138._pm_r, 7.74_pm_r,310._pm_r,  10._pm_r,344._pm_r, 1.11_pm_r,161._pm_r, & 
       232.80_pm_r, 64393._pm_r,  38._pm_r,143._pm_r, 6.67_pm_r,296._pm_r,   9._pm_r,344._pm_r,  .70_pm_r,164._pm_r, & 
       228.60_pm_r, 67771._pm_r,  31._pm_r,152._pm_r, 5.45_pm_r,281._pm_r,   8._pm_r,343._pm_r,  .39_pm_r,178._pm_r, & 
       226.40_pm_r, 71101._pm_r,  28._pm_r,165._pm_r, 4.41_pm_r,264._pm_r,   8._pm_r,342._pm_r,  .21_pm_r,214._pm_r, & 
       225.30_pm_r, 74409._pm_r,  28._pm_r,177._pm_r, 3.78_pm_r,246._pm_r,   8._pm_r,340._pm_r,  .23_pm_r,257._pm_r, & 
       224.70_pm_r, 77703._pm_r,  31._pm_r,186._pm_r, 3.49_pm_r,229._pm_r,   8._pm_r,337._pm_r,  .29_pm_r,278._pm_r, & 
       224.30_pm_r, 80990._pm_r,  35._pm_r,190._pm_r, 3.32_pm_r,217._pm_r,   8._pm_r,334._pm_r,  .33_pm_r,286._pm_r, & 
       223.10_pm_r, 84265._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       218.00_pm_r, 87517._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       208.00_pm_r, 90641._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       201.40_pm_r, 93642._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       199.00_pm_r, 96590._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       201.10_pm_r, 99554._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       207.90_pm_r,102614._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       219.90_pm_r,105853._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       239.80_pm_r,109385._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       273.10_pm_r,113387._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       345.40_pm_r,118272._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)     

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_80 = (/ &      ! donnees latitude 80
       248.50_pm_r,  -108._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       241.10_pm_r,  3466._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       220.90_pm_r,  6838._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       214.10_pm_r, 10013._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       212.30_pm_r, 13125._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       210.70_pm_r, 16221._pm_r,   5._pm_r, 26._pm_r, 6.11_pm_r,200._pm_r,   6._pm_r, 30._pm_r, 1.07_pm_r, 19._pm_r, & 
       207.80_pm_r, 19290._pm_r,   7._pm_r,192._pm_r, 9.24_pm_r,195._pm_r,   8._pm_r, 27._pm_r, 1.48_pm_r, 15._pm_r, & 
       201.60_pm_r, 22280._pm_r,  22._pm_r,191._pm_r,11.23_pm_r,188._pm_r,  10._pm_r, 23._pm_r, 1.53_pm_r,  7._pm_r, & 
       202.30_pm_r, 25227._pm_r,  38._pm_r,188._pm_r,10.82_pm_r,179._pm_r,  12._pm_r, 19._pm_r, 1.14_pm_r,350._pm_r, & 
       212.30_pm_r, 28260._pm_r,  52._pm_r,184._pm_r, 8.89_pm_r,166._pm_r,  13._pm_r, 15._pm_r,  .76_pm_r,309._pm_r, & 
       225.20_pm_r, 31459._pm_r,  63._pm_r,179._pm_r, 6.73_pm_r,146._pm_r,  13._pm_r, 10._pm_r,  .85_pm_r,260._pm_r, & 
       236.40_pm_r, 34845._pm_r,  69._pm_r,174._pm_r, 5.43_pm_r,116._pm_r,  12._pm_r,  5._pm_r, 1.09_pm_r,235._pm_r, & 
       244.90_pm_r, 38369._pm_r,  71._pm_r,168._pm_r, 5.24_pm_r, 84._pm_r,  11._pm_r,359._pm_r, 1.19_pm_r,220._pm_r, & 
       248.70_pm_r, 41988._pm_r,  71._pm_r,162._pm_r, 5.26_pm_r, 57._pm_r,  10._pm_r,356._pm_r,  .53_pm_r,157._pm_r, & 
       251.70_pm_r, 45650._pm_r,  68._pm_r,156._pm_r, 5.06_pm_r, 36._pm_r,  10._pm_r,  2._pm_r, 1.02_pm_r, 92._pm_r, & 
       255.10_pm_r, 49362._pm_r,  63._pm_r,152._pm_r, 4.88_pm_r, 11._pm_r,  10._pm_r, 11._pm_r, 1.11_pm_r, 85._pm_r, & 
       256.00_pm_r, 53108._pm_r,  57._pm_r,148._pm_r, 5.20_pm_r,346._pm_r,  10._pm_r, 18._pm_r,  .43_pm_r,121._pm_r, & 
       252.70_pm_r, 56840._pm_r,  49._pm_r,147._pm_r, 5.70_pm_r,332._pm_r,   9._pm_r, 18._pm_r,  .85_pm_r,218._pm_r, & 
       245.30_pm_r, 60491._pm_r,  41._pm_r,147._pm_r, 5.56_pm_r,321._pm_r,   8._pm_r, 13._pm_r, 1.41_pm_r,229._pm_r, & 
       237.40_pm_r, 64023._pm_r,  33._pm_r,149._pm_r, 4.73_pm_r,312._pm_r,   6._pm_r,  1._pm_r, 1.51_pm_r,232._pm_r, & 
       231.40_pm_r, 67455._pm_r,  28._pm_r,154._pm_r, 3.63_pm_r,299._pm_r,   5._pm_r,343._pm_r, 1.30_pm_r,232._pm_r, & 
       228.10_pm_r, 70818._pm_r,  24._pm_r,161._pm_r, 2.64_pm_r,281._pm_r,   5._pm_r,324._pm_r,  .98_pm_r,231._pm_r, & 
       226.40_pm_r, 74145._pm_r,  24._pm_r,169._pm_r, 2.08_pm_r,255._pm_r,   5._pm_r,310._pm_r,  .67_pm_r,228._pm_r, & 
       225.60_pm_r, 77453._pm_r,  25._pm_r,175._pm_r, 2.00_pm_r,229._pm_r,   5._pm_r,301._pm_r,  .42_pm_r,221._pm_r, & 
       225.40_pm_r, 80752._pm_r,  27._pm_r,180._pm_r, 2.09_pm_r,211._pm_r,   5._pm_r,296._pm_r,  .25_pm_r,209._pm_r, & 
       225.20_pm_r, 84047._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       221.30_pm_r, 87333._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       211.50_pm_r, 90510._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       204.30_pm_r, 93557._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       201.50_pm_r, 96544._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       203.90_pm_r, 99546._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       211.00_pm_r,102648._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       223.40_pm_r,105935._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       242.10_pm_r,109511._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       271.70_pm_r,113520._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       343.20_pm_r,118363._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)      

  real(pm_reel), dimension(10*pm_nb_lat*pm_nb_alt), parameter :: donnees_janvier = &      ! donnees toutes latitudes
       (/donnees_lat_moins_80(:),donnees_lat_moins_70(:),donnees_lat_moins_60(:),donnees_lat_moins_50(:),&
       donnees_lat_moins_40(:),donnees_lat_moins_30(:),donnees_lat_moins_20(:),donnees_lat_moins_10(:),&
       donnees_lat_0(:),donnees_lat_10(:),donnees_lat_20(:),donnees_lat_30(:),donnees_lat_40(:),donnees_lat_50(:),&
       donnees_lat_60(:),donnees_lat_70(:),donnees_lat_80(:)/)

real(pm_reel),dimension(10,pm_nb_alt,pm_nb_lat)::donnees

  character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
       '@(#) Fichier MSPRO mpi_atmi_janvier.f90: derniere modification V5.15 >'

  !************************************************************************

  ! initialisation de la valeur du code retour
  ! ..........................................
 donnees=reshape(donnees_janvier,(/10,pm_nb_alt,pm_nb_lat/))
  retour = pm_OK

  tbar(:,:) = donnees(1,:,:)
  zbar(:,:) = donnees(2,:,:)
  z1(:,:)   = donnees(3,:,:)
  phi1(:,:) = donnees(4,:,:)
  t1(:,:)   = donnees(5,:,:)
  phit1(:,:)= donnees(6,:,:)
  z2(:,:)   = donnees(7,:,:)
  phi2(:,:) = donnees(8,:,:)
  t2(:,:)   = donnees(9,:,:)
  phit2(:,:)= donnees(10,:,:)

end subroutine mpi_atmi_janvier
