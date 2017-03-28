subroutine mpi_atmi_fevrier (tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2, retour )

! (C) Copyright CNES - MSPRO - 2000

!************************************************************************
!
! But: Lecture du fichier ATMospherique pour l'Interplation correspondant au mois de FEVRIER 
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
integer, intent(out)                                           ::  retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Autres declarations
  ! -------------------

  real(pm_reel),dimension(10*pm_nb_alt),parameter :: donnees_lat_moins_80= (/ &
       266.40_pm_r, -212._pm_r,0._pm_r,0._pm_r, .00_pm_r, 0._pm_r,  0._pm_r, 0._pm_r, .00_pm_r, 0._pm_r,&
       248.00_pm_r, 3542._pm_r,0._pm_r,0._pm_r, .00_pm_r, 0._pm_r,  0._pm_r, 0._pm_r, .00_pm_r, 0._pm_r,&
       226.10_pm_r, 7002._pm_r,0._pm_r,0._pm_r, .00_pm_r, 0._pm_r,  0._pm_r, 0._pm_r, .00_pm_r, 0._pm_r,&
       226.90_pm_r,10309._pm_r,0._pm_r,0._pm_r, .00_pm_r, 0._pm_r,  0._pm_r, 0._pm_r, .00_pm_r, 0._pm_r,&
       230.20_pm_r,13645._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       231.40_pm_r,17026._pm_r,4._pm_r,217._pm_r,.38_pm_r,145._pm_r,2._pm_r,224._pm_r,.15_pm_r,18._pm_r, &
       232.30_pm_r,20422._pm_r,4._pm_r,209._pm_r,.47_pm_r,138._pm_r,1._pm_r,228._pm_r,.20_pm_r,18._pm_r, &
       232.80_pm_r,23830._pm_r,5._pm_r,201._pm_r,.49_pm_r,125._pm_r,1._pm_r,236._pm_r,.21_pm_r,22._pm_r, &
       234.00_pm_r,27243._pm_r,5._pm_r,193._pm_r,.42_pm_r,101._pm_r,1._pm_r,245._pm_r,.17_pm_r,31._pm_r, &
       241.90_pm_r,30722._pm_r,5._pm_r,186._pm_r,.39_pm_r,66._pm_r,1._pm_r,254._pm_r,.11_pm_r,49._pm_r, &
       252.30_pm_r,34338._pm_r,4._pm_r,180._pm_r,.43_pm_r,34._pm_r,1._pm_r,255._pm_r,.06_pm_r,108._pm_r, &
       262.60_pm_r,38112._pm_r,4._pm_r,177._pm_r,.45_pm_r,11._pm_r,1._pm_r,242._pm_r,.11_pm_r,162._pm_r, &
       272.10_pm_r,42026._pm_r,3._pm_r,175._pm_r,.41_pm_r,352._pm_r,1._pm_r,224._pm_r,.16_pm_r,176._pm_r, &
       278.20_pm_r,46062._pm_r,3._pm_r,177._pm_r,.20_pm_r,339._pm_r,1._pm_r,217._pm_r,.13_pm_r,225._pm_r, &
       278.60_pm_r,50146._pm_r,2._pm_r,177._pm_r,.09_pm_r,111._pm_r,1._pm_r,227._pm_r,.26_pm_r,281._pm_r, &
       272.40_pm_r,54185._pm_r,3._pm_r,172._pm_r,.34_pm_r,125._pm_r,1._pm_r,246._pm_r,.37_pm_r,288._pm_r, &
       263.60_pm_r,58114._pm_r,3._pm_r,163._pm_r,.50_pm_r,121._pm_r,2._pm_r,256._pm_r,.31_pm_r,270._pm_r, &
       253.00_pm_r,61896._pm_r,4._pm_r,157._pm_r,.40_pm_r,139._pm_r,2._pm_r,252._pm_r,.28_pm_r,196._pm_r, &
       240.50_pm_r,65516._pm_r,4._pm_r,156._pm_r,.30_pm_r,166._pm_r,2._pm_r,238._pm_r,.49_pm_r,165._pm_r, &
       225.50_pm_r,68925._pm_r,4._pm_r,159._pm_r,.25_pm_r,205._pm_r,3._pm_r,219._pm_r,.63_pm_r,152._pm_r, &
       211.20_pm_r,72126._pm_r,5._pm_r,163._pm_r,.27_pm_r,236._pm_r,3._pm_r,202._pm_r,.65_pm_r,146._pm_r, &
       198.00_pm_r,75118._pm_r,5._pm_r,167._pm_r,.28_pm_r,253._pm_r,4._pm_r,189._pm_r,.61_pm_r,142._pm_r, &
       185.50_pm_r,77930._pm_r,5._pm_r,173._pm_r,.29_pm_r,262._pm_r,4._pm_r,180._pm_r,.55_pm_r,139._pm_r, &
       175.00_pm_r,80562._pm_r,5._pm_r,178._pm_r,.27_pm_r,270._pm_r,5._pm_r,174._pm_r,.46_pm_r,135._pm_r, &
       163.30_pm_r,83051._pm_r,5._pm_r,182._pm_r,.24_pm_r,272._pm_r,5._pm_r,170._pm_r,.37_pm_r,133._pm_r, &
       151.70_pm_r,85304._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       146.80_pm_r,87448._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       147.80_pm_r,89596._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       153.60_pm_r,91806._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       164.40_pm_r,94147._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       181.60_pm_r,96704._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       207.20_pm_r,99607._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       243.80_pm_r,103017._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       282.40_pm_r,107076._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       312.50_pm_r,111733._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       392.90_pm_r,117308._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r /)

  real(pm_r),dimension(10*pm_nb_alt),parameter :: donnees_lat_moins_70= (/ &
       
       271.70_pm_r,-202._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       249.90_pm_r,3607._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       228.10_pm_r,7097._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       227.00_pm_r,10420._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       229.40_pm_r,13753._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       230.20_pm_r,17119._pm_r,5._pm_r,223._pm_r,.35_pm_r,104._pm_r,1._pm_r,237._pm_r,.17_pm_r,59._pm_r, &
       230.80_pm_r,20495._pm_r,5._pm_r,217._pm_r,.47_pm_r,95._pm_r,1._pm_r,236._pm_r,.25_pm_r,61._pm_r, &
       230.90_pm_r,23875._pm_r,4._pm_r,209._pm_r,.53_pm_r,80._pm_r,0._pm_r,227._pm_r,.30_pm_r,62._pm_r, &
       233.40_pm_r,27269._pm_r,4._pm_r,201._pm_r,.55_pm_r,61._pm_r,0._pm_r,74._pm_r,.30_pm_r,63._pm_r, &
       241.60_pm_r,30744._pm_r,3._pm_r,193._pm_r,.56_pm_r,38._pm_r,1._pm_r,68._pm_r,.28_pm_r,65._pm_r, &
       251.60_pm_r,34353._pm_r,2._pm_r,188._pm_r,.57_pm_r,16._pm_r,1._pm_r,67._pm_r,.22_pm_r,66._pm_r, &
       261.40_pm_r,38113._pm_r,1._pm_r,188._pm_r,.53_pm_r,357._pm_r,1._pm_r,67._pm_r,.16_pm_r,64._pm_r, &
       270.50_pm_r,42006._pm_r,1._pm_r,207._pm_r,.42_pm_r,335._pm_r,2._pm_r,66._pm_r,.11_pm_r,59._pm_r, &
       276.20_pm_r,46017._pm_r,1._pm_r,241._pm_r,.19_pm_r,293._pm_r,2._pm_r,64._pm_r,.07_pm_r,0._pm_r, &
       275.90_pm_r,50068._pm_r,1._pm_r,242._pm_r,.24_pm_r,205._pm_r,2._pm_r,60._pm_r,.12_pm_r,318._pm_r, &
       269.40_pm_r,54064._pm_r,1._pm_r,225._pm_r,.35_pm_r,175._pm_r,2._pm_r,53._pm_r,.15_pm_r,316._pm_r, &
       260.00_pm_r,57944._pm_r,2._pm_r,209._pm_r,.29_pm_r,145._pm_r,2._pm_r,46._pm_r,.12_pm_r,340._pm_r, &
       249.40_pm_r,61673._pm_r,2._pm_r,198._pm_r,.16_pm_r,103._pm_r,2._pm_r,43._pm_r,.11_pm_r,37._pm_r, &
       237.60_pm_r,65245._pm_r,2._pm_r,190._pm_r,.25_pm_r,38._pm_r,2._pm_r,44._pm_r,.14_pm_r,63._pm_r, &
       224.50_pm_r,68626._pm_r,1._pm_r,183._pm_r,.41_pm_r,19._pm_r,2._pm_r,46._pm_r,.13_pm_r,76._pm_r, &
       211.30_pm_r,71820._pm_r,0._pm_r,159._pm_r,.52_pm_r,13._pm_r,2._pm_r,49._pm_r,.11_pm_r,87._pm_r, &
       198.80_pm_r,74819._pm_r,1._pm_r,32._pm_r,.58_pm_r,10._pm_r,2._pm_r,51._pm_r,.07_pm_r,103._pm_r, &
       186.70_pm_r,77645._pm_r,1._pm_r,19._pm_r,.59_pm_r,9._pm_r,2._pm_r,53._pm_r,.05_pm_r,139._pm_r, &
       176.70_pm_r,80296._pm_r,2._pm_r,15._pm_r,.54_pm_r,8._pm_r,2._pm_r,54._pm_r,.03_pm_r,160._pm_r, &
       166.20_pm_r,82813._pm_r,3._pm_r,13._pm_r,.48_pm_r,8._pm_r,2._pm_r,55._pm_r,.04_pm_r,188._pm_r, &
       156.10_pm_r,85133._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       151.50_pm_r,87352._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       151.70_pm_r,89563._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       156.70_pm_r,91825._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       166.60_pm_r,94205._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       182.20_pm_r,96785._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       205.90_pm_r,99685._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       239.60_pm_r,103054._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       277.90_pm_r,107042._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       312.80_pm_r,111668._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       391.90_pm_r,117248._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r /)

  real(pm_r),dimension(10*pm_nb_alt),parameter :: donnees_lat_moins_60= (/ &
       
       277.00_pm_r,-138._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       254.90_pm_r,3749._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       232.10_pm_r,7308._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       225.50_pm_r,10652._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       225.90_pm_r,13950._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       226.70_pm_r,17264._pm_r,6._pm_r,220._pm_r,.71_pm_r,61._pm_r,0._pm_r,14._pm_r,.19_pm_r,104._pm_r, &
       227.80_pm_r,20593._pm_r,5._pm_r,216._pm_r,.93_pm_r,58._pm_r,1._pm_r,57._pm_r,.27_pm_r,103._pm_r, &
       228.40_pm_r,23931._pm_r,4._pm_r,209._pm_r,1.01_pm_r,52._pm_r,1._pm_r,78._pm_r,.32_pm_r,101._pm_r, &
       232.70_pm_r,27300._pm_r,3._pm_r,199._pm_r,.89_pm_r,42._pm_r,1._pm_r,86._pm_r,.32_pm_r,99._pm_r, &
       240.90_pm_r,30768._pm_r,2._pm_r,189._pm_r,.66_pm_r,25._pm_r,2._pm_r,89._pm_r,.29_pm_r,96._pm_r, &
       250.50_pm_r,34363._pm_r,1._pm_r,185._pm_r,.47_pm_r,354._pm_r,2._pm_r,90._pm_r,.22_pm_r,87._pm_r, &
       259.90_pm_r,38104._pm_r,0._pm_r,232._pm_r,.41_pm_r,314._pm_r,2._pm_r,89._pm_r,.16_pm_r,74._pm_r, &
       268.90_pm_r,41974._pm_r,1._pm_r,271._pm_r,.40_pm_r,282._pm_r,3._pm_r,87._pm_r,.11_pm_r,45._pm_r, &
       274.40_pm_r,45961._pm_r,1._pm_r,270._pm_r,.24_pm_r,254._pm_r,3._pm_r,84._pm_r,.12_pm_r,358._pm_r, &
       273.10_pm_r,49977._pm_r,2._pm_r,263._pm_r,.20_pm_r,195._pm_r,3._pm_r,80._pm_r,.13_pm_r,4._pm_r, &
       266.40_pm_r,53931._pm_r,2._pm_r,251._pm_r,.27_pm_r,163._pm_r,3._pm_r,76._pm_r,.15_pm_r,38._pm_r, &
       256.40_pm_r,57764._pm_r,2._pm_r,236._pm_r,.29_pm_r,155._pm_r,3._pm_r,75._pm_r,.21_pm_r,67._pm_r, &
       245.40_pm_r,61436._pm_r,2._pm_r,222._pm_r,.23_pm_r,130._pm_r,3._pm_r,74._pm_r,.18_pm_r,67._pm_r, &
       234.40_pm_r,64953._pm_r,2._pm_r,212._pm_r,.25_pm_r,76._pm_r,4._pm_r,73._pm_r,.13_pm_r,45._pm_r, &
       223.50_pm_r,68304._pm_r,1._pm_r,202._pm_r,.40_pm_r,44._pm_r,4._pm_r,71._pm_r,.13_pm_r,353._pm_r, &
       212.00_pm_r,71497._pm_r,1._pm_r,178._pm_r,.56_pm_r,33._pm_r,4._pm_r,67._pm_r,.21_pm_r,331._pm_r, &
       200.00_pm_r,74510._pm_r,1._pm_r,56._pm_r,.66_pm_r,27._pm_r,4._pm_r,62._pm_r,.26_pm_r,320._pm_r, &
       188.50_pm_r,77357._pm_r,2._pm_r,36._pm_r,.71_pm_r,24._pm_r,3._pm_r,55._pm_r,.29_pm_r,316._pm_r, &
       179.20_pm_r,80040._pm_r,3._pm_r,31._pm_r,.70_pm_r,22._pm_r,3._pm_r,48._pm_r,.29_pm_r,316._pm_r, &
       170.70_pm_r,82601._pm_r,4._pm_r,28._pm_r,.65_pm_r,22._pm_r,3._pm_r,41._pm_r,.28_pm_r,314._pm_r, &
       163.10_pm_r,85026._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       158.70_pm_r,87362._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       157.70_pm_r,89670._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       161.50_pm_r,92012._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       169.80_pm_r,94453._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       183.20_pm_r,97067._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       203.90_pm_r,99963._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       233.60_pm_r,103273._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       270.90_pm_r,107157._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       312.30_pm_r,111723._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       390.30_pm_r,117301._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r /)

  real(pm_r),dimension(10*pm_nb_alt),parameter :: donnees_lat_moins_50= (/ &
       
       282.30_pm_r,-27._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       262.70_pm_r,3959._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       238.60_pm_r,7625._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       223.40_pm_r,11004._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       219.40_pm_r,14242._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       219.70_pm_r,17455._pm_r,6._pm_r,208._pm_r,.74_pm_r,31._pm_r,2._pm_r,45._pm_r,.16_pm_r,174._pm_r, &
       222.20_pm_r,20689._pm_r,5._pm_r,208._pm_r,.95_pm_r,29._pm_r,1._pm_r,53._pm_r,.22_pm_r,175._pm_r, &
       225.50_pm_r,23967._pm_r,4._pm_r,208._pm_r,.99_pm_r,26._pm_r,1._pm_r,66._pm_r,.24_pm_r,175._pm_r, &
       230.60_pm_r,27303._pm_r,2._pm_r,210._pm_r,.83_pm_r,21._pm_r,1._pm_r,81._pm_r,.22_pm_r,175._pm_r, &
       239.00_pm_r,30741._pm_r,1._pm_r,220._pm_r,.54_pm_r,12._pm_r,1._pm_r,94._pm_r,.16_pm_r,173._pm_r, &
       248.60_pm_r,34309._pm_r,1._pm_r,239._pm_r,.24_pm_r,343._pm_r,1._pm_r,102._pm_r,.09_pm_r,164._pm_r, &
       258.30_pm_r,38024._pm_r,1._pm_r,249._pm_r,.18_pm_r,257._pm_r,1._pm_r,104._pm_r,.03_pm_r,111._pm_r, &
       267.20_pm_r,41871._pm_r,2._pm_r,246._pm_r,.28_pm_r,219._pm_r,1._pm_r,103._pm_r,.06_pm_r,45._pm_r, &
       272.30_pm_r,45830._pm_r,2._pm_r,237._pm_r,.24_pm_r,182._pm_r,2._pm_r,99._pm_r,.08_pm_r,55._pm_r, &
       271.30_pm_r,49819._pm_r,2._pm_r,226._pm_r,.31_pm_r,142._pm_r,2._pm_r,97._pm_r,.09_pm_r,66._pm_r, &
       263.00_pm_r,53734._pm_r,2._pm_r,211._pm_r,.34_pm_r,127._pm_r,2._pm_r,95._pm_r,.10_pm_r,76._pm_r, &
       252.10_pm_r,57509._pm_r,2._pm_r,199._pm_r,.21_pm_r,124._pm_r,2._pm_r,93._pm_r,.12_pm_r,70._pm_r, &
       240.90_pm_r,61116._pm_r,2._pm_r,193._pm_r,.08_pm_r,110._pm_r,2._pm_r,91._pm_r,.13_pm_r,61._pm_r, &
       230.20_pm_r,64568._pm_r,2._pm_r,191._pm_r,.06_pm_r,27._pm_r,2._pm_r,88._pm_r,.13_pm_r,53._pm_r, &
       221.10_pm_r,67869._pm_r,2._pm_r,191._pm_r,.08_pm_r,22._pm_r,2._pm_r,85._pm_r,.10_pm_r,39._pm_r, &
       212.60_pm_r,71048._pm_r,2._pm_r,190._pm_r,.09_pm_r,41._pm_r,2._pm_r,82._pm_r,.07_pm_r,16._pm_r, &
       204.20_pm_r,74099._pm_r,2._pm_r,186._pm_r,.10_pm_r,63._pm_r,2._pm_r,80._pm_r,.06_pm_r,342._pm_r, &
       195.60_pm_r,77029._pm_r,2._pm_r,181._pm_r,.12_pm_r,76._pm_r,2._pm_r,78._pm_r,.06_pm_r,325._pm_r, &
       188.00_pm_r,79831._pm_r,2._pm_r,174._pm_r,.13_pm_r,85._pm_r,2._pm_r,76._pm_r,.06_pm_r,304._pm_r, &
       180.40_pm_r,82530._pm_r,2._pm_r,167._pm_r,.14_pm_r,86._pm_r,2._pm_r,74._pm_r,.07_pm_r,297._pm_r, &
       172.80_pm_r,85105._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       167.50_pm_r,87579._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       165.10_pm_r,90008._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       167.40_pm_r,92450._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       173.70_pm_r,94965._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       184.50_pm_r,97622._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       201.70_pm_r,100514._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       226.60_pm_r,103759._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       262.20_pm_r,107519._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       310.00_pm_r,111996._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       388.40_pm_r,117557._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r /)

  real(pm_r),dimension(10*pm_nb_alt),parameter :: donnees_lat_moins_40= (/ &
       
       289.90_pm_r,9._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       270.00_pm_r,4108._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       244.90_pm_r,7877._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       223.30_pm_r,11304._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       212.70_pm_r,14496._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       211.80_pm_r,17593._pm_r,4._pm_r,194._pm_r,.30_pm_r,341._pm_r,2._pm_r,47._pm_r,.23_pm_r,189._pm_r, &
       216.40_pm_r,20726._pm_r,3._pm_r,199._pm_r,.37_pm_r,343._pm_r,1._pm_r,58._pm_r,.29_pm_r,190._pm_r, &
       222.60_pm_r,23943._pm_r,3._pm_r,206._pm_r,.37_pm_r,345._pm_r,1._pm_r,77._pm_r,.32_pm_r,191._pm_r, &
       228.20_pm_r,27243._pm_r,2._pm_r,214._pm_r,.30_pm_r,348._pm_r,1._pm_r,102._pm_r,.29_pm_r,192._pm_r, &
       235.70_pm_r,30640._pm_r,2._pm_r,221._pm_r,.19_pm_r,358._pm_r,1._pm_r,123._pm_r,.21_pm_r,191._pm_r, &
       245.50_pm_r,34159._pm_r,2._pm_r,224._pm_r,.07_pm_r,45._pm_r,1._pm_r,135._pm_r,.11_pm_r,185._pm_r, &
       256.30_pm_r,37837._pm_r,2._pm_r,221._pm_r,.10_pm_r,117._pm_r,1._pm_r,138._pm_r,.03_pm_r,141._pm_r, &
       266.00_pm_r,41661._pm_r,2._pm_r,215._pm_r,.15_pm_r,132._pm_r,1._pm_r,135._pm_r,.06_pm_r,51._pm_r, &
       270.40_pm_r,45597._pm_r,2._pm_r,208._pm_r,.17_pm_r,108._pm_r,1._pm_r,130._pm_r,.11_pm_r,68._pm_r, &
       268.70_pm_r,49552._pm_r,2._pm_r,198._pm_r,.28_pm_r,90._pm_r,1._pm_r,124._pm_r,.12_pm_r,85._pm_r, &
       259.80_pm_r,53425._pm_r,2._pm_r,184._pm_r,.29_pm_r,84._pm_r,2._pm_r,122._pm_r,.14_pm_r,129._pm_r, &
       249.40_pm_r,57157._pm_r,2._pm_r,172._pm_r,.12_pm_r,90._pm_r,2._pm_r,125._pm_r,.21_pm_r,161._pm_r, &
       238.60_pm_r,60729._pm_r,2._pm_r,174._pm_r,.17_pm_r,248._pm_r,2._pm_r,131._pm_r,.22_pm_r,164._pm_r, &
       227.90_pm_r,64147._pm_r,2._pm_r,187._pm_r,.35_pm_r,254._pm_r,2._pm_r,135._pm_r,.15_pm_r,152._pm_r, &
       219.70_pm_r,67420._pm_r,2._pm_r,201._pm_r,.41_pm_r,252._pm_r,2._pm_r,134._pm_r,.11_pm_r,98._pm_r, &
       213.40_pm_r,70592._pm_r,3._pm_r,212._pm_r,.40_pm_r,249._pm_r,3._pm_r,130._pm_r,.18_pm_r,52._pm_r, &
       208.20_pm_r,73677._pm_r,3._pm_r,218._pm_r,.35_pm_r,248._pm_r,3._pm_r,123._pm_r,.27_pm_r,40._pm_r, &
       202.80_pm_r,76688._pm_r,3._pm_r,222._pm_r,.29_pm_r,243._pm_r,3._pm_r,113._pm_r,.34_pm_r,34._pm_r, &
       197.30_pm_r,79616._pm_r,4._pm_r,224._pm_r,.25_pm_r,241._pm_r,3._pm_r,103._pm_r,.38_pm_r,33._pm_r, &
       190.60_pm_r,82466._pm_r,4._pm_r,225._pm_r,.21_pm_r,239._pm_r,3._pm_r,93._pm_r,.38_pm_r,30._pm_r, &
       182.50_pm_r,85189._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       176.20_pm_r,87801._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       172.80_pm_r,90353._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       173.70_pm_r,92901._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       177.90_pm_r,95496._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       185.80_pm_r,98199._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       199.40_pm_r,101087._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       219.90_pm_r,104266._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       252.70_pm_r,107900._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       305.60_pm_r,112263._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       385.90_pm_r,117785._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r /)

  real(pm_r),dimension(10*pm_nb_alt),parameter :: donnees_lat_moins_30= (/ &
       
       296.40_pm_r,-37._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       274.60_pm_r,4147._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       249.70_pm_r,7988._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       225.10_pm_r,11466._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       207.10_pm_r,14633._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       205.10_pm_r,17630._pm_r,1._pm_r,201._pm_r,.22_pm_r,266._pm_r,1._pm_r,3._pm_r,.22_pm_r,142._pm_r, &
       212.00_pm_r,20682._pm_r,2._pm_r,213._pm_r,.26_pm_r,265._pm_r,1._pm_r,31._pm_r,.28_pm_r,141._pm_r, &
       219.80_pm_r,23848._pm_r,2._pm_r,223._pm_r,.25_pm_r,262._pm_r,1._pm_r,75._pm_r,.30_pm_r,137._pm_r, &
       226.20_pm_r,27112._pm_r,2._pm_r,228._pm_r,.20_pm_r,257._pm_r,1._pm_r,100._pm_r,.28_pm_r,133._pm_r, &
       233.60_pm_r,30479._pm_r,2._pm_r,230._pm_r,.11_pm_r,242._pm_r,1._pm_r,109._pm_r,.22_pm_r,126._pm_r, &
       243.50_pm_r,33968._pm_r,2._pm_r,230._pm_r,.05_pm_r,174._pm_r,2._pm_r,111._pm_r,.16_pm_r,114._pm_r, &
       253.60_pm_r,37613._pm_r,2._pm_r,228._pm_r,.09_pm_r,114._pm_r,2._pm_r,111._pm_r,.10_pm_r,96._pm_r, &
       262.80_pm_r,41394._pm_r,2._pm_r,224._pm_r,.13_pm_r,99._pm_r,2._pm_r,109._pm_r,.07_pm_r,70._pm_r, &
       268.30_pm_r,45291._pm_r,2._pm_r,221._pm_r,.10_pm_r,119._pm_r,2._pm_r,107._pm_r,.06_pm_r,75._pm_r, &
       267.70_pm_r,49224._pm_r,2._pm_r,217._pm_r,.12_pm_r,145._pm_r,2._pm_r,106._pm_r,.03_pm_r,108._pm_r, &
       259.50_pm_r,53088._pm_r,2._pm_r,212._pm_r,.13_pm_r,133._pm_r,2._pm_r,107._pm_r,.05_pm_r,198._pm_r, &
       249.70_pm_r,56818._pm_r,2._pm_r,208._pm_r,.11_pm_r,87._pm_r,2._pm_r,110._pm_r,.10_pm_r,227._pm_r, &
       240.10_pm_r,60403._pm_r,2._pm_r,208._pm_r,.09_pm_r,349._pm_r,2._pm_r,114._pm_r,.07_pm_r,226._pm_r, &
       229.70_pm_r,63846._pm_r,2._pm_r,213._pm_r,.24_pm_r,291._pm_r,2._pm_r,116._pm_r,.01_pm_r,180._pm_r, &
       220.70_pm_r,67138._pm_r,2._pm_r,224._pm_r,.45_pm_r,267._pm_r,2._pm_r,114._pm_r,.10_pm_r,61._pm_r, &
       214.30_pm_r,70322._pm_r,3._pm_r,233._pm_r,.68_pm_r,257._pm_r,2._pm_r,109._pm_r,.21_pm_r,62._pm_r, &
       210.30_pm_r,73428._pm_r,4._pm_r,238._pm_r,.87_pm_r,251._pm_r,2._pm_r,102._pm_r,.30_pm_r,58._pm_r, &
       207.00_pm_r,76485._pm_r,6._pm_r,241._pm_r,1.03_pm_r,248._pm_r,3._pm_r,95._pm_r,.36_pm_r,59._pm_r, &
       203.40_pm_r,79491._pm_r,7._pm_r,242._pm_r,1.10_pm_r,246._pm_r,3._pm_r,89._pm_r,.39_pm_r,59._pm_r, &
       197.80_pm_r,82443._pm_r,9._pm_r,243._pm_r,1.10_pm_r,245._pm_r,4._pm_r,84._pm_r,.39_pm_r,59._pm_r, &
       189.80_pm_r,85274._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       183.40_pm_r,87994._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       179.70_pm_r,90653._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       179.50_pm_r,93296._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       181.80_pm_r,95966._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.10_pm_r,98711._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       197.10_pm_r,101595._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       213.90_pm_r,104714._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       243.50_pm_r,108230._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       299.30_pm_r,112467._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       382.80_pm_r,117929._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r /)

  real(pm_r),dimension(10*pm_nb_alt),parameter :: donnees_lat_moins_20= (/ &
       
       299.80_pm_r,-90._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       276.70_pm_r,4136._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       252.90_pm_r,8019._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       226.40_pm_r,11533._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       203.10_pm_r,14682._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       200.30_pm_r,17607._pm_r,1._pm_r,204._pm_r,.30_pm_r,266._pm_r,1._pm_r,327._pm_r,.21_pm_r,118._pm_r, &
       209.30_pm_r,20605._pm_r,1._pm_r,226._pm_r,.36_pm_r,270._pm_r,0._pm_r,6._pm_r,.27_pm_r,118._pm_r, &
       217.20_pm_r,23732._pm_r,2._pm_r,240._pm_r,.34_pm_r,274._pm_r,0._pm_r,80._pm_r,.29_pm_r,120._pm_r, &
       223.40_pm_r,26957._pm_r,2._pm_r,248._pm_r,.25_pm_r,281._pm_r,1._pm_r,101._pm_r,.28_pm_r,120._pm_r, &
       231.30_pm_r,30285._pm_r,2._pm_r,253._pm_r,.15_pm_r,301._pm_r,1._pm_r,108._pm_r,.23_pm_r,123._pm_r, &
       242.50_pm_r,33751._pm_r,2._pm_r,256._pm_r,.08_pm_r,7._pm_r,1._pm_r,111._pm_r,.15_pm_r,122._pm_r, &
       252.50_pm_r,37380._pm_r,2._pm_r,259._pm_r,.11_pm_r,54._pm_r,2._pm_r,112._pm_r,.07_pm_r,124._pm_r, &
       261.90_pm_r,41143._pm_r,2._pm_r,260._pm_r,.12_pm_r,76._pm_r,2._pm_r,113._pm_r,.01_pm_r,90._pm_r, &
       268.70_pm_r,45036._pm_r,2._pm_r,260._pm_r,.03_pm_r,270._pm_r,2._pm_r,112._pm_r,.03_pm_r,90._pm_r, &
       268.20_pm_r,48976._pm_r,2._pm_r,261._pm_r,.24_pm_r,282._pm_r,2._pm_r,112._pm_r,.03_pm_r,90._pm_r, &
       261.00_pm_r,52853._pm_r,3._pm_r,266._pm_r,.35_pm_r,289._pm_r,2._pm_r,112._pm_r,.08_pm_r,266._pm_r, &
       252.70_pm_r,56615._pm_r,3._pm_r,270._pm_r,.31_pm_r,303._pm_r,1._pm_r,117._pm_r,.28_pm_r,262._pm_r, &
       243.80_pm_r,60250._pm_r,3._pm_r,274._pm_r,.17_pm_r,304._pm_r,1._pm_r,131._pm_r,.35_pm_r,262._pm_r, &
       232.70_pm_r,63744._pm_r,3._pm_r,275._pm_r,.14_pm_r,274._pm_r,1._pm_r,156._pm_r,.25_pm_r,252._pm_r, &
       222.80_pm_r,67073._pm_r,4._pm_r,274._pm_r,.28_pm_r,258._pm_r,1._pm_r,168._pm_r,.09_pm_r,164._pm_r, &
       215.30_pm_r,70280._pm_r,4._pm_r,272._pm_r,.49_pm_r,258._pm_r,1._pm_r,158._pm_r,.35_pm_r,108._pm_r, &
       210.60_pm_r,73396._pm_r,5._pm_r,269._pm_r,.70_pm_r,258._pm_r,2._pm_r,137._pm_r,.62_pm_r,100._pm_r, &
       207.20_pm_r,76455._pm_r,6._pm_r,267._pm_r,.86_pm_r,259._pm_r,3._pm_r,122._pm_r,.85_pm_r,98._pm_r, &
       204.80_pm_r,79470._pm_r,8._pm_r,266._pm_r,.95_pm_r,260._pm_r,4._pm_r,114._pm_r,.99_pm_r,97._pm_r, &
       200.70_pm_r,82454._pm_r,9._pm_r,265._pm_r,.96_pm_r,260._pm_r,5._pm_r,109._pm_r,1.04_pm_r,97._pm_r, &
       193.90_pm_r,85339._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.90_pm_r,88126._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       184.50_pm_r,90856._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       183.90_pm_r,93570._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       184.90_pm_r,96299._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       188.10_pm_r,99077._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       195.00_pm_r,101954._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       208.70_pm_r,105020._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       235.30_pm_r,108435._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       292.20_pm_r,112548._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       379.30_pm_r,117935._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r /)

  real(pm_r),dimension(10*pm_nb_alt),parameter :: donnees_lat_moins_10= (/ &
       301.20_pm_r,-127._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       276.90_pm_r,4113._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       253.60_pm_r,8004._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       226.30_pm_r,11524._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       201.20_pm_r,14660._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       197.80_pm_r,17550._pm_r,1._pm_r,197._pm_r,.29_pm_r,291._pm_r,1._pm_r,260._pm_r,.26_pm_r,96._pm_r, &
       207.10_pm_r,20513._pm_r,1._pm_r,242._pm_r,.35_pm_r,288._pm_r,1._pm_r,247._pm_r,.33_pm_r,97._pm_r, &
       215.90_pm_r,23616._pm_r,1._pm_r,262._pm_r,.34_pm_r,287._pm_r,0._pm_r,184._pm_r,.35_pm_r,97._pm_r, &
       222.30_pm_r,26824._pm_r,2._pm_r,268._pm_r,.28_pm_r,281._pm_r,1._pm_r,126._pm_r,.32_pm_r,99._pm_r, &
       229.80_pm_r,30133._pm_r,2._pm_r,270._pm_r,.18_pm_r,268._pm_r,1._pm_r,115._pm_r,.24_pm_r,100._pm_r, &
       240.90_pm_r,33576._pm_r,2._pm_r,268._pm_r,.10_pm_r,233._pm_r,1._pm_r,112._pm_r,.13_pm_r,101._pm_r, &
       252.20_pm_r,37190._pm_r,2._pm_r,265._pm_r,.10_pm_r,182._pm_r,1._pm_r,111._pm_r,.02_pm_r,120._pm_r, &
       262.20_pm_r,40955._pm_r,2._pm_r,261._pm_r,.11_pm_r,165._pm_r,1._pm_r,112._pm_r,.08_pm_r,274._pm_r, &
       268.60_pm_r,44848._pm_r,2._pm_r,260._pm_r,.07_pm_r,301._pm_r,1._pm_r,114._pm_r,.10_pm_r,277._pm_r, &
       270.50_pm_r,48801._pm_r,2._pm_r,265._pm_r,.31_pm_r,316._pm_r,1._pm_r,117._pm_r,.11_pm_r,259._pm_r, &
       265.80_pm_r,52734._pm_r,3._pm_r,275._pm_r,.45_pm_r,318._pm_r,1._pm_r,127._pm_r,.19_pm_r,238._pm_r, &
       256.50_pm_r,56565._pm_r,3._pm_r,283._pm_r,.41_pm_r,319._pm_r,1._pm_r,149._pm_r,.31_pm_r,224._pm_r, &
       245.10_pm_r,60237._pm_r,4._pm_r,287._pm_r,.18_pm_r,319._pm_r,1._pm_r,172._pm_r,.31_pm_r,219._pm_r, &
       233.00_pm_r,63741._pm_r,4._pm_r,288._pm_r,.01_pm_r,315._pm_r,2._pm_r,181._pm_r,.22_pm_r,198._pm_r, &
       223.00_pm_r,67075._pm_r,4._pm_r,288._pm_r,.06_pm_r,135._pm_r,2._pm_r,179._pm_r,.18_pm_r,130._pm_r, &
       215.20_pm_r,70285._pm_r,4._pm_r,287._pm_r,.06_pm_r,135._pm_r,2._pm_r,168._pm_r,.37_pm_r,95._pm_r, &
       209.90_pm_r,73395._pm_r,4._pm_r,287._pm_r,.03_pm_r,138._pm_r,2._pm_r,149._pm_r,.56_pm_r,85._pm_r, &
       206.40_pm_r,76443._pm_r,3._pm_r,286._pm_r,.01_pm_r,90._pm_r,3._pm_r,130._pm_r,.72_pm_r,82._pm_r, &
       205.00_pm_r,79452._pm_r,4._pm_r,287._pm_r,.03_pm_r,326._pm_r,4._pm_r,115._pm_r,.82_pm_r,79._pm_r, &
       201.90_pm_r,82446._pm_r,4._pm_r,287._pm_r,.04_pm_r,340._pm_r,5._pm_r,106._pm_r,.86_pm_r,79._pm_r, &
       195.70_pm_r,85355._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       190.00_pm_r,88172._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.00_pm_r,90936._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.50_pm_r,93689._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.80_pm_r,96453._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       188.50_pm_r,99249._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       193.10_pm_r,102115._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       204.40_pm_r,105135._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       228.70_pm_r,108468._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       285.50_pm_r,112474._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       375.70_pm_r,117784._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r /)

  real(pm_r),dimension(10*pm_nb_alt),parameter::donnees_lat_0=(/&
       
       301.10_pm_r,-134._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       277.00_pm_r,4107._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       253.80_pm_r,8001._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       226.40_pm_r,11524._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       201.10_pm_r,14660._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       196.90_pm_r,17545._pm_r,1._pm_r,162._pm_r,.16_pm_r,307._pm_r,1._pm_r,267._pm_r,.25_pm_r,95._pm_r, &
       205.60_pm_r,20493._pm_r,1._pm_r,171._pm_r,.18_pm_r,304._pm_r,1._pm_r,262._pm_r,.33_pm_r,95._pm_r, &
       215.20_pm_r,23580._pm_r,1._pm_r,191._pm_r,.16_pm_r,292._pm_r,0._pm_r,177._pm_r,.34_pm_r,95._pm_r, &
       221.90_pm_r,26778._pm_r,1._pm_r,217._pm_r,.13_pm_r,274._pm_r,1._pm_r,106._pm_r,.30_pm_r,94._pm_r, &
       228.80_pm_r,30077._pm_r,1._pm_r,230._pm_r,.11_pm_r,236._pm_r,1._pm_r,102._pm_r,.21_pm_r,94._pm_r, &
       240.80_pm_r,33512._pm_r,1._pm_r,232._pm_r,.13_pm_r,209._pm_r,1._pm_r,101._pm_r,.09_pm_r,96._pm_r, &
       252.90_pm_r,37129._pm_r,1._pm_r,228._pm_r,.13_pm_r,198._pm_r,1._pm_r,102._pm_r,.02_pm_r,270._pm_r, &
       263.30_pm_r,40904._pm_r,1._pm_r,224._pm_r,.11_pm_r,202._pm_r,1._pm_r,105._pm_r,.11_pm_r,275._pm_r, &
       269.90_pm_r,44814._pm_r,2._pm_r,225._pm_r,.14_pm_r,264._pm_r,1._pm_r,109._pm_r,.18_pm_r,267._pm_r, &
       271.90_pm_r,48786._pm_r,2._pm_r,235._pm_r,.26_pm_r,288._pm_r,1._pm_r,119._pm_r,.19_pm_r,249._pm_r, &
       268.10_pm_r,52742._pm_r,2._pm_r,251._pm_r,.37_pm_r,300._pm_r,1._pm_r,138._pm_r,.18_pm_r,193._pm_r, &
       257.70_pm_r,56595._pm_r,2._pm_r,265._pm_r,.41_pm_r,324._pm_r,1._pm_r,143._pm_r,.38_pm_r,145._pm_r, &
       244.60_pm_r,60268._pm_r,2._pm_r,275._pm_r,.43_pm_r,1._pm_r,2._pm_r,138._pm_r,.51_pm_r,127._pm_r, &
       232.60_pm_r,63765._pm_r,2._pm_r,283._pm_r,.60_pm_r,32._pm_r,2._pm_r,134._pm_r,.54_pm_r,113._pm_r, &
       222.40_pm_r,67094._pm_r,2._pm_r,297._pm_r,.83_pm_r,47._pm_r,3._pm_r,130._pm_r,.52_pm_r,98._pm_r, &
       214.70_pm_r,70297._pm_r,2._pm_r,325._pm_r,1.05_pm_r,54._pm_r,4._pm_r,127._pm_r,.48_pm_r,80._pm_r, &
       209.90_pm_r,73408._pm_r,2._pm_r,3._pm_r,1.23_pm_r,59._pm_r,5._pm_r,124._pm_r,.47_pm_r,63._pm_r, &
       206.90_pm_r,76464._pm_r,3._pm_r,28._pm_r,1.37_pm_r,61._pm_r,5._pm_r,121._pm_r,.48_pm_r,50._pm_r, &
       205.70_pm_r,79485._pm_r,4._pm_r,40._pm_r,1.46_pm_r,63._pm_r,6._pm_r,119._pm_r,.50_pm_r,41._pm_r, &
       202.70_pm_r,82489._pm_r,5._pm_r,46._pm_r,1.46_pm_r,63._pm_r,6._pm_r,117._pm_r,.51_pm_r,36._pm_r, &
       196.50_pm_r,85412._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       190.60_pm_r,88238._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.60_pm_r,91010._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.30_pm_r,93773._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.30_pm_r,96548._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       188.20_pm_r,99346._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       191.40_pm_r,102199._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       201.30_pm_r,105184._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       224.00_pm_r,108457._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       280.40_pm_r,112385._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       372.10_pm_r,117629._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r),dimension(10*pm_nb_alt),parameter::donnees_lat_10=(/&
       
       300.20_pm_r,-101._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       277.20_pm_r,4134._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       253.30_pm_r,8025._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       226.20_pm_r,11542._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       201.30_pm_r,14678._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       197.60_pm_r,17567._pm_r,4._pm_r,161._pm_r,.42_pm_r,347._pm_r,1._pm_r,279._pm_r,.28_pm_r,77._pm_r, &
       206.40_pm_r,20523._pm_r,3._pm_r,159._pm_r,.51_pm_r,346._pm_r,0._pm_r,313._pm_r,.35_pm_r,76._pm_r, &
       215.00_pm_r,23612._pm_r,2._pm_r,157._pm_r,.45_pm_r,342._pm_r,0._pm_r,35._pm_r,.31_pm_r,75._pm_r, &
       222.20_pm_r,26811._pm_r,2._pm_r,157._pm_r,.30_pm_r,333._pm_r,1._pm_r,54._pm_r,.21_pm_r,73._pm_r, &
       229.70_pm_r,30120._pm_r,1._pm_r,161._pm_r,.15_pm_r,297._pm_r,1._pm_r,58._pm_r,.06_pm_r,69._pm_r, &
       240.90_pm_r,33560._pm_r,1._pm_r,170._pm_r,.18_pm_r,227._pm_r,1._pm_r,58._pm_r,.09_pm_r,260._pm_r, &
       252.70_pm_r,37178._pm_r,1._pm_r,180._pm_r,.27_pm_r,211._pm_r,1._pm_r,52._pm_r,.20_pm_r,253._pm_r, &
       263.60_pm_r,40957._pm_r,2._pm_r,187._pm_r,.29_pm_r,212._pm_r,0._pm_r,36._pm_r,.26_pm_r,251._pm_r, &
       270.50_pm_r,44875._pm_r,2._pm_r,194._pm_r,.26_pm_r,260._pm_r,0._pm_r,333._pm_r,.24_pm_r,243._pm_r, &
       271.90_pm_r,48853._pm_r,2._pm_r,206._pm_r,.37_pm_r,290._pm_r,0._pm_r,272._pm_r,.19_pm_r,217._pm_r, &
       266.90_pm_r,52804._pm_r,2._pm_r,220._pm_r,.34_pm_r,307._pm_r,0._pm_r,238._pm_r,.21_pm_r,158._pm_r, &
       256.00_pm_r,56641._pm_r,2._pm_r,230._pm_r,.16_pm_r,0._pm_r,1._pm_r,188._pm_r,.44_pm_r,124._pm_r, &
       243.00_pm_r,60291._pm_r,2._pm_r,230._pm_r,.28_pm_r,65._pm_r,1._pm_r,148._pm_r,.57_pm_r,115._pm_r, &
       231.40_pm_r,63767._pm_r,1._pm_r,223._pm_r,.41_pm_r,75._pm_r,2._pm_r,132._pm_r,.63_pm_r,110._pm_r, &
       221.70_pm_r,67080._pm_r,1._pm_r,203._pm_r,.44_pm_r,70._pm_r,3._pm_r,125._pm_r,.62_pm_r,110._pm_r, &
       215.10_pm_r,70277._pm_r,1._pm_r,160._pm_r,.42_pm_r,61._pm_r,4._pm_r,121._pm_r,.59_pm_r,110._pm_r, &
       211.60_pm_r,73398._pm_r,1._pm_r,110._pm_r,.40_pm_r,50._pm_r,4._pm_r,119._pm_r,.54_pm_r,112._pm_r, &
       208.80_pm_r,76477._pm_r,1._pm_r,83._pm_r,.38_pm_r,41._pm_r,5._pm_r,118._pm_r,.51_pm_r,113._pm_r, &
       206.70_pm_r,79517._pm_r,2._pm_r,68._pm_r,.38_pm_r,33._pm_r,6._pm_r,118._pm_r,.47_pm_r,113._pm_r, &
       203.20_pm_r,82529._pm_r,2._pm_r,58._pm_r,.36_pm_r,26._pm_r,7._pm_r,117._pm_r,.43_pm_r,114._pm_r, &
       196.90_pm_r,85463._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       190.80_pm_r,88294._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.20_pm_r,91062._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.90_pm_r,93820._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.90_pm_r,96589._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.50_pm_r,99378._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       190.30_pm_r,102218._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       199.60_pm_r,105181._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       221.70_pm_r,108421._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       277.60_pm_r,112307._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       368.90_pm_r,117504._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r),dimension(10*pm_nb_alt),parameter::donnees_lat_20=(/&
       297.10_pm_r,-101._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       274.50_pm_r,4090._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       250.30_pm_r,7937._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       225.50_pm_r,11426._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       203.90_pm_r,14574._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       200.80_pm_r,17510._pm_r,5._pm_r,161._pm_r,.56_pm_r,345._pm_r,2._pm_r,263._pm_r,.55_pm_r,76._pm_r, &
       208.40_pm_r,20506._pm_r,4._pm_r,160._pm_r,.74_pm_r,347._pm_r,1._pm_r,275._pm_r,.67_pm_r,72._pm_r, &
       215.30_pm_r,23611._pm_r,3._pm_r,158._pm_r,.73_pm_r,347._pm_r,0._pm_r,19._pm_r,.62_pm_r,64._pm_r, &
       222.10_pm_r,26810._pm_r,2._pm_r,152._pm_r,.61_pm_r,349._pm_r,1._pm_r,44._pm_r,.44_pm_r,49._pm_r, &
       229.90_pm_r,30119._pm_r,1._pm_r,138._pm_r,.44_pm_r,351._pm_r,2._pm_r,41._pm_r,.27_pm_r,5._pm_r, &
       241.10_pm_r,33563._pm_r,1._pm_r,113._pm_r,.23_pm_r,353._pm_r,2._pm_r,29._pm_r,.35_pm_r,303._pm_r, &
       252.40_pm_r,37181._pm_r,1._pm_r,92._pm_r,.09_pm_r,342._pm_r,2._pm_r,9._pm_r,.50_pm_r,277._pm_r, &
       263.10_pm_r,40953._pm_r,1._pm_r,83._pm_r,.07_pm_r,297._pm_r,2._pm_r,343._pm_r,.60_pm_r,261._pm_r, &
       270.20_pm_r,44867._pm_r,0._pm_r,52._pm_r,.28_pm_r,339._pm_r,2._pm_r,320._pm_r,.46_pm_r,236._pm_r, &
       270.40_pm_r,48833._pm_r,1._pm_r,18._pm_r,.48_pm_r,0._pm_r,2._pm_r,302._pm_r,.41_pm_r,203._pm_r, &
       263.20_pm_r,52745._pm_r,2._pm_r,14._pm_r,.47_pm_r,19._pm_r,2._pm_r,285._pm_r,.39_pm_r,169._pm_r, &
       251.20_pm_r,56519._pm_r,2._pm_r,20._pm_r,.28_pm_r,73._pm_r,1._pm_r,269._pm_r,.39_pm_r,130._pm_r, &
       239.00_pm_r,60102._pm_r,2._pm_r,28._pm_r,.29_pm_r,158._pm_r,1._pm_r,252._pm_r,.38_pm_r,110._pm_r, &
       229.60_pm_r,63534._pm_r,2._pm_r,38._pm_r,.45_pm_r,192._pm_r,1._pm_r,224._pm_r,.35_pm_r,95._pm_r, &
       222.30_pm_r,66840._pm_r,1._pm_r,50._pm_r,.52_pm_r,211._pm_r,0._pm_r,170._pm_r,.29_pm_r,81._pm_r, &
       217.00_pm_r,70055._pm_r,0._pm_r,89._pm_r,.54_pm_r,230._pm_r,1._pm_r,125._pm_r,.22_pm_r,68._pm_r, &
       214.30_pm_r,73210._pm_r,1._pm_r,227._pm_r,.56_pm_r,246._pm_r,1._pm_r,104._pm_r,.15_pm_r,48._pm_r, &
       211.70_pm_r,76331._pm_r,1._pm_r,242._pm_r,.60_pm_r,258._pm_r,1._pm_r,91._pm_r,.13_pm_r,18._pm_r, &
       208.60_pm_r,79410._pm_r,2._pm_r,250._pm_r,.62_pm_r,266._pm_r,1._pm_r,79._pm_r,.11_pm_r,360._pm_r, &
       204.40_pm_r,82443._pm_r,3._pm_r,255._pm_r,.62_pm_r,271._pm_r,1._pm_r,67._pm_r,.10_pm_r,343._pm_r, &
       198.10_pm_r,85397._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       191.40_pm_r,88245._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.80_pm_r,91013._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.10_pm_r,93760._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.10_pm_r,96516._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.00_pm_r,99294._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       190.20_pm_r,102127._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       199.70_pm_r,105088._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       221.90_pm_r,108328._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       277.00_pm_r,112212._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       366.20_pm_r,117380._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r),dimension(10*pm_nb_alt),parameter::donnees_lat_30=(/&
       
       291.20_pm_r,-38._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       267.20_pm_r,4053._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       242.30_pm_r,7786._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       222.70_pm_r,11193._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       209.60_pm_r,14360._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       207.10_pm_r,17396._pm_r,4._pm_r,194._pm_r,.19_pm_r,145._pm_r,3._pm_r,292._pm_r,.93_pm_r,79._pm_r, &
       211.10_pm_r,20456._pm_r,5._pm_r,191._pm_r,.26_pm_r,116._pm_r,2._pm_r,315._pm_r,1.17_pm_r,77._pm_r, &
       216.10_pm_r,23585._pm_r,5._pm_r,185._pm_r,.40_pm_r,83._pm_r,2._pm_r,2._pm_r,1.10_pm_r,74._pm_r, &
       222.20_pm_r,26789._pm_r,4._pm_r,176._pm_r,.63_pm_r,61._pm_r,3._pm_r,31._pm_r,.78_pm_r,68._pm_r, &
       229.40_pm_r,30098._pm_r,4._pm_r,161._pm_r,.92_pm_r,47._pm_r,4._pm_r,37._pm_r,.33_pm_r,44._pm_r, &
       238.60_pm_r,33520._pm_r,3._pm_r,136._pm_r,1.16_pm_r,39._pm_r,4._pm_r,34._pm_r,.34_pm_r,289._pm_r, &
       249.10_pm_r,37094._pm_r,3._pm_r,105._pm_r,1.27_pm_r,30._pm_r,3._pm_r,21._pm_r,.78_pm_r,267._pm_r, &
       259.80_pm_r,40818._pm_r,4._pm_r,79._pm_r,1.26_pm_r,21._pm_r,3._pm_r,357._pm_r,1.06_pm_r,257._pm_r, &
       266.90_pm_r,44683._pm_r,5._pm_r,62._pm_r,1.26_pm_r,17._pm_r,3._pm_r,328._pm_r,.95_pm_r,243._pm_r, &
       266.50_pm_r,48597._pm_r,7._pm_r,52._pm_r,1.23_pm_r,26._pm_r,3._pm_r,304._pm_r,.69_pm_r,222._pm_r, &
       259.20_pm_r,52450._pm_r,8._pm_r,49._pm_r,.98_pm_r,49._pm_r,3._pm_r,290._pm_r,.42_pm_r,165._pm_r, &
       247.70_pm_r,56168._pm_r,10._pm_r,51._pm_r,.74_pm_r,104._pm_r,2._pm_r,284._pm_r,.75_pm_r,105._pm_r, &
       236.80_pm_r,59711._pm_r,10._pm_r,57._pm_r,.74_pm_r,169._pm_r,1._pm_r,291._pm_r,.89_pm_r,95._pm_r, &
       228.70_pm_r,63120._pm_r,9._pm_r,63._pm_r,1.00_pm_r,206._pm_r,1._pm_r,47._pm_r,.81_pm_r,89._pm_r, &
       223.30_pm_r,66427._pm_r,7._pm_r,68._pm_r,1.16_pm_r,230._pm_r,1._pm_r,74._pm_r,.57_pm_r,80._pm_r, &
       219.50_pm_r,69668._pm_r,6._pm_r,70._pm_r,1.28_pm_r,250._pm_r,2._pm_r,74._pm_r,.30_pm_r,60._pm_r, &
       217.00_pm_r,72864._pm_r,4._pm_r,65._pm_r,1.40_pm_r,266._pm_r,2._pm_r,68._pm_r,.20_pm_r,345._pm_r, &
       214.10_pm_r,76023._pm_r,2._pm_r,36._pm_r,1.54_pm_r,277._pm_r,2._pm_r,58._pm_r,.35_pm_r,308._pm_r, &
       210.60_pm_r,79131._pm_r,2._pm_r,332._pm_r,1.60_pm_r,284._pm_r,2._pm_r,42._pm_r,.48_pm_r,298._pm_r, &
       206.60_pm_r,82190._pm_r,4._pm_r,309._pm_r,1.57_pm_r,288._pm_r,2._pm_r,19._pm_r,.54_pm_r,293._pm_r, &
       200.60_pm_r,85179._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       193.10_pm_r,88060._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.20_pm_r,90840._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       185.70_pm_r,93585._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       185.60_pm_r,96330._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.00_pm_r,99103._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       191.40_pm_r,101943._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       201.60_pm_r,104926._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       224.70_pm_r,108199._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       278.00_pm_r,112113._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       363.90_pm_r,117264._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r),dimension(10*pm_nb_alt),parameter::donnees_lat_40=(/&
       
       284.00_pm_r,-7._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       258.50_pm_r,3965._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       233.70_pm_r,7568._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       219.30_pm_r,10884._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       215.70_pm_r,14068._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       214.50_pm_r,17216._pm_r,7._pm_r,271._pm_r,2.09_pm_r,175._pm_r,5._pm_r,356._pm_r,.45_pm_r,62._pm_r, &
       214.90_pm_r,20358._pm_r,7._pm_r,241._pm_r,2.73_pm_r,167._pm_r,5._pm_r,4._pm_r,.61_pm_r,62._pm_r, &
       217.20_pm_r,23522._pm_r,9._pm_r,213._pm_r,2.91_pm_r,155._pm_r,6._pm_r,12._pm_r,.68_pm_r,63._pm_r, &
       220.60_pm_r,26725._pm_r,11._pm_r,193._pm_r,2.72_pm_r,135._pm_r,7._pm_r,18._pm_r,.60_pm_r,64._pm_r, &
       225.30_pm_r,29991._pm_r,13._pm_r,176._pm_r,2.65_pm_r,105._pm_r,7._pm_r,22._pm_r,.38_pm_r,68._pm_r, &
       233.70_pm_r,33345._pm_r,14._pm_r,159._pm_r,2.96_pm_r,75._pm_r,7._pm_r,24._pm_r,.04_pm_r,166._pm_r, &
       243.90_pm_r,36845._pm_r,14._pm_r,140._pm_r,3.35_pm_r,54._pm_r,7._pm_r,23._pm_r,.46_pm_r,238._pm_r, &
       254.20_pm_r,40490._pm_r,15._pm_r,120._pm_r,3.50_pm_r,38._pm_r,6._pm_r,18._pm_r,.88_pm_r,241._pm_r, &
       261.80_pm_r,44277._pm_r,16._pm_r,102._pm_r,2.90_pm_r,34._pm_r,5._pm_r,7._pm_r,1.12_pm_r,236._pm_r, &
       262.40_pm_r,48125._pm_r,17._pm_r,91._pm_r,2.05_pm_r,33._pm_r,4._pm_r,351._pm_r,.98_pm_r,226._pm_r, &
       254.80_pm_r,51916._pm_r,19._pm_r,85._pm_r,1.03_pm_r,37._pm_r,4._pm_r,340._pm_r,.61_pm_r,181._pm_r, &
       243.30_pm_r,55569._pm_r,19._pm_r,84._pm_r,.04_pm_r,153._pm_r,3._pm_r,347._pm_r,1.03_pm_r,113._pm_r, &
       234.10_pm_r,59059._pm_r,19._pm_r,84._pm_r,.86_pm_r,259._pm_r,2._pm_r,27._pm_r,1.17_pm_r,100._pm_r, &
       229.10_pm_r,62449._pm_r,17._pm_r,84._pm_r,1.49_pm_r,265._pm_r,3._pm_r,55._pm_r,1.07_pm_r,87._pm_r, &
       225.60_pm_r,65777._pm_r,14._pm_r,83._pm_r,1.85_pm_r,272._pm_r,5._pm_r,61._pm_r,.83_pm_r,67._pm_r, &
       222.60_pm_r,69059._pm_r,12._pm_r,80._pm_r,2.03_pm_r,278._pm_r,6._pm_r,60._pm_r,.70_pm_r,35._pm_r, &
       220.90_pm_r,72305._pm_r,9._pm_r,73._pm_r,2.11_pm_r,284._pm_r,7._pm_r,54._pm_r,.76_pm_r,4._pm_r, &
       218.20_pm_r,75522._pm_r,7._pm_r,58._pm_r,2.13_pm_r,289._pm_r,7._pm_r,45._pm_r,.91_pm_r,346._pm_r, &
       214.70_pm_r,78690._pm_r,5._pm_r,30._pm_r,2.09_pm_r,292._pm_r,8._pm_r,36._pm_r,1.00_pm_r,337._pm_r, &
       210.70_pm_r,81810._pm_r,6._pm_r,359._pm_r,1.95_pm_r,295._pm_r,9._pm_r,27._pm_r,1.03_pm_r,333._pm_r, &
       204.10_pm_r,84852._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       195.80_pm_r,87780._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       188.50_pm_r,90588._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       185.90_pm_r,93341._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       185.80_pm_r,96085._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.90_pm_r,98863._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       193.70_pm_r,101725._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       205.20_pm_r,104751._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       229.30_pm_r,108089._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       279.80_pm_r,112055._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       361.60_pm_r,117187._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r),dimension(10*pm_nb_alt),parameter::donnees_lat_50=(/&
       
       277.90_pm_r,-66._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       251.70_pm_r,3807._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       228.30_pm_r,7318._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       218.50_pm_r,10585._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       218.60_pm_r,13782._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       218.30_pm_r,16983._pm_r,10._pm_r,283._pm_r,4.42_pm_r,198._pm_r,12._pm_r,32._pm_r,1.46_pm_r,348._pm_r, &
       217.60_pm_r,20174._pm_r,13._pm_r,246._pm_r,5.88_pm_r,191._pm_r,14._pm_r,25._pm_r,1.73_pm_r,348._pm_r, &
       217.60_pm_r,23360._pm_r,19._pm_r,221._pm_r,6.47_pm_r,180._pm_r,16._pm_r,20._pm_r,1.50_pm_r,347._pm_r, &
       219.00_pm_r,26555._pm_r,26._pm_r,206._pm_r,6.29_pm_r,163._pm_r,17._pm_r,17._pm_r,.79_pm_r,339._pm_r, &
       221.80_pm_r,29784._pm_r,32._pm_r,192._pm_r,5.94_pm_r,137._pm_r,18._pm_r,15._pm_r,.46_pm_r,215._pm_r, &
       228.10_pm_r,33072._pm_r,36._pm_r,179._pm_r,6.05_pm_r,107._pm_r,16._pm_r,15._pm_r,1.54_pm_r,192._pm_r, &
       236.70_pm_r,36477._pm_r,37._pm_r,165._pm_r,6.39_pm_r,82._pm_r,13._pm_r,16._pm_r,2.33_pm_r,191._pm_r, &
       246.10_pm_r,40011._pm_r,38._pm_r,151._pm_r,6.38_pm_r,63._pm_r,10._pm_r,17._pm_r,2.58_pm_r,195._pm_r, &
       254.10_pm_r,43680._pm_r,39._pm_r,138._pm_r,5.37_pm_r,55._pm_r,6._pm_r,15._pm_r,2.11_pm_r,206._pm_r, &
       256.80_pm_r,47428._pm_r,40._pm_r,128._pm_r,4.02_pm_r,45._pm_r,4._pm_r,4._pm_r,1.41_pm_r,216._pm_r, &
       252.40_pm_r,51162._pm_r,40._pm_r,121._pm_r,3.00_pm_r,20._pm_r,3._pm_r,346._pm_r,.57_pm_r,212._pm_r, &
       243.70_pm_r,54800._pm_r,38._pm_r,115._pm_r,3.23_pm_r,344._pm_r,2._pm_r,346._pm_r,.50_pm_r,101._pm_r, &
       236.00_pm_r,58308._pm_r,33._pm_r,110._pm_r,4.53_pm_r,322._pm_r,2._pm_r,11._pm_r,.77_pm_r,70._pm_r, &
       231.50_pm_r,61728._pm_r,27._pm_r,103._pm_r,5.20_pm_r,310._pm_r,3._pm_r,29._pm_r,.93_pm_r,54._pm_r, &
       228.80_pm_r,65097._pm_r,20._pm_r,95._pm_r,5.09_pm_r,302._pm_r,5._pm_r,34._pm_r,.97_pm_r,40._pm_r, &
       226.60_pm_r,68432._pm_r,14._pm_r,83._pm_r,4.59_pm_r,294._pm_r,6._pm_r,34._pm_r,.99_pm_r,26._pm_r, &
       225.30_pm_r,71740._pm_r,9._pm_r,64._pm_r,4.00_pm_r,286._pm_r,7._pm_r,31._pm_r,1.00_pm_r,15._pm_r, &
       223.20_pm_r,75026._pm_r,6._pm_r,27._pm_r,3.49_pm_r,277._pm_r,9._pm_r,28._pm_r,1.01_pm_r,5._pm_r, &
       220.10_pm_r,78273._pm_r,6._pm_r,337._pm_r,3.06_pm_r,270._pm_r,10._pm_r,24._pm_r,.99_pm_r,359._pm_r, &
       215.70_pm_r,81475._pm_r,8._pm_r,308._pm_r,2.65_pm_r,264._pm_r,11._pm_r,21._pm_r,.93_pm_r,354._pm_r, &
       208.20_pm_r,84581._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       199.00_pm_r,87565._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       190.60_pm_r,90411._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.80_pm_r,93183._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.70_pm_r,95939._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       189.60_pm_r,98733._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       196.90_pm_r,101630._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       210.00_pm_r,104714._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       234.80_pm_r,108130._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       281.10_pm_r,112154._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       359.20_pm_r,117256._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r),dimension(10*pm_nb_alt),parameter::donnees_lat_60=(/&
       
       266.90_pm_r,-72._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       247.50_pm_r,3687._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       225.20_pm_r,7141._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       217.40_pm_r,10375._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       217.80_pm_r,13555._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       217.80_pm_r,16745._pm_r,8._pm_r,257._pm_r,6.74_pm_r,215._pm_r,16._pm_r,35._pm_r,3.21_pm_r,6._pm_r, &
       217.10_pm_r,19931._pm_r,18._pm_r,230._pm_r,9.07_pm_r,209._pm_r,20._pm_r,27._pm_r,4.03_pm_r,4._pm_r, &
       215.90_pm_r,23099._pm_r,32._pm_r,219._pm_r,9.90_pm_r,199._pm_r,26._pm_r,22._pm_r,3.71_pm_r,0._pm_r, &
       217.10_pm_r,26267._pm_r,45._pm_r,211._pm_r,9.12_pm_r,183._pm_r,30._pm_r,18._pm_r,2.17_pm_r,346._pm_r, &
       221.50_pm_r,29478._pm_r,54._pm_r,202._pm_r,7.75_pm_r,155._pm_r,31._pm_r,15._pm_r,1.23_pm_r,253._pm_r, &
       227.60_pm_r,32763._pm_r,60._pm_r,193._pm_r,7.69_pm_r,120._pm_r,28._pm_r,12._pm_r,3.23_pm_r,211._pm_r, &
       234.50_pm_r,36149._pm_r,61._pm_r,182._pm_r,8.57_pm_r,93._pm_r,22._pm_r,9._pm_r,4.72_pm_r,203._pm_r, &
       241.60_pm_r,39633._pm_r,61._pm_r,170._pm_r,8.95_pm_r,75._pm_r,15._pm_r,3._pm_r,5.11_pm_r,198._pm_r, &
       248.00_pm_r,43222._pm_r,60._pm_r,158._pm_r,7.88_pm_r,64._pm_r,9._pm_r,354._pm_r,3.56_pm_r,194._pm_r, &
       252.50_pm_r,46888._pm_r,59._pm_r,148._pm_r,6.57_pm_r,46._pm_r,6._pm_r,341._pm_r,1.72_pm_r,186._pm_r, &
       253.00_pm_r,50595._pm_r,55._pm_r,140._pm_r,6.26_pm_r,18._pm_r,4._pm_r,336._pm_r,.36_pm_r,150._pm_r, &
       249.20_pm_r,54277._pm_r,49._pm_r,131._pm_r,7.37_pm_r,353._pm_r,4._pm_r,340._pm_r,.41_pm_r,25._pm_r, &
       243.00_pm_r,57880._pm_r,40._pm_r,123._pm_r,8.32_pm_r,334._pm_r,5._pm_r,345._pm_r,.71_pm_r,5._pm_r, &
       237.50_pm_r,61398._pm_r,29._pm_r,113._pm_r,8.50_pm_r,320._pm_r,6._pm_r,348._pm_r,.90_pm_r,358._pm_r, &
       233.90_pm_r,64848._pm_r,18._pm_r,98._pm_r,7.84_pm_r,310._pm_r,7._pm_r,350._pm_r,.96_pm_r,357._pm_r, &
       231.40_pm_r,68255._pm_r,10._pm_r,69._pm_r,6.83_pm_r,300._pm_r,9._pm_r,351._pm_r,.91_pm_r,357._pm_r, &
       229.00_pm_r,71627._pm_r,7._pm_r,6._pm_r,5.82_pm_r,289._pm_r,10._pm_r,352._pm_r,.82_pm_r,2._pm_r, &
       226.70_pm_r,74964._pm_r,12._pm_r,323._pm_r,5.05_pm_r,277._pm_r,11._pm_r,353._pm_r,.73_pm_r,5._pm_r, &
       223.70_pm_r,78264._pm_r,17._pm_r,304._pm_r,4.47_pm_r,266._pm_r,12._pm_r,354._pm_r,.63_pm_r,10._pm_r, &
       219.20_pm_r,81519._pm_r,22._pm_r,293._pm_r,3.96_pm_r,257._pm_r,13._pm_r,356._pm_r,.53_pm_r,14._pm_r, &
       211.70_pm_r,84673._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       202.30_pm_r,87709._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       193.00_pm_r,90598._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       188.40_pm_r,93395._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       188.20_pm_r,96171._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       191.90_pm_r,98988._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       200.50_pm_r,101926._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       215.20_pm_r,105073._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       239.90_pm_r,108569._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       281.40_pm_r,112636._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       356.60_pm_r,117697._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r),dimension(10*pm_nb_alt),parameter::donnees_lat_70=(/&
       
       254.10_pm_r,-32._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       244.10_pm_r,3606._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       222.90_pm_r,7016._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       215.50_pm_r,10217._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       214.80_pm_r,13359._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       214.60_pm_r,16503._pm_r,4._pm_r,230._pm_r,7.65_pm_r,219._pm_r,8._pm_r,15._pm_r,3.50_pm_r,35._pm_r, &
       214.10_pm_r,19645._pm_r,17._pm_r,220._pm_r,10.92_pm_r,214._pm_r,14._pm_r,23._pm_r,4.73_pm_r,33._pm_r, &
       212.20_pm_r,22762._pm_r,34._pm_r,215._pm_r,12.06_pm_r,207._pm_r,21._pm_r,26._pm_r,4.62_pm_r,27._pm_r, &
       215.00_pm_r,25883._pm_r,50._pm_r,211._pm_r,10.23_pm_r,195._pm_r,27._pm_r,25._pm_r,2.73_pm_r,12._pm_r, &
       222.20_pm_r,29085._pm_r,62._pm_r,206._pm_r,7.32_pm_r,172._pm_r,28._pm_r,22._pm_r,1.44_pm_r,292._pm_r, &
       228.60_pm_r,32385._pm_r,68._pm_r,200._pm_r,6.08_pm_r,130._pm_r,27._pm_r,16._pm_r,3.15_pm_r,242._pm_r, &
       233.70_pm_r,35773._pm_r,69._pm_r,192._pm_r,7.20_pm_r,94._pm_r,23._pm_r,7._pm_r,4.46_pm_r,229._pm_r, &
       238.40_pm_r,39228._pm_r,66._pm_r,182._pm_r,8.45_pm_r,74._pm_r,18._pm_r,354._pm_r,4.73_pm_r,220._pm_r, &
       243.40_pm_r,42757._pm_r,62._pm_r,172._pm_r,8.13_pm_r,67._pm_r,14._pm_r,338._pm_r,3.13_pm_r,208._pm_r, &
       249.50_pm_r,46365._pm_r,60._pm_r,162._pm_r,7.07_pm_r,55._pm_r,11._pm_r,327._pm_r,1.58_pm_r,185._pm_r, &
       255.00_pm_r,50063._pm_r,56._pm_r,153._pm_r,6.37_pm_r,31._pm_r,10._pm_r,324._pm_r,.65_pm_r,141._pm_r, &
       257.60_pm_r,53820._pm_r,49._pm_r,145._pm_r,7.10_pm_r,1._pm_r,10._pm_r,325._pm_r,.22_pm_r,41._pm_r, &
       253.30_pm_r,57565._pm_r,39._pm_r,139._pm_r,8.10_pm_r,339._pm_r,10._pm_r,327._pm_r,.45_pm_r,10._pm_r, &
       246.10_pm_r,61224._pm_r,28._pm_r,133._pm_r,8.50_pm_r,325._pm_r,11._pm_r,330._pm_r,.59_pm_r,2._pm_r, &
       239.10_pm_r,64774._pm_r,16._pm_r,127._pm_r,7.96_pm_r,316._pm_r,11._pm_r,332._pm_r,.58_pm_r,0._pm_r, &
       233.10_pm_r,68232._pm_r,5._pm_r,116._pm_r,6.87_pm_r,307._pm_r,12._pm_r,334._pm_r,.49_pm_r,2._pm_r, &
       228.60_pm_r,71610._pm_r,4._pm_r,310._pm_r,5.65_pm_r,298._pm_r,13._pm_r,335._pm_r,.36_pm_r,9._pm_r, &
       225.80_pm_r,74936._pm_r,12._pm_r,299._pm_r,4.63_pm_r,287._pm_r,13._pm_r,337._pm_r,.24_pm_r,26._pm_r, &
       223.30_pm_r,78226._pm_r,18._pm_r,293._pm_r,3.88_pm_r,276._pm_r,13._pm_r,338._pm_r,.18_pm_r,52._pm_r, &
       219.60_pm_r,81478._pm_r,23._pm_r,288._pm_r,3.30_pm_r,265._pm_r,13._pm_r,339._pm_r,.16_pm_r,79._pm_r, &
       213.50_pm_r,84647._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       204.80_pm_r,87720._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       195.10_pm_r,90644._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       190.10_pm_r,93466._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       189.70_pm_r,96263._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       194.20_pm_r,99106._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       203.70_pm_r,102083._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       219.60_pm_r,105285._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       243.70_pm_r,108844._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       280.50_pm_r,112936._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       354.20_pm_r,117951._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r),dimension(10*pm_nb_alt),parameter::donnees_lat_80=(/&
       
       247.40_pm_r,35._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       241.50_pm_r,3604._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       220.80_pm_r,6978._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       213.60_pm_r,10149._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       211.20_pm_r,13249._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       210.20_pm_r,16332._pm_r,2._pm_r,242._pm_r,4.43_pm_r,219._pm_r,5._pm_r,345._pm_r,1.54_pm_r,73._pm_r, &
       209.60_pm_r,19407._pm_r,10._pm_r,221._pm_r,7.06_pm_r,214._pm_r,6._pm_r,13._pm_r,2.03_pm_r,72._pm_r, &
       208.30_pm_r,22462._pm_r,22._pm_r,215._pm_r,8.54_pm_r,207._pm_r,8._pm_r,32._pm_r,1.65_pm_r,69._pm_r, &
       211.40_pm_r,25527._pm_r,33._pm_r,211._pm_r,7.43_pm_r,197._pm_r,9._pm_r,37._pm_r,.33_pm_r,43._pm_r, &
       219.90_pm_r,28685._pm_r,42._pm_r,206._pm_r,5.36_pm_r,180._pm_r,9._pm_r,33._pm_r,1.09_pm_r,259._pm_r, &
       227.00_pm_r,31958._pm_r,48._pm_r,201._pm_r,3.79_pm_r,149._pm_r,7._pm_r,20._pm_r,2.06_pm_r,255._pm_r, &
       231.80_pm_r,35320._pm_r,49._pm_r,195._pm_r,3.55_pm_r,108._pm_r,6._pm_r,351._pm_r,2.48_pm_r,252._pm_r, &
       235.60_pm_r,38741._pm_r,48._pm_r,189._pm_r,4.21_pm_r,80._pm_r,6._pm_r,315._pm_r,2.36_pm_r,248._pm_r, &
       240.20_pm_r,42223._pm_r,46._pm_r,182._pm_r,3.84_pm_r,74._pm_r,8._pm_r,295._pm_r,1.46_pm_r,239._pm_r, &
       247.80_pm_r,45791._pm_r,45._pm_r,176._pm_r,3.07_pm_r,64._pm_r,8._pm_r,285._pm_r,.65_pm_r,221._pm_r, &
       257.30_pm_r,49492._pm_r,43._pm_r,172._pm_r,2.60_pm_r,38._pm_r,8._pm_r,282._pm_r,.12_pm_r,147._pm_r, &
       265.20_pm_r,53319._pm_r,39._pm_r,169._pm_r,3.10_pm_r,7._pm_r,8._pm_r,284._pm_r,.36_pm_r,36._pm_r, &
       262.80_pm_r,57194._pm_r,34._pm_r,167._pm_r,4.07_pm_r,351._pm_r,8._pm_r,288._pm_r,.65_pm_r,43._pm_r, &
       254.90_pm_r,60991._pm_r,28._pm_r,167._pm_r,4.59_pm_r,343._pm_r,8._pm_r,296._pm_r,.76_pm_r,46._pm_r, &
       245.40_pm_r,64652._pm_r,21._pm_r,170._pm_r,4.56_pm_r,337._pm_r,7._pm_r,304._pm_r,.69_pm_r,46._pm_r, &
       236.40_pm_r,68182._pm_r,15._pm_r,176._pm_r,4.25_pm_r,333._pm_r,7._pm_r,311._pm_r,.50_pm_r,44._pm_r, &
       229.80_pm_r,71592._pm_r,10._pm_r,192._pm_r,3.81_pm_r,328._pm_r,7._pm_r,316._pm_r,.30_pm_r,39._pm_r, &
       225.90_pm_r,74928._pm_r,7._pm_r,224._pm_r,3.41_pm_r,324._pm_r,7._pm_r,318._pm_r,.10_pm_r,6._pm_r, &
       223.10_pm_r,78214._pm_r,8._pm_r,260._pm_r,3.04_pm_r,320._pm_r,7._pm_r,318._pm_r,.11_pm_r,270._pm_r, &
       219.60_pm_r,81461._pm_r,11._pm_r,279._pm_r,2.67_pm_r,317._pm_r,7._pm_r,316._pm_r,.23_pm_r,247._pm_r, &
       214.30_pm_r,84635._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       206.40_pm_r,87727._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       196.50_pm_r,90674._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       191.30_pm_r,93514._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       190.90_pm_r,96327._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       195.80_pm_r,99190._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       206.00_pm_r,102194._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       222.50_pm_r,105435._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       245.90_pm_r,109033._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       279.40_pm_r,113134._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       352.50_pm_r,118114._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_reel),dimension(10*pm_nb_lat*pm_nb_alt),parameter::donnees_fevrier=&
       (/donnees_lat_moins_80(:),donnees_lat_moins_70(:),donnees_lat_moins_60(:),donnees_lat_moins_50(:),&
       donnees_lat_moins_40(:),donnees_lat_moins_30(:),donnees_lat_moins_20(:),donnees_lat_moins_10(:),&
       donnees_lat_0(:),donnees_lat_10(:),donnees_lat_20(:),donnees_lat_30(:),donnees_lat_40(:),donnees_lat_50(:),&
       donnees_lat_60(:),donnees_lat_70(:),donnees_lat_80(:)/)

real(pm_reel),dimension(10,pm_nb_alt,pm_nb_lat)::donnees

  character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur=&
       '@(#) Fichier MSPRO mpi_atmi_fevrier.f90: derniere modification V5.15 >'

  !************************************************************************

  !initialisation de la valeur du code retour
  !..........................................
donnees=reshape(donnees_fevrier,(/10,pm_nb_alt,pm_nb_lat/))
  retour=pm_OK

  tbar(:,:)=donnees(1,:,:)
  zbar(:,:)=donnees(2,:,:)
  z1(:,:)=donnees(3,:,:)
  phi1(:,:)=donnees(4,:,:)
  t1(:,:)=donnees(5,:,:)
  phit1(:,:)=donnees(6,:,:)
  z2(:,:)=donnees(7,:,:)
  phi2(:,:)=donnees(8,:,:)
  t2(:,:)=donnees(9,:,:)
  phit2(:,:)=donnees(10,:,:)

end subroutine mpi_atmi_fevrier
