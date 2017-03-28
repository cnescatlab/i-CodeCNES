subroutine mpi_atmi_decembre (tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2, retour )

! (C) Copyright CNES - MSPRO - 2000

!************************************************************************
!
! But: Lecture du fichier ATMospherique pour l'Interplation correspondant au mois de DECEMBRE 
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

  real(pm_reel), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_80= (/ &
       264.10_pm_r,-113._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       249.20_pm_r,3633._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       226.50_pm_r,7105._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       223.90_pm_r,10392._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       230.10_pm_r,13706._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       234.20_pm_r,17109._pm_r,6._pm_r,198._pm_r,.11_pm_r,267._pm_r,0._pm_r,178._pm_r,.04_pm_r,360._pm_r, &
       236.30_pm_r,20556._pm_r,6._pm_r,200._pm_r,.07_pm_r,297._pm_r,0._pm_r,179._pm_r,.05_pm_r,349._pm_r, &
       236.40_pm_r,24019._pm_r,5._pm_r,200._pm_r,.11_pm_r,41._pm_r,0._pm_r,186._pm_r,.05_pm_r,315._pm_r, &
       237.70_pm_r,27485._pm_r,5._pm_r,198._pm_r,.32_pm_r,58._pm_r,0._pm_r,201._pm_r,.06_pm_r,270._pm_r, &
       249.90_pm_r,31047._pm_r,5._pm_r,193._pm_r,.53_pm_r,62._pm_r,0._pm_r,214._pm_r,.09_pm_r,238._pm_r, &
       263.10_pm_r,34805._pm_r,4._pm_r,183._pm_r,.69_pm_r,64._pm_r,1._pm_r,219._pm_r,.15_pm_r,222._pm_r, &
       274.80_pm_r,38749._pm_r,4._pm_r,169._pm_r,.73_pm_r,64._pm_r,1._pm_r,218._pm_r,.18_pm_r,213._pm_r, &
       284.40_pm_r,42844._pm_r,4._pm_r,154._pm_r,.65_pm_r,61._pm_r,1._pm_r,216._pm_r,.21_pm_r,208._pm_r, &
       288.90_pm_r,47049._pm_r,4._pm_r,142._pm_r,.36_pm_r,46._pm_r,1._pm_r,213._pm_r,.26_pm_r,201._pm_r, &
       287.50_pm_r,51278._pm_r,4._pm_r,137._pm_r,.18_pm_r,351._pm_r,2._pm_r,210._pm_r,.35_pm_r,198._pm_r, &
       279.30_pm_r,55431._pm_r,3._pm_r,136._pm_r,.22_pm_r,312._pm_r,2._pm_r,207._pm_r,.37_pm_r,199._pm_r, &
       267.10_pm_r,59438._pm_r,3._pm_r,136._pm_r,.20_pm_r,336._pm_r,3._pm_r,206._pm_r,.27_pm_r,202._pm_r, &
       251.30_pm_r,63231._pm_r,3._pm_r,133._pm_r,.16_pm_r,18._pm_r,3._pm_r,205._pm_r,.10_pm_r,169._pm_r, &
       236.10_pm_r,66803._pm_r,3._pm_r,128._pm_r,.16_pm_r,55._pm_r,3._pm_r,202._pm_r,.16_pm_r,94._pm_r, &
       221.80_pm_r,70153._pm_r,3._pm_r,125._pm_r,.14_pm_r,100._pm_r,3._pm_r,196._pm_r,.31_pm_r,86._pm_r, &
       206.20_pm_r,73295._pm_r,3._pm_r,124._pm_r,.18_pm_r,139._pm_r,3._pm_r,186._pm_r,.39_pm_r,89._pm_r, &
       189.90_pm_r,76190._pm_r,3._pm_r,127._pm_r,.21_pm_r,159._pm_r,3._pm_r,174._pm_r,.42_pm_r,95._pm_r, &
       174.00_pm_r,78858._pm_r,4._pm_r,130._pm_r,.23_pm_r,170._pm_r,3._pm_r,163._pm_r,.41_pm_r,100._pm_r, &
       160.30_pm_r,81295._pm_r,4._pm_r,133._pm_r,.22_pm_r,175._pm_r,3._pm_r,155._pm_r,.36_pm_r,101._pm_r, &
       147.10_pm_r,83547._pm_r,4._pm_r,137._pm_r,.20_pm_r,180._pm_r,4._pm_r,149._pm_r,.30_pm_r,104._pm_r, &
       137.20_pm_r,85569._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       135.70_pm_r,87527._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       140.10_pm_r,89537._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       149.20_pm_r,91655._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       163.60_pm_r,93953._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       185.70_pm_r,96534._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       219.40_pm_r,99557._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       263.40_pm_r,103213._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       297.30_pm_r,107553._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       321.50_pm_r,112376._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       412.90_pm_r,118213._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_70= (/ &
       269.90_pm_r,-139._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       251.10_pm_r,3665._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       228.00_pm_r,7164._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       223.30_pm_r,10459._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       228.50_pm_r,13758._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       232.10_pm_r,17134._pm_r,10._pm_r,194._pm_r,.32_pm_r,300._pm_r,1._pm_r,216._pm_r,.05_pm_r,155._pm_r, &
       234.30_pm_r,20551._pm_r,10._pm_r,197._pm_r,.35_pm_r,315._pm_r,1._pm_r,207._pm_r,.08_pm_r,148._pm_r, &
       235.10_pm_r,23988._pm_r,9._pm_r,199._pm_r,.33_pm_r,350._pm_r,1._pm_r,197._pm_r,.10_pm_r,148._pm_r, &
       238.00_pm_r,27445._pm_r,9._pm_r,199._pm_r,.45_pm_r,34._pm_r,1._pm_r,188._pm_r,.11_pm_r,144._pm_r, &
       248.90_pm_r,31005._pm_r,8._pm_r,196._pm_r,.70_pm_r,56._pm_r,1._pm_r,181._pm_r,.11_pm_r,141._pm_r, &
       261.30_pm_r,34740._pm_r,7._pm_r,189._pm_r,.92_pm_r,66._pm_r,1._pm_r,175._pm_r,.10_pm_r,137._pm_r, &
       272.70_pm_r,38654._pm_r,7._pm_r,178._pm_r,.98_pm_r,69._pm_r,1._pm_r,170._pm_r,.08_pm_r,128._pm_r, &
       281.90_pm_r,42716._pm_r,6._pm_r,166._pm_r,.86_pm_r,70._pm_r,1._pm_r,167._pm_r,.05_pm_r,114._pm_r, &
       285.70_pm_r,46880._pm_r,6._pm_r,157._pm_r,.49_pm_r,54._pm_r,1._pm_r,165._pm_r,.02_pm_r,113._pm_r, &
       283.60_pm_r,51057._pm_r,6._pm_r,153._pm_r,.23_pm_r,4._pm_r,1._pm_r,165._pm_r,.02_pm_r,198._pm_r, &
       275.60_pm_r,55154._pm_r,6._pm_r,153._pm_r,.21_pm_r,320._pm_r,1._pm_r,166._pm_r,.03_pm_r,261._pm_r, &
       264.30_pm_r,59113._pm_r,5._pm_r,153._pm_r,.11_pm_r,3._pm_r,1._pm_r,169._pm_r,.07_pm_r,298._pm_r, &
       250.00_pm_r,62875._pm_r,5._pm_r,152._pm_r,.12_pm_r,33._pm_r,1._pm_r,174._pm_r,.12_pm_r,322._pm_r, &
       235.90_pm_r,66436._pm_r,5._pm_r,150._pm_r,.12_pm_r,31._pm_r,1._pm_r,180._pm_r,.15_pm_r,330._pm_r, &
       222.30_pm_r,69788._pm_r,5._pm_r,149._pm_r,.09_pm_r,348._pm_r,1._pm_r,188._pm_r,.14_pm_r,333._pm_r, &
       207.80_pm_r,72943._pm_r,5._pm_r,149._pm_r,.14_pm_r,299._pm_r,1._pm_r,199._pm_r,.11_pm_r,339._pm_r, &
       192.40_pm_r,75869._pm_r,5._pm_r,151._pm_r,.20_pm_r,283._pm_r,1._pm_r,210._pm_r,.08_pm_r,341._pm_r, &
       177.00_pm_r,78577._pm_r,4._pm_r,154._pm_r,.23_pm_r,279._pm_r,0._pm_r,220._pm_r,.06_pm_r,345._pm_r, &
       163.60_pm_r,81061._pm_r,4._pm_r,158._pm_r,.23_pm_r,274._pm_r,0._pm_r,227._pm_r,.03_pm_r,338._pm_r, &
       151.20_pm_r,83363._pm_r,4._pm_r,162._pm_r,.21_pm_r,273._pm_r,0._pm_r,230._pm_r,.02_pm_r,0._pm_r, &
       142.40_pm_r,85466._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       141.10_pm_r,87509._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       144.70_pm_r,89593._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       152.90_pm_r,91773._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       166.20_pm_r,94119._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.30_pm_r,96727._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       217.00_pm_r,99740._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       256.80_pm_r,103329._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       292.20_pm_r,107572._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       322.30_pm_r,112367._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       410.50_pm_r,118196._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_60= (/ &
       275.70_pm_r,-127._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       254.80_pm_r,3750._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       231.00_pm_r,7299._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       222.70_pm_r,10615._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       225.80_pm_r,13892._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       228.00_pm_r,17219._pm_r,11._pm_r,196._pm_r,.37_pm_r,334._pm_r,1._pm_r,251._pm_r,.11_pm_r,156._pm_r, &
       229.60_pm_r,20569._pm_r,11._pm_r,198._pm_r,.49_pm_r,346._pm_r,1._pm_r,239._pm_r,.16_pm_r,153._pm_r, &
       231.80_pm_r,23946._pm_r,10._pm_r,200._pm_r,.59_pm_r,2._pm_r,1._pm_r,223._pm_r,.19_pm_r,151._pm_r, &
       236.30_pm_r,27369._pm_r,9._pm_r,201._pm_r,.66_pm_r,21._pm_r,1._pm_r,209._pm_r,.20_pm_r,150._pm_r, &
       245.60_pm_r,30896._pm_r,8._pm_r,199._pm_r,.75_pm_r,39._pm_r,1._pm_r,197._pm_r,.18_pm_r,147._pm_r, &
       257.40_pm_r,34575._pm_r,7._pm_r,195._pm_r,.81_pm_r,53._pm_r,1._pm_r,189._pm_r,.15_pm_r,143._pm_r, &
       269.10_pm_r,38436._pm_r,6._pm_r,188._pm_r,.79_pm_r,61._pm_r,2._pm_r,183._pm_r,.11_pm_r,131._pm_r, &
       278.70_pm_r,42448._pm_r,6._pm_r,179._pm_r,.68_pm_r,65._pm_r,2._pm_r,179._pm_r,.07_pm_r,117._pm_r, &
       282.40_pm_r,46566._pm_r,6._pm_r,172._pm_r,.39_pm_r,58._pm_r,2._pm_r,177._pm_r,.04_pm_r,82._pm_r, &
       279.50_pm_r,50687._pm_r,5._pm_r,168._pm_r,.11_pm_r,45._pm_r,2._pm_r,175._pm_r,.07_pm_r,82._pm_r, &
       271.50_pm_r,54724._pm_r,5._pm_r,168._pm_r,.01_pm_r,270._pm_r,2._pm_r,170._pm_r,.12_pm_r,65._pm_r, &
       260.80_pm_r,58627._pm_r,5._pm_r,168._pm_r,.05_pm_r,53._pm_r,2._pm_r,163._pm_r,.19_pm_r,39._pm_r, &
       247.90_pm_r,62349._pm_r,5._pm_r,167._pm_r,.11_pm_r,45._pm_r,1._pm_r,153._pm_r,.24_pm_r,16._pm_r, &
       235.00_pm_r,65888._pm_r,5._pm_r,165._pm_r,.08_pm_r,60._pm_r,1._pm_r,139._pm_r,.26_pm_r,7._pm_r, &
       222.30_pm_r,69233._pm_r,5._pm_r,164._pm_r,.08_pm_r,137._pm_r,1._pm_r,119._pm_r,.26_pm_r,358._pm_r, &
       209.60_pm_r,72400._pm_r,5._pm_r,164._pm_r,.19_pm_r,171._pm_r,1._pm_r,91._pm_r,.22_pm_r,352._pm_r, &
       195.90_pm_r,75366._pm_r,6._pm_r,165._pm_r,.30_pm_r,179._pm_r,1._pm_r,66._pm_r,.16_pm_r,348._pm_r, &
       181.60_pm_r,78134._pm_r,6._pm_r,166._pm_r,.36_pm_r,183._pm_r,1._pm_r,49._pm_r,.12_pm_r,346._pm_r, &
       168.80_pm_r,80690._pm_r,7._pm_r,168._pm_r,.34_pm_r,184._pm_r,1._pm_r,39._pm_r,.09_pm_r,341._pm_r, &
       157.70_pm_r,83072._pm_r,7._pm_r,169._pm_r,.31_pm_r,184._pm_r,1._pm_r,33._pm_r,.06_pm_r,342._pm_r, &
       150.80_pm_r,85302._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       149.40_pm_r,87478._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       151.70_pm_r,89676._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       158.50_pm_r,91950._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       169.90_pm_r,94366._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.10_pm_r,97012._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       213.50_pm_r,100008._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       248.00_pm_r,103504._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       284.40_pm_r,107612._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       322.50_pm_r,112355._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       407.10_pm_r,118165._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_50= (/ &
       281.50_pm_r,-50._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       260.40_pm_r,3913._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       236.00_pm_r,7543._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       221.70_pm_r,10891._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       220.70_pm_r,14126._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       221.20_pm_r,17364._pm_r,9._pm_r,199._pm_r,.45_pm_r,4._pm_r,0._pm_r,211._pm_r,.05_pm_r,192._pm_r, &
       222.70_pm_r,20612._pm_r,8._pm_r,200._pm_r,.62_pm_r,8._pm_r,1._pm_r,207._pm_r,.08_pm_r,188._pm_r, &
       227.10_pm_r,23906._pm_r,7._pm_r,201._pm_r,.72_pm_r,13._pm_r,1._pm_r,203._pm_r,.10_pm_r,185._pm_r, &
       231.90_pm_r,27264._pm_r,6._pm_r,202._pm_r,.73_pm_r,19._pm_r,1._pm_r,199._pm_r,.11_pm_r,183._pm_r, &
       240.60_pm_r,30722._pm_r,5._pm_r,202._pm_r,.65_pm_r,28._pm_r,1._pm_r,196._pm_r,.10_pm_r,180._pm_r, &
       252.90_pm_r,34330._pm_r,4._pm_r,200._pm_r,.53_pm_r,39._pm_r,1._pm_r,193._pm_r,.09_pm_r,171._pm_r, &
       265.30_pm_r,38131._pm_r,4._pm_r,195._pm_r,.40_pm_r,52._pm_r,1._pm_r,191._pm_r,.07_pm_r,164._pm_r, &
       275.10_pm_r,42089._pm_r,3._pm_r,189._pm_r,.29_pm_r,66._pm_r,1._pm_r,188._pm_r,.05_pm_r,148._pm_r, &
       278.80_pm_r,46154._pm_r,3._pm_r,184._pm_r,.15_pm_r,54._pm_r,1._pm_r,187._pm_r,.01_pm_r,117._pm_r, &
       276.00_pm_r,50223._pm_r,3._pm_r,182._pm_r,.07_pm_r,30._pm_r,1._pm_r,186._pm_r,.04_pm_r,83._pm_r, &
       268.70_pm_r,54212._pm_r,3._pm_r,181._pm_r,.03_pm_r,129._pm_r,1._pm_r,181._pm_r,.11_pm_r,83._pm_r, &
       258.20_pm_r,58076._pm_r,3._pm_r,180._pm_r,.16_pm_r,146._pm_r,1._pm_r,171._pm_r,.19_pm_r,70._pm_r, &
       245.90_pm_r,61764._pm_r,3._pm_r,177._pm_r,.25_pm_r,147._pm_r,1._pm_r,156._pm_r,.22_pm_r,50._pm_r, &
       233.40_pm_r,65277._pm_r,4._pm_r,174._pm_r,.28_pm_r,147._pm_r,1._pm_r,139._pm_r,.22_pm_r,32._pm_r, &
       221.20_pm_r,68602._pm_r,4._pm_r,171._pm_r,.28_pm_r,149._pm_r,1._pm_r,121._pm_r,.21_pm_r,10._pm_r, &
       210.10_pm_r,71764._pm_r,4._pm_r,169._pm_r,.27_pm_r,153._pm_r,1._pm_r,103._pm_r,.19_pm_r,350._pm_r, &
       199.40_pm_r,74759._pm_r,5._pm_r,168._pm_r,.24_pm_r,158._pm_r,1._pm_r,82._pm_r,.20_pm_r,333._pm_r, &
       188.30_pm_r,77601._pm_r,5._pm_r,168._pm_r,.20_pm_r,161._pm_r,1._pm_r,57._pm_r,.18_pm_r,321._pm_r, &
       177.80_pm_r,80275._pm_r,5._pm_r,167._pm_r,.18_pm_r,164._pm_r,1._pm_r,32._pm_r,.18_pm_r,315._pm_r, &
       168.60_pm_r,82805._pm_r,6._pm_r,167._pm_r,.14_pm_r,168._pm_r,1._pm_r,12._pm_r,.16_pm_r,309._pm_r, &
       162.10_pm_r,85210._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       159.70_pm_r,87552._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       160.20_pm_r,89889._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       165.20_pm_r,92278._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       174.10_pm_r,94778._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       188.20_pm_r,97467._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       209.50_pm_r,100444._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       238.70_pm_r,103841._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       275.00_pm_r,107798._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       321.10_pm_r,112460._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       403.20_pm_r,118235._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_40= (/ &
       288.50_pm_r,-1._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       267.20_pm_r,4066._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       242.00_pm_r,7794._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       221.30_pm_r,11185._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       213.90_pm_r,14371._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       213.30_pm_r,17494._pm_r,4._pm_r,194._pm_r,.11_pm_r,311._pm_r,1._pm_r,117._pm_r,.10_pm_r,261._pm_r, &
       216.70_pm_r,20638._pm_r,4._pm_r,196._pm_r,.13_pm_r,318._pm_r,1._pm_r,128._pm_r,.13_pm_r,261._pm_r, &
       222.90_pm_r,23860._pm_r,4._pm_r,198._pm_r,.13_pm_r,338._pm_r,0._pm_r,148._pm_r,.15_pm_r,258._pm_r, &
       228.50_pm_r,27163._pm_r,4._pm_r,200._pm_r,.13_pm_r,4._pm_r,0._pm_r,177._pm_r,.14_pm_r,258._pm_r, &
       236.90_pm_r,30569._pm_r,3._pm_r,200._pm_r,.15_pm_r,31._pm_r,1._pm_r,201._pm_r,.13_pm_r,257._pm_r, &
       249.30_pm_r,34124._pm_r,3._pm_r,198._pm_r,.17_pm_r,50._pm_r,1._pm_r,213._pm_r,.09_pm_r,257._pm_r, &
       261.30_pm_r,37867._pm_r,3._pm_r,195._pm_r,.17_pm_r,60._pm_r,1._pm_r,219._pm_r,.06_pm_r,260._pm_r, &
       271.90_pm_r,41771._pm_r,3._pm_r,191._pm_r,.15_pm_r,62._pm_r,1._pm_r,223._pm_r,.03_pm_r,270._pm_r, &
       276.70_pm_r,45798._pm_r,3._pm_r,190._pm_r,.07_pm_r,21._pm_r,1._pm_r,224._pm_r,.00_pm_r,143._pm_r, &
       273.50_pm_r,49835._pm_r,3._pm_r,190._pm_r,.02_pm_r,243._pm_r,1._pm_r,221._pm_r,.06_pm_r,108._pm_r, &
       265.50_pm_r,53783._pm_r,3._pm_r,189._pm_r,.15_pm_r,156._pm_r,1._pm_r,209._pm_r,.14_pm_r,105._pm_r, &
       254.90_pm_r,57600._pm_r,3._pm_r,184._pm_r,.35_pm_r,130._pm_r,1._pm_r,188._pm_r,.18_pm_r,99._pm_r, &
       242.80_pm_r,61240._pm_r,3._pm_r,175._pm_r,.40_pm_r,111._pm_r,1._pm_r,168._pm_r,.13_pm_r,90._pm_r, &
       231.10_pm_r,64714._pm_r,3._pm_r,165._pm_r,.37_pm_r,90._pm_r,1._pm_r,156._pm_r,.07_pm_r,56._pm_r, &
       219.90_pm_r,68013._pm_r,3._pm_r,157._pm_r,.30_pm_r,60._pm_r,1._pm_r,149._pm_r,.08_pm_r,360._pm_r, &
       210.50_pm_r,71165._pm_r,3._pm_r,151._pm_r,.31_pm_r,25._pm_r,1._pm_r,145._pm_r,.11_pm_r,333._pm_r, &
       202.80_pm_r,74188._pm_r,3._pm_r,145._pm_r,.36_pm_r,2._pm_r,0._pm_r,143._pm_r,.13_pm_r,326._pm_r, &
       195.40_pm_r,77107._pm_r,3._pm_r,138._pm_r,.42_pm_r,349._pm_r,0._pm_r,143._pm_r,.15_pm_r,323._pm_r, &
       188.60_pm_r,79915._pm_r,2._pm_r,129._pm_r,.44_pm_r,343._pm_r,0._pm_r,321._pm_r,.16_pm_r,321._pm_r, &
       181.50_pm_r,82630._pm_r,1._pm_r,115._pm_r,.43_pm_r,339._pm_r,0._pm_r,320._pm_r,.15_pm_r,318._pm_r, &
       174.60_pm_r,85226._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       170.50_pm_r,87739._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       169.30_pm_r,90224._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       172.20_pm_r,92733._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       178.40_pm_r,95321._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       189.20_pm_r,98052._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       205.50_pm_r,101009._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       229.70_pm_r,104310._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       264.80_pm_r,108115._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       317.40_pm_r,112667._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       398.90_pm_r,118388._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_30= (/ &
       294.60_pm_r,-28._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       272.80_pm_r,4129._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       247.50_pm_r,7941._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       223.20_pm_r,11389._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       207.80_pm_r,14547._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       206.20_pm_r,17562._pm_r,2._pm_r,162._pm_r,.25_pm_r,229._pm_r,0._pm_r,88._pm_r,.04_pm_r,183._pm_r, &
       212.50_pm_r,20626._pm_r,2._pm_r,173._pm_r,.29_pm_r,227._pm_r,0._pm_r,103._pm_r,.06_pm_r,185._pm_r, &
       220.20_pm_r,23798._pm_r,2._pm_r,182._pm_r,.27_pm_r,223._pm_r,0._pm_r,120._pm_r,.06_pm_r,191._pm_r, &
       225.70_pm_r,27063._pm_r,3._pm_r,187._pm_r,.20_pm_r,214._pm_r,0._pm_r,135._pm_r,.06_pm_r,198._pm_r, &
       233.50_pm_r,30424._pm_r,3._pm_r,188._pm_r,.10_pm_r,191._pm_r,0._pm_r,147._pm_r,.06_pm_r,203._pm_r, &
       246.00_pm_r,33930._pm_r,3._pm_r,187._pm_r,.08_pm_r,122._pm_r,0._pm_r,156._pm_r,.05_pm_r,204._pm_r, &
       257.90_pm_r,37625._pm_r,3._pm_r,184._pm_r,.12_pm_r,81._pm_r,1._pm_r,162._pm_r,.04_pm_r,208._pm_r, &
       267.90_pm_r,41474._pm_r,3._pm_r,180._pm_r,.14_pm_r,69._pm_r,1._pm_r,166._pm_r,.03_pm_r,198._pm_r, &
       272.90_pm_r,45442._pm_r,3._pm_r,177._pm_r,.12_pm_r,77._pm_r,1._pm_r,166._pm_r,.02_pm_r,124._pm_r, &
       271.30_pm_r,49435._pm_r,3._pm_r,172._pm_r,.18_pm_r,93._pm_r,1._pm_r,160._pm_r,.07_pm_r,73._pm_r, &
       263.60_pm_r,53353._pm_r,3._pm_r,166._pm_r,.22_pm_r,91._pm_r,1._pm_r,147._pm_r,.10_pm_r,80._pm_r, &
       253.70_pm_r,57145._pm_r,3._pm_r,160._pm_r,.21_pm_r,69._pm_r,1._pm_r,136._pm_r,.12_pm_r,107._pm_r, &
       241.90_pm_r,60773._pm_r,3._pm_r,155._pm_r,.17_pm_r,22._pm_r,1._pm_r,135._pm_r,.12_pm_r,146._pm_r, &
       229.30_pm_r,64226._pm_r,3._pm_r,152._pm_r,.20_pm_r,350._pm_r,1._pm_r,139._pm_r,.11_pm_r,177._pm_r, &
       218.90_pm_r,67503._pm_r,2._pm_r,151._pm_r,.20_pm_r,338._pm_r,1._pm_r,145._pm_r,.08_pm_r,217._pm_r, &
       211.20_pm_r,70651._pm_r,2._pm_r,150._pm_r,.17_pm_r,333._pm_r,1._pm_r,151._pm_r,.11_pm_r,259._pm_r, &
       205.90_pm_r,73702._pm_r,2._pm_r,150._pm_r,.11_pm_r,335._pm_r,1._pm_r,160._pm_r,.14_pm_r,286._pm_r, &
       201.70_pm_r,76687._pm_r,2._pm_r,149._pm_r,.08_pm_r,338._pm_r,1._pm_r,170._pm_r,.17_pm_r,298._pm_r, &
       198.10_pm_r,79614._pm_r,2._pm_r,148._pm_r,.05_pm_r,346._pm_r,1._pm_r,187._pm_r,.19_pm_r,304._pm_r, &
       192.70_pm_r,82491._pm_r,2._pm_r,147._pm_r,.03_pm_r,0._pm_r,1._pm_r,211._pm_r,.20_pm_r,306._pm_r, &
       185.10_pm_r,85247._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       179.70_pm_r,87904._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       177.50_pm_r,90521._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       178.60_pm_r,93140._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       182.30_pm_r,95808._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       189.80_pm_r,98576._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       201.70_pm_r,101514._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       221.70_pm_r,104727._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       254.50_pm_r,108390._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       311.50_pm_r,112813._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       394.30_pm_r,118459._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_20= (/ &
       298.40_pm_r,-65._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       275.90_pm_r,4145._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       251.80_pm_r,8014._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       225.20_pm_r,11511._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       203.30_pm_r,14653._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       201.00_pm_r,17586._pm_r,3._pm_r,140._pm_r,.32_pm_r,262._pm_r,0._pm_r,329._pm_r,.16_pm_r,120._pm_r, &
       210.00_pm_r,20594._pm_r,2._pm_r,152._pm_r,.38_pm_r,261._pm_r,0._pm_r,4._pm_r,.21_pm_r,119._pm_r, &
       218.20_pm_r,23734._pm_r,2._pm_r,165._pm_r,.35_pm_r,262._pm_r,0._pm_r,77._pm_r,.21_pm_r,123._pm_r, &
       223.80_pm_r,26971._pm_r,2._pm_r,178._pm_r,.28_pm_r,262._pm_r,1._pm_r,101._pm_r,.21_pm_r,125._pm_r, &
       231.60_pm_r,30304._pm_r,2._pm_r,186._pm_r,.15_pm_r,259._pm_r,1._pm_r,110._pm_r,.17_pm_r,129._pm_r, &
       242.90_pm_r,33774._pm_r,2._pm_r,189._pm_r,.02_pm_r,207._pm_r,1._pm_r,114._pm_r,.11_pm_r,135._pm_r, &
       254.00_pm_r,37417._pm_r,2._pm_r,187._pm_r,.10_pm_r,102._pm_r,1._pm_r,117._pm_r,.06_pm_r,151._pm_r, &
       263.50_pm_r,41206._pm_r,2._pm_r,182._pm_r,.16_pm_r,97._pm_r,1._pm_r,119._pm_r,.03_pm_r,198._pm_r, &
       269.10_pm_r,45112._pm_r,2._pm_r,176._pm_r,.16_pm_r,97._pm_r,1._pm_r,122._pm_r,.09_pm_r,263._pm_r, &
       269.50_pm_r,49063._pm_r,2._pm_r,170._pm_r,.18_pm_r,90._pm_r,1._pm_r,128._pm_r,.18_pm_r,276._pm_r, &
       262.60_pm_r,52963._pm_r,2._pm_r,165._pm_r,.16_pm_r,73._pm_r,1._pm_r,139._pm_r,.14_pm_r,274._pm_r, &
       254.00_pm_r,56747._pm_r,2._pm_r,160._pm_r,.16_pm_r,22._pm_r,1._pm_r,144._pm_r,.11_pm_r,124._pm_r, &
       243.40_pm_r,60390._pm_r,2._pm_r,157._pm_r,.23_pm_r,346._pm_r,1._pm_r,135._pm_r,.31_pm_r,110._pm_r, &
       230.80_pm_r,63867._pm_r,2._pm_r,157._pm_r,.30_pm_r,332._pm_r,2._pm_r,126._pm_r,.37_pm_r,109._pm_r, &
       219.50_pm_r,67158._pm_r,1._pm_r,159._pm_r,.31_pm_r,329._pm_r,2._pm_r,122._pm_r,.27_pm_r,106._pm_r, &
       212.00_pm_r,70315._pm_r,1._pm_r,165._pm_r,.30_pm_r,327._pm_r,2._pm_r,120._pm_r,.10_pm_r,101._pm_r, &
       208.30_pm_r,73390._pm_r,0._pm_r,179._pm_r,.28_pm_r,332._pm_r,2._pm_r,119._pm_r,.08_pm_r,300._pm_r, &
       205.60_pm_r,76421._pm_r,0._pm_r,244._pm_r,.26_pm_r,337._pm_r,2._pm_r,120._pm_r,.21_pm_r,289._pm_r, &
       203.80_pm_r,79416._pm_r,0._pm_r,310._pm_r,.25_pm_r,339._pm_r,2._pm_r,122._pm_r,.31_pm_r,290._pm_r, &
       199.40_pm_r,82388._pm_r,1._pm_r,324._pm_r,.22_pm_r,342._pm_r,1._pm_r,127._pm_r,.35_pm_r,292._pm_r, &
       191.70_pm_r,85241._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.00_pm_r,87993._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       183.50_pm_r,90703._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       183.50_pm_r,93405._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       185.30_pm_r,96135._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       190.00_pm_r,98928._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       198.20_pm_r,101844._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       214.60_pm_r,104979._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       245.00_pm_r,108513._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       304.10_pm_r,112801._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       389.30_pm_r,118355._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_10= (/ &
       300.40_pm_r,-67._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       276.50_pm_r,4164._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       253.10_pm_r,8049._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       225.70_pm_r,11561._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       200.50_pm_r,14687._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       198.20_pm_r,17574._pm_r,2._pm_r,138._pm_r,.37_pm_r,290._pm_r,0._pm_r,296._pm_r,.21_pm_r,107._pm_r, &
       208.60_pm_r,20551._pm_r,2._pm_r,148._pm_r,.43_pm_r,289._pm_r,0._pm_r,339._pm_r,.26_pm_r,108._pm_r, &
       217.20_pm_r,23674._pm_r,1._pm_r,164._pm_r,.42_pm_r,289._pm_r,0._pm_r,98._pm_r,.26_pm_r,107._pm_r, &
       222.70_pm_r,26896._pm_r,1._pm_r,188._pm_r,.35_pm_r,288._pm_r,1._pm_r,102._pm_r,.23_pm_r,106._pm_r, &
       229.90_pm_r,30206._pm_r,1._pm_r,210._pm_r,.22_pm_r,288._pm_r,1._pm_r,104._pm_r,.16_pm_r,106._pm_r, &
       241.40_pm_r,33653._pm_r,1._pm_r,220._pm_r,.08_pm_r,285._pm_r,1._pm_r,104._pm_r,.06_pm_r,104._pm_r, &
       251.70_pm_r,37270._pm_r,1._pm_r,220._pm_r,.04_pm_r,115._pm_r,1._pm_r,103._pm_r,.03_pm_r,308._pm_r, &
       260.10_pm_r,41017._pm_r,1._pm_r,215._pm_r,.11_pm_r,109._pm_r,1._pm_r,102._pm_r,.10_pm_r,298._pm_r, &
       265.20_pm_r,44868._pm_r,1._pm_r,212._pm_r,.09_pm_r,18._pm_r,1._pm_r,101._pm_r,.14_pm_r,275._pm_r, &
       266.80_pm_r,48768._pm_r,1._pm_r,225._pm_r,.35_pm_r,344._pm_r,1._pm_r,106._pm_r,.16_pm_r,249._pm_r, &
       263.40_pm_r,52654._pm_r,1._pm_r,270._pm_r,.53_pm_r,342._pm_r,1._pm_r,121._pm_r,.12_pm_r,213._pm_r, &
       257.20_pm_r,56470._pm_r,1._pm_r,304._pm_r,.48_pm_r,344._pm_r,1._pm_r,132._pm_r,.11_pm_r,149._pm_r, &
       247.50_pm_r,60168._pm_r,2._pm_r,315._pm_r,.19_pm_r,354._pm_r,1._pm_r,129._pm_r,.10_pm_r,86._pm_r, &
       233.90_pm_r,63699._pm_r,2._pm_r,319._pm_r,.07_pm_r,118._pm_r,1._pm_r,120._pm_r,.12_pm_r,40._pm_r, &
       220.90_pm_r,67023._pm_r,2._pm_r,319._pm_r,.19_pm_r,149._pm_r,1._pm_r,109._pm_r,.13_pm_r,351._pm_r, &
       212.10_pm_r,70191._pm_r,1._pm_r,314._pm_r,.25_pm_r,158._pm_r,1._pm_r,96._pm_r,.21_pm_r,319._pm_r, &
       208.20_pm_r,73263._pm_r,1._pm_r,304._pm_r,.26_pm_r,162._pm_r,1._pm_r,70._pm_r,.29_pm_r,305._pm_r, &
       206.60_pm_r,76299._pm_r,1._pm_r,281._pm_r,.26_pm_r,166._pm_r,0._pm_r,5._pm_r,.36_pm_r,298._pm_r, &
       206.10_pm_r,79321._pm_r,1._pm_r,247._pm_r,.25_pm_r,169._pm_r,1._pm_r,324._pm_r,.41_pm_r,294._pm_r, &
       202.40_pm_r,82337._pm_r,1._pm_r,219._pm_r,.23_pm_r,170._pm_r,1._pm_r,311._pm_r,.41_pm_r,292._pm_r, &
       194.70_pm_r,85235._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       188.90_pm_r,88030._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.70_pm_r,90785._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.40_pm_r,93535._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.00_pm_r,96300._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       189.50_pm_r,99104._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       195.10_pm_r,101995._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       208.50_pm_r,105060._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       236.70_pm_r,108484._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       296.60_pm_r,112642._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       383.80_pm_r,118098._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_0= (/ &
       300.60_pm_r,-91._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       276.80_pm_r,4145._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       253.50_pm_r,8035._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       226.00_pm_r,11553._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       200.00_pm_r,14678._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       197.00_pm_r,17554._pm_r,2._pm_r,138._pm_r,.33_pm_r,306._pm_r,1._pm_r,313._pm_r,.19_pm_r,98._pm_r, &
       207.40_pm_r,20518._pm_r,2._pm_r,141._pm_r,.43_pm_r,306._pm_r,0._pm_r,347._pm_r,.23_pm_r,97._pm_r, &
       216.50_pm_r,23627._pm_r,1._pm_r,149._pm_r,.44_pm_r,307._pm_r,1._pm_r,36._pm_r,.23_pm_r,94._pm_r, &
       222.20_pm_r,26837._pm_r,0._pm_r,188._pm_r,.40_pm_r,307._pm_r,1._pm_r,58._pm_r,.19_pm_r,90._pm_r, &
       229.60_pm_r,30141._pm_r,1._pm_r,268._pm_r,.30_pm_r,306._pm_r,1._pm_r,65._pm_r,.12_pm_r,83._pm_r, &
       242.00_pm_r,33588._pm_r,1._pm_r,285._pm_r,.17_pm_r,303._pm_r,1._pm_r,65._pm_r,.04_pm_r,45._pm_r, &
       252.00_pm_r,37208._pm_r,1._pm_r,287._pm_r,.07_pm_r,283._pm_r,1._pm_r,62._pm_r,.07_pm_r,309._pm_r, &
       259.30_pm_r,40949._pm_r,1._pm_r,285._pm_r,.05_pm_r,191._pm_r,1._pm_r,56._pm_r,.13_pm_r,293._pm_r, &
       263.80_pm_r,44786._pm_r,1._pm_r,284._pm_r,.04_pm_r,315._pm_r,1._pm_r,49._pm_r,.10_pm_r,256._pm_r, &
       265.60_pm_r,48668._pm_r,1._pm_r,292._pm_r,.21_pm_r,331._pm_r,1._pm_r,48._pm_r,.14_pm_r,192._pm_r, &
       264.10_pm_r,52551._pm_r,2._pm_r,304._pm_r,.38_pm_r,335._pm_r,1._pm_r,63._pm_r,.16_pm_r,166._pm_r, &
       258.50_pm_r,56379._pm_r,2._pm_r,313._pm_r,.47_pm_r,339._pm_r,1._pm_r,82._pm_r,.07_pm_r,124._pm_r, &
       248.20_pm_r,60088._pm_r,3._pm_r,319._pm_r,.30_pm_r,340._pm_r,1._pm_r,79._pm_r,.13_pm_r,16._pm_r, &
       234.50_pm_r,63627._pm_r,3._pm_r,321._pm_r,.10_pm_r,343._pm_r,1._pm_r,64._pm_r,.19_pm_r,12._pm_r, &
       220.60_pm_r,66953._pm_r,3._pm_r,322._pm_r,.08_pm_r,137._pm_r,1._pm_r,53._pm_r,.21_pm_r,33._pm_r, &
       211.40_pm_r,70117._pm_r,2._pm_r,321._pm_r,.24_pm_r,152._pm_r,1._pm_r,51._pm_r,.23_pm_r,61._pm_r, &
       208.10_pm_r,73187._pm_r,2._pm_r,318._pm_r,.37_pm_r,152._pm_r,1._pm_r,55._pm_r,.29_pm_r,82._pm_r, &
       208.30_pm_r,76232._pm_r,1._pm_r,308._pm_r,.47_pm_r,152._pm_r,2._pm_r,62._pm_r,.36_pm_r,96._pm_r, &
       208.50_pm_r,79280._pm_r,1._pm_r,284._pm_r,.52_pm_r,153._pm_r,2._pm_r,69._pm_r,.41_pm_r,101._pm_r, &
       204.40_pm_r,82325._pm_r,1._pm_r,234._pm_r,.52_pm_r,154._pm_r,2._pm_r,76._pm_r,.41_pm_r,105._pm_r, &
       195.90_pm_r,85245._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       189.60_pm_r,88052._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.50_pm_r,90818._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.30_pm_r,93581._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.40_pm_r,96356._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       188.50_pm_r,99156._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       192.40_pm_r,102020._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       203.60_pm_r,105027._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       230.00_pm_r,108362._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       289.80_pm_r,112411._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       378.20_pm_r,117772._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_10= (/ &
       300.70_pm_r,-105._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       277.10_pm_r,4133._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       253.00_pm_r,8021._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       225.50_pm_r,11531._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       200.40_pm_r,14655._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       197.90_pm_r,17538._pm_r,4._pm_r,142._pm_r,.68_pm_r,325._pm_r,1._pm_r,312._pm_r,.24_pm_r,65._pm_r, &
       208.00_pm_r,20508._pm_r,3._pm_r,140._pm_r,.89_pm_r,326._pm_r,1._pm_r,338._pm_r,.29_pm_r,63._pm_r, &
       216.30_pm_r,23619._pm_r,2._pm_r,135._pm_r,.91_pm_r,326._pm_r,1._pm_r,3._pm_r,.26_pm_r,61._pm_r, &
       222.30_pm_r,26830._pm_r,0._pm_r,96._pm_r,.82_pm_r,328._pm_r,1._pm_r,16._pm_r,.19_pm_r,54._pm_r, &
       230.20_pm_r,30140._pm_r,1._pm_r,350._pm_r,.62_pm_r,330._pm_r,1._pm_r,20._pm_r,.08_pm_r,27._pm_r, &
       241.80_pm_r,33592._pm_r,2._pm_r,342._pm_r,.37_pm_r,333._pm_r,1._pm_r,18._pm_r,.08_pm_r,295._pm_r, &
       251.80_pm_r,37212._pm_r,2._pm_r,340._pm_r,.15_pm_r,340._pm_r,1._pm_r,11._pm_r,.17_pm_r,272._pm_r, &
       259.70_pm_r,40958._pm_r,2._pm_r,341._pm_r,.02_pm_r,90._pm_r,1._pm_r,359._pm_r,.22_pm_r,266._pm_r, &
       264.30_pm_r,44798._pm_r,2._pm_r,340._pm_r,.11_pm_r,313._pm_r,1._pm_r,348._pm_r,.13_pm_r,260._pm_r, &
       266.10_pm_r,48685._pm_r,2._pm_r,336._pm_r,.28_pm_r,296._pm_r,1._pm_r,343._pm_r,.03_pm_r,259._pm_r, &
       263.30_pm_r,52565._pm_r,3._pm_r,329._pm_r,.30_pm_r,283._pm_r,1._pm_r,342._pm_r,.02_pm_r,324._pm_r, &
       257.00_pm_r,56381._pm_r,3._pm_r,323._pm_r,.16_pm_r,230._pm_r,1._pm_r,340._pm_r,.09_pm_r,270._pm_r, &
       246.50_pm_r,60070._pm_r,3._pm_r,320._pm_r,.28_pm_r,162._pm_r,2._pm_r,334._pm_r,.13_pm_r,258._pm_r, &
       232.80_pm_r,63586._pm_r,2._pm_r,316._pm_r,.40_pm_r,154._pm_r,2._pm_r,327._pm_r,.11_pm_r,212._pm_r, &
       219.90_pm_r,66894._pm_r,2._pm_r,308._pm_r,.41_pm_r,160._pm_r,1._pm_r,323._pm_r,.18_pm_r,150._pm_r, &
       211.30_pm_r,70049._pm_r,1._pm_r,291._pm_r,.37_pm_r,175._pm_r,1._pm_r,326._pm_r,.34_pm_r,128._pm_r, &
       208.10_pm_r,73116._pm_r,1._pm_r,265._pm_r,.35_pm_r,194._pm_r,0._pm_r,359._pm_r,.51_pm_r,121._pm_r, &
       207.60_pm_r,76159._pm_r,2._pm_r,247._pm_r,.37_pm_r,210._pm_r,1._pm_r,89._pm_r,.65_pm_r,118._pm_r, &
       207.60_pm_r,79202._pm_r,2._pm_r,239._pm_r,.40_pm_r,222._pm_r,2._pm_r,105._pm_r,.74_pm_r,116._pm_r, &
       204.20_pm_r,82244._pm_r,3._pm_r,236._pm_r,.39_pm_r,229._pm_r,3._pm_r,109._pm_r,.75_pm_r,115._pm_r, &
       196.30_pm_r,85175._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       189.80_pm_r,87989._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.10_pm_r,90751._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.00_pm_r,93508._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.00_pm_r,96278._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.50_pm_r,99069._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       190.50_pm_r,101910._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       200.50_pm_r,104878._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       225.50_pm_r,108153._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       284.60_pm_r,112123._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       372.90_pm_r,117403._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_20= (/ &
       298.50_pm_r,-100._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       275.00_pm_r,4104._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       250.20_pm_r,7955._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       224.20_pm_r,11433._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       203.00_pm_r,14565._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       200.90_pm_r,17496._pm_r,7._pm_r,145._pm_r,1.17_pm_r,323._pm_r,2._pm_r,290._pm_r,.47_pm_r,67._pm_r, &
       209.40_pm_r,20498._pm_r,5._pm_r,144._pm_r,1.61_pm_r,329._pm_r,1._pm_r,319._pm_r,.53_pm_r,65._pm_r, &
       217.00_pm_r,23625._pm_r,3._pm_r,135._pm_r,1.80_pm_r,338._pm_r,1._pm_r,354._pm_r,.40_pm_r,59._pm_r, &
       222.10_pm_r,26840._pm_r,1._pm_r,57._pm_r,1.88_pm_r,347._pm_r,1._pm_r,9._pm_r,.19_pm_r,32._pm_r, &
       229.20_pm_r,30142._pm_r,3._pm_r,12._pm_r,1.76_pm_r,357._pm_r,2._pm_r,5._pm_r,.22_pm_r,295._pm_r, &
       240.50_pm_r,33576._pm_r,6._pm_r,7._pm_r,1.46_pm_r,5._pm_r,2._pm_r,348._pm_r,.47_pm_r,275._pm_r, &
       251.80_pm_r,37186._pm_r,8._pm_r,7._pm_r,1.03_pm_r,9._pm_r,2._pm_r,326._pm_r,.62_pm_r,272._pm_r, &
       261.60_pm_r,40944._pm_r,9._pm_r,7._pm_r,.51_pm_r,2._pm_r,3._pm_r,310._pm_r,.65_pm_r,274._pm_r, &
       266.90_pm_r,44821._pm_r,9._pm_r,6._pm_r,.36_pm_r,305._pm_r,3._pm_r,303._pm_r,.37_pm_r,279._pm_r, &
       266.60_pm_r,48733._pm_r,9._pm_r,2._pm_r,.64_pm_r,263._pm_r,4._pm_r,302._pm_r,.16_pm_r,333._pm_r, &
       260.80_pm_r,52598._pm_r,9._pm_r,355._pm_r,.89_pm_r,245._pm_r,4._pm_r,305._pm_r,.21_pm_r,35._pm_r, &
       252.60_pm_r,56362._pm_r,9._pm_r,346._pm_r,.95_pm_r,231._pm_r,4._pm_r,310._pm_r,.19_pm_r,54._pm_r, &
       243.10_pm_r,59990._pm_r,8._pm_r,337._pm_r,1.02_pm_r,221._pm_r,4._pm_r,313._pm_r,.09_pm_r,56._pm_r, &
       232.30_pm_r,63476._pm_r,7._pm_r,327._pm_r,.93_pm_r,218._pm_r,4._pm_r,314._pm_r,.04_pm_r,76._pm_r, &
       220.30_pm_r,66786._pm_r,7._pm_r,317._pm_r,.74_pm_r,220._pm_r,4._pm_r,315._pm_r,.05_pm_r,68._pm_r, &
       212.00_pm_r,69947._pm_r,7._pm_r,310._pm_r,.53_pm_r,227._pm_r,4._pm_r,317._pm_r,.09_pm_r,58._pm_r, &
       209.20_pm_r,73027._pm_r,7._pm_r,305._pm_r,.37_pm_r,243._pm_r,4._pm_r,319._pm_r,.14_pm_r,62._pm_r, &
       207.60_pm_r,76080._pm_r,7._pm_r,302._pm_r,.30_pm_r,268._pm_r,4._pm_r,323._pm_r,.18_pm_r,61._pm_r, &
       206.70_pm_r,79111._pm_r,8._pm_r,301._pm_r,.28_pm_r,287._pm_r,4._pm_r,328._pm_r,.20_pm_r,57._pm_r, &
       204.10_pm_r,82133._pm_r,8._pm_r,300._pm_r,.28_pm_r,302._pm_r,4._pm_r,333._pm_r,.21_pm_r,59._pm_r, &
       197.80_pm_r,85088._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       191.00_pm_r,87928._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.10_pm_r,90694._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.70_pm_r,93448._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.70_pm_r,96212._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.20_pm_r,98997._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       190.00_pm_r,101831._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       199.60_pm_r,104787._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       223.60_pm_r,108039._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       281.30_pm_r,111966._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       368.00_pm_r,117181._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_30= (/ &
       293.10_pm_r,-52._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       268.60_pm_r,4063._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       243.50_pm_r,7815._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       221.40_pm_r,11221._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       208.40_pm_r,14370._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       206.70_pm_r,17395._pm_r,9._pm_r,187._pm_r,.47_pm_r,38._pm_r,4._pm_r,305._pm_r,.88_pm_r,92._pm_r, &
       211.30_pm_r,20453._pm_r,8._pm_r,182._pm_r,1.15_pm_r,39._pm_r,2._pm_r,323._pm_r,.99_pm_r,90._pm_r, &
       217.00_pm_r,23592._pm_r,6._pm_r,169._pm_r,2.04_pm_r,38._pm_r,2._pm_r,354._pm_r,.72_pm_r,83._pm_r, &
       221.50_pm_r,26801._pm_r,4._pm_r,131._pm_r,2.91_pm_r,36._pm_r,2._pm_r,12._pm_r,.26_pm_r,35._pm_r, &
       228.10_pm_r,30093._pm_r,6._pm_r,80._pm_r,3.52_pm_r,32._pm_r,3._pm_r,3._pm_r,.66_pm_r,303._pm_r, &
       236.50_pm_r,33492._pm_r,10._pm_r,57._pm_r,3.67_pm_r,26._pm_r,3._pm_r,341._pm_r,1.31_pm_r,292._pm_r, &
       247.00_pm_r,37034._pm_r,15._pm_r,45._pm_r,3.24_pm_r,15._pm_r,5._pm_r,321._pm_r,1.71_pm_r,289._pm_r, &
       258.00_pm_r,40731._pm_r,18._pm_r,37._pm_r,2.58_pm_r,355._pm_r,8._pm_r,311._pm_r,1.80_pm_r,289._pm_r, &
       264.60_pm_r,44566._pm_r,21._pm_r,29._pm_r,2.22_pm_r,330._pm_r,10._pm_r,306._pm_r,1.16_pm_r,289._pm_r, &
       264.60_pm_r,48448._pm_r,22._pm_r,21._pm_r,2.36_pm_r,304._pm_r,11._pm_r,304._pm_r,.42_pm_r,301._pm_r, &
       257.50_pm_r,52274._pm_r,22._pm_r,12._pm_r,2.55_pm_r,284._pm_r,11._pm_r,305._pm_r,.26_pm_r,62._pm_r, &
       247.80_pm_r,55978._pm_r,22._pm_r,2._pm_r,2.41_pm_r,265._pm_r,11._pm_r,307._pm_r,.49_pm_r,82._pm_r, &
       239.40_pm_r,59542._pm_r,21._pm_r,353._pm_r,2.54_pm_r,249._pm_r,10._pm_r,310._pm_r,.40_pm_r,69._pm_r, &
       231.60_pm_r,62994._pm_r,20._pm_r,343._pm_r,2.33_pm_r,241._pm_r,10._pm_r,313._pm_r,.40_pm_r,57._pm_r, &
       223.40_pm_r,66323._pm_r,20._pm_r,334._pm_r,1.79_pm_r,241._pm_r,10._pm_r,317._pm_r,.49_pm_r,51._pm_r, &
       216.70_pm_r,69544._pm_r,20._pm_r,328._pm_r,1.17_pm_r,250._pm_r,10._pm_r,322._pm_r,.62_pm_r,50._pm_r, &
       213.30_pm_r,72688._pm_r,21._pm_r,325._pm_r,.74_pm_r,275._pm_r,10._pm_r,327._pm_r,.74_pm_r,51._pm_r, &
       211.30_pm_r,75797._pm_r,21._pm_r,323._pm_r,.65_pm_r,313._pm_r,10._pm_r,334._pm_r,.83_pm_r,52._pm_r, &
       210.00_pm_r,78879._pm_r,22._pm_r,324._pm_r,.75_pm_r,338._pm_r,10._pm_r,341._pm_r,.88_pm_r,52._pm_r, &
       207.90_pm_r,81948._pm_r,24._pm_r,325._pm_r,.84_pm_r,349._pm_r,11._pm_r,347._pm_r,.86_pm_r,53._pm_r, &
       202.30_pm_r,84968._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       194.50_pm_r,87868._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       188.60_pm_r,90666._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.40_pm_r,93433._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.30_pm_r,96205._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       188.20_pm_r,98999._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       191.30_pm_r,101849._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       201.20_pm_r,104828._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       224.40_pm_r,108098._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       279.50_pm_r,112018._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       363.60_pm_r,117178._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_40= (/ &
       285.90_pm_r,-33._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       260.40_pm_r,3966._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       235.50_pm_r,7596._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       218.80_pm_r,10922._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       214.00_pm_r,14090._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       213.20_pm_r,17215._pm_r,10._pm_r,270._pm_r,3.69_pm_r,167._pm_r,6._pm_r,353._pm_r,.74_pm_r,131._pm_r, &
       214.40_pm_r,20345._pm_r,10._pm_r,234._pm_r,4.80_pm_r,156._pm_r,5._pm_r,2._pm_r,.75_pm_r,122._pm_r, &
       216.70_pm_r,23503._pm_r,13._pm_r,199._pm_r,5.28_pm_r,138._pm_r,5._pm_r,12._pm_r,.42_pm_r,88._pm_r, &
       218.50_pm_r,26688._pm_r,17._pm_r,172._pm_r,5.58_pm_r,112._pm_r,5._pm_r,14._pm_r,.67_pm_r,355._pm_r, &
       223.10_pm_r,29921._pm_r,21._pm_r,149._pm_r,6.12_pm_r,84._pm_r,7._pm_r,6._pm_r,1.57_pm_r,332._pm_r, &
       230.00_pm_r,33235._pm_r,25._pm_r,127._pm_r,6.88_pm_r,60._pm_r,9._pm_r,355._pm_r,2.43_pm_r,325._pm_r, &
       238.40_pm_r,36667._pm_r,29._pm_r,107._pm_r,7.14_pm_r,40._pm_r,13._pm_r,345._pm_r,2.96_pm_r,319._pm_r, &
       248.50_pm_r,40228._pm_r,33._pm_r,89._pm_r,6.77_pm_r,20._pm_r,17._pm_r,338._pm_r,3.04_pm_r,314._pm_r, &
       258.20_pm_r,43946._pm_r,36._pm_r,75._pm_r,5.25_pm_r,0._pm_r,20._pm_r,333._pm_r,2.02_pm_r,307._pm_r, &
       260.30_pm_r,47753._pm_r,37._pm_r,64._pm_r,4.39_pm_r,335._pm_r,22._pm_r,330._pm_r,.90_pm_r,299._pm_r, &
       254.20_pm_r,51526._pm_r,36._pm_r,54._pm_r,4.19_pm_r,314._pm_r,22._pm_r,329._pm_r,.07_pm_r,135._pm_r, &
       244.30_pm_r,55182._pm_r,35._pm_r,45._pm_r,3.80_pm_r,302._pm_r,22._pm_r,330._pm_r,.65_pm_r,111._pm_r, &
       235.50_pm_r,58690._pm_r,33._pm_r,36._pm_r,3.85_pm_r,291._pm_r,21._pm_r,331._pm_r,.43_pm_r,119._pm_r, &
       230.00_pm_r,62097._pm_r,32._pm_r,27._pm_r,3.40_pm_r,283._pm_r,21._pm_r,332._pm_r,.19_pm_r,122._pm_r, &
       225.90_pm_r,65435._pm_r,31._pm_r,19._pm_r,2.64_pm_r,278._pm_r,21._pm_r,332._pm_r,.08_pm_r,33._pm_r, &
       222.10_pm_r,68715._pm_r,31._pm_r,13._pm_r,1.83_pm_r,275._pm_r,21._pm_r,333._pm_r,.26_pm_r,9._pm_r, &
       220.40_pm_r,71954._pm_r,30._pm_r,9._pm_r,1.12_pm_r,274._pm_r,22._pm_r,333._pm_r,.44_pm_r,10._pm_r, &
       218.50_pm_r,75169._pm_r,30._pm_r,7._pm_r,.57_pm_r,274._pm_r,22._pm_r,335._pm_r,.58_pm_r,12._pm_r, &
       216.30_pm_r,78351._pm_r,30._pm_r,6._pm_r,.18_pm_r,276._pm_r,23._pm_r,336._pm_r,.66_pm_r,13._pm_r, &
       214.20_pm_r,81505._pm_r,30._pm_r,5._pm_r,.07_pm_r,74._pm_r,24._pm_r,337._pm_r,.69_pm_r,13._pm_r, &
       209.10_pm_r,84618._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       200.20_pm_r,87617._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       192.00_pm_r,90480._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       189.70_pm_r,93287._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       189.30_pm_r,96087._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       190.70_pm_r,98912._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       194.80_pm_r,101803._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       205.20_pm_r,104836._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       227.70_pm_r,108161._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       278.60_pm_r,112100._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       359.50_pm_r,117210._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_50= (/ &
       279.60_pm_r,-87._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       253.60_pm_r,3812._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       230.00_pm_r,7349._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       218.30_pm_r,10628._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       217.10_pm_r,13812._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       216.60_pm_r,16988._pm_r,16._pm_r,309._pm_r,7.59_pm_r,194._pm_r,9._pm_r,33._pm_r,.52_pm_r,6._pm_r, &
       215.90_pm_r,20156._pm_r,15._pm_r,259._pm_r,10.25_pm_r,186._pm_r,10._pm_r,30._pm_r,.88_pm_r,5._pm_r, &
       214.90_pm_r,23309._pm_r,24._pm_r,218._pm_r,11.45_pm_r,174._pm_r,11._pm_r,26._pm_r,1.30_pm_r,2._pm_r, &
       214.60_pm_r,26452._pm_r,37._pm_r,197._pm_r,11.14_pm_r,156._pm_r,13._pm_r,22._pm_r,1.67_pm_r,357._pm_r, &
       216.80_pm_r,29610._pm_r,48._pm_r,182._pm_r,10.36_pm_r,130._pm_r,16._pm_r,17._pm_r,1.92_pm_r,350._pm_r, &
       220.10_pm_r,32807._pm_r,55._pm_r,168._pm_r,10.40_pm_r,100._pm_r,18._pm_r,12._pm_r,2.07_pm_r,342._pm_r, &
       226.70_pm_r,36078._pm_r,59._pm_r,153._pm_r,10.69_pm_r,73._pm_r,21._pm_r,8._pm_r,2.06_pm_r,332._pm_r, &
       235.80_pm_r,39460._pm_r,61._pm_r,138._pm_r,10.47_pm_r,50._pm_r,23._pm_r,3._pm_r,1.94_pm_r,323._pm_r, &
       245.60_pm_r,42991._pm_r,61._pm_r,125._pm_r,8.55_pm_r,33._pm_r,25._pm_r,358._pm_r,1.58_pm_r,299._pm_r, &
       252.50_pm_r,46641._pm_r,60._pm_r,115._pm_r,6.89_pm_r,15._pm_r,25._pm_r,354._pm_r,1.20_pm_r,272._pm_r, &
       252.80_pm_r,50349._pm_r,58._pm_r,106._pm_r,5.92_pm_r,357._pm_r,25._pm_r,351._pm_r,.85_pm_r,228._pm_r, &
       246.70_pm_r,54013._pm_r,55._pm_r,98._pm_r,5.25_pm_r,342._pm_r,24._pm_r,349._pm_r,1.03_pm_r,169._pm_r, &
       240.10_pm_r,57574._pm_r,50._pm_r,91._pm_r,5.49_pm_r,319._pm_r,23._pm_r,350._pm_r,1.01_pm_r,148._pm_r, &
       234.80_pm_r,61052._pm_r,45._pm_r,85._pm_r,5.54_pm_r,303._pm_r,21._pm_r,352._pm_r,.94_pm_r,127._pm_r, &
       231.30_pm_r,64461._pm_r,38._pm_r,79._pm_r,5.19_pm_r,291._pm_r,21._pm_r,355._pm_r,.86_pm_r,103._pm_r, &
       228.80_pm_r,67830._pm_r,32._pm_r,73._pm_r,4.58_pm_r,281._pm_r,21._pm_r,359._pm_r,.89_pm_r,81._pm_r, &
       226.20_pm_r,71161._pm_r,26._pm_r,67._pm_r,3.93_pm_r,272._pm_r,21._pm_r,2._pm_r,.97_pm_r,65._pm_r, &
       223.70_pm_r,74456._pm_r,21._pm_r,62._pm_r,3.38_pm_r,263._pm_r,22._pm_r,5._pm_r,1.07_pm_r,55._pm_r, &
       221.50_pm_r,77716._pm_r,17._pm_r,57._pm_r,2.93_pm_r,255._pm_r,23._pm_r,8._pm_r,1.12_pm_r,48._pm_r, &
       219.40_pm_r,80944._pm_r,13._pm_r,53._pm_r,2.52_pm_r,248._pm_r,24._pm_r,11._pm_r,1.11_pm_r,44._pm_r, &
       215.10_pm_r,84138._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       206.80_pm_r,87242._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       197.60_pm_r,90196._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       193.60_pm_r,93069._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       192.60_pm_r,95918._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       194.30_pm_r,98792._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       199.50_pm_r,101743._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       210.60_pm_r,104851._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       232.20_pm_r,108252._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       277.70_pm_r,112224._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       355.80_pm_r,117278._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_60= (/ &
       268.70_pm_r,-94._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       249.10_pm_r,3690._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       226.50_pm_r,7165._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       217.60_pm_r,10410._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       216.90_pm_r,13585._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       216.00_pm_r,16756._pm_r,13._pm_r,317._pm_r,9.36_pm_r,212._pm_r,13._pm_r,40._pm_r,1.33_pm_r,18._pm_r, &
       214.20_pm_r,19908._pm_r,17._pm_r,254._pm_r,12.95_pm_r,206._pm_r,15._pm_r,36._pm_r,1.90_pm_r,16._pm_r, &
       210.90_pm_r,23019._pm_r,34._pm_r,225._pm_r,14.92_pm_r,195._pm_r,18._pm_r,32._pm_r,2.25_pm_r,13._pm_r, &
       208.60_pm_r,26088._pm_r,53._pm_r,211._pm_r,14.05_pm_r,180._pm_r,21._pm_r,29._pm_r,2.14_pm_r,8._pm_r, &
       210.40_pm_r,29154._pm_r,68._pm_r,200._pm_r,12.10_pm_r,155._pm_r,23._pm_r,26._pm_r,1.68_pm_r,356._pm_r, &
       214.10_pm_r,32260._pm_r,78._pm_r,189._pm_r,11.39_pm_r,122._pm_r,25._pm_r,23._pm_r,1.20_pm_r,332._pm_r, &
       220.70_pm_r,35443._pm_r,81._pm_r,177._pm_r,11.91_pm_r,92._pm_r,26._pm_r,20._pm_r,.98_pm_r,293._pm_r, &
       229.70_pm_r,38736._pm_r,82._pm_r,165._pm_r,12.09_pm_r,71._pm_r,25._pm_r,17._pm_r,1.05_pm_r,262._pm_r, &
       239.60_pm_r,42177._pm_r,80._pm_r,154._pm_r,10.00_pm_r,57._pm_r,24._pm_r,13._pm_r,1.18_pm_r,242._pm_r, &
       249.00_pm_r,45754._pm_r,78._pm_r,144._pm_r,8.05_pm_r,44._pm_r,23._pm_r,11._pm_r,1.15_pm_r,230._pm_r, &
       254.10_pm_r,49447._pm_r,76._pm_r,136._pm_r,6.89_pm_r,32._pm_r,22._pm_r,8._pm_r,1.08_pm_r,213._pm_r, &
       251.90_pm_r,53159._pm_r,73._pm_r,129._pm_r,6.28_pm_r,20._pm_r,20._pm_r,7._pm_r,1.09_pm_r,192._pm_r, &
       247.60_pm_r,56815._pm_r,69._pm_r,123._pm_r,6.03_pm_r,354._pm_r,19._pm_r,8._pm_r,.90_pm_r,171._pm_r, &
       243.20_pm_r,60410._pm_r,62._pm_r,118._pm_r,6.11_pm_r,331._pm_r,18._pm_r,10._pm_r,.72_pm_r,142._pm_r, &
       239.20_pm_r,63942._pm_r,54._pm_r,114._pm_r,6.08_pm_r,314._pm_r,17._pm_r,13._pm_r,.72_pm_r,107._pm_r, &
       235.00_pm_r,67415._pm_r,46._pm_r,111._pm_r,5.89_pm_r,300._pm_r,18._pm_r,16._pm_r,.88_pm_r,81._pm_r, &
       231.40_pm_r,70829._pm_r,37._pm_r,111._pm_r,5.67_pm_r,289._pm_r,18._pm_r,20._pm_r,1.09_pm_r,67._pm_r, &
       227.70_pm_r,74191._pm_r,29._pm_r,113._pm_r,5.47_pm_r,278._pm_r,20._pm_r,23._pm_r,1.26_pm_r,59._pm_r, &
       224.60_pm_r,77498._pm_r,22._pm_r,119._pm_r,5.24_pm_r,270._pm_r,21._pm_r,26._pm_r,1.35_pm_r,56._pm_r, &
       222.60_pm_r,80764._pm_r,16._pm_r,133._pm_r,4.88_pm_r,264._pm_r,23._pm_r,29._pm_r,1.33_pm_r,53._pm_r, &
       220.00_pm_r,84014._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       213.40_pm_r,87217._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       203.70_pm_r,90272._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       198.20_pm_r,93221._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       196.80_pm_r,96132._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       198.50_pm_r,99065._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       204.80_pm_r,102085._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       216.40_pm_r,105275._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       236.90_pm_r,108756._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       276.20_pm_r,112756._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       352.20_pm_r,117750._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_70= (/ &
       257.20_pm_r,-80._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       245.40_pm_r,3590._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       223.80_pm_r,7016._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       215.90_pm_r,10227._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       214.50_pm_r,13370._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       212.70_pm_r,16499._pm_r,4._pm_r,313._pm_r,7.63_pm_r,227._pm_r,10._pm_r,32._pm_r,1.50_pm_r,57._pm_r, &
       209.80_pm_r,19595._pm_r,14._pm_r,241._pm_r,10.74_pm_r,220._pm_r,12._pm_r,37._pm_r,2.00_pm_r,52._pm_r, &
       205.00_pm_r,22631._pm_r,30._pm_r,226._pm_r,12.46_pm_r,208._pm_r,15._pm_r,39._pm_r,2.10_pm_r,43._pm_r, &
       200.30_pm_r,25598._pm_r,48._pm_r,217._pm_r,12.26_pm_r,192._pm_r,18._pm_r,38._pm_r,1.70_pm_r,25._pm_r, &
       201.00_pm_r,28530._pm_r,62._pm_r,207._pm_r,10.95_pm_r,166._pm_r,20._pm_r,35._pm_r,1.29_pm_r,341._pm_r, &
       207.60_pm_r,31516._pm_r,72._pm_r,197._pm_r,10.36_pm_r,135._pm_r,20._pm_r,30._pm_r,1.63_pm_r,294._pm_r, &
       216.70_pm_r,34625._pm_r,76._pm_r,186._pm_r,10.57_pm_r,108._pm_r,19._pm_r,22._pm_r,2.10_pm_r,271._pm_r, &
       226.90_pm_r,37869._pm_r,79._pm_r,175._pm_r,10.34_pm_r,88._pm_r,18._pm_r,13._pm_r,2.19_pm_r,257._pm_r, &
       238.10_pm_r,41278._pm_r,79._pm_r,165._pm_r,8.28_pm_r,74._pm_r,17._pm_r,5._pm_r,1.57_pm_r,242._pm_r, &
       249.20_pm_r,44844._pm_r,78._pm_r,157._pm_r,6.73_pm_r,63._pm_r,16._pm_r,360._pm_r,.92_pm_r,225._pm_r, &
       257.50_pm_r,48563._pm_r,77._pm_r,150._pm_r,5.94_pm_r,52._pm_r,15._pm_r,358._pm_r,.43_pm_r,197._pm_r, &
       258.30_pm_r,52348._pm_r,76._pm_r,144._pm_r,5.45_pm_r,41._pm_r,14._pm_r,358._pm_r,.23_pm_r,128._pm_r, &
       254.50_pm_r,56105._pm_r,73._pm_r,139._pm_r,4.80_pm_r,13._pm_r,14._pm_r,360._pm_r,.45_pm_r,100._pm_r, &
       248.20_pm_r,59789._pm_r,68._pm_r,135._pm_r,4.72_pm_r,347._pm_r,14._pm_r,3._pm_r,.60_pm_r,90._pm_r, &
       241.50_pm_r,63372._pm_r,62._pm_r,133._pm_r,4.81_pm_r,327._pm_r,14._pm_r,6._pm_r,.62_pm_r,85._pm_r, &
       236.30_pm_r,66870._pm_r,55._pm_r,132._pm_r,4.83_pm_r,313._pm_r,15._pm_r,10._pm_r,.53_pm_r,80._pm_r, &
       232.20_pm_r,70299._pm_r,48._pm_r,133._pm_r,4.78_pm_r,302._pm_r,15._pm_r,12._pm_r,.41_pm_r,77._pm_r, &
       228.70_pm_r,73674._pm_r,41._pm_r,135._pm_r,4.73_pm_r,292._pm_r,15._pm_r,14._pm_r,.26_pm_r,70._pm_r, &
       227.10_pm_r,77008._pm_r,35._pm_r,140._pm_r,4.63_pm_r,286._pm_r,15._pm_r,15._pm_r,.14_pm_r,63._pm_r, &
       226.00_pm_r,80322._pm_r,30._pm_r,148._pm_r,4.39_pm_r,281._pm_r,15._pm_r,15._pm_r,.06_pm_r,38._pm_r, &
       224.50_pm_r,83622._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       219.10_pm_r,86892._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       209.20_pm_r,90032._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       202.80_pm_r,93051._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       200.70_pm_r,96023._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       202.80_pm_r,99012._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       209.50_pm_r,102097._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       221.40_pm_r,105360._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       240.50_pm_r,108908._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       274.10_pm_r,112921._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       349.30_pm_r,117856._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/) 
  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_80= (/ &
       251.70_pm_r,-111._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       242.50_pm_r,3496._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       221.90_pm_r,6886._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       214.80_pm_r,10073._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       212.10_pm_r,13189._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       209.30_pm_r,16273._pm_r,2._pm_r,179._pm_r,3.81_pm_r,243._pm_r,4._pm_r,11._pm_r,.86_pm_r,93._pm_r, &
       205.70_pm_r,19316._pm_r,8._pm_r,227._pm_r,5.49_pm_r,234._pm_r,5._pm_r,28._pm_r,1.05_pm_r,89._pm_r, &
       199.70_pm_r,22282._pm_r,17._pm_r,227._pm_r,6.52_pm_r,220._pm_r,6._pm_r,40._pm_r,.87_pm_r,78._pm_r, &
       193.50_pm_r,25161._pm_r,26._pm_r,221._pm_r,6.71_pm_r,200._pm_r,7._pm_r,44._pm_r,.45_pm_r,33._pm_r, &
       194.70_pm_r,27993._pm_r,35._pm_r,213._pm_r,6.35_pm_r,176._pm_r,7._pm_r,38._pm_r,.77_pm_r,313._pm_r, &
       204.90_pm_r,30909._pm_r,41._pm_r,203._pm_r,6.13_pm_r,149._pm_r,7._pm_r,25._pm_r,1.34_pm_r,294._pm_r, &
       216.40_pm_r,33999._pm_r,45._pm_r,193._pm_r,6.09_pm_r,128._pm_r,7._pm_r,8._pm_r,1.57_pm_r,287._pm_r, &
       227.20_pm_r,37244._pm_r,49._pm_r,183._pm_r,5.77_pm_r,110._pm_r,8._pm_r,350._pm_r,1.45_pm_r,280._pm_r, &
       237.90_pm_r,40655._pm_r,51._pm_r,175._pm_r,4.33_pm_r,98._pm_r,8._pm_r,340._pm_r,.72_pm_r,273._pm_r, &
       248.70_pm_r,44214._pm_r,52._pm_r,169._pm_r,3.21_pm_r,84._pm_r,8._pm_r,336._pm_r,.12_pm_r,318._pm_r, &
       258.00_pm_r,47932._pm_r,52._pm_r,164._pm_r,2.77_pm_r,66._pm_r,9._pm_r,338._pm_r,.43_pm_r,43._pm_r, &
       260.30_pm_r,51735._pm_r,51._pm_r,159._pm_r,2.79_pm_r,49._pm_r,9._pm_r,343._pm_r,.71_pm_r,34._pm_r, &
       257.20_pm_r,55531._pm_r,48._pm_r,155._pm_r,2.78_pm_r,27._pm_r,10._pm_r,348._pm_r,.63_pm_r,40._pm_r, &
       250.20_pm_r,59251._pm_r,46._pm_r,152._pm_r,2.77_pm_r,8._pm_r,10._pm_r,351._pm_r,.49_pm_r,47._pm_r, &
       242.40_pm_r,62855._pm_r,42._pm_r,149._pm_r,2.67_pm_r,352._pm_r,11._pm_r,354._pm_r,.33_pm_r,64._pm_r, &
       237.00_pm_r,66364._pm_r,38._pm_r,148._pm_r,2.54_pm_r,338._pm_r,11._pm_r,357._pm_r,.24_pm_r,95._pm_r, &
       233.20_pm_r,69806._pm_r,35._pm_r,147._pm_r,2.46_pm_r,326._pm_r,10._pm_r,358._pm_r,.24_pm_r,138._pm_r, &
       230.20_pm_r,73198._pm_r,31._pm_r,148._pm_r,2.42_pm_r,315._pm_r,10._pm_r,359._pm_r,.34_pm_r,163._pm_r, &
       229.70_pm_r,76563._pm_r,28._pm_r,150._pm_r,2.40_pm_r,307._pm_r,10._pm_r,360._pm_r,.41_pm_r,172._pm_r, &
       229.10_pm_r,79924._pm_r,25._pm_r,154._pm_r,2.31_pm_r,301._pm_r,9._pm_r,360._pm_r,.46_pm_r,179._pm_r, &
       227.70_pm_r,83265._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       222.70_pm_r,86579._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       212.90_pm_r,89774._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       206.00_pm_r,92844._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       203.50_pm_r,95858._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       205.70_pm_r,98889._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       212.80_pm_r,102019._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       224.80_pm_r,105332._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       242.60_pm_r,108923._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       272.10_pm_r,112937._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       347.20_pm_r,117828._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_reel), dimension(10*pm_nb_lat*pm_nb_alt), parameter :: donnees_decembre = &
       (/donnees_lat_moins_80(:),donnees_lat_moins_70(:),donnees_lat_moins_60(:),donnees_lat_moins_50(:),&
       donnees_lat_moins_40(:),donnees_lat_moins_30(:),donnees_lat_moins_20(:),donnees_lat_moins_10(:),&
       donnees_lat_0(:),donnees_lat_10(:),donnees_lat_20(:),donnees_lat_30(:),donnees_lat_40(:),donnees_lat_50(:),&
       donnees_lat_60(:),donnees_lat_70(:),donnees_lat_80(:)/)

real(pm_reel),dimension(10,pm_nb_alt,pm_nb_lat)::donnees

  character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
       '@(#) Fichier MSPRO mpi_atmi_decembre.f90: derniere modification V5.15 >'

  !************************************************************************

  ! initialisation de la valeur du code retour
  ! ..........................................
 donnees=reshape(donnees_decembre,(/10,pm_nb_alt,pm_nb_lat/))
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

end subroutine mpi_atmi_decembre
