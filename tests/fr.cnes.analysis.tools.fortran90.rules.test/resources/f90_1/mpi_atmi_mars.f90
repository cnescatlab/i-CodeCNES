subroutine mpi_atmi_mars (tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2, retour )

! (C) Copyright CNES - MSPRO - 2000

!************************************************************************
!
! But: Lecture du fichier ATMospherique pour l'Interplation correspondant au mois de MARS 
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
integer, intent(out)                          ::  retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Autres declarations
  ! -------------------

  real(pm_reel), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_80= (/ &
264.10_pm_r,  -245._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
245.30_pm_r,  3473._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
224.40_pm_r,  6901._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
224.50_pm_r, 10177._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
225.70_pm_r, 13463._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
225.10_pm_r, 16764._pm_r,   6._pm_r,277._pm_r,  .94_pm_r,190._pm_r,   1._pm_r,117._pm_r,  .04_pm_r,353._pm_r, &
224.20_pm_r, 20055._pm_r,   7._pm_r,263._pm_r, 1.22_pm_r,184._pm_r,   1._pm_r,115._pm_r,  .03_pm_r,342._pm_r, &
222.80_pm_r, 23329._pm_r,   7._pm_r,247._pm_r, 1.29_pm_r,173._pm_r,   1._pm_r,114._pm_r,  .01_pm_r,315._pm_r, &
221.30_pm_r, 26578._pm_r,   8._pm_r,234._pm_r, 1.14_pm_r,154._pm_r,   1._pm_r,115._pm_r,  .04_pm_r,194._pm_r, &
225.50_pm_r, 29844._pm_r,   8._pm_r,222._pm_r, 1.02_pm_r,119._pm_r,   1._pm_r,120._pm_r,  .11_pm_r,185._pm_r, &
231.40_pm_r, 33189._pm_r,   7._pm_r,211._pm_r, 1.22_pm_r, 83._pm_r,   2._pm_r,126._pm_r,  .16_pm_r,180._pm_r, &
239.20_pm_r, 36634._pm_r,   6._pm_r,197._pm_r, 1.49_pm_r, 62._pm_r,   2._pm_r,134._pm_r,  .20_pm_r,179._pm_r, &
248.80_pm_r, 40204._pm_r,   4._pm_r,176._pm_r, 1.56_pm_r, 49._pm_r,   2._pm_r,140._pm_r,  .19_pm_r,174._pm_r, &
258.00_pm_r, 43919._pm_r,   3._pm_r,145._pm_r, 1.31_pm_r, 37._pm_r,   2._pm_r,143._pm_r,  .04_pm_r,196._pm_r, &
263.60_pm_r, 47742._pm_r,   3._pm_r,112._pm_r,  .88_pm_r, 21._pm_r,   2._pm_r,143._pm_r,  .12_pm_r,340._pm_r, &
261.80_pm_r, 51597._pm_r,   3._pm_r, 93._pm_r,  .45_pm_r,334._pm_r,   2._pm_r,140._pm_r,  .12_pm_r,355._pm_r, &
255.00_pm_r, 55385._pm_r,   2._pm_r, 86._pm_r,  .66_pm_r,265._pm_r,   2._pm_r,137._pm_r,  .08_pm_r, 97._pm_r, &
248.10_pm_r, 59067._pm_r,   1._pm_r,100._pm_r,  .92_pm_r,250._pm_r,   2._pm_r,136._pm_r,  .28_pm_r,135._pm_r, &
241.20_pm_r, 62653._pm_r,   1._pm_r,215._pm_r,  .98_pm_r,246._pm_r,   3._pm_r,136._pm_r,  .38_pm_r,138._pm_r, &
232.30_pm_r, 66122._pm_r,   2._pm_r,234._pm_r,  .87_pm_r,246._pm_r,   3._pm_r,136._pm_r,  .36_pm_r,139._pm_r, &
223.30_pm_r, 69459._pm_r,   3._pm_r,239._pm_r,  .68_pm_r,251._pm_r,   4._pm_r,137._pm_r,  .30_pm_r,138._pm_r, &
215.80_pm_r, 72670._pm_r,   4._pm_r,242._pm_r,  .50_pm_r,261._pm_r,   4._pm_r,137._pm_r,  .19_pm_r,134._pm_r, &
209.60_pm_r, 75788._pm_r,   5._pm_r,246._pm_r,  .36_pm_r,276._pm_r,   4._pm_r,136._pm_r,  .11_pm_r,124._pm_r, &
203.90_pm_r, 78815._pm_r,   5._pm_r,249._pm_r,  .28_pm_r,296._pm_r,   4._pm_r,136._pm_r,  .05_pm_r,107._pm_r, &
193.00_pm_r, 81761._pm_r,   5._pm_r,253._pm_r,  .25_pm_r,313._pm_r,   4._pm_r,135._pm_r,  .02_pm_r, 63._pm_r, &
176.80_pm_r, 84426._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
166.40_pm_r, 86891._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
163.00_pm_r, 89287._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
164.80_pm_r, 91689._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
171.70_pm_r, 94166._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
184.00_pm_r, 96797._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
203.00_pm_r, 99688._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
230.80_pm_r,102969._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
264.30_pm_r,106778._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
299.30_pm_r,111189._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
377.70_pm_r,116534._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_70= (/ &
269.90_pm_r,  -184._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
248.40_pm_r,  3601._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
226.60_pm_r,  7069._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
224.40_pm_r, 10362._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
225.70_pm_r, 13649._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
225.00_pm_r, 16950._pm_r,   7._pm_r,259._pm_r,  .84_pm_r,177._pm_r,   3._pm_r,119._pm_r,  .09_pm_r,223._pm_r, &
223.70_pm_r, 20236._pm_r,   7._pm_r,248._pm_r, 1.07_pm_r,165._pm_r,   3._pm_r,122._pm_r,  .14_pm_r,218._pm_r, &
222.30_pm_r, 23502._pm_r,   7._pm_r,234._pm_r, 1.17_pm_r,145._pm_r,   3._pm_r,126._pm_r,  .17_pm_r,215._pm_r, &
222.10_pm_r, 26751._pm_r,   7._pm_r,220._pm_r, 1.28_pm_r,115._pm_r,   3._pm_r,131._pm_r,  .18_pm_r,208._pm_r, &
227.30_pm_r, 30037._pm_r,   6._pm_r,203._pm_r, 1.61_pm_r, 84._pm_r,   3._pm_r,136._pm_r,  .17_pm_r,197._pm_r, &
233.60_pm_r, 33411._pm_r,   5._pm_r,177._pm_r, 2.07_pm_r, 63._pm_r,   3._pm_r,139._pm_r,  .14_pm_r,180._pm_r, &
241.10_pm_r, 36887._pm_r,   4._pm_r,135._pm_r, 2.32_pm_r, 49._pm_r,   4._pm_r,140._pm_r,  .11_pm_r,149._pm_r, &
250.60_pm_r, 40483._pm_r,   5._pm_r, 95._pm_r, 2.25_pm_r, 36._pm_r,   4._pm_r,139._pm_r,  .11_pm_r,108._pm_r, &
259.90_pm_r, 44227._pm_r,   7._pm_r, 71._pm_r, 1.95_pm_r, 15._pm_r,   4._pm_r,138._pm_r,  .10_pm_r, 58._pm_r, &
264.50_pm_r, 48071._pm_r,   8._pm_r, 54._pm_r, 1.62_pm_r,349._pm_r,   4._pm_r,135._pm_r,  .11_pm_r,  3._pm_r, &
260.50_pm_r, 51923._pm_r,   9._pm_r, 40._pm_r, 1.32_pm_r,314._pm_r,   4._pm_r,134._pm_r,  .17_pm_r,318._pm_r, &
251.70_pm_r, 55678._pm_r,   9._pm_r, 29._pm_r, 1.23_pm_r,269._pm_r,   3._pm_r,135._pm_r,  .24_pm_r,293._pm_r, &
244.40_pm_r, 59308._pm_r,   7._pm_r, 19._pm_r, 1.33_pm_r,239._pm_r,   3._pm_r,138._pm_r,  .19_pm_r,287._pm_r, &
237.10_pm_r, 62837._pm_r,   6._pm_r,  9._pm_r, 1.34_pm_r,222._pm_r,   3._pm_r,141._pm_r,  .13_pm_r,268._pm_r, &
229.30_pm_r, 66250._pm_r,   4._pm_r,357._pm_r, 1.16_pm_r,209._pm_r,   3._pm_r,144._pm_r,  .09_pm_r,224._pm_r, &
221.60_pm_r, 69553._pm_r,   3._pm_r,344._pm_r,  .89_pm_r,197._pm_r,   3._pm_r,146._pm_r,  .13_pm_r,185._pm_r, &
214.90_pm_r, 72746._pm_r,   2._pm_r,329._pm_r,  .63_pm_r,182._pm_r,   3._pm_r,148._pm_r,  .16_pm_r,169._pm_r, &
209.00_pm_r, 75852._pm_r,   1._pm_r,315._pm_r,  .46_pm_r,162._pm_r,   3._pm_r,149._pm_r,  .19_pm_r,163._pm_r, &
203.60_pm_r, 78872._pm_r,   1._pm_r,302._pm_r,  .37_pm_r,139._pm_r,   4._pm_r,150._pm_r,  .20_pm_r,161._pm_r, &
193.90_pm_r, 81816._pm_r,   0._pm_r,284._pm_r,  .34_pm_r,120._pm_r,   4._pm_r,151._pm_r,  .20_pm_r,159._pm_r, &
179.40_pm_r, 84514._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
169.40_pm_r, 87027._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
165.40_pm_r, 89465._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
166.60_pm_r, 91899._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
172.80_pm_r, 94398._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
184.00_pm_r, 97039._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
201.70_pm_r, 99923._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
227.90_pm_r,103172._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
261.40_pm_r,106934._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
300.00_pm_r,111328._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
377.80_pm_r,116692._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_60= (/ &
275.70_pm_r,  -120._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
254.60_pm_r,  3755._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
231.10_pm_r,  7304._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
223.20_pm_r, 10624._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
223.40_pm_r, 13887._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
222.80_pm_r, 17157._pm_r,   8._pm_r,238._pm_r,  .78_pm_r,120._pm_r,   4._pm_r,102._pm_r,  .28_pm_r,214._pm_r, &
222.30_pm_r, 20415._pm_r,   7._pm_r,230._pm_r, 1.11_pm_r,108._pm_r,   4._pm_r,109._pm_r,  .38_pm_r,213._pm_r, &
222.90_pm_r, 23675._pm_r,   6._pm_r,217._pm_r, 1.41_pm_r, 93._pm_r,   3._pm_r,119._pm_r,  .45_pm_r,213._pm_r, &
224.30_pm_r, 26947._pm_r,   5._pm_r,198._pm_r, 1.68_pm_r, 74._pm_r,   3._pm_r,130._pm_r,  .42_pm_r,211._pm_r, &
229.10_pm_r, 30264._pm_r,   4._pm_r,167._pm_r, 1.94_pm_r, 56._pm_r,   4._pm_r,139._pm_r,  .32_pm_r,209._pm_r, &
236.60_pm_r, 33671._pm_r,   4._pm_r,119._pm_r, 2.15_pm_r, 39._pm_r,   4._pm_r,144._pm_r,  .17_pm_r,201._pm_r, &
244.80_pm_r, 37198._pm_r,   5._pm_r, 77._pm_r, 2.19_pm_r, 25._pm_r,   4._pm_r,145._pm_r,  .05_pm_r,127._pm_r, &
254.20_pm_r, 40848._pm_r,   7._pm_r, 55._pm_r, 2.04_pm_r, 10._pm_r,   4._pm_r,144._pm_r,  .14_pm_r, 60._pm_r, &
263.20_pm_r, 44644._pm_r,   9._pm_r, 40._pm_r, 1.74_pm_r,349._pm_r,   4._pm_r,139._pm_r,  .27_pm_r, 41._pm_r, &
265.70_pm_r, 48524._pm_r,  10._pm_r, 28._pm_r, 1.50_pm_r,321._pm_r,   4._pm_r,132._pm_r,  .31_pm_r, 27._pm_r, &
259.10_pm_r, 52373._pm_r,  10._pm_r, 17._pm_r, 1.34_pm_r,285._pm_r,   4._pm_r,126._pm_r,  .23_pm_r,  8._pm_r, &
248.90_pm_r, 56096._pm_r,  10._pm_r,  6._pm_r, 1.38_pm_r,245._pm_r,   3._pm_r,124._pm_r,  .09_pm_r,288._pm_r, &
241.20_pm_r, 59682._pm_r,   9._pm_r,357._pm_r, 1.34_pm_r,218._pm_r,   3._pm_r,127._pm_r,  .20_pm_r,214._pm_r, &
233.20_pm_r, 63158._pm_r,   7._pm_r,350._pm_r, 1.19_pm_r,196._pm_r,   4._pm_r,133._pm_r,  .28_pm_r,197._pm_r, &
226.20_pm_r, 66519._pm_r,   6._pm_r,345._pm_r,  .91_pm_r,172._pm_r,   4._pm_r,138._pm_r,  .28_pm_r,187._pm_r, &
220.10_pm_r, 69788._pm_r,   5._pm_r,347._pm_r,  .71_pm_r,141._pm_r,   4._pm_r,142._pm_r,  .24_pm_r,178._pm_r, &
213.50_pm_r, 72962._pm_r,   4._pm_r,358._pm_r,  .70_pm_r,109._pm_r,   4._pm_r,144._pm_r,  .17_pm_r,165._pm_r, &
207.60_pm_r, 76047._pm_r,   4._pm_r, 14._pm_r,  .80_pm_r, 86._pm_r,   4._pm_r,145._pm_r,  .12_pm_r,145._pm_r, &
202.30_pm_r, 79045._pm_r,   4._pm_r, 29._pm_r,  .88_pm_r, 76._pm_r,   5._pm_r,144._pm_r,  .11_pm_r,131._pm_r, &
194.40_pm_r, 81970._pm_r,   5._pm_r, 39._pm_r,  .89_pm_r, 70._pm_r,   5._pm_r,144._pm_r,  .09_pm_r,117._pm_r, &
182.80_pm_r, 84710._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
173.80_pm_r, 87293._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
169.20_pm_r, 89793._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
169.60_pm_r, 92280._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
174.60_pm_r, 94817._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
184.20_pm_r, 97475._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
199.90_pm_r,100349._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
223.40_pm_r,103552._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
256.50_pm_r,107242._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
300.20_pm_r,111598._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
377.90_pm_r,116979._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)       

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_50= (/ &
281.50_pm_r,   -57._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
262.20_pm_r,  3919._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
237.60_pm_r,  7575._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
221.80_pm_r, 10934._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
218.70_pm_r, 14156._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
218.70_pm_r, 17357._pm_r,   6._pm_r,219._pm_r,  .62_pm_r, 86._pm_r,   2._pm_r, 95._pm_r,  .21_pm_r,219._pm_r, &
220.00_pm_r, 20569._pm_r,   6._pm_r,211._pm_r,  .87_pm_r, 76._pm_r,   2._pm_r,104._pm_r,  .29_pm_r,220._pm_r, &
222.50_pm_r, 23810._pm_r,   5._pm_r,200._pm_r, 1.07_pm_r, 63._pm_r,   2._pm_r,117._pm_r,  .31_pm_r,220._pm_r, &
225.00_pm_r, 27084._pm_r,   3._pm_r,183._pm_r, 1.21_pm_r, 47._pm_r,   2._pm_r,131._pm_r,  .28_pm_r,224._pm_r, &
230.20_pm_r, 30415._pm_r,   2._pm_r,152._pm_r, 1.30_pm_r, 30._pm_r,   2._pm_r,143._pm_r,  .19_pm_r,227._pm_r, &
238.80_pm_r, 33845._pm_r,   2._pm_r, 91._pm_r, 1.34_pm_r, 13._pm_r,   2._pm_r,149._pm_r,  .07_pm_r,242._pm_r, &
248.70_pm_r, 37416._pm_r,   3._pm_r, 44._pm_r, 1.29_pm_r,357._pm_r,   2._pm_r,150._pm_r,  .05_pm_r, 17._pm_r, &
258.60_pm_r, 41128._pm_r,   4._pm_r, 23._pm_r, 1.14_pm_r,342._pm_r,   2._pm_r,145._pm_r,  .14_pm_r, 33._pm_r, &
266.00_pm_r, 44977._pm_r,   5._pm_r, 10._pm_r,  .86_pm_r,321._pm_r,   2._pm_r,138._pm_r,  .18_pm_r, 24._pm_r, &
267.40_pm_r, 48891._pm_r,   6._pm_r,360._pm_r,  .71_pm_r,289._pm_r,   1._pm_r,127._pm_r,  .21_pm_r, 14._pm_r, &
259.20_pm_r, 52752._pm_r,   6._pm_r,349._pm_r,  .73_pm_r,253._pm_r,   1._pm_r,116._pm_r,  .17_pm_r, 10._pm_r, &
247.70_pm_r, 56467._pm_r,   5._pm_r,338._pm_r,  .81_pm_r,226._pm_r,   1._pm_r,109._pm_r,  .07_pm_r, 62._pm_r, &
237.90_pm_r, 60020._pm_r,   5._pm_r,327._pm_r,  .76_pm_r,209._pm_r,   2._pm_r,109._pm_r,  .19_pm_r,123._pm_r, &
228.80_pm_r, 63439._pm_r,   4._pm_r,316._pm_r,  .62_pm_r,192._pm_r,   2._pm_r,113._pm_r,  .31_pm_r,130._pm_r, &
221.70_pm_r, 66734._pm_r,   4._pm_r,307._pm_r,  .43_pm_r,172._pm_r,   2._pm_r,117._pm_r,  .37_pm_r,130._pm_r, &
216.50_pm_r, 69943._pm_r,   4._pm_r,303._pm_r,  .30_pm_r,138._pm_r,   3._pm_r,119._pm_r,  .41_pm_r,128._pm_r, &
212.10_pm_r, 73081._pm_r,   3._pm_r,304._pm_r,  .31_pm_r, 98._pm_r,   4._pm_r,120._pm_r,  .41_pm_r,127._pm_r, &
207.90_pm_r, 76158._pm_r,   3._pm_r,310._pm_r,  .37_pm_r, 76._pm_r,   4._pm_r,121._pm_r,  .41_pm_r,125._pm_r, &
203.70_pm_r, 79170._pm_r,   3._pm_r,323._pm_r,  .43_pm_r, 65._pm_r,   5._pm_r,122._pm_r,  .39_pm_r,123._pm_r, &
197.20_pm_r, 82125._pm_r,   2._pm_r,338._pm_r,  .45_pm_r, 58._pm_r,   5._pm_r,122._pm_r,  .36_pm_r,123._pm_r, &
187.30_pm_r, 84927._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
178.90_pm_r, 87589._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
173.80_pm_r, 90164._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
173.50_pm_r, 92715._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
177.10_pm_r, 95301._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
184.70_pm_r, 97985._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
197.80_pm_r,100850._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
218.10_pm_r,104001._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
250.00_pm_r,107599._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
298.80_pm_r,111889._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
377.70_pm_r,117278._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_40= (/ &
289.70_pm_r,   -11._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
268.90_pm_r,  4078._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
243.60_pm_r,  7830._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
222.00_pm_r, 11238._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
213.10_pm_r, 14423._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
212.50_pm_r, 17531._pm_r,   3._pm_r,184._pm_r,  .08_pm_r,135._pm_r,   1._pm_r, 98._pm_r,  .10_pm_r,298._pm_r, &
216.30_pm_r, 20668._pm_r,   3._pm_r,182._pm_r,  .06_pm_r,121._pm_r,   1._pm_r, 95._pm_r,  .14_pm_r,300._pm_r, &
221.30_pm_r, 23876._pm_r,   3._pm_r,181._pm_r,  .04_pm_r,360._pm_r,   1._pm_r, 88._pm_r,  .15_pm_r,302._pm_r, &
225.20_pm_r, 27144._pm_r,   3._pm_r,182._pm_r,  .17_pm_r,335._pm_r,   1._pm_r, 78._pm_r,  .14_pm_r,306._pm_r, &
231.20_pm_r, 30485._pm_r,   2._pm_r,187._pm_r,  .30_pm_r,329._pm_r,   1._pm_r, 65._pm_r,  .10_pm_r,313._pm_r, &
239.70_pm_r, 33929._pm_r,   2._pm_r,197._pm_r,  .39_pm_r,325._pm_r,   1._pm_r, 55._pm_r,  .04_pm_r,346._pm_r, &
250.10_pm_r, 37517._pm_r,   2._pm_r,214._pm_r,  .39_pm_r,322._pm_r,   1._pm_r, 54._pm_r,  .05_pm_r, 84._pm_r, &
260.20_pm_r, 41253._pm_r,   2._pm_r,234._pm_r,  .33_pm_r,317._pm_r,   1._pm_r, 60._pm_r,  .09_pm_r,108._pm_r, &
266.80_pm_r, 45119._pm_r,   2._pm_r,246._pm_r,  .17_pm_r,299._pm_r,   1._pm_r, 68._pm_r,  .09_pm_r,118._pm_r, &
268.10_pm_r, 49043._pm_r,   2._pm_r,249._pm_r,  .09_pm_r,234._pm_r,   1._pm_r, 73._pm_r,  .06_pm_r, 99._pm_r, &
259.60_pm_r, 52913._pm_r,   2._pm_r,244._pm_r,  .20_pm_r,180._pm_r,   1._pm_r, 74._pm_r,  .07_pm_r, 74._pm_r, &
247.30_pm_r, 56628._pm_r,   2._pm_r,234._pm_r,  .33_pm_r,171._pm_r,   1._pm_r, 74._pm_r,  .18_pm_r, 74._pm_r, &
237.00_pm_r, 60170._pm_r,   2._pm_r,222._pm_r,  .37_pm_r,173._pm_r,   1._pm_r, 75._pm_r,  .28_pm_r, 86._pm_r, &
228.30_pm_r, 63578._pm_r,   3._pm_r,214._pm_r,  .32_pm_r,178._pm_r,   2._pm_r, 79._pm_r,  .38_pm_r, 92._pm_r, &
221.10_pm_r, 66866._pm_r,   3._pm_r,210._pm_r,  .22_pm_r,194._pm_r,   3._pm_r, 82._pm_r,  .44_pm_r, 95._pm_r, &
215.30_pm_r, 70061._pm_r,   3._pm_r,210._pm_r,  .13_pm_r,243._pm_r,   3._pm_r, 85._pm_r,  .47_pm_r, 97._pm_r, &
211.70_pm_r, 73186._pm_r,   3._pm_r,214._pm_r,  .19_pm_r,287._pm_r,   4._pm_r, 88._pm_r,  .49_pm_r, 98._pm_r, &
208.70_pm_r, 76265._pm_r,   4._pm_r,219._pm_r,  .29_pm_r,304._pm_r,   5._pm_r, 89._pm_r,  .52_pm_r, 99._pm_r, &
205.40_pm_r, 79297._pm_r,   4._pm_r,227._pm_r,  .34_pm_r,310._pm_r,   5._pm_r, 91._pm_r,  .51_pm_r, 98._pm_r, &
200.00_pm_r, 82283._pm_r,   4._pm_r,235._pm_r,  .37_pm_r,313._pm_r,   6._pm_r, 92._pm_r,  .49_pm_r,101._pm_r, &
191.40_pm_r, 85141._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
183.60_pm_r, 87874._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
178.60_pm_r, 90522._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
177.70_pm_r, 93142._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
179.90_pm_r, 95782._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
185.40_pm_r, 98497._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
195.80_pm_r,101355._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
212.70_pm_r,104454._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
242.70_pm_r,107955._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
295.50_pm_r,112157._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
377.10_pm_r,117533._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_30= (/ &
296.10_pm_r,   -47._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
273.70_pm_r,  4128._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
248.90_pm_r,  7957._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
224.00_pm_r, 11421._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
207.40_pm_r, 14582._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
205.80_pm_r, 17588._pm_r,   2._pm_r,147._pm_r,  .18_pm_r,264._pm_r,   0._pm_r,332._pm_r,  .02_pm_r, 56._pm_r, &
212.50_pm_r, 20649._pm_r,   2._pm_r,155._pm_r,  .23_pm_r,261._pm_r,   0._pm_r,337._pm_r,  .03_pm_r, 68._pm_r, &
219.70_pm_r, 23818._pm_r,   2._pm_r,166._pm_r,  .26_pm_r,258._pm_r,   0._pm_r,345._pm_r,  .04_pm_r, 68._pm_r, &
225.10_pm_r, 27074._pm_r,   2._pm_r,177._pm_r,  .25_pm_r,252._pm_r,   0._pm_r,357._pm_r,  .06_pm_r, 80._pm_r, &
231.50_pm_r, 30417._pm_r,   2._pm_r,186._pm_r,  .21_pm_r,242._pm_r,   0._pm_r, 13._pm_r,  .08_pm_r, 92._pm_r, &
240.80_pm_r, 33871._pm_r,   2._pm_r,192._pm_r,  .18_pm_r,226._pm_r,   0._pm_r, 33._pm_r,  .11_pm_r, 98._pm_r, &
250.70_pm_r, 37474._pm_r,   2._pm_r,194._pm_r,  .17_pm_r,213._pm_r,   1._pm_r, 51._pm_r,  .13_pm_r,104._pm_r, &
260.00_pm_r, 41211._pm_r,   3._pm_r,196._pm_r,  .16_pm_r,203._pm_r,   1._pm_r, 66._pm_r,  .14_pm_r,108._pm_r, &
266.60_pm_r, 45073._pm_r,   3._pm_r,196._pm_r,  .12_pm_r,191._pm_r,   1._pm_r, 75._pm_r,  .11_pm_r, 98._pm_r, &
268.00_pm_r, 48995._pm_r,   3._pm_r,195._pm_r,  .14_pm_r,172._pm_r,   1._pm_r, 76._pm_r,  .04_pm_r, 56._pm_r, &
259.90_pm_r, 52866._pm_r,   3._pm_r,193._pm_r,  .24_pm_r,168._pm_r,   1._pm_r, 72._pm_r,  .14_pm_r,285._pm_r, &
248.40_pm_r, 56590._pm_r,   4._pm_r,191._pm_r,  .39_pm_r,180._pm_r,   1._pm_r, 55._pm_r,  .36_pm_r,275._pm_r, &
239.40_pm_r, 60157._pm_r,   4._pm_r,190._pm_r,  .45_pm_r,196._pm_r,   0._pm_r,349._pm_r,  .32_pm_r,271._pm_r, &
231.90_pm_r, 63610._pm_r,   5._pm_r,192._pm_r,  .42_pm_r,215._pm_r,   1._pm_r,309._pm_r,  .14_pm_r,276._pm_r, &
223.70_pm_r, 66944._pm_r,   6._pm_r,196._pm_r,  .37_pm_r,242._pm_r,   1._pm_r,312._pm_r,  .14_pm_r, 68._pm_r, &
216.50_pm_r, 70167._pm_r,   6._pm_r,200._pm_r,  .38_pm_r,274._pm_r,   1._pm_r,356._pm_r,  .42_pm_r, 75._pm_r, &
211.80_pm_r, 73300._pm_r,   6._pm_r,207._pm_r,  .47_pm_r,298._pm_r,   1._pm_r, 47._pm_r,  .68_pm_r, 76._pm_r, &
208.50_pm_r, 76378._pm_r,   6._pm_r,214._pm_r,  .56_pm_r,310._pm_r,   2._pm_r, 62._pm_r,  .87_pm_r, 76._pm_r, &
205.60_pm_r, 79408._pm_r,   6._pm_r,223._pm_r,  .63_pm_r,316._pm_r,   4._pm_r, 68._pm_r,  .99_pm_r, 76._pm_r, &
201.10_pm_r, 82399._pm_r,   6._pm_r,232._pm_r,  .65_pm_r,320._pm_r,   5._pm_r, 70._pm_r, 1.02_pm_r, 76._pm_r, &
194.00_pm_r, 85289._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
187.20_pm_r, 88072._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
182.70_pm_r, 90780._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
181.70_pm_r, 93461._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
182.80_pm_r, 96155._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
186.40_pm_r, 98901._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
194.10_pm_r,101757._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
208.20_pm_r,104810._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
235.70_pm_r,108221._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
290.90_pm_r,112327._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
376.10_pm_r,117673._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_20= (/ &
299.70_pm_r,   -73._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
276.30_pm_r,  4150._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
252.60_pm_r,  8028._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
225.60_pm_r, 11534._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
203.00_pm_r, 14676._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
200.40_pm_r, 17601._pm_r,   2._pm_r,153._pm_r,  .22_pm_r,287._pm_r,   1._pm_r,306._pm_r,  .17_pm_r,102._pm_r, &
209.40_pm_r, 20599._pm_r,   2._pm_r,161._pm_r,  .28_pm_r,285._pm_r,   1._pm_r,316._pm_r,  .23_pm_r,103._pm_r, &
218.30_pm_r, 23736._pm_r,   2._pm_r,170._pm_r,  .28_pm_r,282._pm_r,   0._pm_r,342._pm_r,  .25_pm_r,106._pm_r, &
224.60_pm_r, 26978._pm_r,   2._pm_r,181._pm_r,  .24_pm_r,272._pm_r,   0._pm_r, 42._pm_r,  .27_pm_r,110._pm_r, &
231.80_pm_r, 30320._pm_r,   2._pm_r,190._pm_r,  .19_pm_r,258._pm_r,   1._pm_r, 77._pm_r,  .24_pm_r,114._pm_r, &
241.60_pm_r, 33782._pm_r,   2._pm_r,196._pm_r,  .16_pm_r,235._pm_r,   1._pm_r, 91._pm_r,  .19_pm_r,122._pm_r, &
252.30_pm_r, 37401._pm_r,   2._pm_r,199._pm_r,  .16_pm_r,217._pm_r,   1._pm_r, 98._pm_r,  .14_pm_r,129._pm_r, &
262.50_pm_r, 41168._pm_r,   3._pm_r,200._pm_r,  .17_pm_r,213._pm_r,   1._pm_r,103._pm_r,  .10_pm_r,143._pm_r, &
268.70_pm_r, 45065._pm_r,   3._pm_r,201._pm_r,  .15_pm_r,204._pm_r,   1._pm_r,106._pm_r,  .03_pm_r,202._pm_r, &
268.60_pm_r, 49008._pm_r,   3._pm_r,201._pm_r,  .12_pm_r,228._pm_r,   1._pm_r,107._pm_r,  .11_pm_r,280._pm_r, &
260.30_pm_r, 52884._pm_r,   3._pm_r,205._pm_r,  .19_pm_r,273._pm_r,   1._pm_r,109._pm_r,  .26_pm_r,285._pm_r, &
250.50_pm_r, 56625._pm_r,   3._pm_r,211._pm_r,  .37_pm_r,290._pm_r,   0._pm_r,108._pm_r,  .40_pm_r,290._pm_r, &
242.30_pm_r, 60232._pm_r,   3._pm_r,222._pm_r,  .44_pm_r,291._pm_r,   0._pm_r,308._pm_r,  .35_pm_r,297._pm_r, &
233.20_pm_r, 63718._pm_r,   4._pm_r,231._pm_r,  .40_pm_r,291._pm_r,   1._pm_r,303._pm_r,  .22_pm_r,313._pm_r, &
224.10_pm_r, 67063._pm_r,   4._pm_r,237._pm_r,  .23_pm_r,285._pm_r,   1._pm_r,312._pm_r,  .12_pm_r, 21._pm_r, &
216.70_pm_r, 70291._pm_r,   4._pm_r,239._pm_r,  .04_pm_r,256._pm_r,   1._pm_r,332._pm_r,  .27_pm_r, 75._pm_r, &
211.10_pm_r, 73420._pm_r,   4._pm_r,237._pm_r,  .16_pm_r,125._pm_r,   1._pm_r, 13._pm_r,  .43_pm_r, 87._pm_r, &
207.20_pm_r, 76483._pm_r,   4._pm_r,233._pm_r,  .31_pm_r,119._pm_r,   1._pm_r, 51._pm_r,  .55_pm_r, 90._pm_r, &
205.40_pm_r, 79499._pm_r,   4._pm_r,225._pm_r,  .40_pm_r,117._pm_r,   2._pm_r, 68._pm_r,  .63_pm_r, 92._pm_r, &
202.10_pm_r, 82496._pm_r,   4._pm_r,216._pm_r,  .46_pm_r,116._pm_r,   3._pm_r, 77._pm_r,  .66_pm_r, 93._pm_r, &
195.70_pm_r, 85407._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
189.40_pm_r, 88219._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
185.60_pm_r, 90966._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
184.80_pm_r, 93695._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
185.20_pm_r, 96432._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
187.30_pm_r, 99206._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
192.80_pm_r,102060._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
204.60_pm_r,105079._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
229.80_pm_r,108417._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
286.00_pm_r,112436._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
374.80_pm_r,117742._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_10= (/ &
301.40_pm_r,  -106._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
276.90_pm_r,  4136._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
253.70_pm_r,  8028._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
226.20_pm_r, 11548._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
200.80_pm_r, 14680._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
197.70_pm_r, 17566._pm_r,   3._pm_r,157._pm_r,  .23_pm_r,295._pm_r,   1._pm_r,306._pm_r,  .22_pm_r,112._pm_r, &
207.70_pm_r, 20532._pm_r,   2._pm_r,164._pm_r,  .28_pm_r,291._pm_r,   1._pm_r,312._pm_r,  .28_pm_r,112._pm_r, &
217.50_pm_r, 23650._pm_r,   2._pm_r,173._pm_r,  .28_pm_r,287._pm_r,   1._pm_r,329._pm_r,  .30_pm_r,112._pm_r, &
224.00_pm_r, 26884._pm_r,   2._pm_r,183._pm_r,  .26_pm_r,279._pm_r,   0._pm_r, 22._pm_r,  .28_pm_r,113._pm_r, &
231.10_pm_r, 30215._pm_r,   2._pm_r,193._pm_r,  .21_pm_r,267._pm_r,   1._pm_r, 72._pm_r,  .21_pm_r,116._pm_r, &
242.00_pm_r, 33674._pm_r,   2._pm_r,200._pm_r,  .17_pm_r,253._pm_r,   1._pm_r, 87._pm_r,  .12_pm_r,121._pm_r, &
253.80_pm_r, 37307._pm_r,   2._pm_r,204._pm_r,  .15_pm_r,246._pm_r,   1._pm_r, 92._pm_r,  .03_pm_r,145._pm_r, &
264.60_pm_r, 41102._pm_r,   3._pm_r,207._pm_r,  .13_pm_r,249._pm_r,   1._pm_r, 93._pm_r,  .05_pm_r,276._pm_r, &
270.80_pm_r, 45031._pm_r,   3._pm_r,211._pm_r,  .17_pm_r,272._pm_r,   1._pm_r, 93._pm_r,  .11_pm_r,266._pm_r, &
270.80_pm_r, 49003._pm_r,   3._pm_r,216._pm_r,  .23_pm_r,270._pm_r,   0._pm_r,103._pm_r,  .17_pm_r,240._pm_r, &
264.50_pm_r, 52927._pm_r,   3._pm_r,222._pm_r,  .25_pm_r,265._pm_r,   0._pm_r,147._pm_r,  .23_pm_r,234._pm_r, &
253.30_pm_r, 56726._pm_r,   3._pm_r,225._pm_r,  .14_pm_r,253._pm_r,   1._pm_r,197._pm_r,  .25_pm_r,250._pm_r, &
241.80_pm_r, 60346._pm_r,   3._pm_r,226._pm_r,  .05_pm_r, 41._pm_r,   1._pm_r,224._pm_r,  .20_pm_r,281._pm_r, &
231.80_pm_r, 63816._pm_r,   3._pm_r,225._pm_r,  .25_pm_r, 56._pm_r,   1._pm_r,242._pm_r,  .13_pm_r,323._pm_r, &
222.50_pm_r, 67139._pm_r,   3._pm_r,223._pm_r,  .41_pm_r, 57._pm_r,   1._pm_r,255._pm_r,  .13_pm_r, 32._pm_r, &
215.30_pm_r, 70344._pm_r,   2._pm_r,218._pm_r,  .54_pm_r, 57._pm_r,   1._pm_r,264._pm_r,  .24_pm_r, 72._pm_r, &
210.80_pm_r, 73461._pm_r,   1._pm_r,205._pm_r,  .62_pm_r, 56._pm_r,   0._pm_r,325._pm_r,  .38_pm_r, 87._pm_r, &
207.80_pm_r, 76526._pm_r,   1._pm_r,150._pm_r,  .69_pm_r, 56._pm_r,   1._pm_r, 87._pm_r,  .49_pm_r, 93._pm_r, &
207.40_pm_r, 79561._pm_r,   1._pm_r, 88._pm_r,  .72_pm_r, 55._pm_r,   1._pm_r, 91._pm_r,  .57_pm_r, 96._pm_r, &
204.50_pm_r, 82596._pm_r,   2._pm_r, 72._pm_r,  .72_pm_r, 55._pm_r,   2._pm_r, 93._pm_r,  .60_pm_r, 98._pm_r, &
197.20_pm_r, 85531._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
190.50_pm_r, 88357._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
187.20_pm_r, 91125._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
186.60_pm_r, 93880._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
186.80_pm_r, 96646._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
188.00_pm_r, 99438._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
191.90_pm_r,102291._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
202.20_pm_r,105286._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
225.70_pm_r,108575._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
282.10_pm_r,112530._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
373.50_pm_r,117798._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_0= (/ &
301.40_pm_r,  -132._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
277.20_pm_r,  4112._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
254.00_pm_r,  8009._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
226.50_pm_r, 11534._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
200.40_pm_r, 14666._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
196.90_pm_r, 17546._pm_r,   2._pm_r,157._pm_r,  .09_pm_r,277._pm_r,   1._pm_r,309._pm_r,  .22_pm_r,107._pm_r, &
207.00_pm_r, 20506._pm_r,   2._pm_r,161._pm_r,  .12_pm_r,270._pm_r,   1._pm_r,319._pm_r,  .27_pm_r,107._pm_r, &
217.10_pm_r, 23613._pm_r,   2._pm_r,167._pm_r,  .14_pm_r,266._pm_r,   1._pm_r,342._pm_r,  .27_pm_r,107._pm_r, &
223.90_pm_r, 26834._pm_r,   2._pm_r,175._pm_r,  .15_pm_r,266._pm_r,   0._pm_r, 23._pm_r,  .21_pm_r,109._pm_r, &
230.60_pm_r, 30158._pm_r,   2._pm_r,184._pm_r,  .17_pm_r,267._pm_r,   0._pm_r, 55._pm_r,  .12_pm_r,117._pm_r, &
242.20_pm_r, 33615._pm_r,   2._pm_r,194._pm_r,  .18_pm_r,273._pm_r,   1._pm_r, 68._pm_r,  .03_pm_r,162._pm_r, &
254.70_pm_r, 37257._pm_r,   2._pm_r,203._pm_r,  .19_pm_r,284._pm_r,   0._pm_r, 71._pm_r,  .09_pm_r,267._pm_r, &
265.90_pm_r, 41065._pm_r,   2._pm_r,213._pm_r,  .22_pm_r,292._pm_r,   0._pm_r, 61._pm_r,  .16_pm_r,277._pm_r, &
272.40_pm_r, 45010._pm_r,   2._pm_r,223._pm_r,  .29_pm_r,293._pm_r,   0._pm_r,349._pm_r,  .18_pm_r,279._pm_r, &
272.40_pm_r, 49001._pm_r,   2._pm_r,233._pm_r,  .26_pm_r,295._pm_r,   0._pm_r,303._pm_r,  .20_pm_r,294._pm_r, &
266.10_pm_r, 52943._pm_r,   2._pm_r,239._pm_r,  .07_pm_r,321._pm_r,   1._pm_r,296._pm_r,  .21_pm_r,307._pm_r, &
254.50_pm_r, 56758._pm_r,   2._pm_r,238._pm_r,  .31_pm_r, 86._pm_r,   1._pm_r,293._pm_r,  .21_pm_r,315._pm_r, &
242.00_pm_r, 60386._pm_r,   2._pm_r,232._pm_r,  .53_pm_r, 81._pm_r,   1._pm_r,290._pm_r,  .10_pm_r,309._pm_r, &
230.50_pm_r, 63848._pm_r,   1._pm_r,219._pm_r,  .67_pm_r, 73._pm_r,   1._pm_r,285._pm_r,  .04_pm_r,124._pm_r, &
221.20_pm_r, 67158._pm_r,   1._pm_r,186._pm_r,  .70_pm_r, 64._pm_r,   1._pm_r,277._pm_r,  .18_pm_r,119._pm_r, &
214.50_pm_r, 70352._pm_r,   1._pm_r,119._pm_r,  .72_pm_r, 55._pm_r,   0._pm_r,249._pm_r,  .34_pm_r,122._pm_r, &
210.50_pm_r, 73465._pm_r,   2._pm_r, 86._pm_r,  .76_pm_r, 47._pm_r,   0._pm_r,157._pm_r,  .47_pm_r,118._pm_r, &
208.80_pm_r, 76536._pm_r,   2._pm_r, 73._pm_r,  .80_pm_r, 39._pm_r,   1._pm_r,131._pm_r,  .58_pm_r,119._pm_r, &
209.20_pm_r, 79592._pm_r,   3._pm_r, 66._pm_r,  .82_pm_r, 36._pm_r,   2._pm_r,123._pm_r,  .65_pm_r,118._pm_r, &
206.30_pm_r, 82655._pm_r,   4._pm_r, 62._pm_r,  .81_pm_r, 33._pm_r,   2._pm_r,120._pm_r,  .67_pm_r,118._pm_r, &
198.00_pm_r, 85606._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
190.80_pm_r, 88436._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
187.60_pm_r, 91210._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
187.30_pm_r, 93974._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
187.30_pm_r, 96749._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
188.10_pm_r, 99547._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
191.30_pm_r,102398._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
201.00_pm_r,105379._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
223.70_pm_r,108644._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
280.10_pm_r,112567._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
372.50_pm_r,117812._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_10= (/ &
300.50_pm_r,  -112._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
277.30_pm_r,  4126._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
253.30_pm_r,  8018._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
226.20_pm_r, 11535._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
201.20_pm_r, 14670._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
197.90_pm_r, 17560._pm_r,   3._pm_r,157._pm_r,  .29_pm_r,333._pm_r,   1._pm_r,318._pm_r,  .22_pm_r, 93._pm_r, &
207.30_pm_r, 20525._pm_r,   2._pm_r,158._pm_r,  .36_pm_r,331._pm_r,   1._pm_r,343._pm_r,  .26_pm_r, 97._pm_r, &
216.00_pm_r, 23628._pm_r,   2._pm_r,162._pm_r,  .36_pm_r,325._pm_r,   1._pm_r, 20._pm_r,  .23_pm_r,101._pm_r, &
223.30_pm_r, 26843._pm_r,   1._pm_r,170._pm_r,  .33_pm_r,317._pm_r,   1._pm_r, 48._pm_r,  .15_pm_r,116._pm_r, &
230.70_pm_r, 30167._pm_r,   1._pm_r,187._pm_r,  .29_pm_r,303._pm_r,   1._pm_r, 61._pm_r,  .08_pm_r,166._pm_r, &
242.60_pm_r, 33626._pm_r,   1._pm_r,212._pm_r,  .26_pm_r,288._pm_r,   1._pm_r, 70._pm_r,  .13_pm_r,233._pm_r, &
254.50_pm_r, 37271._pm_r,   1._pm_r,231._pm_r,  .26_pm_r,278._pm_r,   0._pm_r, 75._pm_r,  .21_pm_r,250._pm_r, &
265.30_pm_r, 41076._pm_r,   1._pm_r,242._pm_r,  .27_pm_r,279._pm_r,   0._pm_r,254._pm_r,  .25_pm_r,258._pm_r, &
271.60_pm_r, 45016._pm_r,   2._pm_r,253._pm_r,  .34_pm_r,291._pm_r,   0._pm_r,262._pm_r,  .23_pm_r,267._pm_r, &
271.40_pm_r, 49000._pm_r,   2._pm_r,262._pm_r,  .32_pm_r,304._pm_r,   1._pm_r,266._pm_r,  .17_pm_r,279._pm_r, &
264.90_pm_r, 52930._pm_r,   2._pm_r,269._pm_r,  .14_pm_r,335._pm_r,   1._pm_r,272._pm_r,  .12_pm_r,315._pm_r, &
252.90_pm_r, 56731._pm_r,   2._pm_r,271._pm_r,  .22_pm_r, 99._pm_r,   1._pm_r,282._pm_r,  .15_pm_r,352._pm_r, &
240.40_pm_r, 60337._pm_r,   2._pm_r,267._pm_r,  .30_pm_r,118._pm_r,   1._pm_r,294._pm_r,  .14_pm_r, 10._pm_r, &
230.30_pm_r, 63785._pm_r,   2._pm_r,257._pm_r,  .26_pm_r,127._pm_r,   1._pm_r,304._pm_r,  .10_pm_r, 57._pm_r, &
221.40_pm_r, 67090._pm_r,   1._pm_r,247._pm_r,  .16_pm_r,155._pm_r,   1._pm_r,311._pm_r,  .16_pm_r,106._pm_r, &
214.60_pm_r, 70282._pm_r,   2._pm_r,241._pm_r,  .14_pm_r,232._pm_r,   1._pm_r,317._pm_r,  .26_pm_r,127._pm_r, &
211.00_pm_r, 73394._pm_r,   2._pm_r,243._pm_r,  .26_pm_r,265._pm_r,   0._pm_r,333._pm_r,  .36_pm_r,135._pm_r, &
209.10_pm_r, 76469._pm_r,   2._pm_r,248._pm_r,  .38_pm_r,273._pm_r,   0._pm_r,131._pm_r,  .44_pm_r,137._pm_r, &
208.70_pm_r, 79526._pm_r,   3._pm_r,254._pm_r,  .47_pm_r,277._pm_r,   1._pm_r,135._pm_r,  .49_pm_r,139._pm_r, &
205.40_pm_r, 82580._pm_r,   4._pm_r,259._pm_r,  .51_pm_r,279._pm_r,   2._pm_r,137._pm_r,  .51_pm_r,141._pm_r, &
197.60_pm_r, 85525._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
190.70_pm_r, 88355._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
187.20_pm_r, 91124._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
186.70_pm_r, 93881._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
186.80_pm_r, 96647._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
187.70_pm_r, 99438._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
191.20_pm_r,102285._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
201.00_pm_r,105266._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
224.00_pm_r,108533._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
280.30_pm_r,112460._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
371.90_pm_r,117701._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)       

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_20= (/ &
297.40_pm_r,  -111._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
275.00_pm_r,  4086._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
250.10_pm_r,  7936._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
225.30_pm_r, 11421._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
204.20_pm_r, 14570._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
201.20_pm_r, 17512._pm_r,   5._pm_r,155._pm_r,  .59_pm_r,314._pm_r,   1._pm_r,303._pm_r,  .34_pm_r, 87._pm_r, &
208.80_pm_r, 20512._pm_r,   4._pm_r,160._pm_r,  .81_pm_r,320._pm_r,   1._pm_r,338._pm_r,  .41_pm_r, 86._pm_r, &
216.30_pm_r, 23628._pm_r,   2._pm_r,168._pm_r,  .89_pm_r,329._pm_r,   1._pm_r, 32._pm_r,  .37_pm_r, 87._pm_r, &
223.20_pm_r, 26844._pm_r,   1._pm_r,185._pm_r,  .90_pm_r,340._pm_r,   1._pm_r, 54._pm_r,  .25_pm_r, 85._pm_r, &
231.10_pm_r, 30170._pm_r,   0._pm_r,285._pm_r,  .88_pm_r,352._pm_r,   1._pm_r, 59._pm_r,  .07_pm_r, 81._pm_r, &
241.80_pm_r, 33628._pm_r,   1._pm_r,340._pm_r,  .80_pm_r,  1._pm_r,   1._pm_r, 58._pm_r,  .12_pm_r,270._pm_r, &
253.00_pm_r, 37254._pm_r,   3._pm_r,350._pm_r,  .67_pm_r,  5._pm_r,   1._pm_r, 49._pm_r,  .27_pm_r,269._pm_r, &
263.80_pm_r, 41037._pm_r,   3._pm_r,354._pm_r,  .51_pm_r,359._pm_r,   1._pm_r, 24._pm_r,  .34_pm_r,270._pm_r, &
270.30_pm_r, 44956._pm_r,   4._pm_r,353._pm_r,  .44_pm_r,332._pm_r,   1._pm_r,342._pm_r,  .31_pm_r,274._pm_r, &
269.70_pm_r, 48918._pm_r,   5._pm_r,348._pm_r,  .42_pm_r,306._pm_r,   1._pm_r,320._pm_r,  .22_pm_r,291._pm_r, &
262.10_pm_r, 52816._pm_r,   5._pm_r,344._pm_r,  .24_pm_r,276._pm_r,   1._pm_r,316._pm_r,  .14_pm_r,327._pm_r, &
249.90_pm_r, 56571._pm_r,   5._pm_r,341._pm_r,  .25_pm_r,162._pm_r,   1._pm_r,322._pm_r,  .13_pm_r, 42._pm_r, &
238.30_pm_r, 60139._pm_r,   4._pm_r,341._pm_r,  .40_pm_r,163._pm_r,   1._pm_r,333._pm_r,  .22_pm_r, 89._pm_r, &
230.30_pm_r, 63569._pm_r,   4._pm_r,340._pm_r,  .43_pm_r,179._pm_r,   1._pm_r,351._pm_r,  .32_pm_r,108._pm_r, &
223.80_pm_r, 66892._pm_r,   3._pm_r,334._pm_r,  .43_pm_r,212._pm_r,   1._pm_r, 21._pm_r,  .39_pm_r,120._pm_r, &
218.30_pm_r, 70130._pm_r,   3._pm_r,321._pm_r,  .55_pm_r,242._pm_r,   1._pm_r, 61._pm_r,  .44_pm_r,128._pm_r, &
214.10_pm_r, 73293._pm_r,   4._pm_r,306._pm_r,  .76_pm_r,258._pm_r,   1._pm_r, 90._pm_r,  .48_pm_r,135._pm_r, &
211.20_pm_r, 76408._pm_r,   5._pm_r,295._pm_r,  .95_pm_r,265._pm_r,   2._pm_r,107._pm_r,  .52_pm_r,139._pm_r, &
209.20_pm_r, 79484._pm_r,   6._pm_r,288._pm_r, 1.07_pm_r,269._pm_r,   3._pm_r,116._pm_r,  .52_pm_r,142._pm_r, &
205.00_pm_r, 82533._pm_r,   7._pm_r,284._pm_r, 1.10_pm_r,271._pm_r,   3._pm_r,122._pm_r,  .50_pm_r,145._pm_r, &
197.30_pm_r, 85475._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
190.30_pm_r, 88302._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
186.10_pm_r, 91059._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
185.30_pm_r, 93795._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
185.50_pm_r, 96539._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
187.10_pm_r, 99314._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
191.70_pm_r,102158._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
202.50_pm_r,105151._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
226.60_pm_r,108448._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
282.50_pm_r,112412._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
371.70_pm_r,117666._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_30= (/ &
291.60_pm_r,   -52._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
268.30_pm_r,  4050._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
242.80_pm_r,  7795._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
222.20_pm_r, 11202._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
209.70_pm_r, 14366._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
207.60_pm_r, 17408._pm_r,   5._pm_r,182._pm_r,  .34_pm_r,260._pm_r,   1._pm_r, 28._pm_r,  .28_pm_r, 24._pm_r, &
211.50_pm_r, 20474._pm_r,   5._pm_r,187._pm_r,  .30_pm_r,304._pm_r,   1._pm_r, 27._pm_r,  .36_pm_r, 29._pm_r, &
216.80_pm_r, 23611._pm_r,   5._pm_r,189._pm_r,  .59_pm_r,  6._pm_r,   2._pm_r, 29._pm_r,  .39_pm_r, 35._pm_r, &
222.80_pm_r, 26825._pm_r,   3._pm_r,186._pm_r, 1.23_pm_r, 24._pm_r,   2._pm_r, 31._pm_r,  .36_pm_r, 44._pm_r, &
230.00_pm_r, 30142._pm_r,   2._pm_r,152._pm_r, 1.89_pm_r, 29._pm_r,   3._pm_r, 35._pm_r,  .29_pm_r, 59._pm_r, &
239.60_pm_r, 33576._pm_r,   3._pm_r, 58._pm_r, 2.31_pm_r, 30._pm_r,   3._pm_r, 39._pm_r,  .20_pm_r, 83._pm_r, &
250.00_pm_r, 37165._pm_r,   6._pm_r, 42._pm_r, 2.32_pm_r, 28._pm_r,   3._pm_r, 43._pm_r,  .14_pm_r,124._pm_r, &
260.50_pm_r, 40901._pm_r,   9._pm_r, 36._pm_r, 1.96_pm_r, 23._pm_r,   3._pm_r, 46._pm_r,  .13_pm_r,176._pm_r, &
267.60_pm_r, 44777._pm_r,  11._pm_r, 32._pm_r, 1.36_pm_r, 11._pm_r,   3._pm_r, 47._pm_r,  .11_pm_r,249._pm_r, &
267.20_pm_r, 48702._pm_r,  13._pm_r, 29._pm_r,  .88_pm_r,355._pm_r,   3._pm_r, 44._pm_r,  .19_pm_r,301._pm_r, &
259.70_pm_r, 52564._pm_r,  14._pm_r, 26._pm_r,  .36_pm_r,341._pm_r,   3._pm_r, 37._pm_r,  .19_pm_r,319._pm_r, &
248.20_pm_r, 56290._pm_r,  14._pm_r, 26._pm_r,  .27_pm_r,161._pm_r,   3._pm_r, 34._pm_r,  .04_pm_r, 56._pm_r, &
238.10_pm_r, 59845._pm_r,  13._pm_r, 28._pm_r,  .75_pm_r,187._pm_r,   3._pm_r, 38._pm_r,  .21_pm_r,143._pm_r, &
231.50_pm_r, 63283._pm_r,  12._pm_r, 29._pm_r, 1.00_pm_r,198._pm_r,   3._pm_r, 46._pm_r,  .33_pm_r,146._pm_r, &
226.30_pm_r, 66634._pm_r,  10._pm_r, 30._pm_r, 1.05_pm_r,210._pm_r,   3._pm_r, 57._pm_r,  .35_pm_r,142._pm_r, &
221.90_pm_r, 69915._pm_r,   9._pm_r, 29._pm_r, 1.01_pm_r,224._pm_r,   3._pm_r, 67._pm_r,  .30_pm_r,136._pm_r, &
218.20_pm_r, 73136._pm_r,   7._pm_r, 24._pm_r, 1.00_pm_r,239._pm_r,   3._pm_r, 74._pm_r,  .24_pm_r,127._pm_r, &
214.40_pm_r, 76305._pm_r,   6._pm_r, 16._pm_r, 1.02_pm_r,251._pm_r,   3._pm_r, 79._pm_r,  .20_pm_r,113._pm_r, &
210.60_pm_r, 79415._pm_r,   6._pm_r,  2._pm_r, 1.01_pm_r,259._pm_r,   3._pm_r, 81._pm_r,  .16_pm_r,102._pm_r, &
205.20_pm_r, 82474._pm_r,   6._pm_r,347._pm_r,  .97_pm_r,265._pm_r,   4._pm_r, 82._pm_r,  .14_pm_r, 90._pm_r, &
197.10_pm_r, 85416._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
189.50_pm_r, 88238._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
184.50_pm_r, 90975._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
183.20_pm_r, 93681._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
183.70_pm_r, 96393._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
186.40_pm_r, 99147._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
192.80_pm_r,101992._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
205.30_pm_r,105014._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
231.20_pm_r,108366._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
285.90_pm_r,112396._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
371.60_pm_r,117668._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_40= (/ &
284.70_pm_r,   -25._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
260.60_pm_r,  3966._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
235.20_pm_r,  7596._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
219.00_pm_r, 10920._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
215.50_pm_r, 14101._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
214.80_pm_r, 17250._pm_r,   5._pm_r,267._pm_r, 2.49_pm_r,192._pm_r,   5._pm_r, 64._pm_r,  .90_pm_r,298._pm_r, &
215.30_pm_r, 20397._pm_r,   7._pm_r,231._pm_r, 2.89_pm_r,184._pm_r,   4._pm_r, 48._pm_r, 1.04_pm_r,298._pm_r, &
217.40_pm_r, 23565._pm_r,  10._pm_r,211._pm_r, 2.55_pm_r,165._pm_r,   4._pm_r, 29._pm_r,  .86_pm_r,298._pm_r, &
220.50_pm_r, 26768._pm_r,  11._pm_r,196._pm_r, 2.17_pm_r,123._pm_r,   4._pm_r, 15._pm_r,  .38_pm_r,299._pm_r, &
225.80_pm_r, 30035._pm_r,  11._pm_r,177._pm_r, 3.11_pm_r, 77._pm_r,   4._pm_r, 14._pm_r,  .28_pm_r,118._pm_r, &
236.00_pm_r, 33409._pm_r,  11._pm_r,149._pm_r, 4.48_pm_r, 57._pm_r,   4._pm_r, 26._pm_r,  .85_pm_r,119._pm_r, &
247.40_pm_r, 36952._pm_r,  12._pm_r,112._pm_r, 5.23_pm_r, 46._pm_r,   4._pm_r, 47._pm_r, 1.14_pm_r,118._pm_r, &
258.30_pm_r, 40653._pm_r,  16._pm_r, 86._pm_r, 5.10_pm_r, 38._pm_r,   5._pm_r, 66._pm_r, 1.09_pm_r,118._pm_r, &
265.70_pm_r, 44498._pm_r,  21._pm_r, 72._pm_r, 3.49_pm_r, 31._pm_r,   6._pm_r, 76._pm_r,  .53_pm_r,135._pm_r, &
266.40_pm_r, 48403._pm_r,  24._pm_r, 65._pm_r, 2.03_pm_r, 17._pm_r,   6._pm_r, 81._pm_r,  .32_pm_r,198._pm_r, &
258.90_pm_r, 52254._pm_r,  25._pm_r, 61._pm_r, 1.28_pm_r,344._pm_r,   5._pm_r, 85._pm_r,  .44_pm_r,223._pm_r, &
247.40_pm_r, 55967._pm_r,  25._pm_r, 56._pm_r, 1.32_pm_r,317._pm_r,   5._pm_r, 91._pm_r,  .43_pm_r,199._pm_r, &
237.80_pm_r, 59513._pm_r,  24._pm_r, 52._pm_r, 1.59_pm_r,301._pm_r,   5._pm_r, 97._pm_r,  .28_pm_r,176._pm_r, &
231.70_pm_r, 62948._pm_r,  23._pm_r, 46._pm_r, 1.73_pm_r,292._pm_r,   5._pm_r,100._pm_r,  .18_pm_r,133._pm_r, &
227.50_pm_r, 66309._pm_r,  22._pm_r, 40._pm_r, 1.73_pm_r,289._pm_r,   5._pm_r,100._pm_r,  .21_pm_r, 79._pm_r, &
223.70_pm_r, 69614._pm_r,  22._pm_r, 34._pm_r, 1.61_pm_r,288._pm_r,   6._pm_r, 98._pm_r,  .29_pm_r, 55._pm_r, &
220.10_pm_r, 72863._pm_r,  21._pm_r, 28._pm_r, 1.49_pm_r,289._pm_r,   6._pm_r, 94._pm_r,  .35_pm_r, 42._pm_r, &
215.40_pm_r, 76055._pm_r,  21._pm_r, 23._pm_r, 1.35_pm_r,290._pm_r,   6._pm_r, 90._pm_r,  .38_pm_r, 35._pm_r, &
210.20_pm_r, 79168._pm_r,  21._pm_r, 18._pm_r, 1.23_pm_r,292._pm_r,   7._pm_r, 86._pm_r,  .41_pm_r, 32._pm_r, &
204.10_pm_r, 82209._pm_r,  21._pm_r, 13._pm_r, 1.06_pm_r,293._pm_r,   7._pm_r, 82._pm_r,  .38_pm_r, 30._pm_r, &
196.20_pm_r, 85139._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
188.30_pm_r, 87947._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
182.40_pm_r, 90656._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
180.70_pm_r, 93326._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
181.90_pm_r, 96003._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
185.90_pm_r, 98736._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
194.50_pm_r,101589._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
209.30_pm_r,104652._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
237.10_pm_r,108082._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
289.50_pm_r,112189._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
371.30_pm_r,117473._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_50= (/ &
279.10_pm_r,   -52._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
254.00_pm_r,  3847._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
229.80_pm_r,  7385._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
218.90_pm_r, 10667._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
218.70_pm_r, 13867._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
218.60_pm_r, 17071._pm_r,   8._pm_r,297._pm_r, 6.05_pm_r,209._pm_r,  13._pm_r, 54._pm_r, 1.90_pm_r,284._pm_r, &
218.20_pm_r, 20269._pm_r,  13._pm_r,244._pm_r, 7.59_pm_r,204._pm_r,  11._pm_r, 42._pm_r, 2.33_pm_r,281._pm_r, &
218.10_pm_r, 23463._pm_r,  22._pm_r,223._pm_r, 7.49_pm_r,194._pm_r,  10._pm_r, 25._pm_r, 2.18_pm_r,274._pm_r, &
218.70_pm_r, 26659._pm_r,  31._pm_r,212._pm_r, 5.86_pm_r,173._pm_r,   9._pm_r, 10._pm_r, 1.47_pm_r,257._pm_r, &
221.90_pm_r, 29882._pm_r,  35._pm_r,201._pm_r, 4.91_pm_r,126._pm_r,   8._pm_r,  2._pm_r,  .96_pm_r,195._pm_r, &
231.00_pm_r, 33191._pm_r,  34._pm_r,188._pm_r, 6.77_pm_r, 85._pm_r,   6._pm_r,  7._pm_r, 1.66_pm_r,145._pm_r, &
242.40_pm_r, 36660._pm_r,  32._pm_r,168._pm_r, 8.79_pm_r, 66._pm_r,   5._pm_r, 35._pm_r, 2.27_pm_r,131._pm_r, &
253.90_pm_r, 40292._pm_r,  30._pm_r,143._pm_r, 9.44_pm_r, 56._pm_r,   6._pm_r, 70._pm_r, 2.31_pm_r,123._pm_r, &
262.50_pm_r, 44083._pm_r,  33._pm_r,121._pm_r, 7.62_pm_r, 48._pm_r,   8._pm_r, 85._pm_r, 1.19_pm_r,124._pm_r, &
264.20_pm_r, 47948._pm_r,  36._pm_r,105._pm_r, 5.62_pm_r, 33._pm_r,   8._pm_r, 90._pm_r,  .25_pm_r,200._pm_r, &
258.20_pm_r, 51779._pm_r,  37._pm_r, 94._pm_r, 4.43_pm_r,  4._pm_r,   8._pm_r, 92._pm_r,  .81_pm_r,256._pm_r, &
247.80_pm_r, 55490._pm_r,  36._pm_r, 85._pm_r, 4.45_pm_r,334._pm_r,   7._pm_r, 97._pm_r,  .94_pm_r,235._pm_r, &
239.30_pm_r, 59050._pm_r,  33._pm_r, 75._pm_r, 4.39_pm_r,314._pm_r,   6._pm_r,105._pm_r,  .65_pm_r,215._pm_r, &
234.20_pm_r, 62515._pm_r,  30._pm_r, 66._pm_r, 4.11_pm_r,299._pm_r,   6._pm_r,113._pm_r,  .40_pm_r,183._pm_r, &
230.30_pm_r, 65915._pm_r,  26._pm_r, 57._pm_r, 3.57_pm_r,287._pm_r,   6._pm_r,116._pm_r,  .26_pm_r,129._pm_r, &
227.00_pm_r, 69263._pm_r,  23._pm_r, 48._pm_r, 2.91_pm_r,279._pm_r,   7._pm_r,115._pm_r,  .33_pm_r, 75._pm_r, &
224.10_pm_r, 72566._pm_r,  21._pm_r, 40._pm_r, 2.27_pm_r,271._pm_r,   7._pm_r,111._pm_r,  .48_pm_r, 50._pm_r, &
219.10_pm_r, 75815._pm_r,  19._pm_r, 34._pm_r, 1.74_pm_r,263._pm_r,   7._pm_r,105._pm_r,  .61_pm_r, 39._pm_r, &
212.60_pm_r, 78973._pm_r,  17._pm_r, 29._pm_r, 1.33_pm_r,255._pm_r,   8._pm_r, 98._pm_r,  .67_pm_r, 33._pm_r, &
205.20_pm_r, 82042._pm_r,  16._pm_r, 25._pm_r, 1.01_pm_r,246._pm_r,   8._pm_r, 92._pm_r,  .67_pm_r, 30._pm_r, &
195.60_pm_r, 84970._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
186.60_pm_r, 87756._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
180.10_pm_r, 90432._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
178.20_pm_r, 93063._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
180.20_pm_r, 95707._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
185.90_pm_r, 98425._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
196.70_pm_r,101291._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
214.20_pm_r,104404._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
243.50_pm_r,107921._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
292.00_pm_r,112102._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
370.70_pm_r,117382._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_60= (/ &
268.60_pm_r,   -21._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
249.20_pm_r,  3763._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
226.50_pm_r,  7239._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
218.80_pm_r, 10493._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
219.30_pm_r, 13694._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
219.20_pm_r, 16906._pm_r,  10._pm_r,264._pm_r, 8.58_pm_r,216._pm_r,  20._pm_r, 37._pm_r, 1.77_pm_r,275._pm_r, &
218.80_pm_r, 20113._pm_r,  23._pm_r,235._pm_r,11.10_pm_r,212._pm_r,  19._pm_r, 29._pm_r, 2.27_pm_r,269._pm_r, &
218.50_pm_r, 23315._pm_r,  38._pm_r,224._pm_r,11.42_pm_r,205._pm_r,  17._pm_r, 20._pm_r, 2.34_pm_r,259._pm_r, &
218.10_pm_r, 26511._pm_r,  53._pm_r,217._pm_r, 9.13_pm_r,191._pm_r,  15._pm_r, 10._pm_r, 1.97_pm_r,240._pm_r, &
220.80_pm_r, 29722._pm_r,  61._pm_r,210._pm_r, 6.18_pm_r,153._pm_r,  13._pm_r,  4._pm_r, 1.64_pm_r,204._pm_r, &
227.90_pm_r, 33001._pm_r,  63._pm_r,202._pm_r, 7.24_pm_r, 99._pm_r,  11._pm_r,  5._pm_r, 1.92_pm_r,165._pm_r, &
237.60_pm_r, 36410._pm_r,  58._pm_r,191._pm_r,10.23_pm_r, 74._pm_r,   8._pm_r, 16._pm_r, 2.32_pm_r,143._pm_r, &
248.30_pm_r, 39965._pm_r,  51._pm_r,175._pm_r,11.80_pm_r, 63._pm_r,   7._pm_r, 40._pm_r, 2.41_pm_r,129._pm_r, &
258.20_pm_r, 43682._pm_r,  46._pm_r,157._pm_r,10.14_pm_r, 55._pm_r,   8._pm_r, 61._pm_r, 1.47_pm_r,130._pm_r, &
262.60_pm_r, 47502._pm_r,  44._pm_r,140._pm_r, 8.16_pm_r, 40._pm_r,   8._pm_r, 71._pm_r,  .62_pm_r,174._pm_r, &
258.40_pm_r, 51324._pm_r,  41._pm_r,125._pm_r, 7.26_pm_r, 15._pm_r,   8._pm_r, 76._pm_r, 1.00_pm_r,229._pm_r, &
249.00_pm_r, 55045._pm_r,  36._pm_r,111._pm_r, 7.69_pm_r,352._pm_r,   6._pm_r, 84._pm_r, 1.37_pm_r,223._pm_r, &
240.20_pm_r, 58622._pm_r,  31._pm_r, 94._pm_r, 7.22_pm_r,336._pm_r,   5._pm_r, 98._pm_r,  .86_pm_r,206._pm_r, &
233.80_pm_r, 62091._pm_r,  27._pm_r, 76._pm_r, 6.26_pm_r,322._pm_r,   5._pm_r,108._pm_r,  .41_pm_r,166._pm_r, &
230.10_pm_r, 65485._pm_r,  23._pm_r, 59._pm_r, 5.03_pm_r,310._pm_r,   6._pm_r,110._pm_r,  .41_pm_r, 91._pm_r, &
227.60_pm_r, 68836._pm_r,  22._pm_r, 43._pm_r, 3.82_pm_r,296._pm_r,   6._pm_r,106._pm_r,  .67_pm_r, 58._pm_r, &
225.00_pm_r, 72150._pm_r,  20._pm_r, 30._pm_r, 2.87_pm_r,281._pm_r,   7._pm_r, 97._pm_r,  .89_pm_r, 44._pm_r, &
221.40_pm_r, 75422._pm_r,  19._pm_r, 20._pm_r, 2.24_pm_r,262._pm_r,   8._pm_r, 88._pm_r, 1.06_pm_r, 37._pm_r, &
215.60_pm_r, 78625._pm_r,  17._pm_r, 12._pm_r, 1.89_pm_r,244._pm_r,   9._pm_r, 80._pm_r, 1.12_pm_r, 33._pm_r, &
206.90_pm_r, 81740._pm_r,  15._pm_r,  5._pm_r, 1.67_pm_r,229._pm_r,  10._pm_r, 73._pm_r, 1.09_pm_r, 31._pm_r, &
194.90_pm_r, 84669._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
184.60_pm_r, 87427._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
177.80_pm_r, 90067._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
176.10_pm_r, 92663._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
179.00_pm_r, 95280._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
186.30_pm_r, 97987._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
199.10_pm_r,100871._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
219.10_pm_r,104036._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
249.20_pm_r,107634._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
293.00_pm_r,111871._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
369.80_pm_r,117130._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_70= (/ &
256.40_pm_r,    77._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
245.00_pm_r,  3739._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
223.60_pm_r,  7160._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
217.70_pm_r, 10383._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
217.70_pm_r, 13562._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
217.20_pm_r, 16748._pm_r,  14._pm_r,241._pm_r, 8.76_pm_r,214._pm_r,  19._pm_r, 30._pm_r, 1.26_pm_r,269._pm_r, &
217.10_pm_r, 19927._pm_r,  28._pm_r,227._pm_r,11.51_pm_r,212._pm_r,  18._pm_r, 24._pm_r, 1.64_pm_r,262._pm_r, &
218.40_pm_r, 23117._pm_r,  45._pm_r,220._pm_r,11.68_pm_r,208._pm_r,  16._pm_r, 17._pm_r, 1.75_pm_r,250._pm_r, &
219.30_pm_r, 26321._pm_r,  60._pm_r,216._pm_r, 8.98_pm_r,198._pm_r,  15._pm_r, 10._pm_r, 1.63_pm_r,230._pm_r, &
222.00_pm_r, 29550._pm_r,  69._pm_r,212._pm_r, 5.08_pm_r,167._pm_r,  13._pm_r,  6._pm_r, 1.47_pm_r,200._pm_r, &
227.60_pm_r, 32837._pm_r,  70._pm_r,207._pm_r, 5.23_pm_r, 97._pm_r,  11._pm_r,  6._pm_r, 1.57_pm_r,168._pm_r, &
234.70_pm_r, 36223._pm_r,  64._pm_r,200._pm_r, 8.71_pm_r, 68._pm_r,   9._pm_r, 15._pm_r, 1.76_pm_r,147._pm_r, &
242.70_pm_r, 39715._pm_r,  54._pm_r,189._pm_r,10.93_pm_r, 58._pm_r,   7._pm_r, 32._pm_r, 1.77_pm_r,132._pm_r, &
253.30_pm_r, 43352._pm_r,  45._pm_r,175._pm_r, 9.85_pm_r, 53._pm_r,   7._pm_r, 49._pm_r, 1.19_pm_r,130._pm_r, &
260.40_pm_r, 47117._pm_r,  39._pm_r,159._pm_r, 8.18_pm_r, 43._pm_r,   8._pm_r, 59._pm_r,  .66_pm_r,138._pm_r, &
260.30_pm_r, 50938._pm_r,  33._pm_r,143._pm_r, 7.32_pm_r, 22._pm_r,   8._pm_r, 64._pm_r,  .40_pm_r,165._pm_r, &
253.50_pm_r, 54709._pm_r,  27._pm_r,126._pm_r, 7.88_pm_r,359._pm_r,   7._pm_r, 69._pm_r,  .43_pm_r,181._pm_r, &
243.30_pm_r, 58344._pm_r,  21._pm_r,104._pm_r, 7.44_pm_r,344._pm_r,   7._pm_r, 71._pm_r,  .14_pm_r,156._pm_r, &
234.60_pm_r, 61843._pm_r,  17._pm_r, 75._pm_r, 6.51_pm_r,331._pm_r,   7._pm_r, 72._pm_r,  .16_pm_r, 34._pm_r, &
229.00_pm_r, 65233._pm_r,  16._pm_r, 45._pm_r, 5.20_pm_r,319._pm_r,   8._pm_r, 70._pm_r,  .33_pm_r, 19._pm_r, &
225.50_pm_r, 68561._pm_r,  17._pm_r, 22._pm_r, 3.85_pm_r,307._pm_r,   8._pm_r, 66._pm_r,  .43_pm_r, 16._pm_r, &
222.30_pm_r, 71839._pm_r,  19._pm_r,  8._pm_r, 2.70_pm_r,293._pm_r,   8._pm_r, 63._pm_r,  .48_pm_r, 18._pm_r, &
218.90_pm_r, 75071._pm_r,  19._pm_r,358._pm_r, 1.90_pm_r,273._pm_r,   9._pm_r, 60._pm_r,  .51_pm_r, 20._pm_r, &
214.80_pm_r, 78251._pm_r,  19._pm_r,351._pm_r, 1.49_pm_r,250._pm_r,  10._pm_r, 57._pm_r,  .50_pm_r, 22._pm_r, &
206.60_pm_r, 81367._pm_r,  19._pm_r,345._pm_r, 1.31_pm_r,229._pm_r,  10._pm_r, 55._pm_r,  .46_pm_r, 24._pm_r, &
193.70_pm_r, 84281._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
182.60_pm_r, 87009._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
175.90_pm_r, 89619._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
174.60_pm_r, 92187._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
178.20_pm_r, 94786._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
186.80_pm_r, 97489._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
201.20_pm_r,100391._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
223.20_pm_r,103601._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
253.30_pm_r,107264._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
292.50_pm_r,111531._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
368.80_pm_r,116761._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_80= (/ &
248.40_pm_r,   189._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
241.40_pm_r,  3764._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
221.30_pm_r,  7141._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
215.90_pm_r, 10332._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
215.00_pm_r, 13477._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
213.90_pm_r, 16619._pm_r,  12._pm_r,230._pm_r, 5.61_pm_r,210._pm_r,   8._pm_r, 23._pm_r,  .50_pm_r,241._pm_r, &
214.30_pm_r, 19751._pm_r,  21._pm_r,221._pm_r, 7.41_pm_r,208._pm_r,   8._pm_r, 20._pm_r,  .62_pm_r,237._pm_r, &
218.10_pm_r, 22919._pm_r,  32._pm_r,216._pm_r, 7.32_pm_r,207._pm_r,   7._pm_r, 15._pm_r,  .63_pm_r,228._pm_r, &
220.60_pm_r, 26131._pm_r,  41._pm_r,214._pm_r, 5.32_pm_r,203._pm_r,   6._pm_r, 12._pm_r,  .52_pm_r,210._pm_r, &
223.80_pm_r, 29385._pm_r,  47._pm_r,212._pm_r, 2.25_pm_r,184._pm_r,   6._pm_r, 12._pm_r,  .40_pm_r,171._pm_r, &
228.60_pm_r, 32694._pm_r,  47._pm_r,210._pm_r, 2.00_pm_r, 70._pm_r,   5._pm_r, 17._pm_r,  .48_pm_r,127._pm_r, &
233.70_pm_r, 36081._pm_r,  43._pm_r,207._pm_r, 4.67_pm_r, 49._pm_r,   5._pm_r, 26._pm_r,  .63_pm_r,107._pm_r, &
239.20_pm_r, 39540._pm_r,  35._pm_r,203._pm_r, 6.29_pm_r, 44._pm_r,   5._pm_r, 36._pm_r,  .68_pm_r, 98._pm_r, &
249.80_pm_r, 43123._pm_r,  27._pm_r,196._pm_r, 5.67_pm_r, 45._pm_r,   6._pm_r, 43._pm_r,  .43_pm_r, 98._pm_r, &
258.80_pm_r, 46849._pm_r,  21._pm_r,186._pm_r, 4.60_pm_r, 39._pm_r,   6._pm_r, 46._pm_r,  .27_pm_r, 90._pm_r, &
261.90_pm_r, 50671._pm_r,  15._pm_r,176._pm_r, 4.14_pm_r, 20._pm_r,   6._pm_r, 49._pm_r,  .30_pm_r, 89._pm_r, &
257.10_pm_r, 54480._pm_r,   9._pm_r,167._pm_r, 4.83_pm_r,359._pm_r,   7._pm_r, 52._pm_r,  .46_pm_r,101._pm_r, &
245.50_pm_r, 58160._pm_r,   2._pm_r,148._pm_r, 4.65_pm_r,349._pm_r,   7._pm_r, 56._pm_r,  .49_pm_r,112._pm_r, &
234.80_pm_r, 61677._pm_r,   4._pm_r,353._pm_r, 4.00_pm_r,340._pm_r,   8._pm_r, 61._pm_r,  .47_pm_r,124._pm_r, &
227.80_pm_r, 65059._pm_r,   9._pm_r,343._pm_r, 3.05_pm_r,331._pm_r,   8._pm_r, 65._pm_r,  .41_pm_r,136._pm_r, &
223.30_pm_r, 68363._pm_r,  13._pm_r,339._pm_r, 2.08_pm_r,321._pm_r,   8._pm_r, 69._pm_r,  .36_pm_r,148._pm_r, &
219.60_pm_r, 71605._pm_r,  15._pm_r,335._pm_r, 1.30_pm_r,304._pm_r,   8._pm_r, 73._pm_r,  .32_pm_r,159._pm_r, &
216.30_pm_r, 74797._pm_r,  16._pm_r,331._pm_r,  .83_pm_r,273._pm_r,   8._pm_r, 76._pm_r,  .28_pm_r,172._pm_r, &
213.10_pm_r, 77944._pm_r,  17._pm_r,327._pm_r,  .73_pm_r,236._pm_r,   8._pm_r, 79._pm_r,  .26_pm_r,178._pm_r, &
205.50_pm_r, 81043._pm_r,  16._pm_r,324._pm_r,  .80_pm_r,213._pm_r,   8._pm_r, 81._pm_r,  .23_pm_r,187._pm_r, &
192.50_pm_r, 83938._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
181.20_pm_r, 86645._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
174.60_pm_r, 89234._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
173.70_pm_r, 91786._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
177.90_pm_r, 94374._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
187.30_pm_r, 97076._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
202.70_pm_r, 99991._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
225.80_pm_r,103233._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
255.70_pm_r,106934._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
291.60_pm_r,111214._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
368.10_pm_r,116417._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_reel),dimension(10*pm_nb_lat*pm_nb_alt),parameter::donnees_mars=&
       (/donnees_lat_moins_80(:),donnees_lat_moins_70(:),donnees_lat_moins_60(:),donnees_lat_moins_50(:),&
       donnees_lat_moins_40(:),donnees_lat_moins_30(:),donnees_lat_moins_20(:),donnees_lat_moins_10(:),&
       donnees_lat_0(:),donnees_lat_10(:),donnees_lat_20(:),donnees_lat_30(:),donnees_lat_40(:),donnees_lat_50(:),&
       donnees_lat_60(:),donnees_lat_70(:),donnees_lat_80(:)/)

real(pm_reel),dimension(10,pm_nb_alt,pm_nb_lat)::donnees

  character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
       '@(#) Fichier MSPRO mpi_atmi_mars.f90: derniere modification V5.15 >'

  !************************************************************************

  ! initialisation de la valeur du code retour
  ! ..........................................
 donnees=reshape(donnees_mars,(/10,pm_nb_alt,pm_nb_lat/))
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

end subroutine mpi_atmi_mars
