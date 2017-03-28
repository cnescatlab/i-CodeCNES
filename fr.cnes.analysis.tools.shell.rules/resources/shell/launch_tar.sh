# Script : Launch_tar_tmarchive.sh
# Author: Arnaud Poulain
# Date : 01/08/2014
############################################################################################
DayToTar=$(date -d "1 week ago" "+%Y%m%d")
echo "$(date)  -------  Lauch of TMArchive   --------"
bash /GFS_SAGA/DPCCTools/TarTools/tar_TMArchive.sh $DayToTar
echo "$(date) -------- Lauch of Daily Nominal  ------"
bash /GFS_SAGA/DPCCTools/TarTools/tar_daily_nominal.sh $DayToTar
echo "$(date) -------- Lauch of Daily Backup   ------"
bash /GFS_SAGA/DPCCTools/TarTools/tar_daily_backup.sh $DayToTar
if=4