#!/bin/ksh
#===============================================================================
# ConvertEWAN.ksh :  Converts the raw RIMS file coming from the Archive Server in ASCII
#
# The following global environment must be defined :
#	- AIVP_LOCAL_INPUT_PATH,
#	- AIVP_CONVERTER,
#	- AIVP_IP,
#	- AIVP_LOGIN
#	- [ CONS_3 , only for RIMS A/B ]CHANNEL
#
# ----------------
#
#   Created the : 27/09/2006
#   by          : C.LE NORMAND
# 
# -----------------------------------------------------
# HISTORIQUE
# VERSION : 23/10/2009 : B. GUENDE
# DM-ID : DM-LOGINAV : 09/09/2009 : Portage LOGINAV
#       Removal of "ls -s" commands too dependant of the filesystem and causing issues with the new host
#
# VERSION : 20/03/2009 : A. BLAGA
# DM-ID : DM-0002 : 13/03/2009 : EWAN
# 	Recodage integral pour utiliser le convertisseur OAT (CCF_archive_rims_converter)
#	Il utilise le module Wave "salmon_process" pour activer le monitoring OAT.
#
# VERSION : 13/03/2009 : A. BLAGA
# FA-ID : FA-0018 : 13/03/2009 : non conformite des cartouches
#
# VERSION : 23/10/2007 : C. LENORMAND
# DM-ID : DM : 23/10/2007 :  DM-EGN-NT-E352E-0492-TH après validation
#
# VERSION : 10/10/2007 : C. LENORMAND
# DM-ID : DM : 10/10/2007 :  DM-EGN-NT-E352E-0492-TH
#
# VERSION : 04/01/2007 : C. LENORMAND
# DM-ID : DM : 04/01/2007 :  Logs
#
# VERSION : 04/01/2007 : C. LENORMAND
# FA-ID : FA-0005 : 04/01/2007 :  FA-0005-part2 corrections
#
# VERSION : 27/09/2006 : C. LENORMAND
# DM-ID : DM : 27/09/2006 :  Creation
#
# FIN HISTORIQUE
# -----------------------------------------------------
#
#===============================================================================
# VERSION $Id: ConvertEWAN.ksh,v 1.13 2010/03/03 16:52:44 loginav Exp $

MODULE="ConvertEWAN.ksh"


#-------------------------------------------------------------------------------
# Loads the current SALMON environment :
#	- SALMON static configuration
#	- SALMON dynamic configurations
#-------------------------------------------------------------------------------
#CLN : a remplacer
. ${TOOLS_DIR}/CommonTools.def


#-------------------------------------------------------------------------------
# Internal parameters
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Script usage 
#-------------------------------------------------------------------------------
if [[ $# != 4 ]]; then
  echo ""
  echo "Converts the raw RIMS file coming from the Archive Server in ASCII"
  echo "=================================================="
  echo "USAGE : $MODULE <RIMS_type> <RIMS_dir> <file_in> <file_out>"
  echo " <RIMS_type> 	: RIMS type : A/B/C/N(NLES)"
  echo " <RIMS_dir>     : Directory of input RIMS files"  
  echo " <file_in>      : Input filename (of raw RIMS file)"
  echo " <file_out>   	: Output filename (of ASCII RIMS file)"
  echo "RESULTS : stdout -> None"
  echo "          stderr -> errors"
  echo "          status -> 0 if OK (an empty list is considered OK)"
  echo "EX : $MODULE A ~oatadmin/RECORD/Receiver/CHANNEL/Data RIMS_A_Raw_Data_0001_20060202_000000.edd RIMS_A_Raw_Data_0001_20060202_000000"
  echo ""
  exit 9
fi


#-------------------------------------------------------------------------------
# Initializes inputs
#-------------------------------------------------------------------------------
RIMS_TYPE=$1
RIMS_DIR=$2
FILE_IN="$3"
FILE_OUT=$4


#===============================================================================
# MAIN
#===============================================================================
#-------------------------------------------------------------------------------
# Broadcast a begin message to the info log file
#-------------------------------------------------------------------------------
${TOOLS_DIR}/Log.ksh -s $MODULE $SALMON_INFO_LOG I "Starting EWAN conversion of ${FILE_IN} "

#-------------------------------------------------------------------------------
# CHECK INPUT PARAMETERS
#-------------------------------------------------------------------------------
# RIMS type
#========================================================================================
# DM-EGN-NT-E352E-0492-TH (CLN) - 12/10/2007
# Integration of NLES in A/B RIMS list   
#========================================================================================
if [[ "X""${RIMS_TYPE}" != "X""A" && "X""${RIMS_TYPE}" != "X""B" && "X""${RIMS_TYPE}" != "X""N" && "X""${RIMS_TYPE}" != "X""C" && "X""${RIMS_TYPE}" != "X""a" && "X""${RIMS_TYPE}" != "X""b" && "X""${RIMS_TYPE}" != "X""n" && "X""${RIMS_TYPE}" != "X""c" ]]
then
	${TOOLS_DIR}/Log.ksh $MODULE $SALMON_ERROR_LOG E "The RIMS type ${RIMS_TYPE} is unknown : A/B/C/N(NLES) expected"
	${TOOLS_DIR}/Log.ksh -e $MODULE $SALMON_INFO_LOG E "Abnormal termination of EWAN conversion"
	exit 9
fi
# End of DM-EGN-NT-E352E-0492-TH - Integration of NLES in A/B RIMS list
#========================================================================================

	
#  Input FILES
# Input RIMS FILE
if [ ! -f ${RIMS_DIR}/${FILE_IN} ]
then
	${TOOLS_DIR}/Log.ksh $MODULE $SALMON_ERROR_LOG E "The input file ${RIMS_DIR}/${FILE_IN} does not exist"
	${TOOLS_DIR}/Log.ksh -e $MODULE $SALMON_INFO_LOG E "Abnormal termination of EWAN conversion for ${RIMS_DIR}/${FILE_IN}"
	exit 9
fi
file_in="${RIMS_DIR}/${FILE_IN}"

# Output RIMS file
# FA-0005-part2 (CLN) - 04/01/2007
# FA-0005-part2 : For timing analysis, the output filename contains the studied constellation
#========================================================================================
# DM-EGN-NT-E352E-0492-TH (CLN) - 12/10/2007
# Integration of NLES in A/B RIMS list   
#========================================================================================
if [[ "X""${RIMS_TYPE}" = "X""A" || "X""${RIMS_TYPE}" = "X""a" || "X""${RIMS_TYPE}" = "X""B" || "X""${RIMS_TYPE}" = "X""b" || "X""${RIMS_TYPE}" = "X""N" || "X""${RIMS_TYPE}" = "X""n" ]]
then
#========================================================================================
# DM-LOGINAV (BGU) - 24/02/2010
# Portage LOGINAV 
#========================================================================================
	#out=`ls -s ${RIMS_DIR}/${FILE_OUT}_${CONS_3} 2> /dev/null`
	out=${RIMS_DIR}/${FILE_OUT}_${CONS_3}
else
	#out=`ls -s ${RIMS_DIR}/${FILE_OUT} 2> /dev/null`
	out=${RIMS_DIR}/${FILE_OUT}
fi
# End of DM-EGN-NT-E352E-0492-TH - Integration of NLES in A/B RIMS list
#========================================================================================
# End of FA-0005-part2

#if [ "X""${out}" != "X""" ]
if [ -f $out ]
then
	# Gets the output file size
	#outSize=`echo ${out} | cut -d" " -f1`
	#if [ ! $outSize -eq 0 ]
	if [ -s $out ]
	then
		${TOOLS_DIR}/Log.ksh $MODULE $SALMON_ERROR_LOG W "The converted EWAN file already exists"
		${TOOLS_DIR}/Log.ksh -e $MODULE $SALMON_INFO_LOG W "Normal termination of EWAN conversion"
		exit 0
	fi
fi
file_out="${RIMS_DIR}/${FILE_OUT}"
#========================================================================================
# END DM-LOGINAV
#========================================================================================

#-------------------------------------------------------------------------------
# Nouvelle version, s'appuyant sur le convertisseur OAT
#-------------------------------------------------------------------------------

   if [[ "X""${RIMS_TYPE}" = "X""A" || "X""${RIMS_TYPE}" = "X""a" || "X""${RIMS_TYPE}" = "X""B" || "X""${RIMS_TYPE}" = "X""b" || "X""${RIMS_TYPE}" = "X""N" || "X""${RIMS_TYPE}" = "X""n" ]]
   then
     # RIMS A / B / N (NLES)
     # Check ${CONSTELLATION}
     # =====================
     if [[ "X""${CONS_3}" != "X""GPS" && "X""${CONS_3}" != "X""GEO" && "X""${CONS_3}" != "X""GLO" ]]
     then
        ${TOOLS_DIR}/Log.ksh $MODULE $SALMON_ERROR_LOG E "CONSTELLATION <${CONSTELLATION}> must be positionned (GPS/GLO/GEO) "
        ${TOOLS_DIR}/Log.ksh -e $MODULE $SALMON_INFO_LOG E "Abnormal termination of EWAN conversion for ${RIMS_DIR}/${FILE_IN}"
        exit 9
     fi
   fi

   # Creation du fichier de conf:
   # ===========================
   CONVERT_CFG=${AE_DB_RECORDS}/${REAL_RECORD}/Receiver/${CHANNEL}/CCF_archive_rims_converter.cfg
   chmod a+w $CONVERT_CFG 2> /dev/null

   echo "# Configuration file of CCF_archive_rims_converter" > $CONVERT_CFG
   echo "# Path and name to the CCF_archive data file" >> $CONVERT_CFG
   echo "${file_in}" >> $CONVERT_CFG
   echo "# Results directory" >> $CONVERT_CFG
   echo "/Results" >> $CONVERT_CFG

   # Lancement d'un leurre pour db_execution
   # =======================================
   cd $TOOLS_DIR/bin
   $WAVE_DIR/bin/wave -r salmon_process > /dev/null 2>&1 &
   wpid=$!
   cd - >/dev/null

   $RIMSEWAN_TOOLS/CCF_archive_rims_converter/CCF_archive_rims_converter.ksh ${REAL_RECORD} ${CHANNEL} > /tmp/log$$
   if [ $? != 0 ]
   then
      ${TOOLS_DIR}/Log.ksh $MODULE $SALMON_ERROR_LOG E "Cannot run CCF_archive_rims_converter.ksh"
      ${TOOLS_DIR}/Log.ksh -e $MODULE $SALMON_INFO_LOG E "Abnormal termination of EWAN conversion for ${RIMS_DIR}/${FILE_IN}"

      if [ ! "$wpid" = "" ]
      then
        kill -9 $wpid
      fi

      exit 9
   fi

   # Renommage des sorties:
   # ===========================
   if [[ "X""${RIMS_TYPE}" = "X""A" || "X""${RIMS_TYPE}" = "X""a" || "X""${RIMS_TYPE}" = "X""B" || "X""${RIMS_TYPE}" = "X""b" ]]
   then
     mv ${AE_DB_RECORDS}/${REAL_RECORD}/Receiver/${CHANNEL}/Results/GPS.txt ${file_out}_GPS
     mv ${AE_DB_RECORDS}/${REAL_RECORD}/Receiver/${CHANNEL}/Results/GLONASS.txt ${file_out}_GLO
     mv ${AE_DB_RECORDS}/${REAL_RECORD}/Receiver/${CHANNEL}/Results/GEO.txt ${file_out}_GEO
     # Purge resultats inutiles:
     # ===========================
     rm -f ${AE_DB_RECORDS}/${REAL_RECORD}/Receiver/${CHANNEL}/Results/GPS_nav.txt
   fi

   if [[ "X""${RIMS_TYPE}" = "X""N" || "X""${RIMS_TYPE}" = "X""n" ]]
   then
     mv ${AE_DB_RECORDS}/${REAL_RECORD}/Receiver/${CHANNEL}/Results/GPS.txt ${file_out}_GPS
   fi

   if [[ "X""${RIMS_TYPE}" = "X""C" || "X""${RIMS_TYPE}" = "X""c" ]]
   then
     mv ${AE_DB_RECORDS}/${REAL_RECORD}/Receiver/${CHANNEL}/Results/RIMS_C_data.txt ${file_out}
   fi

   # Test resultats
   # ===========================
   if [[ "X""${RIMS_TYPE}" = "X""A" || "X""${RIMS_TYPE}" = "X""a" || "X""${RIMS_TYPE}" = "X""B" || "X""${RIMS_TYPE}" = "X""b" || "X""${RIMS_TYPE}" = "X""N" || "X""${RIMS_TYPE}" = "X""n" ]]
   then
#========================================================================================
# DM-LOGINAV (BGU) - 24/02/2010
# Portage LOGINAV 
#========================================================================================
	#out=`ls -s ${file_out}_${CONS_3}`
	out=${file_out}_${CONS_3}
   else
	#out=`ls -s ${file_out}`
	out=${file_out}
   fi

   #if [ "X""${out}" = "X""" ]
   if [ ! -f $out ]
   then
      ${TOOLS_DIR}/Log.ksh $MODULE $SALMON_ERROR_LOG E "The file ${file_out} has not been produced"
      ${TOOLS_DIR}/Log.ksh -e $MODULE $SALMON_INFO_LOG E "Abnormal termination of EWAN conversion for ${RIMS_DIR}/${FILE_IN}"

      if [ ! "$wpid" = "" ]
      then
        kill -9 $wpid
      fi

      exit 9
   else
      #if [ `echo ${out} | cut -d" " -f1` -eq 0 ]
	  if [ ! -s $out ]
      then
         ${TOOLS_DIR}/Log.ksh $MODULE $SALMON_ERROR_LOG E "The produced output converted file is empty"
         ${TOOLS_DIR}/Log.ksh -e $MODULE $SALMON_INFO_LOG E "Abnormal termination of EWAN conversion for ${RIMS_DIR}/${FILE_IN}"

         if [ ! "$wpid" = "" ]
         then
           kill -9 $wpid
         fi

         exit 9
      fi	
   fi
#========================================================================================
# END OF DM-LOGINAV
#========================================================================================
   # Sortie
   # ===========================
   ${TOOLS_DIR}/Log.ksh -e $MODULE $SALMON_INFO_LOG I "Normal termination of EWAN conversion on $FILE_IN"

   if [ ! "$wpid" = "" ]
   then
     kill -9 $wpid
   fi

#-------------------------------------------------------------------------------
# FIN Nouvelle version, s'appuyant sur le convertisseur OAT
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# END OF SCRIPT  
#-------------------------------------------------------------------------------

exit 0

