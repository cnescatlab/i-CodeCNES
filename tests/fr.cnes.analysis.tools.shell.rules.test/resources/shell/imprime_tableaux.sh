for MACHINE in  ${REF_MACHINES[@]} ; do

  ## Les des machines et des information associe
  echo "############################### $MACHINE"
  echo "La machine en cours de traitement est $MACHINE"
  echo "Sa configuration est :"
  eval echo -e \"\\tNom de machine : \${CONF_${MACHINE}[0]}\"
  eval echo -e \"\\tSont fichier de log est  : \${CONF_${MACHINE}[1]}\"
  eval echo -e \"\\tLes fichiers de log devant etre envoyé sont  : \${CONF_${MACHINE}[2]}\"
  eval echo -e \"\\tLes informations pour accerder à la machine sont  : \${CONF_${MACHINE}[3]}\"
  eval echo -e \"\\tLes options necesaire a la connexion sont  : \${CONF_${MACHINE}[4]}\"
  eval echo -e \"\\tLa liste des partitions devant etre sauvegarder  : \${CONF_${MACHINE}[5]}\"
  eval echo -e \"\\tLe resultat du teste de connexion a la machine devant etre sauvegarder  : \${CONF_${MACHINE}[6]}\"
  eval echo -e \"\\tL OS de la machine est : \${CONF_${MACHINE}[7]}\"
  eval echo -e \"\\tLa version de l OS est : \${CONF_${MACHINE}[8]}\"
  eval echo -e \"\\tLe prefix permettant d executer des commandes sur la machine est : \${CONF_${MACHINE}[9]}\"


  ## Liste les partitions
  eval echo -e \"\\t\\tListes des partitions de la machine $MACHINE : \${CONF_${MACHINE}[5]}\"
  echo -e "\t\t########## ou de cette facon ################"
  eval LIST_PART_REF_MACHINE=\${PART_REF_${MACHINE}[@]}
  for REF_PART in $LIST_PART_REF_MACHINE ; do
    eval echo -e \"\\t\\t\${PART_${MACHINE}_${REF_PART}[0]}\"
  done

  ## Liste les informations de chaque partition
  eval LIST_PART_REF_MACHINE=\${PART_REF_${MACHINE}[@]}
  for REF_PART in $LIST_PART_REF_MACHINE ; do
    eval echo -e \"\\t\\t\\t##### Detail de la partitions : \${PART_${MACHINE}_${REF_PART}[0]} \"
    eval echo -e \"\\t\\t\\tRepetoire devant etre une partition : \${PART_${MACHINE}_${REF_PART}[0]}\"
    eval echo -e \"\\t\\t\\tLa taille en Ko de cette partition : \${PART_${MACHINE}_${REF_PART}[1]}\"
    eval echo -e \"\\t\\t\\tL espace utilise de cette partition : \${PART_${MACHINE}_${REF_PART}[2]}\"
    eval echo -e \"\\t\\t\\tLe point de montage de cette partition : \${PART_${MACHINE}_${REF_PART}[3]}\"
    eval echo -e \"\\t\\t\\tLa taille en block de la partition : \${PART_${MACHINE}_${REF_PART}[4]}\"
    eval echo -e \"\\t\\t\\tLe type de FS de la partition : \${PART_${MACHINE}_${REF_PART}[5]}\"
  done


done
