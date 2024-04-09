#!/bin/bash
echo "--------------------"
echo "- SH.MET.PipeLine  -"
echo "-    OK            -"
echo "--------------------"

# Ce script liste les fichiers qu'il y a dans le dossier actuel
# et fait la substituition de les vocals [aeio] pas [u]

 ls -l | sed -e "s/[aeio]/u/g"  

function test ()
{
   test1 ()
      if [$compress]; then
        # this is a comment for the pipe command
        tar tvf ./archive.tar | awk '{print $3, $6}' | egrep '\.c$' | sort -nr | head -1
      fi
   test1
   # another comment
   
    ls -l | sed -e "s/[eiou]/a/g"  
}
# Cette commande permet de recherche dans un archive tous les
# fichier d’extension « .c » et d’afficher le plus volumineux
tar tvf ./archive.tar | awk '{print $3, $6}' | egrep '\.c$' | sort -nr | head -1

case ${OPTARG} in
                help ) input_to_process=0;
                    usage;;
                version ) input_to_process=0;
                    echo "ql_s2 V1.6 using OTB 6.6";;
                sd=?* ) destination_sub_directory=${LONG_OPTARG};;
                sd* ) echo "No arg for --${OPTARG} option" >&2; 
                     exit 1;;
                rgb | irgb | comp ) quicklook_type=${OPTARG};;
                rgb* | irgb* | comp* )
                    echo "No arg allowed for --${OPTARG} option" >&2; 
                    exit 1;;
                '') break ;; # "--" terminates argument processing
                *)  echo "No arg allowed for --${OPTARG} option" >&2; 
                    exit 1;;
esac