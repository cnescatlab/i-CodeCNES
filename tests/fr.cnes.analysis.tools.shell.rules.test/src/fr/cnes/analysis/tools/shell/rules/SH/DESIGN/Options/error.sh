#!/bin/bash

echo "---------------------"
echo "- SH.DESIGN.OPTIONS -"
echo "-        KO         -"
echo "---------------------"

function getopts_internal()
{
   opt=0
   optspec="u:n:h"

   if [ $# -eq 0 ]; then
      return 0
   else

      while getopts "$optspec" OPTION
      do
         echo "The option selected is $1 inside $optspec"
         case "${OPTION}" in
#            h)
#               echo "You choose the HELP option."
#            ;;
#            v)
#               echo "This is the VERSION option."
#            ;;
            *)
               echo "Invalid option -$OPTARG requires an argument."
            ;;
         esac
      done

   return $opt_trouve
}

echo "Call GETOPS_INTERNAL with parameter H."
getopts_internal -h

