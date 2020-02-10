#!/bin/bash

echo "---------------------"
echo "- SH.DESIGN.OPTIONS -"
echo "-        OK         -"
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
         case "${OPTION}" in
            h)
               echo "You choose the HELP option."
            ;;
            n)
               echo "-n option of getopts."
            ;;
			v)
               echo "This is the VERSION option."
            ;;
            *)
               echo "Invalid option -$OPTARG requires an argument."
            ;;
         esac
      done
   fi

   return $opt_trouve
}

function getopt_external()
{
    params="$(getopt -o hvacb: -l help,verbose,all,clear,base: 
								--name "$(basename "$0")" -- "$@")"
    if [ $? -ne 0 ]; then
      return 128
    fi

    eval set -- "$params"
    unset params

    while true
    do
    case $1 in
        -h|--help)
            return 1
            ;;
        -v|--version)
            version=true
            shift
            ;;
        -a|--all)
            all=true
            shift
            ;;
        -b|--base)
            base=true
            shift
            base_value=$1
            shift
            ;;
        -c|--clear)
            clear=true
            shift
            ;;
        --)
            shift
            break
            ;;
        *)
            return 128
            ;;
    esac
    done
    return 0
}

echo "Call GETOPS_INTERNAL with parameter H."
getopts_internal -h

