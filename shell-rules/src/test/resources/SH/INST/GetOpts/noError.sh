#!/bin/bash

# --------------------------------------------------------
# Mise en oeuvre de la fonction interne du shell : getopts
# --------------------------------------------------------
function getopts_internal()
{
   optspec="hvacb:"

   if [ $# -eq 0 ]; then
      return 0
   else
      while getopts "$optspec" OPTION
      do
         case "${OPTION}" in
            h)
               return 1
            ;;
            v)
               verbose=true
            ;;
            a)
               all=true
            ;;
            c)
               clear=true
            ;;
            b)
               base=true
               base_value=$OPTARG
            ;;
            \?)
               echo "Invalid option: -$OPTARG" >&2
               return 128
            ;;
            :)
               echo "Option -$OPTARG requires an argument." >&2
               return 128
            ;;
         esac
      done
   fi
   return 0
}

# ---------------------------------------------------------------
# Mise en oeuvre de la fonction externe : getopt 
# ---------------------------------------------------------------
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
        -v|--verbose)
            verbose=true
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

# --------------------------------------------------------
# usage : pas de parametre
# --------------------------------------------------------
function usage()
{
   echo "usage: $0 -a|--all -b|--base <argument> -c|--clear -h|
											--help -v|--verbose"
   echo "   Cette commande accepte 5 parametres optionnels : "
   echo "      -a ou --all"
   echo "      -b <argument> ou --base <argument>"
   echo "      -c ou --clear"
   echo "      -h ou --help"
   echo "      -v ou --verbose"
}
