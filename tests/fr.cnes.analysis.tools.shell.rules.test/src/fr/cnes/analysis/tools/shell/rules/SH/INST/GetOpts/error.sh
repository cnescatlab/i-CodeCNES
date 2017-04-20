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
     echo "function getopts_internal"
   fi
   return 0
}
#---------------------------------------------------------
# direct_calls : Parameters use directly in the script.
#---------------------------------------------------------
function direct_calls()
{
	echo "Welcome to user $1"
	TEMP = $*
	echo "List of all your friends including yourself $#"
	
	RIMS_TYPE=$1
	RIMS_DIR=$2
	FILE_IN="$3"
	FILE_OUT=$4
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

