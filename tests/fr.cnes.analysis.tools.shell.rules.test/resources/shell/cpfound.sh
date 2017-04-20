#! /bin/sh
# #############################################################################

       NAME_="cpfound"
       HTML_="find files and copy"
    PURPOSE_="recursively find files and copy to a specified dir"
   SYNOPSIS_="$NAME_ [-vhl] [-i <input_dir>] -o <output_dir> -p \"<pattern>\" -s +|-<n>"
   REQUIRES_="standard GNU commands"
    VERSION_="1.2"
       DATE_="2001-10-08; last update: 2006-09-07"
     AUTHOR_="Dawid Michalczyk <dm@eonworks.com>"
        URL_="www.comp.eonworks.com"
   CATEGORY_="file"
   PLATFORM_="Linux"
      SHELL_="bash"
 DISTRIBUTE_="yes"

# #############################################################################
# This program is distributed under the terms of the GNU General Public License

# HISTORY:
# 2006-09-07 v1.2 - improved the way an integer is appended to files with same
#                   file name.


usage () {

echo >&2 "$NAME_ $VERSION_ - $PURPOSE_
Usage: $SYNOPSIS_
Requires: $REQUIRES_
Options:
     -i, <input_dir>, path to where to look for files. Only needed if current
         dir is not used as input dir.
     -o, <output_dir>, path to where to copy the found files. In case files with
         same filenames are found, all copied files have a \"_<n>\" appended
         incrementally to the prefix where n starts at 1.
     -s, +|-<n>, file size in bytes; +n for greater then n; -n for less then n;
         n for exactly n.
     -p, \"<pattern>\", search file pattern as accepted by find's -name option; no
         case distinction is made.
     -v, verbose
     -h, usage and options (this help)
     -l, see this script"
    exit 1
}

# args check
[ $# -eq 0 ] && { echo >&2 missing argument, type $NAME_ -h for help; exit 1; }

trap "exit 1" 1 2 3 15

# var init
file_size=
input_dir=
output_dir=
file_pattern=
verbose=

while getopts vhli:o:s:p: options; do

    case "$options" in
        s) file_size="$OPTARG" ;;
        i) input_dir="$OPTARG" ;;
        o) output_dir="$OPTARG" ;;
        p) file_pattern="$OPTARG" ;;
        v) verbose=on ;;
        h) usage ;;
        l) more $0; exit 1 ;;
       \?) echo invalid argument, type $NAME_ -h for help; exit 1 ;;

    esac

done
shift $(( $OPTIND - 1 ))

# args check
[[ $file_pattern ]] || { echo >&2 missing file pattern argument; exit 1; }
[[ $file_size ]] || { echo >&2 missing file size argument; exit 1; }
[[ -d "$output_dir" ]] || { echo >&2 output dir "$output_dir" does not exist; exit 1; }

c=1
# local fnc
rename_file() {
# usage: fnc <prefix> <suffix>

    # keep on checking if file exist; if so, append integer to prefix and check again
    # Although checking one file at a time starting from 1 may seem like a slow
    # solution, it's only slow in situations when dealing with huge amounts of
    # files with same file names.

    if [ -f "${odir}"/"${prefix}"_${c}."${suffix}" ] ;then

        ((c++))
        rename_file "${prefix}" "${suffix}"

    # file does not exist; return a valid file name
    else
        echo "${prefix}"_${c}."${suffix}"
        c=1
    fi

}

find_and_cp() {

    find . -type f -iname "${file_pattern}" -size ${file_size}c | while read a; do

        file="${a##*/}"
        prefix="${file%%.*}"
        suffix="${file#*.}"

        file_name=$(rename_file "${prefix}" "${suffix}")
        [[ $verbose ]] && echo ${NAME_}: copying "$a -> ${odir}/${file_name}"
        cp -- "$a" "${odir}"/"${file_name}"

    done

}

# relative and full path support
odir=($(cd "$output_dir"; pwd;))

# main
if [ $input_dir ];then

    # relative and full path support
    idir=($(cd "$input_dir"; pwd;))
    [ -d "$idir" ] || { echo >&2 input dir "$idir" does not exist; exit 1; }
    cd -- "$idir" || { echo >&2 can not cd to "$idir"; exit 1; }
    find_and_cp

else

    find_and_cp

fi
