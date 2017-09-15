#!/bin/sh


echo "----------------"
echo "- SH.ERR.HELP  -"
echo "-    OK        -"
echo "----------------"

TEMP=$(getopt -o hv:o:: --long help,version,other:: -- "$@")

if [ $? != 0 ]; then
        echo "Error, no option"
        exit 1;
fi
eval set -- "$TEMP"
while true; do
        case $1 in
                -h|--help)
                        echo "Help command"
                        shift
                        ;;
                -v|--version)
                        echo "Version command"
                        shift
                        ;;
                --other|--|*)
                        echo use -h or -help
                        shift
                        break
                        ;;
                esac
done

