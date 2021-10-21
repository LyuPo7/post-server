#!/bin/bash

helpFunction()
{
   echo ""
   echo "Usage: $0 -y host -p port -t token -l path"
   echo -e "\t-y Host name"
   echo -e "\t-p Port number"
   echo -e "\t-t User's token"
   echo -e "\t-l Path to photo"
   exit 1 # Exit script after printing help
}

while getopts "y:p:t:l:" opt
do
   case "$opt" in
      y ) host="$OPTARG" ;;
      p ) port="$OPTARG" ;;
      t ) token="$OPTARG" ;;
      l ) photoPath="$OPTARG" ;;
      ? ) helpFunction ;; # Print helpFunction in case parameter is non-existent
   esac
done

if [ -z "${token+x}" ]
then opt=""
else opt="token=$token"
fi

if [ -z "${photoPath+x}" ]
then opt="$opt"
else opt="$opt&path=$photoPath"
fi

url=http://$host:$port/setUserPhoto?$opt
curl ${url}