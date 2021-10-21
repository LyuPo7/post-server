#!/bin/bash

helpFunction()
{
   echo ""
   echo "Usage: $0 -y host -p port -t token -o offset"
   echo -e "\t-y Host name"
   echo -e "\t-p Port number"
   echo -e "\t-t User's token"
   echo -e "\t-o Offset"
   exit 1 # Exit script after printing help
}

while getopts "y:p:t:o:" opt
do
   case "$opt" in
      y ) host="$OPTARG" ;;
      p ) port="$OPTARG" ;;
      t ) token="$OPTARG" ;;
      o ) offset="$OPTARG" ;;
      ? ) helpFunction ;; # Print helpFunction in case parameter is non-existent
   esac
done

if [ -z "${token+x}" ]
then opt=""
else opt="token=$token"
fi

if [ -z "${offset+x}" ]
then opt="$opt"
else opt="$opt&offset=$offset"
fi

url=http://$host:$port/getTags?$opt
curl ${url}