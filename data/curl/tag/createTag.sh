#!/bin/bash

helpFunction()
{
   echo ""
   echo "Usage: $0 -t token -p port -y host -n tag"
   echo -e "\t-y Host name"
   echo -e "\t-p Port number"
   echo -e "\t-t User's token"
   echo -e "\t-n Tag's title"
   exit 1 # Exit script after printing help
}

while getopts "y:p:t:n:" opt
do
   case "$opt" in
      y ) host="$OPTARG" ;;
      p ) port="$OPTARG" ;;
      t ) token="$OPTARG" ;;
      n ) tag="$OPTARG" ;;
      ? ) helpFunction ;; # Print helpFunction in case parameter is non-existent
   esac
done

if [ -z "${token+x}" ]
then opt=""
else opt="token=$token"
fi

if [ -z "${tag+x}" ]
then opt="$opt"
else opt="$opt&title=$tag"
fi

url=http://$host:$port/createTag?$opt
curl ${url}