#!/bin/bash

helpFunction()
{
   echo ""
   echo "Usage: $0 -y host -p port -t token -i author_id"
   echo -e "\t-y Host name"
   echo -e "\t-p Port number"
   echo -e "\t-t User's token"
   echo -e "\t-i User's id"
   exit 1 # Exit script after printing help
}

while getopts "y:p:t:i:" opt
do
   case "$opt" in
      y ) host="$OPTARG" ;;
      p ) port="$OPTARG" ;;
      t ) token="$OPTARG" ;;
      i ) id="$OPTARG" ;;
      ? ) helpFunction ;; # Print helpFunction in case parameter is non-existent
   esac
done

if [ -z "${token+x}" ]
then opt=""
else opt="token=$token"
fi

if [ -z "${id+x}" ]
then opt="$opt"
else opt="$opt&id=$id"
fi

url=http://$host:$port/removeAuthor?$opt
curl ${url}