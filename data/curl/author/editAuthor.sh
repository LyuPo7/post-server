#!/bin/bash

helpFunction()
{
   echo ""
   echo "Usage: $0 -y host -p port -t token -i user_id -d description"
   echo -e "\t-y Host name"
   echo -e "\t-p Port number"
   echo -e "\t-t User's token"
   echo -e "\t-i User's id"
   echo -e "\t-d Author's new description"
   exit 1 # Exit script after printing help
}

while getopts "y:p:t:i:d:" opt
do
   case "$opt" in
      y ) host="$OPTARG" ;;
      p ) port="$OPTARG" ;;
      t ) token="$OPTARG" ;;
      i ) id="$OPTARG" ;;
      d ) newDescription="$OPTARG" ;;
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

if [ -z "${newDescription+x}" ]
then opt="$opt"
else opt="$opt&description=$newDescription"
fi

url=http://$host:$port/editAuthor?$opt
curl ${url}