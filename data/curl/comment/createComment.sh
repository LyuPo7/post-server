#!/bin/bash

helpFunction()
{
   echo ""
   echo "Usage: $0 -y host -p port -t token -i post_id -c text"
   echo -e "\t-y Host name"
   echo -e "\t-p Port number"
   echo -e "\t-t User's token"
   echo -e "\t-i Post's id"
   echo -e "\t-c Comment's text"
   exit 1 # Exit script after printing help
}

while getopts "y:p:t:i:c:" opt
do
   case "$opt" in
      y ) host="$OPTARG" ;;
      p ) port="$OPTARG" ;;
      t ) token="$OPTARG" ;;
      i ) postId="$OPTARG" ;;
      c ) text="$OPTARG" ;;
      ? ) helpFunction ;; # Print helpFunction in case parameter is non-existent
   esac
done

if [ -z "${token+x}" ]
then opt=""
else opt="token=$token"
fi

if [ -z "${postId+x}" ]
then opt="$opt"
else opt="$opt&post_id=$postId"
fi

if [ -z "${text+x}" ]
then opt="$opt"
else opt="$opt&text=$text"
fi

url=http://$host:$port/createComment?$opt
curl ${url}