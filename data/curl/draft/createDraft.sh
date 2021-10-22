#!/bin/bash

helpFunction()
{
   echo ""
   echo "Usage: $0 -y host -p port -t token -b text -i post_id"
   echo -e "\t-y Host name"
   echo -e "\t-p Port number"
   echo -e "\t-t User's token"
   echo -e "\t-b Draft's text"
   echo -e "\t-i Post's id"
   exit 1 # Exit script after printing help
}

while getopts "y:p:t:b:i:" opt
do
   case "$opt" in
      y ) host="$OPTARG" ;;
      p ) port="$OPTARG" ;;
      t ) token="$OPTARG" ;;
      b ) text="$OPTARG" ;;
      i ) postId="$OPTARG" ;;
      ? ) helpFunction ;; # Print helpFunction in case parameter is non-existent
   esac
done

if [ -z "${token+x}" ]
then opt=""
else opt="token=$token"
fi

if [ -z "${text+x}" ]
then opt="$opt"
else opt="$opt&text=$text"
fi

if [ -z "${postId+x}" ]
then opt="$opt"
else opt="$opt&post_id=$postId"
fi

url=http://$host:$port/createDraft?$opt
curl ${url}