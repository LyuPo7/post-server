#!/bin/bash

helpFunction()
{
   echo ""
   echo "Usage: $0 -h host -p port -t token -i post_id -l path"
   echo -e "\t-h Host name"
   echo -e "\t-p Port number"
   echo -e "\t-t User's token"
   echo -e "\t-i Post's id"
   echo -e "\t-i Path to photo"
   exit 1 # Exit script after printing help
}

while getopts "h:p:t:i:l:" opt
do
   case "$opt" in
      h ) host="$OPTARG" ;;
      p ) port="$OPTARG" ;;
      t ) token="$OPTARG" ;;
      i ) postId="$OPTARG" ;;
      l ) photoPath="$OPTARG" ;;
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

if [ -z "${photoPath+x}" ]
then opt="$opt"
else opt="$opt&path=$photoPath"
fi

echo $opt
url=http://$host:$port/setPostMainPhoto?$opt
curl ${url}