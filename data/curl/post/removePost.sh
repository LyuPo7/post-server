#!/bin/bash

helpFunction()
{
   echo ""
   echo "Usage: $0 -h host -p port -t token -i post_id"
   echo -e "\t-h Host name"
   echo -e "\t-p Port number"
   echo -e "\t-t User's token"
   echo -e "\t-i Post's id"
   exit 1 # Exit script after printing help
}

while getopts "h:p:t:i:" opt
do
   case "$opt" in
      h ) host="$OPTARG" ;;
      p ) port="$OPTARG" ;;
      t ) token="$OPTARG" ;;
      i ) postId="$OPTARG" ;;
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

url=http://$host:$port/removePost?$opt
curl ${url}