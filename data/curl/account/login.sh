#!/bin/bash

helpFunction()
{
   echo ""
   echo "Usage: $0 -y host -p port -k password -n login"
   echo -e "\t-y Host name"
   echo -e "\t-p Port number"
   echo -e "\t-k User's password"
   echo -e "\t-n User's login"
   exit 1 # Exit script after printing help
}

while getopts "y:p:k:n:" opt
do
   case "$opt" in
      y ) host="$OPTARG" ;;
      p ) port="$OPTARG" ;;
      k ) pass="$OPTARG" ;;
      n ) login="$OPTARG" ;;
      ? ) helpFunction ;; # Print helpFunction in case parameter is non-existent
   esac
done

opt="login=$login&password=$pass"
url=http://$host:$port/login?$opt
curl ${url}