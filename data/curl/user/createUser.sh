#!/bin/bash

helpFunction()
{
   echo ""
   echo "Usage: $0 -h host -p port -f first_name -l last_name -k password -n login"
   echo -e "\t-h Host name"
   echo -e "\t-p Port number"
   echo -e "\t-f User's first name"
   echo -e "\t-l User's last name"
   echo -e "\t-k User's password"
   echo -e "\t-n User's login"
   exit 1 # Exit script after printing help
}

while getopts "h:p:f:l:k:n:" opt
do
   case "$opt" in
      h ) host="$OPTARG" ;;
      p ) port="$OPTARG" ;;
      f ) fn="$OPTARG" ;;
      l ) ln="$OPTARG" ;;
      k ) pass="$OPTARG" ;;
      n ) login="$OPTARG" ;;
      ? ) helpFunction ;; # Print helpFunction in case parameter is non-existent
   esac
done

opt="first_name=$fn&last_name=$ln&login=$login&password=$pass"
url=http://$host:$port/createUser?$opt
curl ${url}