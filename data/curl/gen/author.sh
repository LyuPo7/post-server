#!/bin/bash

#var to store output file name
outputFile="data/curl/req/author/author.sh"

#clear output file before writing
echo "#!/bin/bash" > $outputFile
echo "### Authors" >> $outputFile

#token
token="2a874271-f5e5-451b-bb9f-fe6a281414bf"

# description
desc[1]="'New%20author'"
desc[2]="'Theu%20best'"
desc[3]="'Sport%20articles%20and%20books'"

# getAuthors
echo "## getAuthors" >> $outputFile
echo "curl \"http://localhost:3000/getAuthors?token=$token\"" >> $outputFile

# createAuthor
echo "## createAuthor" >> $outputFile
for i in 1 2 3 4 5 6 7 8 9 10 11 12
do
  echo "curl \"http://localhost:3000/createAuthor?id=$i&description=${desc[$i]}&id=$i&token=$token\"" >> $outputFile
done

# getAuthors
echo "## getAuthors" >> $outputFile
echo "curl \"http://localhost:3000/getAuthors?token=$token\"" >> $outputFile

# editAuthor
echo "## editAuthor" >> $outputFile
for i in 6 7 8 9 10 11 12
do
  echo "curl \"http://localhost:3000/editAuthor?description=${desc[3]}&id=$i&token=$token\"" >> $outputFile
done

# getAuthors
echo "## getAuthors" >> $outputFile
echo "curl \"http://localhost:3000/getAuthors?token=$token\"" >> $outputFile

# removeAuthor
echo "## removeAuthor" >> $outputFile
for i in 6 7 8 9 10 11 12
do
  echo "curl \"http://localhost:3000/removeAuthor?id=$i&token=$token\"" >> $outputFile
done