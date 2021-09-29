#!/bin/bash

#var to store output file name
outputFile="data/curl/req/user/user.sh"

#clear output file before writing
echo "#!/bin/bash" > $outputFile
echo "### Tags" >> $outputFile

#token
token="2a874271-f5e5-451b-bb9f-fe6a281414bf"

# firstName
fn[1]="Tom"
fn[2]="Ann"
fn[3]="Lyuba"
fn[4]="Max"
fn[5]="Alan"
fn[6]="Alex"

# lastName
ln[1]="Marston"
ln[2]="Watson"
ln[3]="Pornova"
ln[4]="Collins"
ln[5]="Estrada"
ln[6]="Richardson"

# Login
login[1]="tom1"
login[2]="ann_watson"
login[3]="lyupo"
login[4]="maX"
login[5]="alXM"
login[6]="alex#rich"

# Password
pass[1]="ggkjh6723"
pass[2]="sdsdsdsds"
pass[3]="17772FGH"
pass[4]="ABCD"
pass[5]="&kopk()hk"
pass[6]="jkjksssjlYvgf"

# photoPath
photoPath="/home/lyupo/Descargas/dog.png"

# getUsers
echo "## getUsers" >> $outputFile
echo "curl \"http://localhost:3000/getUsers?token=$token\"" >> $outputFile

# createUser
echo "## createUser" >> $outputFile
for i in 1 2 3 4 5 6
do
  echo "curl \"http://localhost:3000/createUser?first_name=${fn[$i]}&last_name=${ln[$i]}&login=${login[$i]}&password=${pass[$i]}\"" >> $outputFile
done

# getUsers
echo "## getUsers" >> $outputFile
echo "curl \"http://localhost:3000/getUsers?token=$token\"" >> $outputFile

# setUserPhoto
echo "## gsetUserPhoto" >> $outputFile
echo "curl \"http://localhost:3000/setUserPhoto?path=$photoPath&token=$token\"" >> $outputFile

# removeUser
echo "## createUser" >> $outputFile
for i in 5 6 7 8 9 10
do
  echo "curl \"http://localhost:3000/removeUser?id=$i&&token=$token\"" >> $outputFile
done