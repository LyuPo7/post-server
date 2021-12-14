# post-server
[![Haskell CI](https://github.com/LyuPo7/post-server/actions/workflows/haskell.yml/badge.svg)](https://github.com/LyuPo7/post-server/actions/workflows/haskell.yml)
<p>The REST-API is an HTTP-based interface. The response contains a JSON object</p>

<h3>Project Description:</h3>
    <ul>
        <li>Using <b>Warp</b> as Web-server;</li>
        <li>Using <b>PostgreSQL</b> as BD;</li>
        <li>The response contains a <b>JSON</b> object;</li>
        <li>Supports 4 levels of logging:
            <ul>
                <li><b>error</b>-level(default): show only error messages;</li>
                <li><b>warning</b>-level: show only error and warning messages;</li>
                <li><b>info</b>-level: show info, error and warning messages;</li>
                <li><b>debug</b>-level: show even more details than info-level;</li>
            </ul>
        </li>
    </ul>

<h3>Installation:</h3>
    <p>You may clone GitHub repository</p>
        <p><b>git clone https://github.com/LyuPo7/post-server.git</b></p>

<h3>Create BD:</h3>
    <p>Project uses <b>PostgreSQL</b> as BD. Before use server you'll need to setup BD;</p>
    <ol>
        <li>Install PostgreSQL BD (https://www.postgresqltutorial.com/install-postgresql-linux/)</li>
        <li>Create PostgreSQL BD user:</li>
            <ul>
                <li><b>sudo su - postgres</b> # create super user postgres;</li>
                <li><b>psql</b> #connect the database;</li>
                <li><b>CREATE USER 'user' CREATEDB;</b> #create user without password (option CREATEDB enable to create dbbase); </li>
            </ul>
        <li>Create PostgreSQL BD:</li>
            <ul>
                <li><b>su 'user'</b> # switch to user account (for which was created PostgreSQL user in previous step);</li>
                <li><b>createdb 'dbName'</b> #connect the database;</li>
            </ul>
        <li>Using PostgreSQL BD in data/config.json:</li>
            <ul>
                <li>Use <b>'user'</b> for field <b>db_settings.user</b>;</li>
                <li>Use <b>'dbName'</b> for field <b>db_settings.dbName</b>;</li>
            </ul>
    </ol>
<h3>How to use?</h3>
    <ol>
        <li> <h4>Setup 'data/config.json'</h4> (for example see config-json-file: 'data/config_example.json')
            <ul>
                 <li><b>"logger_settings"</b></li>
                    <ul>
                        <li><b>"verbocity":</b> [Optional]-[String] level of logging - must be one of ["debug", "info", "warning", "error"]
                        By default will be use "info" level;</li>
                    </ul>
             </ul>
             <ul>
                 <li><b>"db_settings"</b></li>
                    <ul>
                        <li><b>"dbname":</b> [Required]-[String] dbname of PostgreSQL DB using for Server;</li>
                        <li><b>"user":</b> [Optional]-[String] user-owner of PostgreSQL DB using for Server;</li>
                        <li><b>"admins":</b> [Optional]-[Array of String] List of admins;</li>
                    </ul>
             </ul>
             <ul>
                 <li><b>"server_settings"</b></li>
                    <ul>
                        <li><b>"host":</b> [Required]-[String] Server host;</li>
                        <li><b>"port":</b> [Optional]-[String] Server port;</li>
                    </ul>
             </ul>
       </li>
       <li><h4>Since bot use PostgreSQL You'll need check if you have installed PostgreSQL in your system (if not - use):</h4>
           <ul><b>$ sudo apt-get update -y && sudo apt-get install libpq-dev postgresql -y</b>
           </ul>
       </li>
       <li><h4>Build project using <b>stack</b>:</h4>
           <ul><b>$ stack build</b>
           </ul>
       </li>
       <li><h4>Run project using <b>stack</b>:</h4>
           <ul><b>$ stack exec post-server-exe</b>
           </ul>
               <p>For more information see asciinema below:
                  <a href="https://asciinema.org/a/443725" target="_blank"><img src="https://asciinema.org/a/443725.svg" /></a></b>
               </p>
           </ul>
       </li>
    </ol>
    <p>
<h3>Making requests</h3>
<ul>
        <li>All supported requests contain in <i>'data/curl/'</i></li>
            <ul>
                <li>Every directory contains executables <i>.sh</i> scripts for supported requests;</li>
                <li>For unicode symbols in requests using <i>.sh</i> scripts use folowing:
                    <ul>
                        <li>Whitespace: <b>%20</b></li>
                        <li>Left square parenthesis "[": <b>%5B</b></li>
                        <li>Right square parenthesis "[": <b>%5D</b></li>
                    </ul>
                </li>
            </ul>
        <li>Supported requests:</li>
            <ul>
                <li><b>Account</b>:
                    <ul>
                        <li><b>login</b>:
                            <ul>
                                <li>Use this method to get new <b>Token</b> for <b>User</b> by login;</li>
                                <li><b>Request: http://HOST:PORT/publishDraft?PARAMETERS</b></li>
                                     <table>
                                            <tr>
                                                <th><b>Parameter</b></th>
                                                <th><b>Type</b></th>
                                                <th><b>Required</b></th>
                                                <th><b>Description</b></th>
                                            </tr>
                                            <tr>
                                                <td>password</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>User's</b> password</td>
                                            </tr>
                                            <tr>
                                                <td>login</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>User's</b> login</td>
                                            </tr>
                                    </table> 
                                <li><b>Curl:</b>
                                    <ul>
                                        <li><b>Script location: <i>'data/curl/account/login.sh'</i></b></li>
                                        <li><b>Usage (from <i>'data/curl/account/'</i>): <i>'$ ./login.sh [flags]'</i></b>
                                            <ul> 
                                                <li><b>-h</b> Print help message and exit;</li>
                                                <li><b>-y</b> Host server name;</li>
                                                <li><b>-p</b> Port server number;</li>
                                                <li><b>-k</b> User password;</li>
                                                <li><b>-k</b> User login;</li>
                                            </ul>
                                        </li>
                                    </ul>
                                </li>
                                <li><b>Response</b>:
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object;</li>
                                        <li><b>Fail</b>: JSON <b>TextResponse</b> object with text error;</li>
                                    </ul>
                                </li>
                            </ul>
                        </li>
                   </ul>
                </li>
                <li><b>User</b>:
                    <ul>
                        <li><b>getUsers</b>:
                            <ul>
                                <li>Use this method to get all <b>User</b> records;</li>
                                <li>This method is available for all registered <b>Users</b>;</li>
                                <li>In one request You may recieve maximum 50 records;</li>
                                <li>For recieve more than 50 records You'll need to use several requests with different offset;</li>
                                <li><b>Request: http://HOST:PORT/getUsers?PARAMETERS</b></li>
                                     <table>
                                            <tr>
                                                <th><b>Parameter</b></th>
                                                <th><b>Type</b></th>
                                                <th><b>Required</b></th>
                                                <th><b>Description</b></th>
                                            </tr>
                                            <tr>
                                                <td>token</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>User's</b> token</td>
                                            </tr>
                                            <tr>
                                                <td>offset</td>
                                                <td>Integer</td>
                                                <td>Yes</td>
                                                <td>Offset from first <b>User's</b> record (ordered by <b>User's</b> id). Must be >=0</td>
                                            </tr>
                                    </table> 
                                <li><b>Curl:</b>
                                    <ul>
                                        <li><b>Script location: <i>'data/curl/user/getUsers.sh'</i></b></li>
                                        <li><b>Usage (from <i>'data/curl/user/'</i>): '$ ./getUsers.sh [flags]'</b>
                                            <ul> 
                                                <li><b>-h</b> Print help message and exit;</li>
                                                <li><b>-y</b> Host server name;</li>
                                                <li><b>-p</b> Port server number;</li>
                                                <li><b>-t</b> <b>User's</b> token;</li>
                                                <li><b>-o</b> Offset from the first record (ordered by <b>User's</b> id);</li>
                                            </ul>
                                        </li>
                                    </ul>
                                </li>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>UserResponse</b> object;</li>
                                        <li><b>Fail</b>: JSON <b>TextResponse</b> object with text error;</li>
                                    </ul>
                                </li>
                            </ul>
                        </li>
                        <li><b>createUser</b>:
                            <ul>
                                <li>Use this method to register new <b>User</b>;</li>
                                <li>This method is available for all;</li>
                                <li><b>Request: http://HOST:PORT/createUser?PARAMETERS</b></li>
                                     <table>
                                            <tr>
                                                <th><b>Parameter</b></th>
                                                <th><b>Type</b></th>
                                                <th><b>Required</b></th>
                                                <th><b>Description</b></th>
                                            </tr>
                                            <tr>
                                                <td>first_name</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>User's</b> first name</td>
                                            </tr>
                                            <tr>
                                                <td>last_name</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>User's</b> last name</td>
                                            </tr>
                                            <tr>
                                                <td>login</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>User's</b> login</td>
                                            </tr>
                                            <tr>
                                                <td>password</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>User's</b> password</td>
                                            </tr>
                                    </table> 
                                <li><b>Curl:</b>
                                    <ul>
                                        <li><b>Script location: <i>'data/curl/user/createUser.sh'</i></b></li>
                                        <li><b>Usage (from <i>'data/curl/user/'</i>): '$ ./createUser.sh [flags]'</b>
                                            <ul> 
                                                <li><b>-h</b> Print help message and exit;</li>
                                                <li><b>-y</b> Host server name;</li>
                                                <li><b>-p</b> Port server number;</li>
                                                <li><b>-f</b> <b>User's</b> first name;</li>
                                                <li><b>-l</b> <b>User's</b> last name;</li>
                                                <li><b>-k</b> <b>User's</b> password;</li>
                                                <li><b>-n</b> <b>User's</b> login;</li>
                                            </ul>
                                        </li>
                                    </ul>
                                </li>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with text success;</li>
                                        <li><b>Fail</b>: JSON <b>TextResponse</b> object with text error;</li>
                                    </ul>
                                </li>
                            </ul>
                        </li>
                        <li><b>setUserPhoto</b>:
                            <ul>
                                <li>Use this method to set <b>Photo</b> for <b>User</b>;</li>
                                <li>This method is available only for <b>User</b>-account owner;</li>
                                <li><b>Request: http://HOST:PORT/setUserPhoto?PARAMETERS</b></li>
                                     <table>
                                            <tr>
                                                <th><b>Parameter</b></th>
                                                <th><b>Type</b></th>
                                                <th><b>Required</b></th>
                                                <th><b>Description</b></th>
                                            </tr>
                                            <tr>
                                                <td>token</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>User's</b> token. For successful request account and token must belong to the same <b>User</b></td>
                                            </tr>
                                            <tr>
                                                <td>path</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td>Local path to photo</td>
                                            </tr>
                                    </table> 
                                <li><b>Curl:</b>
                                    <ul>
                                        <li><b>Script location: <i>'data/curl/user/setUserPhoto.sh'</i></b></li>
                                        <li><b>Usage (from <i>'data/curl/user/'</i>): '$ ./setUserPhoto.sh [flags]'</b>
                                            <ul> 
                                                <li><b>-h</b> Print help message and exit;</li>
                                                <li><b>-y</b> Host server name;</li>
                                                <li><b>-p</b> Port server number;</li>
                                                <li><b>-t</b> <b>User's</b> token;</li>
                                                <li><b>-l</b> Path to photo;</li>
                                            </ul>
                                        </li>
                                    </ul>
                                </li>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with text success;</li>
                                        <li><b>Fail</b>:
                                             <ul> 
                                                 <li><b>If <b>User's</b> token includes required permissions:</b> JSON <b>TextResponse</b> object with text success;</li>
                                                 <li><b>If <b>User's</b> token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with text error;</li>
                                             </ul>
                                        </li>
                                    </ul>
                                </li>
                            </ul>
                        </li>
                        <li><b>removeUser</b>:
                            <ul>
                                <li>Use this method to remove <b>User</b>;</li>
                                <li>This method is available only for admins;</li>
                                <li><b>Request: http://HOST:PORT/removeUser?PARAMETERS</b></li>
                                     <table>
                                            <tr>
                                                <th><b>Parameter</b></th>
                                                <th><b>Type</b></th>
                                                <th><b>Required</b></th>
                                                <th><b>Description</b></th>
                                            </tr>
                                            <tr>
                                                <td>token</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>User's</b> token. Token of <b>User</b> making request. For successful request <b>User</b> must have Admin Permissions</td>
                                            </tr>
                                            <tr>
                                                <td>id</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>User's</b> id (<b>User's</b> to remove)</td>
                                            </tr>
                                    </table> 
                                <li><b>Curl:</b>
                                    <ul>
                                        <li><b>Script location: <i>'data/curl/user/removeUser.sh'</i></b></li>
                                        <li><b>Usage (from <i>'data/curl/user/'</i>): '$ ./removeUser.sh [flags]'</b>
                                            <ul> 
                                                <li><b>-h</b> Print help message and exit;</li>
                                                <li><b>-y</b> Host server name;</li>
                                                <li><b>-p</b> Port server number;</li>
                                                <li><b>-t</b> <b>User's</b> token. Token of <b>User</b> making request. For successful request <b>User</b> must have Admin Permissions;</li>
                                                <li><b>-i</b> <b>User's</b> id (<b>User's</b> to remove)</li>
                                            </ul>
                                        </li>
                                    </ul>
                                </li>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with text success;</li>
                                        <li><b>Fail</b>:
                                             <ul> 
                                                 <li><b>If <b>User's</b> token includes required permissions:</b> JSON <b>TextResponse</b> object with text success;</li>
                                                 <li><b>If <b>User's</b> token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with text error;</li>
                                             </ul>
                                        </li>
                                    </ul>
                                </li>
                            </ul>
                        </li>
                   </ul>
                </li>
                <li><b>Author</b>:
                    <ul>
                        <li><b>getAuthors</b>:
                            <ul>
                                <li>Use this method to get all <b>Author</b> records;</li>
                                <li>This method is available only for admins;</li>
                                <li>In one request You may recieve maximum 50 records;</li>
                                <li>For recieve more than 50 records You'll need to use several requests with different offset;</li>
                                <li><b>Request: http://HOST:PORT/getAuthors?PARAMETERS</b></li>
                                     <table>
                                            <tr>
                                                <th><b>Parameter</b></th>
                                                <th><b>Type</b></th>
                                                <th><b>Required</b></th>
                                                <th><b>Description</b></th>
                                            </tr>
                                            <tr>
                                                <td>token</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>User's</b> token. Token of <b>User</b> making request. For successful request <b>User</b> must have Admin Permissions</td>
                                            </tr>
                                            <tr>
                                                <td>offset</td>
                                                <td>Integer</td>
                                                <td>Yes</td>
                                                <td>Offset from first <b>Author's</b> record (ordered by <b>Author's</b> id). Must be >=0</td>
                                            </tr>
                                    </table> 
                                <li><b>Curl:</b>
                                    <ul>
                                        <li><b>Script location: <i>'data/curl/author/getAuthors.sh'</i></b></li>
                                        <li><b>Usage (from <i>'data/curl/author/'</i>): '$ ./getAuthors.sh [flags]'</b>
                                            <ul> 
                                                <li><b>-h</b> Print help message and exit;</li>
                                                <li><b>-y</b> Host server name;</li>
                                                <li><b>-p</b> Port server number;</li>
                                                <li><b>-t</b> <b>User's</b> token. Token of <b>User</b> making request. For successful request <b>User</b> must have Admin Permissions;</li>
                                                <li><b>-o</b> Offset from first <b>Author's</b> record (ordered by <b>Author's</b> id);</li>
                                            </ul>
                                        </li>
                                    </ul>
                                </li>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>AuthorResponse</b> object;</li>
                                        <li><b>Fail</b>:
                                            <ul> 
                                                 <li><b>If <b>User's</b> token includes required permissions:</b> JSON <b>TextResponse</b> object with text success;</li>
                                                 <li><b>If <b>User's</b> token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with text error;</li>
                                             </ul>
                                        </li>
                                    </ul>
                                </li>
                            </ul>
                        </li>
                        <li><b>createAuthor</b>:
                            <ul>
                                <li>Use this method to create new <b>Author</b> of already existed <b>User</b>;</li>
                                <li>This method is available only for admins;</li>
                                <li><b>Request: http://HOST:PORT/createAuthor?PARAMETERS</b></li>
                                     <table>
                                            <tr>
                                                <th><b>Parameter</b></th>
                                                <th><b>Type</b></th>
                                                <th><b>Required</b></th>
                                                <th><b>Description</b></th>
                                            </tr>
                                            <tr>
                                                <td>token</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>User's</b> token. Token of <b>User</b> making request. For successful request <b>User</b> must have Admin Permissions</td>
                                            </tr>
                                            <tr>
                                                <td>id</td>
                                                <td>Integer</td>
                                                <td>Yes</td>
                                                <td><b>User's</b> id (for <b>Author</b> creation)</td>
                                            </tr>
                                            <tr>
                                                <td>description</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>Author's</b> description</td>
                                            </tr>
                                    </table> 
                                <li><b>Curl:</b>
                                    <ul>
                                        <li><b>Script location: <i>'data/curl/author/createAuthor.sh'</i></b></li>
                                        <li><b>Usage (from <i>'data/curl/author/'</i>): '$ ./createAuthor.sh [flags]'</b>
                                            <ul> 
                                                <li><b>-h</b> Print help message and exit;</li>
                                                <li><b>-y</b> Host server name;</li>
                                                <li><b>-p</b> Port server number;</li>
                                                <li><b>-t</b> <b>User's</b> token. Token of <b>User</b> making request. For successful request <b>User</b> must have Admin Permissions;</li>
                                                <li><b>-i</b> <b>User's</b> id (for <b>Author</b> creation);</li>
                                                <li><b>-d</b> <b>Author's</b> description;</li>
                                            </ul>
                                        </li>
                                    </ul>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with text success;</li>
                                        <li><b>Fail</b>:
                                            <ul> 
                                                 <li><b>If <b>User's</b> token includes required permissions:</b> JSON <b>TextResponse</b> object with text success;</li>
                                                 <li><b>If <b>User's</b> token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with text error;</li>
                                             </ul>
                                        </li>
                                    </ul>
                                </li>
                            </ul>
                        </li>
                        <li><b>editAuthor</b>:
                            <ul>
                                <li>Use this method to edit <b>Author's</b> description;</li>
                                <li>This method is available only for admins;</li>
                                <li><b>Request: http://HOST:PORT/editAuthor?PARAMETERS</b></li>
                                     <table>
                                            <tr>
                                                <th><b>Parameter</b></th>
                                                <th><b>Type</b></th>
                                                <th><b>Required</b></th>
                                                <th><b>Description</b></th>
                                            </tr>
                                            <tr>
                                                <td>token</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>User's</b> token. Token of <b>User</b> making request. For successful request <b>User</b> must have Admin Permissions</td>
                                            </tr>
                                            <tr>
                                                <td>id</td>
                                                <td>Integer</td>
                                                <td>Yes</td>
                                                <td><b>User's</b> id (corresponding to proper <b>Author</b>)</td>
                                            </tr>
                                            <tr>
                                                <td>description</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>New Author's</b> description</td>
                                            </tr>
                                    </table> 
                                <li><b>Curl:</b>
                                    <ul>
                                        <li><b>Script location: <i>'data/curl/author/editAuthor.sh'</i></b></li>
                                        <li><b>Usage (from <i>'data/curl/author/'</i>): '$ ./editAuthor.sh [flags]'</b>
                                            <ul> 
                                                <li><b>-h</b> Print help message and exit;</li>
                                                <li><b>-y</b> Host server name;</li>
                                                <li><b>-t</b> <b>User's</b> token. Token of <b>User</b> making request. For successful request <b>User</b> must have Admin Permissions;</li>
                                                <li><b>-i</b> <b>User's</b> id (corresponding to proper <b>Author</b>);</li>
                                                <li><b>-d</b> New <b>Author's</b> description;</li>
                                            </ul>
                                        </li>
                                    </ul>
                                </li>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with text success;</li>
                                        <li><b>Fail</b>:
                                             <ul> 
                                                 <li><b>If <b>User's</b> token includes required permissions:</b> JSON <b>TextResponse</b> object with text success;</li>
                                                 <li><b>If <b>User's</b> token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with text error;</li>
                                             </ul>
                                        </li>
                                    </ul>
                                </li>
                            </ul>
                        </li>
                        <li><b>removeAuthor</b>:
                            <ul>
                                <li>Use this method to remove <b>Author</b>. If <b>Author</b> has <b>Posts</b> it's imposible to remove his record. Firstly You'll need to remove all his <b>Posts</b>;</li>
                                <li>This method is available only for admins;</li>
                                <li><b>Request: http://HOST:PORT/removeAuthor?PARAMETERS</b></li>
                                     <table>
                                            <tr>
                                                <th><b>Parameter</b></th>
                                                <th><b>Type</b></th>
                                                <th><b>Required</b></th>
                                                <th><b>Description</b></th>
                                            </tr>
                                            <tr>
                                                <td>token</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>User's</b> token. Token of <b>User</b> making request. For successful request <b>User</b> must have Admin Permissions</td>
                                            </tr>
                                            <tr>
                                                <td>id</td>
                                                <td>Integer</td>
                                                <td>Yes</td>
                                                <td><b>User's</b> id (corresponding to proper <b>Author</b>)</td>
                                            </tr>
                                    </table> 
                                <li><b>Curl:</b>
                                    <ul>
                                        <li><b>Script location: <i>'data/curl/authors/removeAuthor.sh'</i></b></li>
                                        <li><b>Usage: '$ ./removeAuthor.sh [flags]'</b>
                                            <ul> 
                                                <li><b>-h</b> Print help message and exit;</li>
                                                <li><b>-y</b> Host server name;</li>
                                                <li><b>-t</b> <b>User's</b> token. Token of <b>User</b> making request. For successful request <b>User</b> must have Admin Permissions;</li>
                                                <li><b>-i</b> <b>User's</b> id (corresponding to proper Author);</li>
                                            </ul>
                                        </li>
                                    </ul>
                                </li>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with text success;</li>
                                        <li><b>Fail</b>:
                                             <ul> 
                                                 <li><b>If <b>User's</b> token includes required permissions:</b> JSON <b>TextResponse</b> object with text success;</li>
                                                 <li><b>If <b>User's</b> token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with text error;</li>
                                             </ul>
                                        </li>
                                    </ul>
                                </li>
                            </ul>
                        </li>
                   </ul>
                </li>
                <li><b>Category</b>:
                    <ul>
                        <li><b>getCategories</b>:
                            <ul>
                                <li>Use this method to get all <b>Category</b> records;</li>
                                <li>This method is available for all registered <b>Users</b>;</li>
                                <li>In one request You may recieve maximum 50 records;</li>
                                <li>For recieve more than 50 records You'll need to use several requests with different offset;</li>
                                <li><b>Request: http://HOST:PORT/getCategories?PARAMETERS</b></li>
                                     <table>
                                            <tr>
                                                <th><b>Parameter</b></th>
                                                <th><b>Type</b></th>
                                                <th><b>Required</b></th>
                                                <th><b>Description</b></th>
                                            </tr>
                                            <tr>
                                                <td>token</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>User's</b> token. Token of <b>User</b> making request</td>
                                            </tr>
                                            <tr>
                                                <td>offset</td>
                                                <td>Integer</td>
                                                <td>Yes</td>
                                                <td>Offset from first <b>Category's</b> record (ordered by <b>Category's</b> id). Must be >=0</td>
                                            </tr>
                                    </table> 
                                <li><b>Curl:</b>
                                    <ul>
                                        <li><b>Script location: <i>'data/curl/category/getCategories.sh'</i></b></li>
                                        <li><b>Usage (from <i>'data/curl/category/'</i>): '$ ./getCategories.sh [flags]'</b>
                                            <ul> 
                                                <li><b>-h</b> Print help message and exit;</li>
                                                <li><b>-y</b> Host server name;</li>
                                                <li><b>-p</b> Port server number;</li>
                                                <li><b>-t</b> <b>User's</b> token. Token of <b>User</b> making request;</li>
                                                <li><b>-o</b> Offset from first <b>Category's</b> record (ordered by <b>Category's</b> id);</li>
                                            </ul>
                                        </li>
                                    </ul>
                                </li>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>CategoryResponse</b> object;</li>
                                        <li><b>Fail</b>:
                                            <ul> 
                                                 <li><b>If <b>User's</b> token includes required permissions:</b> JSON <b>TextResponse</b> object with text success;</li>
                                                 <li><b>If <b>User's</b> token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with text error;</li>
                                             </ul>
                                        </li>
                                    </ul>
                                </li>
                            </ul>
                        </li>
                        <li><b>createCategory</b>:
                            <ul>
                                <li>Use this method to create new <b>Category</b>. <b>Category's</b> title and <b>SubCategory's</b> title can't be the same;</li>
                                <li>This method is available only for admins;</li>
                                <li><b>Request: http://HOST:PORT/createCategory?PARAMETERS</b></li>
                                     <table>
                                            <tr>
                                                <th><b>Parameter</b></th>
                                                <th><b>Type</b></th>
                                                <th><b>Required</b></th>
                                                <th><b>Description</b></th>
                                            </tr>
                                            <tr>
                                                <td>token</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>User's</b> token. Token of <b>User</b> making request. For successful request <b>User</b> must have Admin Permissions</td>
                                            </tr>
                                            <tr>
                                                <td>title</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>Category's</b> title</td>
                                            </tr>
                                            <tr>
                                                <td>subcategory</td>
                                                <td>String</td>
                                                <td>Optional</td>
                                                <td><b>SubCategory's</b> title (if <b>Category</b> expects to have one)</td>
                                            </tr>
                                    </table> 
                                <li><b>Curl:</b>
                                    <ul>
                                        <li><b>Script location: <i>'data/curl/category/createCategory.sh'</i></b></li>
                                        <li><b>Usage (from <i>'data/curl/category/'</i>): '$ ./createCategory.sh [flags]'</b>
                                            <ul> 
                                                <li><b>-h</b> Print help message and exit;</li>
                                                <li><b>-y</b> Host server name;</li>
                                                <li><b>-p</b> Port server number;</li>
                                                <li><b>-t</b> <b>User's</b> token. Token of <b>User</b> making request. For successful request <b>User</b> must have Admin Permissions;</li>
                                                <li><b>-i</b> <b>Category's</b> title;</li>
                                                <li><b>[-s]</b> <b>SubCategory's</b> title (if <b>Category</b> expects to have one);</li>
                                            </ul>
                                        </li>
                                     </ul>
                                </li>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with text success;</li>
                                        <li><b>Fail</b>:
                                            <ul> 
                                                 <li><b>If <b>User's</b> token includes required permissions:</b> JSON <b>TextResponse</b> object with text success;</li>
                                                 <li><b>If <b>User's</b> token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with text error;</li>
                                             </ul>
                                        </li>
                                    </ul>
                                </li>
                            </ul>
                        </li>
                        <li><b>editCategory</b>:
                            <ul>
                                <li>Use this method to edit <b>Category's</b> title, <b>Category's</b> subCategory or both. <b>Category's</b> title and <b>SubCategory's</b> title can't be the same. The <b>SubCategory's</b> title using for request already must exists. You can't use for <b>Category</b> title, title what already exists;</li>
                                <li>This method is available only for admins;</li>
                                <li><b>Request: http://HOST:PORT/editCategory?PARAMETERS</b></li>
                                     <table>
                                            <tr>
                                                <th><b>Parameter</b></th>
                                                <th><b>Type</b></th>
                                                <th><b>Required</b></th>
                                                <th><b>Description</b></th>
                                            </tr>
                                            <tr>
                                                <td>token</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>User's</b> token. Token of <b>User</b> making request. For successful request <b>User</b> must have Admin Permissions</td>
                                            </tr>
                                            <tr>
                                                <td>id</td>
                                                <td>Integer</td>
                                                <td>Yes</td>
                                                <td><b>Category's</b> id</td>
                                            </tr>
                                            <tr>
                                                <td>title</td>
                                                <td>String</td>
                                                <td>Optional</td>
                                                <td>New <b>Category's</b> title (if You want to change <b>Category's</b> title)</td>
                                            </tr>
                                            <tr>
                                                <td>subcategory</td>
                                                <td>String</td>
                                                <td>Optional</td>
                                                <td><b>SubCategory's</b> title (if You want to change/set SubCategory of <b>Category</b>)</td>
                                            </tr>
                                    </table> 
                                <li><b>Curl:</b>
                                    <ul>
                                        <li><b>Script location: <i>'data/curl/category/editCategory.sh'</i></b></li>
                                        <li><b>Usage (from <i>'data/curl/category/'</i>): '$ ./editCategory.sh [flags]'</b>
                                            <ul> 
                                                <li><b>-h</b> Print help message and exit;</li>
                                                <li><b>-y</b> Host server name;</li>
                                                <li><b>-t</b> <b>User's</b> token. Token of <b>User</b> making request. For successful request <b>User</b> must have Admin Permissions;</li>
                                                <li><b>-i</b> <b>Category's</b> id;</li>
                                                <li><b>[-s]</b> <b>SubCategory's</b> title (if You want to change/set SubCategory of <b>Category</b>);</li>
                                                <li><b>[-n]</b> New <b>Category's</b> title (if You want to change <b>Category's</b> title);</li>
                                            </ul>
                                        </li>
                                    </ul>
                                </li>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with text success;</li>
                                        <li><b>Fail</b>:
                                             <ul> 
                                                 <li><b>If <b>User's</b> token includes required permissions:</b> JSON <b>TextResponse</b> object with text success;</li>
                                                 <li><b>If <b>User's</b> token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with text error;</li>
                                             </ul>
                                        </li>
                                    </ul>
                                </li>
                            </ul>
                        </li>
                        <li><b>removeCategory</b>:
                            <ul>
                                <li>Use this method to remove <b>Category</b>. If <b>Category</b> is used for <b>Posts</b> it's imposible to remove his record. Firstly You'll need to remove all <b>Posts</b> which included thic <b>Category</b>;</li>
                                <li>This method is available only for admins;</li>
                                <li><b>Request: http://HOST:PORT/removeCategory?PARAMETERS</b></li>
                                     <table>
                                            <tr>
                                                <th><b>Parameter</b></th>
                                                <th><b>Type</b></th>
                                                <th><b>Required</b></th>
                                                <th><b>Description</b></th>
                                            </tr>
                                            <tr>
                                                <td>token</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>User's</b> token. Token of <b>User</b> making request. For successful request <b>User</b> must have Admin Permissions</td>
                                            </tr>
                                            <tr>
                                                <td>id</td>
                                                <td>Integer</td>
                                                <td>Yes</td>
                                                <td><b>Category's</b> id</td>
                                            </tr>
                                    </table> 
                                <li><b>Curl:</b>
                                    <ul>
                                        <li><b>Script location: <i>'data/curl/category/removeCategory.sh'</i></b></li>
                                        <li><b>Usage (from <i>'data/curl/category/'</i>): '$ ./removeCategory.sh [flags]'</b>
                                            <ul> 
                                                <li><b>-h</b> Print help message and exit;</li>
                                                <li><b>-y</b> Host server name;</li>
                                                <li><b>-t</b> <b>User's</b> token. Token of <b>User</b> making request. For successful request <b>User</b> must have Admin Permissions;</li>
                                                <li><b>-i</b> <b>Category's</b> id;</li>
                                            </ul>
                                        </li>
                                    </ul>
                                </li>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with text success;</li>
                                        <li><b>Fail</b>:
                                             <ul> 
                                                 <li><b>If <b>User's</b> token includes required permissions:</b> JSON <b>TextResponse</b> object with text success;</li>
                                                 <li><b>If <b>User's</b> token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with text error;</li>
                                             </ul>
                                        </li>
                                    </ul>
                                </li>
                            </ul>
                        </li>
                   </ul>
                </li>
                <li><b>Tag</b>:
                    <ul>
                        <li><b>getTags</b>:
                            <ul>
                                <li>Use this method to get all <b>Tag</b> records;</li>
                                <li>This method is available for all registered <b>Users</b>;</li>
                                <li>In one request You may recieve maximum 50 records;</li>
                                <li>For recieve more than 50 records You'll need to use several requests with different offset;</li>
                                <li><b>Request: http://HOST:PORT/getTags?PARAMETERS</b></li>
                                     <table>
                                            <tr>
                                                <th><b>Parameter</b></th>
                                                <th><b>Type</b></th>
                                                <th><b>Required</b></th>
                                                <th><b>Description</b></th>
                                            </tr>
                                            <tr>
                                                <td>token</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>User's</b> token. Token of <b>User</b> making request</td>
                                            </tr>
                                            <tr>
                                                <td>offset</td>
                                                <td>Integer</td>
                                                <td>Yes</td>
                                                <td>Offset from first <b>Tag's</b> record (ordered by <b>Tag's</b> id). Must be >=0</td>
                                            </tr>
                                    </table> 
                                <li><b>Curl:</b>
                                    <ul>
                                        <li><b>Script location: <i>'data/curl/tag/getTags.sh'</i></b></li>
                                        <li><b>Usage (from <i>'data/curl/tag/'</i>): '$ ./getTags.sh [flags]'</b>
                                            <ul> 
                                                <li><b>-h</b> Print help message and exit;</li>
                                                <li><b>-y</b> Host server name;</li>
                                                <li><b>-p</b> Port server number;</li>
                                                <li><b>-t</b> <b>User's</b> token. Token of <b>User</b> making request;</li>
                                                <li><b>-o</b> Offset from first <b>Tag's</b> record (ordered by <b>Tag's</b> id);</li>
                                            </ul>
                                        </li>
                                    </ul>
                                </li>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>TagResponse</b> object;</li>
                                        <li><b>Fail</b>:
                                            <ul> 
                                                 <li><b>If <b>User's</b> token includes required permissions:</b> JSON <b>TextResponse</b> object with text success;</li>
                                                 <li><b>If <b>User's</b> token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with text error;</li>
                                             </ul>
                                        </li>
                                    </ul>
                                </li>
                            </ul>
                        </li>
                        <li><b>createTag</b>:
                            <ul>
                                <li>Use this method to create new <b>Tag</b>;</li>
                                <li>This method is available only for admins;</li>
                                <li><b>Request: http://HOST:PORT/createTag?PARAMETERS</b></li>
                                     <table>
                                            <tr>
                                                <th><b>Parameter</b></th>
                                                <th><b>Type</b></th>
                                                <th><b>Required</b></th>
                                                <th><b>Description</b></th>
                                            </tr>
                                            <tr>
                                                <td>token</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>User's</b> token. Token of <b>User</b> making request. For successful request <b>User</b> must have Admin Permissions</td>
                                            </tr>
                                            <tr>
                                                <td>title</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>Tag's</b> title</td>
                                            </tr>
                                    </table> 
                                <li><b>Curl:</b>
                                    <ul>
                                        <li><b>Script location: <i>'data/curl/tag/createTag.sh'</i></b></li>
                                        <li><b>Usage (from <i>'data/curl/tag/'</i>): '$ ./createTag.sh [flags]'</b>
                                            <ul> 
                                                <li><b>-h</b> Print help message and exit;</li>
                                                <li><b>-y</b> Host server name;</li>
                                                <li><b>-p</b> Port server number;</li>
                                                <li><b>-t</b> User's</b> token. Token of <b>User</b> making request. For successful request <b>User</b> must have Admin Permissions;</li>
                                                <li><b>-n</b> <b>Tag's</b> title;</li>
                                            </ul>
                                        </li>
                                    </ul>
                                </li>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with text success;</li>
                                        <li><b>Fail</b>:
                                            <ul> 
                                                 <li><b>If <b>User's</b> token includes required permissions:</b> JSON <b>TextResponse</b> object with text success;</li>
                                                 <li><b>If <b>User's</b> token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with text error;</li>
                                             </ul>
                                        </li>
                                    </ul>
                                </li>
                            </ul>
                        </li>
                        <li><b>editTag</b>:
                            <ul>
                                <li>Use this method to edit <b>Tag's</b> title;</li>
                                <li>This method is available only for admins;</li>
                                <li><b>Request: http://HOST:PORT/editTag?PARAMETERS</b></li>
                                     <table>
                                            <tr>
                                                <th><b>Parameter</b></th>
                                                <th><b>Type</b></th>
                                                <th><b>Required</b></th>
                                                <th><b>Description</b></th>
                                            </tr>
                                            <tr>
                                                <td>token</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>User's</b> token. Token of <b>User</b> making request. For successful request <b>User</b> must have Admin Permissions</td>
                                            </tr>
                                            <tr>
                                                <td>new_title</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td>New <b>Tag's</b> title</td>
                                            </tr>
                                            <tr>
                                                <td>old_title</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td>Old <b>Tag's</b> title</td>
                                            </tr>
                                    </table> 
                                <li><b>Curl:</b>
                                    <ul>
                                        <li><b>Script location: <i>'data/curl/tag/editTag.sh'</i></b></li>
                                        <li><b>Usage (from <i>'data/curl/tag/'</i>): '$ ./editTag.sh [flags]'</b>
                                            <ul> 
                                                <li><b>-h</b> Print help message and exit;</li>
                                                <li><b>-y</b> Host server name;</li>
                                                <li><b>-t</b> <b>User's</b> token. Token of <b>User</b> making request. For successful request <b>User</b> must have Admin Permissions;</li>
                                                <li><b>-n</b> New <b>Tag's</b> title;</li>
                                                <li><b>-o</b> Old <b>Tag's</b> title;</li>
                                            </ul>
                                        </li>
                                    </ul>
                                </li>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with text success;</li>
                                        <li><b>Fail</b>:
                                             <ul> 
                                                 <li><b>If <b>User's</b> token includes required permissions:</b> JSON <b>TextResponse</b> object with text success;</li>
                                                 <li><b>If <b>User's</b> token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with text error;</li>
                                             </ul>
                                        </li>
                                    </ul>
                                </li>
                            </ul>
                        </li>
                        <li><b>removeTag</b>:
                            <ul>
                                <li>Use this method to remove <b>Tag</b>. If <b>Tag</b> is used for <b>Posts</b> it'll be remove from all <b>Posts</b> records;</li>
                                <li>This method is available only for admins;</li>
                                <li><b>Request: http://HOST:PORT/removeTag?PARAMETERS</b></li>
                                     <table>
                                            <tr>
                                                <th><b>Parameter</b></th>
                                                <th><b>Type</b></th>
                                                <th><b>Required</b></th>
                                                <th><b>Description</b></th>
                                            </tr>
                                            <tr>
                                                <td>token</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>User's</b> token. Token of <b>User</b> making request. For successful request <b>User</b> must have Admin Permissions</td>
                                            </tr>
                                            <tr>
                                                <td>title</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>Tag's</b> title</td>
                                            </tr>
                                    </table> 
                                <li><b>Curl:</b>
                                    <ul>
                                        <li><b>Script location: <i>'data/curl/tag/removeTag.sh'</i></b></li>
                                        <li><b>Usage: '$ ./removeTag.sh [flags]'</b>
                                            <ul> 
                                                <li><b>-h</b> Print help message and exit;</li>
                                                <li><b>-y</b> Host server name;</li>
                                                <li><b>-t</b> <b>User's</b> token. Token of <b>User</b> making request. For successful request <b>User</b> must have Admin Permissions</li>
                                                <li><b>-n</b> <b>Tag's</b> title;</li>
                                            </ul>
                                        </li>
                                    </ul>
                                </li>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with text success;</li>
                                        <li><b>Fail</b>:
                                             <ul> 
                                                 <li><b>If <b>User's</b> token includes required permissions:</b> JSON <b>TextResponse</b> object with text success;</li>
                                                 <li><b>If <b>User's</b> token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with text error;</li>
                                             </ul>
                                        </li>
                                    </ul>
                                </li>
                            </ul>
                        </li>
                   </ul>
                </li>
                <li><b>Post</b>:
                    <ul>
                        <li><b>getPosts</b>:
                            <ul>
                                <li>Use this method to get all <b>Posts</b> records. This method supports:
                                    <ul> 
                                        <li>Ordering by:
                                            <ul> 
                                                <li>Date of <b>Post</b> creation;</li>
                                                <li><b>Author</b> of <b>Post</b>;</li>
                                                <li><b>Category's</b> title;</li>
                                                <li>Number of photos in <b>Post</b>;</li>
                                            </ul>
                                        </li>
                                        <li>Search only in <b>Post's</b> text;</li>
                                        <li>Search only in <b>Post's</b> title;</li>
                                        <li>Search in <b>Post's</b> title/<b>Post's</b> text/<b>Author's</b> First name and      <b>Author's</b> Last name/<b>Category's</b> title <b>Tag's</b> title;
                                        </li>
                                        <li>Search with exact <b>Category's</b> id;</li>
                                        <li>Search with exact <b>Tag's</b> id;</li>
                                        <li>Search <b>Posts</b> with <b>Tag's</b> Id ONE of [tag_id];</li>
                                        <li>Search <b>Posts</b> with <b>Tag's</b> Id ALL in [tag_id];</li>
                                        <li>Search <b>Posts</b> by <b>Author</b> name (must contain 'first_name' and 'last_name' separated by whitespace.");
                                        </li>
                                        <li>Search <b>Posts</b> with exact date of creation;</li>
                                        <li>Search <b>Posts</b> created later than specified date;</li>
                                        <li>Search <b>Posts</b> created earlier than specified date;</li>
                                    </ul>
                                </li>
                                <li>This method is available for all registered <b>Users</b>;</li>
                                <li>In one request You may recieve maximum 50 records;</li>
                                <li>For recieve more than 50 records You'll need to use several requests with different offset;</li>
                                <li><b>Request: http://HOST:PORT/getPosts?PARAMETERS</b></li>
                                     <table>
                                            <tr>
                                                <th><b>Parameter</b></th>
                                                <th><b>Type</b></th>
                                                <th><b>Required</b></th>
                                                <th><b>Description</b></th>
                                            </tr>
                                            <tr>
                                                <td>token</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>User's</b> token. Token of <b>User</b> making request</td>
                                            </tr>
                                            <tr>
                                                <td>offset</td>
                                                <td>Integer</td>
                                                <td>Yes</td>
                                                <td>Offset from first <b>Post's</b> record corresponding to request options (ordered by <b>Post's</b> id). Must be >=0</td>
                                            </tr>
                                            <tr>
                                                <td>order_by_date</td>
                                                <td>-</td>
                                                <td>Optional</td>
                                                <td>Using by <i>default</i>. Activate order by date of <b>Post</b> creation. For activate use in query string: "order_by_date=true"</td>
                                            </tr>
                                            <tr>
                                                <td>order_by_author</td>
                                                <td>-</td>
                                                <td>Optional</td>
                                                <td>Activate order by <b>Author</b> of <b>Post</b>. For activate use in query string: "order_by_author=true" (by <i>default</i> using 'order_by_date')</td>
                                            </tr>
                                            <tr>
                                                <td>order_by_category</td>
                                                <td>-</td>
                                                <td>Optional</td>
                                                <td>Activate order by <b>Category's</b> title. For activate use in query string: "order_by_category=true" (by <i>default</i> using 'order_by_date')</td>
                                            </tr>
                                            <tr>
                                                <td>order_by_photo</td>
                                                <td>-</td>
                                                <td>Optional</td>
                                                <td>Activate order by number of photos in <b>Post</b>. For activate use in query string: "order_by_photo=true" (by <i>default</i> using 'order_by_date')</td>
                                            </tr>
                                            <tr>
                                                <td>find_in_title</td>
                                                <td>String with wildcards</td>
                                                <td>Optional</td>
                                                <td>Find string for search in <b>Post's</b> title (Sting may use wildcards - for more info see Appendix A)</td>
                                            </tr>
                                            <tr>
                                                <td>find_in_text</td>
                                                <td>String with wildcards</td>
                                                <td>Optional</td>
                                                <td>Find string for search in <b>Post's</b> text (Sting may use wildcards - for more info see Appendix A)</td>
                                            </tr>
                                            <tr>
                                                <td>find</td>
                                                <td>String with wildcards</td>
                                                <td>Optional</td>
                                                <td>Find string for search in <b>(Post's</b> title and <b>Post's</b> text) & (Author's First name and <b>Author's</b> Last name) & (<b>Category's</b> title) & (<b>Tag's</b> title) - the result is union of all searches (Sting may use wildcards - for more info see Appendix A)</td>
                                            </tr>
                                            <tr>
                                                <td>category</td>
                                                <td>Integer</td>
                                                <td>Optional</td>
                                                <td>Search <b>Posts</b> with exact <b>Category's</b> id</td>
                                            </tr>
                                            <tr>
                                                <td>tag</td>
                                                <td>[Integer]</td>
                                                <td>Optional</td>
                                                <td>Search <b>Posts</b> with exact <b>Tag's</b> id. Formed like singleton [tag_id]</td>
                                            </tr>
                                            <tr>
                                                <td>tag__in</td>
                                                <td>[Integer]</td>
                                                <td>Optional</td>
                                                <td>Search <b>Posts</b> with <b>Tag's</b> Id ONE of [tag_id]</td>
                                            </tr>
                                            <tr>
                                                <td>tag__all</td>
                                                <td>[Integer]</td>
                                                <td>Optional</td>
                                                <td>Search <b>Posts</b> with <b>Tag's</b> Id ALL in [tag_id]</td>
                                            </tr>
                                            <tr>
                                                <td>author</td>
                                                <td>String with wildcards</td>
                                                <td>Optional</td>
                                                <td>Search <b>Posts</b> by <b>Author</b> name (must contain 'first_name' and 'last_name' separated by whitespace.")</td>
                                            </tr>
                                            <tr>
                                                <td>created_at</td>
                                                <td>String</td>
                                                <td>Optional</td>
                                                <td>Search <b>Posts</b> with exact date of creation (formated like "DD.MM.YY")</td>
                                            </tr>
                                            <tr>
                                                <td>created_at__gt</td>
                                                <td>String</td>
                                                <td>Optional</td>
                                                <td>Search <b>Posts</b> created later than specified date (formated like "DD.MM.YY")</td>
                                            </tr>
                                            <tr>
                                                <td>created_at__lt</td>
                                                <td>String</td>
                                                <td>Optional</td>
                                                <td>Search <b>Posts</b> created earlier than specified date (formated like "DD.MM.YY")</td>
                                            </tr>
                                    </table> 
                                <li><b>Curl:</b>
                                    <ul>
                                        <li><b>Script location: <i>'data/curl/posts/getPosts.sh'</i></b></li>
                                        <li><b>Usage (from <i>'data/curl/posts/'</i>): '$ ./getPosts.sh [flags]'</b>
                                            <ul> 
                                                <li><b>-h</b> Print help message and exit;</li>
                                                <li><b>-y</b> Host server name;</li>
                                                <li><b>-p</b> Port server number;</li>
                                                <li><b>-t</b> <b>User's</b> token;</li>
                                                <li><b>-o</b> Offset from the first record;</li>
                                                <li><b>[-s]</b> Order: key one of ['order_by_date', 'order_by_author', 'order_by_category', 'order_by_photo'] - formated like "key=true";</li>
                                                <li><b>[-f]</b> Find string for search in <b>Post's</b> title (Sting may use wildcards - for more info see Appendix A);</li>
                                                <li><b>[-e]</b> Find string for search in <b>Post's</b> text (Sting may use wildcards - for more info see Appendix A);</li>
                                                <li><b>[-a]</b> Find string for search in <b>(Post's</b> title and <b>Post's</b> text) & (Author's First name and <b>Author's</b> Last name) & (<b>Category's</b> title) & (<b>Tag's</b> title) - the result is union of all searches (Sting may use wildcards - for more info see Appendix A);</li>
                                                <li><b>[-c]</b> Search with exact <b>Category's</b> id;</li>
                                                <li><b>[-n]</b> Search <b>Posts</b> with exact <b>Tag's</b> Id [tag_id] (lenght [tag_id] == 1);</li>
                                                <li><b>[-i]</b> Search <b>Posts</b> with <b>Tag's</b> Id ONE of [tag_id];</li>
                                                <li><b>[-k]</b> Search <b>Posts</b> with <b>Tag's</b> Id ALL in [tag_id];</li>
                                                <li><b>[-l]</b> Search <b>Posts</b> by <b>Author</b> name (must contain 'first_name' and 'last_name' separated by whitespace.");
                                                </li>
                                                <li><b>[-q]</b> Search <b>Posts</b> with exact date of creation;</li>
                                                <li><b>[-g]</b> Search <b>Posts</b> created later than specified date;</li>
                                                <li><b>[-j]</b> Search <b>Posts</b> created earlier than specified date;</li>
                                            </ul>
                                        </li>
                                    </ul>
                                </li>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>PostResponse</b> object;</li>
                                        <li><b>Fail</b>:
                                            <ul> 
                                                 <li><b>If <b>User's</b> token includes required permissions:</b> JSON <b>TextResponse</b> object with text success;</li>
                                                 <li><b>If <b>User's</b> token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with text error;</li>
                                             </ul>
                                        </li>
                                    </ul>
                                </li>
                            </ul>
                        </li>
                        <li><b>createPost</b>:
                            <ul>
                                <li>Use this method to create new <b>Post</b>;</li>
                                <li>This method is available only for <b>Authors</b>;</li>
                                <li><b>Request: http://HOST:PORT/createTag?PARAMETERS</b></li>
                                     <table>
                                            <tr>
                                                <th><b>Parameter</b></th>
                                                <th><b>Type</b></th>
                                                <th><b>Required</b></th>
                                                <th><b>Description</b></th>
                                            </tr>
                                            <tr>
                                                <td>token</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>User's</b> token. Token of <b>User</b> making request. For successful request <b>User</b> must have Admin Permissions</td>
                                            </tr>
                                            <tr>
                                                <td>title</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>Post's</b> title</td>
                                            </tr>
                                            <tr>
                                                <td>text</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>Post's</b> text</td>
                                            </tr>
                                            <tr>
                                                <td>category_id</td>
                                                <td>Integer</td>
                                                <td>Yes</td>
                                                <td><b>Category's</b> id</td>
                                            </tr>
                                            <tr>
                                                <td>tag_ids</td>
                                                <td>[Integer]</td>
                                                <td>Yes</td>
                                                <td>Array of <b>Tag's</b> id</td>
                                            </tr>
                                    </table> 
                                <li><b>Curl:</b>
                                    <ul>
                                        <li><b>Script location: <i>'data/curl/post/createPost.sh'</i></b></li>
                                        <li><b>Usage (from <i>'data/curl/post/'</i>): '$ ./createTag.sh [flags]'</b>
                                            <ul> 
                                                <li><b>-h</b> Print help message and exit;</li>
                                                <li><b>-y</b> Host server name;</li>
                                                <li><b>-p</b> Port server number;</li>
                                                <li><b>-t</b> <b>User's</b> token. Token of <b>User</b> making request. For successful request <b>User</b> must have Admin Permissions;</li>
                                                <li><b>-n</b> <b>Post's</b> title;</li>
                                                <li><b>-b</b> <b>Post's</b> text;</li>
                                                <li><b>-c</b> <b>Category's</b> id;</li>
                                                <li><b>-m</b> <b>Tag's</b> ids;</li>
                                            </ul>
                                        </li>
                                    </ul>
                                </li>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with text success;</li>
                                        <li><b>Fail</b>:
                                            <ul> 
                                                 <li><b>If <b>User's</b> token includes required permissions:</b> JSON <b>TextResponse</b> object with text success;</li>
                                                 <li><b>If <b>User's</b> token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with text error;</li>
                                             </ul>
                                        </li>
                                    </ul>
                                </li>
                            </ul>
                        </li>
                        <li><b>setPostMainPhoto</b>:
                            <ul>
                                <li>Use this method to set Main <b>Photo</b> for <b>Post</b>. If <b>Post</b> already has Main Photo, Main Photo will be updated;</li>
                                <li>This method is available only for <b>Author-Post</b> owner;</li>
                                <li><b>Request: http://HOST:PORT/setPostMainPhoto?PARAMETERS</b></li>
                                     <table>
                                            <tr>
                                                <th><b>Parameter</b></th>
                                                <th><b>Type</b></th>
                                                <th><b>Required</b></th>
                                                <th><b>Description</b></th>
                                            </tr>
                                            <tr>
                                                <td>token</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>User's</b> token. For successful request token must belong to <b>User</b> who is the <b>Author</b> of this <b>Post</b></td>
                                            </tr>
                                            <tr>
                                                <td>id</td>
                                                <td>Integer</td>
                                                <td>Yes</td>
                                                <td><b>Post's</b> id</td>
                                            </tr>
                                            <tr>
                                                <td>path</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td>Local path to photo</td>
                                            </tr>
                                    </table> 
                                <li><b>Curl:</b>
                                    <ul>
                                        <li><b>Script location: <i>'data/curl/post/setPostMainPhoto.sh'</i></b></li>
                                        <li><b>Usage (from <i>'data/curl/post/'</i>): '$ ./setPostMainPhoto.sh [flags]'</b>
                                            <ul> 
                                                <li><b>-h</b> Print help message and exit;</li>
                                                <li><b>-y</b> Host server name;</li>
                                                <li><b>-p</b> Port server number;</li>
                                                <li><b>-t</b> <b>User's</b> token;</li>
                                                <li><b>-i</b> <b>Post's</b> id</li>
                                                <li><b>-l</b> Path to photo</li>
                                            </ul>
                                        </li>
                                    </ul>
                                </li>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with text success;</li>
                                        <li><b>Fail</b>:
                                             <ul> 
                                                 <li><b>If <b>User's</b> token includes required permissions:</b> JSON <b>TextResponse</b> object with text success;</li>
                                                 <li><b>If <b>User's</b> token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with text error;</li>
                                             </ul>
                                        </li>
                                    </ul>
                                </li>
                            </ul>
                        </li>
                        <li><b>setAddMainPhoto</b>:
                            <ul>
                                <li>Use this method to add Additional <b>Photo</b> for <b>Post</b>;</li>
                                <li>This method is available only for <b>Author-Post</b> owner;</li>
                                <li><b>Request: http://HOST:PORT/setAddMainPhoto?PARAMETERS</b></li>
                                     <table>
                                            <tr>
                                                <th><b>Parameter</b></th>
                                                <th><b>Type</b></th>
                                                <th><b>Required</b></th>
                                                <th><b>Description</b></th>
                                            </tr>
                                            <tr>
                                                <td>token</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>User's</b> token. For successful request token must belong to <b>User</b> who is the <b>Author</b> of this <b>Post</b></td>
                                            </tr>
                                            <tr>
                                                <td>id</td>
                                                <td>Integer</td>
                                                <td>Yes</td>
                                                <td><b>Post's</b> id</td>
                                            </tr>
                                            <tr>
                                                <td>path</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td>Local path to photo</td>
                                            </tr>
                                    </table> 
                                <li><b>Curl:</b>
                                    <ul>
                                        <li><b>Script location: <i>'data/curl/post/setAddMainPhoto.sh'</i></b></li>
                                        <li><b>Usage (from <i>'data/curl/post/'</i>): '$ ./setAddMainPhoto.sh [flags]'</b>
                                            <ul> 
                                                <li><b>-h</b> Print help message and exit;</li>
                                                <li><b>-y</b> Host server name;</li>
                                                <li><b>-p</b> Port server number;</li>
                                                <li><b>-t</b> <b>User's</b> token;</li>
                                                <li><b>-i</b> <b>Post's</b> id</li>
                                                <li><b>-l</b> Path to photo</li>
                                            </ul>
                                        </li>
                                    </ul>
                                </li>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with text success;</li>
                                        <li><b>Fail</b>:
                                             <ul> 
                                                 <li><b>If <b>User's</b> token includes required permissions:</b> JSON <b>TextResponse</b> object with text success;</li>
                                                 <li><b>If <b>User's</b> token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with text error;</li>
                                             </ul>
                                        </li>
                                    </ul>
                                </li>
                            </ul>
                        </li>
                        <li><b>removePost</b>:
                            <ul>
                                <li>Use this method to remove <b>Post</b>;</li>
                                <li>This method is available only for admins;</li>
                                <li><b>Request: http://HOST:PORT/removePost?PARAMETERS</b></li>
                                     <table>
                                            <tr>
                                                <th><b>Parameter</b></th>
                                                <th><b>Type</b></th>
                                                <th><b>Required</b></th>
                                                <th><b>Description</b></th>
                                            </tr>
                                            <tr>
                                                <td>token</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>User's</b> token. Token of <b>User</b> making request. For successful request <b>User</b> must have Admin Permissions</td>
                                            </tr>
                                            <tr>
                                                <td>id</td>
                                                <td>Integer</td>
                                                <td>Yes</td>
                                                <td><b>Post's</b> id</td>
                                            </tr>
                                    </table> 
                                <li><b>Curl:</b>
                                    <ul>
                                        <li><b>Script location: <i>'data/curl/post/removePost.sh'</i></b></li>
                                        <li><b>Usage (from <i>'data/curl/post/'</i>): '$ ./removePost.sh [flags]'</b>
                                            <ul> 
                                                <li><b>-h</b> Print help message and exit;</li>
                                                <li><b>-y</b> Host server name;</li>
                                                <li><b>-t</b> <b>User's</b> token. Token of <b>User</b> making request. For successful request <b>User</b> must have Admin Permissions;</li>
                                                <li><b>-i</b> <b>Post's</b> id;</li>
                                            </ul>
                                        </li>
                                    </ul>
                                </li>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with text success;</li>
                                        <li><b>Fail</b>:
                                             <ul> 
                                                 <li><b>If <b>User's</b> token includes required permissions:</b> JSON <b>TextResponse</b> object with text success;</li>
                                                 <li><b>If <b>User's</b> token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with text error;</li>
                                             </ul>
                                        </li>
                                    </ul>
                                </li>
                            </ul>
                        </li>
                   </ul>
                </li>
                <li><b>Draft</b>:
                    <ul>
                        <li><b>getDrafts</b>:
                            <ul>
                                <li>Use this method to get all <b>Draft</b> records of <b>Author</b>;</li>
                                <li>This method is available only for <b>Author-Post</b> owner;</li>
                                <li>In one request You may recieve maximum 50 records;</li>
                                <li>For recieve more than 50 records You'll need to use several requests with different offset;</li>
                                <li><b>Request: http://HOST:PORT/getDrafts?PARAMETERS</b></li>
                                     <table>
                                            <tr>
                                                <th><b>Parameter</b></th>
                                                <th><b>Type</b></th>
                                                <th><b>Required</b></th>
                                                <th><b>Description</b></th>
                                            </tr>
                                            <tr>
                                                <td>token</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>User's</b> token. Token of <b>User</b> making request. For successful request <b>User</b> must have Read Author Permissions;</td>
                                            </tr>
                                            <tr>
                                                <td>offset</td>
                                                <td>Integer</td>
                                                <td>Yes</td>
                                                <td>Offset from first <b>Draft's</b> record (ordered by <b>Draft's</b> id). Must be >=0</td>
                                            </tr>
                                    </table> 
                                <li><b>Curl:</b>
                                    <ul>
                                        <li><b>Script location: <i>'data/curl/tag/getDrafts.sh'</i></b></li>
                                        <li><b>Usage (from <i>'data/curl/tag/'</i>): '$ ./getDrafts.sh [flags]'</b>
                                            <ul> 
                                                <li><b>-h</b> Print help message and exit;</li>
                                                <li><b>-y</b> Host server name;</li>
                                                <li><b>-p</b> Port server number;</li>
                                                <li><b>-t</b> <b>User's</b> token. Token of <b>User</b> making request. For successful request <b>User</b> must have Read Author Permissions;</li>
                                                <li><b>-o</b> Offset from first <b>Draft's</b> record (ordered by <b>Draft's</b> id);</li>
                                            </ul>
                                        </li>
                                     </ul>
                                </li>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>DraftResponse</b> object;</li>
                                        <li><b>Fail</b>:
                                            <ul> 
                                                 <li><b>If <b>User's</b> token includes required permissions:</b> JSON <b>TextResponse</b> object with text success;</li>
                                                 <li><b>If <b>User's</b> token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with text error;</li>
                                             </ul>
                                        </li>
                                    </ul>
                                </li>
                            </ul>
                        </li>
                        <li><b>createDraft</b>:
                            <ul>
                                <li>Use this method to create <b>Draft</b> of <b>Post</b>;</li>
                                <li>This method is available only for <b>Author-Post</b> owner;</li>
                                <li><b>Request: http://HOST:PORT/createDraft?PARAMETERS</b></li>
                                     <table>
                                            <tr>
                                                <th><b>Parameter</b></th>
                                                <th><b>Type</b></th>
                                                <th><b>Required</b></th>
                                                <th><b>Description</b></th>
                                            </tr>
                                            <tr>
                                                <td>token</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>User's</b> token. Token of <b>User</b> making request. For successful request <b>User</b> must have Admin Permissions</td>
                                            </tr>
                                            <tr>
                                                <td>post_id</td>
                                                <td>Integer</td>
                                                <td>Yes</td>
                                                <td><b>Post's</b> id corresponding to <b>Draft</b></td>
                                            </tr>
                                            <tr>
                                                <td>text</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>Draft's</b> text</td>
                                            </tr>
                                    </table> 
                                <li><b>Curl:</b>
                                    <ul>
                                        <li><b>Script location: <i>'data/curl/draft/createDraft.sh'</i></b></li>
                                        <li><b>Usage (from <i>'data/curl/draft/'</i>): '$ ./createDraft.sh [flags]'</b>
                                            <ul> 
                                                <li><b>-h</b> Print help message and exit;</li>
                                                <li><b>-y</b> Host server name;</li>
                                                <li><b>-p</b> Port server number;</li>
                                                <li><b>-t</b> <b>User's</b> token. Token of <b>User</b> making request. For successful request <b>User</b> must have Read Author Permissions;</li>
                                                <li><b>-b</b> Draft's text;</li>
                                                <li><b>-i</b> <b>Post's</b> id corresponding to <b>Draft</b>;</li>
                                            </ul>
                                        </li>
                                    </ul>
                                </li>   
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with text success;</li>
                                        <li><b>Fail</b>:
                                            <ul> 
                                                 <li><b>If <b>User's</b> token includes required permissions:</b> JSON <b>TextResponse</b> object with text success;</li>
                                                 <li><b>If <b>User's</b> token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with text error;</li>
                                             </ul>
                                        </li>
                                    </ul>
                                </li>
                            </ul>
                        </li>
                        <li><b>editDraft</b>:
                            <ul>
                                <li>Use this method to edit <b>Draft's</b> text;</li>
                                <li>This method is available only for <b>Author-Post</b> owner;</li>
                                <li><b>Request: http://HOST:PORT/editDraft?PARAMETERS</b></li>
                                     <table>
                                            <tr>
                                                <th><b>Parameter</b></th>
                                                <th><b>Type</b></th>
                                                <th><b>Required</b></th>
                                                <th><b>Description</b></th>
                                            </tr>
                                            <tr>
                                                <td>token</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>User's</b> token. Token of <b>User</b> making request. For successful request <b>User</b> must have Admin Permissions</td>
                                            </tr>
                                            <tr>
                                                <td>post_id</td>
                                                <td>Integer</td>
                                                <td>Yes</td>
                                                <td><b>Post's</b> id corresponding to <b>Draft</b></td>
                                            </tr>
                                            <tr>
                                                <td>text</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td>New <b>Draft's</b> text</td>
                                            </tr>
                                    </table> 
                                <li><b>Curl:</b>
                                    <ul>
                                        <li><b>Script location: <i>'data/curl/drafts/editDraft.sh'</i></b></li>
                                        <li><b>Usage (from <i>'data/curl/drafts/'</i>): '$ ./editDraft.sh [flags]'</b>
                                            <ul> 
                                                <li><b>-h</b> Print help message and exit;</li>
                                                <li><b>-y</b> Host server name;</li>
                                                <li><b>-t</b> <b>User's</b> token. Token of <b>User</b> making request. For successful request <b>User</b> must have Admin Permissions;</li>
                                                <li><b>-b</b> New Draft's text;</li>
                                                <li><b>-i</b> <b>Post's</b> id corresponding to <b>Draft</b>;</li>
                                            </ul>
                                        </li>
                                    </ul>
                                </li>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with text success;</li>
                                        <li><b>Fail</b>:
                                             <ul> 
                                                 <li><b>If <b>User's</b> token includes required permissions:</b> JSON <b>TextResponse</b> object with text success;</li>
                                                 <li><b>If <b>User's</b> token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with text error;</li>
                                             </ul>
                                        </li>
                                    </ul>
                                </li>
                            </ul>
                        </li>
                        <li><b>publishDraft</b>:
                            <ul>
                                <li>Use this method to publish text of <b>Draft</b> (Change corresponding <b>Post's</b> text to <b>Draft's</b> text);</li>
                                <li>This method is available only for <b>Author-Post</b> owner;</li>
                                <li><b>Request: http://HOST:PORT/publishDraft?PARAMETERS</b></li>
                                     <table>
                                            <tr>
                                                <th><b>Parameter</b></th>
                                                <th><b>Type</b></th>
                                                <th><b>Required</b></th>
                                                <th><b>Description</b></th>
                                            </tr>
                                            <tr>
                                                <td>token</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>User's</b> token. Token of <b>User</b> making request. For successful request <b>User</b> must have Admin Permissions</td>
                                            </tr>
                                            <tr>
                                                <td>post_id</td>
                                                <td>Integer</td>
                                                <td>Yes</td>
                                                <td><b>Post's</b> id corresponding to <b>Draft</b></td>
                                            </tr>
                                    </table> 
                                <li><b>Curl:</b>
                                    <ul>
                                        <li><b>Script location: <i>'data/curl/draft/publishDraft.sh'</i></b></li>
                                        <li><b>Usage (from <i>'data/curl/draft/'</i>): '$ ./publishDraft.sh [flags]'</b>
                                            <ul> 
                                                <li><b>-h</b> Print help message and exit;</li>
                                                <li><b>-y</b> Host server name;</li>
                                                <li><b>-t</b> <b>User's</b> token. Token of <b>User</b> making request. For successful request <b>User</b> must have Admin Permissions;</li>
                                                <li><b>-i</b> <b>Post's</b> id corresponding to <b>Draft</b>;</li>
                                            </ul>
                                        </li>
                                    </ul>
                                </li>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with text success;</li>
                                        <li><b>Fail</b>:
                                             <ul> 
                                                 <li><b>If <b>User's</b> token includes required permissions:</b> JSON <b>TextResponse</b> object with text success;</li>
                                                 <li><b>If <b>User's</b> token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with text error;</li>
                                             </ul>
                                        </li>
                                    </ul>
                                </li>
                            </ul>
                        </li>
                        <li><b>removeDraft</b>:
                            <ul>
                                <li>Use this method to remove <b>Draft</b>;</li>
                                <li>This method is available only for <b>Author-Post</b> owner;</li>
                                <li><b>Request: http://HOST:PORT/removeDraft?PARAMETERS</b></li>
                                     <table>
                                            <tr>
                                                <th><b>Parameter</b></th>
                                                <th><b>Type</b></th>
                                                <th><b>Required</b></th>
                                                <th><b>Description</b></th>
                                            </tr>
                                            <tr>
                                                <td>token</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>User's</b> token. Token of <b>User</b> making request. For successful request <b>User</b> must have Admin Permissions</td>
                                            </tr>
                                            <tr>
                                                <td>post_id</td>
                                                <td>Integer</td>
                                                <td>Yes</td>
                                                <td><b>Post's</b> id corresponding to <b>Draft</b></td>
                                            </tr>
                                    </table> 
                                <li><b>Curl:</b>
                                    <ul>
                                        <li><b>Script location: <i>'data/curl/draft/removeDraft.sh'</i></b></li>
                                        <li><b>Usage (from <i>'data/curl/draft/</i>): '$ ./removeDraft.sh [flags]'</b>
                                            <ul> 
                                                <li><b>-h</b> Print help message and exit;</li>
                                                <li><b>-y</b> Host server name;</li>
                                                <li><b>-t</b> <b>User's</b> token. Token of <b>User</b> making request. For successful request <b>User</b> must have Admin Permissions;</li>
                                                <li><b>-i</b> <b>Post's</b> id corresponding to <b>Draft</b>;</li>
                                            </ul>
                                        </li>
                                    </ul>
                                </li>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with text success;</li>
                                        <li><b>Fail</b>:
                                             <ul> 
                                                 <li><b>If <b>User's</b> token includes required permissions:</b> JSON <b>TextResponse</b> object with text success;</li>
                                                 <li><b>If <b>User's</b> token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with text error;</li>
                                             </ul>
                                        </li>
                                    </ul>
                                </li>
                            </ul>
                        </li>
                   </ul>
                </li>
                <li><b>Comment</b>:
                    <ul>
                        <li><b>createComment</b>:
                            <ul>
                                <li>Use this method to create <b>Comment</b> record to specified <b>Post</b>;</li>
                                <li>This method is available for all registered <b>Users</b>;</li>
                                <li><b>Request: http://HOST:PORT/createComment?PARAMETERS</b></li>
                                     <table>
                                            <tr>
                                                <th><b>Parameter</b></th>
                                                <th><b>Type</b></th>
                                                <th><b>Required</b></th>
                                                <th><b>Description</b></th>
                                            </tr>
                                            <tr>
                                                <td>token</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>User's</b> token. Token of <b>User</b> making request</td>
                                            </tr>
                                            <tr>
                                                <td>post_id</td>
                                                <td>Integer</td>
                                                <td>Yes</td>
                                                <td><b>Post's</b> id corresponding to <b>Comment</b></td>
                                            </tr>
                                            <tr>
                                                <td>text</td>
                                                <td>String</td>
                                                <td>Yes</td>
                                                <td><b>Comment's</b> text</td>
                                            </tr>
                                    </table> 
                                <li><b>Curl:</b>
                                    <ul>
                                        <li><b>Script location: <i>'data/curl/comment/createComment.sh'</i></b></li>
                                        <li><b>Usage (from <i>'data/curl/comment/'</i>): '$ ./createComment.sh [flags]'</b>
                                            <ul> 
                                                <li><b>-h</b> Print help message and exit;</li>
                                                <li><b>-y</b> Host server name;</li>
                                                <li><b>-p</b> Port server number;</li>
                                                <li><b>-t</b> <b>User's</b> token. Token of <b>User</b> making request;</li>
                                                <li><b>-c</b> <b>Comment's</b> text;</li>
                                            </ul>
                                        </li>
                                     </ul>
                                </li>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object;</li>
                                        <li><b>Fail</b>: JSON <b>TextResponse</b> object;</li>
                                    </ul>
                                </li>
                            </ul>
                        </li>
            </ul>
        </li>
    </ul>
<h3>Objects</h3>
<ul> 
    <li><b>Post</b>
        <p>This object represents a <b>Post</b> record.
           <table>
                <tr>
                    <th><b>Field</b></th>
                    <th><b>Type</b></th>
                    <th><b>Description</b></th>
                </tr>
                <tr>
                    <td>id</td>
                    <td>Integer</td>
                    <td>Unique identifier for this Post</td>
                </tr>
                <tr>
                    <td>author</td>
                    <td>Author</td>
                    <td>Author of <b>Post</b> record</td>
                </tr>
                <tr>
                    <td>title</td>
                    <td>String</td>
                    <td>Title of <b>Post</b> record</td>
                </tr>
                <tr>
                    <td>text</td>
                    <td>String</td>
                    <td>Text of <b>Post</b> record</td>
                </tr>
                <tr>
                    <td>created_at</td>
                    <td>String</td>
                    <td>Date of creation of this <b>Post</b> record. Formated like "DD.MM.YY"</td>
                </tr>
                <tr>
                    <td>category</td>
                    <td>Category</td>
                    <td><b>Category</b> of <b>Post</b> record</td>
                </tr>
                <tr>
                    <td>tags</td>
                    <td>[Tag]</td>
                    <td><i>Optional.</i> List of <b>Tags</b> of <b>Post</b> record</td>
                </tr>
                <tr>
                    <td>main_photo</td>
                    <td>Photo</td>
                    <td><i>Optional.</i> Main <b>Photo</b> of <b>Post</b> record</td>
                </tr>
                <tr>
                    <td>add_photo</td>
                    <td>[Photo]</td>
                    <td><i>Optional.</i> Additional <b>Photos</b> of <b>Post</b> record</td>
                </tr>
                <tr>
                    <td>comments</td>
                    <td>[Comment]</td>
                    <td><i>Optional.</i><b>Comments</b> of <b>Post</b> record</td>
                </tr>
            </table> 
        </p>
    </li>
    <li><b>User</b>
        <p>This object represents a <b>User</b> record.
           <table>
                <tr>
                    <th><b>Field</b></th>
                    <th><b>Type</b></th>
                    <th><b>Description</b></th>
                </tr>
                <tr>
                    <td>id</td>
                    <td>Integer</td>
                    <td>Unique identifier for this <b>User</b></td>
                </tr>
                <tr>
                    <td>is_admin</td>
                    <td>Boolean</td>
                    <td>True, if this <b>User</b> is a admin</td>
                </tr>
                <tr>
                    <td>first_name</td>
                    <td>String</td>
                    <td><b>User's</b> first name</td>
                </tr>
                <tr>
                    <td>last_name</td>
                    <td>String</td>
                    <td><b>User's</b> last name</td>
                </tr>
                <tr>
                    <td>photo</td>
                    <td>Photo</td>
                    <td><b>User's</b> <b>Photo</b></td>
                </tr>
            </table> 
        </p>
    </li>
    <li><b>Author</b>
        <p>This object represents a <b>Author</b> record.
           <table>
                <tr>
                    <th><b>Field</b></th>
                    <th><b>Type</b></th>
                    <th><b>Description</b></th>
                </tr>
                <tr>
                    <td>user</td>
                    <td>User</td>
                    <td><b>User</b> corresponding to this <b>Author</b></td>
                </tr>
                <tr>
                    <td>description</td>
                    <td>String</td>
                    <td><b>Author's</b> description</td>
                </tr>
            </table> 
        </p>
    </li>
    <li><b>Category</b>
        <p>This object represents a <b>Category</b> record.
           <table>
                <tr>
                    <th><b>Field</b></th>
                    <th><b>Type</b></th>
                    <th><b>Description</b></th>
                </tr>
                <tr>
                    <td>id</td>
                    <td>Integer</td>
                    <td>Unique identifier for this <b>Category</b></td>
                </tr>
                <tr>
                    <td>title</td>
                    <td>String</td>
                    <td><b>Category's</b> title</td>
                </tr>
                <tr>
                    <td>subcategory</td>
                    <td>Category</td>
                    <td><i>Optional.</i><b>SubCategory</b> of this <b>Category</b></td>
                </tr>
            </table> 
        </p>
    </li>
    <li><b>Tag</b>
        <p>This object represents a <b>Tag</b> record.
           <table>
                <tr>
                    <th><b>Field</b></th>
                    <th><b>Type</b></th>
                    <th><b>Description</b></th>
                </tr>
                <tr>
                    <td>id</td>
                    <td>Integer</td>
                    <td>Unique identifier for this <b>Tag</b></td>
                </tr>
                <tr>
                    <td>title</td>
                    <td>String</td>
                    <td><b>Tag's</b> title</td>
                </tr>
            </table> 
        </p>
    </li>
    <li><b>Draft</b>
        <p>This object represents a <b>Draft</b> record.
           <table>
                <tr>
                    <th><b>Field</b></th>
                    <th><b>Type</b></th>
                    <th><b>Description</b></th>
                </tr>
                <tr>
                    <td>id</td>
                    <td>Integer</td>
                    <td>Unique identifier for this <b>Draft</b></td>
                </tr>
                <tr>
                    <td>text</td>
                    <td>String</td>
                    <td><b>Draft's</b> text</td>
                </tr>
                <tr>
                    <td>post_id</td>
                    <td>Integer</td>
                    <td><b>Post's</b> id corresponding to <b>Draft</b></td>
                </tr>
            </table> 
        </p>
    </li>
    <li><b>Photo</b>
        <p>This object represents a <b>Photo</b> record.
           <table>
                <tr>
                    <th><b>Field</b></th>
                    <th><b>Type</b></th>
                    <th><b>Description</b></th>
                </tr>
                <tr>
                    <td>id</td>
                    <td>Integer</td>
                    <td>Unique identifier for this <b>Photo</b></td>
                </tr>
                <tr>
                    <td>link</td>
                    <td>String</td>
                    <td>Link to <b>Photo</b></td>
                </tr>
            </table> 
        </p>
    </li>
    <li><b>Comment</b>
        <p>This object represents a <b>Comment</b> record.
           <table>
                <tr>
                    <th><b>Field</b></th>
                    <th><b>Type</b></th>
                    <th><b>Description</b></th>
                </tr>
                <tr>
                    <td>id</td>
                    <td>Integer</td>
                    <td>Unique identifier for this <b>Comment</b></td>
                </tr>
                <tr>
                    <td>text</td>
                    <td>String</td>
                    <td><b>Comment's</b> text</td>
                </tr>
            </table> 
        </p>
    </li>
<h3>Responses</h3>
<ul> 
    <li><b>PostResponse</b>
        <p>This object represents a Response containing <b>Post</b> records.
           <table>
                <tr>
                    <th><b>Field</b></th>
                    <th><b>Type</b></th>
                    <th><b>Description</b></th>
                </tr>
                <tr>
                    <td>offset</td>
                    <td>Integer</td>
                    <td>Offset from first <b>Post's</b> record corresponding to request options (ordered by <b>Post's</b> id)</td>
                </tr>
                <tr>
                    <td>posts</td>
                    <td>[Post]</td>
                    <td>List of <b>Post's</b> records corresponding to request options (Limit 50 records in one request)</td>
                </tr>
            </table> 
        </p>
    </li>
    <li><b>UserResponse</b>
        <p>This object represents a Response containing <b>User</b> records.
           <table>
                <tr>
                    <th><b>Field</b></th>
                    <th><b>Type</b></th>
                    <th><b>Description</b></th>
                </tr>
                <tr>
                    <td>offset</td>
                    <td>Integer</td>
                    <td>Offset from first <b>User's</b> record (ordered by <b>User's</b> id)</td>
                </tr>
                <tr>
                    <td>users</td>
                    <td>[User]</td>
                    <td>List of <b>User's</b> records corresponding to request options (Limit 50 records in one request)</td>
                </tr>
            </table> 
        </p>
    </li>
    <li><b>AuthorResponse</b>
        <p>This object represents a Response containing <b>Author</b> records.
           <table>
                <tr>
                    <th><b>Field</b></th>
                    <th><b>Type</b></th>
                    <th><b>Description</b></th>
                </tr>
                <tr>
                    <td>offset</td>
                    <td>Integer</td>
                    <td>Offset from first <b>Author's</b> record (ordered by <b>Author's</b> id)</td>
                </tr>
                <tr>
                    <td>users</td>
                    <td>[Author]</td>
                    <td>List of <b>Author's</b> records corresponding to request options (Limit 50 records in one request)</td>
                </tr>
            </table> 
        </p>
    </li>
    <li><b>CategoryResponse</b>
        <p>This object represents a Response containing <b>Category</b> records.
           <table>
                <tr>
                    <th><b>Field</b></th>
                    <th><b>Type</b></th>
                    <th><b>Description</b></th>
                </tr>
                <tr>
                    <td>offset</td>
                    <td>Integer</td>
                    <td>Offset from first <b>Category's</b> record (ordered by <b>Category's</b> id)</td>
                </tr>
                <tr>
                    <td>categories</td>
                    <td>[Category]</td>
                    <td>List of <b>Category's</b> records corresponding to request options (Limit 50 records in one request)</td>
                </tr>
            </table> 
        </p>
    </li>
    <li><b>TagResponse</b>
        <p>This object represents a Response containing <b>Tag</b> records.
           <table>
                <tr>
                    <th><b>Field</b></th>
                    <th><b>Type</b></th>
                    <th><b>Description</b></th>
                </tr>
                <tr>
                    <td>offset</td>
                    <td>Integer</td>
                    <td>Offset from first <b>Tag's</b> record (ordered by <b>Tag's</b> id)</td>
                </tr>
                <tr>
                    <td>tags</td>
                    <td>[Tag]</td>
                    <td>List of <b>Tag's</b> records corresponding to request options (Limit 50 records in one request)</td>
                </tr>
            </table> 
        </p>
    </li>
    <li><b>DraftResponse</b>
        <p>This object represents a Response containing <b>Draft</b> records.
           <table>
                <tr>
                    <th><b>Field</b></th>
                    <th><b>Type</b></th>
                    <th><b>Description</b></th>
                </tr>
                <tr>
                    <td>offset</td>
                    <td>Integer</td>
                    <td>Offset from first <b>Draft's</b> record (ordered by <b>Draft's</b> id)</td>
                </tr>
                <tr>
                    <td>drafts</td>
                    <td>[Draft]</td>
                    <td>List of <b>Draft's</b> records corresponding to request options (Limit 50 records in one request)</td>
                </tr>
            </table> 
        </p>
    </li>
    <li><b>TextResponse</b>
        <p>This object represents a message detailing success/error of processing request.
           <table>
                <tr>
                    <th><b>Field</b></th>
                    <th><b>Type</b></th>
                    <th><b>Description</b></th>
                </tr>
                <tr>
                    <td>message</td>
                    <td>String</td>
                    <td>Message detailing success/error of processing request</td>
                </tr>
            </table> 
        </p>
    </li>
</ul>
<h3>Permissions</h3>
<ul>
    <li><b>Admin Permission</b>
        <p>This type of permissions have only persons from admin list in <i>data/config.json</i> file</p>
        <ul>
            <li>Permissions to get/create/edit/remove <b>Author's</b> records;</li>
            <li>Permissions to get/remove <b>Post's</b> records;</li>
            <li>Permissions to get/remove <b>User's</b> records;</li>
            <li>Permissions to get/create/edit/remove <b>Category's</b> records;</li>
            <li>Permissions to get/create/edit/remove <b>Tag's</b> records;</li>
            <li>Permissions to create <b>Comment's</b> records;</li>
        </ul>
    </li>
    <li><b>User Permission</b>
        <p>This type of permissions have all registered <b>Users</b></p>
        <ul>
            <li>Permissions to get <b>Post's</b> records;</li>
            <li>Permissions to get <b>User's</b> records;</li>
            <li>Permissions to get <b>Category's</b> records;</li>
            <li>Permissions to get <b>Tag's</b> records;</li>
            <li>Permissions to create <b>Comment's</b> records;</li>
        </ul>
    </li>
    <li><b>Author Write Permission</b>
        <p>This type of permissions have all registered <b>Users</b></p>
        <ul>
            <li>Permissions to get/create <b>Post's</b> records;</li>
            <li>Permissions to get <b>User's</b> records;</li>
            <li>Permissions to get <b>Category's</b> records;</li>
            <li>Permissions to get <b>Tag's</b> records;</li>
            <li>Permissions to create <b>Comment's</b> records;</li>
        </ul>
    </li>
    <li><b>Author Write Permission</b>
        <p>This type of permissions have all registered <b>Users</b></p>
        <ul>
            <li>Permissions to get/create <b>Post's</b> records;</li>
            <li>Permissions to get <b>User's</b> records;</li>
            <li>Permissions to get/edit/publish/remove <b>Draft's</b> records of <b>Post's</b> belonging to this <b>Author</b>;</li>
            <li>Permissions to get <b>Category's</b> records;</li>
            <li>Permissions to get <b>Tag's</b> records;</li>
            <li>Permissions to create <b>Comment's</b> records;</li>
        </ul>
    </li>
</ul>
<h3>Appendix A</h3>
<ul>
    <li>For unicode symbols in requests (using <i>.sh</i> scripts) use folowing:
        <ul>
            <li>Instead of whitespace: <b>%20</b></li>
            <li>Instead of left square parenthesis "[": <b>%5B</b></li>
            <li>Instead of right square parenthesis "[": <b>%5D</b></li>
        </ul>
    </li>
    <li>Supporting wildcard symbols:
        <ul>
            <li><b>_</b> denotes any single character (is comparable to <b>.</b> in POSIX regular expressions);</li>
            <li><b>%</b> denotes any string (is comparable to <b>.*</b> in POSIX regular expressions);</li>
            <li><b>|</b> denotes alternation (either of two alternatives);</li>
            <li><b>*</b> denotes repetition of the previous item zero or more times;</li>
            <li><b>+</b> denotes repetition of the previous item one or more times;</li>
            <li><b>?</b> denotes repetition of the previous item zero or one time;</li>
            <li><b>{m}</b> denotes repetition of the previous item exactly m times;</li>
            <li><b>{m,}</b> denotes repetition of the previous item m or more times;</li>
            <li><b>{m,n}</b> denotes repetition of the previous item at least m and not more than n times;</li>
            <li><b>Parentheses ()</b> can be used to group items into a single logical item;</li>
            <li><b>A bracket expression [...]</b> specifies a character class, just as in POSIX regular expressions;</li>
        </ul>
    </li>
</ul>