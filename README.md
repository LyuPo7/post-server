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
                <li><b>error</b>-level: show only error messages;</li>
                <li><b>warning</b>-level: show only error and warning messages;</li>
                <li><b>info</b>-level(default): show info, error and warning messages;</li>
                <li><b>debug</b>-level: show even more details than info-level;</li>
            </ul>
        </li>
    </ul>

<h3>Installation:</h3>
    <p>You may clone GitHub repository</p>
        <p><b>git clone https://github.com/LyuPo7/post-server.git</b></p>
 
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
                                        <li><b>Fail</b>: JSON <b>TextResponse</b> object with Text error;</li>
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
                                <li>This method is available for all registered Users;</li>
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
                                        <li><b>Fail</b>: JSON <b>TextResponse</b> object with Text error;</li>
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
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with Text success;</li>
                                        <li><b>Fail</b>: JSON <b>TextResponse</b> object with Text error;</li>
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
                                                <li><b>-t</b> User's token;</li>
                                                <li><b>-l</b> Path to photo;</li>
                                            </ul>
                                        </li>
                                    </ul>
                                </li>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with Text success;</li>
                                        <li><b>Fail</b>:
                                             <ul> 
                                                 <li><b>If User's token includes required permissions:</b> JSON <b>TextResponse</b> object with Text success;</li>
                                                 <li><b>If User's token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with Text error;</li>
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
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with Text success;</li>
                                        <li><b>Fail</b>:
                                             <ul> 
                                                 <li><b>If User's token includes required permissions:</b> JSON <b>TextResponse</b> object with Text success;</li>
                                                 <li><b>If User's token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with Text error;</li>
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
                                                 <li><b>If User's token includes required permissions:</b> JSON <b>TextResponse</b> object with Text success;</li>
                                                 <li><b>If User's token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with Text error;</li>
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
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with Text success;</li>
                                        <li><b>Fail</b>:
                                            <ul> 
                                                 <li><b>If User's token includes required permissions:</b> JSON <b>TextResponse</b> object with Text success;</li>
                                                 <li><b>If User's token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with Text error;</li>
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
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with Text success;</li>
                                        <li><b>Fail</b>:
                                             <ul> 
                                                 <li><b>If User's token includes required permissions:</b> JSON <b>TextResponse</b> object with Text success;</li>
                                                 <li><b>If User's token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with Text error;</li>
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
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with Text success;</li>
                                        <li><b>Fail</b>:
                                             <ul> 
                                                 <li><b>If User's token includes required permissions:</b> JSON <b>TextResponse</b> object with Text success;</li>
                                                 <li><b>If User's token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with Text error;</li>
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
                                <li>This method is available for all registered Users</li>
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
                                                 <li><b>If User's token includes required permissions:</b> JSON <b>TextResponse</b> object with Text success;</li>
                                                 <li><b>If User's token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with Text error;</li>
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
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with Text success;</li>
                                        <li><b>Fail</b>:
                                            <ul> 
                                                 <li><b>If User's token includes required permissions:</b> JSON <b>TextResponse</b> object with Text success;</li>
                                                 <li><b>If User's token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with Text error;</li>
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
                                        <li><b>Usage: '$ ./editCategory.sh [flags]'</b>
                                            <ul> 
                                                <li><b>-h</b> Print help message and exit;</li>
                                                <li><b>-y</b> Host server name;</li>
                                                <li><b>-t</b> <b>User's</b> token. Token of <b>User</b> making request. For successful request <b>User</b> must have Admin Permissions;</li>
                                                <li><b>-i</b> <b>Category's</b> id;</li>
                                                <li><b>[-s]</b> <b>SubCategory's</b> title (if You want to change/set SubCategory of <b>Category</b>);</li>
                                                <li><b>[-s]</b> New <b>Category's</b> title (if You want to change <b>Category's</b> title);</li>
                                            </ul>
                                        </li>
                                    </ul>
                                </li>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with Text success;</li>
                                        <li><b>Fail</b>:
                                             <ul> 
                                                 <li><b>If User's token includes required permissions:</b> JSON <b>TextResponse</b> object with Text success;</li>
                                                 <li><b>If User's token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with Text error;</li>
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
                                <li><b>Script: 'data/curl/category/removeCategory.sh'</b></li>
                                <li><b>Usage: '$ ./removeCategory.sh [flags]'</b></li>
                                    <ul> 
                                        <li><b>-h</b> Print help message and exit;</li>
                                        <li><b>-y</b> Host server name;</li>
                                        <li><b>-t</b> User's token (must has Admin Permissions);</li>
                                        <li><b>-i</b> Category's id;</li>
                                    </ul>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with Text success;</li>
                                        <li><b>Fail</b>:
                                             <ul> 
                                                 <li><b>If User's token includes required permissions:</b> JSON <b>TextResponse</b> object with Text success;</li>
                                                 <li><b>If User's token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with Text error;</li>
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
                                <li>This method is available for all registered Users</li>
                                <li>In one request You may recieve maximum 50 records;</li>
                                <li>For recieve more than 50 records You'll need to use several requests with different offset;</li>
                                <li><b>Script: 'data/curl/tag/getTags.sh'</b></li>
                                <li><b>Usage: '$ ./getTags.sh [flags]'</b></li>
                                    <ul> 
                                        <li><b>-h</b> Print help message and exit;</li>
                                        <li><b>-y</b> Host server name;</li>
                                        <li><b>-p</b> Port server number;</li>
                                        <li><b>-t</b> User's token;</li>
                                        <li><b>-o</b> Offset from the first record;</li>
                                    </ul>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>TagResponse</b> object;</li>
                                        <li><b>Fail</b>:
                                            <ul> 
                                                 <li><b>If User's token includes required permissions:</b> JSON <b>TextResponse</b> object with Text success;</li>
                                                 <li><b>If User's token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with Text error;</li>
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
                                <li><b>Script: 'data/curl/tag/createTag.sh'</b></li>
                                <li><b>Usage: '$ ./createTag.sh [flags]'</b></li>
                                    <ul> 
                                        <li><b>-h</b> Print help message and exit;</li>
                                        <li><b>-y</b> Host server name;</li>
                                        <li><b>-p</b> Port server number;</li>
                                        <li><b>-t</b> User's token (must has Admin Permissions);</li>
                                        <li><b>-n</b> Tag's title;</li>
                                    </ul>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with Text success;</li>
                                        <li><b>Fail</b>:
                                            <ul> 
                                                 <li><b>If User's token includes required permissions:</b> JSON <b>TextResponse</b> object with Text success;</li>
                                                 <li><b>If User's token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with Text error;</li>
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
                                <li><b>Script: 'data/curl/category/editTag.sh'</b></li>
                                <li><b>Usage: '$ ./editTag.sh [flags]'</b></li>
                                    <ul> 
                                        <li><b>-h</b> Print help message and exit;</li>
                                        <li><b>-y</b> Host server name;</li>
                                        <li><b>-t</b> User's token (must has Admin Permissions);</li>
                                        <li><b>-n</b> New Tag's title;</li>
                                        <li><b>-o</b> Old Tag's title;</li>
                                    </ul>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with Text success;</li>
                                        <li><b>Fail</b>:
                                             <ul> 
                                                 <li><b>If User's token includes required permissions:</b> JSON <b>TextResponse</b> object with Text success;</li>
                                                 <li><b>If User's token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with Text error;</li>
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
                                <li><b>Script: 'data/curl/tag/removeTag.sh'</b></li>
                                <li><b>Usage: '$ ./removeTag.sh [flags]'</b></li>
                                    <ul> 
                                        <li><b>-h</b> Print help message and exit;</li>
                                        <li><b>-y</b> Host server name;</li>
                                        <li><b>-t</b> User's token (must has Admin Permissions);</li>
                                        <li><b>-n</b> Tag's title;</li>
                                    </ul>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with Text success;</li>
                                        <li><b>Fail</b>:
                                             <ul> 
                                                 <li><b>If User's token includes required permissions:</b> JSON <b>TextResponse</b> object with Text success;</li>
                                                 <li><b>If User's token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with Text error;</li>
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
                                                <li><b>Author</b> of <b>Post</b> ('<b>order_by_author</b>');</li>
                                                <li><b>Category's</b> title ('<b>order_by_category</b>');</li>
                                                <li>Number of <b>Post's</b> photos ('<b>order_by_photo</b>');</li>
                                            </ul>
                                        </li>
                                        <li>Search only in <b>Post's</b> text;</li>
                                        <li>Search only in <b>Post's</b> title;</li>
                                        <li>Search in <b>Post's</b> title/<b>Post's</b> text/<b>Author's</b> First name and      <b>Author's</b> Last name/<b>Category's</b> Title<b>Tag's</b> Title;
                                        </li>
                                        <li>Search with exact <b>Category's</b> id;</li>
                                        <li>Search with exact <b>Tag's</b> id;</li>
                                        <li>Search Posts with <b>Tag's</b> Id ONE of [tag_id];</li>
                                        <li>Search Posts with <b>Tag's</b> Id ALL in [tag_id];</li>
                                        <li>Search Posts by <b>Author</b> name (must contain 'first_name' and 'last_name' separated by whitespace.");
                                        </li>
                                        <li>Search Posts with exact date of creation ('created_at');</li>
                                        <li>Search Posts created later than specified date - 'created_at__gt';</li>
                                        <li>Search Posts created earlier than specified date - 'created_at__lt';</li>
                                    </ul>
                                </li>
                                <li>This method is available for all registered Users</li>
                                <li>In one request You may recieve maximum 50 records;</li>
                                <li>For recieve more than 50 records You'll need to use several requests with different offset;</li>
                                <li><b>Script: 'data/curl/posts/getPosts.sh'</b></li>
                                <li><b>Usage: '$ ./getPosts.sh [flags]'</b></li>
                                    <ul> 
                                        <li><b>-h</b> Print help message and exit;</li>
                                        <li><b>-y</b> Host server name;</li>
                                        <li><b>-p</b> Port server number;</li>
                                        <li><b>-t</b> User's token;</li>
                                        <li><b>-o</b> Offset from the first record;</li>
                                        <li><b>[-s]</b> Order: key one of ['order_by_date', 'order_by_author', 'order_by_category', 'order_by_photo'] - formated like "key=true";</li>
                                        <li><b>[-f]</b> Find string for search in Post's Title (Sting may use wildcards - for more info see Appendix A);</li>
                                        <li><b>[-e]</b> Find string for search in Post's Text (Sting may use wildcards - for more info see Appendix A);</li>
                                        <li><b>[-a]</b> Find string for search in (Post's Title and Post's Text) & (Author's First name and Author's Last name) & (Category's Title) & (Tag's Title) - the result is union of all searches (Sting may use wildcards - for more info see Appendix A);</li>
                                        <li><b>[-c]</b> Search with exact <b>Category's</b> id;</li>
                                        <li><b>[-n]</b> Search Posts with exact <b>Tag's</b> Id [tag_id] (lenght [tag_id] == 1);</li>
                                        <li><b>[-i]</b> Search Posts with <b>Tag's</b> Id ONE of [tag_id];</li>
                                        <li><b>[-k]</b> Search Posts with <b>Tag's</b> Id ALL in [tag_id];</li>
                                        <li><b>[-l]</b> Search Posts by <b>Author</b> name (must contain 'first_name' and 'last_name' separated by whitespace.");
                                        </li>
                                        <li><b>[-q]</b> Search Posts with exact date of creation ('created_at');</li>
                                        <li><b>[-g]</b> Search Posts created later than specified date - 'created_at__gt';</li>
                                        <li><b>[-j]</b> Search Posts created earlier than specified date - 'created_at__lt';</li>
                                    </ul>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>PostResponse</b> object;</li>
                                        <li><b>Fail</b>:
                                            <ul> 
                                                 <li><b>If User's token includes required permissions:</b> JSON <b>TextResponse</b> object with Text success;</li>
                                                 <li><b>If User's token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with Text error;</li>
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
                                <li><b>Script: 'data/curl/post/createPost.sh'</b></li>
                                <li><b>Usage: '$ ./createTag.sh [flags]'</b></li>
                                    <ul> 
                                        <li><b>-h</b> Print help message and exit;</li>
                                        <li><b>-y</b> Host server name;</li>
                                        <li><b>-p</b> Port server number;</li>
                                        <li><b>-t</b> User's token (must has Author Write Permissions);</li>
                                        <li><b>-n</b> Post's title;</li>
                                        <li><b>-b</b> Post's text;</li>
                                        <li><b>-c</b> Category's id;</li>
                                        <li><b>-m</b> Tag's ids;</li>
                                    </ul>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with Text success;</li>
                                        <li><b>Fail</b>:
                                            <ul> 
                                                 <li><b>If User's token includes required permissions:</b> JSON <b>TextResponse</b> object with Text success;</li>
                                                 <li><b>If User's token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with Text error;</li>
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
                                <li><b>Script: 'data/curl/post/setPostMainPhoto.sh'</b></li>
                                <li><b>Usage: '$ ./setPostMainPhoto.sh [flags]'</b></li>
                                    <ul> 
                                        <li><b>-h</b> Print help message and exit;</li>
                                        <li><b>-y</b> Host server name;</li>
                                        <li><b>-p</b> Port server number;</li>
                                        <li><b>-t</b> User's token;</li>
                                        <li><b>-i</b> Post's id</li>
                                        <li><b>-l</b> Path to photo</li>
                                    </ul>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with Text success;</li>
                                        <li><b>Fail</b>:
                                             <ul> 
                                                 <li><b>If User's token includes required permissions:</b> JSON <b>TextResponse</b> object with Text success;</li>
                                                 <li><b>If User's token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with Text error;</li>
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
                                <li><b>Script: 'data/curl/post/setAddMainPhoto.sh'</b></li>
                                <li><b>Usage: '$ ./setAddMainPhoto.sh [flags]'</b></li>
                                    <ul> 
                                        <li><b>-h</b> Print help message and exit;</li>
                                        <li><b>-y</b> Host server name;</li>
                                        <li><b>-p</b> Port server number;</li>
                                        <li><b>-t</b> User's token;</li>
                                        <li><b>-i</b> Post's id</li>
                                        <li><b>-l</b> Path to photo</li>
                                    </ul>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with Text success;</li>
                                        <li><b>Fail</b>:
                                             <ul> 
                                                 <li><b>If User's token includes required permissions:</b> JSON <b>TextResponse</b> object with Text success;</li>
                                                 <li><b>If User's token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with Text error;</li>
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
                                <li><b>Script: 'data/curl/post/removePost.sh'</b></li>
                                <li><b>Usage: '$ ./removePost.sh [flags]'</b></li>
                                    <ul> 
                                        <li><b>-h</b> Print help message and exit;</li>
                                        <li><b>-y</b> Host server name;</li>
                                        <li><b>-t</b> User's token (must has Admin Permissions);</li>
                                        <li><b>-i</b> Post's id;</li>
                                    </ul>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with Text success;</li>
                                        <li><b>Fail</b>:
                                             <ul> 
                                                 <li><b>If User's token includes required permissions:</b> JSON <b>TextResponse</b> object with Text success;</li>
                                                 <li><b>If User's token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with Text error;</li>
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
                                <li><b>Script: 'data/curl/tag/getDrafts.sh'</b></li>
                                <li><b>Usage: '$ ./getDrafts.sh [flags]'</b></li>
                                    <ul> 
                                        <li><b>-h</b> Print help message and exit;</li>
                                        <li><b>-y</b> Host server name;</li>
                                        <li><b>-p</b> Port server number;</li>
                                        <li><b>-t</b> User's token (must has Author Read Permissions);</li>
                                        <li><b>-o</b> Offset from the first record;</li>
                                    </ul>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>DraftResponse</b> object;</li>
                                        <li><b>Fail</b>:
                                            <ul> 
                                                 <li><b>If User's token includes required permissions:</b> JSON <b>TextResponse</b> object with Text success;</li>
                                                 <li><b>If User's token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with Text error;</li>
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
                                <li><b>Script: 'data/curl/draft/createDraft.sh'</b></li>
                                <li><b>Usage: '$ ./createDraft.sh [flags]'</b></li>
                                    <ul> 
                                        <li><b>-h</b> Print help message and exit;</li>
                                        <li><b>-y</b> Host server name;</li>
                                        <li><b>-p</b> Port server number;</li>
                                        <li><b>-t</b> User's token (must has Author Read Permissions);</li>
                                        <li><b>-b</b> Draft's text;</li>
                                        <li><b>-i</b> Post's id;</li>
                                    </ul>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with Text success;</li>
                                        <li><b>Fail</b>:
                                            <ul> 
                                                 <li><b>If User's token includes required permissions:</b> JSON <b>TextResponse</b> object with Text success;</li>
                                                 <li><b>If User's token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with Text error;</li>
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
                                <li><b>Script: 'data/curl/drafts/editDraft.sh'</b></li>
                                <li><b>Usage: '$ ./editDraft.sh [flags]'</b></li>
                                    <ul> 
                                        <li><b>-h</b> Print help message and exit;</li>
                                        <li><b>-y</b> Host server name;</li>
                                        <li><b>-t</b> User's token (must has Author Read Permissions);</li>
                                        <li><b>-b</b> New Draft's text;</li>
                                        <li><b>-i</b> Post's id;</li>
                                    </ul>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with Text success;</li>
                                        <li><b>Fail</b>:
                                             <ul> 
                                                 <li><b>If User's token includes required permissions:</b> JSON <b>TextResponse</b> object with Text success;</li>
                                                 <li><b>If User's token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with Text error;</li>
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
                                <li><b>Script: 'data/curl/draft/publishDraft.sh'</b></li>
                                <li><b>Usage: '$ ./publishDraft.sh [flags]'</b></li>
                                    <ul> 
                                        <li><b>-h</b> Print help message and exit;</li>
                                        <li><b>-y</b> Host server name;</li>
                                        <li><b>-t</b> User's token (must has Admin Permissions);</li>
                                        <li><b>-i</b> Post's id (corresponding to proper Draft);</li>
                                    </ul>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with Text success;</li>
                                        <li><b>Fail</b>:
                                             <ul> 
                                                 <li><b>If User's token includes required permissions:</b> JSON <b>TextResponse</b> object with Text success;</li>
                                                 <li><b>If User's token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with Text error;</li>
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
                                <li><b>Script: 'data/curl/draft/removeDraft.sh'</b></li>
                                <li><b>Usage: '$ ./removeDraft.sh [flags]'</b></li>
                                    <ul> 
                                        <li><b>-h</b> Print help message and exit;</li>
                                        <li><b>-y</b> Host server name;</li>
                                        <li><b>-t</b> User's token (must has Admin Permissions);</li>
                                        <li><b>-i</b> Post's id (corresponding to proper Draft);</li>
                                    </ul>
                                <li><b>Response:</b>
                                    <ul> 
                                        <li><b>Success</b>: JSON <b>TextResponse</b> object with Text success;</li>
                                        <li><b>Fail</b>:
                                             <ul> 
                                                 <li><b>If User's token includes required permissions:</b> JSON <b>TextResponse</b> object with Text success;</li>
                                                 <li><b>If User's token doesn't includes required permissions:</b> JSON <b>TextResponse</b> object with Text error;</li>
                                             </ul>
                                        </li>
                                    </ul>
                                </li>
                            </ul>
                        </li>
                   </ul>
                </li>
            </ul>
        </li>
    </ul>