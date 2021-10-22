# post-server
[![Haskell CI](https://github.com/LyuPo7/post-server/actions/workflows/haskell.yml/badge.svg)](https://github.com/LyuPo7/post-server/actions/workflows/haskell.yml)
<p>The REST-API is an HTTP-based interface. The response contains a JSON object</p>

<h3>Project Description:</h3>
    <ul>
        <li>Using <b>Warp</b> as Web-server;</li>
        <li>Using <b>PostgreSQL</b> as BD;</li>
        <li>The response contains a <b>JSON</b> object</li>
        <li>Supports 4 levels of logging:
            <ul>
                <li><b>error</b>-level: show only error messages;</li>
                <li><b>warning</b>-level: show only error and warning messages;</li>
                <li><b>info</b>-level(default): show info messages for every step of downloading page including error and warning messages;</li>
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
       </li>
       <li><h4>For more information see asciinema below:</h4></p>
            <a href="https://asciinema.org/a/443725" target="_blank"><img src="https://asciinema.org/a/443725.svg" /></a></b>:</h4>
           <ul><b>$ stack exec post-server-exe</b>
           </ul>
       </li>
    </ol>
    <p>
<h3>Making requests</h3>
<ul>
        <li>All supported requests contain in 'data/curl/'</li>
            <ul>
                <li>Every directory contains executables .sh scripts for supported requests</li>
                <li>For use .sh scripts for supported requests</li>
            </ul>
        <li>Supported requests</li>
            <ul>
                <li><b>Account</b>:
                    <ul>
                        <li><b>login</b>:
                            <ul>
                                <li>Use this method to get new <b>Token</b> for <b>User</b> by login;</li>
                                <li><b>Script: 'data/curl/account/login.sh'</b></li>
                                <li><b>Usage: './login.sh [flags]'</b></li>
                                    <ul> 
                                        <li><b>-h</b> Print help message and exit;</li>
                                        <li><b>-y</b> Host server name;</li>
                                        <li><b>-p</b> Port server number;</li>
                                        <li><b>-k</b> User password;</li>
                                        <li><b>-k</b> User login;</li>
                                    </ul>
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
                                <li>In one request You may recieve maximum 50 records</li>
                                <li>For recieve more than 50 records You'll need to use several requests with different offset;</li>
                                <li><b>Script: 'data/curl/user/getUsers.sh'</b></li>
                                <li><b>Usage: './getUsers.sh [flags]'</b></li>
                                    <ul> 
                                        <li><b>-h</b> Print help message and exit;</li>
                                        <li><b>-y</b> Host server name;</li>
                                        <li><b>-p</b> Port server number;</li>
                                        <li><b>-t</b> User's token;</li>
                                        <li><b>-o</b> Offset from the first record;</li>
                                    </ul>
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
                                <li><b>Script: 'data/curl/user/createUser.sh'</b></li>
                                <li><b>Usage: './createUser.sh [flags]'</b></li>
                                    <ul> 
                                        <li><b>-h</b> Print help message and exit;</li>
                                        <li><b>-y</b> Host server name;</li>
                                        <li><b>-p</b> Port server number;</li>
                                        <li><b>-f</b> User's first name;</li>
                                        <li><b>-l</b> User's last name;</li>
                                        <li><b>-k</b> User's password;</li>
                                        <li><b>-n</b> User's login;</li>
                                    </ul>
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
                                <li><b>Script: 'data/curl/user/setUserPhoto.sh'</b></li>
                                <li><b>Usage: './setUserPhoto.sh [flags]'</b></li>
                                    <ul> 
                                        <li><b>-h</b> Print help message and exit;</li>
                                        <li><b>-y</b> Host server name;</li>
                                        <li><b>-p</b> Port server number;</li>
                                        <li><b>-t</b> User's token;</li>
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
                        <li><b>removeUser</b>:
                            <ul>
                                <li>Use this method to remove <b>User</b>;</li>
                                <li>This method is available only for admins;</li>
                                <li><b>Script: 'data/curl/user/removeUser.sh'</b></li>
                                <li><b>Usage: './removeUser.sh [flags]'</b></li>
                                    <ul> 
                                        <li><b>-h</b> Print help message and exit;</li>
                                        <li><b>-y</b> Host server name;</li>
                                        <li><b>-p</b> Port server number;</li>
                                        <li><b>-t</b> User's token;</li>
                                        <li><b>-i</b> User's id (to remove)</li>
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
                <li><b>Author</b>:
                    <ul>
                        <li><b>getAuthors</b>:
                            <ul>
                                <li>Use this method to get all <b>Author</b> records;</li>
                                <li>This method is available only for admins;</li>
                                <li>In one request You may recieve maximum 50 records;</li>
                                <li>For recieve more than 50 records You'll need to use several requests with different offset;</li>
                                <li><b>Script: 'data/curl/author/getAuthors.sh'</b></li>
                                <li><b>Usage: './getAuthors.sh [flags]'</b></li>
                                    <ul> 
                                        <li><b>-h</b> Print help message and exit;</li>
                                        <li><b>-y</b> Host server name;</li>
                                        <li><b>-p</b> Port server number;</li>
                                        <li><b>-t</b> User's token (must has Admin Permissions);</li>
                                        <li><b>-o</b> Offset from the first record;</li>
                                    </ul>
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
                                <li><b>Script: 'data/curl/author/createAuthor.sh'</b></li>
                                <li><b>Usage: './createAuthor.sh [flags]'</b></li>
                                    <ul> 
                                        <li><b>-h</b> Print help message and exit;</li>
                                        <li><b>-y</b> Host server name;</li>
                                        <li><b>-p</b> Port server number;</li>
                                        <li><b>-t</b> User's token (must has Admin Permissions);</li>
                                        <li><b>-i</b> User's id;</li>
                                        <li><b>-d</b> Author's description;</li>
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
                                <li><b>Script: 'data/curl/author/editAuthor.sh'</b></li>
                                <li><b>Usage: './editAuthor.sh [flags]'</b></li>
                                    <ul> 
                                        <li><b>-h</b> Print help message and exit;</li>
                                        <li><b>-y</b> Host server name;</li>
                                        <li><b>-t</b> User's token (must has Admin Permissions);</li>
                                        <li><b>-i</b> User's id (corresponding to proper Author);</li>
                                        <li><b>-d</b> New Author's description;</li>
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
                        <li><b>removeAuthor</b>:
                            <ul>
                                <li>Use this method to remove <b>Author</b>. If <b>Author</b> has <b>Posts</b> it's imposible to remove his record. Firstly You'll need to remove all his <b>Posts</b>;</li>
                                <li>This method is available only for admins;</li>
                                <li><b>Script: 'data/curl/authors/removeAuthor.sh'</b></li>
                                <li><b>Usage: './removeAuthor.sh [flags]'</b></li>
                                    <ul> 
                                        <li><b>-h</b> Print help message and exit;</li>
                                        <li><b>-y</b> Host server name;</li>
                                        <li><b>-t</b> User's token (must has Admin Permissions);</li>
                                        <li><b>-i</b> User's id (corresponding to proper Author);</li>
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