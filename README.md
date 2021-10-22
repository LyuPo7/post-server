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
                <li>Every directory contains executables .sh scripts for supported requests;</li>
                <li>For use .sh scripts for supported requests;</li>
            </ul>
        <li>Supported requests</li>
            <ul>
                <li><b>Account</b>:
                    <ul>
                        <li><b>login</b>:
                            <ul>
                                <li>Use this method to get new <b>Token</b> for <b>User</b> by login;</li>
                                <li><b>Script: 'data/curl/account/login.sh'</b></li>
                                <li><b>Usage: '$ ./login.sh [flags]'</b></li>
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
                                <li>In one request You may recieve maximum 50 records;</li>
                                <li>For recieve more than 50 records You'll need to use several requests with different offset;</li>
                                <li><b>Script: 'data/curl/user/getUsers.sh'</b></li>
                                <li><b>Usage: '$ ./getUsers.sh [flags]'</b></li>
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
                                <li><b>Usage: '$ ./createUser.sh [flags]'</b></li>
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
                                <li><b>Usage: '$ ./setUserPhoto.sh [flags]'</b></li>
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
                                <li><b>Usage: '$ ./removeUser.sh [flags]'</b></li>
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
                                <li><b>Usage: '$ ./getAuthors.sh [flags]'</b></li>
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
                                <li><b>Usage: '$ ./createAuthor.sh [flags]'</b></li>
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
                                <li><b>Usage: '$ ./editAuthor.sh [flags]'</b></li>
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
                                <li><b>Usage: '$ ./removeAuthor.sh [flags]'</b></li>
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
                <li><b>Category</b>:
                    <ul>
                        <li><b>getCategories</b>:
                            <ul>
                                <li>Use this method to get all <b>Category</b> records;</li>
                                <li>This method is available for all registered Users</li>
                                <li>In one request You may recieve maximum 50 records;</li>
                                <li>For recieve more than 50 records You'll need to use several requests with different offset;</li>
                                <li><b>Script: 'data/curl/category/getCategories.sh'</b></li>
                                <li><b>Usage: '$ ./getCategories.sh [flags]'</b></li>
                                    <ul> 
                                        <li><b>-h</b> Print help message and exit;</li>
                                        <li><b>-y</b> Host server name;</li>
                                        <li><b>-p</b> Port server number;</li>
                                        <li><b>-t</b> User's token;</li>
                                        <li><b>-o</b> Offset from the first record;</li>
                                    </ul>
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
                                <li>Use this method to create new <b>Category</b>. Category's title and SubCategory's title can't be the same;</li>
                                <li>This method is available only for admins;</li>
                                <li><b>Script: 'data/curl/category/createCategory.sh'</b></li>
                                <li><b>Usage: '$ ./createCategory.sh [flags]'</b></li>
                                    <ul> 
                                        <li><b>-h</b> Print help message and exit;</li>
                                        <li><b>-y</b> Host server name;</li>
                                        <li><b>-p</b> Port server number;</li>
                                        <li><b>-t</b> User's token (must has Admin Permissions);</li>
                                        <li><b>-i</b> Category's title;</li>
                                        <li><b>[-s]</b> SubCategory's title (if Category expects to have one);</li>
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
                        <li><b>editCategory</b>:
                            <ul>
                                <li>Use this method to edit <b>Category's</b> title, <b>Category's</b> subCategory or both. Category's title and SubCategory's title can't be the same. The subCategory title using for request already must exists. You can't use for <b>Category</b> title title what already exists;</li>
                                <li>This method is available only for admins;</li>
                                <li><b>Script: 'data/curl/category/editCategory.sh'</b></li>
                                <li><b>Usage: '$ ./editCategory.sh [flags]'</b></li>
                                    <ul> 
                                        <li><b>-h</b> Print help message and exit;</li>
                                        <li><b>-y</b> Host server name;</li>
                                        <li><b>-t</b> User's token (must has Admin Permissions);</li>
                                        <li><b>-i</b> Category's title;</li>
                                        <li><b>[-s]</b> SubCategory's title (if You want to change/set SubCategory of Category);</li>
                                        <li><b>[-s]</b> Category's title (if You want to change Category's title);</li>
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