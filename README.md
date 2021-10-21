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
                        <li><b>login</b>:</li>
                            <ul>
                                <li>Use this method to get new <b>Token</b> for <b>User</b> by login;</li>
                                <li><b>Script: 'data/curl/account/login.sh'</b>:</li>
                                <li><b>Usage: './login.sh [flags]'</b>:</li>
                                    <ul> 
                                        <li><b>-h</b> Print help message and exit;</li>
                                        <li><b>-y</b> Host server name;</li>
                                        <li><b>-p</b> Port server number;</li>
                                        <li><b>-k</b> User password;</li>
                                        <li><b>-k</b> User login;</li>
                                    </ul>
                                <li><b>Response</b>: JSON TextResponse object;</li>
                            </ul>
                   </ul>
                </li>
            </ul>
        </li>
    </ul>