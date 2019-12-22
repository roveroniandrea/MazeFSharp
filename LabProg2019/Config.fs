(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Config.fs: static configuration
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Config

open Prelude

let filled_pixel_char = '*' //'\u2588'    // ascii '\219' for character '█'
let wall_pixel_char = '|' //'\u2588'    // ascii '\219' for character '█'
let empty_pixel_char = ' '

let default_flip_queue = 2  // double buffering
let default_fps_cap = 30

let log_pipe_name = "LogPipe"
let log_pipe_translate_eol = '\255'

let game_console_title = "Game Window"
let log_console_title = "Log Window"


let log_msg_color = Color.Gray
let log_warn_color = Color.Yellow
let log_error_color = Color.Red
let log_debug_color = Color.Cyan

let startingScreenLogo = "



                                                            ████  ████████████████████████
                                                            ██                          ██
                                                            ██  ██████████  ██████████  ██
                                                            ██  ██  ██  ██  ██      ██  ██
                                                            ██  ██  ██  ██  ██████████  ██
                                                            ██  ██  ██  ██  ██      ██  ██
                                                            ██  ██  ██  ██  ██      ██  ██
                                                            ██                          ██
                                                            ██  ██████████  ██████████  ██
                                                            ██          ██  ██          ██
                                                            ██  ██████████  ██████████  ██
                                                            ██  ██          ██          ██
                                                            ██  ██████████  ██████████  ██
                                                            ██                          ██
                                                            ██████████████████  ██████████


                                                    By:       Checchin       Fasolato      Roveroni






                                                                Press any key to start...
            "

let mutable menuScreen = "
    \219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219
    \219\219    | `-.  | `.  -_-_ _-_  _-  _- -_ -  .'|   |.'|     \219\219
    \219\219._  |    |`!  |`.  -_ -__ -_ _- _-_-  .'  |.;'   |   _.\219\219
    \219\219| `-!._  |  `;!  ;. _______________ ,'| .-' |   _!.i'  \219\219
    \219\219|     |`-!._ | `.| [@]           [@]|.''|  _!.;'   |   \219\219
    \219\219'..__ |    |`';.| i|_|  -------  |_|'| _!-|   |   _|..-\219\219
    \219\219    |``--..|_ | `;!|i| |       | |i|.'j   |_..!-'|     \219\219
    \219\219    |    |   |`-,!_|_|  -------  |_||.!-;'  |    |     \219\219
    \219\219____|____!.,.!,.!,!|i|           |i|,!,.!.,.!..__|_____\219\219
    \219\219|     |    |  |  | |_|  -------  |_|| |   |   |    |   \219\219
    \219\219|     |    |..!-;'i|i| |       | |i| |`-..|   |    |   \219\219
    \219\219|    _!.-j'  | _!,'|_|  -------  |_||!._|  `i-!.._ |   \219\219
    \219\219!.-'|    | _.'|  !;|i|           |i|`.| `-._|    |``-..\219\219
    \219\219    |  _.''|  !-| !|_|  -------  |_|.|`-. | ``._ |     \219\219
    \219\219    |.|    |.|  !| |i| |       | |i||`. |`!   | `'.    \219\219
    \219\219_.-'  |  .'  |.' |/|_|  -------  |_|! |`!  `,.|    |-._\219\219
    \219\219|     !.'|  .'| .'|[@]___________[@] \|  `. | `._  |   \219\219
    \219\219|   .'   |.|  |/| /                 \|`.  |`!    |.|   \219\219
    \219\219|_.'|   .' | .' |/                   \  \ |  `.  | `._-\219\219
    \219\219'   | .'   |/|  /                     \ |`!   |`.|    `\219\219
    \219\219    !'|   .' | /                       \|  `  |  `.    \219\219
    \219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219
"

let mutable instructionMenu = "
                        \219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219
                        \219\219                                                       \219\219
                        \219\219                      Menu' Tasti                      \219\219
                        \219\219                                                       \219\219
                        \219\219              -------                                  \219\219
                        \219\219             |   W   |        Tasti per                \219\219
                        \219\219             | A S D |        i movimenti              \219\219
                        \219\219              -------                                  \219\219
                        \219\219              -------                                  \219\219
                        \219\219             | Enter |        Invio                    \219\219
                        \219\219              -------                                  \219\219
                        \219\219              -------                                  \219\219
                        \219\219             |   Q   |        Esci                     \219\219
                        \219\219              -------                                  \219\219
                        \219\219              -------                                  \219\219
                        \219\219             |   E   |        Vis. soluzione           \219\219
                        \219\219              -------                                  \219\219
                        \219\219                                                       \219\219
                        \219\219                                                       \219\219
                        \219\219               Premi un tasto per uscire...            \219\219
                        \219\219                                                       \219\219
                        \219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219
                        "
let mutable victory = "
    
                                                                                .   ..                                                                
                                              ... ...   .....   ....... .  ... .'..;c'  ....'.                                                        
     .................                       ...',;;;;,'.,::,;;,';;;c;,',,';:,.;:.:xo;':ccll:......                                           ........
   ...',,;;:ccccloloooolc:,..             .....,:lodxxxxoodkkxkOkkkxkkdodxdxOd:odlOKkodOO00xlcc:,.                           ..',,,,,;;;;::ccc:;;,... 
   ....,;:lodxkO00O00OO0000Okdc,..         ..';:oxO0KXXK0000KK0KXXKK000O000KK0kOOOKKOOKXKKKKOOdc'....                  ..;codxkkkkOOO0000Okxdl:;,'... 
 ....,;::clodxk0Kd,............cO0xc,.     ...,cok0XNNx'..................................:0X0xc:;,'........   . ..';ld:.'''''''',,,,l0NK0kdlc;,'..   
     ...,:loxOKXNK:           .oNWWWN0kdl:;,,,:lok0XNO.                                    oNKOkdolcclcc;,,''',,;lx0XNNk.           .dKKOxoc;,...     
           ..,:ldkd;,,,,'.    ,0WWWWWWWWWNXKK0O00KXNXo'...'..''''.....     .......'''''....cKNXXKXXKK0Okxxxdxxk0KXNNWWWWd.    .,,,,'cxdlc;'..         
        .......',;codxOKKl    .OMWWMWWMW0ollcxKxclclONNX0xoc:::cokKXK:    :0KXXKkdllllodOXNN0ollcclccox0XXx:clcdXWWWWNWNo.   'ONXKOxdoc;,'..          
          ....',;:lodxOOKO,    ;KWWWWWM0,   .d0,   .xMXd'        .:0Nc    lNNNO:.       .oXMd.         .;xd.   .xWWMWWWk.   .dXK0kdol:,'..            
              ..',:coxO0KXx.    cXWWWWNc    cXK,   .xXc    ..'.    cXc    lXNk'    ...   .oNx.           .dd.   .OMMWM0,    c00kxdlc:;,'....          
              ...',;cldkOKKl    .dWWMWd.   '0MK,   .xO.   .xXN0l;:;l0c    :00:    ;KN0,   .Od.   .oOOd'   ,Ol    ,KWWNl    ;0NKK0Oxdoc:;,'....        
              ...';:loxkOKXK:    '0MWO'   .kWWK,   .xk.   cNWNNNWWWWNc    ;0k,   .xWWWo    dd.   .OWWO'   .OK;    :XWd.   .xKOkxol:;,'....            
              ....,;:ldxO0KNO.    :XK;    oWWWK,   .xd    oNNXXXXXNWNc    ;Ox'   'OWWMx.   ld.   .ox:.    lNW0,    lk'   .oK0Oxoc;,'...               
                ...';:loxO0XXd.   .lc    :KMWWK,   .xd   .xWWNNNNNNWNc    ;Od.   '0WWMx.   co.          'xNMMWk.    .    cKXK0kxoc:,'...              
              ...';:codxO0KXNXl         .kWMMMK,   .xd.   oWWNXKKKXXXc    ;0x'   .OWWWd    od.         .OWWWWWWd.       ,0NKKOkxol:;,'...             
               ...',;codk0KXNWK;        cNMWMMK,   .xk.   .dK0o,...,xc    ;OO;    cKXk'   .kd    .l;    lXNNWWWXc      .xXK00kxolc:,'..               
               ...';:codkO0KKXNk.      'OMMWWMK,   .xX:     ..     :Kc    ;OKd.    ...   .lNd    ;XO'   .xWWWWWWX;     lXXK0kxdlc:;'....              
               ......',:cldxk0XXo.    .xWWWWWWK,   .dWXo.        .c0Nc    ,d0Nk,        ,xXWd    ;XWo.   .OWMWMMK;    :0K0Okxdl:;,...                 
                    ...,;coxkO0XK:   .dNNNWNWWKdcclcdXNWKxoc;:::;lONNkodollldOXXOocccldkKK0KkccccxXN0oloooOWWWMWo.   ,ONK0Oxdl:;,'....                
                    ...',:loxk0KXk'  lXWNX0kxollclodkO000XX0kd:''lxkxkK0kxc.':x0XXKKNNXK0OdlodkOOO0kxxxxk0XNWWMWk.  .xXXXK0kdl:;,...                  
                     ..',:codxkO0Ko.;0Kkl,......'';:;:cloc:'.   .,,',cl::;.  .,coxocdxdxl:c;..';:::;;;'...,:oOXWWx. cOOkkxdol::;;'....                
                     ....',:cdxO0KO:;:.      .... ....'..       .  .......     ..''..,,':,...    ... ..      .'cxOocOOxoc:;,''.....                   
                       ..';:loooc;.                                                   . .,'                      .,lxOOxl:'..                         
                      ....''...                                                           ..                         .,:c:;,..                        
                                                                                                                         ......                       
    

"


let mutable lose = "         
                            ............                   ...               ...                ...     .................                          
                       ....              ....         ....     ....          ........      .........   .....                                       
                      .....             .....         ....     ....          .........    ..........   .....                                       
                      .'''.              ....     ........     ........     ..'''...............'''.   .'''..............                          
                      .;;;.                     .';,,.             .;,;'..  .,;;,.   .',;,.    ';;;.   .;;;;,,;;;;;;;;,;;.                         
                      ';;;'                     .,;;;'            .';;:,..  .;;;,.   .,;;,.    ,:;;.   .;;;;,,,,,,,,,,,,,.                         
                      ;oll;        .;ccccccc:.  .:ollcccc::cccc::ccclll:'.  .lllc.   .,;;,.   .:lll,   ,lll:.                                      
                      ;ooo:        .cdooooool.  .coooooooooooooooooooooc,.  'lool.   .,;;,.   .:ooo;   ;ooo:.                                      
                     .cxxxc.        .'''cxxxd'  .oxxxc'''''.''''.',lxxxl;.  ,dxxd'    ....    .lxxx:   :xxxc.                                      
                     .lOkOl.            ;kkOx,  .dkkk:             cOkko;.  ;xOkd'            .oOkOc  .ckkOl.                                      
                      ,lclc:::::::::::::d000k,  'x000c             l000x:.  ;O00k,            .xK0Kl  .l00Kkc::::::::;;::.                         
                          ;KMMMMMWMMMMMWWWWWK;  'OWWWo             oWWWOl'  :NWWK;            .OWWWd  .dWWMWWMMMMMMMMWMMMd.                        
                          ,kKKKKKKKKKKKKKKKKO,  .xKKKc             lKKKd:.  ;OKKO,            .dKKKl   lKKKKKKKKKKKKKKKKKl                         
                           ..................    ....               ....     ....              ....     .................                          
                                                                                                                                                   
                                                                                                                                                   
                                                                                                                                                   
                                                                                                                                                   
                                                                                                                                                   
                            ............          ...               ...       ................     .................                               
                                                 ....               ....     ....                  ....                                            
                      .....              ....    .....             ......   .....                  ....              .....                         
                      .'.'.             .....    .'...             ......   ......                .....              .....                         
                      .,,,.             .,,,,.   ',,,.             .,,,'.   .,,,,,''''''''''',.   .,,,,.             ',,,.                         
                      .;;;'             .,;;,.  .';;;.             .;;;'.   .,;;;;,;;;;;;;;;;;'.  .,;;,.             ';;;.                         
                      ,ccc,             .ccc:.  .;ccc'             ,ccc;'.  .cccc'.............   .cccc;,,,,,,,,,,,,,.....                         
                      ;ooo:             'oool.  .cooo,             ,oooc'.  'oool.                'looooooooooooooodc.                             
                      :ddd:.            'dddo'  .,;;;,,,,'    .',,,;;;;'.   ,dddo.                ,odddc;;;;;;;;;;;;;,,''.                         
                     .lkkkl.            ;xkkx,       ;kkOd'   'xkkx;        ;kkkx'                ,xkOx,            .oOkkc.                        
                     .:dddc'............:dddo'       ,dddo,...;dddo'        ;kOOk:.............   ;kOOx,            .dOOOc                         
                       ...:OKKKKKKKKKKKKo....         ...'dKKKd....         :XNNNXKKKKKKKKKKKKx.  :XNNK;            .kNNNo                         
                          ;KWMMMMMMMMWWWd                .kWWWx.            cNWWWWWWMMMMMMMMWWO'  cNWWX;            .OWWWo                         
                          .,;;;;;;;;;;;;.                 ';;;.             .;;;;;;;;;;;;;;;;;'   .;;;,.             ';;;.                         
                                                                                                                                                  "
