module LabProg2019.menu

open System
open System.IO
open Engine
open Gfx
open System.Media
open LabProg2019.Prova

type Status = Menu|InGame|Victory|ShowSolution
type ButtonAction = StartGame|Quit

[< NoEquality; NoComparison >]
type state = {
    player: sprite
    indicatore : sprite
    mutable status: Status
}

type Button (etichetta:string, codice:ButtonAction) =
    let mutable y = 0
    member this.etichetta = etichetta
    member this.codice:ButtonAction = codice
    member this.Y with get() = y and set(value) = y <- value


let init ()  =
            
            

            let W = 60
            let H = 30
            let startPosition: Position = new Position (0,1)
            let endPosition: Position = new Position ((W/2)-1,H-2)
            let sameDirectionMin = 2
            let sameDirectionMax = 4


            let arr_options:Button list = [ new Button ("Play!", ButtonAction.StartGame);
                                             new Button ("Exit!", ButtonAction.Quit)
                                          ]
            
            let engine = new engine (W, H)

            let intro_game = new SoundPlayer("..\..\Game_sounds\intro.wav")
            intro_game.Load()
            intro_game.PlayLooping()


            let menu_sound = new SoundPlayer("..\..\Game_sounds\misc_menu.wav")
            menu_sound.Load()

            let winning = new SoundPlayer("..\..\Game_sounds\Victory.wav")
            winning.Load()

            let game_sound = new SoundPlayer("..\..\Game_sounds\ingame.wav")
            game_sound.Load()


            Console.ForegroundColor <- ConsoleColor.Green

            printfn "



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
            ignore(Console.ReadKey())
            
            intro_game.Stop()

            Console.Clear()
            let enter = char (13)
            let mutable mazeString = ""
            let mutable MyMaze: Maze option = None
            let player = engine.create_and_register_sprite (image.rectangle (2,1, pixel.create('\219', Color.Cyan)), startPosition.X*2, startPosition.Y, 2)
            player.clear
           //player.draw_rectangle(1,1, pixel.create('*', Color.Cyan)
            let mutable mazeSolution: MazeCell list = []
            let mutable spriteSolution: sprite list = []

            let myLoop (keyo : ConsoleKeyInfo option) (screen : wronly_raster) (st : state) =
                    if (st.status = Status.Menu) then 
                                        
                                        
                                let mutable wantToQuit = false

                                //screen.draw_text("",- ,1, Color.Green)

                                screen.draw_text("
                                \219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219
                                \219\219    | `-.  | `.  -_-_ _-_  _-  _- -_ -  .'|   |.'|     \219\219
                                \219\219._  |    |`!  |`.  -_ -__ -_ _- _-_-  .'  |.;'   |   _.\219\219
                                \219\219| `-!._  |  `;!  ;. _______________ ,'| .-' |   _!.i'  \219\219
                                \219\219|     |`-!._ | `.| |_______________||.''|  _!.;'   |   \219\219
                                \219\219'..__ |    |`';.| i|_|           |_|'| _!-|   |   _|..-\219\219
                                \219\219    |``--..|_ | `;!|i|           |i|.'j   |_..!-'|     \219\219
                                \219\219    |    |   |`-,!_|_|           |_||.!-;'  |    |     \219\219
                                \219\219____|____!.,.!,.!,!|i|  -------  |i|,!,.!.,.!..__|_____\219\219
                                \219\219|     |    |  |  | |_| |       | |_|| |   |   |    |   \219\219
                                \219\219|     |    |..!-;'i|i|  -------  |i| |`-..|   |    |   \219\219
                                \219\219|    _!.-j'  | _!,'|_|  -------  |_||!._|  `i-!.._ |   \219\219
                                \219\219!.-'|    | _.'|  !;|i| |       | |i|`.| `-._|    |``-..\219\219
                                \219\219    |  _.''|  !-| !|_|  -------  |_|.|`-. | ``._ |     \219\219
                                \219\219    |.|    |.|  !| |i|           |i||`. |`!   | `'.    \219\219
                                \219\219_.-'  |  .'  |.' |/|_|           |_|! |`!  `,.|    |-._\219\219
                                \219\219|     !.'|  .'| .'|[@]___________[@] \|  `. | `._  |   \219\219
                                \219\219|   .'   |.|  |/| /                 \|`.  |`!    |.|   \219\219
                                \219\219|_.'|   .' | .' |/                   \  \ |  `.  | `._-\219\219
                                \219\219'   | .'   |/|  /                     \ |`!   |`.|    `\219\219
                                \219\219    !'|   .' | /                       \|  `  |  `.    \219\219
                                \219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219
                                ", -31, 4, Color.Green)

                                for i=0 to arr_options.Length - 1 do
                                    arr_options.[i].Y <- 14 + 3 * i
                                    screen.draw_text(arr_options.[i].etichetta, 28, arr_options.[i].Y, Color.White)
                                
                                
                                let dx, dy =
                                    match keyo with
                                    None -> 0. ,0.
                                    |Some key -> menu_sound.Play()
                                                 match key.KeyChar with 
                                                      'w' -> 0., -3.
                                                    | 's' -> 0., 3.
                                                    | _ when key.KeyChar = enter -> let clickedButton: Button option = List.tryFind (fun (button:Button)-> button.Y = int(st.indicatore.y)) arr_options
                                                                                    in match clickedButton with
                                                                                         None -> ignore()
                                                                                        |Some button -> match button.codice with
                                                                                                        ButtonAction.StartGame -> st.status <- Status.InGame
                                                                                                                                  let (MazeStr, myMaze) = Prova.main(W / 2, H, startPosition, endPosition, sameDirectionMin, sameDirectionMax)
                                                                                                                                  mazeString <- MazeStr
                                                                                                                                  MyMaze <- Some(myMaze)
                                                                                                                                  st.player.drawSprite (pixel.create ('\219',Color.Cyan))
                                                                                                                                  //resetto la posizione a quella di partenza
                                                                                                                                  st.player.x <- (float(startPosition.X*2))
                                                                                                                                  st.player.y <- float(startPosition.Y)
                                                                                                                                  st.indicatore.clear
                                                                                                                                  game_sound.PlayLooping()

                                                                                                        |ButtonAction.Quit -> Environment.Exit(0)
                                                                                                                              wantToQuit <- true
                                                                                    0., 0.
                                                    | _   -> 0., 0.
                                st.indicatore.move_by(dx, dy)

                                st.indicatore.x <- 25.0

                                if (st.indicatore.y < 14.0) then st.indicatore.y <- 14.0
                                    else if (st.indicatore.y > float ( 14+(3*(arr_options.Length - 1)) ) ) 
                                            then st.indicatore.y <- float ( 14+(3*(arr_options.Length - 1)) )
                                st, wantToQuit

                    elif st.status = Status.InGame then
                         screen.draw_text(mazeString, 0, 0, Color.DarkGray)
                         //partenza
                         screen.draw_text("\219\219", startPosition.X*2, startPosition.Y, Color.DarkGreen)
                         //arrivo
                         screen.draw_text("\219\219", endPosition.X*2, endPosition.Y, Color.DarkRed)
                         //movimento giocatore da fare

                         //ignore(engine.show_sprites )
                         
                         let dx, dy =
                             match keyo with
                             None -> 0., 0.
                             |Some key -> 
                                          match key.KeyChar with 
                                               'w' -> 0., -1.
                                             | 'a' -> -2.,0.
                                             | 's' -> 0., 1.
                                             | 'd' -> 2.,0.
                                             | 'e' -> st.status <- Status.ShowSolution
                                                      st.player.clear
                                                      0.,0.
                                             | 'q' -> st.status <- Status.Menu
                                                      st.player.clear

                                                      st.indicatore.drawSprite(pixel.create('>', Color.White)) //flood_fill(0, 0, pixel.create('>', Color.Green))
                                                      0., 0.
                                             | _   -> 0., 0.
                         let nextPosition: Position = new Position(int(st.player.x + dx) / 2, int(st.player.y + dy))
                         let nextCell: MazeCell = MyMaze.Value.getCell(nextPosition)
                         if not(nextCell.isWall) then st.player.move_by(dx, dy)
                         if(nextCell.position.X = endPosition.X && nextCell.position.Y = endPosition.Y) then winning.Play()
                                                                                                             st.status <- Status.Victory
                         st, false
                    elif st.status = Status.Victory then 
                                                         
                                                         st.player.clear
                                                         
                                                         screen.draw_text ("HAI VINTO!!",15,15,Color.Green)
                                                         screen.draw_text ("Premi un tasto per tornare al menu'",15,20,Color.Green)
                                                         

                                                         if(keyo.IsSome) then winning.Stop()
                                                                              st.status <- Status.Menu
                                                                              st.indicatore.drawSprite(pixel.create('>', Color.White))
                                                         st,false
                    elif st.status = Status.ShowSolution then 
                        screen.draw_text(mazeString, 0, 0, Color.DarkGray)
                        if mazeSolution.Length = 0 then
                                                        mazeSolution <- MyMaze.Value.findSolution()
                                                        spriteSolution <- []
                                                        List.iter (fun (cell:MazeCell) -> (spriteSolution <- (engine.create_and_register_sprite (image.rectangle(2, 1, pixel.create('\219', Color.DarkCyan)), cell.position.X * 2, cell.position.Y, 1)) :: spriteSolution)) mazeSolution
                        
                        if(keyo.IsSome) then st.status <- Status.Menu
                                             st.indicatore.drawSprite(pixel.create('>', Color.White))
                                             List.iter (fun (mySprite:sprite) -> (*mySprite.clear*) engine.removeSprite(mySprite)) spriteSolution
                                             mazeSolution <- []
                        st, false
                    
                    else st, false

            let freccia = engine.create_and_register_sprite (image.rectangle (1,1,pixel.create('>', Color.White)), 2, 3, 1)

            // initialize state
            let st0 = {
                    player = player
                    indicatore = freccia
                    status = Status.Menu
                    }
            engine.show_fps <- false
            // start engine
            //engine.loop_on_key my_update st0
            engine.loop myLoop st0
                 
