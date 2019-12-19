module LabProg2019.menu

open System
open Engine
open Gfx
open System.Media
open LabProg2019.MazeGenerator

type Status = Menu|InGame|Victory|ShowSolution|MenuTasti|SelectMode
type ButtonAction = StartGame|Quit|MenuTasti|Arcade|Blind
type Mode = Arcade|Blind

[< NoEquality; NoComparison >]
type state = {
    player: sprite
    indicatore : sprite
    maze : sprite
    mutable mode: Mode
    mutable status: Status
}

type Button (etichetta:string, actionCode:ButtonAction) =
    let mutable y = 0.
    member this.etichetta = etichetta
    member this.actionCode:ButtonAction = actionCode
    member this.Y with get() = y and set(value) = y <- value

let distanceBetweenPoints (x1:int, y1:int, x2:int, y2:int) =
    sqrt (float(((pown (x1 - x2) 2) + pown (y1 - y2) 2)))

let returnToMenu (st:state) (screen:wronly_raster) =
    st.player.clear
    st.maze.clear
    st.status <- Status.Menu
    st.indicatore.drawSprite(pixel.create('>', Color.White))

let drawButtons (buttonList: Button list) (menuYstart:float) (menuYIncrease:float) (screen:wronly_raster)=
    for i=0 to buttonList.Length - 1 do
        buttonList.[i].Y <- menuYstart + menuYIncrease * float(i)
        screen.draw_text(buttonList.[i].etichetta, 70, int(buttonList.[i].Y), Color.White)

let executeIfButtonPressed (buttonList: Button list) (menuYstart:float) (menuYIncrease:float) (menu_sound: SoundPlayer) (keyo:ConsoleKeyInfo option) (st:state) (matchPressed: ButtonAction -> unit)=
    let enter = char (13)
    let dx, dy =
        match keyo with
        None -> 0. ,0.
        |Some key -> menu_sound.Play()
                     match key.KeyChar with 
                          'w' -> 0., -4.
                        | 's' -> 0., 4.
                        | _ when key.KeyChar = enter -> let clickedButton: Button option = List.tryFind (fun (button:Button)-> button.Y = st.indicatore.y) buttonList
                                                        in match clickedButton with
                                                             None -> ignore()
                                                            |Some button -> matchPressed button.actionCode
                                                        0., 0.
                        | _   -> 0., 0.
    st.indicatore.move_by(dx, dy)

    st.indicatore.x <- 67.0

    if (st.indicatore.y < 11.) then st.indicatore.y <- menuYstart + menuYIncrease * float(buttonList.Length - 1)
        else if st.indicatore.y > menuYstart + menuYIncrease * float(buttonList.Length - 1)
                then st.indicatore.y <- 11.

let init ()  =
            let W = 150
            let H = 35
            let startPosition: Vector = new Vector (0,1)
            let endPosition: Vector = new Vector ((W/2)-1,H-2)
            let sameDirectionMin = 2
            let sameDirectionMax = 2

            let menuYstart = 11.
            let menuYIncrease = 4.


            let arr_options:Button list = [ new Button ("Play!", ButtonAction.StartGame);
                                            new Button ("Tasti", ButtonAction.MenuTasti);
                                            new Button ("Exit!", ButtonAction.Quit)
                                          ]

            let modeButtons:Button list = [ new Button ("Easy!", ButtonAction.Arcade);
                                            new Button ("Blind", ButtonAction.Blind)
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

            printfn "%s" Config.startingScreenLogo

            ignore(Console.ReadKey())
            
            intro_game.Stop()

            Console.Clear()

            Config.instructionMenu <- String.map (fun (ch:char)-> if ch = '\r' then char(0) else ch) Config.instructionMenu
            Config.menuScreen <- String.map (fun (ch:char)-> if ch = '\r' then char(0) else ch) Config.menuScreen
            Config.victory <- String.map (fun (ch:char)-> if ch = '\r' then char(0) else ch) Config.victory

            let mutable myMaze: Maze option = None
            let player = engine.create_and_register_sprite (image.rectangle (2,1, pixel.create('\219', Color.Cyan)), startPosition.X*2, startPosition.Y, 2)
            player.clear

            let mutable mazeSolution: MazeCell list = []
            let mutable spriteSolution: sprite list = []

            let myLoop (keyo : ConsoleKeyInfo option) (screen : wronly_raster) (st : state) =
                    if (st.status = Status.Menu) then                                
                                screen.draw_text(Config.menuScreen, 39, 4, Color.Green)

                                drawButtons arr_options menuYstart menuYIncrease screen
                                
                                executeIfButtonPressed arr_options menuYstart menuYIncrease menu_sound keyo st (fun (buttonAction:ButtonAction) ->
                                    match buttonAction with
                                        ButtonAction.StartGame -> st.status <- Status.SelectMode
                                        |ButtonAction.Quit -> Environment.Exit(0)
                                        |ButtonAction.MenuTasti -> st.status <- Status.MenuTasti
                                        |_ -> ignore())
                                st, false

                    elif st.status = Status.InGame then
                         if(st.mode = Mode.Blind) then 
                             st.maze.clear
                             List.iter (fun (cell:MazeCell) -> if ((cell.isWall && distanceBetweenPoints (cell.position.X * 2, cell.position.Y, int(st.player.x), int(st.player.y)) <= 7.)) then st.maze.draw_line(cell.position.X * 2, cell.position.Y, cell.position.X * 2 + 1, cell.position.Y, pixel.create('\219', Color.DarkGray))) myMaze.Value.maze 
                             if (distanceBetweenPoints (endPosition.X * 2, endPosition.Y, int(st.player.x), int(st.player.y))<=7.) then screen.draw_text("\219\219", endPosition.X*2, endPosition.Y, Color.DarkRed)
                             if (distanceBetweenPoints (startPosition.X * 2, startPosition.Y, int(st.player.x), int(st.player.y))<=7.) then screen.draw_text("\219\219", startPosition.X*2, startPosition.Y, Color.Green)
                         else
                             List.iter (fun (cell:MazeCell) -> if cell.isWall then st.maze.draw_line(cell.position.X * 2, cell.position.Y, cell.position.X * 2 + 1, cell.position.Y, pixel.create('\219', Color.DarkGray))) myMaze.Value.maze
                             //partenza
                             screen.draw_text("\219\219", startPosition.X*2, startPosition.Y, Color.DarkGreen)
                             //arrivo
                             screen.draw_text("\219\219", endPosition.X*2, endPosition.Y, Color.DarkRed)
                         
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
                                             | 'q' -> returnToMenu st screen
                                                      0., 0.
                                             | _   -> 0., 0.
                         let nextPosition: Vector = new Vector(int(st.player.x + dx) / 2, int(st.player.y + dy))
                         let nextCell: MazeCell = myMaze.Value.getCell(nextPosition)
                         if not(nextCell.isWall) then st.player.move_by(dx, dy)
                         if(nextCell.position.X = endPosition.X && nextCell.position.Y = endPosition.Y) then //winning.Play()
                                                                                                             st.status <- Status.Victory
                         st, false
                    
                    elif st.status = Status.Victory then screen.draw_text(Config.victory, 0, 0, Color.Green)
                                                         

                                                         if(keyo.IsSome) then winning.Stop()
                                                                              returnToMenu st screen
                                                         st,false
                    
                    elif st.status = Status.ShowSolution then
                        if st.mode = Mode.Blind then List.iter (fun (cell:MazeCell) ->
                            if cell.isWall then st.maze.draw_line(cell.position.X * 2, cell.position.Y, cell.position.X * 2 + 1, cell.position.Y, pixel.create('\219', Color.DarkGray))) myMaze.Value.maze

                        if mazeSolution.Length = 0 then let startCell = myMaze.Value.getCell(startPosition)
                                                        startCell.weight <- 0
                                                        mazeSolution <- myMaze.Value.weightedSolution([startCell])
                                                        spriteSolution <- []                        
                                                        List.iter (fun (cell:MazeCell) -> (spriteSolution <- (engine.create_and_register_sprite (image.rectangle(2, 1, pixel.create('\219', Color.DarkCyan)), cell.position.X * 2, cell.position.Y, 1)) :: spriteSolution)) mazeSolution
                        
                        if(keyo.IsSome) then returnToMenu st screen
                                             List.iter (fun (mySprite:sprite) -> engine.removeSprite(mySprite)) spriteSolution
                                             mazeSolution <- []
                        st, false
                    elif st.status = Status.MenuTasti then 
                        st.indicatore.clear
                        
                        screen.draw_text(Config.instructionMenu, 19, 4, Color.Green)

                        if(keyo.IsSome) then winning.Stop()
                                             returnToMenu st screen
                        st, false

                    elif st.status = Status.SelectMode then 
                        
                        screen.draw_text(Config.menuScreen, 39, 4, Color.Green)

                        drawButtons modeButtons menuYstart menuYIncrease screen

                        executeIfButtonPressed modeButtons menuYstart menuYIncrease menu_sound keyo st (fun (buttonAction:ButtonAction) ->
                            match buttonAction with
                                ButtonAction.Arcade -> st.mode <- Mode.Arcade
                                                       st.status <- Status.InGame                                              
                                |ButtonAction.Blind -> st.mode <- Mode.Blind
                                                       st.status <- Status.InGame                                                                                                
                                |_ -> ignore()

                            myMaze <- Some(new Maze(W / 2, H, startPosition, endPosition, sameDirectionMin, sameDirectionMax))
                            st.player.drawSprite (pixel.create ('\219',Color.Cyan))
                            //resetto la posizione a quella di partenza
                            st.player.x <- (float(startPosition.X*2))
                            st.player.y <- float(startPosition.Y)
                            st.indicatore.clear
                            game_sound.PlayLooping())
                                
                        
                        st, false
                    else st, false

            let freccia = engine.create_and_register_sprite (image.rectangle (1,1,pixel.create('>', Color.White)), 25, 11, 1)
            let Maze = engine.create_and_register_sprite (image.rectangle (W,H, pixel.create(' ',Color.DarkBlue)),0,0,1)

            // initialize state
            let st0 = {
                    player = player
                    indicatore = freccia
                    maze = Maze
                    mode = Mode.Arcade
                    status = Status.MenuTasti
                    }
            engine.show_fps <- false
            // start engine
            engine.loop myLoop st0
                 
