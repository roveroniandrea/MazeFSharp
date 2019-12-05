module LabProg2019.menu

open System
open Engine
open Gfx

type Status = Menu|InGame
type ButtonAction = StartGame|Quit

[< NoEquality; NoComparison >]
type state = {
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

            let arr_options:Button list = [ new Button ("Gioca", ButtonAction.StartGame);
                                             new Button ("Esci", ButtonAction.Quit)
                                           ]
            
            let engine = new engine (W, H)

            Console.ForegroundColor <- ConsoleColor.Green
            (*
            printfn "
            
            ███╗   ███╗ █████╗ ███████╗███████╗
            ████╗ ████║██╔══██╗╚══███╔╝██╔════╝
            ██╔████╔██║███████║  ███╔╝ █████╗  
            ██║╚██╔╝██║██╔══██║ ███╔╝  ██╔══╝  
            ██║ ╚═╝ ██║██║  ██║███████╗███████╗
            ╚═╝     ╚═╝╚═╝  ╚═╝╚══════╝╚══════╝                                   
            "*)
            
            let enter = char (13)
            let mutable mazeString = ""

            let myLoop (keyo : ConsoleKeyInfo option) (screen : wronly_raster) (st : state) =
                    if (st.status = Status.Menu) then 
                                let mutable wantToQuit = false
                                screen.draw_text("", 0, 7, Color.Green)
                                for i=0 to arr_options.Length - 1 do
                                    arr_options.[i].Y <- 3 + 2 * i
                                    screen.draw_text(arr_options.[i].etichetta, 4, arr_options.[i].Y, Color.White)

                                let dx, dy =
                                    match keyo with
                                    None -> 0. ,0.
                                    |Some key -> Console.Beep(330, 125) 
                                                 match key.KeyChar with 
                                                      'w' -> 0., -2.
                                                    | 's' -> 0., 2.
                                                    | _ when key.KeyChar = enter -> let clickedButton: Button option = List.tryFind (fun (button:Button)-> button.Y = int(st.indicatore.y)) arr_options
                                                                                    in match clickedButton with
                                                                                        None -> ignore()
                                                                                        |Some button -> match button.codice with
                                                                                                        ButtonAction.StartGame -> st.status <- Status.InGame
                                                                                                                                  mazeString <- Prova.main(W / 2, H)
                                                                                                                                  screen.draw_text(mazeString, 0, 0, Color.DarkGray)
                                                                                                                                  st.indicatore.clear
                                                                                                        |ButtonAction.Quit -> wantToQuit <- true
                                                                                    0., 0.
                                                    | _   -> 0., 0.
                                st.indicatore.move_by(dx, dy)
                
                                if (st.indicatore.y < 3.0) then st.indicatore.y <- 3.0
                                    else if (st.indicatore.y > float ( 3+(2*(arr_options.Length - 1)) ) ) 
                                            then st.indicatore.y <- float ( 3+(2*(arr_options.Length - 1)) )
                                st, wantToQuit

                    elif st.status = Status.InGame then
                         screen.draw_text(mazeString, 0, 0, Color.DarkGray)
                         //movimento giocatore da fare
                         let dx, dy =
                             match keyo with
                             None -> 0., 0.
                             |Some key -> Console.Beep(330, 125) 
                                          match key.KeyChar with 
                                               'w' -> 0., 0.
                                             | 'a' -> 0., 0.
                                             | 's' -> 0., 0.
                                             | 'd' -> 0., 0.
                                             | 'q' -> st.status <- Status.Menu
                                                      st.indicatore.flood_fill(0, 0, pixel.create('>', Color.Green))
                                                      0., 0.
                                             | _   -> 0., 0.
                         st, false
                    else st, false

            let freccia = engine.create_and_register_sprite (image.rectangle (2,1,pixel.create('>', Color.Green)), 0, 3, 1) //è una freccia anche bella da vedere con il giusto approccio bella lì colibrì stai tranquillo ho solo fatto un commento lungo

            // initialize state
            let st0 = { 
                    indicatore = freccia
                    status = Status.Menu
                    }
            engine.show_fps <- false
            // start engine
            //engine.loop_on_key my_update st0
            engine.loop myLoop st0
                 
