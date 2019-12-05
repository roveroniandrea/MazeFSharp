module LabProg2019.menu

open System
open Engine
open Gfx

[< NoEquality; NoComparison >]
type state = {
    indicatore : sprite
}

type Buttons (etichetta:string, codice:int) =
    let mutable y = 0
    member this.etichetta = etichetta
    member this.codice = codice
    member this.Y with get() = y and set(value) = y <- value


let init ()  =

            let W = 60
            let H = 30

            let arr_options:Buttons list = [ new Buttons ("Gioca",1);
                                             new Buttons ("Esci", 2)
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

            let a = "                          
            "
            let myLoop (keyo : ConsoleKeyInfo option) (screen : wronly_raster) (st : state) =
                screen.draw_text(a, 0, 7, Color.Green)
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
                                   
                                    | _   -> 0., 0.
                st.indicatore.move_by(dx, dy)
                
                if (st.indicatore.y < 3.0) then st.indicatore.y <- 3.0
                    else if (st.indicatore.y > float ( 3+(2*(arr_options.Length - 1)) ) ) 
                            then st.indicatore.y <- float ( 3+(2*(arr_options.Length - 1)) )
                st, false


            let my_update (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) =
                  
                // move player
                let dx, dy =
                    match key.KeyChar with 
                    | 'w' -> 0., -2. 
                    | 's' -> 0., 2.
                    | _   -> 0., 0.
                // TODO: check bounds
                Console.Beep(330, 125)

                //for x = 0 to arr_options.Length - 1 do
                    //screen.draw_text (arr_options.[x, 4, 3+(2*x), Color.White)

                st.indicatore.move_by (dx, dy)

                if (st.indicatore.y < 3.0) then st.indicatore.y <- 3.0
                    else if (st.indicatore.y > float ( 3+(2*(arr_options.Length - 1)) ) ) 
                         then st.indicatore.y <- float ( 3+(2*(arr_options.Length - 1)) )

                st, key.KeyChar = 'q'            

            let freccia = engine.create_and_register_sprite (image.rectangle (2,1,pixel.filled Color.White), 0, 3, 1) //è una freccia anche bella da vedere con il giusto approccio bella lì colibrì stai tranquillo ho solo fatto un commento lungo

                // initialize state
            let st0 = { 
                    indicatore = freccia
                    }
                // start engine
            //engine.loop_on_key my_update st0
            engine.loop myLoop st0
                 
