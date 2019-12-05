module LabProg2019.Prova


open System
open Engine
open Gfx

let printMenu =
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


    " 
    printfn"
            88888888888888888888888888888888888888888888888888888888888888888888888
            88.._|      | `-.  | `.  -_-_ _-_  _-  _- -_ -  .'|   |.'|     |  _..88
            88   `-.._  |    |`!  |`.  -_ -__ -_ _- _-_-  .'  |.;'   |   _.!-'|  88
            88      | `-!._  |  `;!  ;. _______________ ,'| .-' |   _!.i'     |  88
            88..__  |     |`-!._ | `.| |_______________||.''|  _!.;'   |     _|..88
            88   |``'..__ |    |`';.| i|_|MMMMMMMMMMM|_|'| _!-|   |   _|..-|'    88
            88   |      |``--..|_ | `;!|i|MMoMMMMoMMM|i|.'j   |_..!-'|     |     88
            88   |      |    |   |`-,!_|_|MMMMY'YMMMM|_||.!-;'  |    |     |     88
            88___|______|____!.,.!,.!,!|i|MMMl * loMM|i|,!,.!.,.!..__|_____|_____88
            88      |     |    |  |  | |_|MMMMb,dMMMM|_|| |   |   |    |      |  88
            88      |     |    |..!-;'i|i|MoMMMMMMMoM|i| |`-..|   |    |      |  88
            88      |    _!.-j'  | _!,'|_|MMMoMMMoMMM|_||!._|  `i-!.._ |      |  88
            88     _!.-'|    | _.'|  !;|i|MMMMMoMMMMM|i|`.| `-._|    |``-.._  |  88
            88..-i'     |  _.''|  !-| !|_|MMoMMMMMoMM|_|.|`-. | ``._ |     |``'..88
            88   |      |.|    |.|  !| |i|MMMoMMMoMMM|i||`. |`!   | `'.    |     88
            88   |  _.-'  |  .'  |.' |/|_|MMoMMMMMoMM|_|! |`!  `,.|    |-._|     88
            88  _!''|     !.'|  .'| .'|[@]MMMMMMMMMMM[@] \|  `. | `._  |   `-._  88
            88-'    |   .'   |.|  |/| /                 \|`.  |`!    |.|      |`-88
            88      |_.'|   .' | .' |/                   \  \ |  `.  | `._-   |  88
            88     .'   | .'   |/|  /                     \ |`!   |`.|    `.  |  88
            88  _.'     !'|   .' | /                       \|  `  |  `.    |`.|  88
            88888888888888888888888888888888888888888888888888888888888888888888888
    
    "

    printfn"
        ---------
       | Opzioni |
        ---------
    "


    Console.ResetColor ()
    
    printf "Premi un tasto per generare il labirinto..."
    Console.ReadKey()

let printMaze (maze: String) =
    for i=0 to maze.Length - 1 do
        if maze.[i] = 'A' then
            printf "  "//strada
        elif maze.[i] = 'S' then
            Console.ForegroundColor <- ConsoleColor.Green
            printf "██" 
            Console.ResetColor ()
        elif maze.[i] = 'E' then
            Console.ForegroundColor <- ConsoleColor.Red
            printf "██"
            Console.ResetColor ()
        else
            printf "%c" maze.[i]

[< NoEquality; NoComparison >]

type state = {
    player : sprite
}

type Direction (dirX:int, dirY:int) =
    member this.dirX = dirX
    member this.dirY = dirY
    member this.compatibleDirection () =
        let probDirezione = 100
        let mutable newX = Random().Next(3)  - 1
        let mutable newY = Random().Next(3)  - 1
        if Random().Next(100) <= probDirezione then
            newX <- this.dirX
            newY <- this.dirY
        while (newX + this.dirX) = 0 && (newY + this.dirY) = 0 do
            newX <- Random().Next(3)  - 1
            newY <- Random().Next(3)  - 1
        new Direction (newX, newY)

type Position (x:int, y:int) =
    let mutable x = x
    let mutable y = y
    member this.X with get() = x
    member this.Y with get() = y
    member this.isInsideMaze (W:int) (H:int) =
        this.X >= 1 && this.X < (W-1) && this.Y >= 1 && this.Y < (H-1)

    member this.getTranslated (direction:Direction) = new Position(this.X + direction.dirX, this.Y + direction.dirY)

type MazeCell (x:int, y:int, isWall:bool) =
    let mutable mutableIsWall:bool = isWall
    let mutable mutableVisited: bool = false
    let mutable mutableposition: Position = new Position(x, y)

    member this.isWall with get() = mutableIsWall and set(value) = mutableIsWall <- value
    member this.isVisited with get() = mutableVisited and set(value) = mutableVisited <- value
    member this.position with get() = mutableposition        



type Maze (W:int, H:int) =
    let w = W
    let h = H
    let mutable privateMazeSolution:MazeCell list = []
    let mutable privateBlockedCells:MazeCell list = []
    let mutable mutableMaze = List.init (W * H) (fun (cellIndex) -> new MazeCell(cellIndex % w, cellIndex / w, true))
    let privateGetCell (position:Position):MazeCell = mutableMaze.[(position.Y * w) + position.X]


    



    let privateGetAdiacentCells (cell:MazeCell) (endPosition:Position)=
        let mutable resultCells: MazeCell list = []

        let addIfInsideMaze (position:Position) =
            if position.isInsideMaze w h || ((position.X = endPosition.X)&&(position.Y = endPosition.Y || position.Y = (endPosition.Y + 1)))then resultCells <- resultCells @ [privateGetCell position]
        //est
        let direction = new Direction(-1, 0)
        addIfInsideMaze (cell.position.getTranslated(direction))
        //nord
        let direction = new Direction(0, 1)
        addIfInsideMaze (cell.position.getTranslated(direction))
        //ovest
        let direction = new Direction(1, 0)
        addIfInsideMaze (cell.position.getTranslated(direction))
        //sud
        let direction = new Direction(0, -1)
        addIfInsideMaze (cell.position.getTranslated(direction))
        resultCells

    let privateGenMazeString () =
        let mutable stringResult = ""
        for cellIndex = 0 to (w * h) - 1 do
            let position:Position = new Position(cellIndex % w, cellIndex / w)
            let currentCell = privateGetCell(position)
            if false && (List.contains currentCell privateBlockedCells) then stringResult <- stringResult + "X"
            else
                if currentCell.isWall then stringResult <- stringResult + "██"  //alt + 178 -> ▓  alt + 219 -> █
                elif (cellIndex=w) 
                    then stringResult <- stringResult + "S" 
                elif (cellIndex=((w-1)+(w*29))) 
                    then stringResult <- stringResult + "E" 
                elif List.contains currentCell privateMazeSolution 
                    then stringResult <- stringResult + "A" 
                else stringResult <- stringResult + "  "
                if position.X = (w - 1) then stringResult <- stringResult + "\n"
                
        stringResult

        //celle non visitate e non bloccate
    let rec generateMaze (startPosition:Position, endPosition:Position, ripetiNVolte:int) =
        if ripetiNVolte < 1 then ignore()
        else
            //let startPosition = new Position(0,1)
            //let endPosition = new Position (w-1,29)
            let startCell = privateGetCell startPosition
            startCell.isVisited <- true
            startCell.isWall <- false
            let mutable mazeSolution:MazeCell list = [startCell]
            let mutable blockedCells:MazeCell list = []
            let mutable exitFound = false
            let myRandom = Random()
            //let mutable direction = new Direction(1, 0)
            (*
            while currentPosition.isInsideMaze w h do
                let currentCell:MazeCell = privateGetCell currentPosition
                currentCell.isVisited <- true
                currentCell.isWall <- false
                currentPosition <- currentPosition.getTranslated(direction)*)
            let mutable stessoIndexPer = myRandom.Next(3, 5)
            let mutable currentDirection = new Direction(1, 0)
            let mutable mazeCompleatelyExplored = false

            while not((*exitFound ||*) mazeCompleatelyExplored) do
                if mazeSolution.Length = 0 then mazeCompleatelyExplored <- true
                else
                    let adiacentCells:MazeCell list = privateGetAdiacentCells(privateGetCell mazeSolution.Head.position) (endPosition)
                    let notVisitedCells = List.filter (fun (cell:MazeCell) -> not(cell.isVisited)) adiacentCells
                    let mutable notBlockedList = []
                    for i=0 to notVisitedCells.Length - 1 do
                        if (not(List.contains notVisitedCells.[i] blockedCells)) then
                            notBlockedList <- notVisitedCells.[i]::notBlockedList
                            
                    if notVisitedCells.Length > 0 && notBlockedList.Length > 0 then
                        let mutable nextCell = notBlockedList.[myRandom.Next(notBlockedList.Length)]
                        if stessoIndexPer <= 0 then
                            nextCell <- notBlockedList.[myRandom.Next(notBlockedList.Length)]
                            currentDirection <- new Direction(nextCell.position.X - mazeSolution.Head.position.X, nextCell.position.Y - mazeSolution.Head.position.Y)
                            stessoIndexPer <- myRandom.Next(3, 5)
                        else
                            let containsStessaDirezione = List.exists (fun (cell:MazeCell) -> (cell.position.X - mazeSolution.Head.position.X) = currentDirection.dirX && (cell.position.Y - mazeSolution.Head.position.Y) = currentDirection.dirY) notBlockedList
                            if containsStessaDirezione then
                                nextCell <- List.find (fun (cell:MazeCell) -> (cell.position.X - mazeSolution.Head.position.X) = currentDirection.dirX && (cell.position.Y - mazeSolution.Head.position.Y) = currentDirection.dirY) notBlockedList
                            else
                                nextCell <- notBlockedList.[myRandom.Next(notBlockedList.Length)]
                                currentDirection <- new Direction(nextCell.position.X - mazeSolution.Head.position.X, nextCell.position.Y - mazeSolution.Head.position.Y)
                                stessoIndexPer <- myRandom.Next(3, 5)
                                



                        stessoIndexPer <- stessoIndexPer - 1
                        blockedCells <-  (List.filter (fun (cell:MazeCell) -> cell <> nextCell) notVisitedCells) @ blockedCells
                        nextCell.isVisited <- true
                        nextCell.isWall <- false
                        mazeSolution <- nextCell::mazeSolution
 
                                
                                

                        if nextCell.position.X = (w - 1) && nextCell.position.Y = (29) &&  notBlockedList.Length = 0 then exitFound <- true
                    else
                        let mutable sbloccoCellaIndex = -1
                        for i=0 to notVisitedCells.Length - 1 do
                            if List.contains notVisitedCells.[i] blockedCells then
                                let adiacentOfBlockedCell = privateGetAdiacentCells notVisitedCells.[i] endPosition
                                let visited = List.filter (fun (cell:MazeCell)-> cell.isVisited) adiacentOfBlockedCell
                                if visited.Length <= 1 then
                                    sbloccoCellaIndex <- i
                        if sbloccoCellaIndex <> -1 then
                            let cellaDaSbloccare:MazeCell = notVisitedCells.[sbloccoCellaIndex]
                            cellaDaSbloccare.isVisited <- true
                            cellaDaSbloccare.isWall <- false
                            mazeSolution <- cellaDaSbloccare::mazeSolution
                                    
                                    
                                    
                            //let celleNonVisitateNeBloccate  =   List.filter (fun (cell:MazeCell) -> cell.isWall) notBlockedList

                            if cellaDaSbloccare.position.X = (w - 1) && cellaDaSbloccare.position.Y = (29) && notBlockedList.Length = 0 then exitFound <- true
                            blockedCells <- List.filter (fun (cell:MazeCell)-> cell <> cellaDaSbloccare) blockedCells
                        else
                            mazeSolution <- mazeSolution.Tail
                            
                privateMazeSolution <- mazeSolution
                privateBlockedCells <- blockedCells
                (*
                let str = privateGenMazeString()
                printfn "%s" str
                Threading.Thread.Sleep(1000)
                Console.Clear()*)
                generateMaze(endPosition, startPosition, ripetiNVolte - 1)


    do generateMaze(new Position(0,1), new Position (w-1,29), 2)
    member this.W with get() = w
    member this.H with get() = h
    member this.maze with get() = mutableMaze

    member this.getCell (position:Position):MazeCell = privateGetCell position

    member this.getSolution with get() = privateMazeSolution
    member this.blockedCellsList with get() = privateBlockedCells

    member this.generateMazeString () = privateGenMazeString()


let W = 60
let H = 30

let main () =       
    //let engine = new engine (W, H)
    let rec nextLabirinto () = 
        let myMaze:Maze = new Maze(50, 50)
        let str = myMaze.generateMazeString()

        ignore(printMenu)
        //stampo il labirinto
        printMaze str
        //printfn "\n \n \n \n \n \n \n \n"

        printf "Premi q per cuscrire:"
        let soluzione = Console.ReadLine()
        //printfn "\n \n \n \n \n \n \n \n"
    
        if soluzione = "q" then 
            Console.Clear()
        else 
            Console.Clear()
            nextLabirinto()
        
                                    
    in nextLabirinto()
            

    
    (*
    let my_update (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) =
        screen.draw_text("Hai premuto qualcosa", 0, 3, Color.Green)
        List.iter (fun (asteroide:sprite) -> asteroide.move_by(0, 1)) st.asteroidi
        st, key.KeyChar = 'q'

    // create simple backgroud and player
    //ignore <| engine.create_and_register_sprite (image.rectangle (W, H, pixel.filled Color.Yellow, pixel.filled Color.Blue), 0, 0, 0)
    let player = engine.create_and_register_sprite (image.rectangle (1, 1, pixel.filled Color.White, pixel.filled Color.Gray), W / 2, H / 2, 1)
    engine.show_fps <- false
    let asteroidi = List.init 5 (fun x -> engine.create_and_register_sprite(image.rectangle(1, 1, pixel.filled Color.DarkGray, pixel.filled Color.DarkGray), Random().Next(0, W) , 0, 0 ))
    //let testo = engine.create_and_register_sprite(image.rectangle(22,1, pixel.filled Color.Green),0,4, 0)
    // initialize state
    let st0 = { 
        player = player
        asteroidi = asteroidi
        }
    // start engine
    engine.loop_on_key my_update st0*)

