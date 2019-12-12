module LabProg2019.MazeGenerator

//█   ▀▄

open System

(*type Direction (dirX:int, dirY:int) =
    //inizializzo random
    let myRandom = new Random()
    member this.dirX = dirX
    member this.dirY = dirY

    //ritorna una direzione che non sia l'opposta
    member this.compatibleDirection () =
        let mutable newX = myRandom.Next(3)  - 1
        let mutable newY = myRandom.Next(3)  - 1
        while (newX + this.dirX) = 0 && (newY + this.dirY) = 0 do
            newX <- myRandom.Next(3)  - 1
            newY <- myRandom.Next(3)  - 1
        new Direction (newX, newY)*)

[<NoComparison;NoEquality>]
type Vector (x:int, y:int) =
    let mutable x = x
    let mutable y = y
    //inizializzo random
    let myRandom = new Random()

    member this.X with get() = x
    member this.Y with get() = y

    //ritorna true o false se la posizione è all'interno del labirinto
    member this.isInsideMaze (W:int) (H:int):bool =
        this.X >= 1 && this.X < (W-1) && this.Y >= 1 && this.Y < (H-1)

    member this.getTranslated (direction:Vector) = new Vector(this.X + direction.X, this.Y + direction.Y)

    member this.isSameAs (other:Vector) = this.X = other.X && this.Y = other.Y

    //ritorna una direzione che non sia l'opposta
    member this.compatibleDirection () =
        let mutable newX = myRandom.Next(3)  - 1
        let mutable newY = myRandom.Next(3)  - 1
        while (newX + this.X) = 0 && (newY + this.Y) = 0 do
            newX <- myRandom.Next(3)  - 1
            newY <- myRandom.Next(3)  - 1
        new Vector (newX, newY)

//classe della singola cella
type MazeCell (x:int, y:int, isWall:bool) =
    let mutable mutableIsWall:bool = isWall
    let mutable mutableVisited: bool = false
    let mutable mutableBlocked: bool = false
    let mutable mutablePosition: Vector = new Vector(x, y)

    member this.isWall with get() = mutableIsWall and set(value) = mutableIsWall <- value
    member this.isVisited with get() = mutableVisited and set(value) = mutableVisited <- value
    member this.isBlocked with get() = mutableBlocked and set(value) = mutableBlocked <- value
    member this.position with get() = mutablePosition        



type Maze (W:int, H:int, startPosition:Vector, endPosition:Vector, sameDirectionIntervalMin:int, sameDirectionIntervalMax:int) =
    let w = W
    let h = H
    let mutable mutableMaze = List.init (W * H) (fun (cellIndex) -> new MazeCell(cellIndex % w, cellIndex / w, true))
    let privateGetCell (position:Vector):MazeCell = mutableMaze.[(position.Y * w) + position.X]

    //ritorna tutte le celle adiacenti ad essa se sono all'interno del labirinto
    let privateGetAdiacentCells (cell:MazeCell) (endPosition:Vector)=
        let mutable resultCells: MazeCell list = []

        let addIfInsideMaze (position:Vector) =
            if position.isInsideMaze w h || position.isSameAs(endPosition) then resultCells <- resultCells @ [privateGetCell position]
        
        //cella est
        let direction = new Vector(-1, 0)
        addIfInsideMaze (cell.position.getTranslated(direction))
        //cella nord
        let direction = new Vector(0, 1)
        addIfInsideMaze (cell.position.getTranslated(direction))
        //cella ovest
        let direction = new Vector(1, 0)
        addIfInsideMaze (cell.position.getTranslated(direction))
        //cella sud
        let direction = new Vector(0, -1)
        addIfInsideMaze (cell.position.getTranslated(direction))
        resultCells

    //ritorna una stringa
    let privateGenMazeString () =
        let mutable stringResult = ""
        //per ogni cella
        for cellIndex = 0 to (w * h) - 1 do
            //trovo la sua posizione
            let position:Vector = new Vector(cellIndex % w, cellIndex / w)
            //trovo la cella
            let currentCell = privateGetCell(position)
            //se è un muro oppure è bloccata
            if currentCell.isWall then stringResult <- stringResult + "\219\219"  //alt + 178 -> ▓  alt + 219 -> █
            //se è la cella iniziale
            elif (cellIndex=w) 
                then stringResult <- stringResult + "S"
            //se è la cella finale
            elif (cellIndex=((w-1)+(w*29))) 
                then stringResult <- stringResult + "E"
            //altrimenti è una via
            else stringResult <- stringResult + "  "
            //vado a capo se ho raggiunto la fine della riga
            if position.X = (w - 1) then stringResult <- stringResult + "\n"
        stringResult

    //tutte le celle con isBlocked vengono messe isWall
    let makeWallIfBlocked () =
        List.iter (fun (cell:MazeCell) -> if cell.isBlocked then cell.isWall <- true) mutableMaze

    let resetCellsStatus () =
        List.iter (fun (cell:MazeCell) -> cell.isBlocked <- false
                                          cell.isVisited <- false) mutableMaze

    let rec linkExit (cell:MazeCell) (direction:Vector) =
        if not(cell.isVisited) then
            cell.isWall <- false
            cell.isVisited <- true
            linkExit (privateGetCell (cell.position.getTranslated(direction))) direction

    //genera il labirinto 
    let rec generateMaze (startPosition:Vector, endPosition:Vector, sameDirectionIntervalMin:int, sameDirectionIntervalMax:int) =
        //inizializzo
        let startCell = privateGetCell startPosition
        startCell.isVisited <- true
        startCell.isWall <- false
        let mutable mazePath:MazeCell list = [startCell]
        let myRandom = Random()
        let mutable stessoIndexPer = myRandom.Next(sameDirectionIntervalMin, sameDirectionIntervalMax)
        let mutable currentDirection = new Vector(1, 0)
        let mutable mazeCompleatelyExplored = false

        while not(mazeCompleatelyExplored) do
            if mazePath.Length = 0 then mazeCompleatelyExplored <- true
            else
                //ottengo tutte le celle adiacenti a quella corrente
                let adiacentCells:MazeCell list = privateGetAdiacentCells(privateGetCell mazePath.Head.position) (endPosition)
                //sottolista delle celle non visitate tra quelle adiacenti
                let notVisitedCells = List.filter (fun (cell:MazeCell) -> not(cell.isVisited)) adiacentCells
                //sottolista delle celle non bloccate tra quelle non visitate
                let notBlockedList = List.filter (fun (cell:MazeCell) -> not(cell.isBlocked))  notVisitedCells
                    
                //se ci sono delle celle non visitate e delle celle non bloccate
                if notBlockedList.Length > 0 then
                    //inizializzo nextCell
                    let mutable nextCell = notBlockedList.[myRandom.Next(notBlockedList.Length)]
                    //se posso cambiare direzione ne scelgo una casuale
                    if stessoIndexPer <= 0 then
                        nextCell <- notBlockedList.[myRandom.Next(notBlockedList.Length)]
                        currentDirection <- new Vector(nextCell.position.X - mazePath.Head.position.X, nextCell.position.Y - mazePath.Head.position.Y)
                        //resetto il countdown per la direzione
                        stessoIndexPer <- myRandom.Next(sameDirectionIntervalMin, sameDirectionIntervalMax)
                    //altrimenti scelgola cella che mantiene la stessa direzione
                    else
                        //controllo se esiste
                        let containsStessaDirezione = List.exists (fun (cell:MazeCell) -> (cell.position.X - mazePath.Head.position.X) = currentDirection.X && (cell.position.Y - mazePath.Head.position.Y) = currentDirection.Y) notBlockedList
                        //se esiste la scelgo
                        if containsStessaDirezione then
                            nextCell <- List.find (fun (cell:MazeCell) -> (cell.position.X - mazePath.Head.position.X) = currentDirection.X && (cell.position.Y - mazePath.Head.position.Y) = currentDirection.Y) notBlockedList
                        //altrimenti ne prendo uan a caso (DUPLICATO da if precedente)
                        else
                            nextCell <- notBlockedList.[myRandom.Next(notBlockedList.Length)]
                            currentDirection <- new Vector(nextCell.position.X - mazePath.Head.position.X, nextCell.position.Y - mazePath.Head.position.Y)
                            stessoIndexPer <- myRandom.Next(sameDirectionIntervalMin, sameDirectionIntervalMax)
                    //segno che sono andato nella stessa direzione un'altra volta
                    stessoIndexPer <- stessoIndexPer - 1
                    //blocco tutte le celle adiacenti
                    List.iter (fun (cell:MazeCell) -> if (cell <> nextCell) then cell.isBlocked <- true) notVisitedCells
                    //questa cella è visitata
                    nextCell.isVisited <- true
                    //abbatto il muro
                    nextCell.isWall <- false
                    //la aggiungo in testa al percorso del labirinto
                    mazePath <- nextCell::mazePath

                //se invece la cella corrente non ha celle non bloccate
                else
                    //ottengo la prima cella sbloccabile se esiste
                    let existsUnblockableCell = List.tryFind (fun (cell:MazeCell) -> 
                                //ottengo tutte le celle adiacenti a quella non visitata
                                let adiacentOfBlockedCell = privateGetAdiacentCells cell endPosition
                                let visitedOfAdiacent = List.filter (fun (cell:MazeCell)-> cell.isVisited) adiacentOfBlockedCell
                                visitedOfAdiacent.Length < 2 ) notVisitedCells

                    match existsUnblockableCell with
                    //se esiste una cella sbloccabile la sblocco e la aggiungo al percorso
                    Some value ->
                        value.isVisited <- true
                        value.isWall <- false
                        value.isBlocked <- false
                        mazePath <- value::mazePath
                    //altrimenti torno indietro di una cella
                    |None ->
                        mazePath <- mazePath.Tail

        //chiamo correzione delle celle bloccate
        ignore(makeWallIfBlocked())
        ignore(linkExit (privateGetCell endPosition) (new Vector(-1, 0)))
        ignore(resetCellsStatus())

    do generateMaze(startPosition, endPosition, sameDirectionIntervalMin, sameDirectionIntervalMax)
    member this.W with get() = w
    member this.H with get() = h
    member this.maze with get() = mutableMaze

    member this.getCell (position:Vector):MazeCell = privateGetCell position

    member this.findSolution () =
       let mutable solution:MazeCell list = [this.getCell(startPosition)]
       while not(solution.Head.position.isSameAs(endPosition)) do
            solution.Head.isVisited <- true
            let adiacentCells : MazeCell list = privateGetAdiacentCells solution.Head endPosition
            let notVisitedAdiacent: MazeCell list = List.filter (fun (cell:MazeCell) -> not(cell.isVisited) && not(cell.isWall)) adiacentCells
            if notVisitedAdiacent.Length > 0 then
                let nextCell:MazeCell = notVisitedAdiacent.[0]
                solution <- nextCell::solution
            else solution <- solution.Tail
       solution

    member this.generateMazeString () = privateGenMazeString()


let initMaze (W:int, H:int, startPosition:Vector, endPosition:Vector, sameDirectionIntervalMin:int, sameDirectionIntervalMax:int) =
    let myMaze:Maze = new Maze(W, H, startPosition, endPosition, sameDirectionIntervalMin, sameDirectionIntervalMax)
    let str = myMaze.generateMazeString()

    let mutable convertedString = ""
    //stampo il labirinto
    for i=0 to str.Length - 1 do
        if str.[i] = 'S' then
            convertedString <- convertedString +  "  "
        elif str.[i] = 'E' then
            convertedString <- convertedString +  "  "
        else
            convertedString <- convertedString + string(str.[i])
    (convertedString,myMaze)

