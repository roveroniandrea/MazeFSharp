module LabProg2019.MazeGenerator

open System

//inizializzo random
let myRandom = new Random()

[<NoComparison;NoEquality>]
///Type used for the direction 
type Vector (x:int, y:int) =
    let mutable x = x
    let mutable y = y

    member this.X with get() = x
    member this.Y with get() = y

    ///Ritorna true o false se la posizione è all'interno del labirinto
    member this.isInsideMaze (W:int) (H:int):bool =
        this.X >= 1 && this.X < (W-1) && this.Y >= 1 && this.Y < (H-1)

    member this.getTranslated (direction:Vector) = new Vector(this.X + direction.X, this.Y + direction.Y)

    member this.isSameAs (other:Vector) = this.X = other.X && this.Y = other.Y

    ///Ritorna una direzione che non sia l'opposta
    member this.compatibleDirection () =
        let mutable newX = myRandom.Next(3)  - 1
        let mutable newY = myRandom.Next(3)  - 1
        while (newX + this.X) = 0 && (newY + this.Y) = 0 do
            newX <- myRandom.Next(3)  - 1
            newY <- myRandom.Next(3)  - 1
        new Vector (newX, newY)

///Classe della singola cella
type MazeCell (x:int, y:int, isWall:bool) =
    
    let mutable mutableIsWall:bool = isWall
    let mutable mutableVisited: bool = false
    let mutable mutableBlocked: bool = false
    let mutable mutablePosition: Vector = new Vector(x, y)

    ///Metodo che imposta o restituisce true/false se una cella è muro o meno
    member this.isWall with get() = mutableIsWall and set(value) = mutableIsWall <- value
    ///Metodo che imposta o restituisce true/false se una cella è stata visitata o meno
    member this.isVisited with get() = mutableVisited and set(value) = mutableVisited <- value
    ///Metodo che imposta o restituisce true/false se una cella è bloccata o meno
    member this.isBlocked with get() = mutableBlocked and set(value) = mutableBlocked <- value
    ///Metodo che restituisce la posizione della cella
    member this.position with get() = mutablePosition        


///Tipo del labirinto
type Maze (W:int, H:int, startPosition:Vector, endPosition:Vector, sameDirectionIntervalMin:int, sameDirectionIntervalMax:int) =
    let w = W
    let h = H
    let mutable mutableMaze = List.init (W * H) (fun (cellIndex) -> new MazeCell(cellIndex % w, cellIndex / w, true))
    let privateGetCell (position:Vector):MazeCell = mutableMaze.[(position.Y * w) + position.X]

    ///ritorna tutte le celle adiacenti ad essa se sono all'interno del labirinto
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

    ///Ritorna la stringa rappresentante il labirinto
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
                then stringResult <- stringResult + "  "
            //se è la cella finale
            elif (cellIndex=((w-1)+(w*29))) 
                then stringResult <- stringResult + "  "
            //altrimenti è una via
            else stringResult <- stringResult + "  "
        stringResult

    ///Trasforma le celle con isBlocked in celle con isWall
    let makeWallIfBlocked () =
        List.iter (fun (cell:MazeCell) -> if cell.isBlocked then cell.isWall <- true) mutableMaze

    ///Resetta le proprietà delle celle (cell.isBlocked <- false / cell.isVisited <- false)
    let resetCellsStatus () =
        List.iter (fun (cell:MazeCell) -> cell.isBlocked <- false
                                          cell.isVisited <- false) mutableMaze

    ///Collega l'uscita con la prima via che trova muovendosi verso il centro del labirinto(work in progress)
    let rec linkExit (cell:MazeCell) (direction:Vector) =
        if not(cell.isVisited) then
            cell.isWall <- false
            cell.isVisited <- true
            linkExit (privateGetCell (cell.position.getTranslated(direction))) direction

    ///Rompe qualche muro per rendere difficile il labirinto
    let linkPaths () =
        List.iter (fun (cell:MazeCell) -> if cell.isWall then let adiacentCells: MazeCell list = privateGetAdiacentCells cell endPosition
                                                              let notWallCells:MazeCell list = List.filter (fun (adjCell:MazeCell) -> not(adjCell.isWall)) adiacentCells
                                                              let sameX:bool = notWallCells.Length = 2 && notWallCells.[0].position.X = notWallCells.[1].position.X
                                                              let sameY:bool = notWallCells.Length = 2 && notWallCells.[0].position.Y = notWallCells.[1].position.Y
                                                              if (sameX || sameY) && myRandom.Next(100) <=10 then cell.isWall <- false
        ) mutableMaze

    ///Genera il labirinto 
    let rec generateMaze (startPosition:Vector, endPosition:Vector, sameDirectionIntervalMin:int, sameDirectionIntervalMax:int) =
        //inizializzo
        let startCell = privateGetCell startPosition
        startCell.isVisited <- true
        startCell.isWall <- false
        //Inizializzo il percorso del labirinto. La prima cella corrisponde alla cella di partenza
        let mutable mazePath:MazeCell list = [startCell]
        let myRandom = Random()

        ///Usato per creare corridoi "lunghi"
        let mutable stessoIndexPer = myRandom.Next(sameDirectionIntervalMin, sameDirectionIntervalMax)
        let mutable currentDirection = new Vector(1, 0)

        //Usato per verificare se il labirinto è stato esplorato per intero
        let mutable mazeCompleatelyExplored = false

        //Se il labirinto non è stato tutto visitato, continuo a visitare
        while not(mazeCompleatelyExplored) do
            //se "l'esploratore" è tornato alla posiziobe iniale, significa che il labirinto è stato tutto visitato
            if mazePath.Length = 0 then mazeCompleatelyExplored <- true
            else
                ///Restituisce tutte le celle adiacenti a quella corrente
                let adiacentCells:MazeCell list = privateGetAdiacentCells(privateGetCell mazePath.Head.position) (endPosition)
                ///Sottolista delle celle non visitate tra quelle adiacenti alla posizione corrente
                let notVisitedCells = List.filter (fun (cell:MazeCell) -> not(cell.isVisited)) adiacentCells
                ///Sottolista delle celle non bloccate tra quelle non visitate
                let notBlockedList = List.filter (fun (cell:MazeCell) -> not(cell.isBlocked))  notVisitedCells
                    
                //se ci sono delle celle non visitate e celle non bloccate
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
                        //altrimenti ne prendo una a caso (DUPLICATO da if precedente)
                        else
                            nextCell <- notBlockedList.[myRandom.Next(notBlockedList.Length)]
                            currentDirection <- new Vector(nextCell.position.X - mazePath.Head.position.X, nextCell.position.Y - mazePath.Head.position.Y)
                            stessoIndexPer <- myRandom.Next(sameDirectionIntervalMin, sameDirectionIntervalMax)
                    //segno che ho proseguito nella stessa direzione
                    stessoIndexPer <- stessoIndexPer - 1
                    //blocco tutte le celle adiacenti tranne quella che ho scelto come prossima cella
                    List.iter (fun (cell:MazeCell) -> if (cell <> nextCell) then cell.isBlocked <- true) notVisitedCells
                    //questa cella è visitata
                    nextCell.isVisited <- true
                    //abbatto il muro
                    nextCell.isWall <- false
                    //la aggiungo in testa al percorso del labirinto
                    mazePath <- nextCell::mazePath

                //se invece la cella corrente ha celle adiacenti tutte bloccate
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
        //collego l'uscita con la prima strada disponibile
        ignore(linkExit (privateGetCell endPosition) (new Vector(-1, 0)))
        //resetto lo stato delle celle
        ignore(resetCellsStatus())
        //rompo qualche muro per rendere difficile il labirinto
        ignore(linkPaths())
         
    //genero il labirinto
    do generateMaze(startPosition, endPosition, sameDirectionIntervalMin, sameDirectionIntervalMax)

    //getter di w e h
    member this.W with get() = w
    member this.H with get() = h
    //getter della lista delle celle
    member this.maze with get() = mutableMaze

    //ottiene la cella ad una certa posizione
    member this.getCell (position:Vector):MazeCell = privateGetCell position

    //trova la soluzione più corta per uscire dal labirinto
    member this.findSolution () =

       let mutable bestSolution: MazeCell list =[]
       let starCell = this.getCell(startPosition)
       let mutable solution: MazeCell list = []
       

       for i = 0 to 100 do

           solution <- starCell::solution

           while not(solution.Head.position.isSameAs(endPosition)) do
                solution.Head.isVisited <- true
                //trovo le celle adiacenti
                let adiacentCells : MazeCell list = privateGetAdiacentCells solution.Head endPosition
                //prendo quelle non visitate
                let notVisitedAdiacent: MazeCell list = List.filter (fun (cell:MazeCell) -> not(cell.isVisited) && not(cell.isWall)) adiacentCells
                //se ce ne sono di non visitate ne scelgo una
                if notVisitedAdiacent.Length > 0 then
                    let nextCell:MazeCell = notVisitedAdiacent.[myRandom.Next(notVisitedAdiacent.Length)]
                    solution <- nextCell::solution
                //altrimenti è un vicolo cieco e torno indietro
                else solution <- solution.Tail

           if(i=0) then bestSolution <-solution
                        solution <-[]
                        ignore(resetCellsStatus())

           elif(bestSolution.Length>=solution.Length)then bestSolution <-solution
                                                          solution <-[]
                                                          ignore(resetCellsStatus())

       bestSolution

    member this.generateMazeString () = privateGenMazeString()

//funzione di init. Genera un labirinto e lo ritorna insieme alla stringa da stampare
let initMaze (W:int, H:int, startPosition:Vector, endPosition:Vector, sameDirectionIntervalMin:int, sameDirectionIntervalMax:int) =
    let myMaze:Maze = new Maze(W, H, startPosition, endPosition, sameDirectionIntervalMin, sameDirectionIntervalMax)
    let str = myMaze.generateMazeString()
    (str,myMaze)

