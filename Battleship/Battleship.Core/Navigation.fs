namespace Battleship.Core

module Navigation =
    open Grid
    open Ship

    type Sector = Clear | Active of Name * int

    type Rotation =
        | Clockwise
        | Counterclockwise

    let getDegrees (direction: Direction) : int =
        match direction with
        | South -> 0
        | West -> 90
        | North -> 180
        | East -> 270

    (* ------- À COMPLÉTER ------- *)
    (* --- Nouvelles fonctions --- *)
    
    let isSectorActive sector : bool =
        match sector with
        | Some (Active _) -> true
        | _ -> false
        
        // Fonction pour obtenir le périmètre de tous les bateaux sur la grille
    let getAllShipsPerimeters (grid: Sector Grid) : Coord list =
        let gridDims = getDimsFromGrid grid
        let activeSectors = getGridCoords gridDims |> List.filter (fun (x, y) -> 
            isSectorActive(getSector x y grid)
        )
        
        activeSectors 
        |> List.collect adjacentCells
        |> List.distinct
        |> List.filter
               (fun coord -> 
                    let (x, y) = coord
                    not (isSectorActive(getSector x y grid))             
                )
    
    let isCoordsAvailable (coords : Coord List) (grid: Sector Grid) : bool =
        let rec statesVerification coords =
            match coords with
            | [] -> true
            | (x, y) :: rest ->
                let sector = getSector x y grid
                if isSectorActive sector then false
                else statesVerification rest
        
        statesVerification coords
    
    let isCoordsInsideGrid (coords : Coord List) (grid: Sector Grid) : bool =
        let gridCoords = getGridCoords (getDimsFromGrid grid)
        let invalidCoords = coords |> List.filter (fun coord -> not (List.contains coord gridCoords))
        let insideGrid = invalidCoords.Length = 0
        
        insideGrid
    
    
    let canPlace (center: Coord) (direction: Direction) (name: Name) (grid: Sector Grid) : bool =
        let ship = createShip center direction name
        let coords = ship.Coords
        
        // Vérifiez qu'il n'y a pas déjà un autre bateau à ces coordonnées
        let coordsVerification = isCoordsAvailable coords grid
        
        // Vérifie que le bateau est à l’intérieur de la grille
        let insideGrid = isCoordsInsideGrid coords grid

        // Vérifiez que les coordonnées voulues ne touche pas aux périmètres d'un autre bateau
        let shipsPerimeters = getAllShipsPerimeters grid
        let coordsColliding= coords |> List.filter (fun coord -> (List.contains coord shipsPerimeters))
        let doesNotCollideWithPerimeters = coordsColliding.Length = 0
        
        insideGrid && coordsVerification && doesNotCollideWithPerimeters

    let calculateNewCoords (ship: Ship) (direction: Direction) : Coord List =
        let coords = ship.Coords
        let rec newCoords coords resultsList =
            match coords with
            | [] -> resultsList
            | (x,y) :: rest ->
                let newCoord =
                    match direction with
                    | North -> (x-1, y)
                    | South -> (x+1, y)
                    | East  -> (x, y+1)
                    | West  -> (x, y-1)
                newCoords rest (newCoord :: resultsList)                
        newCoords coords [] |> List.rev

            

    let canMove (ship: Ship) (direction: Direction) (grid: Sector Grid) : bool =
        let newCoords = calculateNewCoords ship direction
        printfn "Ship coords: %A" ship.Coords
        printfn "New coords: %A" newCoords
        let newCenter = getCenterFromCoords newCoords
        printfn "New center: %A" newCenter
        let canPlace = canPlace newCenter ship.Facing ship.Name grid
        printfn "Can place: %A" canPlace
        canPlace 
        
    let move (ship: Ship) (direction: Direction) : Ship =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        let newCoords = calculateNewCoords ship direction
        let newCenter = getCenterFromCoords newCoords
        let newShip = createShip newCenter ship.Facing ship.Name
        
        newShip


    let canRotate (ship: Ship) (direction: Direction) (grid: Sector Grid) : bool =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        false

    let rotate (ship: Ship) (direction: Direction) : Ship =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        { Coords = []; Center = (0, 0); Facing = North; Name = Spy }

    let canMoveForward (ship: Ship) (grid: Sector Grid) : bool =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        false

    let moveForward (ship: Ship) : Ship =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        { Coords = []; Center = (0, 0); Facing = North; Name = Spy }

    let getNextDirection (current: Direction) (rotation: Rotation) : Direction =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        North

    let canRotateForward (ship: Ship) (rotation: Rotation) (grid: Sector Grid) : bool =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        false

    let rotateForward (ship: Ship) (rotation: Rotation) : Ship =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        { Coords = []; Center = (0, 0); Facing = North; Name = Spy }