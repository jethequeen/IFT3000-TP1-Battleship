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
    
    // Fonction pour déterminer si le secteur est actif
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
        |> List.filter (fun coord -> 
            let x, y = coord
            let width, height = gridDims
            x >= 0 && x < width && y >= 0 && y < height &&
            not (isSectorActive(getSector x y grid))
        )
    
    // Fonction qui détermine si les coordonnées de la liste sont disponible
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
        invalidCoords.Length = 0   
    
    let coordsVerification (coords : Coord List) (grid: Sector Grid) : bool =
        let coordsVerification = isCoordsAvailable coords grid
        let insideGrid = isCoordsInsideGrid coords grid
        insideGrid && coordsVerification
        
    
    let canPlace (center: Coord) (direction: Direction) (name: Name) (grid: Sector Grid) : bool =
        let ship = createShip center direction name
        let isCoordsValid = coordsVerification ship.Coords grid
        let dims = getDimsFromGrid grid

        let otherShipsCoords =
            getGridCoords dims
            |> List.filter (fun (x, y) ->
                match getSector x y grid with
                | Some (Active (n, _)) -> n <> name
                | _ -> false)

        let otherShipsPerimeters =
            otherShipsCoords
            |> List.collect adjacentCells
            |> List.filter (fun (x, y) ->
                x >= 0 && y >= 0 && x < fst dims && y < snd dims &&
                not (List.contains (x, y) otherShipsCoords))
            |> List.distinct

        let forbiddenZones = otherShipsCoords @ otherShipsPerimeters

        let coordsColliding =
            ship.Coords |> List.filter (fun coord -> List.contains coord forbiddenZones)

        let doesNotCollide = coordsColliding.IsEmpty
        isCoordsValid && doesNotCollide




    let canMove (ship: Ship) (direction: Direction) (grid: Sector Grid) : bool =
        let newCenter = calculateNewCenter ship direction
        let newShip = createShip newCenter ship.Facing ship.Name
        let dims = getDimsFromGrid grid
        
        // Check if new coordinates are inside grid
        let insideGrid = isCoordsInsideGrid newShip.Coords grid
        
        // Check if new coordinates are available (excluding current ship position)
        let isAvailable = 
            newShip.Coords 
            |> List.forall (fun coord ->
                let sector = getSector (fst coord) (snd coord) grid
                match sector with
                | Some (Active (name, _)) -> name = ship.Name  // Allow if it's the same ship
                | Some Clear | None -> true
                | _ -> false
            )
        
        // Check collision with other ships and their perimeters
        let otherShipsCoords =
            getGridCoords dims
            |> List.filter (fun (x, y) ->
                match getSector x y grid with
                | Some (Active (n, _)) -> n <> ship.Name  // Exclude current ship
                | _ -> false)

        let otherShipsPerimeters =
            otherShipsCoords
            |> List.collect adjacentCells
            |> List.filter (fun (x, y) ->
                x >= 0 && y >= 0 && x < fst dims && y < snd dims &&
                not (List.contains (x, y) otherShipsCoords))
            |> List.distinct

        let forbiddenZones = otherShipsCoords @ otherShipsPerimeters
        let doesNotCollide = 
            newShip.Coords 
            |> List.forall (fun coord -> not (List.contains coord forbiddenZones))

        insideGrid && isAvailable && doesNotCollide
        
        
    let move (ship: Ship) (direction: Direction) : Ship =
        createShip (calculateNewCenter ship direction) ship.Facing ship.Name


    let canRotate (ship: Ship) (direction: Direction) (grid: Sector Grid) : bool =
        let newShip = createShip ship.Center direction ship.Name
        let dims = getDimsFromGrid grid

        let insideGrid = isCoordsInsideGrid newShip.Coords grid
        
        let isAvailable = 
            newShip.Coords 
            |> List.forall (fun coord ->
                let sector = getSector (fst coord) (snd coord) grid
                match sector with
                | Some (Active (name, _)) -> name = ship.Name
                | Some Clear | None -> true
            )
        
        let otherShipsCoords =
            getGridCoords dims
            |> List.filter (fun (x, y) ->
                match getSector x y grid with
                | Some (Active (n, _)) -> n <> ship.Name
                | _ -> false)

        let otherShipsPerimeters =
            otherShipsCoords
            |> List.collect adjacentCells
            |> List.filter (fun (x, y) ->
                x >= 0 && y >= 0 && x < fst dims && y < snd dims &&
                not (List.contains (x, y) otherShipsCoords))
            |> List.distinct

        let forbiddenZones = otherShipsCoords @ otherShipsPerimeters
        let doesNotCollide = 
            newShip.Coords 
            |> List.forall (fun coord -> not (List.contains coord forbiddenZones))

        insideGrid && isAvailable && doesNotCollide          

    
    let rotate (ship: Ship) (direction: Direction) : Ship =
        createShip ship.Center direction ship.Name

    
    let canMoveForward (ship: Ship) (grid: Sector Grid) : bool =
        coordsVerification (calculateNewCoordsForward ship) grid


    let moveForward (ship: Ship) : Ship =
        move ship ship.Facing

    
    let getNextDirection (current: Direction) (rotation: Rotation) : Direction =
        let direction =
            match rotation with
            | Clockwise ->
                match current with
                | North -> East
                | East -> South
                | South -> West
                | West -> North
            | Counterclockwise ->
                match current with
                | North -> West
                | West -> South
                | South -> East
                | East -> North
        direction

    let canRotateForward (ship: Ship) (rotation: Rotation) (grid: Sector Grid) : bool =
        let newDirection = getNextDirection ship.Facing rotation
        canMoveForward (rotate ship newDirection) grid

    let rotateForward (ship: Ship) (rotation: Rotation) : Ship =
        let newDirection = getNextDirection ship.Facing rotation 
        moveForward (rotate ship newDirection)
       