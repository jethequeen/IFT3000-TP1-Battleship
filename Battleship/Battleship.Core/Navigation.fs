namespace Battleship.Core

module Navigation =
    open Grid
    open Ship

    type Sector = Clear | Active of Name * int * bool | Torpedo

    type Rotation =
        | Clockwise
        | Counterclockwise

    let getDegrees (direction: Direction) : int =
        match direction with
        | South -> 0
        | West -> 90
        | North -> 180
        | East -> 270

    let isSectorActive sector : bool =
        match sector with
        | Some (Active _) -> true
        | _ -> false

    
    let getOtherShipsPerimetersAndCoords (grid: Sector Grid) (ship: Ship) : Coord list =
        let dims = getDimsFromGrid grid
       
        let otherShipsCoords =
            getGridCoords dims
            |> List.filter (fun (x, y) ->
                match getSector x y grid with
                | Some (Active (n, _, _)) -> n <> ship.Name
                | _ -> false)

        let otherShipsPerimeters =
            otherShipsCoords
            |> List.collect adjacentCells
            |> List.filter (fun (x, y) ->
                x >= 0 && y >= 0 && x < fst dims && y < snd dims &&
                not (List.contains (x, y) otherShipsCoords))
            |> List.distinct
        
        otherShipsCoords @ otherShipsPerimeters
    
    let isCoordsAvailable (coords: Coord List) (grid: Sector Grid) (shipName: Name) : bool =
        let rec statesVerification coords =
            match coords with
            | [] -> true
            | (x, y) :: rest ->
                let sector = getSector x y grid
                match sector with
                | Some (Active (name, _, _)) when name = shipName -> 
                    statesVerification rest
                | _ when isSectorActive sector -> 
                    false
                | _ -> 
                    statesVerification rest
        
        statesVerification coords
    
    let isCoordsInsideGrid (coords : Coord List) (grid: Sector Grid) : bool =
        let gridCoords = getGridCoords (getDimsFromGrid grid)
        let invalidCoords = coords |> List.filter (fun coord -> not (List.contains coord gridCoords))
        invalidCoords.Length = 0   
    
    let coordsVerification (coords : Coord List) (grid: Sector Grid) (shipName: Name) : bool =
        let coordsVerification = isCoordsAvailable coords grid shipName
        let insideGrid = isCoordsInsideGrid coords grid
        insideGrid && coordsVerification
        
        
    let canPlace (center: Coord) (direction: Direction) (name: Name) (grid: Sector Grid) : bool =
        let ship = createShip center direction name
        let isCoordsValid = coordsVerification ship.Coords grid name
        let forbiddenZones = getOtherShipsPerimetersAndCoords grid ship

        let coordsColliding =
            ship.Coords |> List.filter (fun coord -> List.contains coord forbiddenZones)

        let doesNotCollide = coordsColliding.IsEmpty
        isCoordsValid && doesNotCollide

    let canMove (ship: Ship) (direction: Direction) (grid: Sector Grid) : bool =
        let newCenter = calculateNewCenter ship direction
        let newShip = createShip newCenter ship.Facing ship.Name

        let insideGrid = isCoordsInsideGrid newShip.Coords grid
        if not insideGrid then
            false
        else
            let isAvailable = isCoordsAvailable newShip.Coords grid ship.Name
            let forbiddenZones = getOtherShipsPerimetersAndCoords grid ship
            let doesNotCollide = 
                newShip.Coords 
                |> List.forall (fun coord -> not (List.contains coord forbiddenZones))

            isAvailable && doesNotCollide
        
    let move (ship: Ship) (direction: Direction) : Ship =
        createShip (calculateNewCenter ship direction) ship.Facing ship.Name

    let canRotate (ship: Ship) (direction: Direction) (grid: Sector Grid) : bool =
        let newShip = createShip ship.Center direction ship.Name
        let insideGrid = isCoordsInsideGrid newShip.Coords grid
        let isAvailable = isCoordsAvailable newShip.Coords grid ship.Name
        let forbiddenZones = getOtherShipsPerimetersAndCoords grid ship
        
        let doesNotCollide = 
            newShip.Coords 
            |> List.forall (fun coord -> not (List.contains coord forbiddenZones))

        insideGrid && isAvailable && doesNotCollide          

    let rotate (ship: Ship) (direction: Direction) : Ship =
        createShip ship.Center direction ship.Name

    let canMoveForward (ship: Ship) (grid: Sector Grid) : bool =
        coordsVerification (calculateNewCoordsForward ship) grid ship.Name

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