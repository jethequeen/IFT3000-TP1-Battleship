namespace Battleship.Core

module Ship =
    open Grid

    type Name =
        | Spy
        | PatrolBoat
        | Destroyer
        | Submarine
        | Cruiser
        | AircraftCarrier

    type Direction =
        | North
        | South
        | East
        | West

    type Ship = {Coords: Coord list; Center: Coord; Facing: Direction; Name: Name}

    (* --- Helper functions --- *)
    
    let getLength (name: Name) : int =
        match name with
        | Spy -> 2
        | PatrolBoat -> 2
        | Destroyer -> 3
        | Submarine -> 3
        | Cruiser -> 4
        | AircraftCarrier -> 5
    
    let getIndexOfCenter (length: int) : int =
        match length with
        | 2 -> 1
        | 3 -> 1
        | 4 -> 2
        | 5 -> 2
        | _ -> length/2 - 1

    let getGenerationOffset (direction: Direction) (offset: int) : (int * int) =
        match direction with
        | North -> (-offset, 0)  
        | South -> (offset, 0)   
        | East  -> (0, offset)    
        | West  -> (0, -offset)   

    let getMovementOffset (direction: Direction) : (int * int) =
        match direction with
        | North -> (-1, 0) 
        | South -> (1, 0)         
        | East  -> (0, 1)        
        | West  -> (0, -1)        

    let applyOffset (x: int, y: int) (dx: int, dy: int) : Coord =
        (x + dx, y + dy)

    let generateCoords (center: Coord) (facing: Direction) (name: Name) : Coord List =
        let length = getLength name
        let indexCenter = getIndexOfCenter length
        let x, y = center
        
        [0 .. length - 1]
        |> List.map (fun currentIndex ->
            let offset = currentIndex - indexCenter
            let dx, dy = getGenerationOffset facing offset
            applyOffset (x, y) (dx, dy))

    let adjacentCells (x, y) : Coord List =
        [ (x+1, y); (x, y+1); (x+1, y+1); (x-1, y); 
          (x, y-1); (x-1, y-1); (x+1, y-1); (x-1, y+1) ]
        
    let getAllAdjacentsCells (ship: Ship) : Coord List =
        ship.Coords
        |> List.collect adjacentCells
        |> List.distinct
 
    let getCenterFromCoords (coords : Coord list) : Coord =
        let length = coords.Length
        let indexCenter = getIndexOfCenter length
        coords[indexCenter]
        
    let calculateNewCoords (ship: Ship) (direction: Direction) : Coord List =
        let dx, dy = getMovementOffset direction
        ship.Coords
        |> List.map (fun coord -> applyOffset coord (dx, dy))
    
    let calculateNewCoordsForward (ship: Ship) : Coord List =
        calculateNewCoords ship ship.Facing
    
    let calculateNewCenter (ship: Ship) (direction: Direction) : Coord =
        getCenterFromCoords (calculateNewCoords ship direction)   

    (* ------- À COMPLÉTER ------- *)
    let createShip (center: Coord) (facing: Direction) (name: Name) : Ship =
        let coords = generateCoords center facing name
        { Coords = coords; Center = center; Facing = facing; Name = name }

    let getPerimeter (ship: Ship) (dims: Dims) : Coord list =
        let gridCoords = getGridCoords dims
        let adjacentCoords = getAllAdjacentsCells ship

        adjacentCoords 
        |> List.filter (fun coord -> 
            not (List.contains coord ship.Coords) && 
            List.contains coord gridCoords)