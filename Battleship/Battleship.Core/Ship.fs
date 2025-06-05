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

    (* --- Nouvelles fonctions --- *)
       
    let getLength (name: Name) : int =
        let length =
            match name with
            | Spy -> 2
            | PatrolBoat -> 2
            | Destroyer -> 3
            | Submarine -> 3
            | Cruiser -> 4
            | AircraftCarrier -> 5
        length
        
    let generateCoords (center: Coord) (facing: Direction) (name: Name) : Coord List =
        let length = getLength name
        let indexCenter =
            if length % 2 = 0 then (length / 2) else length / 2
        let x,y = center
        
        let rec nextCoords (length: int) (remainingCoordsList : Coord List)  =
            if length <= 0 then List.rev remainingCoordsList
            else
                let offset = length - 1 - indexCenter
                if offset = 0
                    then nextCoords (length-1) (center :: remainingCoordsList)
                else
                    let coord =
                        match facing with
                        | North -> (x-offset, y)
                        | South -> (x+offset, y)
                        | East  -> (x, y+offset)
                        | West -> (x, y-offset)
                    nextCoords (length-1) (coord :: remainingCoordsList)
                
        nextCoords length []
        
        
    let adjacentCells (x,y) : Coord List =
        [ (x+1, y); (x, y+1); (x+1, y+1); (x-1, y); 
        (x, y-1); (x-1, y-1); (x+1, y-1); (x-1, y+1) ]
        
    let getAllAdjacentsCells (ship: Ship) : Coord List =
        ship.Coords
        |> List.collect adjacentCells
        |> List.distinct
 
    let getCenterFromCoords (coords : Coord list) : Coord =
        let length = coords.Length
        let centerIndex = 
            if length % 2 = 0 then (length / 2) else length / 2       
        coords[centerIndex]
        
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
        let totalCoords = getAllAdjacentsCells ship

        totalCoords |> List.filter (fun coord -> not (List.contains coord ship.Coords))
                    |> List.filter (fun coord -> (List.contains coord gridCoords))
        
        

    

    
