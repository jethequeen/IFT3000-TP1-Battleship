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

    let generateCoords (length: int) (coordsList : Coord List)
                           (facing: Direction) (center: Coord) : Coord List =
        
        let indexCenter =
            if length % 2 = 0 then (length / 2) else length / 2
        let (x,y) = center
        
        let rec nextCoords (length: int) (remainingCoordsList : Coord List)  =
            if length <= 0 then List.rev remainingCoordsList
            else
                let offset = (length) - 1 - indexCenter
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

    let createShip (center: Coord) (facing: Direction) (name: Name) : Ship =
        let length =
            match name with
            | Spy -> 2
            | PatrolBoat -> 2
            | Destroyer -> 3
            | Submarine -> 3
            | Cruiser -> 4
            | AircraftCarrier -> 5
        
        let coords = generateCoords length [] facing center
        { Coords = coords; Center = center; Facing = facing; Name = name }

    let adjacentCells (x,y) : Coord List =
        [ (x+1, y); (x, y+1); (x+1, y+1); (x-1, y); 
        (x, y-1); (x-1, y-1); (x+1, y-1); (x-1, y+1) ]
        
    let getAllAdjacentsCells (ship: Ship) : Coord List =
        ship.Coords
        |> List.collect adjacentCells
        |> List.distinct
    
    let getGridCoords (dims: Dims) : Coord List =
        let (i, j) = dims
        let x = 0
        let y = 0
        
        let rec addRow x y (coordList : Coord List) =
            if i = x then coordList
            else
               addRow (x + 1) y ((x, y) :: coordList)
        
        let rec addCoords y (coordList: Coord List) =
            if j = y then coordList
            else
                let row = addRow 0 y []
                addCoords (y + 1) (coordList @ row)
        
        addCoords 0 []
        
            
        
    let getPerimeter (ship: Ship) (dims: Dims) : Coord list =
        
        let gridCoords = getGridCoords dims
        let totalCoords = getAllAdjacentsCells ship
        
        totalCoords |> List.filter (fun coord -> not (List.contains coord ship.Coords))
                    |> List.filter (fun coord -> (List.contains coord gridCoords))
        
        

    

    
