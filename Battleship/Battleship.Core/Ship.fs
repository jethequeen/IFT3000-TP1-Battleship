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

    (* ------- À COMPLÉTER ------- *)
    (* --- Nouvelles fonctions --- *)
    
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

    let getPerimeter (ship: Ship) (dims: Dims) : Coord list =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        []
