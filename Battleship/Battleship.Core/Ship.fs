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
    
    let rec generateCoords (remaining: int) (coordsList : Coord List)
                           (facing: Direction) (center: Coord) : Coord List =
        match remaining with
        | 0 -> coordsList
        | _ ->
            let (x,y) = center
            let nextCoord =
                match facing with
                | North -> (x, y-1)
                | South -> (x, y+1)
                | East  -> (x+1, y)
                | West -> (x-1, y)
                    
            generateCoords (remaining-1) (nextCoord :: coordsList) facing center

    let createShip (center: Coord) (facing: Direction) (name: Name) : Ship =
        let length =
            match name with
            | Spy -> 2
            | PatrolBoat -> 2
            | Destroyer -> 3
            | Submarine -> 4
            | Cruiser -> 4
            | AircraftCarrier -> 5
        
        let coords = generateCoords length [] facing center
        { Coords = coords; Center = center; Facing = facing; Name = name }

    let getPerimeter (ship: Ship) (dims: Dims) : Coord list =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        []
