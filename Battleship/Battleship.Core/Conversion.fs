namespace Battleship.Core

module Conversion =
    open Ship

    type Color =
        | Red
        | Lime
        | Aqua
        | Blue
        | Green
        | Purple

    let nameToColor (n: Name) : Color =
        match n with
        | Spy -> Red
        | PatrolBoat -> Lime
        | Destroyer -> Aqua
        | Submarine -> Blue
        | Cruiser -> Green
        | AircraftCarrier -> Purple

    let colorToName (c: Color) : Name =
        match c with
        | Red -> Spy
        | Lime -> PatrolBoat
        | Aqua -> Destroyer
        | Blue -> Submarine
        | Green -> Cruiser
        | Purple -> AircraftCarrier

    let colorToString (c: Color) : string =
        match c with
        | Red -> "Red"
        | Lime -> "Lime"
        | Aqua -> "Aqua"
        | Blue -> "Blue"
        | Green -> "Green"
        | Purple -> "Purple"

    let stringToColor (s: string) : Color =
        match s with
        | "Red" -> Red
        | "Lime" -> Lime
        | "Aqua" -> Aqua
        | "Blue" -> Blue
        | "Green" -> Green
        | "Purple" -> Purple
        | _ -> failwith "Unknown color."

    let nameToString (n: Name) : string =
        match n with
        | Spy -> "Spy"
        | PatrolBoat -> "Patrol Boat"
        | Destroyer -> "Destroyer"
        | Submarine -> "Submarine"
        | Cruiser -> "Cruiser"
        | AircraftCarrier -> "Aircraft Carrier"

    let stringToName (s: string) : Name =
        match s with
        | "Spy" -> Spy
        | "Patrol Boat" -> PatrolBoat
        | "Destroyer" -> Destroyer
        | "Submarine" -> Submarine
        | "Cruiser" -> Cruiser
        | "Aircraft Carrier" -> AircraftCarrier
        | _ -> failwith "Unknown name."

    let directionToString (d: Direction) : string =
        match d with
        | North -> "North"
        | South -> "South"
        | East -> "East"
        | West -> "West"

    let stringToDirection (s: string) : Direction =
        match s with
        | "North" -> North
        | "South" -> South
        | "East" -> East
        | "West" -> West
        | _ -> failwith "Unknown direction."