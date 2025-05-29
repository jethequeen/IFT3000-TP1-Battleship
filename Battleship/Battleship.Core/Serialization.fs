namespace Battleship.Core

module Serialization =
    open System.Text.Json
    open Grid
    open Ship
    open Conversion
    open Battlefield

    type SerializableShip = {Coords: Coord list; Center: Coord; Facing: string; Name: string}

    type SerializableData = { Dims: Dims; Ships: SerializableShip list }

    let serializeShip (ship: Ship) : SerializableShip =
        {
            Coords = ship.Coords;
            Center = ship.Center;
            Facing = Conversion.directionToString ship.Facing;
            Name = Conversion.nameToString ship.Name;
        }

    let deserializeShip (ship: SerializableShip) : Ship =
        {
            Coords = ship.Coords;
            Center = ship.Center;
            Facing = Conversion.stringToDirection ship.Facing;
            Name = Conversion.stringToName ship.Name
        }

    let getDump (data: Data) : string =
        let serializedShips = List.foldBack (fun ship acc -> (serializeShip ship)::acc) data.Ships []
        let serializedData = { SerializableData.Dims = data.Dims; Ships = serializedShips }
        let options = JsonSerializerOptions(WriteIndented = true, IndentSize = 4);
        JsonSerializer.Serialize(serializedData, options)

    let loadData (dump: string) : Data =
        let data = JsonSerializer.Deserialize<SerializableData>(dump)
        let deserializedShips = List.foldBack (fun ship acc -> (deserializeShip ship)::acc) data.Ships []
        { Data.Dims = data.Dims; Ships = deserializedShips }

