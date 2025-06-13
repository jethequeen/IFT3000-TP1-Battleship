namespace Battleship.Core

module Battlefield =
    open Grid
    open Ship
    open Navigation

    type Data = { Dims: Dims; Ships: Ship list }

    (* ------- À COMPLÉTER ------- *)
    (* --- Nouvelles fonctions --- *)

    let initClearGrid (dims: Dims) : Sector Grid =
        let width, height = dims      
        let createRow (width) : Sector List =
            List.init (width) (fun _ -> Clear)

        let rec createGrid (width) (height) : Sector Grid =
            if height <= 0 then Empty
            else Row(createRow width, createGrid width (height - 1))
        
        createGrid (width) (height)
        
    let buildGridFromValues (width: int) (height: int) (values: 'a list) : 'a Grid =
        let rec build rows remainingValues =
            match rows with
            | 0 -> Empty
            | _ ->
                let row = remainingValues |> List.take width
                let rest = remainingValues |> List.skip width
                Row (row, build (rows - 1) rest)

        build height values
    let updateGridWithMap (updates: Map<Coord, Sector>) (grid: Sector Grid) : Sector list =
        let dims = getDimsFromGrid grid
        let coords = getGridCoords dims

        coords
        |> List.map (fun coord ->
            match Map.tryFind coord updates with
            | Some sector -> sector
            | None ->
                match getSector (fst coord) (snd coord) grid with
                | Some s -> s
                | None -> Clear)
    
    
    let addShip (ship: Ship) (grid: Sector Grid) : Sector Grid =
        if not (coordsVerification ship.Coords grid ship.Name) then
            failwith "Invalid ship placement: coordinates are either out of bounds or unavailable."
        let shipMap =
            ship.Coords
            |> List.mapi (fun idx coord -> (coord, Active(ship.Name, idx)))
            |> Map.ofList
        let updatedValues = updateGridWithMap shipMap grid
        let width, height = getDimsFromGrid grid

        buildGridFromValues width height updatedValues
    

    let getCoordsOfShip (name: Name) (grid: Sector Grid) : Coord list =
        filterCoords (fun _ sector ->
            match sector with
            | Active (n, _) when n = name -> true
            | _ -> false) grid
        
        
    let replaceShip (ship: Ship) (grid: Sector Grid) : Sector Grid =
        let oldCoords = getCoordsOfShip ship.Name grid

        let clearMap =
            oldCoords
            |> List.map (fun coord -> (coord, Clear))
            |> Map.ofList

        let clearedGridValues = updateGridWithMap clearMap grid
        let width, height = getDimsFromGrid grid
        let clearedGrid = buildGridFromValues width height clearedGridValues

        addShip ship clearedGrid

    let getSelectedName (coord: Coord) (grid: Sector Grid) : Name option =
        match getSector (fst coord) (snd coord) grid with
        | Some (Active (name, _)) -> Some name
        | _ -> None

    let determineFacing (coords: Coord list) : Direction =
        match coords with
        | (x1, y1) :: (x2, y2) :: _ ->
            match (x2 - x1, y2 - y1) with
            | dx, _ when dx < 0 -> North
            | dx, _ when dx > 0 -> South
            | _, dy when dy < 0 -> West
            | _, dy when dy > 0 -> East
            | _ -> North
        | _ -> North
        
    let extractData (grid: Sector Grid) : Data =
        let dims = getDimsFromGrid grid

        let shipParts =
            getGridCoords dims
            |> List.choose (fun coord ->
                match getSector (fst coord) (snd coord) grid with
                | Some (Active (name, index)) -> Some (name, coord, index)
                | _ -> None)

        let ships =
            shipParts
            |> List.groupBy (fun (name, _, _) -> name)
            |> List.map (fun (shipName, parts) ->
                let shipCoords =
                    parts
                    |> List.sortBy (fun (_, _, index) -> index)
                    |> List.map (fun (_, coord, _) -> coord)
                
                if List.isEmpty shipCoords then
                    failwith $"Cannot build ship {shipName} with no coordinates."
                else
                    { Name = shipName
                      Coords = shipCoords
                      Center = Ship.getCenterFromCoords shipCoords
                      Facing = determineFacing shipCoords
                    } : Ship)
        { Dims = dims; Ships = ships }

    let loadData (data: Data) : Sector Grid =
        let initialGrid = initClearGrid data.Dims
        
        List.fold (fun currentGrid shipToAdd ->
            addShip shipToAdd currentGrid
        ) initialGrid data.Ships
                 