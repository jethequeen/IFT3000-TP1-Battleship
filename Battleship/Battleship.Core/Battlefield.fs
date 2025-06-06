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
        
        //On créé une rangée de la bonne taille d'abord
        let createRow (width) : Sector List =
            List.init (width) (fun _ -> Clear)
            
        //On remplit notre grille du bon nombre de rangée
        let rec createGrid (width) (height) : Sector Grid =
            if height <= 0 then Empty
            else Row(createRow width, createGrid width (height - 1))
        
        createGrid (width) (height)

    let addShip (ship: Ship) (grid: Sector Grid) : Sector Grid =
        if not (coordsVerification ship.Coords grid) then
            failwith "Invalid ship placement: coordinates are either out of bounds or unavailable."

        // Associer coordonnées à un statut
        let shipMap =
            ship.Coords
            |> List.mapi (fun idx coord -> (coord, Active(ship.Name, idx)))
            |> Map.ofList

        let dims = getDimsFromGrid grid
        let coords = getGridCoords dims

        // Changer les bonnes valeurs de la grille
        let updatedValues =
            coords
            |> List.map (fun coord ->
                match Map.tryFind coord shipMap with
                | Some sector -> sector
                | None ->
                    match getSector (fst coord) (snd coord) grid with
                    | Some s -> s
                    | None -> Clear)

        // Reconstruire la liste
        let width, height = dims

        let rec build rows valuesLeft =
            match rows with
            | 0 -> Empty
            | _ ->
                let row = valuesLeft |> List.take width
                let rest = valuesLeft |> List.skip width
                Row (row, build (rows - 1) rest)

        build height updatedValues

    let replaceShip (ship: Ship) (grid: Sector Grid) : Sector Grid =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        Empty

    let getSelectedName (coord: Coord) (grid: Sector Grid) : Name option =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        None

    let extractData (grid: Sector Grid) : Data =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        { Dims = (0, 0); Ships = [] }

    let loadData (data: Data) : Sector Grid =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        Empty