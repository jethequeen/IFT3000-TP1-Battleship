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
        if not (coordsVerification ship.Coords grid) then
            failwith "Invalid ship placement: coordinates are either out of bounds or unavailable."
        // Associer coordonnées à un statut
        let shipMap =
            ship.Coords
            |> List.mapi (fun idx coord -> (coord, Active(ship.Name, idx)))
            |> Map.ofList
        // Changer les bonnes valeurs de la grille
        let updatedValues = updateGridWithMap shipMap grid
        // Reconstruire la liste
        let width, height = getDimsFromGrid grid

        buildGridFromValues width height updatedValues
    
    
    //Trouver les coordonnées sur la grille qui sont occupées par un bateau spécifique
    let getCoordsOfShip (name: Name) (grid: Sector Grid) : Coord list =
        filterCoords (fun _ sector ->
            match sector with
            | Active (n, _) when n = name -> true
            | _ -> false) grid
        
        
    let replaceShip (ship: Ship) (grid: Sector Grid) : Sector Grid =
        // Déterminer les coordonnées actuelles du bateau sur la grille
        let oldCoords = getCoordsOfShip ship.Name grid

        // Construire un dictionaire qui sélectionne les valeurs cleared
        let clearMap =
            oldCoords
            |> List.map (fun coord -> (coord, Clear))
            |> Map.ofList
        
        // On utilise le dictionnaire pour supprimer "l'ancien" bateau
        let clearedGridValues = updateGridWithMap clearMap grid
        let width, height = getDimsFromGrid grid
        let clearedGrid = buildGridFromValues width height clearedGridValues

        // Ajouter le bateau aux nouvelles coordonnées
        addShip ship clearedGrid

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
                 