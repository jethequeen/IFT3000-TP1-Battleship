namespace Battleship.Core

module Grid =

    type Dims = int * int

    type Coord = int * int

    type 'a Grid = Empty | Row of 'a list * 'a Grid
    
    
    let getGridCoords (dims: Dims) : Coord List =
        let i, j = dims
        
        let rec addRow x y (coordList : Coord List) =
            if i = x then coordList
            else
               addRow (x + 1) y (coordList @ [(x,y)])
        
        let rec addCoords y (coordList: Coord List) =
            if j = y then coordList
            else
                let row = addRow 0 y []
                addCoords (y + 1) (coordList @ row)
        
        addCoords 0 []
        
    let getDimsFromGrid (grid : 'a Grid) : Dims =
        let rec rowsCount grid count =
            match grid with
            | Empty -> count
            | Row (_, rest) -> rowsCount rest (count + 1)

        let rowLength =
            match grid with
            | Empty -> 0
            | Row (row, _) -> List.length row

        let width = rowLength
        let height = rowsCount grid 0
        (width, height)       
       
    
    let rec getSector (x: int) (y: int) (grid: 'a Grid) : 'a option =
        if x < 0 || y < 0 then None  // <-- ADD THIS LINE
        else
            match grid with
            | Empty -> None
            | Row (actualRowIndex, remainingRows) ->
                if y = 0 then
                    if x < List.length actualRowIndex then Some (List.item x actualRowIndex)
                    else None
                else
                    getSector x (y - 1) remainingRows
    
    let filterCoords (predicate: Coord -> 'a -> bool) (grid: 'a Grid) : Coord list =
        let dims = getDimsFromGrid grid
        getGridCoords dims
        |> List.filter (fun coord ->
            match getSector (fst coord) (snd coord) grid with
            | Some value -> predicate coord value
            | None -> false)