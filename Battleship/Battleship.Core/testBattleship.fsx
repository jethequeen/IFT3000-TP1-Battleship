#load "Grid.fs"
#load "Ship.fs"
#load "Navigation.fs"
#load "Battlefield.fs"

open Battleship.Core.Grid
open Battleship.Core.Navigation
open Battleship.Core.Battlefield
open Battleship.Core.Ship

let printTest name result expected =
    if result = expected then
        printfn $"[OK] %s{name}"
    else
        printfn $"[FAIL] %s{name}\nRésultat  : %A{result}\nAttendu   : %A{expected}\n"

// Fonction qui permet de comparer des listes de coordonnées peu importe l'ordre de celles-ci
let printCoordsTest name result expected =
    let resultSorted = List.sort result
    let expectedSorted = List.sort expected
    if resultSorted = expectedSorted then
        printfn $"[OK] %s{name}"
    else
        printfn $"[FAIL] %s{name}\nRésultat  : %A{result}\nAttendu   : %A{expected}"
let runNewTests () =
    printfn "=== Tests des nouvelles fonctions ==="

    
    let expectedGrid =
        Row ([Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear],
        Row ([Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear],
        Row ([Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear],
        Row ([Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear],
        Row ([Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear],
        Row ([Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear],
        Row ([Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear],
        Row ([Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear],
        Row ([Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear],
        Row ([Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear], Empty))))))))))

    let resultGrid = initClearGrid (10, 10)
    printTest "Test 1 - initClearGrid crée une grille 10x10" resultGrid expectedGrid
    
    let expectedGrid2 =
        Row ([for _ in 1..15 -> Clear],
        Row ([for _ in 1..15 -> Clear],
        Row ([for _ in 1..15 -> Clear],
        Row ([for _ in 1..15 -> Clear],
        Row ([for _ in 1..15 -> Clear],
        Row ([for _ in 1..15 -> Clear],
        Row ([for _ in 1..15 -> Clear],
        Row ([for _ in 1..15 -> Clear],
        Row ([for _ in 1..15 -> Clear],
        Row ([for _ in 1..15 -> Clear],
        Row ([for _ in 1..15 -> Clear],
        Row ([for _ in 1..15 -> Clear], Empty))))))))))))
    
    let resultGrid2 = initClearGrid (15, 12)
    printTest "Test 2 - initClearGrid crée une grille 15x12" resultGrid2 expectedGrid2
    
    let expectedGrid3 = Empty
    let resultGrid3 = initClearGrid (0,0)
    printTest "Test 3 - initClearGrid crée une grille 0x0" resultGrid3 expectedGrid3
    
    // Test 4
    let grid = initClearGrid (10, 10)
    let ship4 = createShip (2,2) East Destroyer
    let gridWithShip = addShip ship4 grid

    let coordsWithShip =
        filterCoords (fun _ sector -> match sector with | Active _ -> true | _ -> false) gridWithShip

    printCoordsTest "Test 4 - addShip ajoute bien le Destroyer sur la grille" coordsWithShip ship4.Coords

    // Test 5
    let newShip = createShip (1,1) South Destroyer
    let gridReplaced = replaceShip newShip gridWithShip

    let coordsAfterReplace =
        filterCoords (fun _ sector -> match sector with | Active _ -> true | _ -> false) gridReplaced

    printCoordsTest "Test 5 - replaceShip remplace correctement le Destroyer" coordsAfterReplace newShip.Coords
    
    // Test 6
    let grid6 = initClearGrid (10, 10)
    let ship6 = createShip (4, 2) West AircraftCarrier
    let gridWithShip6 = addShip ship6 grid6

    let coordsWithShip6 =
        filterCoords (fun _ sector -> match sector with | Active _ -> true | _ -> false) gridWithShip6

    printCoordsTest "Test 6 - addShip ajoute AircraftCarrier à (4,2) West" coordsWithShip6 ship6.Coords

    // Test 7
    let grid7 = initClearGrid (10, 10)
    let ship7 = createShip (5,5) South Cruiser
    let gridWithShip7 = addShip ship7 grid7

    let coordsWithShip7 =
        filterCoords (fun _ sector -> match sector with | Active _ -> true | _ -> false) gridWithShip7

    printCoordsTest "Test 7 - addShip ajoute Cruiser à (5,5) South" coordsWithShip7 ship7.Coords

    // Test 8
    let ship8Old = createShip (4,4) South PatrolBoat
    let grid8Old = addShip ship8Old (initClearGrid (10,10))
    let ship8New = createShip (4,4) East PatrolBoat
    let grid8Replaced = replaceShip ship8New grid8Old

    let coordsWithShip8 =
        filterCoords (fun _ sector -> match sector with | Active _ -> true | _ -> false) grid8Replaced

    printCoordsTest "Test 8 - replaceShip change l’orientation de South à East" coordsWithShip8 ship8New.Coords

    // Test 9
    let ship9Old = createShip (6,6) North Submarine
    let grid9Old = addShip ship9Old (initClearGrid (10,10))
    let ship9New = createShip (1,6) East Submarine
    let grid9Replaced = replaceShip ship9New grid9Old

    let coordsWithShip9 =
        filterCoords (fun _ sector -> match sector with | Active _ -> true | _ -> false) grid9Replaced

    printCoordsTest "Test 9 - replaceShip déplace Submarine sans chevauchement" coordsWithShip9 ship9New.Coords
    
    printfn "\n=== Fin des tests ==="

// Lancer les tests
runNewTests ()
