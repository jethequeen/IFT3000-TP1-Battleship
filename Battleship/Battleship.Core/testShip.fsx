#load "Grid.fs"
#load "Ship.fs"
#load "Navigation.fs"

open Battleship.Core.Ship
open Battleship.Core.Grid
open Battleship.Core.Navigation

let printTest name result expected =
    if result = expected then
        printfn $"[OK] %s{name}"
    else
        printfn $"[FAIL] %s{name}\nRésultat  : %A{result}\nAttendu   : %A{expected}\n"

let printShipTest name (ship: Ship) expectedCoords =
    if ship.Coords = expectedCoords then
        printfn $"[OK] %s{name}"
    else
        printfn $"[FAIL] %s{name}\nRésultat  : %A{ship.Coords}\nAttendu   : %A{expectedCoords}\n"

let dims = (10, 10) // Grille 10x10

let runTests () =
    // Coords generation
    let coords1 = generateCoords (3,3) East Destroyer
    printTest "Test 1 - East (3,3), length 3" coords1 [(3,4); (3,3); (3,2)]

    let coords2 = generateCoords (3,3) North Destroyer
    printTest "Test 2 - North (3,3), length 3" coords2 [(2,3); (3,3); (4,3)]

    let coords3 = generateCoords (1,1) South PatrolBoat 
    printTest "Test 3 - South (1,1), length 2" coords3 [(1,1); (0,1)]

    let coords4 = generateCoords (3,3) East Cruiser
    printTest "Test 4 - East (3,3), length 4" coords4 [(3,4); (3,3); (3,2); (3,1)]

    let coords5 = generateCoords (3,3) North Cruiser
    printTest "Test 5 - North (3,3), length 4" coords5 [(2,3); (3,3); (4,3); (5,3)]

    let coords6 = generateCoords (3,3) West Cruiser
    printTest "Test 6 - West (3,3), length 4" coords6 [(3,2); (3,3); (3,4); (3,5)]

    let coords7 = generateCoords (3,3) South Cruiser
    printTest "Test 7 - South (3,3), length 4" coords7 [(4,3); (3,3); (2,3); (1,3)]

    let coords8 = generateCoords (2,2) East AircraftCarrier
    printTest "Test 8 - East (2,2), length 5" coords8 [(2,4); (2,3); (2,2); (2,1); (2,0)]

    let coords9 = generateCoords (3,5) North AircraftCarrier
    printTest "Test 9 - North (3,5), length 5" coords9 [(1,5); (2,5); (3,5); (4,5); (5,5)]

    let coords10 = generateCoords (0,0) West AircraftCarrier
    printTest "Test 10 - West (0,0), length 5" coords10 [(0,-2); (0,-1); (0,0); (0,1); (0,2)]

    let coords11 = generateCoords (2,2) South AircraftCarrier
    printTest "Test 11 - South (2,2), length 5" coords11 [(4,2); (3,2); (2,2); (1,2); (0,2)]

    // Ship creation tests
    let ship1 = createShip (3,3) East Destroyer
    printShipTest "Test 1 - Destroyer (3,3) East" ship1 [(3,4); (3,3); (3,2)]

    let ship2 = createShip (2,2) North Cruiser
    printShipTest "Test 2 - Cruiser (2,2) North" ship2 [(1,2); (2,2); (3,2); (4,2)]

    let ship3 = createShip (1,1) South PatrolBoat
    printShipTest "Test 3 - PatrolBoat (1,1) South" ship3 [(1,1); (0,1)]

    let ship4 = createShip (5,5) West AircraftCarrier
    printShipTest "Test 4 - AircraftCarrier (5,5) West" ship4 [(5,3); (5,4); (5,5); (5,6); (5,7)]

    // Périmètre
    let ship12 = createShip (5,5) North Cruiser
    let peri12 = getPerimeter ship12 dims
    printTest "Test 12 - Périmètre autour de (5,5) North Cruiser" peri12 peri12

    let ship13 = createShip (0,0) East PatrolBoat
    let peri13 = getPerimeter ship13 dims
    printTest "Test 13 - Périmètre autour de (0,0) East PatrolBoat" peri13 peri13

    // Placement tests
    let clearRow = [for _ in 1 .. 10 -> Clear]
    let emptyGrid =
        Row (clearRow,
        Row (clearRow,
        Row (clearRow,
        Row (clearRow,
        Row (clearRow,
        Row (clearRow,
        Row (clearRow,
        Row (clearRow,
        Row (clearRow,
        Row (clearRow, Empty))))))))))

    let grid14 = emptyGrid
    let result14 = canPlace (3,3) East Destroyer grid14
    printTest "Test 14 - Placement valide (Destroyer à (3,3), East)" result14 true

    let grid15 =
        Row ([Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear],
        Row ([Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear],
        Row ([Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear],
        Row ([Clear; Clear; Active(Destroyer, 1); Clear; Clear; Clear; Clear; Clear; Clear; Clear],
        Row ([Clear; Clear; Active(Destroyer, 1); Clear; Clear; Clear; Clear; Clear; Clear; Clear],
        Row ([Clear; Clear; Active(Destroyer, 1); Clear; Clear; Clear; Clear; Clear; Clear; Clear],
        Row ([Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear],
        Row ([Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear],
        Row ([Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear],
        Row ([Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear], Empty))))))))))

    let result15 = canPlace (2,4) North Submarine grid15
    printTest "Test 15 - Placement invalide (collision avec Destroyer déjà en (2,4))" result15 false

// Lancer les tests
runTests ()
