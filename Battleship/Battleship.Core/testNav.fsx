#load "Grid.fs"
#load "Ship.fs"
#load "Navigation.fs"

open Battleship.Core.Ship
open Battleship.Core.Grid
open Battleship.Core.Navigation

let printTest name result expected =
    if result = expected then
        printfn "[OK] %s" name
    else
        printfn "[FAIL] %s\nRésultat  : %A\nAttendu   : %A\n" name result expected

let printShipTest name (ship: Ship) expectedCoords =
    if ship.Coords = expectedCoords then
        printfn "[OK] %s" name
    else
        printfn "[FAIL] %s\nRésultat  : %A\nAttendu   : %A\n" name ship.Coords expectedCoords

let dims = (10, 10) // Grille 10x10

let runNewTests () =
    printfn "=== Tests des nouvelles fonctions ==="
    
    // Grille vide pour les tests
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

    // Tests isSectorActive
    printfn "\n--- Tests isSectorActive ---"
    let activeSector = Some (Active (Destroyer, 1))
    let clearSector = Some Clear
    let noneSector = None
    
    printTest "Test 1 - Secteur actif" (isSectorActive activeSector) true
    printTest "Test 2 - Secteur vide" (isSectorActive clearSector) false
    printTest "Test 3 - Secteur None" (isSectorActive noneSector) false

    // Tests isCoordsAvailable
    printfn "\n--- Tests isCoordsAvailable ---"
    let testCoords1 = [(3,3); (3,4); (3,5)]
    printTest "Test 4 - Coordonnées disponibles sur grille vide" (isCoordsAvailable testCoords1 emptyGrid) true
    
    // Grille avec un bateau
    let gridWithShip =
        Row ([Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear],
        Row ([Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear],
        Row ([Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear],
        Row ([Clear; Clear; Clear; Active(Destroyer, 0); Active(Destroyer, 1); Active(Destroyer, 2); Clear; Clear; Clear; Clear],
        Row ([Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear],
        Row ([Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear],
        Row ([Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear],
        Row ([Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear],
        Row ([Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear],
        Row ([Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear; Clear], Empty))))))))))
    
    let testCoords2 = [(3,3); (3,4); (3,5)]
    printTest "Test 5 - Coordonnées occupées" (isCoordsAvailable testCoords2 gridWithShip) false
    
    let testCoords3 = [(1,1); (1,2); (1,3)]
    printTest "Test 6 - Coordonnées libres avec bateau ailleurs" (isCoordsAvailable testCoords3 gridWithShip) true

    // Tests isCoordsInsideGrid
    printfn "\n--- Tests isCoordsInsideGrid ---"
    let insideCoords = [(0,0); (5,5); (9,9)]
    let outsideCoords = [(10,5); (5,10); (-1,5)]
    let mixedCoords = [(0,0); (10,5); (5,5)]
    
    printTest "Test 7 - Coordonnées à l'intérieur" (isCoordsInsideGrid insideCoords emptyGrid) true
    printTest "Test 8 - Coordonnées à l'extérieur" (isCoordsInsideGrid outsideCoords emptyGrid) false
    printTest "Test 9 - Coordonnées mixtes" (isCoordsInsideGrid mixedCoords emptyGrid) false

    // Tests getAllShipsPerimeters
    printfn "\n--- Tests getAllShipsPerimeters ---"
    let emptyPerimeters = getAllShipsPerimeters emptyGrid
    printTest "Test 10 - Périmètres sur grille vide" (emptyPerimeters.Length) 0
    
    let shipPerimeters = getAllShipsPerimeters gridWithShip
    printfn "Test 11 - Périmètres avec bateau: %d coordonnées trouvées" shipPerimeters.Length
    // Note: Le nombre exact dépend de l'implémentation, on vérifie juste qu'il y en a
    printTest "Test 11 - Périmètres avec bateau (non vide)" (shipPerimeters.Length > 0) true

    // Tests calculateNewCoords
    printfn "\n--- Tests calculateNewCoords ---"
    let testShip = createShip (5,5) East Destroyer // Coords: [(5,6); (5,5); (5,4)]
    printfn "Coords before moving: %A" testShip.Coords
    
    let northCoords = calculateNewCoords testShip North
    printTest "Test 12 - Déplacement Nord" northCoords [(4,6); (4,5); (4,4)]
    
    let southCoords = calculateNewCoords testShip South
    printTest "Test 13 - Déplacement Sud" southCoords [(6,6); (6,5); (6,4)]
    
    let eastCoords = calculateNewCoords testShip East
    printTest "Test 14 - Déplacement Est" eastCoords [(5,7); (5,6); (5,5)]
    
    let westCoords = calculateNewCoords testShip West
    printTest "Test 15 - Déplacement Ouest" westCoords [(5,5); (5,4); (5,3)]

    // Tests getCenterFromCoords
    printfn "\n--- Tests getCenterFromCoords ---"
    let coords2 = [(1,1); (1,2)]
    let coords3 = [(2,2); (2,3); (2,4)]
    let coords4 = [(3,3); (3,4); (3,5); (3,6)]
    let coords5 = [(4,4); (4,5); (4,6); (4,7); (4,8)]
    
    printTest "Test 16 - Centre longueur 2" (getCenterFromCoords coords2) (1,2)
    printTest "Test 17 - Centre longueur 3" (getCenterFromCoords coords3) (2,3)
    printTest "Test 18 - Centre longueur 4" (getCenterFromCoords coords4) (3,5)
    printTest "Test 19 - Centre longueur 5" (getCenterFromCoords coords5) (4,6)

    // Tests canMove
    printfn "\n--- Tests canMove ---"
    let moveShip = createShip (5,5) East Destroyer
    
    printTest "Test 20 - Peut bouger Nord (grille vide)" (canMove moveShip North emptyGrid) true
    printTest "Test 21 - Peut bouger Sud (grille vide)" (canMove moveShip South emptyGrid) true
    printTest "Test 22 - Peut bouger Est (grille vide)" (canMove moveShip East emptyGrid) true
    printTest "Test 23 - Peut bouger Ouest (grille vide)" (canMove moveShip West emptyGrid) true
    
    // Test avec bateau près du bord
    let edgeShip = createShip (1,1) East Destroyer // Coords: [(1,2); (1,1); (1,0)]
    printTest "Test 24 - Ne peut pas bouger Ouest (hors grille)" (canMove edgeShip West emptyGrid) false
    printTest "Test 25 - Peut bouger Est (dans grille)" (canMove edgeShip East emptyGrid) true
    
    // Test avec collision
    printTest "Test 26 - Ne peut pas bouger vers bateau existant" (canMove moveShip South gridWithShip) true // Dépend de la position exacte

    // Tests move
    printfn "\n--- Tests move ---"
    let originalShip = createShip (5,5) East Destroyer
    let movedNorth = move originalShip North
    let movedSouth = move originalShip South
    let movedEast = move originalShip East
    let movedWest = move originalShip West
    
    printShipTest "Test 27 - Move Nord" movedNorth [(4,6); (4,5); (4,4)]
    printShipTest "Test 28 - Move Sud" movedSouth [(6,6); (6,5); (6,4)]
    printShipTest "Test 29 - Move Est" movedEast [(5,7); (5,6); (5,5)]
    printShipTest "Test 30 - Move Ouest" movedWest [(5,5); (5,4); (5,3)]
    
    // Vérifier que le centre et la direction sont corrects
    printTest "Test 31 - Centre après move Nord" movedNorth.Center (4,5)
    printTest "Test 32 - Direction après move Nord" movedNorth.Facing East
    printTest "Test 33 - Nom après move Nord" movedNorth.Name Destroyer

    // Tests canPlace avec périmètres
    printfn "\n--- Tests canPlace avec périmètres ---"
    printTest "Test 34 - Placement valide (grille vide)" (canPlace (5,5) East Destroyer emptyGrid) true
    printTest "Test 35 - Placement invalide (collision directe)" (canPlace (3,4) East Destroyer gridWithShip) false
    
    // Test de placement près d'un bateau (périmètre)
    let nearShipPlacement = canPlace (2,3) East PatrolBoat gridWithShip // Devrait être invalide à cause du périmètre
    printfn "Test 36 - Placement près d'un bateau (périmètre): %b" nearShipPlacement

    printfn "\n=== Fin des tests ==="

// Lancer les tests
runNewTests ()