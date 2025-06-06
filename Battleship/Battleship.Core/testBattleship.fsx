#load "Grid.fs"
#load "Ship.fs"
#load "Navigation.fs"
#load "Battlefield.fs"

open Battleship.Core.Grid
open Battleship.Core.Navigation
open Battleship.Core.Battlefield

let printTest name result expected =
    if result = expected then
        printfn $"[OK] %s{name}"
    else
        printfn $"[FAIL] %s{name}\nRésultat  : %A{result}\nAttendu   : %A{expected}\n"


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
    
    
    printfn "\n=== Fin des tests ==="

// Lancer les tests
runNewTests ()
