#load "Grid.fs"
#load "Ship.fs"

open Battleship.Core.Ship
open Battleship.Core.Grid

let printTest name result expected =
    printfn "%s: %A" name result
    printfn "Attendu : %A\n" expected

// === Test 1 ===
let coords1 = generateCoords 3 [] East (3,3)
printTest "Test 1 - East (3,3), length 3" coords1 [(3,4); (3,3); (3,2)]

// === Test 2 ===
let coords2 = generateCoords 3 [] North (3,3)
printTest "Test 2 - North (3,3), length 3" coords2 [(2,3); (3,3); (4,3)]

// === Test 3 ===
let coords3 = generateCoords 2 [] South (1,1)
printTest "Test 3 - South (1,1), length 2" coords3 [(1,1); (0,1)]

// === Test 4 ===
let coords4 = generateCoords 4 [] East (3,3)
printTest "Test 4 - East (3,3), length 4" coords4 [(3,4); (3,3); (3,2); (3,1)]

// === Test 5 ===
let coords5 = generateCoords 4 [] North (3,3)
printTest "Test 5 - North (3,3), length 4" coords5 [(2,3); (3,3); (4,3); (5,3)]

// === Test 6 ===
let coords6 = generateCoords 4 [] West (3,3)
printTest "Test 6 - West (3,3), length 4" coords6 [(3,2); (3,3); (3,4); (3,5)]

// === Test 7 ===
let coords7 = generateCoords 4 [] South (3,3)
printTest "Test 7 - South (3,3), length 4" coords7 [(4,3); (3,3); (2,3); (1,3)]

// === Test 8 ===
let coords8 = generateCoords 5 [] East (2,2)
printTest "Test 8 - East (2,2), length 5" coords8 [(2,4); (2,3); (2,2); (2,1); (2,0)]

// === Test 9 ===
let coords9 = generateCoords 5 [] North (3,5)
printTest "Test 9 - North (3,5), length 5" coords9 [(1,5); (2,5); (3,5); (4,5); (5,5)]

// === Test 10 ===
let coords10 = generateCoords 5 [] West (0,0)
printTest "Test 10 - West (0,0), length 5" coords10 [(0,-2); (0,-1); (0,0); (0,1); (0,2)]

// === Test 11 ===
let coords11 = generateCoords 5 [] South (2,2)
printTest "Test 11 - South (2,2), length 5" coords11 [(4,2); (3,2); (2,2); (1,2); (0,2)]
