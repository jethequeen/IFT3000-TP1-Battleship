namespace Battleship.Core

module Grid =

    type Dims = int * int

    type Coord = int * int

    type 'a Grid = Empty | Row of 'a list * 'a Grid

    (* ------- À COMPLÉTER ------- *)
    (* --- Nouvelles fonctions --- *)