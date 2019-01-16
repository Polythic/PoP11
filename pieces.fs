module Pieces
open Chess
/// A king is a chessPiece which moves 1 square in any direction
type king(col : Color) =
  inherit chessPiece(col)
  override this.nameOfType = "king"
  // king has runs of 1 in 8 directions: (N, NE, E, SE, S, SW, W, NW)
  override this.candiateRelativeMoves =
      [[(-1,0)];[(-1,1)];[(0,1)];[(1,1)];
      [(1,0)];[(1,-1)];[(0,-1)];[(-1,-1)]]
  // king cannot move to threatened squares
  override this.availableMoves (board: Board) : (Position list * chessPiece list) =
    let allMoves: (Position list * chessPiece list) = board.getVacantNNeighbours this
    let mutable validMoves, piecesList = allMoves
    //let piecesList = snd allMoves
    let mutable listOfPieces: chessPiece list = []
    for i = 0 to 7 do
      for j = 0 to 7 do
        if (i,j) <> this.position.Value then
          let boardPiece: chessPiece option = board.Item(i,j)
          if boardPiece.IsSome then
            listOfPieces <- boardPiece.Value :: listOfPieces

    let rec remove index list =
      match index, list with
      | 0, x::xs -> xs
      | i, x::xs -> x::remove (index - 1) xs
      | i, [] -> failwith "Index out of range"

    let opponentColour = if col = White then Black else White
    for i in listOfPieces do
      for j in validMoves do
        let colorOfI: Color = i.color
        if colorOfI = opponentColour then
          let moves = fst (i.availableMoves board)
          for k in moves do
            if j = k then
              let jIndex = List.findIndex (fun x -> x = j) validMoves
              validMoves<- remove jIndex validMoves
    (validMoves, piecesList)

/// A rook is a chessPiece which moves horisontally and vertically
type rook(col : Color) =
  inherit chessPiece(col)
  // rook can move horisontally and vertically
  // Make a list of relative coordinate lists. We consider the
  // current position and try all combinations of relative moves
  // (1,0); (2,0) ... (7,0); (-1,0); (-2,0); ...; (0,-7).
  // Some will be out of board, but will be assumed removed as
  // illegal moves.
  // A list of functions for relative moves
  let indToRel = [
    fun elm -> (elm,0); // South by elm
    fun elm -> (-elm,0); // North by elm
    fun elm -> (0,elm); // West by elm
    fun elm -> (0,-elm) // East by elm
    ]
  // For each function in indToRel, we calculate List.map f [1..7].
  // swap converts (List.map fct indices) to (List.map indices fct).
  let swap f a b = f b a
  override this.candiateRelativeMoves =
    List.map (swap List.map [1..7]) indToRel (*//§\label{chessPieceSwapApp}§*)
  override this.nameOfType = "rook"
