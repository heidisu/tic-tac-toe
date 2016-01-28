module TicTacToe where

import Window
import Mouse
import List exposing (..)
import Color exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)

type alias Input = { userInput:(Int,Int), dimensions:(Int, Int) }

type Symbol = X | O | E
type Solution = H1 | H2 | H3 | V1 | V2 | V3 | D1 | D2

type alias Board = List (Int, Symbol)
type alias Solutions = List Solution
type alias GameState = {board: Board, player : Symbol, solutions : Solutions}

defaultGame : GameState
defaultGame = {board = [(0,E), (1,E), (2,E), (3,E), (4,E), (5,E), (6,E), (7,E), (8,E)], player = O, solutions = []}

solutions : List (Int, Int, Int, Solution)
solutions = [(0,1,2,H1), (3,4,5,H2), (6,7,8,H3), (0,3,6,V1), (1,4,7,V2), (2,5,8,V3),(0,4,8,D1), (2,4,6,D2)]

stepGame : Input -> GameState -> GameState
stepGame {userInput, dimensions} gameState = 
    if isEmpty gameState.solutions
    then
      let inSquare = inBoardSquare userInput dimensions
          brd = gameState.board
          plyr = gameState.player
      in
        case inSquare of
          -1 -> gameState
          x -> let val = filter (\(pos, symb) -> pos == x) brd
                            |> head
               in 
                 case val of
                   Just (_, E) -> let newBoard = (x, plyr) :: (filter (\(pos, symb) -> pos /= x) brd)
                        in {board = newBoard,
                            player = updatePlayer plyr,
                            solutions = concatMap (checkSolution newBoard) solutions}
                   _ -> gameState
    else gameState
     
inBoardSquare : (Int,Int) -> (Int, Int) -> Int
inBoardSquare (posX, posY) (width, height) = 
       let midX = toFloat width / 2
           midY = toFloat height / 2
           x = toFloat posX
           y = toFloat posY
       in 
            if  x < midX - 300 || x > midX + 300 then -1
            else if y < midY - 300 || y > midY + 300 then -1
            else if x >= midX - 300 && x <= midX - 100 then findYpos midY y 0
            else if x > midX - 100 && x <= midX + 100 then findYpos midY y 1
            else if x > midX + 100 && x <= midX + 300 then findYpos midY y 2
            else -1
                                   
findYpos : Float -> Float -> Int -> Int 
findYpos midY y col = 
    if y >= midY - 300 && y <= midY - 100 then col
    else if  y > midY - 100 && y <= midY + 100 then col + 3
    else if y > midY + 100 && y <= midY + 300 then col + 6
    else -1
                                                              
updatePlayer : Symbol -> Symbol
updatePlayer player = if player == O then X else O

checkSolution : List (Int, Symbol) -> (Int, Int, Int, Solution) -> List Solution
checkSolution board (x, y, z, sol) = let (indices, values) = unzip (filter (\(a, b) -> a == x || a == y || a == z) board)
                                         symbols = filter (\x -> x /= E) values
                                    in
                                    if length symbols == 3 && length (filter (\x -> Just x == head symbols) symbols) == 3
                                    then [sol]
                                    else []

boardColor = yellow
symbolColor = white
gridColor = red
solutionColor = purple 
gridLine = traced (solid gridColor) (path [(-300,0), (300, 0)])
solutionLine = rect 550 10 |> filled solutionColor
solutionDiagonal = rect 778 10 |> filled solutionColor

moveShape pos shape =
   shape |> case pos of 
            0 -> move (-200, 200)
            1 -> move (0, 200)
            2 -> move (200, 200)
            3 -> move (-200, 0)
            4 -> move (0,0)
            5 -> move (200, 0)
            6 -> move (-200,-200)
            7 -> move (0, -200)
            8 -> move (200, -200)
            _ -> move (0,0)

fillTile (pos,val) = 
   if val == O then oval 100 100 |> filled symbolColor
                                 |> moveShape pos
   else rect 100 100 |> filled symbolColor
                     |> moveShape pos
                                                                             
fillSolution val = 
    case val of 
      H1 -> solutionLine |> move (0, 200)
      H2 -> solutionLine
      H3 -> solutionLine |> move (0, -200)
      V1 -> (rotate (degrees 90) solutionLine) |> move (-200, 0)
      V2 -> (rotate (degrees 90) solutionLine)
      V3 -> (rotate (degrees 90) solutionLine) |> move (200, 0)
      D1 -> (rotate (degrees 135) solutionDiagonal)
      D2 -> (rotate (degrees 45) solutionDiagonal)

display : (Int,Int) -> GameState -> Element
display (w,h) {board, player, solutions} = 
    container w h middle <| collage 600 600
       ([ rect 600 600 |> filled boardColor
       , move (-100,0) (rotate (degrees 90) gridLine)
       , move (100,0) (rotate (degrees 90) gridLine)
       , move (0, -100) gridLine
       , move (0, 100) gridLine] ++
       map fillTile (filter (\(x,y) -> y /= E) board) ++
       map fillSolution solutions)

input = Signal.sampleOn Mouse.clicks (Signal.map2 Input Mouse.position Window.dimensions)

gameState = Signal.foldp stepGame defaultGame input

main = Signal.map2 display Window.dimensions gameState