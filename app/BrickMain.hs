module BrickMain where

import Brick.Main (App(..), defaultMain, neverShowCursor, resizeOrQuit, findClickedExtents)
import Brick.Types (Widget, BrickEvent(..), EventM, get, put)
import Brick.Widgets.Core (str, withAttr, clickable, (<+>), vBox)
import Brick.Widgets.Center (center)
import Brick.Widgets.Table (table, renderTable)
import Brick.Widgets.Border (border)
import Brick.AttrMap (AttrMap, attrMap, AttrName, attrName)
import Brick.Util (fg)

import qualified Graphics.Vty as V
import Graphics.Vty.Input.Events (Button(..))

import Data.Char (isDigit)

import TicTacToe (Grid, Player(..), size, emptyGrid, next, isWinner, fullBoard, move)

import qualified Debug.Trace as DT

data St = St
    { _grid :: Grid
    , _curPlayer :: Player
    , _cellToPlay :: String
    , _playable :: Bool
    , _statusMessage :: String
    } deriving (Eq, Show)

oAttr :: AttrName
oAttr = attrName "o"

bAttr :: AttrName
bAttr = attrName "b"

xAttr :: AttrName
xAttr = attrName "x"

getAttr :: Player -> AttrName
getAttr p = case p of
    O -> oAttr
    B -> bAttr
    X -> xAttr

outputAttr :: AttrName
outputAttr = attrName "output"

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (oAttr, fg V.blue)
    , (bAttr, fg V.black)
    , (xAttr, fg V.red)
    , (outputAttr, fg V.magenta)
    ]

drawCellToPlay :: String -> Widget ()
drawCellToPlay ctp =
    withAttr outputAttr
    (str "Cell Num: " <+> str ctp)

drawGridCell :: Int -> Player -> Widget ()
drawGridCell l B =
    withAttr bAttr $
    str (show l)
drawGridCell l p =
    withAttr (getAttr p) $
    str (show p)

drawGrid :: Grid -> Widget ()
drawGrid grid = renderTable (table labeledGrid)
    where
        toI c r = c + (r * size)
        labeledGrid =
            [ [ drawGridCell (toI c r) p | (p, c)   <- zip row  [0..] ] |
                                           (row, r) <- zip grid [0..] ]

drawCurPlayerBox :: Player -> Widget ()
drawCurPlayerBox p =
    withAttr outputAttr $
    border (str "Current Player: " <+> str (show p))

drawStatusBox :: String -> Widget ()
drawStatusBox s =
    withAttr outputAttr $
    border (str "Status: " <+> str s)

drawUI :: St -> [Widget ()]
drawUI s = [
    vBox [
        drawCellToPlay (_cellToPlay s),
        drawCurPlayerBox (_curPlayer s),
        drawGrid (_grid s),
        drawStatusBox (_statusMessage s)]]

play :: Int -> St -> St
play i s
    | _playable s = case move (_grid s) i (_curPlayer s) of
        Nothing -> s { _statusMessage = "Invalid Move!" }
        Just g  -> s { _grid = g
                     , _curPlayer = next (_curPlayer s)
                     , _statusMessage = ""
                     }
    | otherwise   = s

checkBoard :: St -> St
checkBoard s
    | isWinner O (_grid s) = s { _statusMessage = "Player O wins!", _playable = False }
    | isWinner X (_grid s) = s { _statusMessage = "Player X wins!", _playable = False }
    | fullBoard (_grid s)  = s { _statusMessage = "It's a draw!",   _playable = False }
    | otherwise            = s

handleEvent :: BrickEvent () () -> EventM () St ()
handleEvent (VtyEvent (V.EvKey V.KEnter _)) = do
    s <- get
    let s'  = play (read (_cellToPlay s)) s
    let s'' = checkBoard s'
    put (s'' { _cellToPlay = "" })
handleEvent bevent@(VtyEvent (V.EvKey (V.KChar 'q') _)) =
    resizeOrQuit bevent
handleEvent (VtyEvent (V.EvKey (V.KChar c) _))
    | isDigit c = do
        s <- get
        if _playable s
        then put (s { _cellToPlay = _cellToPlay s ++ [c] })
        else put s
handleEvent bevent = DT.trace (show bevent) $ return ()

app :: App St () ()
app = App
    { appDraw         = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent  = handleEvent
    , appStartEvent   = return ()
    , appAttrMap      = const theMap
    }

run :: IO ()
run = do
    let s = St emptyGrid O "" True ""
    _ <- defaultMain app s
    return ()