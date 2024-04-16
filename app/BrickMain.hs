module BrickMain where

import Brick.Main (App(..), defaultMain, neverShowCursor, resizeOrQuit, findClickedExtents)
import Brick.Types (Widget, BrickEvent(..), EventM, get, put)
import Brick.Widgets.Core (str, withAttr, clickable, (<+>), vBox, hBox)
import Brick.Widgets.Center (center)
import Brick.Widgets.Table (table, renderTable)
import Brick.Widgets.Border (border, hBorder, vBorder)
import Brick.AttrMap (AttrMap, attrMap, AttrName, attrName)
import Brick.Util (fg)

import qualified Graphics.Vty as V
import Graphics.Vty.Input.Events (Button(..))

import Data.Char (isDigit)
import Data.List (intersperse, intercalate)

import TicTacToe (Grid, Player(..), size, emptyGrid, next, isWinner, fullBoard, move)

import qualified Debug.Trace as DT

data InputMode = Typing | Arrowing
    deriving (Eq, Show)

data St = St
    { _grid :: Grid
    , _curPlayer :: Player
    , _inputMode :: InputMode
    , _textCell :: String
    , _arrowCell :: Int
    , _playable :: Bool
    , _statusMessage :: String
    } deriving (Eq, Show)

oAttr :: AttrName
oAttr = attrName "o"

bAttr :: AttrName
bAttr = attrName "b"

xAttr :: AttrName
xAttr = attrName "x"

outputAttr :: AttrName
outputAttr = attrName "output"

selectedAttr :: AttrName
selectedAttr = attrName "selected"

getAttr :: Player -> AttrName
getAttr p = case p of
    O -> oAttr
    B -> bAttr
    X -> xAttr

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (oAttr, fg V.green)
    , (bAttr, fg V.black)
    , (xAttr, fg V.red)
    , (outputAttr, fg V.magenta)
    , (selectedAttr, fg V.cyan)
    ]

drawCellToPlay :: String -> Widget ()
drawCellToPlay ctp =
    withAttr outputAttr
    (str "Cell Num: " <+> str ctp)

drawGridCell :: Bool -> Int -> Player -> Widget ()
drawGridCell sel i p =
    withAttr atr $
    str (txt pst)
    where
        atr = if sel then selectedAttr else getAttr p
        txt = if sel then ("→" ++) else pad 2 ' '
        pst = if p == B then show i else show p

pad :: Int -> a -> [a] -> [a]
pad i a as
    | lenAs < i = replicate (i - lenAs) a ++ as
    | otherwise = as
    where
        lenAs = length as

drawGrid :: Int -> Grid -> Widget ()
drawGrid ac grid = renderTable (table labeledGrid)
    where
        go c r p =
            let i = c + (r * size)
            in drawGridCell (i == ac) i p
        labeledGrid =
            [ [ go c r p | (p, c)   <- zip row  [0..] ] |
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
        drawCellToPlay (_textCell s),
        drawCurPlayerBox (_curPlayer s),
        drawGrid (_arrowCell s) (_grid s),
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
    | isWinner O (_grid s) = s { _statusMessage = "Player O wins!"
                               , _playable = False
                               }
    | isWinner X (_grid s) = s { _statusMessage = "Player X wins!"
                               , _playable = False
                               }
    | fullBoard (_grid s)  = s { _statusMessage = "It's a draw!"
                               , _playable = False
                               }
    | otherwise            = s

arrowKeyToOffset V.KUp    = -size
arrowKeyToOffset V.KRight = 1
arrowKeyToOffset V.KDown  = size
arrowKeyToOffset V.KLeft  = -1
arrowKeyToOffset _        = 0

handleEvent :: BrickEvent () () -> EventM () St ()
handleEvent (VtyEvent (V.EvKey V.KEnter _)) = do
    s <- get
    let i = case _inputMode s of
                Typing   -> read (_textCell s)
                Arrowing -> _arrowCell s
    let s'  = play i s
    let s'' = checkBoard s'
    put (s'' { _textCell = "" })
    

handleEvent bevent@(VtyEvent (V.EvKey (V.KChar 'q') _)) =
    resizeOrQuit bevent
handleEvent (VtyEvent (V.EvKey (V.KChar c) _))
    | isDigit c = do
        s <- get
        if _playable s
        then put (s { _textCell = _textCell s ++ [c], _inputMode = Typing })
        else put s
handleEvent (VtyEvent (V.EvKey key mods))
    | key `elem` [ V.KUp, V.KRight, V.KDown, V.KLeft ] = do
        s <- get
        let offset = arrowKeyToOffset key
        let newAC  = (_arrowCell s + offset) `mod` (size * size)
        put $ s { _arrowCell = newAC, _inputMode = Arrowing }
    | otherwise = return ()
handleEvent _ = return ()

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
    let s = St emptyGrid O Typing "" 0 True ""
    _ <- defaultMain app s
    return ()