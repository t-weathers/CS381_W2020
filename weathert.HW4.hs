-- | HW 4 template, copyright of Erik Walkingshaw, Oregon State Universtiy Computer Science Dept.
-- | provided template to be edited

-- Alekos Hovekamp - hovekama 
-- Andrew Gates - gatesand
-- Thomas Weathers - weathert


module HW3 where

import MiniMiniLogo
import Render


--
-- * Semantics of MiniMiniLogo
--

-- NOTE:
--  * MiniMiniLogo.hs defines the abstract syntax of MiniMiniLogo and some
--    functions for generating MiniMiniLogo programs. It contains the type
--    definitions for Mode, Cmd, and Prog.
--  * Render.hs contains code for rendering the output of a MiniMiniLogo
--    program in HTML5. It contains the types definitions for Point and Line.

-- | A type to represent the current state of the pen.
type State = (Mode,Point)

-- | The initial state of the pen.
start :: State
start = (Up,(0,0))

-- | A function that renders the image to HTML. Only works after you have
--   implemented `prog`. Applying `draw` to a MiniMiniLogo program will
--   produce an HTML file named MiniMiniLogo.html, which you can load in
--   your browswer to view the rendered image.
draw :: Prog -> IO ()
draw p = let (_,ls) = prog p start in toHTML ls


-- Semantic domains:
--   * Cmd:  State -> (State, Maybe Line)
--   * Prog: State -> (State, [Line])


-- | Semantic function for Cmd.
--   
--   >>> cmd (Pen Down) (Up,(2,3))
--   ((Down,(2,3)),Nothing)
--
--   >>> cmd (Pen Up) (Down,(2,3))
--   ((Up,(2,3)),Nothing)
--
--   >>> cmd (Move 4 5) (Up,(2,3))
--   ((Up,(4,5)),Nothing)
--
--   >>> cmd (Move 4 5) (Down,(2,3))
--   ((Down,(4,5)),Just ((2,3),(4,5)))
--
cmd :: Cmd -> State -> (State, Maybe Line)
cmd (Pen Up) (a ,(x, y)) = ((Up,(x,y)), Nothing)
cmd (Pen Down) (a ,(x, y))= ((Down, (x,y)), Nothing)
cmd (Move x2 y2) (Up ,(x1, y1)) = ((Up, (x2,y2)),Nothing)
cmd (Move x2 y2) (Down ,(x1, y1))= ((Down,(x2,y2)),Just ((x1,y1),(x2,y2)))


-- | Semantic function for Prog.
--
--   >>> prog (nix 10 10 5 7) start
--   ((Down,(15,10)),[((10,10),(15,17)),((10,17),(15,10))])
--
--   >>> prog (steps 2 0 0) start
--   ((Down,(2,2)),[((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2))])
prog :: Prog -> State -> (State, [Line])
prog [] state = (state, [])
prog (s:ss) (ud, (x, y)) = case cmd s (ud, (x, y)) of
                        (state, Nothing) -> prog ss state
                        (state, Just line) -> progh ss state [line] -- need to find a way to append Line to the second term of the result from (prog ss state)

progh :: Prog -> State -> [Line] -> (State, [Line])
progh [] state linea = (state, linea)
progh (s:ss) (ud, (x,y)) linea  =  case cmd s (ud, (x, y)) of
                        (state, Nothing) -> progh ss state linea
                        (state, Just line) -> progh ss state (linea ++ [line])



--
-- * Extra credit
--

-- | This should be a MiniMiniLogo program that draws an amazing picture.
--   Add as many helper functions as you want.
amazing :: Prog
amazing = undefined
