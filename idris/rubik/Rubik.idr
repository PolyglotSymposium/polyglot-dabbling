module Rubik

import Data.List
import Data.Vect

%default total
%access public

data Color = Green | Red | White | Blue | Orange | Yellow

%name Color color

nextColor : Color -> Color
nextColor Green = Red
nextColor Red = White
nextColor White = Blue
nextColor Blue = Orange
nextColor Orange = Yellow
nextColor Yellow = Green

data CubeDistance = Same | Adjacent | Opposite

instance Eq CubeDistance where
  Same == Same = True
  Adjacent == Adjacent = True
  Opposite == Opposite = True
  _ == _ = False

cubeDistance : Color -> Color -> CubeDistance
cubeDistance Green Green = Same
cubeDistance Green Blue = Opposite
cubeDistance Red Red = Same
cubeDistance Red Orange = Opposite
cubeDistance White White = Same
cubeDistance White Yellow = Opposite
cubeDistance Blue Blue = Same
cubeDistance Blue Green = Opposite
cubeDistance Orange Orange = Same
cubeDistance Orange Red = Opposite
cubeDistance Yellow Yellow = Same
cubeDistance Yellow White = Opposite
cubeDistance _ _ = Adjacent

instance Eq Color where
  color1 == color2 = cubeDistance color1 color2 == Same

nextColorN : Color -> Nat -> Color
nextColorN color Z = color
nextColorN color (S k) = nextColor $ nextColorN color k

oppositeColor : Color -> Color
oppositeColor color = nextColorN color 3

green_is_opposite_blue : oppositeColor Blue = Green
green_is_opposite_blue = Refl

yellow_is_opposite_white : oppositeColor Yellow = White
yellow_is_opposite_white = Refl

orange_is_opposite_red : oppositeColor Red = Orange
orange_is_opposite_red = Refl

--oppositeColor_is_involute : oppositeColor x = y -> oppositeColor y = x

threeAreAdjacent : Color -> Color -> Color -> Bool
threeAreAdjacent color1 color2 color3 =
  cubeDistance color1 color2 == Adjacent &&
  cubeDistance color1 color3 == Adjacent &&
  cubeDistance color2 color3 == Adjacent

data SubcubeKind = Inside | Middle | Edge | Corner

data Subcube : SubcubeKind -> Type where
  InsideCube : Subcube Inside
  MiddleCube : Color ->
               Subcube Middle
  EdgeCube : (c1 : Color) -> (c2 : Color) ->
             {auto prf : cubeDistance c1 c2 = Adjacent} ->
             Subcube Edge
  CornerCube : (c1 : Color) -> (c2 : Color) -> (c3 : Color) ->
               {prf : threeAreAdjacent c1 c2 c3 = True} ->
               Subcube Corner

data AnySubcube = UpcastSubcube (Subcube a)

data Adjacency2 : SubcubeKind -> Type where
  CornerAdj2 : Subcube Corner -> Subcube Corner -> Subcube Corner -> Adjacency2 Corner

data Adjacency3 : SubcubeKind -> Type where
  MiddleAdj3 : Subcube Edge -> Subcube Edge -> Subcube Edge -> Subcube Edge -> Adjacency3 Middle
  EdgeAdj3 : Subcube Middle -> Subcube Middle -> Subcube Edge -> Subcube Edge -> Adjacency3 Edge
  CornerAdj3 : Subcube Edge -> Subcube Edge -> Subcube Edge -> Adjacency3 Corner

UnitOfRotation2 : Type
UnitOfRotation2 = (Subcube Corner, Subcube Corner, Subcube Corner, Subcube Corner)

data FaceId = Up | Down | Left | Right | Front | Back

%name FaceId faceId

--MiddleId : Type

data EdgeId =
  UF
  | UR
  | UB
  | UL
  | FR
  | RB
  | BL
  | LF
  | DF
  | DR
  | DB
  | DL

CornerId : Type
CornerId = (FaceId, FaceId, FaceId)

data RotationDirection = CW | CCW

%name RotationDirection rot

data AxisId = Vertical | Depth | Horizontal

Cube2 : Type
Cube2 = (Subcube Corner, Subcube Corner, Subcube Corner, Subcube Corner, Subcube Corner, Subcube Corner, Subcube Corner, Subcube Corner)

%name Cube2 cube

solvedCube2 : Cube2
solvedCube2 = (
  CornerCube Green Red White {prf=Refl},
  CornerCube Green White Orange {prf=Refl},
  CornerCube Blue White Red {prf=Refl},
  CornerCube Blue Orange White {prf=Refl},
  CornerCube Green Yellow Red {prf=Refl},
  CornerCube Green Orange Yellow {prf=Refl},
  CornerCube Blue Red Yellow {prf=Refl},
  CornerCube Blue Yellow Orange {prf=Refl}
  )

UnitOfRotation2_v2 : Type
UnitOfRotation2_v2 = Vect 4 (CornerId, Subcube Corner)

Cube2_v2 : Type
Cube2_v2 = Vect 8 (CornerId, Subcube Corner)

%name Cube2_v2 cube

solvedCube2_v2 : Cube2_v2
solvedCube2_v2 = [
  ((Up, Front, Right), CornerCube Green Red White {prf=Refl}),
  ((Up, Front, Left), CornerCube Green White Orange {prf=Refl}),
  ((Up, Right, Back), CornerCube Blue White Red {prf=Refl}),
  ((Up, Left, Back), CornerCube Blue Orange White {prf=Refl}),
  ((Down, Right, Front), CornerCube Green Yellow Red {prf=Refl}),
  ((Down, Front, Left), CornerCube Green Orange Yellow {prf=Refl}),
  ((Down, Right, Back), CornerCube Blue Red Yellow {prf=Refl}),
  ((Down, Back, Left), CornerCube Blue Yellow Orange {prf=Refl})
  ]

instance Eq FaceId where
  Up == Up = True
  Down == Down = True
  Left == Left = True
  Right == Right = True
  Front == Front = True
  Back == Back = True
  _ == _ = False

oppositeFace : FaceId -> FaceId
oppositeFace Up = Down
oppositeFace Down = Up
oppositeFace Left = Right
oppositeFace Right = Left
oppositeFace Front = Back
oppositeFace Back = Front

||| E.g.
||| rotateFace CW Up Left
||| Rotates the left face with respect to the up face.
private
rotateFaceCW : FaceId -> FaceId -> FaceId
rotateFaceCW Up Left = Back
rotateFaceCW Up Right = Front
rotateFaceCW Up Front = Left
rotateFaceCW Up Back = Right
rotateFaceCW Down Left = Front
rotateFaceCW Down Right = Back
rotateFaceCW Down Front = Right
rotateFaceCW Down Back = Left
rotateFaceCW Left Up = Front
rotateFaceCW Left Down = Back
rotateFaceCW Left Front = Down
rotateFaceCW Left Back = Up
rotateFaceCW Right Up = Back
rotateFaceCW Right Down = Front
rotateFaceCW Right Front = Up
rotateFaceCW Right Back = Down
rotateFaceCW Front Left = Up
rotateFaceCW Front Right = Down
rotateFaceCW Front Up = Right
rotateFaceCW Front Down = Left
rotateFaceCW Back Left = Down
rotateFaceCW Back Right = Up
rotateFaceCW Back Up = Left
rotateFaceCW Back Down = Right
rotateFaceCW face1 face2 = face2

rotateFace : RotationDirection -> FaceId -> FaceId -> FaceId
rotateFace CW face1 face2 = rotateFaceCW face1 face2
rotateFace CCW face1 face2 = rotateFaceCW (oppositeFace face1) face2

rotateEdge' : (FaceId, FaceId) -> RotationDirection -> FaceId -> (FaceId, FaceId)
rotateEdge' (face1, face2) rotd face0 =
  let rotate = rotateFace rotd face0
  in (rotate face1, rotate face2)

rotateCorner : CornerId -> RotationDirection -> FaceId -> CornerId
rotateCorner (face1, face2, face3) rotd face0 =
  let rotate = rotateFace rotd face0
  in (rotate face1, rotate face2, rotate face3)

rotate2 : Cube2 -> RotationDirection -> FaceId -> Cube2

unitOfRotation : Cube2 -> FaceId -> UnitOfRotation2
unitOfRotation (corner1, corner2, corner3, corner4, _) Up =
  (corner1, corner2, corner3, corner4)
unitOfRotation (_, _, _, _, corner5, corner6, corner7, corner8) Down =
  (corner5, corner6, corner7, corner8)
unitOfRotation (_, corner2, _, corner4, _, corner6, _, corner8) Left =
  (corner2, corner4, corner6, corner8)
unitOfRotation (corner1, _, corner3, _, corner5, _, corner7, _) Right =
  (corner1, corner3, corner5, corner7)
unitOfRotation (corner1, corner2, _, _, corner5, corner6, _) Front =
  (corner1, corner2, corner5, corner6)
unitOfRotation (_, _, corner3, corner4, _, _, corner7, corner8) Back =
  (corner3, corner4, corner7, corner8)

cornerOfFace : FaceId -> CornerId -> Bool
cornerOfFace face0 (face1, face2, face3) =
  face1 == face0 || face2 == face0 || face3 == face0

unitOfRotation2_v2 : Cube2_v2 -> FaceId -> UnitOfRotation2_v2
unitOfRotation2_v2 cube faceId = ?asdfasdf
  --Vect.filter (\x => cornerOfFace faceId $ fst x) cube

twoCornersColorsInCommon : Subcube Corner -> Subcube Corner -> List Color
twoCornersColorsInCommon (CornerCube c1 c2 c3) (CornerCube c4 c5 c6) =
  intersect [c1, c2, c3] [c4, c5, c6]

fourCornersColorsInCommon : Subcube Corner -> Subcube Corner ->
                            Subcube Corner -> Subcube Corner ->
                            List Color
fourCornersColorsInCommon corner1 corner2 corner3 corner4 =
  intersect (twoCornersColorsInCommon corner1 corner2) (twoCornersColorsInCommon corner3 corner4)

uor2ColorsInCommon : UnitOfRotation2 -> List Color
uor2ColorsInCommon (corner1, corner2, corner3, corner4) = fourCornersColorsInCommon corner1 corner2 corner3 corner4

isSolved2 : Cube2 -> Bool
isSolved2 cube =
  (uor2ColorsInCommon $ unitOfRotation cube Up) == [White] &&
  (uor2ColorsInCommon $ unitOfRotation cube Down) == [Yellow] &&
  (uor2ColorsInCommon $ unitOfRotation cube Left) == [Orange] &&
  (uor2ColorsInCommon $ unitOfRotation cube Right) == [Red] &&
  (uor2ColorsInCommon $ unitOfRotation cube Front) == [Green] &&
  (uor2ColorsInCommon $ unitOfRotation cube Back) == [Blue]

isSolved2_v2 : Cube2 -> Bool
isSolved2_v2 cube = ?asdfqwer


u2 : Cube2 -> Cube2
u2 (corner1, corner2, corner3, corner4, tail) =
  (corner3, corner1, corner4, corner2, tail)

f2 : Cube2 -> Cube2
f2 (corner1, corner2, corner3, corner4, corner5, corner6, tail) =
  (corner2, corner6, corner3, corner4, corner1, corner5, tail)

--r : Cube2 -> Cube2
--
--d : Cube2 -> Cube2
--
--l : Cube2 -> Cube2
--
--b : Cube2 -> Cube2

ui2 : Cube2 -> Cube2
ui2 (corner1, corner2, corner3, corner4, tail) =
  (corner2, corner4, corner1, corner3, tail)

--fi : Cube2 -> Cube2
--
--ri : Cube2 -> Cube2
--
--di : Cube2 -> Cube2
--
--li : Cube2 -> Cube2
--
--bi : Cube2 -> Cube2

--ui_is_u_inverse : ui (u cube) = cube

record Cube (n : Nat) where
  constructor MkCube
  corners : Vect 8 (Subcube Corner)
  edges : Vect ((minus n 2)*12) (Subcube Edge)
  middles : Vect ((minus n 2)*(minus n 2)*6) (Subcube Middle)

