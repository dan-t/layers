
module GameData.Player where
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified FileData.Data2 as FD
import qualified Gamgine.Math.Vect as V
import qualified Gamgine.Math.BoxTree as BT
import qualified Gamgine.Math.Box as B
import qualified Gamgine.Ressources as R
import qualified Gamgine.Gfx as G
import qualified Event as EV
import qualified GameData.Entity as E


-- | in which direction the player is moving
data Movement = ToTheLeft | ToTheRight | AtRest deriving (Show, Eq)


data Player = Player {
   playerId        :: Int,
   initialPosition :: V.Vect,
   position        :: V.Vect,
   velocity        :: V.Vect,
   onBottom        :: Bool,
   movement        :: Movement,
   bound           :: E.Bound,
   textureId       :: GL.GLuint
   } deriving Show


instance E.ToFileEntity Player where
   toFileEntity Player {playerId = id, initialPosition = initPos} =
      FD.Player id (V.toTuple initPos)


instance E.EntityT Player where
   initRessources p _ = do
      fp  <- R.getImageFilePath "Player.png"
      tex <- G.makeTexture2d fp GL.gl_REPEAT
      return p {textureId = tex}

   update e s = e

   render Player {position = pos, textureId = texId} _ =
      G.renderTexturedQuad playerSize pos texId

   handleEvent e ev s = e

   getBound p = Just $ bound p


newPlayer :: Int -> V.Vect -> Player
newPlayer id initPos = Player {
  playerId = id,
  initialPosition = initPos,
  position        = initPos,
  velocity        = (V.v3 0 0 0),
  onBottom        = False,
  movement        = AtRest,
  bound           = playerBound playerSize,
  textureId       = 0
  }


playerSize :: (Double, Double)
playerSize = (2,2.5)


playerBound :: (Double, Double) -> E.Bound
playerBound (x, y) =
   let frac1      = 1/20
       frac2      = 1 - frac1
       topBox     = B.Box (V.v3 (frac1*x) (frac2*y) 0) (V.v3 (frac2*x) (1*y) 0)
       bottomBox  = B.Box (V.v3 (frac1*x) 0 0)         (V.v3 (frac2*x) (frac1*y) 0)
       leftBox    = B.Box (V.v3 0 (frac1*y) 0)         (V.v3 (frac1*x) (frac2*y) 0)
       rightBox   = B.Box (V.v3 (frac2*x) (frac1*y) 0) (V.v3 (1*x) (frac2*y) 0)
       middleBox  = B.Box (V.v3 (frac1*x) (frac1*y) 0) (V.v3 (frac2*x) (frac2*y) 0)
       topLeaf    = BT.Leaf topBox E.Top
       bottomLeaf = BT.Leaf bottomBox E.Bottom
       leftLeaf   = BT.Leaf leftBox E.Left
       rightLeaf  = BT.Leaf rightBox E.Right
       middleLeaf = BT.Leaf middleBox E.Whatever
       in BT.Node (B.Box (V.v3 0 0 0) (V.v3 x y 0))
                  [leftLeaf, topLeaf, rightLeaf, bottomLeaf, middleLeaf]
