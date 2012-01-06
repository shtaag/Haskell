module Rect where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
rect w = do 
  renderPrimitive Quads $ do
    vertex $ Vertex3 w w 0
    vertex $ Vertex3 w (-w) 0
    vertex $ Vertex3 (-w) (-w) 0 
    vertex $ Vertex3 (-w) w 0
    vertex $ Vertex3 w w 0
