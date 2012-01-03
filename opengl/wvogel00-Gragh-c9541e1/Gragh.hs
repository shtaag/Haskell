{-# LANGUAGE ViewPatterns  #-}

module Gragh where

import Graphics.UI.GLUT

data GraghInfo
  = GraghInfo {
        width :: Float,
        height:: Float,
        func  :: Float -> Float,
        fcolor:: Color3 GLdouble,
        x_axis:: Float,
        y_axis:: Float,
        px    :: Float,
        py    :: Float
    }

--軸描画
drawAxis :: Float -> Float -> Float -> Float -> IO()
drawAxis w h x y
 = do
      color (Color3 1.0 1.0 1.0 :: Color3 GLdouble)
      renderPrimitive LineStrip $ mapM_ vertex
       [ Vertex2  ((y-w/2)/w*2)  1.0,
         Vertex2 ((y-w/2)/w*2) (-1.0) :: Vertex2 GLfloat]
      renderPrimitive LineStrip $ mapM_ vertex
       [ Vertex2  1.0   ((h/2-x)/h*2),
         Vertex2 (-1.0) ((h/2-x)/h*2) :: Vertex2 GLfloat]

--描画コールバック
drawGragh :: GraghInfo -> IO()
drawGragh info
 = do
      clear [ColorBuffer]
      preservingMatrix $ do
        drawAxis (width info) (height info) (x_axis info) (y_axis info)
      preservingMatrix $ do
        color (fcolor info)
        renderPrimitive Points $ mapM_ vertex
            $ map (calcPos info) $ xList (width info)
      swapBuffers

--描画するx座標のリスト
xList :: Float -> [Float]
xList width = [(-1.0),(-1.0+0.5/width)..1.0]

--座標計算
calcPos :: GraghInfo -> Float -> Vertex2 GLfloat
calcPos info = \x -> Vertex2 x (graghFunc info x)

--xの座標系変換
transX :: GraghInfo -> Float -> Float
transX info x = (x*width info/2+width info/2-y_axis info)/px info

--func info -> y
graghFunc :: GraghInfo -> Float -> GLfloat
graghFunc info (transX info -> x')
 = let y' = (func info) x'
   in  (y'*py info*2+height info-2*x_axis info)/height info

--テキスト表示
drawText x y str =
 preservingMatrix $ do
  translate (Vector3 x y 0::Vector3 Float)
  scale 0.0005 0.0005 (1.0 :: Double)
  renderString Roman str
