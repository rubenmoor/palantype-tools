{-# LANGUAGE ScopedTypeVariables #-}
module Palantype.Tools.Statistics where

import           Control.Applicative            ( (<$>) )
import           Control.Category               ( Category((.)) )
import           Data.Default                   ( Default(def) )
import           Data.Foldable                  ( Foldable(length) )
import           Data.Function                  ( ($)
                                                , (&)
                                                )
import           Data.Functor                   ( (<&>) )
import           Data.List                      ( dropWhile
                                                , tail
                                                )
import           Data.Ord                       ( (>) )
import           Data.Semigroup                 ( Semigroup((<>)) )
import           Data.Text.Lazy                 ( lines
                                                , splitOn
                                                )
import qualified Data.Text.Lazy                as Text
import           Data.Text.Lazy.IO              ( readFile )
import           GHC.Err                        ( error )
import           GHC.Float                      ( Double )
-- import           Graphics.Rendering.Chart       ( Plot
--                                                 , Renderable
--                                                 , autoScaledLogAxis
--                                                 , defaultFloatPlotHist
--                                                 , histToPlot
--                                                 , la_nLabels
--                                                 , la_nTicks
--                                                 , laxis_generate
--                                                 , layout_plots
--                                                 , layout_title
--                                                 , layout_x_axis
--                                                 , layout_y_axis
--                                                 , plot_hist_bins
--                                                 , plot_hist_range
--                                                 , plot_hist_title
--                                                 , plot_hist_values
--                                                 , scaledAxis
--                                                 , toRenderable
--                                                 )
-- import           Graphics.Rendering.Chart.Gtk   ( renderableToWindow )
import           System.IO                      ( IO )
import           Text.Read                      ( read )
import           Text.Show                      ( Show(show) )

-- plotScoresFile
--   :: FilePath
--   -> [Double]
--   -> IO ()
-- plotScoresFile filename values =
--   void $ renderableToFile def filename $ chart values

-- plotScoresShow :: [Double] -> IO ()
-- plotScoresShow values = renderableToWindow chart 800 600
--   where
--     chart :: Renderable ()
--     chart =
--         toRenderable
--             $  def
--             &  layout_plots
--             .~ [histogram]
--             &  layout_title
--             .~ "Scores"
--             &  layout_x_axis
--             .  laxis_generate
--             .~ scaledAxis (def & la_nLabels .~ 13 & la_nTicks .~ 13) (0, 12)
--
--     histogram :: Plot Double Double
--     histogram =
--         histToPlot
--             $  defaultFloatPlotHist
--             &  plot_hist_values
--             .~ values
--             &  plot_hist_bins
--             .~ 12
--             &  plot_hist_title
--             .~ "#Total: "
--             <> show (length values)
--             &  plot_hist_range
--             ?~ (0, 12)
--
--
-- plotFrequencies :: IO ()
-- plotFrequencies = do
--     ls <- lines <$> readFile "deu_news_2020_freq.txt"
--     let ns = tail ls <&> \l -> case splitOn "\t" l of
--             [_, n'] -> read $ Text.unpack n'
--             _       -> error "missing tab character?"
--     renderableToWindow (chart $ dropWhile (> 200) ns) 800 600
--   where
--     chart :: [Double] -> Renderable ()
--     chart ns =
--         toRenderable
--             $  def
--             &  layout_plots
--             .~ [histogram ns]
--             &  layout_title
--             .~ "Frequency"
--             &  layout_x_axis
--             .  laxis_generate
--             .~ scaledAxis (def & la_nLabels .~ 200 & la_nTicks .~ 200) (1, 200)
--             &  layout_y_axis
--             .  laxis_generate
--             .~ autoScaledLogAxis def
--
--     histogram :: [Double] -> Plot Double Double
--     histogram ns =
--         histToPlot
--             $  defaultFloatPlotHist
--             &  plot_hist_values
--             .~ ns
--             &  plot_hist_bins
--             .~ 200
--             &  plot_hist_title
--             .~ "#Total shown: "
--             <> show (length ns)
--             &  plot_hist_range
--             ?~ (0, 200)
