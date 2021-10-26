module Palantype.Tools.Statistics where

import           Control.Category                       (Category ((.)))
import           Control.Lens.Getter                    ((^.))
import           Control.Lens.Setter                    ((.~), (?~))
import           Data.Default                           (Default (def))
import           Data.Function                          (($), (&))
import           Data.Functor                           (void)
import           Data.Int                               (Int)
import           Data.Maybe                             (Maybe (Nothing, Just))
import           Data.Text.IO                           (putStrLn)
import           GHC.Float                              (Double)
import           Graphics.Rendering.Chart               (Plot,
                                                         Renderable (render),
                                                         defaultFloatPlotHist,
                                                         histToPlot,
                                                         layout_plots,
                                                         layout_title,
                                                         plot_hist_bins,
                                                         plot_hist_range,
                                                         plot_hist_values,
                                                         plot_render,
                                                         toRenderable, plot_hist_title, layout_x_axis, autoScaledAxis, laxis_generate, la_nLabels, la_nTicks, scaledAxis)
import           Graphics.Rendering.Chart.Backend.Cairo (renderableToFile,
                                                         toFile)
import           Graphics.Rendering.Chart.Gtk           (renderableToWindow)
import           Graphics.Rendering.Chart.State         (plot)
import           System.IO                              (FilePath, IO)
import Text.Show (Show(show))
import Data.Semigroup (Semigroup((<>)))
import Data.Foldable (Foldable(length))

histogram
  :: [Double]
  -> Plot Double Double
histogram values =
  histToPlot $ defaultFloatPlotHist
    & plot_hist_values .~ values
    & plot_hist_bins .~ 12
    & plot_hist_title .~ "#Total: " <> show (length values)
    & plot_hist_range ?~ (0,12)

chart
  :: [Double]
  -> Renderable ()
chart values =
  toRenderable $ def
    & layout_plots .~ [histogram values]
    & layout_title .~ "Scores"
    & layout_x_axis . laxis_generate .~ scaledAxis
        ( def & la_nLabels .~ 13
              & la_nTicks .~ 13
        ) (0, 12)

plotScoresFile
  :: FilePath
  -> [Double]
  -> IO ()
plotScoresFile filename values =
  void $ renderableToFile def filename $ chart values

plotScoresShow
  :: [Double]
  -> IO ()
plotScoresShow values =
  renderableToWindow (chart values) 800 600
