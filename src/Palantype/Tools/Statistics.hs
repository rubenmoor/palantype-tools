module Palantype.Tools.Statistics where

import           Control.Category                       (Category ((.)))
import           Control.Lens.Setter                    ((.~), (?~))
import           Data.Default                           (Default (def))
import           Data.Foldable                          (Foldable (length))
import           Data.Function                          (($), (&))
import           Data.Functor                           (void)
import           Data.Semigroup                         (Semigroup ((<>)))
import           GHC.Float                              (Double)
import           Graphics.Rendering.Chart               (Plot, Renderable,
                                                         defaultFloatPlotHist,
                                                         histToPlot, la_nLabels,
                                                         la_nTicks,
                                                         laxis_generate,
                                                         layout_plots,
                                                         layout_title,
                                                         layout_x_axis,
                                                         plot_hist_bins,
                                                         plot_hist_range,
                                                         plot_hist_title,
                                                         plot_hist_values,
                                                         scaledAxis,
                                                         toRenderable)
import           Graphics.Rendering.Chart.Backend.Cairo (renderableToFile)
import           Graphics.Rendering.Chart.Gtk           (renderableToWindow)
import           System.IO                              (FilePath, IO)
import           Text.Show                              (Show (show))

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
