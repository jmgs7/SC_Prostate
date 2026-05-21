#' @title ViolinPlotGradient 
#' @description `ViolinPlotGradient()` adds the functionality over `Seurat::ViolinPlot()` to color the dots according a gradient determined by the value of another feature. This results very useful for visualizing the distribution of QC parameters such as read count, feature count and % of mitochondrial genes simultaneously.
#' @param SeuratObject The Seurat data.
#' @param features The features to be represented.
#' @param gradient: The feature which values will be used to set the color gradient.
#' @param scale.colors = Color pallette to use in the gradient. Corresponds to the color palletes codes included in scale_gradient_viridis:
#'     "magma" (or "A")
#'     "inferno" (or "B")
#'     "plasma" (or "C")
#'     "viridis" (or "D")
#'     "cividis" (or "E")
#'     "rocket" (or "F")
#'     "mako" (or "G")
#'     "turbo" (or "H")
#' @param lower.limit: Lower limit of the gradient. Default is 0. Only applicable when upper.limit is specified.
#' @param upper.limit: Upper limit of the gradient. When not specified, the gradient limits are automatically set.
#' @examples 
#' ViolinPlotGradient(SeuratObject, features=c("nFeature_RNA", "nCount_RNA", "percent.mt"), gradient="nCells", upper.limit = NULL, lower.limit = 0, scale.colors = "viridis")
#' @import Seurat
#' @import ggplot2
#' @noRd

ViolinPlotGradient <- function(
    SeuratObject, features, gradient, upper.limit = NULL,
    lower.limit = 0, scale.colors = "viridis"
    ) {

  df = data.frame(
    SeuratObject[[features]], SeuratObject[[gradient]]
    )
  names(df) <- c(features, gradient)

  plot <- ggplot(data = df, aes(x = df[,feature1], y = df[,feature2], color = df[,gradient])) +
    geom_point() +
    labs(x = feature1, y = feature2, subtitle = corr_label) +
    scale_color_viridis_c(name = gradient, option = scale.colors) +
    theme_classic()

  if (!is.null(upper.limit)) {
    plot <- plot +
      scale_color_viridis_c(name = gradient, limits = c(lower.limit, upper.limit), option = scale.colors)
  }

  return(plot)
}