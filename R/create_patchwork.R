#' Componer una lista de ggplots en retículas 2x2 con patchwork
#'
#' Toma una lista de objetos `ggplot` y los agrupa en composiciones de dos
#' filas por dos columnas (cuatro gráficos por composición) usando los
#' operadores de \pkg{patchwork}. Si el número total de gráficos no es
#' múltiplo de cuatro, la última composición se completa con paneles vacíos
#' (`theme_void()`) para mantener la retícula uniforme.
#'
#' Esta función se utiliza en varios capítulos del libro *R-Stars* para
#' presentar de forma ordenada baterías de gráficos: centroides por clúster,
#' comparaciones variable-variable, análisis por grupos, etc.
#'
#' @param plot_list Lista de objetos `ggplot`. Si la lista está vacía la
#'   función devuelve `NULL`.
#'
#' @return Una lista de composiciones `patchwork`. Cada elemento contiene
#'   como máximo cuatro gráficos dispuestos en una retícula 2x2. Para
#'   imprimir las composiciones basta con recorrer la lista y llamar a
#'   `print()` sobre cada elemento.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(patchwork)
#'
#' # Lista de 5 gráficos de ejemplo
#' graficos <- lapply(1:5, function(i) {
#'   ggplot(mtcars, aes(x = wt, y = mpg)) +
#'     geom_point() +
#'     ggtitle(paste("Gráfico", i))
#' })
#'
#' composiciones <- create_patchwork(graficos)
#'
#' for (comp in composiciones) print(comp)
#' }
#'
#' @importFrom patchwork wrap_plots
#' @importFrom ggplot2 ggplot theme_void
#' @export
create_patchwork <- function(plot_list) {
  n <- length(plot_list)
  if (n == 0) return(NULL)
  full_rows <- n %/% 4
  remaining <- n %% 4
  patchworks <- list()

  if (full_rows > 0) {
    for (i in seq(1, full_rows * 4, by = 4)) {
      patchworks <- c(patchworks,
                      list((plot_list[[i]]     + plot_list[[i + 1]]) /
                           (plot_list[[i + 2]] + plot_list[[i + 3]])))
    }
  }

  if (remaining > 0) {
    last_plots  <- plot_list[(full_rows * 4 + 1):n]
    empty_plots <- lapply(1:(4 - remaining),
                          function(x) ggplot2::ggplot() + ggplot2::theme_void())
    last_patchwork <- do.call(patchwork::wrap_plots,
                              c(last_plots, empty_plots))
    patchworks <- c(patchworks, list(last_patchwork))
  }
  return(patchworks)
}
