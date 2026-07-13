#' Presentar los resultados de un modelo lineal
#'
#' Convierte el objeto devuelto por [stats::lm()] (u otros modelos compatibles
#' con [broom::tidy()] y [broom::glance()]) en un conjunto de tres tablas
#' formateadas con [kable_rstars()], listas para su inclusion en un documento
#' R Markdown con salida HTML. Las tres tablas son:
#'
#' \enumerate{
#'   \item **Coeficientes**: estimaciones, errores estandar, estadistico t,
#'         p-valor y niveles de significacion (columna `stars`).
#'   \item **Estadisticos del modelo**: R^2, R^2 ajustado, sigma, estadistico F,
#'         p-valor global, AIC y numero de observaciones.
#'   \item **Factor de inflacion de la varianza (VIF)**: valores de VIF para
#'         cada variable explicativa del modelo.
#' }
#'
#' Internamente, cada una de las tres tablas se construye con
#' [kable_rstars()], por lo que hereda el estilo tipografico habitual del
#' libro. Cualquier argumento adicional se reenvia a [kable_rstars()] mediante
#' `...`, aplicandose por igual a las tres tablas.
#'
#' @param modelo Objeto de modelo lineal ajustado (habitualmente devuelto por
#'   [stats::lm()]). Debe ser compatible con [broom::tidy()], [broom::glance()]
#'   y [car::vif()]. Este ultimo requiere que el modelo tenga al menos dos
#'   variables explicativas.
#' @param captions Vector de caracteres de longitud 3 con los titulos de las
#'   tres tablas, en orden: coeficientes, estadisticos, VIF. Por defecto,
#'   \code{c("Modelo Lineal", "Estad\u00edsticos del modelo",
#'   "Factor de inflaci\u00f3n de la varianza")}.
#' @param ... Argumentos adicionales pasados a [kable_rstars()] (por ejemplo,
#'   `font_size` o `bootstrap_options`), aplicados por igual a las tres tablas.
#'
#' @return Lista de tres objetos `knitr_kable` con nombres `coef`, `stats` y
#'   `vif`, accesibles tanto por posicion (`x[[1]]`) como por nombre
#'   (`x$coef`).
#'
#' @examples
#' \dontrun{
#' modelo <- lm(mpg ~ wt + hp + disp, data = mtcars)
#' salida <- presenta_modelo(modelo)
#'
#' salida[[1]]   # tabla de coeficientes
#' salida[[2]]   # tabla de estadisticos
#' salida[[3]]   # tabla de VIF
#'
#' # Personalizar titulos y tamano de fuente:
#' presenta_modelo(modelo,
#'                 captions  = c("Coeficientes", "Ajuste global", "VIF"),
#'                 font_size = 12)
#' }
#'
#' @importFrom broom tidy glance
#' @importFrom car vif
#' @importFrom tibble rownames_to_column
#' @export
presenta_modelo <- function(modelo,
                            captions = c("Modelo Lineal",
                                         "Estad\u00edsticos del modelo",
                                         "Factor de inflaci\u00f3n de la varianza"),
                            ...) {

  if (!is.character(captions) || length(captions) != 3) {
    stop("`captions` debe ser un vector de caracteres de longitud 3.")
  }

  piezas <- list()

  # --- Tabla 1: coeficientes ---------------------------------------------
  resultados <- broom::tidy(modelo)
  resultados <- resultados[, c("term", "estimate", "std.error",
                               "statistic", "p.value")]
  resultados$stars <- cut(resultados$p.value,
                          breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                          labels = c("***", "**", "*", "\u00b7", " "),
                          right  = FALSE)
  resultados$estimate <- formatC(resultados$estimate,
                                 format = "f", digits = 5)

  piezas[[1]] <- kable_rstars(
    resultados,
    caption   = captions[1],
    col.names = c("Variable", "Coeficiente", "Desv. T\u00edpica",
                  "Estad\u00edstico t", "p-valor", "Sig."),
    digits    = 3,
    ...
  )

  # --- Tabla 2: estadisticos del modelo ----------------------------------
  estadisticos <- broom::glance(modelo)
  estadisticos <- estadisticos[, c("r.squared", "adj.r.squared", "sigma",
                                   "statistic", "p.value", "AIC", "nobs")]

  piezas[[2]] <- kable_rstars(
    estadisticos,
    caption   = captions[2],
    col.names = c("R2", "R2 ajustado", "Sigma", "Estad\u00edstico F",
                  "p-valor", "AIC", "num. observaciones"),
    digits    = 3,
    ...
  )

  # --- Tabla 3: VIF ------------------------------------------------------
  vif_df <- as.data.frame(car::vif(modelo))
  vif_df <- tibble::rownames_to_column(vif_df, var = "Variable")

  piezas[[3]] <- kable_rstars(
    vif_df[, 1:2],
    caption   = captions[3],
    col.names = c("Variable", "Valor VIF"),
    digits    = 3,
    ...
  )

  names(piezas) <- c("coef", "stats", "vif")
  piezas
}
