#' Imputar celdas con frecuencia cero de una tabla de contingencia
#'
#' Reemplaza en una tabla de contingencia los ceros por un valor minimo
#' (por defecto, `1L`), preservando el resto de las celdas (nunca modifica
#' celdas con frecuencia positiva). El objetivo es preparar la tabla para
#' la estimacion de modelos log-lineales, cuyo procedimiento puede fallar
#' o producir estimaciones inestables en presencia de ceros.
#'
#' La tabla devuelta lleva dos atributos que documentan la operacion:
#' \itemize{
#'   \item `zeros_imputados`: data frame identico al devuelto por
#'         [detect_zeros_any()], con una columna adicional
#'         `Frecuencia_Imputada` que registra el valor con el que se ha
#'         sustituido cada cero. Vale `NULL` si no habia ceros.
#'   \item `nota_imputacion`: cadena de caracteres describiendo el numero
#'         de celdas imputadas (o indicando que no habia ceros).
#' }
#'
#' Utiliza internamente [detect_zeros_any()] para localizar las celdas
#' afectadas.
#'
#' @param tab Tabla de contingencia (objeto `table`, `array` o `matrix`)
#'   de cualquier numero de dimensiones.
#' @param value Valor con el que sustituir las celdas con frecuencia cero.
#'   Por defecto, `1L`.
#'
#' @return Una tabla del mismo tipo y dimensiones que `tab`, con los
#'   ceros sustituidos por `value` y con los atributos `zeros_imputados`
#'   y `nota_imputacion` adjuntos.
#'
#' @examples
#' \dontrun{
#' # Tabla de contingencia con ceros
#' tab <- xtabs(~ GALAXIA + FJUR + EFLO, data = seleccion)
#'
#' # Imputar ceros con 1
#' tab_use <- impute_zeros_with_one(tab, value = 1L)
#'
#' # Consultar la nota y el detalle de las celdas imputadas
#' cat(attr(tab_use, "nota_imputacion"), "\n")
#' attr(tab_use, "zeros_imputados")
#' }
#'
#' @export
impute_zeros_with_one <- function(tab, value = 1L) {
  zeros_df <- detect_zeros_any(tab)
  tab_adj <- tab
  tab_adj[tab_adj == 0] <- value

  if (!is.null(zeros_df)) {
    zeros_df$Frecuencia_Imputada <- value
    attr(tab_adj, "zeros_imputados") <- zeros_df
    attr(tab_adj, "nota_imputacion") <-
      sprintf("Se imputaron %d celdas 0 con valor %d para poder preparar la estimaci\u00f3n.",
              nrow(zeros_df), value)
  } else {
    attr(tab_adj, "zeros_imputados") <- NULL
    attr(tab_adj, "nota_imputacion") <- "No hab\u00eda celdas con frecuencia 0."
  }
  tab_adj
}
