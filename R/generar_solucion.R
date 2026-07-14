#' Generar tablas y grafico a partir de un modelo log-lineal
#'
#' Automatiza la presentacion de resultados de un modelo log-lineal
#' ajustado (habitualmente devuelto por [MASS::loglm()]). A partir del
#' modelo, la funcion:
#'
#' \enumerate{
#'   \item Extrae las pruebas de bondad de ajuste (razon de verosimilitud
#'         y ji-cuadrado de Pearson) y las presenta en una tabla
#'         formateada con [kable_rstars()].
#'   \item Extrae los coeficientes del modelo mediante
#'         [extraer_coeficientes()] y los presenta en una segunda tabla
#'         formateada con [kable_rstars()].
#'   \item Dibuja un grafico de mosaico con los residuos del modelo
#'         (usando [vcd::mosaic()] con sombreado [vcd::shading_hcl()])
#'         en el dispositivo grafico activo.
#' }
#'
#' Las dos tablas se devuelven en una lista de dos elementos:
#' `Informacion` y `Coeficientes`. El grafico de mosaico es un efecto
#' colateral del renderizado grafico y no forma parte del valor de
#' retorno.
#'
#' @note Esta funcion sustituye a la version anterior (previa a la
#'   integracion en `MATrstars`) que asignaba la lista de resultado en
#'   el Global Environment mediante `assign(..., envir = .GlobalEnv)`.
#'   El nuevo comportamiento requiere que el usuario capture
#'   explicitamente el valor devuelto:
#'   `solucion <- generar_solucion(modelo)`.
#'
#' @param modelo Objeto de modelo log-lineal ajustado (habitualmente
#'   devuelto por [MASS::loglm()]).
#' @param captions Vector de caracteres de longitud 2 con los titulos de
#'   las dos tablas, en orden: validacion y coeficientes. Por defecto,
#'   `c("Validaci\u00f3n del modelo", "Coeficientes del modelo")`.
#' @param mosaic_main Titulo del grafico de mosaico. Por defecto,
#'   `"Residuos del modelo"`.
#' @param ... Argumentos adicionales pasados a [kable_rstars()] (por
#'   ejemplo, `font_size` o `bootstrap_options`), aplicados por igual a
#'   las dos tablas. Si no se pasa `digits`, se aplica un valor por
#'   defecto de `3`.
#'
#' @return Lista con dos elementos:
#' \itemize{
#'   \item `Informacion`: objeto `knitr_kable` con la tabla de las
#'         pruebas de bondad de ajuste.
#'   \item `Coeficientes`: objeto `knitr_kable` con la tabla de
#'         coeficientes del modelo.
#' }
#'
#' @examples
#' \dontrun{
#' modelo_indep <- MASS::loglm(~ GALAXIA + FJUR + EFLO, data = tab_use)
#' solucion <- generar_solucion(modelo_indep)
#'
#' solucion$Informacion    # tabla de validacion
#' solucion$Coeficientes   # tabla de coeficientes
#' # (el mosaico se dibuja automaticamente al llamar a la funcion)
#' }
#'
#' @importFrom vcd mosaic shading_hcl
#' @importFrom grid gpar
#' @export
generar_solucion <- function(modelo,
                             captions = c("Validaci\u00f3n del modelo",
                                          "Coeficientes del modelo"),
                             mosaic_main = "Residuos del modelo",
                             ...) {

  if (!is.character(captions) || length(captions) != 2) {
    stop("`captions` debe ser un vector de caracteres de longitud 2.")
  }

  # Extraer la informacion del modelo
  summary_modelo <- summary(modelo)

  # Argumentos comunes para las dos tablas
  kwargs <- list(...)
  if (!"digits" %in% names(kwargs)) kwargs$digits <- 3

  # --- Tabla 1: pruebas de bondad de ajuste --------------------------------
  tabla_informacion <- data.frame(
    Statistic = c("Likelihood Ratio", "Pearson"),
    X2        = c(summary_modelo$tests[1, "X^2"],
                  summary_modelo$tests[2, "X^2"]),
    df        = c(summary_modelo$tests[1, "df"],
                  summary_modelo$tests[2, "df"]),
    P_value   = c(summary_modelo$tests[1, "P(> X^2)"],
                  summary_modelo$tests[2, "P(> X^2)"]),
    stringsAsFactors = FALSE
  )

  independencia_valida_tab <- do.call(
    kable_rstars,
    c(list(tabla_informacion,
           caption   = captions[1],
           col.names = c("Prueba", "Estad\u00edstico",
                         "Grados Libertad", "P-valor")),
      kwargs)
  )

  # --- Tabla 2: coeficientes del modelo ------------------------------------
  independencia_df <- extraer_coeficientes(modelo)

  independencia_coef_tab <- do.call(
    kable_rstars,
    c(list(independencia_df, caption = captions[2]),
      kwargs)
  )

  # --- Grafico de mosaico --------------------------------------------------
  plot(modelo,
       panel          = vcd::mosaic,
       main           = mosaic_main,
       residuals_type = c("deviance"),
       gp             = vcd::shading_hcl,
       gp_args        = list(interpolate = c(0, 1)),
       main_gp        = grid::gpar(fontsize = 14),
       sub_gp         = grid::gpar(fontsize = 9),
       labeling_args  = list(rot_labels = c(0, 45),
                             gp_labels  = grid::gpar(fontsize = 8)))

  # --- Salida --------------------------------------------------------------
  list(
    Informacion  = independencia_valida_tab,
    Coeficientes = independencia_coef_tab
  )
}
