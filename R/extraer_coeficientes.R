#' Extraer los coeficientes de un modelo log-lineal
#'
#' Toma un modelo log-lineal ajustado (habitualmente devuelto por
#' [MASS::loglm()]) y descompone sus parametros (`modelo$param`) en un
#' data frame plano con dos columnas: `Coefficient` (nombre) y `Value`
#' (valor estimado). El nombre se construye combinando el termino y los
#' niveles asociados, separados por `":"`, para facilitar la lectura de
#' modelos con multiples interacciones.
#'
#' La funcion contempla los distintos casos que puede presentar
#' `modelo$param`:
#' \itemize{
#'   \item Intercepto: se etiqueta como `"T. Independiente"`.
#'   \item Efectos principales: el nombre resulta de `variable:nivel`.
#'   \item Interacciones dobles (matrices): se recorren filas y columnas,
#'         etiquetando como `nivel_fila:nivel_columna`.
#'   \item Interacciones triples (arrays 3D): se recorren las tres
#'         dimensiones, etiquetando como `nivel1:nivel2:nivel3`.
#' }
#'
#' Se utiliza internamente en [generar_solucion()], pero se expone como
#' funcion publica del paquete para permitir su uso independiente si el
#' usuario quiere construir su propia presentacion de resultados.
#'
#' @param modelo Modelo log-lineal ajustado (habitualmente devuelto por
#'   [MASS::loglm()]).
#'
#' @return Un data frame con dos columnas:
#' \itemize{
#'   \item `Coefficient`: nombre del coeficiente.
#'   \item `Value`: valor estimado del coeficiente.
#' }
#'
#' @examples
#' \dontrun{
#' modelo <- MASS::loglm(~ GALAXIA + FJUR + EFLO, data = tab_use)
#' extraer_coeficientes(modelo)
#' }
#'
#' @export
extraer_coeficientes <- function(modelo) {
  # Extraer los parametros del modelo
  parametros <- modelo$param

  # Crear listas para almacenar nombres y valores de coeficientes
  coef_names  <- c()
  coef_values <- c()

  # Funcion para generar nombres de coeficientes
  generate_coef_name <- function(levels) {
    return(paste(levels, collapse = ":"))
  }

  # Recorrer los coeficientes y extraer los nombres y valores
  for (term in names(parametros)) {
    if (term == "(Intercept)") {
      coef_names  <- c(coef_names, "T. Independiente")
      coef_values <- c(coef_values, parametros[[term]])
    } else if (is.matrix(parametros[[term]])) {
      # Si es una matriz, recorrer filas y columnas
      for (i in 1:nrow(parametros[[term]])) {
        for (j in 1:ncol(parametros[[term]])) {
          coef_names <- c(coef_names,
                          paste(rownames(parametros[[term]])[i],
                                colnames(parametros[[term]])[j],
                                sep = ":"))
          coef_values <- c(coef_values, parametros[[term]][i, j])
        }
      }
    } else if (is.array(parametros[[term]])) {
      # Si es un array de mas de dos dimensiones
      dims <- dim(parametros[[term]])
      dimnames_list <- dimnames(parametros[[term]])
      for (i in seq_len(dims[1])) {
        for (j in seq_len(dims[2])) {
          for (k in seq_len(dims[3])) {
            coef_name <- paste(dimnames_list[[1]][i],
                               dimnames_list[[2]][j],
                               dimnames_list[[3]][k],
                               sep = ":")
            coef_names  <- c(coef_names, coef_name)
            coef_values <- c(coef_values,
                             parametros[[term]][i, j, k])
          }
        }
      }
    } else {
      levels <- names(parametros[[term]])
      for (level in levels) {
        coef_names  <- c(coef_names,
                         generate_coef_name(c(term, level)))
        coef_values <- c(coef_values,
                         parametros[[term]][[level]])
      }
    }
  }

  # Verificar la longitud de los vectores antes de crear el data frame
  if (length(coef_names) == length(coef_values)) {
    tabla_coeficientes <- data.frame(Coefficient = coef_names,
                                     Value       = coef_values,
                                     stringsAsFactors = FALSE)
    return(tabla_coeficientes)
  } else {
    stop("Error: Las longitudes de coef_names y coef_values no coinciden.")
  }
}
