#' Detectar celdas con frecuencia cero en una tabla de contingencia
#'
#' Identifica de forma robusta todas las celdas con frecuencia cero en una
#' tabla de contingencia de cualquier numero de dimensiones. Es util antes
#' de estimar un modelo log-lineal, dado que la presencia de ceros
#' estructurales o de muestreo puede causar fallos de estimacion o
#' inestabilidad numerica.
#'
#' Si la tabla tiene `dimnames`, la funcion mapea los indices numericos a
#' las etiquetas correspondientes de cada dimension. Si no los tiene,
#' devuelve los indices tal cual y crea nombres genericos (`Var1`, `Var2`,
#' etc.) para las columnas del data frame de salida.
#'
#' @param tab Tabla de contingencia (objeto `table`, `array` o `matrix`) de
#'   cualquier numero de dimensiones.
#'
#' @return Un data frame con una fila por celda de frecuencia cero, con una
#'   columna por dimension de la tabla y una columna adicional
#'   `Frecuencia_Original` con valor `0L`. Si no hay celdas con frecuencia
#'   cero, devuelve `NULL`.
#'
#' @examples
#' \dontrun{
#' # Tabla de contingencia tridimensional
#' tab <- xtabs(~ GALAXIA + FJUR + EFLO, data = seleccion)
#'
#' # Detectar celdas con frecuencia cero
#' zeros <- detect_zeros_any(tab)
#' if (is.null(zeros)) {
#'   message("No hay celdas con frecuencia 0.")
#' } else {
#'   print(zeros)
#' }
#' }
#'
#' @export
detect_zeros_any <- function(tab) {
  idx <- which(tab == 0, arr.ind = TRUE)
  # Si no hay ceros, devuelve NULL
  if (is.null(idx) ||
      (is.matrix(idx) && nrow(idx) == 0) ||
      (is.vector(idx) && length(idx) == 0)) {
    return(NULL)
  }
  idx <- as.data.frame(idx)
  dn  <- dimnames(tab)
  dn_names <- names(dn)
  if (is.null(dn_names) || any(nchar(dn_names) == 0)) {
    dn_names <- paste0("Var", seq_along(dn))
  }
  # Mapear indices -> etiquetas por dimension (si hay dimnames)
  out_cols <- lapply(seq_along(dn), function(k) {
    if (!is.null(dn[[k]])) dn[[k]][ idx[[k]] ] else idx[[k]]
  })
  names(out_cols) <- dn_names
  out <- as.data.frame(out_cols, stringsAsFactors = FALSE)
  out$Frecuencia_Original <- 0L
  out
}
