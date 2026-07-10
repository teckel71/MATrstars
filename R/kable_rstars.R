#' Presentar una tabla con el estilo del libro R-Stars
#'
#' Genera una tabla HTML aplicando el estilo tipográfico y de color
#' utilizado sistemáticamente en el libro *R-Stars* (encabezado en negrita
#' centrado, cuerpo con líneas alternas, bordes discretos, ancho ajustado,
#' cuerpo centrado). Es un envoltorio de `knitr::kable()` seguido de
#' `kableExtra::kable_styling()` y `kableExtra::row_spec()` con los ajustes
#' habituales del libro; los defaults resuelven el 95% de las tablas, y el
#' resto se puede personalizar sobrescribiendo los argumentos correspondientes.
#'
#' Sustituye el bloque repetitivo de 12-18 líneas que aparece en cada tabla
#' de los *scripts* del libro por una única llamada compacta. Además, elimina
#' de raíz un error frecuente que aparecía en varias tablas (los tres
#' `bootstrap_options` sin envolver en `c(...)`, lo que hacía que el estilo
#' solo aplicara `"striped"` y descartara los otros dos).
#'
#' @param x Data frame o matriz con los datos a mostrar.
#' @param caption Título de la tabla. Por defecto `NULL` (sin título).
#' @param col.names Vector de caracteres con los nombres de columna a mostrar.
#'   Por defecto `NULL` (se usan los nombres del data frame).
#' @param digits Número de dígitos decimales. Puede ser un escalar (aplicado
#'   a todas las columnas numéricas) o un vector con un valor por columna
#'   (`NA` para columnas no numéricas). Por defecto, el valor de
#'   `getOption("digits")`.
#' @param font_size Tamaño de fuente en puntos. Por defecto `11`.
#' @param full_width Lógico. Si `TRUE`, la tabla ocupa todo el ancho
#'   disponible. Por defecto `FALSE`.
#' @param bootstrap_options Vector de caracteres con opciones de estilo de
#'   Bootstrap para `kable_styling()`. Por defecto
#'   `c("striped", "bordered", "condensed")`.
#' @param align Alineación de las celdas del cuerpo. Por defecto `"c"`
#'   (centrado). Ver `kableExtra::row_spec()`.
#' @param align_header Alineación de las celdas del encabezado. Por defecto
#'   `"c"` (centrado).
#' @param bold_header Lógico. Si `TRUE` (por defecto), el encabezado va en
#'   negrita.
#' @param bold_body Lógico. Si `TRUE`, el cuerpo va en negrita. Por defecto
#'   `FALSE`.
#' @param format.args Lista con argumentos de formato numérico pasada a
#'   `knitr::kable()`. Por defecto, punto como separador decimal y
#'   notación científica desactivada.
#' @param ... Argumentos adicionales pasados a `knitr::kable()`.
#'
#' @return Un objeto de clase `knitr_kable` con el estilo del libro
#'   aplicado, listo para su inclusión en un documento R Markdown con
#'   salida HTML.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(MATrstars)
#'
#' # Ejemplo con caption y col.names personalizados
#' mtcars %>%
#'   head(5) %>%
#'   kable_rstars(caption   = "Primeros 5 coches del dataset mtcars",
#'                col.names = c("mpg", "cil", "cc", "hp", "gr", "peso",
#'                              "1/4 mi", "vs", "am", "marchas", "carbs"),
#'                digits    = 2)
#' }
#'
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling row_spec
#' @importFrom magrittr %>%
#' @export
kable_rstars <- function(x,
                         caption   = NULL,
                         col.names = NULL,
                         digits    = getOption("digits"),
                         font_size = 11,
                         full_width = FALSE,
                         bootstrap_options = c("striped", "bordered", "condensed"),
                         align  = "c",
                         align_header = "c",
                         bold_header  = TRUE,
                         bold_body    = FALSE,
                         format.args  = list(decimal.mark = ".", scientific = FALSE),
                         ...) {

  # Determinar número de filas de forma robusta (data frame, matrix, tibble)
  n_filas <- NROW(x)

  # Construir la llamada a kable() pasando solo los argumentos no nulos
  # para que knitr::kable use sus propios defaults cuando corresponda.
  kable_args <- list(x           = x,
                     caption     = caption,
                     col.names   = col.names,
                     digits      = digits,
                     format.args = format.args,
                     ...)

  tabla <- do.call(knitr::kable, kable_args) %>%
    kableExtra::kable_styling(full_width        = full_width,
                              bootstrap_options = bootstrap_options,
                              position          = "center",
                              font_size         = font_size) %>%
    kableExtra::row_spec(0,
                         bold  = bold_header,
                         align = align_header)

  # row_spec sobre el cuerpo solo si la tabla tiene filas
  if (n_filas > 0) {
    tabla <- tabla %>%
      kableExtra::row_spec(seq_len(n_filas),
                           bold  = bold_body,
                           align = align)
  }

  tabla
}
