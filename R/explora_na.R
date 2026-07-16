#' Explora (y opcionalmente elimina) valores ausentes.
#'
#' Diagnostica los valores ausentes (`NA`) de un data frame sobre un
#' conjunto de variables. Genera un grafico [visdat::vis_miss()] con el
#' estilo del libro y, si hay `NA`s, una tabla con los casos afectados
#' formateada con [kable_rstars()]. Si no se detecta ningun `NA`, la
#' funcion solo emite un mensaje informativo. Opcionalmente, elimina del
#' data frame los casos con algun `NA` en las variables analizadas.
#'
#' La seleccion de variables sigue el estilo *tidy-select* del *tidyverse*:
#' se pueden pasar los nombres sin comillas, con `c()`, o con los *helpers*
#' de [dplyr::select()] (por ejemplo, `starts_with("I")`). Si no se
#' especifica ninguna variable, se analizan todas las columnas del data
#' frame.
#'
#' Cuando el numero de variables analizadas es reducido (hasta 4), la
#' funcion corrige la posicion por defecto de las etiquetas del eje x
#' superior en el grafico de `vis_miss()`, que en ese caso quedan muy
#' separadas del panel. Para mas de 4 variables, mantiene el angulo de 45
#' grados habitual.
#'
#' # Integracion con el preprocesador del libro R-Stars
#'
#' Cuando la funcion se ejecuta dentro de un documento R Markdown / knitr,
#' emite el grafico y la tabla con el estilo tipografico habitual, e
#' inserta ademas las etiquetas manuales "**Figura X.Y**" y
#' "**Tabla X.Y**" justo debajo de cada uno de ellos, para respetar el
#' patron gráfico -> etiqueta -> tabla -> etiqueta del libro.
#'
#' Para conocer los numeros X.Y de la figura y de la tabla, la funcion
#' lee las opciones de sesion `matrstars.figura_num` y
#' `matrstars.tabla_num`, que el preprocesador `numerar_tablas_figuras.R`
#' inyecta al principio del chunk que la contiene. Si esas opciones no
#' estan definidas (por ejemplo, porque la funcion se ejecuta en una
#' sesion interactiva, en un script sin preprocesado, o en un chunk que
#' no es el "oficial" del libro), la funcion NO emite etiquetas: solo el
#' grafico y la tabla.
#'
#' El chunk debe llevar la opcion `results='asis'` para que las tablas
#' HTML y las etiquetas markdown se rendericen correctamente.
#'
#' @param datos      Data frame de entrada.
#' @param variables  Variables a analizar (tidy-select). Si se omite, se
#'                   analizan todas las columnas de `datos`.
#' @param accion     `"documentar"` (por defecto) solo diagnostica.
#'                   `"eliminar"` ademas filtra los casos con algun `NA`
#'                   en las variables analizadas.
#' @param titulo     Titulo del grafico. Por defecto, `"Valores ausentes"`.
#' @param subtitulo  Subtitulo del grafico. Por defecto, `NULL`.
#' @param etiqueta_y Etiqueta del eje Y. Por defecto, `"Observacion"`.
#' @param caption    Titulo de la tabla `kable_rstars()` con los casos con
#'                   `NA`. Si es `NULL` (por defecto), se genera
#'                   automaticamente.
#' @param ...        Argumentos adicionales reenviados a [kable_rstars()]
#'                   (por ejemplo, `digits`, `font_size` o
#'                   `bootstrap_options`).
#'
#' @return El data frame (invisible): sin cambios si
#'         `accion = "documentar"`, o filtrado (solo casos completos en las
#'         variables analizadas) si `accion = "eliminar"`.
#'
#' @examples
#' \dontrun{
#'   library(MATrstars)
#'
#'   # Una sola variable (sin comillas):
#'   muestra <- explora_na(
#'     muestra, RENECO,
#'     accion    = "eliminar",
#'     titulo    = "Rentabilidad Economica: valores ausentes",
#'     subtitulo = "Transporte de mercancias interestelar"
#'   )
#'
#'   # Varias variables (sin comillas):
#'   muestra2 <- explora_na(
#'     muestra2, c(RENECO, ACTIVO, MARGEN, RES),
#'     accion = "eliminar"
#'   )
#'
#'   # Todo el data frame (sin argumento `variables`):
#'   seleccion <- explora_na(seleccion, accion = "eliminar")
#' }
#'
#' @importFrom visdat vis_miss
#' @importFrom dplyr select
#' @importFrom rlang enquo quo_is_null
#' @importFrom ggplot2 labs scale_fill_manual theme element_text margin
#' @importFrom stats complete.cases
#' @export
explora_na <- function(datos,
                       variables  = NULL,
                       accion     = c("documentar", "eliminar"),
                       titulo     = "Valores ausentes",
                       subtitulo  = NULL,
                       etiqueta_y = "Observaci\u00f3n",
                       caption    = NULL,
                       ...) {

  accion <- match.arg(accion)

  # Helper: emite (y consume) una etiqueta manual del tipo indicado
  # si estamos en knit y el preprocesador ha inyectado el numero
  # correspondiente en la opcion de sesion.
  emitir_etiqueta <- function(tipo) {
    if (!isTRUE(getOption("knitr.in.progress"))) return(invisible())
    nombre_opcion <- paste0("matrstars.", tipo, "_num")
    num <- getOption(nombre_opcion, NULL)
    if (is.null(num) || !nzchar(as.character(num))) return(invisible())
    etiqueta <- if (tipo == "figura") "Figura" else "Tabla"
    cat(
      "\n\n::: {style=\"text-align: center;\"}\n",
      "**", etiqueta, " ", num, "**\n",
      ":::\n\n",
      sep = ""
    )
    # Consumir la opcion: si por error el chunk contiene mas de una
    # llamada, la segunda no reutiliza el mismo numero.
    args_reset <- list(NULL)
    names(args_reset) <- nombre_opcion
    do.call(options, args_reset)
    invisible()
  }

  # 1. Seleccion de variables (tidy-select) ---------------------------------
  vars_expr <- rlang::enquo(variables)
  if (rlang::quo_is_null(vars_expr)) {
    sub <- datos
  } else {
    sub <- dplyr::select(datos, !!vars_expr)
  }
  variables_char <- names(sub)

  # 2. Hay NAs? -------------------------------------------------------------
  hay_na <- any(is.na(sub))

  # 3. Caso "sin NAs": mensaje y salida limpia ------------------------------
  if (!hay_na) {
    message(
      "OK. No se han detectado valores ausentes en las variables ",
      "analizadas: ", paste(variables_char, collapse = ", "), "."
    )
    return(invisible(datos))
  }

  # 4. Caso "con NAs": grafico bien maquetado -------------------------------
  n_vars  <- length(variables_char)
  angulo  <- if (n_vars <= 4) 0   else 45
  hjust_x <- if (n_vars <= 4) 0.5 else 0
  vjust_x <- if (n_vars <= 4) 1   else 0

  g <- visdat::vis_miss(sub) +
    ggplot2::labs(
      title    = titulo,
      subtitle = subtitulo,
      y        = etiqueta_y,
      fill     = NULL
    ) +
    ggplot2::scale_fill_manual(
      values = c("TRUE" = "red",  "FALSE" = "grey"),
      labels = c("TRUE" = "NA",   "FALSE" = "Presente")
    ) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold", size = 14),
      axis.text.x.top = ggplot2::element_text(
        angle  = angulo,
        hjust  = hjust_x,
        vjust  = vjust_x,
        margin = ggplot2::margin(b = 2)
      )
    )
  print(g)

  # Etiqueta de la figura, inmediatamente debajo del grafico.
  emitir_etiqueta("figura")

  # 5. Listado de casos con NAs --------------------------------------------
  filas_na <- Reduce(`|`, lapply(sub, is.na))
  casos_na <- datos[filas_na, variables_char, drop = FALSE]

  if (is.null(caption)) {
    caption <- paste0(
      "Casos con valores ausentes (", nrow(casos_na),
      ") en: ", paste(variables_char, collapse = ", ")
    )
  }

  tabla_na <- kable_rstars(casos_na, caption = caption, ...)

  # Emision robusta de la tabla:
  #  - En knit (RMarkdown / bookdown): cat() del HTML crudo. El chunk
  #    debe llevar `results='asis'` para que se renderice.
  #  - Fuera de knit: print() normal.
  if (isTRUE(getOption("knitr.in.progress"))) {
    cat("\n\n", as.character(tabla_na), "\n\n", sep = "")
  } else {
    print(tabla_na)
  }

  # Etiqueta de la tabla, inmediatamente debajo de la tabla.
  emitir_etiqueta("tabla")

  # 6. Accion opcional: eliminar --------------------------------------------
  if (accion == "eliminar") {
    completos <- stats::complete.cases(sub)
    datos     <- datos[completos, , drop = FALSE]
    message(
      "Se han eliminado ", sum(!completos),
      " caso(s) con NA. El data frame resultante contiene ",
      nrow(datos), " observaciones."
    )
  }

  invisible(datos)
}
