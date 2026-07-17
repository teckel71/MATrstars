#' Explora (y opcionalmente elimina) outliers univariantes o multivariantes.
#'
#' Diagnostica los outliers (valores atipicos) de un data frame sobre un
#' conjunto de variables metricas, siguiendo la regla clasica de
#' Tukey de 1.5 x IQR:
#'
#' - Si se pasa **una sola** variable, el analisis se hace directamente
#'   sobre ella.
#' - Si se pasan **dos o mas**, se calcula la **distancia de Mahalanobis**
#'   de cada caso y el analisis se hace sobre esa medida sintetica.
#'
#' En ambos casos, genera un grafico `ggplot2::geom_boxplot()` con el
#' estilo del libro y, si se detectan outliers, una tabla con los casos
#' afectados formateada con `kable_rstars()`. Si no se detecta ningun
#' outlier, la funcion solo emite un mensaje informativo. Opcionalmente,
#' elimina del data frame los casos identificados como outliers.
#'
#' La seleccion de variables sigue el estilo *tidy-select* del *tidyverse*
#' (idem que en [explora_na()]).
#'
#' # Variables no metricas
#'
#' Si alguna de las variables seleccionadas no es metrica (por ejemplo,
#' un factor o una cadena), la funcion emite un aviso y no realiza el
#' analisis, devolviendo el data frame sin cambios. El usuario debe
#' seleccionar solo variables numericas.
#'
#' # Caja "aplastada" por outliers extremos
#'
#' Cuando existen outliers muy extremos, la caja del boxplot se comprime
#' y practicamente desaparece. Para mitigarlo, la funcion detecta
#' automaticamente esta situacion (cuando el IQR ocupa menos del 10% del
#' rango total de la variable) y transforma el eje Y con una escala
#' pseudo-logaritmica (`scales::pseudo_log_trans()`), que se comporta
#' como una escala lineal cerca de 0 y como una logaritmica en los
#' extremos. Esta transformacion funciona incluso con valores negativos o
#' nulos, a diferencia de `log10()`. El subtitulo del grafico indica
#' cuando se ha aplicado.
#'
#' El comportamiento se controla con el argumento `escala`:
#' - `"auto"` (por defecto): detecta y aplica pseudo-log si es necesario.
#' - `"lineal"`: fuerza escala lineal (comportamiento clasico).
#' - `"pseudo_log"`: fuerza escala pseudo-logaritmica.
#'
#' # Integracion con el preprocesador del libro R-Stars
#'
#' Igual que [explora_na()], cuando la funcion se ejecuta dentro de un
#' documento R Markdown / knitr, emite el grafico y la tabla con el
#' estilo tipografico habitual e inserta ademas las etiquetas manuales
#' "**Figura X.Y**" y "**Tabla X.Y**" justo debajo de cada uno, leyendo
#' los numeros de las opciones `matrstars.figura_num` y
#' `matrstars.tabla_num` que el preprocesador `numerar_tablas_figuras.R`
#' inyecta. El chunk debe llevar `results='asis'`.
#'
#' @param datos      Data frame de entrada.
#' @param variables  Variables a analizar (tidy-select). Si se omite, se
#'                   analizan todas las columnas de `datos` que sean
#'                   numericas.
#' @param accion     `"documentar"` (por defecto) solo diagnostica.
#'                   `"eliminar"` ademas filtra los casos identificados
#'                   como outliers.
#' @param titulo     Titulo del grafico. Si es `NULL` (por defecto), se
#'                   genera automaticamente segun sea univariante o
#'                   multivariante.
#' @param subtitulo  Subtitulo del grafico. Por defecto, `NULL`.
#' @param escala     `"auto"` (por defecto): detecta automaticamente si la
#'                   caja se comprime y aplica escala pseudo-logaritmica.
#'                   `"lineal"`: siempre escala lineal.
#'                   `"pseudo_log"`: siempre escala pseudo-logaritmica.
#' @param caption    Titulo de la tabla `kable_rstars()` con los casos
#'                   outliers. Si es `NULL` (por defecto), se genera
#'                   automaticamente.
#' @param ...        Argumentos adicionales reenviados a [kable_rstars()]
#'                   (por ejemplo, `digits`, `font_size` o
#'                   `bootstrap_options`).
#'
#' @return El data frame (invisible): sin cambios si
#'         `accion = "documentar"`, o filtrado (sin los casos outliers) si
#'         `accion = "eliminar"`. En el caso multivariante, la columna
#'         auxiliar MAHALANOBIS que se calcula internamente NO se
#'         conserva en el data frame devuelto.
#'
#' @examples
#' \dontrun{
#'   library(MATrstars)
#'
#'   # Univariante:
#'   muestra_so <- explora_outliers(
#'     muestra, RENECO,
#'     accion = "eliminar",
#'     titulo = "Rentabilidad Economica"
#'   )
#'
#'   # Multivariante (via distancia de Mahalanobis):
#'   muestra2_so <- explora_outliers(
#'     muestra2, c(RENECO, ACTIVO, MARGEN, RES),
#'     accion = "eliminar"
#'   )
#'
#'   # Todo el df (todas las columnas numericas):
#'   seleccion_so <- explora_outliers(seleccion, accion = "eliminar")
#' }
#'
#' @importFrom dplyr select
#' @importFrom rlang enquo quo_is_null .data
#' @importFrom ggplot2 ggplot aes geom_boxplot labs theme element_text
#'   element_blank scale_y_continuous
#' @importFrom stats mahalanobis cov quantile IQR complete.cases
#' @importFrom scales pseudo_log_trans
#' @export
explora_outliers <- function(datos,
                             variables  = NULL,
                             accion     = c("documentar", "eliminar"),
                             titulo     = NULL,
                             subtitulo  = NULL,
                             escala     = c("auto", "lineal", "pseudo_log"),
                             caption    = NULL,
                             ...) {

  accion <- match.arg(accion)
  escala <- match.arg(escala)

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

  # 2. Validacion: todas las variables deben ser metricas -------------------
  no_metricas <- variables_char[!vapply(sub, is.numeric, logical(1))]
  if (length(no_metricas) > 0) {
    message(
      "AVISO: Las siguientes variables no son metricas y no pueden ",
      "analizarse para outliers: ",
      paste(no_metricas, collapse = ", "), ". ",
      "El analisis no se ha realizado. Seleccione solo variables ",
      "numericas."
    )
    return(invisible(datos))
  }

  n_vars <- length(variables_char)
  if (n_vars < 1L) {
    message("AVISO: No hay variables para analizar.")
    return(invisible(datos))
  }

  # 3. Calculo de la metrica de referencia ---------------------------------
  # Univariante: la variable en si. Multivariante: distancia de Mahalanobis.
  if (n_vars == 1L) {
    var_metrica <- variables_char[1]
    valores     <- sub[[1]]
    etiqueta_y  <- var_metrica
    if (is.null(titulo)) titulo <- paste("Outliers en", var_metrica)
    # Alineamiento con el df de entrada: mismos indices
    indice_orig <- seq_len(nrow(datos))
    valores_alineados <- valores  # mismo orden que datos
  } else {
    # Multivariante: mahalanobis requiere casos completos
    filas_completas <- stats::complete.cases(sub)
    if (!any(filas_completas)) {
      message(
        "AVISO: No hay ningun caso completo en las variables ",
        "seleccionadas. No se puede calcular Mahalanobis."
      )
      return(invisible(datos))
    }
    if (!all(filas_completas)) {
      message(
        "AVISO: ", sum(!filas_completas), " caso(s) con NAs en las ",
        "variables analizadas seran ignorados en el calculo de ",
        "Mahalanobis. Considere aplicar explora_na() antes."
      )
    }
    X   <- as.matrix(sub[filas_completas, , drop = FALSE])
    dm  <- stats::mahalanobis(X, center = colMeans(X), cov = stats::cov(X))
    var_metrica <- "MAHALANOBIS"
    valores     <- dm
    etiqueta_y  <- "Distancia de Mahalanobis"
    if (is.null(titulo)) {
      titulo <- paste0(
        "Outliers (Mahalanobis) en: ",
        paste(variables_char, collapse = ", ")
      )
    }
    # Alineamiento con el df de entrada
    indice_orig <- which(filas_completas)
    valores_alineados <- rep(NA_real_, nrow(datos))
    valores_alineados[filas_completas] <- dm
  }

  # 4. Deteccion de outliers por regla 1.5 x IQR ---------------------------
  Q1  <- stats::quantile(valores, 0.25, na.rm = TRUE)
  Q3  <- stats::quantile(valores, 0.75, na.rm = TRUE)
  iqr <- Q3 - Q1
  lim_inf <- Q1 - 1.5 * iqr
  lim_sup <- Q3 + 1.5 * iqr

  es_outlier <- valores_alineados < lim_inf | valores_alineados > lim_sup
  es_outlier[is.na(es_outlier)] <- FALSE
  hay_outliers <- any(es_outlier)

  # 5. Caso "sin outliers": mensaje y salida limpia ------------------------
  if (!hay_outliers) {
    message(
      "OK. No se han detectado outliers en las variables analizadas: ",
      paste(variables_char, collapse = ", "), "."
    )
    return(invisible(datos))
  }

  # 6. Decidir escala del eje Y --------------------------------------------
  # Criterio "auto": si IQR ocupa menos del 10% del rango total, la caja
  # se comprime; pasamos a pseudo-log para verla bien.
  rango <- range(valores, na.rm = TRUE)
  amp   <- diff(rango)
  ratio_iqr_rango <- if (amp > 0) as.numeric(iqr) / as.numeric(amp) else 1

  escala_efectiva <- if (escala == "auto") {
    if (ratio_iqr_rango < 0.10) "pseudo_log" else "lineal"
  } else {
    escala
  }

  # 7. Grafico --------------------------------------------------------------
  df_plot <- data.frame(y = valores)
  subtitulo_efectivo <- subtitulo
  if (escala_efectiva == "pseudo_log") {
    aviso_esc <- "(eje Y en escala pseudo-logaritmica por rango extremo)"
    subtitulo_efectivo <- if (is.null(subtitulo) || !nzchar(subtitulo)) {
      aviso_esc
    } else {
      paste(subtitulo, aviso_esc, sep = "\n")
    }
  }

  g <- ggplot2::ggplot(df_plot, ggplot2::aes(y = .data[["y"]])) +
    ggplot2::geom_boxplot(fill = "orange", alpha = 0.9) +
    ggplot2::labs(
      title    = titulo,
      subtitle = subtitulo_efectivo,
      y        = etiqueta_y,
      x        = NULL
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      axis.text.x  = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )

  if (escala_efectiva == "pseudo_log") {
    g <- g +
      ggplot2::scale_y_continuous(trans = scales::pseudo_log_trans())
  }
  print(g)

  # Etiqueta de la figura, inmediatamente debajo del grafico.
  emitir_etiqueta("figura")

  # 8. Tabla de casos outliers ---------------------------------------------
  filas_out <- which(es_outlier)
  if (n_vars == 1L) {
    # Solo la variable analizada, ordenada
    casos_out <- datos[filas_out, variables_char, drop = FALSE]
    # Ordenar por el valor de la variable
    casos_out <- casos_out[order(casos_out[[1]]), , drop = FALSE]
  } else {
    # Mahalanobis + variables originales
    casos_out <- data.frame(
      MAHALANOBIS = valores_alineados[filas_out],
      datos[filas_out, variables_char, drop = FALSE],
      check.names = FALSE
    )
    # Preservar los rownames del df original
    rownames(casos_out) <- rownames(datos)[filas_out]
    # Ordenar por Mahalanobis descendente
    casos_out <- casos_out[order(-casos_out$MAHALANOBIS), , drop = FALSE]
  }

  if (is.null(caption)) {
    caption <- if (n_vars == 1L) {
      paste0(
        "Casos outliers en ", var_metrica,
        " (", nrow(casos_out), ")"
      )
    } else {
      paste0(
        "Casos outliers segun Mahalanobis (", nrow(casos_out),
        ") en: ", paste(variables_char, collapse = ", ")
      )
    }
  }

  tabla_out <- kable_rstars(casos_out, caption = caption, ...)

  if (isTRUE(getOption("knitr.in.progress"))) {
    cat("\n\n", as.character(tabla_out), "\n\n", sep = "")
  } else {
    print(tabla_out)
  }

  # Etiqueta de la tabla, inmediatamente debajo de la tabla.
  emitir_etiqueta("tabla")

  # 9. Accion opcional: eliminar -------------------------------------------
  if (accion == "eliminar") {
    datos <- datos[!es_outlier, , drop = FALSE]
    message(
      "Se han eliminado ", sum(es_outlier),
      " caso(s) outlier. El data frame resultante contiene ",
      nrow(datos), " observaciones."
    )
  }

  invisible(datos)
}
