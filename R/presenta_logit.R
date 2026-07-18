#' Presenta los resultados de un modelo logit binomial
#'
#' Convierte un objeto \code{glm} de familia \code{binomial(link = "logit")} en
#' una presentacion formateada con el estilo del libro R-Stars, compuesta por
#' cuatro tablas: coeficientes con \emph{odd ratios}, bondad del ajuste,
#' \emph{odd ratios} interpretados y matriz de confusion. Todas las tablas se
#' construyen internamente con \code{\link{kable_rstars}()}.
#'
#' @param modelo Objeto de clase \code{glm} ajustado con
#'   \code{family = binomial(link = "logit")}.
#' @param datos Data frame utilizado para la estimacion. Si se proporciona, se
#'   calcula la matriz de confusion (tabla 4). Si es \code{NULL} (por defecto),
#'   la tabla 4 no se genera.
#' @param umbral Umbral de probabilidad para la clasificacion
#'   (por defecto 0.5).
#' @param captions Vector de caracteres de longitud 4 con los titulos de las
#'   tablas: coeficientes, bondad del ajuste, odd ratios y matriz de confusion.
#' @param ... Argumentos adicionales pasados a \code{\link{kable_rstars}()}
#'   (por ejemplo, \code{font_size} o \code{bootstrap_options}), aplicados por
#'   igual a todas las tablas.
#'
#' @return Lista de 3 o 4 objetos \code{knitr_kable} con nombres \code{coef},
#'   \code{gof}, \code{or} y, opcionalmente, \code{conf}. Accesibles tanto por
#'   posicion (\code{x[[1]]}) como por nombre (\code{x$coef}).
#'
#' @details
#' \strong{Tabla 1 – Coeficientes:} estimacion, error estandar, estadistico z
#' (contraste de Wald individual), p-valor y nivel de significacion.
#'
#' \strong{Tabla 2 – Bondad del ajuste:} pseudo R\eqn{^2} de Nagelkerke, AIC,
#' devianza nula, devianza residual y numero de observaciones.
#'
#' \strong{Tabla 3 – Odd ratios:} \eqn{e^{\hat{\beta}_j}} para cada variable,
#' junto con la variacion porcentual de la ventaja (\emph{odds}) cuando la
#' variable se incrementa en una unidad. Un valor positivo indica que la ventaja
#' aumenta; uno negativo, que disminuye.
#'
#' \strong{Tabla 4 – Matriz de confusion} (solo si se suministra \code{datos}):
#' tabla cruzada entre valores observados y predichos, junto con el porcentaje
#' de predicciones correctas.
#'
#' @examples
#' \dontrun{
#' library(MATrstars)
#'
#' modelo <- glm(am ~ wt + hp + disp, data = mtcars,
#'               family = binomial(link = "logit"))
#'
#' salida <- presenta_logit(modelo, datos = mtcars)
#'
#' salida$coef    # Tabla de coeficientes
#' salida$gof     # Bondad del ajuste
#' salida$or      # Odd ratios
#' salida$conf    # Matriz de confusion
#' }
#'
#' @importFrom broom tidy glance
#' @export
presenta_logit <- function(modelo,
                           datos    = NULL,
                           umbral   = 0.5,
                           captions = c("Modelo Logit Binomial",
                                        "Bondad del ajuste",
                                        "Odd ratios",
                                        "Matriz de confusi\u00f3n"),
                           ...) {

  # --- Validaciones -----------------------------------------------------------
  if (!inherits(modelo, "glm")) {
    stop("`modelo` debe ser un objeto de clase `glm`.")
  }
  if (!is.character(captions) || length(captions) != 4) {
    stop("`captions` debe ser un vector de caracteres de longitud 4.")
  }

  piezas <- list()

  # --- Tabla 1: Coeficientes -------------------------------------------------
  resultados <- broom::tidy(modelo)
  resultados <- resultados[, c("term", "estimate", "std.error",
                               "statistic", "p.value")]

  # Columna de significacion (estrellas)
  resultados$stars <- cut(resultados$p.value,
                          breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                          labels = c("***", "**", "*", "\u00b7", " "),
                          right  = FALSE)
  resultados$estimate <- formatC(resultados$estimate,
                                 format = "f", digits = 5)

  piezas[["coef"]] <- kable_rstars(
    resultados,
    caption   = captions[1],
    col.names = c("Variable", "Coeficiente", "Desv. T\u00edpica",
                  "Estad\u00edstico z", "p-valor", "Sig."),
    digits    = 4,
    ...
  )

  # --- Tabla 2: Bondad del ajuste --------------------------------------------
  # Pseudo R2 de Nagelkerke (calculado manualmente para evitar dependencia
  # del paquete fmsb)
  n  <- nrow(modelo$model)
  L1 <- as.numeric(stats::logLik(modelo))
  L0 <- as.numeric(stats::logLik(stats::update(modelo, . ~ 1)))

  cox_snell  <- 1 - exp(-(2 / n) * (L1 - L0))
  nagelkerke <- cox_snell / (1 - exp((2 / n) * L0))

  gof <- data.frame(
    Nagelkerke_R2      = nagelkerke,
    AIC                = stats::AIC(modelo),
    Devianza_Nula      = modelo$null.deviance,
    gl_nula            = modelo$df.null,
    Devianza_Residual  = modelo$deviance,
    gl_residual        = modelo$df.residual,
    n                  = n
  )

  piezas[["gof"]] <- kable_rstars(
    gof,
    caption   = captions[2],
    col.names = c("R\u00b2 Nagelkerke", "AIC",
                  "Devianza nula", "g.l.",
                  "Devianza residual", "g.l.",
                  "n"),
    digits    = 4,
    ...
  )

  # --- Tabla 3: Odd ratios ---------------------------------------------------
  coefs      <- stats::coef(modelo)
  odd_ratios <- exp(coefs)
  cambio_pct <- (odd_ratios - 1) * 100

  or_df <- data.frame(
    Variable  = names(coefs),
    Beta      = coefs,
    OR        = odd_ratios,
    Cambio    = cambio_pct,
    row.names = NULL
  )

  piezas[["or"]] <- kable_rstars(
    or_df,
    caption   = captions[3],
    col.names = c("Variable",
                  "Coeficiente",
                  "Odd Ratio (e^\u03b2)",
                  "Cambio en ventaja (%)"),
    digits    = 4,
    ...
  )

  # --- Tabla 4: Matriz de confusion (opcional) --------------------------------
  if (!is.null(datos)) {
    pred_prob  <- stats::predict(modelo, newdata = datos, type = "response")

    # Obtener los niveles de la variable dependiente
    var_dep    <- as.character(stats::formula(modelo)[[2]])
    y_real     <- datos[[var_dep]]

    # Si y_real es factor, usar sus niveles; si no, usar 0/1
    if (is.factor(y_real)) {
      niveles    <- levels(y_real)
      pred_class <- factor(ifelse(pred_prob > umbral, niveles[2], niveles[1]),
                           levels = niveles)
    } else {
      niveles    <- c(0, 1)
      pred_class <- ifelse(pred_prob > umbral, 1, 0)
      y_real     <- factor(y_real, levels = c(0, 1))
      pred_class <- factor(pred_class, levels = c(0, 1))
    }

    tb <- table(Observado = y_real, Predicho = pred_class)
    acierto <- round(sum(diag(tb)) / sum(tb) * 100, 2)

    # Convertir tabla a data frame para kable_rstars
    conf_df <- as.data.frame.matrix(tb)
    conf_df <- cbind(Observado = rownames(conf_df), conf_df)
    rownames(conf_df) <- NULL

    # Anadir fila con el porcentaje de acierto
    fila_acierto <- c(paste0("% Correctas: ", acierto, "%"),
                      rep("", ncol(conf_df) - 1))
    conf_df <- rbind(conf_df, fila_acierto)

    piezas[["conf"]] <- kable_rstars(
      conf_df,
      caption   = captions[4],
      col.names = c("Observado \\ Predicho",
                     names(as.data.frame.matrix(tb))),
      ...
    )
  }

  return(piezas)
}
