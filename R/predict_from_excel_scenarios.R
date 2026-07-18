#' Predecir por escenarios definidos en Excel
#'
#' Lee escenarios de simulacion o prevision desde una hoja de un archivo Excel,
#' donde cada celda puede contener un valor numerico o una formula de R (por
#' ejemplo, `mean(train$ACTIVO)` o `df1$IVENTAS[1]`), y devuelve las
#' predicciones del modelo para cada escenario. Opcionalmente, presenta los
#' resultados en una tabla formateada con [kable_rstars()].
#'
#' La funcion detecta automaticamente el tipo de modelo:
#' \itemize{
#'   \item **Modelos lineales** (`lm`): devuelve la prediccion puntual con
#'         intervalos de prediccion o confianza, como hasta ahora.
#'   \item **Modelos logit binomiales** (`glm` con `family = binomial`):
#'         devuelve la probabilidad estimada con un intervalo de confianza
#'         calculado en la escala del enlace (logit) y transformado a
#'         probabilidades. Incluye ademas una columna de clasificacion segun
#'         el umbral especificado.
#' }
#'
#' Utilidad principal: en las practicas de los capitulos 9 (regresion lineal
#' multiple) y 10 (modelos de eleccion discreta) permite al alumno definir en
#' Excel varios escenarios de prediccion sin necesidad de codificarlos en R,
#' incluyendo formulas que referencian variables de los data frames del script.
#'
#' Las formulas en las celdas de Excel se evaluan en un entorno donde:
#' \itemize{
#'   \item cada variable de `train_df` esta disponible por su nombre (por
#'         ejemplo, `mean(ACTIVO)`);
#'   \item el propio `train_df` esta disponible bajo el alias `train` (salvo
#'         que `eval_dfs` lo sobrescriba);
#'   \item cualquier data frame adicional pasado por `eval_dfs` esta
#'         disponible bajo el nombre que se le asigne en la lista.
#' }
#'
#' @param model Objeto de modelo ajustado, devuelto por [stats::lm()] o por
#'   [stats::glm()] con `family = binomial(link = "logit")`.
#' @param train_df Data frame de entrenamiento del modelo. Se utiliza para
#'   determinar los niveles de las variables factor y para resolver las
#'   referencias a variables sin prefijo dentro de las formulas de Excel.
#'   Ademas, queda disponible bajo el alias `train` en dichas formulas.
#' @param excel_path Ruta al archivo Excel con los escenarios.
#' @param sheet Nombre o indice de la hoja del Excel. Por defecto,
#'   `"Escenarios"`.
#' @param id_col Nombre de la columna del Excel que identifica cada escenario.
#'   Por defecto, `"ESCENARIO"`. Si la columna no existe en la hoja, se crea
#'   con un identificador correlativo.
#' @param eval_dfs Lista con nombre de data frames adicionales que se quieren
#'   exponer, bajo esos nombres, a las formulas escritas en las celdas de
#'   Excel. Por ejemplo, `list(df1 = seleccion)` permite escribir en Excel
#'   `df1$IVENTAS`. Por defecto, `list()` (solo `train_df` bajo el alias
#'   `train`).
#' @param interval Tipo de intervalo devuelto por [stats::predict.lm()]:
#'   `"prediction"` (por defecto) o `"confidence"`. Para modelos logit se
#'   ignora (siempre se calcula un intervalo de confianza sobre la
#'   probabilidad).
#' @param level Nivel de confianza del intervalo. Por defecto `0.95`.
#' @param transforma_log Si `TRUE`, aplica `exp()` a las predicciones y a los
#'   extremos del intervalo. Util cuando el modelo estima `log(y)` pero se
#'   desea presentar los resultados en la escala original de `y`. La
#'   desviacion tipica se transforma mediante el metodo delta aproximado. Por
#'   defecto, `FALSE`. Se ignora para modelos logit.
#' @param umbral Probabilidad umbral para la clasificacion en modelos logit.
#'   Por defecto `0.5`. Se ignora para modelos lineales.
#' @param return_kable Si `TRUE` (por defecto), devuelve ademas una tabla
#'   formateada con [kable_rstars()] en el elemento `$table` de la lista de
#'   salida. Si `FALSE`, ese elemento vale `NULL`.
#' @param caption Titulo base de la tabla formateada. La funcion lo completa
#'   automaticamente anadiendo el nombre de la variable dependiente, el tipo
#'   de intervalo, el nivel de confianza y, si procede, una indicacion de que
#'   los valores estan en la escala original tras antilog. Por defecto,
#'   `"Predicciones por escenario"`.
#' @param ... Argumentos adicionales pasados a [kable_rstars()] (por ejemplo,
#'   `digits`, `font_size` o `bootstrap_options`). Si no se pasa `digits`, se
#'   aplica un valor por defecto de `3`.
#'
#' @return Una lista con dos elementos:
#' \itemize{
#'   \item `data`: data frame con una fila por escenario. Para modelos
#'         lineales incluye `.fitted`, `.lwr`, `.upr`, `.se_fit` y metadatos.
#'         Para modelos logit incluye `.prob` (probabilidad estimada),
#'         `.prob_lwr` y `.prob_upr` (intervalo de confianza sobre la
#'         probabilidad), `.se_link` (error estandar en la escala logit) y
#'         `.clasif` (clasificacion segun el umbral).
#'   \item `table`: objeto `knitr_kable` con el data frame anterior
#'         formateado con [kable_rstars()] (o `NULL` si `return_kable = FALSE`).
#' }
#'
#' @examples
#' \dontrun{
#' # --- Modelo lineal (capitulo 9) ---
#' modelo_lm <- lm(log(mpg) ~ wt + hp, data = mtcars)
#' res <- predict_from_excel_scenarios(
#'   model          = modelo_lm,
#'   train_df       = mtcars,
#'   excel_path     = "escenarios.xlsx",
#'   sheet          = "Simula1",
#'   transforma_log = TRUE
#' )
#' res$table
#'
#' # --- Modelo logit (capitulo 10) ---
#' modelo_glm <- glm(am ~ wt + hp, data = mtcars,
#'                   family = binomial(link = "logit"))
#' res2 <- predict_from_excel_scenarios(
#'   model    = modelo_glm,
#'   train_df = mtcars,
#'   excel_path = "escenarios_logit.xlsx",
#'   umbral   = 0.5
#' )
#' res2$table
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom stats family
#' @export
predict_from_excel_scenarios <- function(model,
                                         train_df,
                                         excel_path,
                                         sheet = "Escenarios",
                                         id_col = "ESCENARIO",
                                         eval_dfs = list(),
                                         interval = c("prediction", "confidence"),
                                         level = 0.95,
                                         transforma_log = FALSE,
                                         umbral = 0.5,
                                         return_kable = TRUE,
                                         caption = "Predicciones por escenario",
                                         ...) {

  interval <- match.arg(interval)

  # --- Detectar tipo de modelo ------------------------------------------------
  es_logit <- inherits(model, "glm") &&
    family(model)$family == "binomial" &&
    family(model)$link   == "logit"

  form   <- stats::formula(model)
  y_name <- all.vars(form)[1]
  vars_x <- setdiff(all.vars(form), y_name)

  # --- Lectura del archivo Excel ---------------------------------------------
  escenarios_raw <- readxl::read_excel(excel_path, sheet = sheet)
  escenarios_raw <- as.data.frame(escenarios_raw, check.names = FALSE)
  if (!(id_col %in% names(escenarios_raw))) {
    escenarios_raw[[id_col]] <- seq_len(nrow(escenarios_raw))
  }

  # --- Auxiliares internas ---------------------------------------------------
  parece_num <- function(x_chr) grepl("^\\s*-?\\d+(?:[\\.,]\\d+)?\\s*$", x_chr)

  evalua_celda <- function(x, env_eval) {
    if (is.numeric(x)) return(x)
    if (is.na(x)) return(NA_real_)
    xc <- as.character(x)

    if (parece_num(xc)) return(as.numeric(gsub(",", ".", xc)))

    es_formula <- grepl("[\\(\\)\\+\\-\\*/\\^:\\$]", xc)
    if (!es_formula) {
      xc <- gsub("^\\s*['\"]|['\"]\\s*$", "", xc)
      return(xc)
    }

    expr_txt <- gsub("(\\d),(\\d)", "\\1.\\2", xc) # 2,5 -> 2.5
    val <- try(eval(parse(text = expr_txt), envir = env_eval), silent = TRUE)
    if (inherits(val, "try-error")) {
      stop(paste0("No se pudo evaluar la f\u00f3rmula: ", xc,
                  "\nTransformada como: ", expr_txt,
                  "\nError: ", as.character(val)))
    }
    if (length(val) > 1) val <- mean(val, na.rm = TRUE)
    as.numeric(val)
  }

  # --- Entorno de evaluacion para las formulas de Excel ----------------------
  env_eval <- new.env(parent = parent.frame())

  if (length(eval_dfs)) {
    for (nm in names(eval_dfs)) {
      if (!is.null(eval_dfs[[nm]])) assign(nm, eval_dfs[[nm]], envir = env_eval)
    }
  }
  if (is.null(eval_dfs$train)) assign("train", train_df, envir = env_eval)

  for (vn in names(train_df)) assign(vn, train_df[[vn]], envir = env_eval)

  factor_levels <- lapply(intersect(vars_x, names(train_df)), function(vn) {
    if (is.factor(train_df[[vn]])) levels(train_df[[vn]]) else NULL
  })
  names(factor_levels) <- intersect(vars_x, names(train_df))

  # --- Construccion de las filas newdata a partir de las celdas --------------
  split_list <- split(escenarios_raw, escenarios_raw[[id_col]], drop = TRUE)

  newdatas_list <- lapply(names(split_list), function(id) {
    fila <- split_list[[id]][1, , drop = FALSE]
    nd_vals <- vector("list", length(vars_x))
    names(nd_vals) <- vars_x
    for (vn in vars_x) {
      if (!(vn %in% names(fila))) {
        stop(paste0("Falta la columna '", vn, "' en la hoja '", sheet, "'."))
      }
      celda <- fila[[vn]][[1]]

      if (vn %in% names(train_df) && is.factor(train_df[[vn]])) {
        v <- evalua_celda(celda, env_eval)
        nd_vals[[vn]] <- factor(v, levels = factor_levels[[vn]])
      } else {
        nd_vals[[vn]] <- evalua_celda(celda, env_eval)
      }
    }
    nd <- as.data.frame(nd_vals, check.names = FALSE)
    nd[[id_col]] <- id
    nd
  })

  newdatas <- do.call(rbind, newdatas_list)
  rownames(newdatas) <- NULL

  # --- Prediccion ------------------------------------------------------------

  nd_x <- newdatas[, vars_x, drop = FALSE]

  if (es_logit) {

    # ── Rama GLM logit ──────────────────────────────────────────────────────
    # Predecir en la escala del enlace (logit) con error estandar
    preds <- stats::predict(model, newdata = nd_x,
                            type = "link", se.fit = TRUE)

    eta    <- as.numeric(preds$fit)
    se_eta <- as.numeric(preds$se.fit)

    # Intervalo de confianza en la escala del logit
    z_crit  <- stats::qnorm(1 - (1 - level) / 2)
    eta_lwr <- eta - z_crit * se_eta
    eta_upr <- eta + z_crit * se_eta

    # Transformacion a probabilidades (inversa logistica)
    inv_logit <- function(x) 1 / (1 + exp(-x))

    prob_est <- inv_logit(eta)
    prob_lwr <- inv_logit(eta_lwr)
    prob_upr <- inv_logit(eta_upr)

    # Clasificacion segun umbral
    clasif <- ifelse(prob_est >= umbral, 1, 0)

    if (transforma_log) {
      message("Nota: 'transforma_log' se ignora para modelos logit ",
              "(la transformaci\u00f3n a probabilidades es inherente al modelo).")
    }

    out_df <- cbind(
      newdatas[, c(id_col, vars_x), drop = FALSE],
      .prob     = prob_est,
      .prob_lwr = prob_lwr,
      .prob_upr = prob_upr,
      .se_link  = se_eta,
      .clasif   = clasif,
      .level    = level,
      .umbral   = umbral
    )

  } else {

    # ── Rama LM (comportamiento original) ───────────────────────────────────
    preds <- stats::predict(model, newdata = nd_x,
                            interval = interval, level = level,
                            se.fit = TRUE)

    fit_mat <- as.data.frame(preds$fit)
    names(fit_mat) <- c("fit", "lwr", "upr")
    se_vec <- as.numeric(preds$se.fit)

    if (transforma_log) {
      fitted_vals <- exp(fit_mat$fit)
      lwr_vals    <- exp(fit_mat$lwr)
      upr_vals    <- exp(fit_mat$upr)
      se_vals     <- exp(fit_mat$fit) * se_vec
    } else {
      fitted_vals <- as.numeric(fit_mat$fit)
      lwr_vals    <- as.numeric(fit_mat$lwr)
      upr_vals    <- as.numeric(fit_mat$upr)
      se_vals     <- se_vec
    }

    out_df <- cbind(
      newdatas[, c(id_col, vars_x), drop = FALSE],
      .fitted        = fitted_vals,
      .lwr           = lwr_vals,
      .upr           = upr_vals,
      .se_fit        = se_vals,
      .interval      = interval,
      .level         = level,
      .log_transform = transforma_log
    )
  }

  # --- Salida formateada opcional --------------------------------------------
  if (return_kable) {
    args <- list(...)
    if (!"digits" %in% names(args)) args$digits <- 3

    if (es_logit) {
      caption_final <- paste0(
        caption,
        " \u2014 P(", y_name, " = 1)",
        " (IC ", level * 100, "%",
        ", umbral = ", umbral, ")"
      )
    } else {
      caption_final <- paste0(
        caption,
        " \u2014 Variable dependiente: ", y_name,
        " (", interval, " ", level * 100, "%)",
        if (transforma_log) " \u2014 valores antilog."
      )
    }

    tab <- do.call(
      kable_rstars,
      c(list(out_df, caption = caption_final), args)
    )
  } else {
    tab <- NULL
  }

  list(data = out_df, table = tab)
}
