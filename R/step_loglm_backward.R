#' Seleccion backward por AIC para modelos log-lineales
#'
#' Implementa un procedimiento de seleccion de modelos log-lineales por
#' eliminacion progresiva de terminos (*backward*) usando el criterio de
#' informacion de Akaike (AIC). Es una alternativa a [stats::step()]
#' pensada especificamente para modelos ajustados con [MASS::loglm()].
#'
#' La motivacion para no usar [stats::step()] es puramente tecnica:
#' `step()` localiza la tabla de datos de cada submodelo mediante
#' `eval.parent()`, que sube por la pila de llamadas hasta encontrarla.
#' Este mecanismo funciona en una sesion interactiva, pero es fragil
#' cuando el codigo se ejecuta a traves de varias capas de funciones
#' envolventes (por ejemplo, al compilar un libro con
#' `bookdown::render_book()` desde una funcion como `build_book()`),
#' porque el numero de saltos en la pila cambia y `step()` deja de
#' localizar la tabla, produciendo un error dificil de diagnosticar.
#'
#' Esta funcion reimplementa el algoritmo *backward* por AIC sin
#' depender de `eval.parent()`: la tabla de datos se pasa como argumento
#' explicito y queda capturada en el *closure* de una funcion auxiliar
#' interna, resolviendose por alcance lexico (no dinamico). El resultado
#' es equivalente al de `step()` en sesion interactiva, pero es robusto
#' en cualquier contexto de ejecucion (consola, `knitr`, `bookdown`,
#' `pkgdown`, funciones envolventes, etc.).
#'
#' Internamente, en cada iteracion:
#' \enumerate{
#'   \item Se identifican los terminos candidatos a eliminar mediante
#'         [stats::drop.scope()] aplicado a los terminos de la formula
#'         actual (esto respeta la jerarquia: no se elimina un efecto
#'         principal si esta implicado en una interaccion todavia
#'         presente en el modelo).
#'   \item Se ajusta un submodelo por cada termino candidato y se
#'         calcula su AIC mediante [stats::extractAIC()].
#'   \item Se selecciona el termino cuya eliminacion produce el menor
#'         AIC. Si el AIC minimo no mejora el actual, el algoritmo
#'         termina y devuelve el modelo actual.
#' }
#'
#' @param formula Formula del modelo inicial (habitualmente el modelo
#'   saturado, como `~ A * B * C`).
#' @param data Tabla de contingencia sobre la que se ajusta el modelo
#'   (objeto `table`, `array` o `matrix` de cualquier numero de
#'   dimensiones, compatible con [MASS::loglm()]).
#' @param trace Si `TRUE` (por defecto), imprime en la consola cada
#'   paso del algoritmo con el AIC actual, los terminos candidatos y
#'   sus respectivos AIC.
#' @param steps Numero maximo de iteraciones. Por defecto, `1000`. En
#'   la practica, la seleccion termina mucho antes al no encontrar mas
#'   terminos que mejoren el AIC.
#'
#' @return Objeto de clase `loglm` correspondiente al modelo final
#'   seleccionado, ajustado con `fitted = TRUE` (por lo que incluye las
#'   frecuencias ajustadas necesarias para diagnostico posterior).
#'
#' @examples
#' \dontrun{
#' # Partiendo del modelo saturado sobre una tabla de contingencia:
#' tab0 <- xtabs(~ GALAXIA + FJUR + EFLO, data = seleccion)
#'
#' modelo_def <- step_loglm_backward(~ GALAXIA * EFLO * FJUR,
#'                                   data  = tab0,
#'                                   trace = TRUE,
#'                                   steps = 1000)
#'
#' # El resultado es un objeto loglm equivalente al de step()
#' # pero robusto en contextos de compilacion:
#' generar_solucion(modelo_def)
#' }
#'
#' @importFrom MASS loglm
#' @importFrom stats drop.scope terms extractAIC update.formula
#' @export
step_loglm_backward <- function(formula, data, trace = TRUE, steps = 1000) {

  ajustar <- function(form) MASS::loglm(form, data = data, fitted = FALSE)
  aic_de  <- function(fit) stats::extractAIC(fit)[2]

  formula_actual <- formula
  ajuste_actual  <- ajustar(formula_actual)
  aic_actual     <- aic_de(ajuste_actual)

  if (trace) {
    cat("Start:  AIC=", round(aic_actual, 2), "\n",
        deparse(formula_actual), "\n\n", sep = "")
  }

  for (i in seq_len(steps)) {

    terminos <- stats::drop.scope(stats::terms(formula_actual))
    if (length(terminos) == 0) break

    aics <- vapply(terminos, function(term) {
      nueva_formula <- stats::update.formula(formula_actual,
                                             paste("~ . -", term))
      ajuste <- tryCatch(ajustar(nueva_formula), error = function(e) NULL)
      if (is.null(ajuste)) Inf else aic_de(ajuste)
    }, numeric(1))

    if (trace) {
      cat("Step", i, ":\n")
      print(data.frame(Term = c("<none>", terminos),
                       AIC  = round(c(aic_actual, aics), 2)),
            row.names = FALSE)
      cat("\n")
    }

    mejor <- which.min(aics)
    if (aics[mejor] >= aic_actual) break

    formula_actual <- stats::update.formula(formula_actual,
                                            paste("~ . -", terminos[mejor]))
    aic_actual <- aics[mejor]
  }

  ajuste_final <- MASS::loglm(formula_actual, data = data, fitted = TRUE)
  if (trace) {
    cat("\nFormula final:\n", deparse(formula_actual), "\n",
        "AIC final = ", round(aic_de(ajuste_final), 2), "\n", sep = "")
  }
  ajuste_final
}
