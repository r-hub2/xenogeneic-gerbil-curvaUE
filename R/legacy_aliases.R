#' Legacy Spanish function names
#'
#' These wrappers preserve earlier entry points and Spanish output names.
#' New analyses should use the English API.
#' @param ... Arguments passed on to the corresponding English-named function;
#'   legacy Spanish argument names and values are translated automatically.
#' @return The value returned by the corresponding English-named function,
#'   with legacy Spanish field names restored on data frames and lists.
#' @name legacy_aliases
NULL
.legacy_arg_names <- c(
  blanco="trial", ancho_ub="unit_width_m", largo_ub="unit_length_m",
  min_parcelas="min_plots", criterio="criterion", metodo_smith="smith_method",
  metodo="method", config="configurations", superficie="surface",
  archivo="path", separador="sep", decimal="dec", cultivo="crop",
  datos="trials", intervalo="cv_interval", n_grid="grid_size"
)
.legacy_values <- c(arroz="rice", maiz="maize", frijol_nambi="common_bean", vigna="cowpea",
  muestral="sample", poblacional="population", ambos="both", gaussiana="gaussian",
  seccional="sectional", log_convencional="unweighted_log")
.legacy_output_names <- c(
  rows="filas", columns="columnas", row="fila", column="columna",
  yield="rendimiento", n_ub="n_UB", minimum="minimo", maximum="maximo",
  mean="media", std_dev="desv", cv_ub="cv_UB", skewness="asimetria",
  excess_kurtosis="curtosis", width="ancho", length="largo",
  width_m="ancho_m", length_m="largo_m", n_plots="n_parcelas",
  mean_total="media_total", total_variance="var_totales",
  unit_variance="vx_convencional", smith_normalized_variance="vx_smith",
  smith_normalized_cv="cv_smith", criterion="criterio",
  model="modelo", residuals="residuos", rmse_descriptive="rmse_descriptivo",
  b_prime="b_prima", b_corrected="b_corregido", V1_fitted="V1_ajustado",
  method="metodo", n_configurations="n_configuraciones", weights="pesos",
  fit_data="tabla_ajuste", warning="advertencia", descriptives="descriptivos",
  dimensions="dimensiones", long_data="datos_largos", configurations="configuraciones",
  surface="superficie", solution="solucion", basic_unit_dimensions_m="dimensiones_ub_m"
)
.legacy_to_english <- function(x) {
  if (inherits(x, "ggplot") || inherits(x, "lm") || inherits(x, "nls")) return(x)
  if (is.data.frame(x)) {
    nm <- names(x)
    inv <- stats::setNames(names(.legacy_output_names), .legacy_output_names)
    # One-basic-unit configuration dimensions have _n suffix in the legacy API.
    nm[nm == "ancho_n"] <- "width_ub"
    nm[nm == "largo_n"] <- "length_ub"
    nm[nm == "ancho_ub"] <- "width_ub"
    nm[nm == "largo_ub"] <- "length_ub"
    nm <- ifelse(nm %in% names(inv), inv[nm], nm)
    names(x) <- nm
    if ("criterio" %in% names(x)) {
      x$criterio <- unname(ifelse(x$criterio %in% names(.legacy_values),
                                  .legacy_values[x$criterio], x$criterio))
    }
    return(x)
  }
  if (is.list(x) && !is.object(x)) return(lapply(x, .legacy_to_english))
  x
}
.legacy_to_spanish <- function(x) {
  if (inherits(x, "ggplot") || inherits(x, "lm") || inherits(x, "nls")) return(x)
  if (is.data.frame(x)) {
    sol <- "criterion" %in% names(x)
    nm <- names(x)
    nm[nm == "width_ub"] <- if (sol) "ancho_ub" else "ancho_n"
    nm[nm == "length_ub"] <- if (sol) "largo_ub" else "largo_n"
    nm <- ifelse(nm %in% names(.legacy_output_names), .legacy_output_names[nm], nm)
    names(x) <- nm
    if ("criterio" %in% names(x)) {
      rev <- stats::setNames(names(.legacy_values), .legacy_values)
      x$criterio <- unname(ifelse(x$criterio %in% names(rev), rev[x$criterio], x$criterio))
    }
    return(x)
  }
  if (is.list(x) && !is.object(x)) {
    nm <- names(x)
    y <- lapply(x, .legacy_to_spanish)
    if (!is.null(nm)) names(y) <- ifelse(nm %in% names(.legacy_output_names), .legacy_output_names[nm], nm)
    return(y)
  }
  x
}
.legacy_dispatch <- function(target, ...) {
  a <- list(...)
  nm <- names(a)
  if (!is.null(nm)) {
    new_nm <- ifelse(nm %in% names(.legacy_arg_names), .legacy_arg_names[nm], nm)
    names(a) <- new_nm
  }
  a <- lapply(a, function(v) {
    if (is.character(v) && length(v) == 1L && v %in% names(.legacy_values))
      return(unname(.legacy_values[[v]]))
    if (is.data.frame(v)) return(.legacy_to_english(v))
    v
  })
  .legacy_to_spanish(do.call(target, a))
}
#' @rdname legacy_aliases
#' @export
leer_matriz_ue <- function(...) .legacy_dispatch(read_trial_matrix, ...)
#' @rdname legacy_aliases
#' @export
validar_matriz_ue <- function(...) .legacy_dispatch(validate_trial_matrix, ...)
#' @rdname legacy_aliases
#' @export
datos_largos_ue <- function(...) .legacy_dispatch(as_long_trial_data, ...)
#' @rdname legacy_aliases
#' @export
datos_ejemplo_ue <- function(...) .legacy_dispatch(example_trial, ...)
#' @rdname legacy_aliases
#' @export
describir_ensayo <- function(...) .legacy_dispatch(describe_trial, ...)
#' @rdname legacy_aliases
#' @export
configuraciones <- function(...) .legacy_dispatch(plot_configurations, ...)
#' @rdname legacy_aliases
#' @export
indice_smith <- function(...) .legacy_dispatch(fit_smith, ...)
#' @rdname legacy_aliases
#' @export
ajustar_superficie <- function(...) .legacy_dispatch(fit_cv_surface, ...)
#' @rdname legacy_aliases
#' @export
curvatura_K <- function(...) .legacy_dispatch(gaussian_curvature_at, ...)
#' @rdname legacy_aliases
#' @export
curvatura_gaussiana <- function(...) .legacy_dispatch(max_gaussian_curvature, ...)
#' @rdname legacy_aliases
#' @export
curvatura_seccional <- function(...) .legacy_dispatch(simultaneous_sectional_curvature, ...)
#' @rdname legacy_aliases
#' @export
unidad_experimental_2d <- function(...) .legacy_dispatch(analyze_uniformity_trial, ...)
#' @rdname legacy_aliases
#' @export
heatmap_ensayo <- function(...) .legacy_dispatch(plot_trial_heatmap, ...)
#' @rdname legacy_aliases
#' @export
perfil_cv <- function(...) .legacy_dispatch(plot_cv_profiles, ...)
#' @rdname legacy_aliases
#' @export
grafico_contornos <- function(...) .legacy_dispatch(plot_cv_contours, ...)
#' @rdname legacy_aliases
#' @export
grafico_superficie <- function(...) .legacy_dispatch(plot_cv_surface, ...)
#' @rdname legacy_aliases
#' @export
grafico_secciones <- function(...) .legacy_dispatch(plot_curvature_sections, ...)
#' @rdname legacy_aliases
#' @export
figura_curvatura_cultivos <- function(...) .legacy_dispatch(plot_multicrop_curvature, ...)
