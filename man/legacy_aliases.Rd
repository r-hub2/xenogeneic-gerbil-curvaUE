\name{legacy_aliases}
\alias{ajustar_superficie}
\alias{configuraciones}
\alias{curvatura_K}
\alias{curvatura_gaussiana}
\alias{curvatura_seccional}
\alias{datos_ejemplo_ue}
\alias{datos_largos_ue}
\alias{describir_ensayo}
\alias{figura_curvatura_cultivos}
\alias{grafico_contornos}
\alias{grafico_secciones}
\alias{grafico_superficie}
\alias{heatmap_ensayo}
\alias{indice_smith}
\alias{leer_matriz_ue}
\alias{perfil_cv}
\alias{unidad_experimental_2d}
\alias{validar_matriz_ue}
\title{Legacy Spanish function names}
\description{Previous function names remain as forwarding aliases. New analyses should use the English functions. The returned schema uses English names.}
\usage{
leer_matriz_ue(...)
validar_matriz_ue(...)
datos_largos_ue(...)
datos_ejemplo_ue(...)
describir_ensayo(...)
configuraciones(...)
indice_smith(...)
ajustar_superficie(...)
curvatura_K(...)
curvatura_gaussiana(...)
curvatura_seccional(...)
unidad_experimental_2d(...)
heatmap_ensayo(...)
perfil_cv(...)
grafico_contornos(...)
grafico_superficie(...)
grafico_secciones(...)
figura_curvatura_cultivos(...)
}
\arguments{
\item{...}{Arguments passed on to the corresponding English-named function;
legacy Spanish argument names and values are translated automatically.}
}
\value{
The value returned by the corresponding English-named function, with legacy
Spanish field names restored on data frames and lists.
}
