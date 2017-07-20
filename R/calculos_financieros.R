#' Calcular cuota de un crédito
#'
#' Calcula el monto de la cuota de un crédito personal, tomando en cuenta el monto, plazo y tasa.
#'
#' @param monto_credito Vector numérico con el monto total del crédito
#' @param tasa_anual Vector numérico con la tasa de interés anual (puntos porcentuales)
#' @param plazo_meses Vector numérico con el plazo del crédito en meses
#' @param per_anuales Número de periodos por año, de acuerdo con convención de días usada (opcional)
#' @return Vector numérico con las cuotas de los créditos.
#' @export
#'
#' @examples
#' calcular_cuota(1000000, 17, 24)
#' calcular_cuota(c(50000, 174000), c(17.5, 18), c(24, 36))
calcular_cuota <-
    function(monto_credito, tasa_anual, plazo_meses, per_anuales = 12) {
    tasa_mensual <- (tasa_anual / 100) / per_anuales
    factor <- (1 - (1 + tasa_mensual) ^ -plazo_meses)
    cuota <- monto_credito * (tasa_mensual / factor)
    return(cuota)
}

#' calcular principal restante tras N periodos
#'
#' Calcula el principal restante en una operación de crédito tras N periodos.
#' No toma en cuenta cancelaciones o pagos anticipados.
#'
#' @inheritParams calcular_cuota
#' @param periodos_transcurridos Número de periodos transcurridos al calcular el principal o intereses
#' @param plazo_meses Vector numérico con el plazo del crédito en meses (opcional)
#' @param cuota Vector numérico con el valor de la cuota mensual (opcional)
#'
#' @details Uno de \code{plazo_meses} o \code{cuota} debe ser pasado como argumento.
#' @return Vector númerico con el principal restante tras \code{periodos_transcurridos} períodos.
#' @export
#'
#' @examples
#' calcular_principal_restante(1e6, 17, 5, plazo_meses = 24)
#' calcular_principal_restante(c(2e6, 1e6), c(18, 12), c(10, 5), cuota = c(120000, 175000))
calcular_principal_restante <- function(monto_credito, tasa_anual,
    periodos_transcurridos, plazo_meses = NULL, cuota = NULL, per_anuales = 12) {
    if (is.null(cuota) & is.null(plazo_meses)) {
        stop(
            paste0(
                "Uno de la cuota o el plazo en meses de la operación",
                " debe ser indicado como argumento de la función."
            )
        )
    }
    if (is.null(cuota)) {
        cuota <- calcular_cuota(monto_credito, tasa_anual, plazo_meses)
    }

    tasa_mensual <- (tasa_anual / 100) / per_anuales
    principal <- monto_credito * (1 + tasa_mensual) ^ periodos_transcurridos -
        cuota * ((1 + tasa_mensual) ^ periodos_transcurridos - 1) / tasa_mensual

    principal <- ifelse(periodos_transcurridos == 0, monto_credito, principal)
    return(principal)
}

#' calcular ingresos por intereses
#'
#' Calcula el monto de los ingresos por cuotas que son atribuíbles a intereses.
#' No toma en cuenta cancelaciones o pagos anticipados.
#'
#' @inheritParams calcular_cuota
#' @inheritParams calcular_principal_restante
#'
#' @return Vector numérico con los ingresos por intereses tras un período
#' @export
#'
#' @examples
#' calcular_ingresos_intereses(1e6, 17, 5, plazo_meses = 24)
#' calcular_ingresos_intereses(c(2e6, 1e6), c(18, 12), c(10, 5), cuota = c(125000, 175000))
calcular_ingresos_intereses <- function(monto_credito, tasa_anual,
    periodos_transcurridos, plazo_meses = NULL, cuota = NULL, per_anuales = 12) {
    if (is.null(cuota) & is.null(plazo_meses)) {
        stop(
            paste0(
                "Uno de la cuota o el plazo en meses de la operación",
                " debe ser indicado como argumento de la función."
            )
        )
    }

    if (is.null(cuota)) {
        cuota <- calcular_cuota(monto_credito, tasa_anual, plazo_meses)
    }

    delta_principal <- monto_credito -
        calcular_principal_restante(monto_credito, tasa_anual,
            periodos_transcurridos, plazo_meses, cuota)

    ingresos_cuotas <- cuota * periodos_transcurridos - delta_principal

    return(ingresos_cuotas)
}
