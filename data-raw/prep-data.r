
## Importar librerias
library(data.table)

## Funciones
calcular_indices_mctq <- function(SOw, SEw, SOf, SEf) {

  # Convertir las horas a formato de 24 horas
  SOw <- as.POSIXct(SOw, format="%H:%M")
  SEw <- as.POSIXct(SEw, format="%H:%M")
  SOf <- as.POSIXct(SOf, format="%H:%M")
  SEf <- as.POSIXct(SEf, format="%H:%M")

  # Ajustar las horas si el tiempo de finalización es menor que el tiempo de inicio
  SEw[SOw > SEw & complete.cases(SOw, SEw)] <- SEw[SOw > SEw & complete.cases(SOw, SEw)] + 86400
  SEf[SOf > SEf & complete.cases(SOf, SEf)] <- SEf[SOf > SEf & complete.cases(SOf, SEf)] + 86400

  # Calcular la duración del sueño en días laborales y libres
  SDw <- as.numeric(difftime(SEw, SOw, units="hours"))
  SDf <- as.numeric(difftime(SEf, SOf, units="hours"))

  # Calcular el punto medio del sueño en días laborales (MSW)
  MSW <- SOw + (SEw - SOw) / 2
  MSWn <- as.numeric(format(MSW, format = "%H")) + as.numeric(format(MSW, format = "%M")) / 60
  MSW <- format(MSW, "%H:%M")

  # Calcular el punto medio del sueño en días libres (MSF)
  MSF <- SOf + (SEf - SOf) / 2
  MSFn <- as.numeric(format(MSF, format = "%H")) + as.numeric(format(MSF, format = "%M")) / 60
  MSF <- format(MSF, "%H:%M")

  # Calcular el punto medio del sueño en días libres corregido (MSFsc)
  MSFsc <- ifelse(SDf <= SDw, MSF, format(as.POSIXct(MSF, format="%H:%M") - (SDf - SDw) / 2, "%H:%M"))

  # Calcular el jetlag social
  jetlag_social <- as.numeric(difftime(as.POSIXct(MSF, format="%H:%M"), as.POSIXct(MSW, format="%H:%M"), units="hours"))

  # Crear un data frame con los resultados
  resultados <- data.frame(
    sdwd = SDw,
    sdfd = SDf,
    sddiff = SDf - SDw,
    msfc = MSFn,
    mswc = MSWn,
    sjlc = jetlag_social
  )

  return(resultados)
}

spaq_to_num <- function(x) {
  return(as.numeric(x) - 1L)
}

# Datos de Punta Arenas ---------------------------------------------------

## Importar datos
data <- read.csv("data-raw/raw_puq.csv")
data <- data[, grep("___|_q|_complete$", names(data), ignore.case = TRUE, value = TRUE, invert = TRUE)]

## Crear variables
mctq_data_puq <- with(data, {
  calcular_indices_mctq(SOw = hora_de_dormir_1,
                        SEw = hora_de_despertar_1,
                        SOf = hora_de_dormir_2,
                        SEf = hora_de_despertar_2)
})

data$gss <- with(data, {
  spaq_to_num(horas_de_sueno) +
  spaq_to_num(meterse_en_problemas) +
  spaq_to_num(actividad_social) +
  spaq_to_num(abuso_de_sustancias) +
  spaq_to_num(estado_de_animo) +
  spaq_to_num(rendimiento_escolar) +
  spaq_to_num(notas) +
  spaq_to_num(peso_corporal) +
  spaq_to_num(irritabilidad) +
  spaq_to_num(nivel_de_energia) +
  spaq_to_num(apetito)
})

## Seleccionar variables
data_puq <- data.frame(id = data$record_id,
                       Pitcat = as.numeric(data$clasificacion_global > 5) + 1,
                       HO = data$puntaje_total_matutinindadvespertinidad,
                       sdwd = mctq_data_puq$sdwd,
                       sdfd = mctq_data_puq$sdfd,
                       sddiff = mctq_data_puq$sddiff,
                       msfc = mctq_data_puq$msfc,
                       mswc = mctq_data_puq$mswc,
                       sjlc = mctq_data_puq$sjlc,
                       GSS = data$gss,
                       spaqprob = data$es_un_problema,
                       sadcrit = as.numeric(data$gss > 18 & spaq_to_num(data$gravedad_problema) > 0))


# Datos de Santiago -------------------------------------------------------

## Importar datos
data_scl <- readxl::read_excel(path = "data-raw/raw_scl.xlsx")

## Nombrar variables
names(data_scl) <- data_scl[23,] |> as.character()

## Remover filas sin datos
data_scl <- data_scl[-c(1:23), ]

## Seleccionar variables
data_scl <- data_scl[, c(2:13)]

## Transformar variables a numérico
data_scl |> str()
data_scl <- within(data_scl, {
  Pitcat <- as.numeric(Pitcat)
  HO <- as.numeric(HO)
  sdwd <- as.numeric(sdwd)
  sdfd <- as.numeric(sdfd)
  sddiff <- as.numeric(sddiff)
  msfc <- as.numeric(msfc)
  mswc <- as.numeric(mswc)
  sjlc <- as.numeric(sjlc)
  GSS <- as.numeric(GSS)
  spaqprob <- as.numeric(spaqprob)
  sadcrit <- as.numeric(sadcrit)
})

## Juntamos las filas
data_full <- rbind(
  cbind(comuna = "Punta Arenas", data_puq),
  cbind(comuna = "Santiago", data_scl)
)

names(data_full) <- c("comuna", "id", "psqi_cat", "ho_score",
                      "munich_horas_semana", "munich_horas_finde",
                      "munich_horas_diff", "munich_msfc",
                      "munich_mswc", "munich_sjlc", "spaq_gss",
                      "spaq_problema_cat", "spaq_sad_cat")

data_full$psqi_cat <- factor(data_full$psqi_cat, levels = 1:2, labels = c("Buen dormir", "Mal dormir"))
data_full$spaq_problema_cat <- factor(data_full$spaq_problema_cat, levels = 0:1, labels = c("No es un problema", "Es un problema"))
data_full$spaq_sad_cat <- factor(data.table::fifelse(data_full$spaq_sad_cat == 1, 1, 0, na = 0), levels = 0:1, labels = c("Típico","SAD"))

data_full$id <- seq_len(nrow(data_full))

sleep <- as.data.table(data_full)
rm(data_full)

save(sleep, file = "data/sleep.RData")

data.table::fwrite(x = sleep, file = "data-raw/data_full.csv")
data.table::fwrite(x = sleep[comuna == "Punta Arenas", -c("comuna")], file = "data-raw/data_puq.csv")
data.table::fwrite(x = sleep[comuna == "Santiago", -c("comuna")], file = "data-raw/data_scl.csv")
