

## Funciones
sleep_time <- function(dormir, despertar) {
  x_a <- x <- lubridate::hm(dormir, quiet = TRUE)
  y <- lubridate::hm(despertar, quiet = TRUE)
  ind <- x > (12 * 60 * 60) & !is.na(x)
  x_a[ind] <- x[ind] - lubridate::hm("24:00")
  h_sueno <- round(as.numeric(y - x_a)/3600, 1)
  h_sueno[h_sueno < 0] <- NA
  return(h_sueno)
}

spaq_to_num <- function(x) {
  return(as.numeric(x) - 1L)
}

# Datos de Punta Arenas ---------------------------------------------------

## Importar datos
data <- read.csv("data-raw/raw_puq.csv")
data <- data[, grep("___|_q|_complete$", names(data), ignore.case = TRUE, value = TRUE, invert = TRUE)]

## Crear variables
data$sueno_semana_academica <- sleep_time(data$hora_de_dormir_1, data$hora_de_despertar_1)
data$sueno_semana_finde <- sleep_time(data$hora_de_dormir_2, data$hora_de_despertar_2)
data$sueno_diff <- data$sueno_semana_finde - data$sueno_semana_academica

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
                       sdwd = data$sueno_semana_academica,
                       sdfd = data$sueno_semana_finde,
                       sddiff = data$sueno_diff,
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
data_scl <- data_scl[, c(2:7,11:13)]

## Transformar variables a numérico
data_scl |> str()
data_scl <- within(data_scl, {
  Pitcat <- as.numeric(Pitcat)
  HO <- as.numeric(HO)
  sdwd <- as.numeric(sdwd)
  sdfd <- as.numeric(sdfd)
  sddiff <- as.numeric(sddiff)
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
                      "munich_horas_diff", "spaq_gss",
                      "spaq_problema_cat", "spaq_sad_cat")

data_full$psqi_cat <- factor(data_full$psqi_cat, levels = 1:2, labels = c("Buen dormir", "Mal dormir"))
data_full$spaq_problema_cat <- factor(data_full$spaq_problema_cat, levels = 0:1, labels = c("No es un problema", "Es un problema"))
data_full$spaq_sad_cat <- factor(data.table::fifelse(data_full$spaq_sad_cat == 1, 1, 0, na = 0), levels = 0:1, labels = c("Típico","SAD"))

data_full$id <- seq_len(nrow(data_full))

sleep <- data_full
rm(data_full)

save(sleep, file = "data/sleep.RData")
