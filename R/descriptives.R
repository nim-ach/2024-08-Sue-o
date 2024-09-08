
# Prepare workspace --------------------------------------------------------

## Load libraries
library(gtsummary)

## Load dataset
load(file = "data/sleep.RData")

## Auxiliary functions
######################


# Generate descriptive statistics -----------------------------------------

gtsummary::theme_gtsummary_language("es")

tbl <- tbl_summary(sleep[,-2L],
            by = comuna,
            label = list(
              psqi_cat ~ "Categoría PSQI", ho_score ~ "Puntaje de Matutinidad (Horne-Ostberg)",
              munich_horas_semana ~ "Horas de sueño dia de semana", munich_horas_finde ~ "Horas de sueño fin de semana",
              munich_horas_diff ~ "Diferencia horas de sueño", spaq_gss ~ "Puntaje total SPAQ",
              spaq_problema_cat ~ "Percepción de problema SPAQ", spaq_sad_cat ~ "Clasificación SPAQ"
            ),
            statistic = list(
              all_continuous() ~ "{mean} ± {sd}, {median} ({IQR})"
            ),
            digits = list(
              all_continuous() ~ 1
            ),
            missing = "no") |>
  add_overall() |>
  add_difference(include = all_continuous()) |>
  bold_labels() |>
  bold_p()

save(tbl, file = "R/tbl-descriptives.RData")
