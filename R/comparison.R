# Prepare workspace --------------------------------------------------------

## Load libraries
######################

## Load dataset
load(file = "data/sleep.RData")

## Auxiliary functions
######################

# Between cities comparisons ----------------------------------------------

wilcox.test(ho_score ~ comuna, sleep, exact = FALSE) # *
wilcox.test(munich_horas_semana ~ comuna, sleep, exact = FALSE) # ns
wilcox.test(munich_horas_finde ~ comuna, sleep, exact = FALSE) # ns
wilcox.test(munich_horas_diff ~ comuna, sleep, exact = FALSE) # ns
wilcox.test(spaq_gss ~ comuna, sleep, exact = FALSE) # ns

t.test(ho_score ~ comuna, sleep, var.equal = FALSE) # *
t.test(munich_horas_semana ~ comuna, sleep, var.equal = FALSE) # ns
t.test(munich_horas_finde ~ comuna, sleep, var.equal = FALSE) # ns
t.test(munich_horas_diff ~ comuna, sleep, var.equal = FALSE) # ns
t.test(spaq_gss ~ comuna, sleep, var.equal = FALSE) # ns

with(sleep, table(comuna, psqi_cat)) |> chisq.test() # *
with(sleep, table(comuna, spaq_problema_cat)) |> chisq.test() # ns
with(sleep, table(comuna, spaq_sad_cat)) |> chisq.test() # ns


