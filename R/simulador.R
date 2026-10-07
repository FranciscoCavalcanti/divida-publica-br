# Dinâmica da dívida (% do PIB):
#   d[t+1] = d[t] * (1 + i) / ((1 + g) * (1 + pi)) - primario
# i = juro nominal (Selic), g = crescimento real, pi = inflação,
# primario = superávit primário em % do PIB (negativo = déficit).
# Simplificação: supõe que toda a dívida é remunerada pela Selic.
simular <- function(d0, selic, crescimento, inflacao, primario, anos = 10) {
  d <- numeric(anos + 1)
  d[1] <- d0
  fator <- (1 + selic / 100) / ((1 + crescimento / 100) * (1 + inflacao / 100))
  for (t in seq_len(anos)) d[t + 1] <- d[t] * fator - primario
  data.frame(ano = 0:anos, divida = d)
}
