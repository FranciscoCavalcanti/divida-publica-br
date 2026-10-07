# Decomposição anual da variação da DLSP (% do PIB), com dados de dezembro:
#   variação = déficit primário + juros nominais + efeito crescimento + outros
# efeito crescimento = -d[t-1] * g / (1 + g), g = crescimento do PIB nominal
# "outros" é o resíduo: ajuste cambial, privatizações, reconhecimento de dívidas etc.
decompor <- function(dados) {
  dez <- dados[format(dados$mes, "%m") == "12", ]
  dez <- dez[complete.cases(dez[, c("dlsp_pib", "primario_pib", "juros_pib", "pib_12m")]), ]
  n <- nrow(dez)
  if (n < 2) stop("Dados insuficientes para decompor.")
  g <- dez$pib_12m[-1] / dez$pib_12m[-n] - 1
  d_ant <- dez$dlsp_pib[-n]
  out <- data.frame(
    ano = as.integer(format(dez$mes[-1], "%Y")),
    variacao = dez$dlsp_pib[-1] - d_ant,
    primario = dez$primario_pib[-1],
    juros = dez$juros_pib[-1],
    crescimento = -d_ant * g / (1 + g)
  )
  out$outros <- out$variacao - out$primario - out$juros - out$crescimento
  out
}
