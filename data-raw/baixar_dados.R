# Rode na raiz do projeto: Rscript data-raw/baixar_dados.R
source("R/dados.R")
dados <- baixar_tudo()
dir.create("data", showWarnings = FALSE)
saveRDS(dados, "data/series.rds")
write.csv(dados, "data/series.csv", row.names = FALSE)
cat("Salvo:", nrow(dados), "meses, de", format(min(dados$mes)), "a", format(max(dados$mes)), "\n")
