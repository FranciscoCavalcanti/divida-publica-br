MANDATOS <- data.frame(
  mandato = c("FHC 1", "FHC 2", "Lula 1", "Lula 2", "Dilma 1", "Dilma 2",
              "Temer", "Bolsonaro", "Lula 3"),
  inicio = as.Date(c("1995-01-01", "1999-01-01", "2003-01-01", "2007-01-01",
                     "2011-01-01", "2015-01-01", "2016-09-01", "2019-01-01",
                     "2023-01-01")),
  fim = as.Date(c("1998-12-01", "2002-12-01", "2006-12-01", "2010-12-01",
                  "2014-12-01", "2016-08-01", "2018-12-01", "2022-12-01",
                  "2026-12-01")),
  stringsAsFactors = FALSE
)

# Resumo por mandato para o indicador escolhido ("dlsp_pib" ou "dbgg_pib").
# Mandatos sem dados do indicador no início (ex.: DBGG antes de dez/2006) ficam NA:
# não emendamos séries de metodologias diferentes.
resumo_mandatos <- function(dados, indicador = "dlsp_pib") {
  linhas <- lapply(seq_len(nrow(MANDATOS)), function(i) {
    m <- MANDATOS[i, ]
    d <- dados[dados$mes >= m$inicio & dados$mes <= m$fim, ]
    # ponto de partida: último mês antes da posse
    antes <- dados[dados$mes < m$inicio & !is.na(dados[[indicador]]), ]
    ini <- if (nrow(antes) > 0) tail(antes[[indicador]], 1) else NA_real_
    v <- d[[indicador]][!is.na(d[[indicador]])]
    fim <- if (length(v) > 0) tail(v, 1) else NA_real_
    data.frame(
      mandato = m$mandato,
      divida_inicio = ini,
      divida_fim = fim,
      variacao = fim - ini,
      primario_medio = mean(d$primario_pib, na.rm = TRUE),
      juros_medio = mean(d$juros_pib, na.rm = TRUE),
      selic_media = mean(d$selic, na.rm = TRUE)
    )
  })
  out <- do.call(rbind, linhas)
  out$mandato <- factor(out$mandato, levels = MANDATOS$mandato)
  out
}
