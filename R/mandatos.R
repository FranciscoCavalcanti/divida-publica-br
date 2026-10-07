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

# Média só quando a série cobre o mandato desde o primeiro mês; senão NA
# (ex.: o primário começa em nov/2002 e não serve para descrever FHC 2).
media_coberta <- function(x, meses, inicio) {
  ok <- !is.na(x)
  if (!any(ok) || min(meses[ok]) > inicio) return(NA_real_)
  mean(x[ok])
}

# Número de meses de a até b (datas no primeiro dia do mês).
meses_entre <- function(a, b) {
  12 * (as.integer(format(b, "%Y")) - as.integer(format(a, "%Y"))) +
    as.integer(format(b, "%m")) - as.integer(format(a, "%m"))
}

# Resumo por mandato para o indicador escolhido ("dlsp_pib" ou "dbgg_pib").
# Mandatos sem dados do indicador no início (ex.: DBGG antes de dez/2006) ficam NA:
# não emendamos séries de metodologias diferentes.
resumo_mandatos <- function(dados, indicador = "dlsp_pib", hoje = Sys.Date()) {
  tem <- !is.na(dados[[indicador]])
  linhas <- lapply(seq_len(nrow(MANDATOS)), function(i) {
    m <- MANDATOS[i, ]
    d <- dados[dados$mes >= m$inicio & dados$mes <= m$fim, ]
    # ponto de partida: último mês antes da posse; ponto final: último mês com dado
    antes <- dados[tem & dados$mes < m$inicio, ]
    durante <- dados[tem & dados$mes >= m$inicio & dados$mes <= m$fim, ]
    mes_ini <- if (nrow(antes) > 0) tail(antes$mes, 1) else as.Date(NA)
    mes_fim <- if (nrow(durante) > 0) tail(durante$mes, 1) else as.Date(NA)
    ini <- if (nrow(antes) > 0) tail(antes[[indicador]], 1) else NA_real_
    fim <- if (nrow(durante) > 0) tail(durante[[indicador]], 1) else NA_real_
    ultimo_dia <- seq(m$fim, by = "month", length.out = 2)[2] - 1
    em_andamento <- hoje >= m$inicio && hoje <= ultimo_dia
    parcial <- !is.na(mes_fim) && mes_fim < m$fim
    data.frame(
      mandato = m$mandato,
      mes_inicio = mes_ini,
      divida_inicio = ini,
      mes_fim = mes_fim,
      divida_fim = fim,
      variacao = fim - ini,
      # mandatos têm durações diferentes (Dilma 2: 20 meses; Temer: 28)
      variacao_ano = (fim - ini) / (meses_entre(mes_ini, mes_fim) / 12),
      situacao = if (em_andamento) "em andamento" else if (parcial) "dados parciais" else "",
      # a NFSP é positiva no déficit; invertemos para a convenção usual
      # (resultado primário positivo = superávit), a mesma do simulador
      resultado_primario = -media_coberta(d$primario_pib, d$mes, m$inicio),
      juros_medio = media_coberta(d$juros_pib, d$mes, m$inicio),
      selic_media = media_coberta(d$selic, d$mes, m$inicio)
    )
  })
  out <- do.call(rbind, linhas)
  out$mandato <- factor(out$mandato, levels = MANDATOS$mandato)
  out
}
