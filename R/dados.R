# Coleta de séries do SGS (Banco Central do Brasil).
# Códigos conferidos em out/2026 no catálogo de dados abertos do BCB.
# Cobertura: DLSP desde dez/2001, DBGG desde dez/2006, NFSP desde nov/2002.
SERIES <- c(
  dlsp_pib     = 4513,   # DLSP, % do PIB, setor público consolidado
  dbgg_pib     = 13762,  # DBGG, % do PIB, metodologia vigente a partir de 2008
  selic        = 4189,   # Selic acumulada no mês, anualizada (% a.a.)
  primario_pib = 5793,   # NFSP primário, % do PIB, acum. 12 meses (positivo = déficit)
  juros_pib    = 5760,   # NFSP juros nominais, % do PIB, acum. 12 meses
                         # (não usar 5727: é o resultado nominal = primário + juros)
  pib_12m      = 4382    # PIB nominal acumulado em 12 meses (R$ milhões)
)

# Baixa uma janela da API. Devolve NULL só quando a API responde 404 (série sem
# dados no período, ex.: DBGG antes de 2006). Outras falhas (502, rede) são
# tentadas de novo e, se persistirem, interrompem o download: engolir o erro
# salvaria séries com buracos e o app mostraria números errados sem aviso.
baixar_janela <- function(url, tentativas = 4) {
  for (k in seq_len(tentativas)) {
    msgs <- character()
    res <- tryCatch(
      withCallingHandlers(jsonlite::fromJSON(url), warning = function(w) {
        msgs <<- c(msgs, conditionMessage(w))
        invokeRestart("muffleWarning")
      }),
      error = function(e) {
        msgs <<- c(msgs, conditionMessage(e))
        NULL
      }
    )
    if (is.data.frame(res) || (!is.null(res) && length(res) == 0)) return(res)
    if (any(grepl("404", msgs))) return(NULL)
    if (k < tentativas) Sys.sleep(2 * k)
  }
  stop("Falha ao baixar ", url, "\n", paste(msgs, collapse = "\n"))
}

# A API do SGS limita cada consulta a 10 anos; baixamos em janelas.
baixar_sgs <- function(codigo, inicio = as.Date("1995-01-01"), fim = Sys.Date()) {
  cortes <- seq(inicio, fim, by = "10 years")
  partes <- lapply(seq_along(cortes), function(i) {
    ini <- cortes[i]
    f <- if (i < length(cortes)) cortes[i + 1] - 1 else fim
    url <- sprintf(
      "https://api.bcb.gov.br/dados/serie/bcdata.sgs.%s/dados?formato=json&dataInicial=%s&dataFinal=%s",
      codigo, format(ini, "%d/%m/%Y"), format(f, "%d/%m/%Y")
    )
    baixar_janela(url)
  })
  partes <- Filter(function(x) is.data.frame(x) && nrow(x) > 0, partes)
  if (length(partes) == 0) stop("Série ", codigo, " não retornou dados.")
  df <- do.call(rbind, partes)
  data.frame(data = as.Date(df$data, "%d/%m/%Y"), valor = as.numeric(df$valor))
}

# Baixa todas as séries e devolve uma tabela mensal larga (uma coluna por série).
baixar_tudo <- function() {
  lista <- lapply(names(SERIES), function(nome) {
    df <- baixar_sgs(SERIES[[nome]])
    df$mes <- as.Date(format(df$data, "%Y-%m-01"))
    df <- df[!duplicated(df$mes, fromLast = TRUE), c("mes", "valor")]
    names(df)[2] <- nome
    df
  })
  out <- Reduce(function(a, b) merge(a, b, by = "mes", all = TRUE), lista)
  out[order(out$mes), ]
}

carregar_dados <- function(arquivo = "data/series.rds") {
  if (!file.exists(arquivo)) {
    dados <- baixar_tudo()
    dir.create(dirname(arquivo), showWarnings = FALSE)
    saveRDS(dados, arquivo)
  }
  readRDS(arquivo)
}
