library(shiny)
library(bslib)
library(ggplot2)

for (f in list.files("R", full.names = TRUE)) source(f)
dados <- carregar_dados()

ROTULOS <- c(dlsp_pib = "DLSP (dívida líquida do setor público)",
             dbgg_pib = "DBGG (dívida bruta do governo geral, metodologia 2008)")
ultimo <- function(x) tail(x[!is.na(x)], 1)
tema <- theme_minimal(base_size = 14) + theme(legend.position = "bottom")

# Formatação fixa em português, sem depender do locale do servidor.
MESES <- c("jan", "fev", "mar", "abr", "mai", "jun",
           "jul", "ago", "set", "out", "nov", "dez")
mes_ano <- function(d) paste0(MESES[as.integer(format(d, "%m"))], "/", format(d, "%Y"))
num <- function(x, sinal = FALSE)
  formatC(x, format = "f", digits = 1, decimal.mark = ",", flag = if (sinal) "+" else "")

ui <- page_navbar(
  title = "Dívida pública: gasto ou juros?",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  # sem "fillable", os gráficos mantêm a altura definida e a página rola;
  # com ele, os gráficos eram espremidos para caber na janela
  fillable = FALSE,
  sidebar = sidebar(
    radioButtons("indicador", "Qual dívida?", setNames(names(ROTULOS), ROTULOS)),
    helpText("Tudo em % do PIB. DBGG e DLSP não são comparáveis entre si.")
  ),
  nav_panel("Mandatos",
    selectInput("mandato", "Mandato", MANDATOS$mandato, selected = "Lula 3"),
    uiOutput("cartoes"),
    radioButtons("escala", NULL, inline = TRUE,
                 c("Variação total no mandato" = "variacao",
                   "Variação por ano de mandato" = "variacao_ano")),
    plotOutput("graf_mandatos", height = 380)
  ),
  nav_panel("Por que a dívida mudou?",
    uiOutput("aviso_decomp"),
    p("Variação anual da DLSP separada em: déficit primário, juros, efeito do",
      "crescimento do PIB e outros ajustes (câmbio, privatizações etc.)."),
    plotOutput("graf_decomp", height = 460)
  ),
  nav_panel("Simulador",
    layout_columns(
      col_widths = c(4, 8),
      card(
        sliderInput("selic", "Selic (% a.a.)", 2, 20, 13, step = 0.25),
        sliderInput("cresc", "Crescimento real do PIB (% a.a.)", -2, 6, 2, step = 0.25),
        sliderInput("infl", "Inflação (% a.a.)", 0, 12, 4, step = 0.25),
        sliderInput("prim", "Resultado primário (% do PIB; negativo = déficit)",
                    -4, 4, 0, step = 0.25),
        sliderInput("anos", "Horizonte (anos)", 5, 20, 10)
      ),
      card(plotOutput("graf_sim", height = 420), textOutput("txt_sim"))
    )
  ),
  nav_panel("Metodologia", includeMarkdown("METODOLOGIA.md"))
)

server <- function(input, output, session) {
  resumo <- reactive(resumo_mandatos(dados, input$indicador))

  output$cartoes <- renderUI({
    r <- resumo()[resumo()$mandato == input$mandato, ]
    val <- function(x, unidade = "%", sinal = FALSE)
      if (is.na(x)) "sem dado" else paste0(num(x, sinal), unidade)
    nota <- function(...) {
      partes <- c(...)
      if (length(partes) > 0) p(paste(partes, collapse = " · "))
    }
    quando <- function(d) if (!is.na(d)) mes_ano(d)
    layout_column_wrap(
      width = "170px", fill = FALSE,
      value_box("Dívida no início", val(r$divida_inicio), nota(quando(r$mes_inicio))),
      value_box("Dívida no fim", val(r$divida_fim),
                nota(quando(r$mes_fim), if (r$situacao != "") r$situacao)),
      value_box("Variação no mandato", val(r$variacao, " p.p.", sinal = TRUE),
                nota(if (!is.na(r$variacao_ano))
                  paste(val(r$variacao_ano, " p.p.", sinal = TRUE), "por ano"))),
      value_box("Resultado primário médio", val(r$resultado_primario, sinal = TRUE),
                nota("% do PIB; negativo = déficit")),
      value_box("Selic média", val(r$selic_media), nota("ao ano"))
    )
  })

  output$graf_mandatos <- renderPlot({
    r <- resumo()
    r <- r[!is.na(r$variacao), ]
    r$destaque <- r$mandato == input$mandato
    r$valor <- r[[input$escala]]
    rotulos <- setNames(ifelse(r$situacao == "", as.character(r$mandato),
                               paste0(r$mandato, "\n(", r$situacao, ")")),
                        r$mandato)
    por_ano <- input$escala == "variacao_ano"
    ggplot(r, aes(mandato, valor, fill = destaque)) +
      geom_col(show.legend = FALSE) +
      geom_hline(yintercept = 0) +
      scale_x_discrete(labels = rotulos) +
      scale_fill_manual(values = c(`FALSE` = "grey70", `TRUE` = "#2C3E50")) +
      labs(x = NULL,
           y = if (por_ano) "Variação da dívida por ano (p.p. do PIB)"
               else "Variação da dívida no mandato (p.p. do PIB)",
           caption = paste(if (por_ano) "Por ano = variação total ÷ duração do mandato.",
                           "Fonte: Banco Central do Brasil (SGS)")) + tema
  })

  # A decomposição usa o primário e os juros da NFSP, que correspondem à dívida
  # líquida; não servem para explicar a DBGG. Avisamos em vez de ignorar a escolha.
  output$aviso_decomp <- renderUI({
    if (input$indicador == "dbgg_pib")
      div(class = "alert alert-info",
          "Esta aba mostra sempre a DLSP. Os dados de déficit primário e juros do",
          "Banco Central são calculados para a dívida líquida e não explicam a",
          "variação da dívida bruta (DBGG).")
  })

  output$graf_decomp <- renderPlot({
    dec <- decompor(dados)
    comp <- c(primario = "Déficit primário", juros = "Juros",
              crescimento = "Crescimento do PIB", outros = "Outros ajustes")
    longo <- do.call(rbind, lapply(names(comp), function(k)
      data.frame(ano = dec$ano, componente = comp[[k]], valor = dec[[k]])))
    longo$componente <- factor(longo$componente, levels = comp)
    ggplot(longo, aes(ano, valor, fill = componente)) +
      geom_col() +
      geom_point(data = dec, aes(ano, variacao), inherit.aes = FALSE, size = 2) +
      geom_hline(yintercept = 0) +
      scale_fill_manual(values = c("#E67E22", "#C0392B", "#27AE60", "grey60")) +
      labs(x = NULL, y = "p.p. do PIB", fill = NULL,
           caption = "Ponto preto = variação total da DLSP no ano. Fonte: BCB (SGS)") + tema
  })

  sim <- reactive(simular(ultimo(dados[[input$indicador]]), input$selic,
                          input$cresc, input$infl, input$prim, input$anos))

  output$graf_sim <- renderPlot({
    ggplot(sim(), aes(ano, divida)) +
      geom_line(linewidth = 1.2, colour = "#C0392B") + geom_point(size = 2) +
      labs(x = "Anos à frente", y = "Dívida (% do PIB)") + tema
  })

  output$txt_sim <- renderText({
    s <- sim()
    sprintf("Com esses parâmetros, a dívida vai de %s%% para %s%% do PIB em %d anos.",
            num(s$divida[1]), num(tail(s$divida, 1)), input$anos)
  })
}

shinyApp(ui, server)
