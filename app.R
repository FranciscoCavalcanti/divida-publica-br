library(shiny)
library(bslib)
library(ggplot2)

for (f in list.files("R", full.names = TRUE)) source(f)
dados <- carregar_dados()

ROTULOS <- c(dlsp_pib = "DLSP (dívida líquida do setor público)",
             dbgg_pib = "DBGG (dívida bruta do governo geral, metodologia 2008)")
ultimo <- function(x) tail(x[!is.na(x)], 1)
tema <- theme_minimal(base_size = 14) + theme(legend.position = "bottom")

ui <- page_navbar(
  title = "Dívida pública: gasto ou juros?",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  sidebar = sidebar(
    radioButtons("indicador", "Qual dívida?", setNames(names(ROTULOS), ROTULOS)),
    helpText("Tudo em % do PIB. DBGG e DLSP não são comparáveis entre si.")
  ),
  nav_panel("Mandatos",
    selectInput("mandato", "Mandato", MANDATOS$mandato, selected = "Lula 3"),
    uiOutput("cartoes"),
    plotOutput("graf_mandatos", height = 380)
  ),
  nav_panel("Por que a dívida mudou?",
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
    fmt <- function(x) if (is.na(x)) "sem dado" else
      paste0(formatC(x, format = "f", digits = 1, decimal.mark = ","), "%")
    layout_columns(
      value_box("Dívida no início", fmt(r$divida_inicio)),
      value_box("Dívida no fim", fmt(r$divida_fim)),
      value_box("Primário médio (+ = déficit)", fmt(r$primario_medio)),
      value_box("Selic média", fmt(r$selic_media))
    )
  })

  output$graf_mandatos <- renderPlot({
    r <- resumo()
    r <- r[!is.na(r$variacao), ]
    r$destaque <- r$mandato == input$mandato
    ggplot(r, aes(mandato, variacao, fill = destaque)) +
      geom_col(show.legend = FALSE) +
      geom_hline(yintercept = 0) +
      scale_fill_manual(values = c(`FALSE` = "grey70", `TRUE` = "#2C3E50")) +
      labs(x = NULL, y = "Variação da dívida no mandato (p.p. do PIB)",
           caption = "Fonte: Banco Central do Brasil (SGS)") + tema
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
    pct <- function(x) formatC(x, format = "f", digits = 1, decimal.mark = ",")
    sprintf("Com esses parâmetros, a dívida vai de %s%% para %s%% do PIB em %d anos.",
            pct(s$divida[1]), pct(tail(s$divida, 1)), input$anos)
  })
}

shinyApp(ui, server)
