# Dívida pública: gasto ou juros?

App Shiny interativo para explicar a dívida pública brasileira a quem não é economista.
A ideia não é dizer quem gastou mais, e sim deixar a pessoa mexer nos dados e ver
quanto da dívida vem do déficit primário e quanto vem dos juros.

## O que o app faz

| Aba | Conteúdo |
|---|---|
| Mandatos | Dívida no início e no fim, primário médio e Selic média de cada mandato (FHC 1 a Lula 3; a dívida só tem dado a partir de Lula 1 na DLSP e Lula 2 na DBGG) |
| Por que a dívida mudou? | Barras empilhadas por ano: déficit primário, juros, crescimento do PIB, outros |
| Simulador | Sliders de Selic, crescimento, inflação e primário projetando a dívida em 5 a 20 anos |
| Metodologia | Definições, fórmulas e limitações |

## Como rodar

```r
install.packages(c("shiny", "bslib", "ggplot2", "jsonlite", "markdown"))
```

```bash
Rscript data-raw/baixar_dados.R   # baixa as séries do Banco Central para data/
Rscript -e 'shiny::runApp()'
```

## Estrutura

```
app.R                  interface e servidor
R/dados.R              download das séries (API SGS do Banco Central)
R/mandatos.R           datas dos mandatos e resumo por mandato
R/decomposicao.R       decomposição da variação da dívida
R/simulador.R          projeção da dívida
data-raw/baixar_dados.R  script de atualização dos dados
METODOLOGIA.md         texto exibido na aba Metodologia
```

## Cuidados metodológicos

- DBGG e DLSP nunca aparecem no mesmo gráfico; o usuário escolhe um.
- A DBGG só é mostrada na metodologia de 2008 (a partir de dez/2006), sem emenda com a série antiga.
- Tudo em % do PIB, nunca em reais nominais.

## Próximos passos

- [x] Conferir os códigos SGS em `R/dados.R` no site do Banco Central (out/2026: juros trocado de 5727 para 5760)
- [ ] Efeito cambial separado do resíduo "outros"
- [ ] Despesas obrigatórias x discricionárias (dados do Tesouro Nacional)
- [ ] Botão para exportar gráfico em formato vertical (stories)
- [ ] Publicar no shinyapps.io

## Licença

MIT. Dados: Banco Central do Brasil.
