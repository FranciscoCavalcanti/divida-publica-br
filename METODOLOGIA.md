## Metodologia

**Indicadores.** O app mostra dois indicadores, sempre em % do PIB, e nunca os mistura:

- **DLSP** – Dívida Líquida do Setor Público: desconta ativos (como as reservas internacionais).
- **DBGG** – Dívida Bruta do Governo Geral, na metodologia adotada pelo Banco Central em 2008.
  A série começa em dezembro de 2006; por isso não há valores de DBGG para os governos
  anteriores. Não emendamos a série antiga com a nova.

**Mandatos.** A dívida "no início" é o último dado antes da posse; "no fim" é o último mês
do mandato. Dilma 2 termina em agosto de 2016 e Temer começa em setembro de 2016.

**Decomposição** (somente DLSP, dados de dezembro de cada ano):

    variação da dívida = déficit primário + juros nominais + efeito do crescimento + outros

O efeito do crescimento é `-d[t-1] * g / (1 + g)`, com `g` = crescimento do PIB nominal.
"Outros" é o resíduo e inclui ajuste cambial, privatizações e reconhecimento de dívidas.

**Simulador.** `d[t+1] = d[t] * (1 + Selic) / ((1 + crescimento) * (1 + inflação)) - primário`.
É uma simplificação didática: supõe que toda a dívida rende a Selic, o que exagera a
sensibilidade de curto prazo (parte da dívida é prefixada ou indexada à inflação).

**Fonte.** Banco Central do Brasil, Sistema Gerenciador de Séries Temporais (SGS).
Os códigos das séries estão em `R/dados.R`.
