---
title       : Núcleo de Métodos Quantitativos (QUANT) - FNDE
subtitle    : Proposta inicial de estruturação
author      : Allan Quadros
job         : 
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : [mathjax, quiz, bootstrap, interactive] # {mathjax, quiz, bootstrap}
ext_widgets : #{rCharts: [libraries/nvd3, libraries/leaflet, libraries/dygraphs]}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
logo        : fnde_blue.jpg
biglogo     : fnde_logo1.jpg
assets      : {assets: ../../assets}
--- .class #id

<style type="text/css">
body {background:grey transparent;
}
.quiz-option label{
  display: inline;
  font-size: 1em;
}
ul.nav li::before { content: ""; }  
ul.nav li{ font-size: 18px; line-height: 24px;}
</style>


## O FNDE ($)

<iframe src="./assets/widgets/plotly_orcamento.html" width="90%" height="50%" scrolling="no" seamless="seamless" frameBorder="1"> </iframe>



--- .class #id
## O FNDE

<img src="./assets/img/programas_fnde.PNG" style="float: left; width: 40%; margin-right: 1%; margin-bottom: 0.5em;">
  
- <img src="./assets/img/fies_logo.jpg" style="float: center; width: 15%; margin-right: 1%; margin-bottom: 0.5em;">
  <img src="./assets/img/saleduc_logo.jpg" style="float: center; width: 15%; margin-right: 1%; margin-bottom: 0.5em;">
  <img src="./assets/img/fundeb_logo.png" style="float: center; width: 15%; margin-right: 1%; margin-bottom: 0.5em;">



--- .segue bg:grey
# Como o FNDE manipula e utiliza esse grande volume de dados para gerar informação?



--- .segue bg:grey
<img src="./assets/img/excel_logo.jpg" style="float: center; width: 90%; margin: auto">

--- .segue bg:grey
<img src="./assets/img/basic_arithmetic.png" style="float: left; width: 70%; margin:auto">

--- .segue bg:grey
<img src="./assets/img/summation.png" style="float: left; width: 35%; margin:auto"> </br>

--- .segue bg:grey
<img src="./assets/img/mu.png" style="float: center; width: 60%; margin:auto">



--- &twocol
## Maus Exemplos
<br>
*** =left
> - <img src="./assets/img/indicador1.png" style="float: center; width: 90%; margin:auto">

> - <img src="./assets/img/indicador2.png" style="float: center; width: 90%; margin:auto">


*** =right

<br>
<br>
> - <img src="./assets/img/survival_bias.jpg" style="float: center; width: 80%; margin:auto">



--- .class #id
## Maus Exemplos

<br>

<img src="./assets/img/planilha_emendas.PNG" style="float: center; width: 100%; margin:auto">



--- &twocol
## Resumo do Problema

*** =left
<br>
<br>
> + .fragment imensa quantidade de dados
<br>
<br>
> + .fragment dados extremamente sensíveis (financeiros, estratégicos)
<br>
<br>

*** =right
<br>
<br>
> + .fragment `análise de dados amadora`
<br>
<br>
> + projeções equivocadas
> + indicadores inócuos/imprecisos
> + controles falhos
> + retrabalho
> + desperdício de tempo
> + desperdício de `$$$`



--- .segue bg:grey
# O Núcleo ...



--- .class #id
## Missão do Núcleo
<br>
Qual seria então o papel do Núcleo?

> .fragment `Tratar problemas` cuja origem ou solução estão nos dados utilizados pela autarquia e `produzir conhecimento` a partir destes dados de modo a contribuir para a melhora da gestão da educação brasileira.



--- .class #id
## Localização do Núcleo
<br>
<br>
<img src="./assets/img/organograma_NuQnt2.PNG" style="float: center; width: 80%; margin-right: 1%; margin-bottom: 0.5em;">



--- .class #id
## Atuação do Núcleo
<br>
<br>
> - indicadores <img src="./assets/img/check_mark.svg" style="width: 2%">
<br>
> - estruturação/organização/governança de dados <img src="./assets/img/check_mark.svg" style="width: 2%">
<br>
> - modelagem estatística/matemática <img src="./assets/img/check_mark.svg" style="width: 2%">
<br>
> - capacitação <img src="./assets/img/check_mark.svg" style="width: 2%">
<br>
> - coordenação de parcerias técnicas <img src="./assets/img/check_mark.svg" style="width: 2%">
<br>
> - otimização de rotinas e prototipação de soluções <img src="./assets/img/check_mark.svg" style="width: 2%">



--- .class #id
## Funcionamento do Núcleo
<br>
<br>
> - análise contínua de dados dos programas e formulação de indicadores
<br>
> - sistema de concorrência de demandas $\Rightarrow$ projetos estratégicos
    + presidência
    + diretorias/prospecção do Núcleo junto as áreas



--- .class #id
## Funcionamento do NMQ
<br>
<br>
<video width="480" height="360" controls >
<source src="./assets/img/apres_nuqnt_rev3.mp4" type="video/mp4">
</video>



--- .class #id
## Capacitação
<br>
<br>
> - rodadas de capacitação com as diretorias
  + intro à análise de dados 
  + R & Python
  + Estatística Básica
  


--- .class #id
## Como o núcleo pode ajudar?

> - <img src="./assets/img/inadimplencia_FIES.PNG" style="float: center; width: 50%; margin:auto">

> - `ideia:` utilizar um modelo de regressão logística

> - <img src="./assets/img/log_reg.PNG" style="width: 47%">



--- &twocol
## Como o núcleo pode ajudar?

*** =left

<iframe src="./assets/widgets/plotly_scatter1.html" width="90%" height="50%" scrolling="no" seamless="seamless" frameBorder="1"> </iframe>

*** =right

<iframe src="./assets/widgets/plotly_scatter2.html" width="90%" height="50%" scrolling="no" seamless="seamless" frameBorder="1"> </iframe>


--- &twocol
## Como o núcleo pode ajudar?

*** =left

<img src="./assets/img/modLog.png" style="float: center; width: 80%; margin:auto">

*** =right

<img src="assets/fig/unnamed-chunk-4-1.png" title="plot of chunk unnamed-chunk-4" alt="plot of chunk unnamed-chunk-4" style="display: block; margin: auto;" />


```
## [1] "accuracy: 0.66"
```

```
## [1] "sensitivity: 0.73"
```

```
## [1] "specificity: 0.57"
```

- falar do que um modelo bom de avaliação de risco de crédito para o sistema financeiro (mostrar aquele gráfico)



--- .class #id
## Algumas iniciativas isoladas

<br>
<br>
> - Malha-Fina FNDE
<br>
> - FisFa - Fiscaliza Fatura FIES
<br>
> - Conferência da carteira de títulos do FIES
<br>
> - Projeções financeiras de arrecadação
<br>
> - App para controle orçamentário de emendas parlamentares na DIFIN
<br>
> - Robô para raspagem do Diário Oficial da União
<br>
> - Capacitação de servidores em Análise de Dados em R (2 módulos)
<br>
- Outras ...



--- .class #id
## FisFa - Fiscaliza Fatura FIES
<br>
<br>
FisFa também transforma dados em informação:

<video width="480" height="320" controls >
<source src="./assets/img/FisFa_video2.mp4" type="video/mp4">
</video>




--- .class #id
## Cooperação Técnica & Parcerias

<div>
<p>

<img src="./assets/img/ence_logo.svg" style="float: left; width: 15%; margin-right: 1%; margin-bottom: 1.5em;">

<img src="./assets/img/impa_logo.png" style="float: left; width: 45%; margin-right: 1%; margin-bottom: 1.5em;">

</p>
</div>

<br>
<br>
<br>

<div>
<p>

<img src="./assets/img/estunb_logo1.png" style="float: left; width: 18%; margin-right: 1%; margin-bottom: 0.5em;"> <img src="./assets/img/estunb_logo2.png" style="float: left; width: 18%; margin-right: 1%; margin-bottom: 0.5em;">

<img src="./assets/img/enap_logo.jpg" style="float: left; width: 30%; margin-right: 1%; margin-bottom: 1.5em;">

<img src="./assets/img/i2a2_logo.jpg" style="float: left; width: 30%; margin-right: 1%; margin-bottom: 1.5em;">
</p>
</div>



--- .class #id
## Outras atividades

<br>
<br>
> - Abrir os dados do FNDE para a comunidade acadêmica
<br>
> - Receber estudantes de pós-graduação e graduação
  + criar um prêmio para trabalhos de conclusão de curso/dissertações/teses voltados para a análise de dados do FNDE
  + no caso de pós-graduação, agraciar o vencedor com uma vaga de consultoria para tocar o projeto
  + oferecer bolsas de estudos estudantes com projetos promissores de pós-graduação
  + oferecer vagas de estágio para estudantes com projetos de conclusão de curso promissores



--- .class #id
## Passos importantes para a estruturação do Núcleo

<br>
> - `Estruturação como unidade física` 
<br>
> - Portaria instituindo o Núcleo vinculado à AGEST, compondo a estrutura do Gabinete da presidência
<br>
> - formar inicialmente um Grupo de Trabalho (GT)
  + servidores, consultores e terceirizados de preferência já com formação em área afim
  + servidores e colaboradores full time
  + alguns servidores com conhecimento especializado, mas que ocupam outros cargos, podem contribuir eventualmente
<br>
> - iniciar imediatamente revisão do regimento interno para contemplar a estrutura do Núcleo no organograma do FNDE



--- .segue bg:grey
# Obrigado!!

<!-- como colocar figuras ao lado de palavras varias vezes em html -->
<div>
    <p>Feito no
    <img src="./assets/img/rball.png" style="float: center; width: 5%;"> com 
    <img src="./assets/img/slidifylogo2.png" style="float: center; width: 12%;">,
    <img src="./assets/img/plotlylogo.png" style="float: center; width: 7%;"> e
    <img src="./assets/img/ggplotlogo.png" style="float: center; width: 7%;"> !! </p>
</div>

Códigos e apresentação disponíveis no<img src="./assets/img/gitlogo5.png" style="float: center; width: 13%;">em:
<br>
<p>
<a href="http://github.com/allanvc/" style="color:black">http://github.com/allanvc/</a>
</p>

<p>
<a href="http://allanvc.github.io/apres_TCC2/" style="color:black">http://allanvc.github.io/slide_NMQ/</a>
</p>


