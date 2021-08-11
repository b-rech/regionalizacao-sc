# Autor: Bruno Rech
# E-mail: b.rech@outlook.com; bruno.rech@grad.ufsc.br

# PACOTES REQUERIDOS

library(shiny)
library(magrittr)
library(plotly)

# DADOS AUXILIARES

# Coeficientes da curva de permanência
permanence = read.csv('RegHomPerm.csv', sep = ';')

# Coeficientes da Q7,10
k7_10 = c('I'=0.59, 'II'=0.59, 'III'=0.40, 'IV'=0.57, 'V'=0.55, 'VI'=0.41, 'VII'=0.49,
             'VIII'=0.49, 'IX'=0.47, 'X'=0.56, 'XI'=0.58, 'XII'=0.52, 'XIII'=0.37)

# INTERFACE

ui = fluidPage(
  titlePanel('Regionalização de vazões para Santa Catarina'),
  h4('Elaborado por', a('Bruno Rech', href = 'https://github.com/b-rech')),
  hr(),
  
  # Texto inicial
  h4(strong('Leia com atenção antes de realizar os cálculos')),
  p('Esta aplicação foi construída no intuito de facilitar a obtenção de vazões de
    referência para corpos hídricos no estado de Santa Catarina.'),
  p('Todas as relações funcionais requeridas para a execução dos cálculos foram obtidas
    no estudo de regionalização de vazões das bacias hidrográficas de Santa Catarina (2006).'),
  p('Como dados de entrada são requeridas informações de chuva média anual, área e de localização
    da sub-bacia hidrográfica de interesse.'),
  p('O estudo de regionalização dividiu Santa Catarina em áreas homogêneas, tanto no respeito da
    vazão média de longo termo quanto em relação à curva de permanência e à vazão mínima anual
    de sete dias.'),
  p('Para tanto, antes de preencher os campos é necessário consultar o relatório do estudo para
    definir as regiões homogêneas às quais pertencem a área de interesse. Abaixo de cada campo
    é dada a referência do desenho (mapa) e a página do relatório onde estas podem ser consultadas.'),
  a(strong('ACESSE O RELATÓRIO.'), 
    href = 'https://www.aguas.sc.gov.br/jsmallfib_top/DHRI/Legislacao/estudo_de_regionalizacao_hidrologica.pdf'),
  hr(),
  h4(strong('Sobre a superestimação de vazões')),
  p('Você pode se deparar com um aviso de que há uma provável superestimação da vazão média 
    de longo termo. Esse aviso é gerado quando a Q', tags$sub('MLT'), 'é superior à vazão que
    seria gerada caso toda a precipitação anual fosse convertida em vazão. Para mais informações,
    consulte a referência ', a('Pruski et al. (2013).', href = 'http://dx.doi.org/10.1016/j.jhydrol.2012.10.005')),
  hr(),
  
  # Painel de entradas
  wellPanel(
    selectInput('rh_qmlt', 'Região homogênea das vazões', 
                choices = c('M1', 'M2', 'M3', 'M4', 'M5')),
    helpText('Consulte o desenho 676-BAM-SEC-A1-P027 (p. 61).'),
    hr(),
    selectInput('rh_perm', 'Região homogênea da curva de permanência', 
                choices = colnames(permanence)[-1]),
    helpText('Consulte o desenho 676-BAM-SEC-A1-P028 (p. 86).'),
    hr(),
    selectInput('rh_q7', 'Região homogênea para vazão mínima de 7 dias',
                choices = c('M7-1', 'M7-2', 'M7-3', 'M7-4', 'M7-5', 'M7-6')),
    helpText('Consulte o desenho 676-BAM-SEC-A1-P029 (p. 114).'),
    hr(),
    numericInput('adr', 'Área de drenagem (km²)', value = 0),
    helpText('Se refere à área que drena para o ponto de interesse. NÃO utilize separador de milhar.'),
    hr(),
    numericInput('pmedia', 'Precipitação média anual (mm/ano)', value = 0),
    helpText('Pode-se consultar o desenho 676-BAM-SEC-A1-P026 (p. 40),
             que apresenta o mapa de isoietas para Santa Catarina.'),
    actionButton('calc', 'Calcular', class = "btn-lg btn-success")),
  
  # Painel de saídas
  sidebarLayout(
    # Lateral
    sidebarPanel(
      h4(strong('Vazões de referência')),
      hr(),
      htmlOutput('qmlt'),
      htmlOutput('q90'),
      htmlOutput('q95'),
      htmlOutput('q98'),
      htmlOutput('q7_10'),
      hr(),
      span(strong(htmlOutput('concluido'), style = 'color:green')),
      span(strong(htmlOutput('consist'), style = 'color:red'))),
    # Principal (plot)
    mainPanel(plotlyOutput('permplot')))
)

# BACK END

server = function(input, output, session){
    
  # Qmlt
  
  qmlt = eventReactive(input$calc,
    {if(input$rh_qmlt == 'M1'){
        qmlt = 1.240E-4*(input$pmedia**0.759)*(input$adr**0.968)
    }else if(input$rh_qmlt == 'M2'){
        qmlt = 6.570E-5*(input$pmedia**0.748)*(input$adr**1.021)
    }else if(input$rh_qmlt == 'M3'){
        qmlt = 1.887E-5*(input$pmedia**1.142)*(input$adr**0.828)
    }else if(input$rh_qmlt == 'M4'){
        qmlt = 9.393E-4*(input$pmedia**0.362)*(input$adr**1.092)
    }else{
        qmlt = 2.698E-5*(input$pmedia**0.946)*(input$adr**1.049)
    }
    qmlt})
  
  # Q7
  
  q7 = eventReactive(input$calc,
    {if(input$rh_q7 == 'M7-1'){
        q7 = 4.984E-3*(input$adr**0.986)
    }else if(input$rh_q7 == 'M7-2'){
        q7 = 1.601E-1*(input$adr**0.395)
    }else if(input$rh_q7 == 'M7-3'){
        q7 = 1.846E-3*(input$adr**1.070)
    }else if(input$rh_q7 == 'M7-4'){
        q7 = 4.412E-2*(input$adr**0.744)
    }else if(input$rh_q7 == 'M7-5'){
        q7 = 3.563E-3*(input$adr**1.119)
    }else{
        q7 = 3.747E-4*(input$adr**1.366)
    }
    q7})

  # Permanencia - Realiza o cálculo dos percentuais de permanência
  permanencia = eventReactive(input$calc,
    {data.frame(permanence$Permanence, round(qmlt()*permanence[input$rh_perm], 6))})
  
  # Vazoes de referencia
  
  qmlt_out = eventReactive(input$calc,
    {HTML(paste0('Q',tags$sub('MLT'), ' = ', round(qmlt(), 6), ' m³/s',
                                       ' = ', round(60*60*qmlt(), 2), ' m³/h'))})
  
  q90_out = eventReactive(input$calc,
    {HTML(paste0('Q',tags$sub('90'), ' = ', round(permanencia()[18, 2], 6), ' m³/s',
                                       ' = ', round(60*60*permanencia()[18, 2], 2), ' m³/h'))})
  
  q95_out = eventReactive(input$calc,
    {HTML(paste0('Q',tags$sub('95'), ' = ', round(permanencia()[19, 2], 6), ' m³/s',
                                       ' = ', round(60*60*permanencia()[19, 2], 2), ' m³/h'))})
  
  q98_out = eventReactive(input$calc,
    {HTML(paste0('Q',tags$sub('98'), ' = ', round(permanencia()[20, 2], 6), ' m³/s',
                                       ' = ', round(60*60*permanencia()[20, 2], 2), ' m³/h'))})
  
  q7_10_out = eventReactive(input$calc,
    {HTML(paste0('Q',tags$sub('7,10'), ' = ', round(q7()*k7_10[input$rh_perm], 6),
                              ' m³/s', ' = ', round(60*60*q7()*k7_10[input$rh_perm], 2), ' m³/h'))})
  
  q_efetiva = eventReactive(input$calc,
    {input$adr*input$pmedia/31536})
  
  aviso = eventReactive(input$calc,
    {if(qmlt() > q_efetiva()){
      aviso = paste0('ATENÇÃO: provável superestimação da Q',tags$sub('MLT'), '.')
    }else{
      aviso = NULL
    }
    aviso})
  
  conclusao = eventReactive(input$calc,
                            {paste0('Cálculos concluídos.')})
  
  
  output$qmlt = renderText(qmlt_out())
  output$q90 = renderText(q90_out())
  output$q95 = renderText(q95_out())
  output$q98 = renderText(q98_out())
  output$q7_10 = renderText(q7_10_out())
  output$concluido = renderText(HTML(conclusao()))
  output$consist = renderText(HTML(aviso()))
  
  
  # Plot
  
  axx = list(title = 'Permanência (%)')
  axy = list(title = 'Vazão (m³/s)')
  
  plot = eventReactive(input$calc,
    {plot_ly(x = permanencia()[, 1], y = permanencia()[, 2], width = 800, height = 600,
              type = 'scatter', mode = 'lines+markers') %>%
    layout(xaxis = axx, yaxis = axy, title = 'Permanência das vazões médias mensais')})
  
  output$permplot = renderPlotly(plot())
}

shinyApp(ui, server)