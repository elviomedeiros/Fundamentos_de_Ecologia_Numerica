install.packages("shiny")
library(shiny)

ui <- fluidPage(
  titlePanel("Dinâmica da ANOVA"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("mean1", "Média Grupo A", -5, 15, 5),
      sliderInput("mean2", "Média Grupo B", -5, 15, 8),
      sliderInput("mean3", "Média Grupo C", -5, 15, 11),
      sliderInput("sd", "Desvio-padrão", 0.5, 6, 2),
      sliderInput("n", "Observações por grupo", 5, 50, 20)
    ),
    
    mainPanel(
      plotOutput("boxplot"),
      verbatimTextOutput("anova")
    )
  )
)

server <- function(input, output){
  
  dados <- reactive({
    
    set.seed(123)
    
    grupo <- factor(rep(c("A","B","C"), each = input$n))
    
    y <- c(
      rnorm(input$n, input$mean1, input$sd),
      rnorm(input$n, input$mean2, input$sd),
      rnorm(input$n, input$mean3, input$sd)
    )
    
    data.frame(grupo, y)
    
  })
  
  output$boxplot <- renderPlot({
    
    boxplot(y ~ grupo,
            data = dados(),
            col = c("tomato","skyblue","lightgreen"),
            ylab = "Resposta")
    
  })
  
  output$anova <- renderPrint({
    
    modelo <- aov(y ~ grupo, data = dados())
    
    print(summary(modelo))
    
  })
  
}

shinyApp(ui, server)

########################################

library(shiny)

# Dados originais
dados <- read.table(text = "
Site Grupos Srare dens S
S-R-CI-1 CI 5.000000 1.7857143 5
S-R-CI-2 CI 12.719603 86.3690476 13
S-R-CI-3 CI 15.057283 257.4404762 21
S-R-CI-4 CI 16.000000 57.9761905 16
B-A-SA-1 SA 2.000000 0.2976190 2
B-A-SA-2 SA 3.000000 0.8333333 3
B-A-SA-3 SA 6.000000 0.8928571 6
B-A-SA-4 SA 3.000000 2.3809524 3
B-A-MU-2 MU 1.000000 0.1785714 1
B-A-MU-3 MU 2.000000 0.2380952 2
B-A-MU-4 MU 2.000000 0.7142857 2
B-R-EP-2 EP 3.920860 82.7380952 4
B-R-EP-3 EP 5.000000 26.4880952 5
B-R-EP-4 EP 2.955224 79.7619048 3
S-A-RE-1 RE 13.171426 95.5952381 14
S-A-RE-2 RE 7.000000 64.3452381 7
S-A-RE-3 RE 19.932602 80.8333333 20
S-A-RE-4 RE 11.638514 103.6904762 12
S-R-SE-1 SE 7.000000 1.8452381 7
S-R-SE-2 SE 3.000000 75.4166667 3
S-R-SE-3 SE 3.763162 249.4642857 4
S-R-SE-4 SE 8.603085 406.6071429 11
", header = TRUE)

ui <- fluidPage(
  
  titlePanel("ANOVA dinâmica"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      sliderInput("CI","Adicionar aos valores de CI",
                  min=-10,max=10,value=0,step=0.5),
      
      sliderInput("SA","Adicionar aos valores de SA",
                  min=-10,max=10,value=0,step=0.5),
      
      sliderInput("MU","Adicionar aos valores de MU",
                  min=-10,max=10,value=0,step=0.5),
      
      sliderInput("EP","Adicionar aos valores de EP",
                  min=-10,max=10,value=0,step=0.5),
      
      sliderInput("RE","Adicionar aos valores de RE",
                  min=-10,max=10,value=0,step=0.5),
      
      sliderInput("SE","Adicionar aos valores de SE",
                  min=-10,max=10,value=0,step=0.5)
      
    ),
    
    mainPanel(
      
      plotOutput("box"),
      
      verbatimTextOutput("anova"),
      
      tableOutput("medias")
      
    )
  )
)

server <- function(input, output){
  
  dados2 <- reactive({
    
    d <- dados
    
    d$Srare[d$Grupos=="CI"] <- d$Srare[d$Grupos=="CI"] + input$CI
    d$Srare[d$Grupos=="SA"] <- d$Srare[d$Grupos=="SA"] + input$SA
    d$Srare[d$Grupos=="MU"] <- d$Srare[d$Grupos=="MU"] + input$MU
    d$Srare[d$Grupos=="EP"] <- d$Srare[d$Grupos=="EP"] + input$EP
    d$Srare[d$Grupos=="RE"] <- d$Srare[d$Grupos=="RE"] + input$RE
    d$Srare[d$Grupos=="SE"] <- d$Srare[d$Grupos=="SE"] + input$SE
    
    d
    
  })
  
  output$box <- renderPlot({
    
    boxplot(Srare ~ Grupos,
            data=dados2(),
            col="lightblue",
            pch=19)
    
    stripchart(Srare ~ Grupos,
               data=dados2(),
               vertical=TRUE,
               method="jitter",
               pch=19,
               add=TRUE,
               col="red")
    
  })
  
  output$anova <- renderPrint({
    
    modelo <- aov(Srare ~ Grupos, data=dados2())
    
    summary(modelo)
    
  })
  
  output$medias <- renderTable({
    
    aggregate(Srare ~ Grupos,
              dados2(),
              mean)
    
  })
  
}

shinyApp(ui, server)

############################################

library(shiny)

#=========================
# Dados originais
#=========================

dados <- data.frame(
  Grupos = c(rep("CI",4),rep("SA",4),rep("MU",3),
             rep("EP",3),rep("RE",4),rep("SE",4)),
  Srare = c(
    5,12.719603,15.057283,16,
    2,3,6,3,
    1,2,2,
    3.920860,5,2.955224,
    13.171426,7,19.932602,11.638514,
    7,3,3.763162,8.603085)
)

# Médias iniciais
medias <- tapply(dados$Srare, dados$Grupos, mean)

#=========================
# Interface
#=========================

ui <- fluidPage(
  
  titlePanel("ANOVA dinâmica"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Médias dos grupos"),
      
      sliderInput("CI","CI",0,25,medias["CI"],0.1),
      sliderInput("SA","SA",0,25,medias["SA"],0.1),
      sliderInput("MU","MU",0,25,medias["MU"],0.1),
      sliderInput("EP","EP",0,25,medias["EP"],0.1),
      sliderInput("RE","RE",0,25,medias["RE"],0.1),
      sliderInput("SE","SE",0,25,medias["SE"],0.1),
      
      hr(),
      
      sliderInput("sd","Desvio-padrão comum",
                  min=0.1,
                  max=8,
                  value=sd(dados$Srare),
                  step=0.1)
      
    ),
    
    mainPanel(
      
      plotOutput("box",height=400),
      
      verbatimTextOutput("anova")
      
    )
    
  )
  
)

#=========================
# Servidor
#=========================

server <- function(input, output){
  
  dados2 <- reactive({
    
    set.seed(123)
    
    d <- dados
    
    grupos <- unique(d$Grupos)
    
    medias <- c(
      CI=input$CI,
      SA=input$SA,
      MU=input$MU,
      EP=input$EP,
      RE=input$RE,
      SE=input$SE
    )
    
    for(g in grupos){
      
      n <- sum(d$Grupos==g)
      
      d$Srare[d$Grupos==g] <-
        rnorm(n,
              mean=medias[g],
              sd=input$sd)
      
    }
    
    d
    
  })
  
  output$box <- renderPlot({
    
    boxplot(Srare~Grupos,
            data=dados2(),
            col="lightblue",
            outline=FALSE,
            ylab="Srare")
    
    stripchart(Srare~Grupos,
               data=dados2(),
               vertical=TRUE,
               method="jitter",
               pch=19,
               col="red",
               add=TRUE)
    
  })
  
  output$anova <- renderPrint({
    
    modelo <- aov(Srare~Grupos,data=dados2())
    
    summary(modelo)
    
  })
  
}

shinyApp(ui,server)
