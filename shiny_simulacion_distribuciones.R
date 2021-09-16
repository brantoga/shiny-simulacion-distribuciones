# Dashboard de variables aleatorias continuas ##################################

#library(actuar)
library(stats)
library(shiny)
library(ggplot2)
library(qqplotr)
help(distributions)
# Diseño del tema de las gráficas ##############################################
tema <- theme(plot.title = element_text(size = (14), colour="steelblue4",hjust = 0.5,face = "bold"), 
              legend.title = element_text(colour = "steelblue4"), 
              legend.text = element_text(colour="steelblue4"), 
              axis.title = element_text(size = (12), colour = "steelblue4",face = "bold"),
              axis.text = element_text(colour = "black", size = (12)))

# Diseño del Tablero ###########################################################
ui<-fluidPage(
  titlePanel("Distribuciones continuas"),
  
  sidebarLayout(position = "right",
  
                sidebarPanel(
                  selectInput('dist',label="Distribuciones",
                              choices = c("Normal","Beta","Cauchy",
                                          "Chi cuadrada","Exponencial",
                                          "F","Gamma","Log normal","Poisson",
                                          "t de Student","Uniforme","Weibull")),
                  sliderInput(inputId = "n",label="Número de Simulaciones",
                              value=100,min = 1,max = 1000),
                  sliderInput(inputId = "p1",label="Parámetro 1",
                              value=1,min=0,max=100),
                  sliderInput(inputId = "p2",label="Parámetro 2",
                              value=1,min=0,max=100)
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel("Densidad", plotOutput("g1")),
                    tabPanel("Distribución", plotOutput("g2")),
                    tabPanel("Sobrevivencia", plotOutput("g3"))
                  )
                )
  ),
  
  verbatimTextOutput("t1"),
  
  fluidRow(align="center",
           column(5, plotOutput('g4')),
           column(2, plotOutput('g5')),
           column(5, plotOutput('g6'))
  )
  
)

# Carga de los datos ###########################################################
server<-function(input,output){

  data<-reactive(data.frame(nn = c(1:input$n),rr = r()))
  
  r<-reactive({
    if(input$dist=="Normal"){
      rnorm(input$n,input$p1,input$p2)
    }
    else if(input$dist=="Exponencial"){
      rexp(input$n,input$p1)
    }
    else if(input$dist=="Gamma"){
      rgamma(input$n,input$p1,input$p2)
    }
    else if(input$dist=="Beta"){ #2
      rbeta(input$n,input$p1,input$p2)
    }
    else if(input$dist=="Cauchy"){ #2
      rcauchy(input$n,input$p1,input$p2)
    }
    else if(input$dist=="Chi cuadrada"){ #2
      rchisq(input$n,input$p1,input$p2)
    }
    else if(input$dist=="F"){ #2
      rf(input$n,input$p1,input$p2)
    }
    else if(input$dist=="Log normal"){ #2
      rlnorm(input$n,input$p1,input$p2)
    }
    else if(input$dist=="Poisson"){ #1
      rpois(input$n,input$p1)
    }
    else if(input$dist=="t de Student"){ #2
      rt(input$n,input$p1,input$p2)
    }
    else if(input$dist=="Uniforme"){ #2
      runif(input$n,input$p1,input$p2)
    }
    else if(input$dist=="Weibull"){ #2
      rweibull(input$n,input$p1,input$p2)
    }
  })
  
  d<-reactive({
    if(input$dist=="Normal"){
      data.frame(x = seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                 y = dnorm(seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                           input$p1,input$p2))
    }
    else if(input$dist=="Exponencial"){
      data.frame(x = seq(input$p1-4*1,input$p1+4*1,by=0.25),
                 y = dexp(seq(input$p1-4*1,input$p1+4*1,by=0.25),
                           input$p1))
    }
    else if(input$dist=="Gamma"){
      data.frame(x = seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                 y = dgamma(seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                           input$p1,input$p2))
    }
    else if(input$dist=="Beta"){ #2
      data.frame(x = seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                 y = dbeta(seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                           input$p1,input$p2))
    }
    else if(input$dist=="Cauchy"){ #2
      data.frame(x = seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                 y = dcauchy(seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                           input$p1,input$p2))
    }
    else if(input$dist=="Chi cuadrada"){ #2
      data.frame(x = seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                 y = dchisq(seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                           input$p1,input$p2))
    }
    else if(input$dist=="F"){ #2
      data.frame(x = seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                 y = df(seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                           input$p1,input$p2))
    }
    else if(input$dist=="Log normal"){ #2
      data.frame(x = seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                 y = dlnorm(seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                           input$p1,input$p2))
    }
    else if(input$dist=="Poisson"){ #1
      data.frame(x = seq(input$p1-4*1,input$p1+4*1,by=0.25),
                 y = dpois(seq(input$p1-4*1,input$p1+4*1,by=0.25),
                          input$p1))
    }
    else if(input$dist=="t de Student"){ #2
      data.frame(x = seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                 y = dt(seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                           input$p1,input$p2))
    }
    else if(input$dist=="Uniforme"){ #2
      data.frame(x = seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                 y = dunif(seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                           input$p1,input$p2))
    }
    else if(input$dist=="Weibull"){ #2
      data.frame(x = seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                 y = dweibull(seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                           input$p1,input$p2))
    }
  })
  
  p<-reactive({
    if(input$dist=="Normal"){
      data.frame(x = seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                          y = pnorm(seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                                    input$p1,input$p2))
    }
    else if(input$dist=="Exponencial"){
      data.frame(x = seq(input$p1-4*1,input$p1+4*1,by=0.25),
                          y = pexp(seq(input$p1-4*1,input$p1+4*1,by=0.25),
                                    input$p1))
    }
    else if(input$dist=="Gamma"){
      data.frame(x = seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                          y = pgamma(seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                                    input$p1,input$p2))
    }
    else if(input$dist=="Beta"){ #2
      data.frame(x = seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                 y = pbeta(seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                            input$p1,input$p2))
    }
    else if(input$dist=="Cauchy"){ #2
      data.frame(x = seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                 y = pcauchy(seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                            input$p1,input$p2))
    }
    else if(input$dist=="Chi cuadrada"){ #2
      data.frame(x = seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                 y = pchisq(seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                            input$p1,input$p2))
    }
    else if(input$dist=="F"){ #2
      data.frame(x = seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                 y = pf(seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                            input$p1,input$p2))
    }
    else if(input$dist=="Log normal"){ #2
      data.frame(x = seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                 y = plnorm(seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                            input$p1,input$p2))
    }
    else if(input$dist=="Poisson"){ #1
      data.frame(x = seq(input$p1-4*1,input$p1+4*1,by=0.25),
                 y = ppois(seq(input$p1-4*1,input$p1+4*1,by=0.25),
                          input$p1))
    }
    else if(input$dist=="t de Student"){ #2
      data.frame(x = seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                 y = pt(seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                            input$p1,input$p2))
    }
    else if(input$dist=="Uniforme"){ #2
      data.frame(x = seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                 y = punif(seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                            input$p1,input$p2))
    }
    else if(input$dist=="Weibull"){ #2
      data.frame(x = seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                 y = pweibull(seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                            input$p1,input$p2))
    }
  })
  
  s<-reactive({
    if(input$dist=="Normal"){
      data.frame(x = seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                 y = 1 - pnorm(seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                           input$p1,input$p2))
    }
    else if(input$dist=="Exponencial"){
      data.frame(x = seq(input$p1-4*1,input$p1+4*1,by=0.25),
                 y = 1 - pexp(seq(input$p1-4*1,input$p1+4*1,by=0.25),
                          input$p1))
    }
    else if(input$dist=="Gamma"){
      data.frame(x = seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                 y = 1 - pgamma(seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                            input$p1,input$p2))
    }
    else if(input$dist=="Beta"){ #2
      data.frame(x = seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                 y = 1 - pbeta(seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                           input$p1,input$p2))
    }
    else if(input$dist=="Cauchy"){ #2
      data.frame(x = seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                 y = 1 - pcauchy(seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                             input$p1,input$p2))
    }
    else if(input$dist=="Chi cuadrada"){ #2
      data.frame(x = seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                 y = 1 - pchisq(seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                            input$p1,input$p2))
    }
    else if(input$dist=="F"){ #2
      data.frame(x = seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                 y = 1 - pf(seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                        input$p1,input$p2))
    }
    else if(input$dist=="Log normal"){ #2
      data.frame(x = seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                 y = 1 - plnorm(seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                            input$p1,input$p2))
    }
    else if(input$dist=="Poisson"){ #1
      data.frame(x = seq(input$p1-4*1,input$p1+4*1,by=0.25),
                 y = 1 - ppois(seq(input$p1-4*1,input$p1+4*1,by=0.25),
                           input$p1))
    }
    else if(input$dist=="t de Student"){ #2
      data.frame(x = seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                 y = 1 - pt(seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                        input$p1,input$p2))
    }
    else if(input$dist=="Uniforme"){ #2
      data.frame(x = seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                 y = 1 - punif(seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                           input$p1,input$p2))
    }
    else if(input$dist=="Weibull"){ #2
      data.frame(x = seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                 y = 1 - pweibull(seq(input$p1-4*input$p2,input$p1+4*input$p2,by=0.25),
                              input$p1,input$p2))
    }
  })

  g1a<-reactive({
    if(input$dist=="Normal"){
      geom_point(aes(y = dnorm(r(),input$p1,input$p2)), color = "black",alpha=.7,size = 2)
    }
    else if(input$dist=="Exponencial"){
      geom_point(aes(y = dexp(r(),input$p1)), color = "black",alpha=.7,size = 2)
    }
    else if(input$dist=="Gamma"){
      geom_point(aes(y = dgamma(r(),input$p1,input$p2)), color = "black",alpha=.7,size = 2)
    }
    else if(input$dist=="Beta"){ #2
      geom_point(aes(y = dbeta(r(),input$p1,input$p2)), color = "black",alpha=.7,size = 2)
    }
    else if(input$dist=="Cauchy"){ #2
      geom_point(aes(y = dcauchy(r(),input$p1,input$p2)), color = "black",alpha=.7,size = 2)
    }
    else if(input$dist=="Chi cuadrada"){ #2
      geom_point(aes(y = dchisq(r(),input$p1,input$p2)), color = "black",alpha=.7,size = 2)
    }
    else if(input$dist=="F"){ #2
      geom_point(aes(y = df(r(),input$p1,input$p2)), color = "black",alpha=.7,size = 2)
    }
    else if(input$dist=="Log normal"){ #2
      geom_point(aes(y = dlnorm(r(),input$p1,input$p2)), color = "black",alpha=.7,size = 2)
    }
    else if(input$dist=="Poisson"){ #1
      geom_point(aes(y = dpois(r(),input$p1)), color = "black",alpha=.7,size = 2)
    }
    else if(input$dist=="t de Student"){ #2
      geom_point(aes(y = dt(r(),input$p1,input$p2)), color = "black",alpha=.7,size = 2)
    }
    else if(input$dist=="Uniforme"){ #2
      geom_point(aes(y = dunif(r(),input$p1,input$p2)), color = "black",alpha=.7,size = 2)
    }
    else if(input$dist=="Weibull"){ #2
      geom_point(aes(y = dweibull(r(),input$p1,input$p2)), color = "black",alpha=.7,size = 2)
    }
  })
  
  g2a<-reactive({
    if(input$dist=="Normal"){
      ggplot(data(), aes(x=rr,pnorm(r(),input$p1,input$p2)))
    }
    else if(input$dist=="Exponencial"){
      ggplot(data(), aes(x=rr,pexp(r(),input$p1)))
    }
    else if(input$dist=="Gamma"){
      ggplot(data(), aes(x=rr,pgamma(r(),input$p1,input$p2)))
    }
    else if(input$dist=="Beta"){ #2
      ggplot(data(), aes(x=rr,pbeta(r(),input$p1,input$p2)))
    }
    else if(input$dist=="Cauchy"){ #2
      ggplot(data(), aes(x=rr,pcauchy(r(),input$p1,input$p2)))
    }
    else if(input$dist=="Chi cuadrada"){ #2
      ggplot(data(), aes(x=rr,pchisq(r(),input$p1,input$p2)))
    }
    else if(input$dist=="F"){ #2
      ggplot(data(), aes(x=rr,pf(r(),input$p1,input$p2)))
    }
    else if(input$dist=="Log normal"){ #2
      ggplot(data(), aes(x=rr,plnormal(r(),input$p1,input$p2)))
    }
    else if(input$dist=="Poisson"){ #1
      ggplot(data(), aes(x=rr,ppois(r(),input$p1)))
    }
    else if(input$dist=="t de Student"){ #2
      ggplot(data(), aes(x=rr,pt(r(),input$p1,input$p2)))
    }
    else if(input$dist=="Uniforme"){ #2
      ggplot(data(), aes(x=rr,punif(r(),input$p1,input$p2)))
    }
    else if(input$dist=="Weibull"){ #2
      ggplot(data(), aes(x=rr,pweibull(r(),input$p1,input$p2)))
    }
  })
  
  g2b<-reactive({
    if(input$dist=="Normal"){
      geom_point(aes(y = pnorm(r(),input$p1,input$p2)), color = "black",alpha=.7,size = 2)
    }
    else if(input$dist=="Exponencial"){
      geom_point(aes(y = pexp(r(),input$p1)), color = "black",alpha=.7,size = 2)
    }
    else if(input$dist=="Gamma"){
      geom_point(aes(y = pgamma(r(),input$p1,input$p2)), color = "black",alpha=.7,size = 2)
    }
    else if(input$dist=="Beta"){ #2
      geom_point(aes(y = pbeta(r(),input$p1,input$p2)), color = "black",alpha=.7,size = 2)
    }
    else if(input$dist=="Cauchy"){ #2
      geom_point(aes(y = pcauchy(r(),input$p1,input$p2)), color = "black",alpha=.7,size = 2)
    }
    else if(input$dist=="Chi cuadrada"){ #2
      geom_point(aes(y = pchisq(r(),input$p1,input$p2)), color = "black",alpha=.7,size = 2)
    }
    else if(input$dist=="F"){ #2
      geom_point(aes(y = pf(r(),input$p1,input$p2)), color = "black",alpha=.7,size = 2)
    }
    else if(input$dist=="Log normal"){ #2
      geom_point(aes(y = plnorm(r(),input$p1,input$p2)), color = "black",alpha=.7,size = 2)
    }
    else if(input$dist=="Poisson"){ #1
      geom_point(aes(y = ppois(r(),input$p1)), color = "black",alpha=.7,size = 2)
    }
    else if(input$dist=="t de Student"){ #2
      geom_point(aes(y = pt(r(),input$p1,input$p2)), color = "black",alpha=.7,size = 2)
    }
    else if(input$dist=="Uniforme"){ #2
      geom_point(aes(y = punif(r(),input$p1,input$p2)), color = "black",alpha=.7,size = 2)
    }
    else if(input$dist=="Weibull"){ #2
      geom_point(aes(y = pweibull(r(),input$p1,input$p2)), color = "black",alpha=.7,size = 2)
    }
  })
  
  g3a<-reactive({
    if(input$dist=="Normal"){
      ggplot(data(), aes(x=rr,pnorm(r(),input$p1,input$p2)))
    }
    else if(input$dist=="Exponencial"){
      ggplot(data(), aes(x=rr,pexp(r(),input$p1)))
    }
    else if(input$dist=="Gamma"){
      ggplot(data(), aes(x=rr,pgamma(r(),input$p1,input$p2)))
    }
    else if(input$dist=="Beta"){ #2
      ggplot(data(), aes(x=rr,pbeta(r(),input$p1,input$p2)))
    }
    else if(input$dist=="Cauchy"){ #2
      ggplot(data(), aes(x=rr,pcauchy(r(),input$p1,input$p2)))
    }
    else if(input$dist=="Chi cuadrada"){ #2
      ggplot(data(), aes(x=rr,pchisq(r(),input$p1,input$p2)))
    }
    else if(input$dist=="F"){ #2
      ggplot(data(), aes(x=rr,pf(r(),input$p1,input$p2)))
    }
    else if(input$dist=="Log normal"){ #2
      ggplot(data(), aes(x=rr,plnorm(r(),input$p1,input$p2)))
    }
    else if(input$dist=="Poisson"){ #1
      ggplot(data(), aes(x=rr,ppois(r(),input$p1)))
    }
    else if(input$dist=="t de Student"){ #2
      ggplot(data(), aes(x=rr,pt(r(),input$p1,input$p2)))
    }
    else if(input$dist=="Uniforme"){ #2
      ggplot(data(), aes(x=rr,punif(r(),input$p1,input$p2)))
    }
    else if(input$dist=="Weibull"){ #2
      ggplot(data(), aes(x=rr,pweibull(r(),input$p1,input$p2)))
    }
  })
  
  g3b<-reactive({
    if(input$dist=="Normal"){
      geom_point(aes(y = 1 - pnorm(r(),input$p1,input$p2)), color = "black",alpha=.7,size = 2)
    }
    else if(input$dist=="Exponencial"){
      geom_point(aes(y = 1 - pexp(r(),input$p1)), color = "black",alpha=.7,size = 2)
    }
    else if(input$dist=="Gamma"){
      geom_point(aes(y = 1 - pgamma(r(),input$p1,input$p2)), color = "black",alpha=.7,size = 2)
    }
    else if(input$dist=="Beta"){ #2
      geom_point(aes(y = 1 - pbeta(r(),input$p1,input$p2)), color = "black",alpha=.7,size = 2)
    }
    else if(input$dist=="Cauchy"){ #2
      geom_point(aes(y = 1 - pcauchy(r(),input$p1,input$p2)), color = "black",alpha=.7,size = 2)
    }
    else if(input$dist=="Chi cuadrada"){ #2
      geom_point(aes(y = 1 - pchisq(r(),input$p1,input$p2)), color = "black",alpha=.7,size = 2)
    }
    else if(input$dist=="F"){ #2
      geom_point(aes(y = 1 - pf(r(),input$p1,input$p2)), color = "black",alpha=.7,size = 2)
    }
    else if(input$dist=="Log normal"){ #2
      geom_point(aes(y = 1 - plnorm(r(),input$p1,input$p2)), color = "black",alpha=.7,size = 2)
    }
    else if(input$dist=="Poisson"){ #1
      geom_point(aes(y = 1 - ppois(r(),input$p1)), color = "black",alpha=.7,size = 2)
    }
    else if(input$dist=="t de Student"){ #2
      geom_point(aes(y = 1 - pt(r(),input$p1,input$p2)), color = "black",alpha=.7,size = 2)
    }
    else if(input$dist=="Uniforme"){ #2
      geom_point(aes(y = 1 - punif(r(),input$p1,input$p2)), color = "black",alpha=.7,size = 2)
    }
    else if(input$dist=="Weibull"){ #2
      geom_point(aes(y = 1 - pweibull(r(),input$p1,input$p2)), color = "black",alpha=.7,size = 2)
    }
  })
  
  
  output$g1 <- renderPlot({
    ggplot(data(), aes(x=rr)) + 
      geom_histogram(aes(y=..density..), binwidth=.5, colour="white", fill="gray", alpha = 0.5) +
      labs(y="Densidad", x = "Datos", colour = "NA") +
      ggtitle(paste("Histograma de la distribución",as.character(input$dist),
                    "vs densidad empírica y teórica")) +
      tema +
      geom_area(data=d(),aes(x = x, y = y), color = "turquoise",alpha=.2, fill="turquoise") +
      g1a() +
      geom_vline(aes(xintercept=summary(r())[4]), linetype="dashed", size=1, colour="blue") +
      geom_vline(aes(xintercept=summary(r())[2]), linetype="dashed", size=1, colour="maroon4")+
      geom_vline(aes(xintercept=summary(r())[3]), linetype="dashed", size=1, colour="maroon4")+
      geom_vline(aes(xintercept=summary(r())[5]), linetype="dashed", size=1, colour="maroon4")
  })
  
  output$g2 <- renderPlot({
      g2a() + 
      labs(y="Densidad", x = "Datos", colour = "NA") +
      ggtitle(paste("Función de distribución",as.character(input$dist),"empírica y teórica")) +
      tema +
      geom_line(data=p(),aes(x = x, y = y), color = "turquoise3",size=1) +
      g2b() +
      geom_vline(aes(xintercept=summary(r())[4]), linetype="dashed", size=1, colour="blue") +
      geom_vline(aes(xintercept=summary(r())[2]), linetype="dashed", size=1, colour="maroon4")+
      geom_vline(aes(xintercept=summary(r())[3]), linetype="dashed", size=1, colour="maroon4")+
      geom_vline(aes(xintercept=summary(r())[5]), linetype="dashed", size=1, colour="maroon4")
  })
  
  output$g3 <- renderPlot({
      g3a() + 
      labs(y="Densidad", x = "Datos", colour = "NA") +
      ggtitle(paste("Función de sobrevivencia empírica y teórica de",as.character(input$dist))) +
      tema +
      geom_line(data=s(),aes(x = x, y = y), color = "turquoise3",size=1) +
      g3b() +
      geom_vline(aes(xintercept=summary(r())[4]), linetype="dashed", size=1, colour="blue") +
      geom_vline(aes(xintercept=summary(r())[2]), linetype="dashed", size=1, colour="maroon4")+
      geom_vline(aes(xintercept=summary(r())[3]), linetype="dashed", size=1, colour="maroon4")+
      geom_vline(aes(xintercept=summary(r())[5]), linetype="dashed", size=1, colour="maroon4")
  })
  
  output$g4<-renderPlot({
    ggplot(data(),aes(nn,rr)) + geom_point(size = 3,col="turquoise4") + 
    labs(y="Datos", x = "Índice", colour = "NA") +
    ggtitle(paste("Datos de la distribución",as.character(input$dist))) +
    tema
  })

  output$g5<-renderPlot({
    ggplot(data(), aes(x="d",y=rr)) + geom_boxplot(fill="turquoise", alpha=0.2) +
    stat_summary(fun=mean, geom="point", shape=20, size=6, color="maroon4") + 
      labs(y="Datos", x = as.character(input$dist), colour = "NA") +
      ggtitle(paste("Boxplot",as.character(input$dist))) +
      tema
  })

  output$g6<-renderPlot({
      ggplot(data = data(), mapping = aes(sample = rr)) +
      stat_qq_band(color="turquoise",fill="turquoise", alpha=0.2) +
      stat_qq_line(color="maroon4") +
      stat_qq_point() +
      labs(x = "Cuantiles teóricos", y = "Cuantiles empíricos") +
      ggtitle(paste("Q-Q Plot",as.character(input$dist))) +
      tema
  })
  
  output$t1 <- renderPrint({
    summary(r())
  })
  
}
# Publicación del Tablero ######################################################
shinyApp(ui=ui,server=server)

################################################################################

  