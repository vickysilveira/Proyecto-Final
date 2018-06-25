library(shiny)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("simplex"),
  titlePanel("Turismo Receptivo 2015 Uruguay"),
  tabsetPanel(
    tabPanel("Cantidad de turistas",navlistPanel(
      "Segun:",
      tabPanel("Destino",
               plotOutput("plot.2015.d.ct")),
      tabPanel("Alojamiento",
               plotOutput("plot.2015.a.ct")),
      tabPanel("Nacionalidad",
               plotOutput("plot.2015.n.ct")),
      tabPanel("Motivo del viaje",
               plotOutput("plot.2015.m.ct")),
      tabPanel("Transporte",
               plotOutput("plot.2015.t.ct"))
    )),
   
    tabPanel("Dias de Hospedaje",navlistPanel(
      "Segun:",
      tabPanel("Destino",
               plotOutput("plot.2015.d.d")),
      tabPanel("Alojamiento",
               plotOutput("plot.2015.a.d")),
      tabPanel("Nacionalidad",
               plotOutput("plot.2015.n.d")),
      tabPanel("Motivo del viaje",
               plotOutput("plot.2015.m.d")),
      tabPanel("Transporte",
               plotOutput("plot.2015.t.d"))
    )),
    
    tabPanel("Gastos",navlistPanel(
      "Segun:",
      tabPanel("Destino",
               plotOutput("plot.2015.d.g.g"),
               plotOutput("plot.2015.d.g.gp"),
               plotOutput("plot.2015.d.g.gdp")),
      tabPanel("Alojamiento",
               plotOutput("plot.2015.a.g.g"),
               plotOutput("plot.2015.a.g.gp"),
               plotOutput("plot.2015.a.g.gdp")),
      tabPanel("Nacionalidad",
               plotOutput("plot.2015.n.g.g"),
               plotOutput("plot.2015.n.g.gp"),
               plotOutput("plot.2015.n.g.gdp")),
      tabPanel("Motivo del viaje",
               plotOutput("plot.2015.m.g.g"),
               plotOutput("plot.2015.m.g.gp"),
               plotOutput("plot.2015.m.g.gdp")),
      tabPanel("Transporte",
               plotOutput("plot.2015.t.g.g"),
               plotOutput("plot.2015.t.g.gp"),
               plotOutput("plot.2015.t.g.gdp"))
    ))
  )
)
  


server <- function(input, output){
 #Destino
  output$plot.2015.d.ct<- renderPlot({d.por.p %>% ggplot() + 
      geom_col(aes(x=fct_reorder(Destino,value, .desc = TRUE), y= value,fill=Destino)) + 
      labs(x = "Zona de destino", y = "Poroporción de turistas") + 
      theme_classic() +
      theme(legend.position="none", axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0))})
  
  output$plot.2015.d.d<- renderPlot({destino2015%>% ggplot() + 
      theme_classic()+
      geom_col(aes(x=fct_reorder(Destino,dias, .desc = TRUE),dias,fill=Destino)) +
      labs(x = "Zona de destino") +
      theme(legend.position="none", axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0))})
  
  output$plot.2015.d.g.g<- renderPlot({destino2015%>% ggplot() + 
      geom_col(aes(x=fct_reorder(Destino,Gastos, .desc = TRUE),Gastos,fill=Destino)) +
      theme_classic()+ 
      theme(legend.position="none" , axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0)) +
      labs(x = "Zona de destino", y="Gastos Totales (moneda Dolar)")})
  
  output$plot.2015.d.g.gp<- renderPlot({destino2015%>% ggplot() + 
      geom_col(aes(x=fct_reorder(Destino,Gastos_P, .desc = TRUE),Gastos_P,fill=Destino)) + 
      theme_classic() + 
      theme(legend.position="none", axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0)) +
      labs(x = "Zona de destino", y="Gastos Por Persona (moneda Dolar)")})
  
  output$plot.2015.d.g.gdp<- renderPlot({destino2015%>% ggplot() + 
      geom_col(aes(x=fct_reorder(Destino,Gastos_PDia, .desc = TRUE),Gastos_PDia,fill=Destino)) +
      theme_classic() + 
      theme(legend.position="none", axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0)) +
      labs(x = "Zona de destino", y="Gastos Por Persona Por Dia (moneda Dolar)")})
  
  #Alojamiento
  
  output$plot.2015.a.ct<- renderPlot({a.por.p %>% ggplot() + 
      geom_col(aes(x=fct_reorder(ALOJA,value, .desc = TRUE),value,fill=ALOJA)) +
      labs(x = "Alojamiento utilizado", y = "Proporción de visitantes") + 
      theme_classic() + 
      theme(legend.position = "none", axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0))})
  
  output$plot.2015.a.d<- renderPlot({aloja2015%>% ggplot() + 
      geom_col(aes(x=fct_reorder(ALOJA,dias, .desc = TRUE),dias,fill=ALOJA)) +
      theme_classic() + 
      theme(legend.position = "none", axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0)) + 
      labs(x="Alojamiento utilizado")})
  
  output$plot.2015.a.g.g<- renderPlot({aloja2015%>% ggplot() + 
      geom_col(aes(x=fct_reorder(ALOJA,Gastos, .desc = TRUE),Gastos,fill=ALOJA)) +
      theme_classic() + 
      theme(legend.position = "none", axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0)) + 
      labs(x="Alojamiento utilizado", y="Gastos Totales (moneda Dolar)")})
  
  output$plot.2015.a.g.gp<- renderPlot({aloja2015%>% ggplot() + 
      geom_col(aes(x=fct_reorder(ALOJA,Gastos_P, .desc = TRUE),Gastos_P,fill=ALOJA)) +
      theme_classic() + 
      theme(legend.position = "none", axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0)) + 
      labs(x="Alojamiento utilizado", y="Gastos por persona (moneda Dolar)")})
  
  output$plot.2015.a.g.gdp<- renderPlot({aloja2015%>% ggplot() + 
      geom_col(aes(x=fct_reorder(ALOJA,Gastos_PDia, .desc = TRUE),Gastos_PDia,fill=ALOJA)) +
      theme_classic() + 
      theme(legend.position = "none", axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0)) + 
      labs(x="Alojamiento utilizado", y="Gastos por persona por dia (moneda Dolar)")})
  
  #Nacionalidad
  
  output$plot.2015.n.ct<- renderPlot({n.por.p %>% ggplot() + 
      geom_col(aes(x=fct_reorder(NACIONALIDAD,value, .desc = TRUE), y= value, fill = NACIONALIDAD)) +
      labs(x = "Nacionalidad", y = "Proporción de visitantes") + 
      theme_classic() +
      theme(legend.position="none", axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0))})
  
  output$plot.2015.n.d<- renderPlot({nacionalidad2015%>% ggplot() + 
      geom_col(aes(x=fct_reorder(NACIONALIDAD,dias, .desc = TRUE),dias,fill=NACIONALIDAD)) + 
      theme_classic() + 
      labs(x="Nacionalidad") +
      theme(legend.position = "none", axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0))})
  
  output$plot.2015.n.g.g<- renderPlot({nacionalidad2015%>% ggplot() + 
      geom_col(aes(x=fct_reorder(NACIONALIDAD,Gastos, .desc = TRUE),Gastos,fill=NACIONALIDAD)) +
      theme_classic() + 
      theme(legend.position = "none", axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0)) + 
      labs(x="Nacionalidad") +
      labs(y="Gastos Totales (moneda Dolar)")})
  
  output$plot.2015.n.g.gp<- renderPlot({nacionalidad2015%>% ggplot() + 
      geom_col(aes(x=fct_reorder(NACIONALIDAD,Gastos_P, .desc = TRUE),Gastos_P,fill=NACIONALIDAD)) + 
      theme_classic() + 
      theme(legend.position = "none", axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0)) + 
      labs(x="Nacionalidad") +
      labs(y="Gastos por persona (moneda Dolar)")})
  
  output$plot.2015.n.g.gdp<- renderPlot({nacionalidad2015%>% ggplot() + 
      geom_col(aes(x=fct_reorder(NACIONALIDAD,Gastos_PDia, .desc = TRUE),Gastos_PDia,fill=NACIONALIDAD)) +
      theme_classic() + 
      theme(legend.position = "none", axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0)) + 
      labs(x="Nacionalidad") +
      labs(y="Gastos por persona por dia (moneda Dolar)")})
  
  #Motivo del viaje
  
  output$plot.2015.m.ct<- renderPlot({m.por.p %>% ggplot() + 
      geom_col(aes(x=fct_reorder(Motivo,value, .desc = TRUE), y= value, fill=Motivo)) +
      labs(x = "Motivo de ingreso al país", y = "Proporción de turistas")+ 
      theme_classic() + 
      theme(legend.position="none", axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0))})
  
  output$plot.2015.m.d<- renderPlot({motivoingreso%>% ggplot() + 
      geom_col(aes(x=fct_reorder(Motivo,dias, .desc = TRUE),dias,fill=Motivo)) + 
      theme_classic() + 
      theme(legend.position = "none", axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0)) + 
      labs(x = "Motivo de ingreso al país")})
  
  output$plot.2015.m.g.g<- renderPlot({motivoingreso%>% ggplot() + 
      geom_col(aes(x=fct_reorder(Motivo,Gastos, .desc = TRUE),Gastos,fill=Motivo)) +
      theme_classic() + 
      theme(legend.position = "none", axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0)) + 
      labs(x = "Motivo de ingreso al país",y="Gastos Totales (moneda Dolar)")})
  
  output$plot.2015.m.g.gp<- renderPlot({motivoingreso%>% ggplot() + 
      geom_col(aes(x=fct_reorder(Motivo,Gastos_P, .desc = TRUE),Gastos_P,fill=Motivo)) +
      theme_classic() + 
      theme(legend.position = "none", axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0)) + 
      labs(x = "Motivo de ingreso al país",y="Gastos por persona (moneda Dolar)")})
  
  output$plot.2015.m.g.gdp<- renderPlot({motivoingreso%>% ggplot() + 
      geom_col(aes(x=fct_reorder(Motivo,Gastos_PDia, .desc = TRUE),Gastos_PDia,fill=Motivo)) +  
      theme_classic() + 
      theme(legend.position = "none") + 
      theme(axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0)) + 
      labs(x = "Motivo de ingreso al país",y="Gastos por persona por dia (moneda Dolar)")})
  
  #Transporte
  
  output$plot.2015.t.ct<- renderPlot({t.por.p %>% ggplot() + 
      geom_col(aes(x=fct_reorder(TransIn,value, .desc = TRUE), y= value, fill=TransIn)) +
      theme_classic() +
      theme(legend.position="none", axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0)) +
      labs(x = "Transporte mediante el cual ingreso al pais", y = "Proporción de visitantes")})
  
  output$plot.2015.t.d<- renderPlot({movilingreso%>% ggplot() + 
      geom_col(aes(x=fct_reorder(TransIn,dias, .desc = TRUE),dias,fill=TransIn)) + 
      theme_classic() + 
      theme(legend.position = "none", axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0)) + 
      labs(x = "Transporte mediante el cual ingreso al país")})
  
  output$plot.2015.t.g.g<- renderPlot({movilingreso%>% ggplot() + 
      geom_col(aes(x=fct_reorder(TransIn,Gastos, .desc = TRUE),Gastos,fill=TransIn)) + 
      theme_classic() + 
      theme(legend.position = "none", axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0)) + 
      labs(x = "Transporte mediante el cual ingreso al país",y="Gastos Totales (moneda Dolar)")})
  
  output$plot.2015.t.g.gp<- renderPlot({movilingreso%>% ggplot() + 
      geom_col(aes(x=fct_reorder(TransIn,Gastos_P, .desc = TRUE),Gastos_P,fill=TransIn)) +
      theme_classic() + 
      theme(legend.position = "none", axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0)) + 
      labs(x = "Transporte mediante el cual ingreso al pais",y="Gastos por persona (moneda Dolar)")})
  
  output$plot.2015.t.g.gdp<- renderPlot({movilingreso%>% ggplot() + 
      geom_col(aes(x=fct_reorder(TransIn,Gastos_PDia, .desc = TRUE),Gastos_PDia,fill=TransIn)) +  
      theme_classic() + 
      theme(legend.position = "none") + 
      theme(axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0)) + 
      labs(x = "Transporte mediante el cual ingreso al pais",y="Gastos por persona por dia (moneda Dolar)")})


}

shinyApp(ui = ui, server = server)

