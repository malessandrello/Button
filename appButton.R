library(shiny)
library(bslib)
library(dplyr)
library(readxl)
library(tidyverse)
library(shinyWidgets)
library(shinycssloaders)
library(plotly)
library(flextable)
library(tools)

# Define UI for application that draws a histogram
ui <- page_fluid(
  tags$head(
    tags$style(HTML("

     body{
      background-color: #73B77B;
      padding-top: 70px;
    }"))),

  # Application title
  titlePanel("Análisis de Buttons"),
  fileInput(inputId = "subir", NULL, buttonLabel = "Subir archivo", accept = c(".xls", ".xlsx", ".csv")),
  hr(),
  actionBttn(inputId = "analisis", label = "ANALIZAR"),
  hr(),
  conditionalPanel(condition = "output.xls",
  card(
    card_header(htmlOutput(outputId = "fact_coerr")),
    card_body(
      height = 600,
      withSpinner(plotlyOutput(outputId = "grafico"))
    )
  ),
  hr(),
  layout_columns(
    card(
      card_header("Desvíos por debajo del límite de tolerancia"),
      card_body(uiOutput(outputId = "tabla")),
      card_footer(htmlOutput(outputId = "porcentaje_menor" ))
    ),
    card(
      card_header("Desvíos por encima del límite de tolerancia"),
      card_body(uiOutput(outputId = "tabla2")),
      card_footer(htmlOutput(outputId = "porcentaje_mayor"))
    )
  ),
layout_columns(
  card(
    card_header("Desvíos por debajo del límite de tolerancia mayores a 1 °C"),
    card_body(uiOutput(outputId = "tabla3")),
    card_footer(htmlOutput(outputId = "porcentaje_menor1"))
  ),
  card(
    card_header("Desvíos por encima del límite de tolerancia mayores a 1 °C"),
    card_body(uiOutput(outputId = "tabla4")),
    card_footer(htmlOutput(outputId = "porcentaje_mayor1"))
  )
)),
conditionalPanel(condition = "output.csv",
                 card(
                   card_header(htmlOutput(outputId = "fact_coerr_csv")),
                   card_body(
                     height = 600,
                     withSpinner(plotlyOutput(outputId = "grafico_csv"))
                   )
                 ),
                 hr(),
                 layout_columns(
                   card(
                     card_header("Desvíos por debajo del límite de tolerancia"),
                     card_body(uiOutput(outputId = "tabla_csv")),
                     card_footer(htmlOutput(outputId = "porcentaje_menor_csv" ))
                   ),
                   card(
                     card_header("Desvíos por encima del límite de tolerancia"),
                     card_body(uiOutput(outputId = "tabla2_csv")),
                     card_footer(htmlOutput(outputId = "porcentaje_mayor_csv"))
                   )
                 ),
                 layout_columns(
                   card(
                     card_header("Desvíos por debajo del límite de tolerancia mayores a 1 °C"),
                     card_body(uiOutput(outputId = "tabla3_csv")),
                     card_footer(htmlOutput(outputId = "porcentaje_menor1_csv"))
                   ),
                   card(
                     card_header("Desvíos por encima del límite de tolerancia mayores a 1 °C"),
                     card_body(uiOutput(outputId = "tabla4_csv")),
                     card_footer(htmlOutput(outputId = "porcentaje_mayor1_csv"))
                   )
                 )),

card(
  textInput(inputId = "conc", label = "Conclusiones", value = "Escriba sus conclusiones aquí"),
  downloadBttn("reporte", "Generar reporte")
)
)





server <- function(input, output, session) {
  
    ext1 <- reactive({
  req(input$subir)
      file_ext(input$subir$name)
    
  })
    
 ################################################### XLS ######################################### 
datos_xls <- eventReactive(input$analisis, {  
   if(ext1()== "xlsx" | ext1() == "xls"){
  # LEE EL ARCHIVO QUE ES SUBIDO POR EL USUARIO Y GUARDA LOS DATOS EN LA VARIABLE datos
  datos <- read_xlsx(input$subir$datapath)
  #LEE LA CELDA D14 DE LA HOJA 3 DEL ARCHIVO SUBIDO POR EL USUARIO Y GUARDA EL DATO EN LA VARIABLE button
  button <- read_xlsx(input$subir$datapath, sheet = 3, range = "D13:D14") %>% rename("button" = "...1")
    


  n_button <- as.character(str_match(button$button, ("\\d+")))


  estufas <- as.character(str_match_all(button$button, ("E \\d+|H\\d+|E\\d+")))


  datos_estufas <- switch(estufas,
      "E 655" = tibble(
        factor_correc = -0.3,
        lim_sup = 35.01,
        lim_inf = 34.76
      ),
      "E 656" = tibble(
        factor_correc = -0.2,
        lim_sup = 37.72,
        lim_inf = 36.49
      ),
      "E 654" = tibble(
        factor_correc = -0.4,
        lim_sup = 41.88,
        lim_inf = 41.22
      ),
      "E 1652" = tibble(
        factor_correc = -0.2,
        lim_sup = 29.8,
        lim_inf = 29.31
      ),
      "E410" = tibble(
        factor_correc = 0.5,
        lim_sup = 45,
        lim_inf = 43
      ),
      "E 1682" = tibble(
        factor_correc = 0.3,
        lim_sup = 56,
        lim_inf = 54
      ),
      "E 409" = tibble(
        factor_correc = 0,
        lim_sup = 38,
        lim_inf = 36
      ),
      "E 1683" = tibble(
        factor_correc = -0.4,
        lim_sup = 27,
        lim_inf = 23
      ),
      "E 1594" = tibble(
        factor_correc = 0,
        lim_sup = 62,
        lim_inf = 58
      ),
      "E 1645" = tibble(
        factor_correc = -0.3,
        lim_sup = 38,
        lim_inf = 36
      ),
      "E 0081" = tibble(
        factor_correc = -0.2,
        lim_sup = 31,
        lim_inf = 29
      ),
      "E 1681" = tibble(
        factor_correc = 0.3,
        lim_sup = 45,
        lim_inf = 43
      ),
      "H1684" = tibble(
        factor_correc = 0,
        lim_sup = 7.7,
        lim_inf = 2.3
      ),
      "E 1644" = tibble(
        factor_correc = 1,
        lim_sup = 31,
        lim_inf = 29
      )

    )

  datos <-datos %>% mutate(across(where(is.numeric), ~ round(., digits = 2)))
  

  datos <- datos %>%
      setNames(c("Número", "Fecha", "Temperatura")) %>%
      mutate(Temperatura_corregida = Temperatura + datos_estufas$factor_correc)
  


  max <- datos$Temperatura_corregida[which.max(datos$Temperatura_corregida)]


  min <- datos$Temperatura_corregida[which.min(datos$Temperatura_corregida)]
  

  promedio <-  mean(datos$Temperatura_corregida) %>% round(2)

  desv_inf <- datos %>%
      filter(Temperatura_corregida < datos_estufas$lim_inf) %>%
      mutate(Desvío = Temperatura_corregida - datos_estufas$lim_inf)


  desv_sup <- datos %>%
      filter(Temperatura_corregida > datos_estufas$lim_sup) %>%
      mutate(Desvío = Temperatura_corregida - datos_estufas$lim_sup)

  
  desv_inf1 <-  desv_inf %>% filter(Desvío < -1)
  
  
  desv_sup1 <-desv_sup %>% filter(Desvío > 1)


  percent_menor <- round(nrow(desv_inf) * 100 / nrow(datos), digits = 2)


  percent_mayor <- round(nrow(desv_sup) * 100 / nrow(datos), digits = 2)

  
  percent_menor1 <- round(nrow(desv_inf1) * 100 / nrow(datos), digits = 2)

  
  percent_mayor1 <- round(nrow(desv_sup1) * 100 / nrow(datos), digits = 2)

  
  inicio <- datos$Fecha[1]

  fin <-  datos$Fecha[length(datos$Fecha)]

  
  list(datos = datos, fin = fin, inicio =inicio,
         percent_mayor1 = percent_mayor1, percent_menor1 = percent_menor1, 
         percent_mayor = percent_mayor, percent_menor = percent_menor,
         desv_sup1 = desv_sup1, desv_inf1 = desv_inf1, desv_sup = desv_sup,
         desv_inf = desv_inf, promedio = promedio, min = min, max = max,
         datos_estufas = datos_estufas, n_button = n_button, estufas = estufas)
 } })
  ############################################### CSV #######################################
  datos_csv <- eventReactive(input$analisis, {
    if(ext1() == "csv"){

    datos <- read_csv(input$subir$datapath, locale(encoding = "latin1"))
    
    cols <- datos[2,]
  

    estufas <- as.character(str_match_all(cols, ":\\s.+|2005|488"))
    
    datos_estufas <- switch(estufas,
                            ": E600000041109921" = tibble(
                              factor_correc = 0,
                              lim_inf = 34,
                              lim_sup = 36,
                              n_button = "K",
                              estufas = "Estufa 400"
                            ),
                            ": 0E0000004105FE21" = tibble(
                              factor_correc = 0,
                              lim_inf = 21,
                              lim_sup = 23,
                              n_button = "I",
                              estufas = "Estufa 1918"
                              ),
                            ": BB00000031B37121" = tibble(
                              factor_correc = -0.5,
                              lim_inf = -40,
                              lim_sup = -5,
                              n_button = 14,
                              estufas = "Freezer vertical 580"
                              ),
                            ": BE0000002BEE4721" = tibble(
                              factor_correc = -0.5,
                              lim_inf = -40,
                              lim_sup = -5,
                              n_button = 324,
                              estufas = "Freezer 1650"
                              ),
                            ": 7200000032267E21" = tibble(
                              factor_correc = 0,
                              lim_inf = 2,
                              lim_sup = 8,
                              n_button = "C",
                              estufas = "Heladera 191"
                            ),
                            ": 0F00000031B72621" = tibble(
                              factor_correc = 0,
                              lim_inf = 2,
                              lim_sup = 8,
                              n_button = 13,
                              estufas = "Heladera 192"
                              ),
                            ": AE00000032255121" = tibble(
                              factor_correc = 0,
                              lim_inf = 2,
                              lim_sup = 8,
                              n_button = "D",
                              estufas = "Heladera 1650"
                              ),
                            "2005" = tibble(
                              factor_correc = 0,
                              lim_inf = 1,
                              lim_sup = 5,
                              n_button = "Sonda",
                              estufas = "Cámara 2005"
                            
                            ),
                            "488" = tibble(
                              factor_correc = 0,
                              lim_inf = -80,
                              lim_sup = -50,
                              n_button = "Sonda",
                              estufas = "Ultrafreezer 488"
                            ),
                            ": 7000000032430D21" = tibble(
                              factor_correc = 0.5,
                              lim_inf = 43,
                              lim_sup = 45,
                              n_button = "B",
                              estufas = "Estufa 410"
                            ),
                            ": 8F0000002BD72021" = tibble(
                              factor_correc = 0,
                              lim_inf = 36,
                              lim_sup = 38,
                              n_button = "325",
                              estufas = "Estufa 409"
                            ),
                            ": 7E0000004117A621" = tibble(
                              factor_correc = 1,
                              lim_inf = 29,
                              lim_sup = 31,
                              n_button = "J",
                              estufas = "Estufa 1644"
                            ),
                            ": C500000037D8DC21" = tibble(
                              factor_correc = 0,
                              lim_inf = 2,
                              lim_sup = 8,
                              n_button = "327",
                              estufas = "Heladera 1684"
                            ) 
    )
    datos1 <- if(estufas == "2005" | estufas == "488"){ datos %>%
      slice(-(1:2)) %>%
        separate_wider_delim(cols = names(datos), names = c("Fecha", "unidad", "temp", "dec"), delim = ",", too_few = "align_start") %>%
        unite(col = "Temperatura", temp:dec, sep = ".", remove = FALSE, na.rm = TRUE) %>%
        select(Fecha, Temperatura) %>%
        mutate(Fecha =mdy_hms(Fecha), Temperatura = as.double(Temperatura)) %>% drop_na()
      
    }else{ datos %>%
      slice(-(1:12)) %>%
      separate_wider_delim(cols = names(datos), names = c("Fecha", "unidad", "temp", "dec"), delim = ",", too_few = "align_start") %>%
      slice(-1) %>%
      unite(col = "Temperatura", temp:dec, sep = ".", remove = FALSE, na.rm = TRUE) %>%
      select(Fecha, Temperatura) %>%
      mutate(Fecha = dmy_hms(Fecha), Temperatura = as.double(Temperatura)) %>% drop_na()
    }
    
    datos1 <-datos1 %>% mutate(across(where(is.numeric), ~ round(., digits = 2)))
    
    
    datos1 <- datos1 %>%
      mutate(Temperatura_corregida = Temperatura + datos_estufas$factor_correc)
    
    
    
    max <- datos1$Temperatura_corregida[which.max(datos1$Temperatura_corregida)]
    
    
    min <- datos1$Temperatura_corregida[which.min(datos1$Temperatura_corregida)]
    
    
    promedio <-  mean(datos1$Temperatura_corregida) %>% round(2)
    
    desv_inf <- datos1 %>%
      filter(Temperatura_corregida < datos_estufas$lim_inf) %>%
      mutate(Desvío = Temperatura_corregida - datos_estufas$lim_inf)
    
    
    desv_sup <- datos1 %>%
      filter(Temperatura_corregida > datos_estufas$lim_sup) %>%
      mutate(Desvío = Temperatura_corregida - datos_estufas$lim_sup)
    
    
    desv_inf1 <-  desv_inf %>% filter(Desvío < -1)
    
    
    desv_sup1 <-desv_sup %>% filter(Desvío > 1)
    
    
    percent_menor <- round(nrow(desv_inf) * 100 / nrow(datos1), digits = 2)
    
    
    percent_mayor <- round(nrow(desv_sup) * 100 / nrow(datos1), digits = 2)
    
    
    percent_menor1 <- round(nrow(desv_inf1) * 100 / nrow(datos1), digits = 2)
    
    
    percent_mayor1 <- round(nrow(desv_sup1) * 100 / nrow(datos1), digits = 2)
    
    
    inicio <- if(estufas == "2005" | estufas == "488"){
      datos1$Fecha[length(datos1$Fecha)]
    }else{
      datos1$Fecha[1]
    }
    
    fin <- if(estufas == "2005" | estufas == "488"){
      datos1$Fecha[1]
    } else{
      datos1$Fecha[length(datos1$Fecha)]
    }
    
    list(datos = datos1, fin = fin, inicio =inicio,
         percent_mayor1 = percent_mayor1, percent_menor1 = percent_menor1, 
         percent_mayor = percent_mayor, percent_menor = percent_menor,
         desv_sup1 = desv_sup1, desv_inf1 = desv_inf1, desv_sup = desv_sup,
         desv_inf = desv_inf, promedio = promedio, min = min, max = max,
         datos_estufas = datos_estufas, n_button = datos_estufas$n_button, estufas = datos_estufas$estufas)
 } })
    
    
  
  conclusiones <- reactive({
    HTML(input$conc)
  })

  output$grafico <- renderPlotly({
    req(input$analisis)
    p <- datos_xls()$datos %>% ggplot(aes(.data[["Fecha"]], .data[["Temperatura_corregida"]])) +
      geom_point() +
      geom_hline(yintercept = datos_xls()$datos_estufas$lim_sup) +
      geom_hline(yintercept = datos_xls()$datos_estufas$lim_inf) +
      xlab("Fecha") +
      ylab("Temperatura (°C)")
    ggplotly(p, height = 500, dynamicTicks = TRUE)
  })

  output$fact_coerr <- renderUI(HTML(paste0(
    "<b>EQUIPO: </b>", datos_xls()$estufas, "</br>",
    "<b>BUTTON: </b>", datos_xls()$n_button, "</br>",
    "<b>FACTOR DE CORRECCIÓN: </b>", datos_xls()$datos_estufas$factor_correc, "</br>",
    "<b>TEMPERATURA DE TRABAJO: </b>", datos_xls()$datos_estufas$lim_inf, " °C ", " - ",
    datos_xls()$datos_estufas$lim_sup, " °C", "</br>",
    "<b>INICIO DE REGISTRO: </b>", datos_xls()$inicio, "</br>",
    "<b>FIN DE REGISTRO: </b>", datos_xls()$fin, "</br>",
    "<b>TEMPERATURA MÁXIMA: </b>", datos_xls()$max, " °C", "</br>",
    "<b>TEMPERATURA MÍNIMA: </b>", datos_xls()$min, " °C", "</br>",
    "<b>TEMPERATURA PROMEDIO: </b>", datos_xls()$promedio, " °C"
  )))

  output$tabla <- renderUI({
    req(input$analisis)
    datos_xls()$desv_inf %>%
      flextable() %>%
      autofit() %>%
      htmltools_value()
  })
  output$tabla2 <- renderUI({
    req(input$analisis)
    datos_xls()$desv_sup %>%
      flextable() %>%
      autofit() %>%
      htmltools_value()
  })
  
  output$tabla3 <- renderUI({
    req(input$analisis)
    datos_xls()$desv_inf1 %>%
      flextable() %>%
      autofit() %>%
      htmltools_value()
  })
  
  output$tabla4 <- renderUI({
    req(input$analisis)
    datos_xls()$desv_sup1 %>%
      flextable() %>%
      autofit() %>%
      htmltools_value()
  })
  output$porcentaje_menor <- renderUI({
    req(input$analisis)
    HTML(paste0("<b>Porcentaje de desvíos: </b>", datos_xls()$percent_menor, " %"))
  })
  output$porcentaje_mayor <- renderUI({
    req(input$analisis)
    HTML(paste0("<b>Porcentaje de desvíos: </b>", datos_xls()$percent_mayor, " %"))
  })
  output$porcentaje_menor1 <- renderUI({
    req(input$analisis)
    HTML(paste0("<b>Porcentaje de desvíos mayores a 1 °C por debajo del límite de tolerancia: </b>", datos_xls()$percent_menor1, " %"))
  })
  output$porcentaje_mayor1 <- renderUI({
    req(input$analisis)
    HTML(paste0("<b>Porcentaje de desvíos mayores a 1 °C por encima del límite de tolerancia: </b>", datos_xls()$percent_mayor1, " %"))
  })
  
  ############################################# CSV ############################################################
  
  output$grafico_csv <- renderPlotly({
    req(input$analisis)
    p <- datos_csv()$datos %>% ggplot(aes(.data[["Fecha"]], .data[["Temperatura_corregida"]])) +
      geom_point() +
      geom_hline(yintercept = datos_csv()$datos_estufas$lim_sup) +
      geom_hline(yintercept = datos_csv()$datos_estufas$lim_inf) +
      xlab("Fecha") +
      ylab("Temperatura (°C)")
    ggplotly(p, height = 500, dynamicTicks = TRUE)
  })
  
  output$fact_coerr_csv <- renderUI(HTML(paste0(
    "<b>EQUIPO: </b>", datos_csv()$datos_estufas$estufas, "</br>",
    "<b>BUTTON: </b>", datos_csv()$datos_estufas$n_button, "</br>",
    "<b>FACTOR DE CORRECCIÓN: </b>", datos_csv()$datos_estufas$factor_correc, "</br>",
    "<b>TEMPERATURA DE TRABAJO: </b>", datos_csv()$datos_estufas$lim_inf, " °C ", " - ",
    datos_csv()$datos_estufas$lim_sup, " °C", "</br>",
    "<b>INICIO DE REGISTRO: </b>", datos_csv()$inicio, "</br>",
    "<b>FIN DE REGISTRO: </b>", datos_csv()$fin, "</br>",
    "<b>TEMPERATURA MÁXIMA: </b>", datos_csv()$max, " °C", "</br>",
    "<b>TEMPERATURA MÍNIMA: </b>", datos_csv()$min, " °C", "</br>",
    "<b>TEMPERATURA PROMEDIO: </b>", datos_csv()$promedio, " °C"
  )))
  
  output$tabla_csv <- renderUI({
    req(input$analisis)
    datos_csv()$desv_inf %>%
      flextable() %>%
      autofit() %>%
      htmltools_value()
  })
  output$tabla2_csv <- renderUI({
    req(input$analisis)
    datos_csv()$desv_sup %>%
      flextable() %>%
      autofit() %>%
      htmltools_value()
  })
  
  output$tabla3_csv <- renderUI({
    req(input$analisis)
    datos_csv()$desv_inf1 %>%
      flextable() %>%
      autofit() %>%
      htmltools_value()
  })
  
  output$tabla4_csv <- renderUI({
    req(input$analisis)
    datos_csv()$desv_sup1 %>%
      flextable() %>%
      autofit() %>%
      htmltools_value()
  })
  output$porcentaje_menor_csv <- renderUI({
    req(input$analisis)
    HTML(paste0("<b>Porcentaje de desvíos: </b>", datos_csv()$percent_menor, " %"))
  })
  output$porcentaje_mayor_csv <- renderUI({
    req(input$analisis)
    HTML(paste0("<b>Porcentaje de desvíos: </b>", datos_csv()$percent_mayor, " %"))
  })
  output$porcentaje_menor1_csv <- renderUI({
    req(input$analisis)
    HTML(paste0("<b>Porcentaje de desvíos mayores a 1 °C por debajo del límite de tolerancia: </b>", datos_csv()$percent_menor1, " %"))
  })
  output$porcentaje_mayor1_csv <- renderUI({
    req(input$analisis)
    HTML(paste0("<b>Porcentaje de desvíos mayores a 1 °C por encima del límite de tolerancia: </b>", datos_csv()$percent_mayor1, " %"))
  })
  
  output$xls <- renderUI({
    if(ext1() == "xlsx" | ext1() == "xls"){
      HTML(paste0(
        "<b>", "TIPO DE ARCHIVO: EXCEL", "</b>"
      ))
    }
  })
  
  output$csv <- renderUI({
  if(ext1() == "csv"){
      HTML(paste0(
        "<b>", "TIPO DE ARCHIVO: CSV", "</b>"
      ))
  }
  })
  
  output$reporte <- downloadHandler( filename = "reporte.html",
                                     content = function (file){
                                       tempReport <- file.path(tempdir(), "reporte.Rmd")
                                       file.copy("reporte.Rmd", tempReport, overwrite = TRUE)  
                                        if(ext1() == "xlsx" | ext1() == "xls") {
                                                      
                                         params <- list(data = datos_xls()$datos,
                                                      max = datos_xls()$max,
                                                      min = datos_xls()$min,
                                                      promedio =  datos_xls()$promedio,
                                                      desv_inf = datos_xls()$desv_inf,
                                                      desv_sup = datos_xls()$desv_sup, 
                                                      desv_inf1 = datos_xls()$desv_inf1,
                                                      desv_sup1 = datos_xls()$desv_sup1,
                                                      percent_mayor = datos_xls()$percent_mayor,
                                                      percent_menor = datos_xls()$percent_menor,
                                                      percent_menor1 = datos_xls()$percent_menor1,
                                                      percent_mayor1 = datos_xls()$percent_mayor1,
                                                      inicio = datos_xls()$inicio,
                                                      fin = datos_xls()$fin,
                                                      estufas = datos_xls()$estufas,
                                                      datos_estufas = datos_xls()$datos_estufas,
                                                      n_button = datos_xls()$n_button,
                                                      conclusiones = conclusiones())
                                         
                                       }else if(ext1() == "csv"){

                                              params <- list(data = datos_csv()$datos,
                                                        max = datos_csv()$max,
                                                        min = datos_csv()$min,
                                                        promedio =  datos_csv()$promedio,
                                                        desv_inf = datos_csv()$desv_inf,
                                                        desv_sup = datos_csv()$desv_sup,
                                                        desv_inf1 = datos_csv()$desv_inf1,
                                                        desv_sup1 = datos_csv()$desv_sup1,
                                                        percent_mayor = datos_csv()$percent_mayor,
                                                        percent_menor = datos_csv()$percent_menor,
                                                        percent_menor1 = datos_csv()$percent_menor1,
                                                        percent_mayor1 = datos_csv()$percent_mayor1,
                                                        inicio = datos_csv()$inicio,
                                                        fin = datos_csv()$fin,
                                                        estufas = datos_csv()$datos_estufas$estufas,
                                                        datos_estufas = datos_csv()$datos_estufas,
                                                        n_button = datos_csv()$datos_estufas$n_button,
                                                        conclusiones = conclusiones())
                                       }
                                       
                                       
                                       rmarkdown::render(tempReport, output_file = file,
                                                         params = params,
                                                         envir = new.env(parent = globalenv()))

                                                                          })
  outputOptions(output, "xls", suspendWhenHidden = FALSE)
  outputOptions(output, "csv", suspendWhenHidden = FALSE)
}




# Run the application
shinyApp(ui = ui, server = server)
