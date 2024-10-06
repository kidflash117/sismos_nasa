  library(shiny)
  library(shinythemes)
  library(ggplot2)
  library(seewave)
  library(signal)
  
  ui <- fluidPage(
    theme = shinytheme("cerulean"),
    titlePanel("Visualización de Sismos: Lunares y de Marte"),
    
    sidebarLayout(
      sidebarPanel(
        h3("Seleccione un tipo de sismo"),
        fluidRow(
          column(6, actionButton("lunar", "Sismos Lunares", icon = icon("moon"), class = "btn btn-primary")),
          column(6, actionButton("marte", "Sismos de Marte", icon = icon("globe"), class = "btn btn-success"))
        ),
        br(), br(),
        conditionalPanel(
          condition = "output.tipo_sismo == 'lunar'",
          h4("Datos de sismos lunares"),
          selectInput("muestra_lunar", "Seleccionar Muestra", choices = 1:7),
          uiOutput("stats_lunar")
        ),
        conditionalPanel(
          condition = "output.tipo_sismo == 'marte'",
          h4("Datos de sismos de marte"),
          selectInput("muestra_marte", "Seleccionar Muestra", choices = 1:2),
          uiOutput("stats_marte")
        )
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Sismograma Lunar", plotOutput("sismograma_lunar")),
          tabPanel("Espectrograma Lunar", plotOutput("espectrograma_lunar")),
          tabPanel("Sismograma de Marte", plotOutput("sismograma_marte")),
          tabPanel("Espectrograma de Marte", plotOutput("espectrograma_marte"))
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    
    tipo_sismo <- reactiveVal(NULL)
    
    observeEvent(input$lunar, {
      tipo_sismo("lunar")
      showModal(modalDialog(
        title = "Sismos Lunares Seleccionado",
        "Cargando el archivo CSV para los sismos lunares automáticamente.",
        easyClose = TRUE
      ))
    })
    
    observeEvent(input$marte, {
      tipo_sismo("marte")
      showModal(modalDialog(
        title = "Sismos de Marte Seleccionado",
        "Cargando el archivo CSV para los sismos de marte automáticamente.",
        easyClose = TRUE
      ))
    })
    
    output$tipo_sismo <- reactive({
      tipo_sismo()
    })
    outputOptions(output, "tipo_sismo", suspendWhenHidden = FALSE)
    
    cargar_datos <- function(file) {
      datos <- read.csv(file$datapath)
      colnames(datos) <- make.names(colnames(datos))  # Limpia los nombres de columnas
      datos
    }
    
    data_lunar <- reactive({
      req(tipo_sismo() == "lunar")
      cargar_datos(list(datapath = "data_frame_7sismos.csv"))
    })
    
    stats_lunar <- reactive({
      req(data_lunar())
      df_muestra <- subset(data_lunar(), Muestra == input$muestra_lunar)
      velocity <- df_muestra$velocity.m.s
      time <- df_muestra$time_rel.sec
      list(
        Maximo_Velocidad = max(velocity, na.rm = TRUE),
        Minimo_Velocidad = min(velocity, na.rm = TRUE),
        Media_Velocidad = mean(velocity, na.rm = TRUE),
        Cuartiles_Velocidad = quantile(velocity, na.rm = TRUE),
        Maximo_Tiempo = max(time, na.rm = TRUE),
        Minimo_Tiempo = min(time, na.rm = TRUE),
        Media_Tiempo = mean(time, na.rm = TRUE),
        Cuartiles_Tiempo = quantile(time, na.rm = TRUE)
      )
    })
    
    detectar_inicios_sismos_lunar <- reactive({
      req(data_lunar())
      df_muestra <- subset(data_lunar(), Muestra == input$muestra_lunar)
      velocity <- df_muestra$velocity.m.s
      velocity <- velocity[is.finite(velocity)]
      
      # Calcular la potencia y ajustar el umbral
      potencia <- velocity^2
      threshold <- 0.5 * max(potencia, na.rm = TRUE)  # Ajusta este valor según sea necesario
      
      # Detectar inicios de sismos
      inicio_sismos <- which(potencia > threshold)
      return(inicio_sismos)
    })
    
    output$stats_lunar <- renderUI({
      stats <- stats_lunar()
      stats_formatted <- sapply(stats, function(x) {
        if (is.vector(x)) {
          return(gsub("\\[|\\]|\\$|\\s+", "", paste(x, collapse = ", ")))  # Eliminar corchetes y símbolos
        }
        return(as.character(x))
      })
      
      results <- paste0("<div style='text-align: left;'>",
                        paste(names(stats_formatted), stats_formatted, sep = ": ", collapse = "<br/>"),
                        "</div>")
      HTML(results)
    })
    
    # Renderiza el gráfico  
    output$sismograma_lunar <- renderPlot({  
      req(data_lunar())  
      df_muestra <- subset(data_lunar(), Muestra == input$muestra_lunar)
      
      ggplot(df_muestra, aes(x = time_rel.sec, y = velocity.m.s)) +  
        geom_line(color = "blue") +  
        geom_point(data = df_muestra[detectar_inicios_sismos_lunar(),], aes(x = time_rel.sec, y = velocity.m.s), color = "red", size = 2, shape = 21) + 
        labs(title = "Sismograma Lunar",  
             x = "Tiempo Relativo (s)",  
             y = "Velocidad (m/s)") +  
        theme_minimal() +  
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
        geom_point(aes(color = "Sismos detectados"), size = 2)
    })
    
    output$espectrograma_lunar <- renderPlot({
      req(data_lunar())
      df_muestra <- subset(data_lunar(), Muestra == input$muestra_lunar)
      signal <- df_muestra$velocity.m.s
      fs <- 1 / mean(diff(df_muestra$time_rel.sec))
      
      spec <- spectro(signal, f = fs, wl = 512, plot = FALSE)
      
      color_palette <- colorRampPalette(c("blue", "cyan", "yellow", "red"))
      
      image(spec$time, spec$freq, t(spec$amp), col = color_palette(256), 
            xlab = "Tiempo (s)", ylab = "Frecuencia (Hz)", main = "Espectrograma Lunar")
      
      abline(v = df_muestra$time_rel.sec[detectar_inicios_sismos_lunar()], col = "red", lty = 2)
      
      text(x = max(spec$time), y = max(spec$freq), labels = "Sismos detectados", col = "red", pos = 3)
      
      # Agregar leyenda
      legend("topright", legend = c("Sismos detectados"), col = "red", pch = 19, cex = 0.8, bty = "n")
    })
    
    data_marte <- reactive({
      req(tipo_sismo() == "marte")
      cargar_datos(list(datapath = "XB.ELYSE.02.BHV.2022-01-02HR04_evid0006.csv"))
    })
    
    stats_marte <- reactive({
      req(data_marte())
      df_muestra <- subset(data_marte(), Muestra == input$muestra_marte)
      velocity <- df_muestra$velocity.m.s
      time <- df_muestra$time_rel.sec
      list(
        Maximo_Velocidad = max(velocity, na.rm = TRUE),
        Minimo_Velocidad = min(velocity, na.rm = TRUE),
        Media_Velocidad = mean(velocity, na.rm = TRUE),
        Cuartiles_Velocidad = quantile(velocity, na.rm = TRUE),
        Maximo_Tiempo = max(time, na.rm = TRUE),
        Minimo_Tiempo = min(time, na.rm = TRUE),
        Media_Tiempo = mean(time, na.rm = TRUE),
        Cuartiles_Tiempo = quantile(time, na.rm = TRUE)
      )
    })
    
    detectar_inicios_sismos_marte <- reactive({
      req(data_marte())
      df_muestra <- subset(data_marte(), Muestra == input$muestra_marte)
      velocity <- df_muestra$velocity.m.s
      velocity <- velocity[is.finite(velocity)]
      
      potencia <- velocity^2
      threshold <- 0.5 * max(potencia, na.rm = TRUE)  
      
      inicio_sismos <- which(potencia > threshold)
      return(inicio_sismos)
    })
    
    output$stats_marte <- renderUI({
      stats <- stats_marte()
      stats_formatted <- sapply(stats, function(x) {
        if (is.vector(x)) {
          return(gsub("\\[|\\]|\\$|\\s+", "", paste(x, collapse = ", ")))  
        }
        return(as.character(x))
      })
      
      results <- paste0("<div style='text-align: left;'>",
                        paste(names(stats_formatted), stats_formatted, sep = ": ", collapse = "<br/>"),
                        "</div>")
      HTML(results)
    })
    
    output$sismograma_marte <- renderPlot({
      req(data_marte())  
      df_muestra <- subset(data_marte(), Muestra == input$muestra_marte)
      
      ggplot(df_muestra, aes(x = time_rel.sec, y = velocity.m.s)) +  
        geom_line(color = "blue") +  
        geom_point(data = df_muestra[detectar_inicios_sismos_marte(),], aes(x = time_rel.sec, y = velocity.m.s), color = "red", size = 2, shape = 21) + 
        labs(title = "Sismograma de Marte",  
             x = "Tiempo Relativo (s)",  
             y = "Velocidad (m/s)") +  
        theme_minimal() +  
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
        geom_point(aes(color = "Sismos detectados"), size = 2)
    })
    
    output$espectrograma_marte <- renderPlot({
      req(data_marte())
      df_muestra <- subset(data_marte(), Muestra == input$muestra_marte)
      signal <- df_muestra$velocity.m.s
      fs <- 1 / mean(diff(df_muestra$time_rel.sec))
      
      spec <- spectro(signal, f = fs, wl = 512, plot = FALSE)
      
      color_palette <- colorRampPalette(c("blue", "cyan", "yellow", "red"))
      
      image(spec$time, spec$freq, t(spec$amp), col = color_palette(256), 
            xlab = "Tiempo (s)", ylab = "Frecuencia (Hz)", main = "Espectrograma de Marte")
      
      abline(v = df_muestra$time_rel.sec[detectar_inicios_sismos_marte()], col = "red", lty = 2)
      
      text(x = max(spec$time), y = max(spec$freq), labels = "Sismos detectados", col = "red", pos = 3)
      
      # Agregar leyenda
      legend("topright", legend = c("Sismos detectados"), col = "red", pch = 19, cex = 0.8, bty = "n")
    })
  }
  
  shinyApp(ui, server)
