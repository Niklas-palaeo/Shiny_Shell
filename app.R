library(shiny)
library(ggplot2)
library(viridis)
library(readr)
library(dplyr)
library(cowplot)

source("global.R")

ui <- fluidPage(
  titlePanel("Interactive ggplot2 Plot from CSV"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV file"),
      conditionalPanel(
        condition = "input.geom_type !== 'geom_path'",
        sliderInput("xrange", "X-axis range", min = -100, max = 100, value = c(-100, 100), step = 0.1),
        sliderInput("yrange", "Y-axis range", min = -100, max = 100, value = c(-100, 100), step = 0.1)
      ),
      conditionalPanel(
        condition = "input.geom_type === 'geom_path'",
        sliderInput("distance_range", "Distance range in mm", min = 0, max = 50, value = c(0, 50), step = 1),
        numericInput("resolution", "Resolution (mm)", value = 0.02, step = 0.01)
      ),
      conditionalPanel(
        condition = "input.geom_type === 'geom_path'",
        checkboxInput("enable_smooth", "Enable Smoothing"),
        conditionalPanel(
          condition = "input.enable_smooth",
          numericInput("smooth_span", "Smoothing Span:", value = 0.75, min = 0.01, max = 1, step = 0.01)
        ),
        checkboxInput("show_error_bars", "Show Error Bars")
      ),
      # fileInput("line_scan_file", "Line scan location CSV file"),
      actionButton("add_remove_line_scan", "Add line scan location"),
      uiOutput("line_scan_file_ui"),
      conditionalPanel(
        condition = "input.geom_type === 'geom_point'",
        sliderInput("point_size", "Point size", min = 0.1, max = 10, value = 10, step = 0.1)
      ),
      sliderInput("colorrange", "Color range", min = 0, max = 2, value = c(0, 2), step = 0.01),
      selectInput("color_scale", "Select color scale", choices = c("viridis", "magma", "inferno", "plasma")),
      selectInput("geom_type", "Select geom type", choices = c("geom_point", "geom_tile", "geom_path")),
      flowLayout(
        checkboxInput("switch_xy", "Switch X and Y coordinates", value = FALSE),
        checkboxInput("reverse_x", "Reverse X-axis", value = FALSE),
        checkboxInput("reverse_y", "Reverse Y-axis", value = FALSE)
      ),
      selectInput("color_var", "Color Variable:",
                  choices = c("mg_ca" = "mg_ca", "Standard Deviation" = "std", "Relative Standard Deviation" = "rel_std"),
                  selected = "mg_ca"),
      sliderInput("std_range", "Standard Deviation Range:",
                  min = 0, max = 1, value = c(0, 1), step = 0.01),
      sliderInput("rel_std_range", "Relative Standard Deviation Range:",
                  min = 0, max = 1, value = c(0, 1), step = 0.01),
      textInput("plot_title", "Plot title", value = ""),
      numericInput("font_size", "Font size:", value = 20, min = 5, max = 100, step = 1)
      
    ),
    mainPanel(
      plotOutput("scatterplot", width = "1200px", height = "1200px")
    )
  )
)


server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    col_names <- c("x", "y", "z", "mg_ca", "std", "rel_std")
    read_csv(input$file$datapath, col_names = col_names)
  })
  
  observeEvent(input$color_options, {
    shinyjs::toggle("color_options_panel")
  })
  
  observeEvent(input$axis_options, {
    shinyjs::toggle("axis_options_panel")
  })
  
  observeEvent(input$deviation_filtering, {
    shinyjs::toggle("deviation_filtering_panel")
  })
  
  
  
  observeEvent(input$geom_type, {
    if (input$geom_type == "geom_path") {
      plot_data <- data()
      plot_data$distance <- seq_along(plot_data$mg_ca) * input$resolution
      max_distance <- max(plot_data$distance)
      
      updateSliderInput(session, "distance_range", value = c(0, max_distance))
    }
  }, ignoreInit = TRUE)
  
  observeEvent(data(), {
    updateSliderInput(session, "xrange", min = min(data()$x), max = max(data()$x), value = c(min(data()$x), max(data()$x)))
    updateSliderInput(session, "yrange", min = min(data()$y), max = max(data()$y), value = c(min(data()$y), max(data()$y)))
    updateTextInput(session, "plot_title", value = tools::file_path_sans_ext(input$file$name))
  })
  
  observeEvent(input$switch_xy, {
    if (input$switch_xy) {
      updateSliderInput(session, "xrange", min = min(data()$y), max = max(data()$y), value = c(min(data()$y), max(data()$y)))
      updateSliderInput(session, "yrange", min = min(data()$x), max = max(data()$x), value = c(min(data()$x), max(data()$x)))
    } else {
      updateSliderInput(session, "xrange", min = min(data()$x), max = max(data()$x), value = c(min(data()$x), max(data()$x)))
      updateSliderInput(session, "yrange", min = min(data()$y), max = max(data()$y), value = c(min(data()$y), max(data()$y)))
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$switch_xy, {
    if (!is.null(line_scan_data())) {
      line_scan <- line_scan_data()
      line_scan <- line_scan %>%
        mutate(temp_x = x) %>%
        transmute(x = y, y = temp_x)
      line_scan_data(line_scan)
    }
  }, ignoreInit = TRUE)
  
  
# Reactive for the line scan dataset
line_scan_data <- reactiveVal(NULL)
line_scan_options <- reactiveVal(NULL)

# Add or remove line scan dataset when button is clicked
observeEvent(input$add_remove_line_scan, {
  if (is.null(line_scan_data())) {
    output$line_scan_file_ui <- renderUI({
      tagList(
        fileInput("line_scan_file", "Line scan location CSV file"),
        sliderInput("line_size", "Line size", min = 0.1, max = 5, value = 1, step = 0.1),
        selectInput("line_style", "Line style", choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")),
        selectInput("line_color", "Line color",   choices = c("red"="firebrick",  "blue"="cornflowerblue" ,  "green"="forestgreen" , "white" = "white", "black" = "black"))

      )
    })
  } else {
    line_scan_data(NULL)
    line_scan_options(NULL)
    updateActionButton(session, "add_remove_line_scan", label = "Add line scan location")
    output$line_scan_file_ui <- renderUI({})
  }
})

observeEvent(input$line_scan_file, {
  req(input$line_scan_file)
  line_scan <- read_csv(input$line_scan_file$datapath, col_names = c("x", "y"))
  line_scan_data(line_scan)
  updateActionButton(session, "add_remove_line_scan", label = "Remove line scan location")

  # Store line size, style, and color in the reactive variable line_scan_options
  line_scan_options(list(size = input$line_size, style = input$line_style, color = input$line_color))
})

  
  
  
  
plot_output <- reactive({
    req(data())    

  output$scatterplot <- renderPlot({
    req(data())
    
    plot_data <- data() %>%
      filter(std >= input$std_range[1], std <= input$std_range[2],
             rel_std >= input$rel_std_range[1], rel_std <= input$rel_std_range[2])
    
    if (input$switch_xy) {
      plot_data <- plot_data %>%
        mutate(temp_x = x) %>%
        transmute(x = y, y = temp_x, z = z, mg_ca = mg_ca, std = std, rel_std = rel_std)
    }
    
    selected_var <- input$color_var
    
    p <- ggplot(plot_data)
    
    if (input$geom_type == "geom_point") {
      p <- p + geom_point(aes_string(x = "x", y = "y", color = selected_var), size = 3)
    } else if (input$geom_type == "geom_tile") {
      p <- p + geom_tile(aes_string(x = "x", y = "y", fill = selected_var))
    } else {
      plot_data$distance <- seq_along(plot_data$x) * input$resolution
      p <- ggplot(plot_data)
      p <- p + geom_path(aes_string(x = "distance", y = "mg_ca", color = selected_var))
    }
    
    
    
    if (input$geom_type == "geom_path") {
      plot_data$distance <- seq_along(plot_data$x) * input$resolution
      p <- ggplot(plot_data)
      p <- p + geom_path(aes(x = distance, y = mg_ca, color = mg_ca))
      
      if (input$enable_smooth) {
        p <- p + geom_smooth(aes(x = distance, y = mg_ca), method = "loess", span = input$smooth_span, se = FALSE)
      }
      
      if (input$show_error_bars) {
        p <- p + geom_ribbon(aes(x = distance, ymin = mg_ca - std, ymax = mg_ca + std), alpha = 0.3)
      }
      
      p <- p + xlim(input$distance_range) + ylim(input$colorrange)
    } else {
      if (input$geom_type == "geom_point") {
        p <- p + geom_point(aes_string(x = "x", y = "y", color = selected_var), size = input$point_size)
      } else if (input$geom_type == "geom_tile") {
        p <- p + geom_tile(aes_string(x = "x", y = "y", fill = selected_var))
      }
      
      if (input$reverse_x) {
        xlim <- c(input$xrange[2], input$xrange[1])
      } else {
        xlim <- input$xrange
      }
      
      if (input$reverse_y) {
        ylim <- c(input$yrange[2], input$yrange[1])
      } else {
        ylim <- input$yrange
      }
      
      if (!is.null(line_scan_data())) {
        p <- p + geom_path(data = line_scan_data(), aes_string(x = "x", y = "y"), size = input$line_size,
                           linetype = input$line_style,
                           color = input$line_color)
      }
      

      
      
      p <- p + coord_fixed(xlim = xlim, ylim = ylim)
    }
    
    
    if (input$geom_type %in% c("geom_point", "geom_path")) {
      p <- p + scale_color_viridis_c(option = input$color_scale,
                                     limits = input$colorrange,
                                     aes_string(color = input$color_var),  # <- Use the selected color variable
                                     guide = guide_colorbar(title = input$color_var,
                                                            title.position = "top",  
                                                            title.theme = element_text(size = input$font_size),
                                                            label.theme = element_text(size = input$font_size)))
    } else {
      p <- p + scale_fill_viridis_c(option = input$color_scale,
                                    limits = input$colorrange,
                                    aes_string(fill = input$color_var),  # <- Use the selected color variable
                                    guide = guide_colorbar(title = input$color_var,
                                                           title.position = "top",  
                                                           title.theme = element_text(size = input$font_size),
                                                           label.theme = element_text(size = input$font_size)))
    }
    
    
    p <- p + ggtitle(input$plot_title) +
      theme_cowplot() +
      theme(legend.position = "right",
            text = element_text(size = input$font_size),
            axis.title = element_text(size = input$font_size),
            axis.text = element_text(size = input$font_size),
            legend.title = element_text(size = input$font_size),
            legend.text = element_text(size = input$font_size))
    
   
p


})
  })

output$scatterplot <- renderPlot({
  plot_output()
}, res = 300)  # Add the 'res' argument outside the function, and adjust the resolution as needed


# 
# output$downloadPlot <- downloadHandler(
#   filename = function() {
#     paste('data-plot-', Sys.Date(), '.png', sep='')
#   },
#   content = function(file) {
#     png(file)
#     print(plot_output())
#     dev.off()
#   },
#   contentType = "image/png"
# )



}








shinyApp(ui = ui, server = server)
