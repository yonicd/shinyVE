library(ggplot2)

ui <- shiny::fluidPage(
  titlePanel('Interactive Application to Visualize Non-Inferiority FDA Criteria of Vaccine Trials for COVID19'),
  shiny::sidebarLayout(
    sidebarPanel = shiny::sidebarPanel(
      shiny::sliderInput('margin', label = 'Margin',min = 0.1,max = 0.5,value = 0.1),
      shiny::hr(),
      shiny::sliderInput('o_lb', label = 'Current LB',min = 0.3,max = 1,value = stats::runif(1,0.3,0.5)),
      shiny::sliderInput('o_w', label = 'Current CI Width', min = 0.25, max = 0.5, value = 0.4),
      shiny::hr(),
      shiny::sliderInput('c_w', label = 'Candidate CI Width', min = 0.25, max = 0.5, value = 0.4),
      shiny::hr(),
      shiny::a('FDA Guidelines', href = 'https://www.fda.gov/media/139638/download')
    ),
    mainPanel = shiny::mainPanel(
      shiny::plotOutput('plot',height = 300),
      shiny::hr(),
      gt::gt(tibble::tibble(
        Term = c('Margin','Current LB', 'Current CI Width', 'Candidate CI Width'),
        Definition = c('Loss of Vaccine Effect of Candidate Vaccine Compared to Current Vaccine',
                       'The estimated lower bound of the current approved vaccine',
                       'The width of the confidence interval of the current vaccine',
                       'The width of the confidence interval of the candidate vaccine'),
        FDA = c('10%','min 30%','','')
      ))

    )
  )
)

server <- function(input, output) {

  output$plot <- shiny::renderPlot({

    seg_start <- input$o_lb * (1-input$margin/2)
    seg_end <- input$o_lb + 0.1

    xmin = input$o_lb * (1-input$margin) - input$c_w
    xmax <- input$o_lb + input$o_w

    ggplot() +
      geom_errorbarh(aes(y = 2, xmin = input$o_lb, xmax = xmax)) +
      geom_errorbarh(aes(y = 1, xmin = xmin, xmax = input$o_lb * (1-input$margin))) +
      geom_segment(aes(y=1.5,yend = 1.5, x = input$o_lb * (1-input$margin), xend = input$o_lb), size = 2,colour = 'red') +
      geom_curve(aes(y = 1.48, yend=1.4,x = seg_start, xend = seg_end),
                 arrow = grid::arrow(length = unit(0.1, "inches"),ends = 'first'), curvature = 0.5) +
      geom_label(aes(y = 1.39, x = seg_end),label = glue::glue('Margin:\n{scales::percent(x = input$margin)}')) +
      ggrepel::geom_label_repel(aes(y = 1, x = (input$o_lb * (1-input$margin)) + 0.01),
                 label = glue::glue('Target Upper Bound:\n{scales::percent(x = input$o_lb * (1-input$margin))}'),nudge_x = 0.1,nudge_y = -0.05,arrow = grid::arrow(length = unit(0.1, "inches"))) +
      scale_y_continuous(breaks = c(1,2),labels = c('Candidate','Current')) +
      scale_x_continuous(labels = scales::percent,limits = c(pmin(0,xmin),pmax(1,xmax))) +
      labs(
        x = 'Vaccine Effect\n(1 - (P(trt)/P(plc)))',
        y = NULL
      ) +
      theme_linedraw()
  })

}

# Return a Shiny app object
shinyApp(ui = ui, server = server,options = list(port=6012))
