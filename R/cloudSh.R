#' @title 3-D Scatter Plots

#' @description Uses the \code{cloud} function from the \code{lattice} package.
#'
#' @rdname cloudSh
#' @usage cloudSh(x, data = parent.frame(),adjust.anim=1,options=NULL,...)
#' @param x Could be a formula.  If so, it should be of the form z~x*y|g1*g2, as in the
#' \code{lattice} function \code{cloud}.
#' @param data data frame supplying variables for formula x.  If variables in x are not found in the data,
#' then they will be searched for in the parent environment.
#' @param adjust.anim Use this to adjust animation speed for oprtimum viewing.  The app measures
#' elapsed time to plot the cloud, then attempts to determine a
#' reasonable time interval between successive plots when animation is requested.
#' The interval actually
#' used is the intially-determined interval multiplied by this argument.
#' @param options Options that will be passed to \code{shiny::runApp}.
#' @param ... Other arguments passed to \code{cloud}.
#' @return side effects
#' @note This is a locally-run Shiny app.  It may not work properly on some R Studio Server set-ups,
#' especially on the CentOS operating system.  For best views, open the app in the browser.
#' @import shiny
#' @importFrom lattice cloud
#' @export
#' @author Homer White \email{hwhite0@@georgetowncollege.edu}
#' @examples
#' \dontrun{
#' cloudSh(Petal.Length ~ Sepal.Length * Sepal.Width,data=iris,
#'      group=Species,pch=19,auto.key=TRUE)
#' # to embed in R Markdown:
#' cloudSh(Petal.Length ~ Sepal.Length * Sepal.Width,data=iris,
#'      group=Species,pch=19,auto.key=TRUE,
#'      options=list(width="100%",height=800))
#' }
cloudSh <-
  function (x,data=parent.frame(),adjust.anim=1,options=NULL,...)
  {

  argList <- as.list(match.call(expand.dots = TRUE)[-1])
  argList$adjust.anim <- NULL
  argList$options <- NULL

  plotTime <- system.time(do.call(cloud,argList))[3]+0.001
  interval <- 0.7*10^5*plotTime*adjust.anim

# Define server logic for cloudSh app
server <- shinyServer(function(input, output,session) {

  step <- reactive({
    as.numeric(input$speed)
  })

  output$zControl <- renderUI({
    step <- step()
    sliderInput("zScreen","z",0,360,value=0,step=step,
                animate=animationOptions(interval=interval,loop=TRUE))

  })

  output$xControl <- renderUI({
    step <- step()
    sliderInput("xScreen","x",0,360,value=90,step=step,
                animate=animationOptions(interval=interval,loop=TRUE))

  })

  output$yControl <- renderUI({
    step <- step()
    sliderInput("yScreen","y",0,360,value=40,step=step,
                animate=animationOptions(interval=interval,loop=TRUE))

  })

  output$cloudplot <- renderPlot({
    if (!is.null(input$xScreen)) {
      allArgs <- c(list(screen=list(x=-input$xScreen,y=input$yScreen,z=input$zScreen)),
                   argList)
      do.call(cloud,allArgs)
    }
  })


})


# Define ui for cloudSh app
ui <- shinyUI(fluidPage(

                        #  Application title
                        title="3-D Scatter Plot",
                        titlePanel("3-D Scatter Plot"),

                        fluidRow(
                          column(3,
                                 helpText("Use these sliders to rotate the plot."),
                                 uiOutput("zControl"),
                                 uiOutput("xControl"),
                                 uiOutput("yControl"),
                                 hr(),
                                 helpText("Press a Play button to animate the rotation.",
                                          "For best results, animate only",
                                          "one direction at a time."),
                                 hr(),
                                selectInput("speed","Animation Speed",
                                             c("1 degree"="1",
                                               "10 degrees"="10",
                                               "30 degrees"="30",
                                               "45 degrees"="45"))
                          ),
                          column(9,
                                 plotOutput("cloudplot",width="700px",height="700px")
                                 )
                        )

))  #end fluidPage and and shinyUI

shiny::shinyApp(ui = ui, server = server,options=options)


  }#end cloudSh

