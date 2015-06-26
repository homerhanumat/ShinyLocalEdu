#' @title Dynamic Faceting in Scatter Plots

#' @description Interactively facet a scatterplot by a third variable.
#'
#' @rdname scatterDF
#' @usage scatterDF(form, data = parent.frame())
#' @param form a formula, of the form y~x|z, where z is the facetting variable.
#' @param data dataframe supplying variables for formula.  If variables in the formula are not found in the data,
#' then they will be searched for in the parent environment.
#' @return side effects
#' @note This is a locally-run Shiny app.  It may not work properly on some R Studio Server set-ups,
#' especially on the CentOS operating system.
#' @import shiny ggplot2
#' @export
#' @author Homer White \email{hwhite0@@georgetowncollege.edu}
#' @examples
#' \dontrun{
#' scatterDF(Sepal.Length~Sepal.Width|Species,data=iris)
#' if (require(ggplot2)) scatterDF(price ~ carat | clarity, data = diamonds)
#' if (require(mosaicData)) scatterDF(sat ~ salary | frac, data = SAT)
#' }



scatterDF <-   function (form,data=parent.frame()) {

 prsd <- ParseFormula(form)
 yname <- as.character(prsd$lhs)
 xname <- as.character(prsd$rhs)
 zname <- as.character(prsd$cond)

 y <- simpleFind(yname,data)
 x <- simpleFind(xname,data)
 z <- simpleFind(zname,data)

 df <- data.frame(x,y,z)

 otherColour <- "blue"
 selectColour <- "red"
 myColours <- c(otherColour,selectColour)

 mod <- lm(y ~ x, data = df)


##################################################
## begin ui and server
#################################################

 ui <- basicPage(
   plotOutput("condVar",
              brush = brushOpts(id = "plot_brush", fill = "#ccc", direction = "x"),
              height = 250
   ),
   plotOutput("scatter")
 )

 server <- function(input, output) {

   rv <- reactiveValues(
     dfSelected = NULL
   )

   output$condVar <- renderPlot({
     if ( is.numeric(df$z)) {
      qplot(x = z, data = df, geom = "density")
     } else qplot(x = z, data = df, geom = "bar")
   })

   observeEvent(input$plot_brush,
                {
                  rv$dfSelected <- brushedPoints(df, input$plot_brush, xvar = "z")
                }
                )

   output$scatter <- renderPlot({
     with(df, plot(x,y))
     abline(coef(mod))
     small <- rv$dfSelected
     if ( ! is.null(small) ) {
       with(small, points(x,y, pch = 19, col = "red"))
       modSub <- lm(y ~ x, data = small)
       abline(coef(modSub), lty = 2, lwd = 2, col = "red")
     }
   })

 }

#############################
## knit the app
###########################

 shinyApp(ui = ui, server = server)

} #end scatterDF
