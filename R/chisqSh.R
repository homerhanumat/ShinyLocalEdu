#' @title Chi-Square Simulation (Contingency Table)

#' @description Perform chi-square test for association or goodness of fit, by simulation.
#' Enter either formula-data input or a summary table.
#'
#' @rdname chisqSh
#' @usage chisqSh(x, data = parent.frame(),p=NULL,options=NULL)
#' @param x Could be a formula.  If so, it should be of the form ~var (for goodness of fit testing),
#' or~var1+var2 (for association testing).  Otherwise it is either a table or matrix of summary data.
#' @param data data frame supplying variables for formula x.  If variables in x are not found in the data,
#' then they will be searched for in the parent environment.
#' @param p Null probabilities in a goodness of fit test
#' @param options passed to \code{shiny::runApp}.
#' @return side effects
#' @note This is a locally-run Shiny app.  It may not work properly on some R Studio Server set-ups,
#' especially on the CentOS operating system.  For best views, open the app in the browser.
#' @import shiny
#' @import shinythemes
#' @export
#' @author Homer White \email{hwhite0@@georgetowncollege.edu}
#' @examples
#' \dontrun{
#' # test for association, from a data frame:
#' chisqSh(~am+cyl,data=mtcars)
#'
#' # test for association, from a summary table:
#' DoesNotSmoke <- c(NeitherSmokes=1168,OneSmokes=1823,BothSmoke=1380)
#' Smokes <- c(188,416,400)
#' ChildParents <- rbind(DoesNotSmoke,Smokes)
#' chisqSh(ChildParents)
#'
#' # test for goodness of fit, from a data frame:
#' chisqSh(~cyl,data=mtcars,p=rep(1/3,3))
#'
#' #test for goodness of fit, from a summary table:
#' obs <- c(one=8,two=18,three=11,four=7,five=9,six=7)
#' chisqSh(obs,p=rep(1/6,6))
#' }
chisqSh <-
  function (x,data=parent.frame(),p=NULL,options=NULL)
  {

###########################################################
#  begin with utiltiy functions
##########################################################


# Modified from pchisqGC.R in package tigerstats
chisqGraph <- function(bound,region="above",df=NA,xlab="chi_square_statistic",graph=FALSE) {
  if (!is.numeric(bound)) stop("Specify a numerical boundary")
  if (bound < 0)  stop("The chi-square statistic must be at least 0")
  if (is.na(df)) stop("Specify the degrees of freedom using the argument df")
  if (!(region %in% c("below","above"))) stop("Specify either \"region=\"below\" or
                                              \"region=\"above\"")
  if (df < 0) stop("Degrees of freedom must be positive")

  if (region=="below")  {
    area <- pchisq(bound,df=df)
    if (graph && df==1) warning("No graph produced for region below when df=1")
    if (graph) {
      bound <- round(bound,2)
      upper <- max(qchisq(.9999,df=df),bound+1)
      lower <- 0
      curve(dchisq(x,df=df),from=lower,to=upper,ylab="density",axes=FALSE,n=501,xlab=xlab,
            main=paste("Chi-Square Curve, df = ",df,"\nShaded Area = ",round(area,4)))
      axis(1,at=c(lower,bound,upper),labels=c(as.character(0),as.character(bound),""))
      axis(2)
      x.coords <- c(lower,seq(lower,bound,length.out=301),bound)
      y.coords <- c(0,dchisq(seq(lower,bound,length.out=301),df=df),0)
      polygon(x.coords,y.coords,col="lightblue",cex=2)
    }
  }

  if (region=="above")  {
    area <- pchisq(bound,df=df,lower.tail=FALSE)
    if (graph) {
      bound <- round(bound,2)
      upper <- max(qchisq(.9999,df=df),bound+1)
      lower <- 0
      curve(dchisq(x,df=df),from=lower,to=upper,ylab="density",axes=FALSE,n=501,xlab=xlab,
            main=paste("Chi-Square Curve, df = ",df,"\nShaded Area = ",round(area,4)))
      axis(1,at=c(lower,bound,upper),labels=c(as.character(0),as.character(bound),""))
      axis(2)
      x.coords <- c(bound,seq(bound,upper,length.out=301),upper)
      y.coords <- c(0,dchisq(seq(bound,upper,length.out=301),df=df),0)
      polygon(x.coords,y.coords,col="lightblue",cex=2)
    }
  }



}#end of chisqGraph


    exp.counts <- function(x) (rowSums(x) %*% t(colSums(x)))/sum(x)

    chisq.calc <- function(x) {
      expected <- exp.counts(x)
      contributions <- (x - expected)^2/expected
      return(sum(contributions[!is.nan(contributions)]))
    }

##########################################################################
#    simulation for "rcfix" option
##########################################################################

    DoubleFixedResampler <- function(x,n) {
      expected <- exp.counts(x)
      csq <- function(x) {
        sum((x-expected)^2/expected)
      }
      statistic <- csq(x)
      nullDist <- numeric(n)

      r <- rowSums(x)
      c <- colSums(x)

      rtabs <- r2dtable(n,r=r,c=c)
      sims <- sapply(rtabs,FUN=csq,USE.NAMES=FALSE)

      return(list(sims=sims,last_table=rtabs[[n]]))
    }


#################################################
#   simulation for "rfix" and "gtfix" options
#################################################

    RandFixedResampler <- function (x, n, effects = "random")
    {
      #x is a two-way table, n is number of resamples
      TableResampler <- function(x, n = 1000, effects) {
        rowsampler <- function(x, p) {
          rmultinom(1, size = sum(x), prob = p)
        }

      table.samp <- function(x) {
        nullprobs <- colSums(x)/sum(x)
        resamp <- t(apply(x, 1, rowsampler, p = nullprobs))
        rownames(resamp) <- rownames(x)
        colnames(resamp) <- colnames(x)
        as.table(resamp)
      }

      rtabsamp <- function(x, n) {
          expected <- exp.counts(x)
          probs <- expected/sum(x)
          resamp.tab <- rmultinom(1, size = n, prob = probs)
          resamp.tab <- matrix(resamp.tab, nrow = nrow(x))
          rownames(resamp.tab) <- rownames(x)
          colnames(resamp.tab) <- colnames(x)
          return(resamp.tab)
      }

      resampled.tabs <- array(0, dim = c(nrow(x), ncol(x),n))

        if (effects == "fixed") {
          for (i in 1:n) {
            resampled.tabs[, , i] <- table.samp(x)
          }
          return(resampled.tabs)
        }
        if (effects == "random") {
          for (i in 1:n) {
            resampled.tabs[, , i] <- rtabsamp(x, sum(x))
          }
          return(resampled.tabs)
        }
      }
      tables <- TableResampler(x, n, effects = effects)
      nullDist <- apply(tables, 3, chisq.calc)
      return(list(sims=nullDist,last_table=tables[,,n]))
    }#end of RandFixedResampler



 ##################################################################
 ####
 ####
 #### Process Input
 ####
 ####
 #################################################################

type <- NULL #will be set to the type of test (association or goodness)

#first see if we have formula-data input, or summary data
if (is(x,"formula")) #we have formula-data input
{

  prsd <- ParseFormula(x)
  pullout <- as.character(prsd$rhs)

  if (length(pullout)==3) #Test for association

  {
    type <- "association"

    expname <- as.character(prsd$rhs)[2]
    respname <- as.character(prsd$rhs)[3]

    explanatory <- simpleFind(varName=expname,data=data)
    response <- simpleFind(varName=respname,data=data)
    data2 <- data.frame(explanatory,response)
    names(data2) <- c(expname,respname)
    tab <- table(data2)

  } #end processing of association test

  if(length(pullout)==1)  #goodness of fit
  {
    type <- "goodness"
    varname <- pullout[1]


    variable <- simpleFind(varName=varname,data=data)
    tab <- table(variable)

    #Note:  if variable has inherited levels (from a previous life) that user no longer
    #expects to see, then length of table will exceed length of p and there will be
    #problems.

  } #end processing of goodness of fit test

}#end formula processing

if (!is(x,"formula"))  #we have summary data
{
  if (length(dim(x))>2) #array more than two dimensions
  {
    stop("This function does not handle tables with more than two dimensions")
  }

  tab <- as.table(x)
  if (length(dim(tab))==1) {
    type <- "goodness"
  }#end of goodness of fit processing

  if (length(dim(tab))==2) {
    type <- "association"
  }#end of association processing

}#end processing for summary data

# Define server logic for goodness of fit test
server1 <- shinyServer(function(input, output,session) {
  simLimit <- 10000

  #Keep track of number of simulations in a given "set-up"
  numberSims <- 0
  chisqSims <- numeric()
  latestSim <- NULL
  fullSim <-character()

  #we also want the ability to refresh the "set-up
  total <- 0 #total number of sims over all set-ups including current one
  totalPrev <- 0 #total number of sims over all set-ups excluding current one

  p <- p/sum(p)
  nullsInput <- p

  obsInput <- as.integer(tab)

  namesInput <- names(tab)

  expectedInput <- sum(tab)*nullsInput

  obschisqInput <- sum((obsInput-expectedInput)^2/expectedInput)

  yatesCorrection <- sum((abs(obsInput-expectedInput)-0.5)^2/expectedInput)

  simsUpdate <- reactive({
    if (input$resample > 0) {
      nullProbs <- nullsInput/sum(nullsInput)
      totalCounts <- sum(obsInput)
      expCounts <- nullProbs*totalCounts
      reps <- min(simLimit,isolate(input$sims))
      newSims <- rmultinom(n=reps,size=totalCounts,prob=nullProbs)
      chisqNew <- colSums(newSims^2/expCounts)-totalCounts
      chisqSims <<- c(chisqSims,chisqNew)
      latestSim <<- newSims[,reps]
      numberSims <<- numberSims + reps
      total <<- total+reps

      #now build fake list of outcomes for each trial, on the last sim
      varLevels <- namesInput
      namesList <- rep(varLevels,times=latestSim)
      fullSim <<- sample(namesList,size=totalCounts,replace=FALSE)
      if (total - totalPrev == 1) updateTabsetPanel(session,"simsTabset",selected="Latest Simulation")
      list(numberSims,latestSim)
    }
  })


  #this erases the simulation history and puts user back to initial graph
  simsReset <- reactive({
    input$reset
    totalPrev <<- totalPrev + numberSims
    numberSims <<- 0
    chisqSims <<- numeric()
    latestSim <<- NULL
    return(totalPrev)
  })


  dfInput <- length(obsInput)-1


  xmaxInput <- qchisq(0.999,df=dfInput)


  #help with conditonal panals
  output$totalPrev <- reactive({
    simsReset()
  })

  # needed for the conditional panels to work
  outputOptions(output, 'totalPrev', suspendWhenHidden=FALSE)

  output$total <- reactive({
    simsUpdate() #for dependency
    total
  })

  # needed for the conditional panels to work
  outputOptions(output, 'total', suspendWhenHidden=FALSE)

  chisqDensities <- reactive({
    input$resample
    if (length(chisqSims)==1) band <- 1 else band <- "nrd0"
    density(chisqSims,n=500,from=0,to=xmaxInput,bw=band)
  })

  output$barGraphInitial <- renderPlot({

    observed <- obsInput
    nulls <- nullsInput/sum(nullsInput)
    names <- namesInput

    observed <- obsInput
    expected <- nulls*sum(observed)
    tab <- rbind(observed,expected)
    rownames(tab) <-c("Observed","Expected")
    colnames(tab) <- names
    barplot(tab,beside=T,col=c("#ee7700","grey"),
            main="Bargraph of Observed and Expected Counts",xlab="",ylab="Counts",
            legend.text=TRUE)
  })

  output$remarksInitial <- renderText({

    observed <- obsInput
    nulls <- nullsInput/sum(nullsInput)
    names <- namesInput

    chisq <- obschisqInput
    rounded1 <- round(chisq,2)
    paste("Observed chi-square statistic =  ",as.character(rounded1),sep="")
  })

  output$obsTable <- renderTable({

    observed <- obsInput
    nulls <- nullsInput/sum(nullsInput)
    names <- namesInput

    expected <- nulls*sum(observed)
    contribs <- (observed-expected)^2/expected
    df <- data.frame(Levels=names,
                     Observed=observed,
                     Expected=round(expected,2),
                     cont=round(contribs,2)
    )
    names(df)[4] <- c("Contribution to Chi-Square")
    df
  })

  output$remarksLatest1 <- renderText({
    input$resample
    chisq <- obschisqInput
    rounded1 <- round(chisq,2)
    rounded2 <- round(chisqSims[length(chisqSims)],2)
    paste("Observed chi-square statistic =  ",as.character(rounded1),
          ", Latest resampled chi-square = ",as.character(rounded2),sep="")
  })

  output$remarksLatest2 <- renderText({
    input$resample
    chisq <- obschisqInput
    rounded1 <- round(chisq,2)
    rounded2 <- round(chisqSims[length(chisqSims)],2)
    paste("Observed chi-square statistic =  ",as.character(rounded1),
          ", Latest resampled chi-square = ",as.character(rounded2),sep="")
  })


  output$barGraphLatest <- renderPlot({
    input$resample
    if (length(chisqSims) > 0) {
      totalCounts <- sum(obsInput)
      nulls <- nullsInput/sum(nullsInput)
      expected <- totalCounts*nulls
      tab <- rbind(obsInput,expected,latestSim)
      rownames(tab) <-c("Observed","Expected","Resampled")
      colnames(tab) <- isolate(namesInput)
      barplot(tab,beside=T,col=c("#ee7700","grey","#3333ff"),
              main="Bargraph of Observed, Expected, and Latest Resample",xlab="",
              ylab="Counts",
              legend.text=TRUE)
    }

  })

  output$densityplot <-
    renderPlot({
      input$resample
      if (length(chisqSims)==1) band <- 1 else band <- "nrd0"
      dchisq <- density(chisqSims,n=500,from=0,to=xmaxInput,bw=band)
      plot(dchisq$x,dchisq$y,type="l",col="blue",
           xlab="Chi-Square Value",ylab="Estimated Density",
           main="Distribution of Resampled Chi-Square Statistics")
      if (length(chisqSims) <= 100) rug(chisqSims)
      latest <- chisqSims[length(chisqSims)]
      points(latest,0,col="blue",pch=19)
      abline(v=obschisqInput)

    })

  output$summary1 <- renderTable({
    input$resample
    obs <- obschisqInput
    if (length(chisqSims) >0) {
      n <- length(chisqSims)
      latest <- chisqSims[n]
      p.value <- length(chisqSims[chisqSims>=obs])/n
      percentage <- paste(as.character(round(p.value*100,2)),"%",sep="")
      df <- data.frame(round(latest,2),n,percentage)
      names(df) <- c("Last Resampled Chi-Square",
                     "Number of Resamples So Far",
                     paste("Percent Above ",round(obs,2),sep="")
      )
      df
    }
  })

  output$remarksProbBar <- renderText({
    obs <- obschisqInput
    paste0("The percentage in the table gives the approximate probability, based on our resamples so far, of getting a chi-square statistic of ",
           round(obs,2)," or more, if the probability of each outcome is as the Null probabilities state.",
           "  The more resamples you take the better this approximations will be!")
  })


  output$summary2 <- renderTable({
    input$resample
    obs <- obschisqInput
    n <- length(chisqSims)
    latest <- chisqSims[n]
    p.value <- length(chisqSims[chisqSims>=obs])/n
    percentage <- paste(as.character(round(p.value*100,2)),"%",sep="")
    df <- data.frame(round(latest,2),n,percentage)
    names(df) <- c("Last Resampled Chi-Square",
                   "Number of Resamples So Far",
                   paste("Percent Above ",round(obs,2),sep="")
    )
    df
  })

  output$remarksProbDensity <- renderText({
    obs <- obschisqInput
    paste0("The curve above approximates the true probability distribution of the chi-square statistic.",
           " It is based on our resamples so far.  The percentage in the table gives the approximate probability, based on our resamples so far, of getting a chi-square statistic of ",
           round(obs,2)," or more, if the probability of each outcome is as the Null probabilities state.",
           "  The more resamples you take the better these approximations will be!")
  })


  output$chisqCurve <- renderPlot({
    obs <- obschisqInput
    degFreedom <- dfInput
    if (input$yates) {
      chisqGraph(bound=yatesCorrection,region="above",df=degFreedom,xlab="Chi-Square Values",
                 graph=TRUE)
      abline(v=yatesCorrection)
    } else {
      chisqGraph(bound=obs,region="above",df=degFreedom,xlab="Chi-Square Values",
               graph=TRUE)
      abline(v=obs)
    }
    if (input$compareDen) {
      lines(chisqDensities(),col="blue",lwd=4)
    }
  })

  output$remarksProb <- renderText({
    obs <- obschisqInput
    paste0("The curve above approximates the true probability distribution of the chi-square statistic.",
           " The shaded area gives the approximate probability of getting a chi-square statistic of ",
           round(obs,2)," or more, if the probability of each outcome is as the Null probabilities state.")
  })

})


# Define server logic for association test
server2 <- function(input, output,session) {

  simLimit <- 10000 # no more than this many sims at one time

  #Keep track of number of simulations in a given "set-up"
  numberSims <- 0
  chisqSims <- numeric()
  latest_table <- NULL
  latestSim <- NULL

  #we also want the ability to refresh the set-up
  total <- 0 #total number of sims over all set-ups including current one
  totalPrev <- 0 #total number of sims over all set-ups excluding current one

  # The observed two-way table:
  observed <- tab
  rowNames <- rownames(tab)
  colNames <- colnames(tab)

  expected <- exp.counts(tab)

  obschisq <- chisq.calc(observed)

  yatesCorrection <- sum((abs(observed-expected)-0.5)^2/expected)


  simsUpdate <- reactive({
    if (input$resample > 0) {
      reps <- min(simLimit,isolate(input$sims))
      simType <- isolate(input$simType) #probably don't need isolate
      if (simType=="rcfix") newSims <- DoubleFixedResampler(observed,reps)
      if (simType=="rfix") newSims <- RandFixedResampler(observed,reps,effects="fixed")
      if (simType=="gtfix") newSims <- RandFixedResampler(observed,reps,effects="random")
      chisqNew <- newSims$sims
      latest_table <<- newSims$last_table
      chisqSims <<- c(chisqSims,chisqNew)
      latestSim <<- newSims[reps]
      numberSims <<- numberSims + reps
      total <<- total+reps
      if (total - totalPrev == 1) updateTabsetPanel(session,"simsTabset",selected="Latest Simulation")
      list(numberSims,latestSim)
    }
  })


  #this erases the simulation history and puts user back to initial graph
  simsReset <- reactive({
    input$reset
    totalPrev <<- totalPrev + numberSims
    numberSims <<- 0
    chisqSims <<- numeric()
    latest_table <<- NULL
    latestSim <<- NULL
    return(totalPrev)
  })


  df <- (nrow(observed)-1)*(ncol(observed)-1)

  xmax <- qchisq(0.999,df=df)

  #help with conditonal panels
  output$totalPrev <- reactive({
    simsReset()
  })

  # needed for the conditional panels to work
  outputOptions(output, 'totalPrev', suspendWhenHidden=FALSE)

  output$total <- reactive({
    simsUpdate() #for dependency
    total
  })

  # also needed for the conditional panels to work
  outputOptions(output, 'total', suspendWhenHidden=FALSE)


  output$remarksInitial <- renderText({
    paste("Observed chi-square statistic =  ",as.character(round(obschisq,2)),sep="")
  })

  output$obsTable <- renderTable({
    observed
  })

  output$expTable <- renderTable({
    expected <- exp.counts(observed)
    rownames(expected) <- rowNames
    expected
  })

  output$mosaicInitial <- renderPlot({
    par(mfrow=c(1,2))
    mosaicplot(t(observed),col="orange",main="Observed Table",cex.axis=1.3)
    expected <- exp.counts(observed)
    rownames(expected) <- rowNames
    mosaicplot(t(expected),col="grey",main="Expected Table",cex.axis=1.3)
    par(mfrow=c(1,1))
  })

  output$contrTable <- renderTable({
    (observed-exp.counts(observed))^2/exp.counts(observed)
  })

  output$remarksLatest1 <- renderText({
    input$resample
    rounded1 <- round(obschisq,2)
    rounded2 <- round(chisqSims[length(chisqSims)],2)
    paste("Observed chi-square statistic =  ",as.character(rounded1),
          ", Latest resampled chi-square = ",as.character(rounded2),sep="")
  })

  output$mosaicLatest <- renderPlot({
    if(input$resample > 0) { # for the dependency
      par(mfrow=c(1,2))
      rownames(latest_table) <- rowNames
      colnames(latest_table) <- colNames
      latest_table <- as.matrix(latest_table)
      mosaicplot(t(latest_table),col="blue",main="Simulated Table",cex.axis=1.3)
      expected <- exp.counts(latest_table)
      rownames(expected) <- rowNames
      mosaicplot(t(expected),col="grey",main="Expected Table\n(from simulation)",cex.axis=1.3)
      par(mfrow=c(1,1))
    }
  })

output$latestTable <- renderTable({
  input$resample
  if (!is.null(latest_table)) {
    rownames(latest_table) <- rowNames
    colnames(latest_table) <- colNames
    storage.mode(latest_table) <- "integer"
    return(latest_table)
  }
})

output$latestExpTable <- renderTable({
  input$resample
  if (!is.null(latest_table)) {
    exp <- exp.counts(latest_table)
    rownames(exp) <- rowNames
    colnames(exp) <- colNames
    return(exp)
  }
})

  output$remarksLatest2 <- renderText({
    input$resample
    rounded1 <- round(obschisq,2)
    rounded2 <- round(chisqSims[length(chisqSims)],2)
    paste("Observed chi-square statistic =  ",as.character(rounded1),
          ", Latest resampled chi-square = ",as.character(rounded2),sep="")
  })

  chisqDensities <- reactive({
    input$resample
    if (length(chisqSims)==1) band <- 1 else band <- "nrd0"
    density(chisqSims,n=500,from=0,to=xmax,bw=band)
  })

  output$densityplot <-
    renderPlot({
      input$resample
      dchisq <- chisqDensities()
      plot(dchisq$x,dchisq$y,type="l",col="blue",
           xlab="Chi-Square Value",ylab="Estimated Density",
           main="Distribution of Resampled Chi-Square Statistics")
      if (length(chisqSims) <= 200) rug(chisqSims)
      latest <- chisqSims[length(chisqSims)]
      points(latest,0,col="blue",pch=19)
      abline(v=isolate(obschisq))

    })

  output$summary1 <- renderTable({
    input$resample
    obs <- obschisq
    if (length(chisqSims) >0) {
      n <- length(chisqSims)
      latest <- chisqSims[n]
      p.value <- length(chisqSims[chisqSims>=obs])/n
      percentage <- paste(as.character(round(p.value*100,2)),"%",sep="")
      df <- data.frame(round(latest,2),n,percentage)
      names(df) <- c("Last Resampled Chi-Square",
                     "Number of Resamples So Far",
                     paste("Percent Above ",round(obs,2),sep="")
      )
      df
    }
  })

  output$remarksProbBar <- renderText({
    obs <- obschisq
    paste0("The percentage in the table gives the approximate probability, based on our resamples so far, of getting a chi-square statistic of ",
           round(obs,2)," or more, if the Null Hypothesis (no relationship between the two factor",
           " variables under study) is true. The more resamples you take the better these",
           "approximations will be!")
  })


  output$summary2 <- renderTable({
    input$resample
    obs <- obschisq
    n <- length(chisqSims)
    latest <- chisqSims[n]
    p.value <- length(chisqSims[chisqSims>=obs])/n
    percentage <- paste(as.character(round(p.value*100,2)),"%",sep="")
    df <- data.frame(round(latest,2),n,percentage)
    names(df) <- c("Last Resampled Chi-Square",
                   "Number of Resamples So Far",
                   paste("Percent Above ",round(obs,2),sep="")
    )
    df
  })

  output$remarksProbDensity <- renderText({
    obs <- obschisq
    paste0("The curve above approximates the true probability distribution of the chi-square statistic.",
           " It is based on our resamples so far.  The percentage in the table gives the approximate",
            "probability, based on our resamples so far, of getting a chi-square statistic of ",
           round(obs,2)," or more, if the Null Hypothesis (no relationship between the two factor",
           " variables under study) is true. The more resamples you take the better these",
            "approximations will be!")
  })


  output$chisqCurve <- renderPlot({
    obs <- obschisq
    degFreedom <- df
    if (input$yates) {
      chisqGraph(bound=yatesCorrection,region="above",df=degFreedom,xlab="Chi-Square Values",
                 graph=TRUE)
      abline(v=yatesCorrection)
    } else {
      chisqGraph(bound=obs,region="above",df=degFreedom,xlab="Chi-Square Values",
                 graph=TRUE)
      abline(v=obs)
    }
    if (input$compareDen) {
      lines(chisqDensities(),col="blue",lwd=4)
    }
  })

  output$remarksProb <- renderText({
    obs <- obschisq
    paste0("The curve above approximates the true probability distribution of the chi-square statistic.",
           " The shaded area gives the approximate probability of getting a chi-square statistic of ",
           round(obs,2)," or more, if the Null Hypothesis (no relationshoip between the two factor",
           " variables under study) is true.")
  })

} #end server


# Define ui for goodness of fit test
ui1 <- shinyUI(fluidPage(shinythemes::shinytheme("cerulean"),

  #  Application title
  titlePanel("Chi-Square Goodness-of-Fit Resampling"),

  # Sidebar
  sidebarLayout(
    sidebarPanel(
      helpText("One simulation means the machine will produce one table of",
             "counts, using the Null probabilities.  How many simulations do",
             "you want the machine to perform at once?  (Limit is 10000.)"),
      numericInput("sims","Number of Simulations at Once",1,min=0,step=1),
      br(),
      actionButton("resample","Simulate Now"),
      conditionalPanel(
        condition="(input.resample > 0 && input.reset == 0) || output.total > output.totalPrev",
        actionButton("reset","Start Over")
      )
    ), #end sidebarPanel

  # Here comes the main panel
  mainPanel(

    conditionalPanel(
      condition="input.resample == 0 || output.totalPrev == output.total",
      tabsetPanel(selected="Setup",
        tabPanel("Setup",
          plotOutput("barGraphInitial"),
          p(textOutput("remarksInitial")),
          tableOutput("obsTable")
        ),
        tabPanel("App Help",
                 HTML('<style type="text/css">body {max-width: 100%!important;}</style>'),
                 includeHTML(system.file("doc/chisqSh.html",package="ShinyLocalEdu"))
        )
      ) #end tabsetPanel
    ), #end conditionalPaenl

    conditionalPanel(
      condition="(input.resample > 0 && input.reset == 0) || output.total > output.totalPrev",
      tabsetPanel(id="simsTabset",selected="Latest Simulation",
                  tabPanel("Latest Simulation",
                           plotOutput("barGraphLatest"),
                           p(textOutput("remarksLatest1")),
                           tableOutput("summary1"),
                           p(textOutput("remarksProbBar"))),
                  tabPanel(HTML("Density Plot<br>of Simulations"),
                           plotOutput("densityplot"),
                           p(textOutput("remarksLatest2")),
                           tableOutput("summary2"),
                           p(textOutput("remarksProbDensity"))),
                  tabPanel("Probability Distribution",
                           plotOutput("chisqCurve"),
                           br(),
                           splitLayout(
                            checkboxInput("compareDen",
                                          HTML("Compare with simulated <br>chi-square distribution")),
                            checkboxInput("yates","Use Yates correction")
                            ),
                           p(textOutput("remarksProb"))
                           ),
                  tabPanel("App Help",
                           HTML('<style type="text/css">body {max-width: 100%!important;}</style>'),
                           includeHTML(system.file("doc/chisqSh.html",package="ShinyLocalEdu"))
                          )
                  ),
                  id="MyPanel"
      )


  ) #end mainPanel

  ) #nd SidebarLayout

))  #end fluidPage and and shinyUI


# Define ui for association test
ui2 <- shinyUI(fluidPage(

  #  Application title
  titlePanel("Chi-Square Test for Association: Resampling"),

  # Sidebar
  sidebarLayout(
  sidebarPanel(
    conditionalPanel(
      condition="input.resample == 0 || output.totalPrev == output.total",
      radioButtons(inputId="simType",
                    label="Choose type of simulation",
                    choices=c("Row and Column Sums Fixed"="rcfix",
                           "Row Sums Fixed"="rfix",
                           "Grand Total Fixed"="gtfix"))
      ),
    helpText("One simulation means the machine will produce one simulated table of",
             "counts, assuming the Null Hypothesis.  How many simulations do",
             "you want the machine to perform at once?  (Limit is 10000.)"),
    numericInput("sims","Number of Simulations at Once",1,min=0,step=1),
    br(),
    actionButton("resample","Simulate Now"),
    conditionalPanel(
      condition="(input.resample > 0 && input.reset == 0) || output.total > output.totalPrev",
      actionButton("reset","Start Over")
    ),
    width=3
  ), #end sidebarPanel


  # Here comes the main panel
  mainPanel(

    conditionalPanel(
      condition="input.resample == 0 || output.totalPrev == output.total",
        tabsetPanel(selected="Setup",
            tabPanel("Setup",
                  plotOutput("mosaicInitial"),
                  fluidRow(
                        column(3,
                             h5("Observed"),
                             tableOutput("obsTable")
                             ),
                        column(3,
                             h5("Expected by Null"),
                             tableOutput("expTable")
                             ),
                        column(3,offset=1,
                             h5("Contributions"),
                             tableOutput("contrTable")
                             )
                  ),
                  hr(),
                  h5(textOutput("remarksInitial"))
            ),
            tabPanel("App Help",
              HTML('<style type="text/css">body {max-width: 100%!important;}</style>'),
              includeHTML(system.file("doc/chisqSh.html",package="ShinyLocalEdu"))
            )
        ) # end tabsetPanel

    ), #end conditionalPanel

    conditionalPanel(
      condition="(input.resample > 0 && input.reset == 0) || output.total > output.totalPrev",
      tabsetPanel(id="simsTabset",selected="Latest Simulation",
                  tabPanel("Latest Simulation",
                           plotOutput("mosaicLatest"),
                           fluidRow(
                             column(4,
                                h5("Simulated Table"),
                                tableOutput("latestTable")
                             ),
                             column(4,offset=2,
                                    h5("Expected Table"),
                                    tableOutput("latestExpTable")
                                    )
                           ),
                           p(textOutput("remarksLatest1")),
                           tableOutput("summary1"),
                           p(textOutput("remarksProbBar"))),
                  tabPanel(HTML("Density Plot<br>of Simulations"),
                           plotOutput("densityplot"),
                           p(textOutput("remarksLatest2")),
                           tableOutput("summary2"),
                           p(textOutput("remarksProbDensity"))),
                  tabPanel("Probability Distribution",
                           plotOutput("chisqCurve"),
                           br(),
                           splitLayout(
                            checkboxInput("compareDen",
                                          HTML("Compare with simulated <br>chi-square distribution")),
                            checkboxInput("yates","Use Yates correction")
                            ),
                           p(textOutput("remarksProb"))
                        ),
                  tabPanel("App Help",
                           HTML('<style type="text/css">body {max-width: 100%!important;}</style>'),
                           includeHTML(system.file("doc/chisqSh.html",package="ShinyLocalEdu"))
                        )
                  ),
                  id="MyPanel"
      ),
    width = 9

  )# end mainPanel

  ) # end sidebarLayout

)) # end fluidPage and shinyUI


#choose ui and server based on type of test:
if (type=="goodness") {
  server <- server1
  ui <- ui1
} else {
  server <- server2
  ui <- ui2
}

shiny::shinyApp(ui = ui, server = server,options=options)


  }#end chisqSimShiny

