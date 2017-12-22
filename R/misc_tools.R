####################################################################################
##
## Some misc tools that are of special use
##
####################################################################################

## Clear console and plots, and remove variables from workspace unless given as arguments
    clean_slate <- function(...){
      for(i in dev.list()) dev.off(which=i)
      keep = as.list(match.call())
      rm(list=setdiff(ls(envir = .GlobalEnv), keep), envir = .GlobalEnv)
      assign("savePDF", FALSE, envir = .GlobalEnv)
      cat("\014")  # Clear console
    }

####################################################################################

## Add an alpha value to a color
    add.alpha <- function(col, alpha=1){
      if(missing(col))
        stop("Please provide a vector of colours.")
      apply(sapply(col, col2rgb)/255, 2,
            function(x)
              rgb(x[1], x[2], x[3], alpha=alpha))
    }


####################################################################################

## Send notification when done!
# open client
# browseURL("https://client.pushover.net/")

    notify <- function(msg){
      suppressMessages(pushoverr::set_pushover_app("avy6wjvgqsbfpodjr14xduaph52gsc"))
      suppressMessages(pushoverr::set_pushover_user("uc6dn1q1b87vbsko5okxqeh8tjzepi"))
      pushoverr::pushover_quiet(msg)
    }


####################################################################################

## Loop timer display

    displayprogress <- function(perc,est=NULL){
      msg = format(round(perc, 1), nsmall = 1)

      if(!is.null(est)){
        if(est > 60){
          if(est/60 >= 10){
            est = paste(" ",as.character(format(round(est/60, 1), nsmall = 1)),"m   ",sep="")
          }else{
            est = paste("  ",as.character(format(round(est/60, 1), nsmall = 1)),"m   ",sep="")
          }
        }else{
          if(est >= 10){
            est = paste(" ",as.character(format(round(est, 1), nsmall = 1)),"s   ",sep="")
          }else{
            est = paste("  ",as.character(format(round(est, 1), nsmall = 1)),"s   ",sep="")
          }
        }

        if(perc == 100){
          cat("\r",paste("Progress ",msg,"%", sep=""))
          cat("\r"," ")
        } else if (perc < 10){
          cat("\r",paste("Progress   ",msg,"% \tEstimated time remaining ",est, sep=""))
        } else {
          cat("\r",paste("Progress  ",msg,"% \tEstimated time remaining ",est, sep=""))
        }
      } else{
        if(perc == 100){
          cat("\r",paste("Progress ",msg,"%", sep=""))
          cat("\r"," ")
        } else if (perc < 10){
          cat("\r",paste("Progress   ",msg,"%", sep=""))
        } else {
          cat("\r",paste("Progress  ",msg,"%", sep=""))
        }
      }

    }


####################################################################################

    ## Capitalize text expression

    capitalize <- function(x) {

      if(length(x)==1){
        s <- strsplit(x, " ")[[1]]
        out = paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse=" ")
      } else{
        out = numeric(0)
        for(i in 1:length(x)){
          s <- strsplit(x[i], " ")[[1]]
          out[i] = paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse=" ")
        }
      }
      return(out)
    }

####################################################################################

    ## Check if an input vector has length=0

    is.empty <- function(x){
      return(length(x)==0)
    }
