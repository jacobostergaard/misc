####################################################################################
##
## Some misc tools that are of special use
##
####################################################################################



####################################################################################


.onLoad <- function(lib, pkg) {
  ## Path to iCloud drive
  assign("icloud", "~/Library/Mobile Documents/com~apple~CloudDocs/", envir = .GlobalEnv)
}

####################################################################################



icloud_lib <- function(libname){
  ## Name a folder in the icloud drive
  paste0(icloud,libname)
}


####################################################################################


## Clear console and plots, and remove variables from workspace unless given as arguments
    # clean_slate <- function(...){
    #   for(i in dev.list()) dev.off(which=i)
    #   keep = as.list(match.call())
    #   rm(list=setdiff(ls(envir = .GlobalEnv), keep), envir = .GlobalEnv)
    #   assign("savePDF", FALSE, envir = .GlobalEnv)
    #   assign("icloud", "~/Library/Mobile Documents/com~apple~CloudDocs/", envir = .GlobalEnv)
    #   cat("\014")  # Clear console
    # }

  ## New name!

    clean_up <- function(...){
      ## Clear console and plots, and remove variables from workspace unless given as arguments
      for(i in dev.list()) dev.off(which=i)
      keep = as.list(match.call())
      rm(list=setdiff(ls(envir = .GlobalEnv), keep), envir = .GlobalEnv)
      # assign("savePDF", FALSE, envir = .GlobalEnv)
      assign("icloud", "~/Library/Mobile Documents/com~apple~CloudDocs/", envir = .GlobalEnv)
      cat("\014")  # Clear console
    }


####################################################################################
    clear_console <- function(){
      cat("\014")  # Clear console
    }

####################################################################################


    add.alpha <- function(col, alpha=1){
      ## Add an alpha value to a color
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

    notify <- function(msg, device = NULL){
      # Pushover email: vyer78ud7h@pomail.net
      suppressMessages(pushoverr::set_pushover_app("avy6wjvgqsbfpodjr14xduaph52gsc"))
      suppressMessages(pushoverr::set_pushover_user("uc6dn1q1b87vbsko5okxqeh8tjzepi"))
      pushoverr::pushover_quiet(msg, device = device)
    }


####################################################################################



    displayprogress <- function(perc,est=NULL){
      ## Loop timer display
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

    # cat("\n")
    }


####################################################################################


    capitalize <- function(x) {
      ## Capitalize text expression
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

    is.empty <- function(x){
      ## Check if an input vector has length=0
      return(length(x)==0)
    }


####################################################################################


    set_cobalt_plot_bg <- function(getcol=FALSE){
      ## Cobalt theme background color
      par(bg="#002240", col.lab="#FFFFFF", col.axis="#FFFFFF", col.main="#FFFFFF", col.sub="#FFFFFF", col="#FFFFFF", fg="#FFFFFF")
      if(getcol){
        print("#002240")
      }
    }

    set_normal_plot_bg <- function(){
      ## Standard plot colors
      par(bg="#FFFFFF", col.lab="#000000", col.axis="#000000", col.main="#000000", col.sub="#000000", col="#000000", fg="#000000")
    }

####################################################################################


    `%notin%` <- function(x,y){
      ## Not in set function
      return(!(x %in% y))
      }

####################################################################################

    add_legend <- function(...) {
      ## Add a legend outside the plot region
      opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0),
                  mar=c(0, 0, 0, 0), new=TRUE)
      on.exit(par(opar))
      plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
      legend(...)
    }


####################################################################################

    write_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))


####################################################################################

    quiet <- function(code, nowarnings=TRUE){
      sink("NUL") # use /dev/null in UNIX
      tmp = NULL
      try({
        if(nowarnings){
          suppressWarnings({tmp = code})
        } else{
          tmp = code
        }
      })

      sink()
      return(tmp)
    }


