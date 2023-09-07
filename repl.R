## This is an R implementation of the top level REPL. The public
## interface is
##
##     repl() for running a read-eval-print loop
##     quit() for exiting the repl.
##
## I believe most major features are at least approximately supported
## except for
##
##     catching a Q in a browser
##     top level callbacks.
##
## Q could be handled either by adding an appropriate restart in
## repl() and modifying the C internals of browser, or, better in the
## long run, by also writing the browser code in R and modifying
## do_browser to use the R version.  Some fiddling with context
## entries is needed for that and to make sure that efficiency of
## return() calls isn't messed up.
##
## Some minor issues:
##
##     return(1) at repl top level does not give an error and probably
##     should.
##
##     In gui's the gui thinks it is busy and so shows the hour glass
##     (Rgui)/spinner(AQUA).  The spinner in AQUA seems to go away
##     after a while.
##
##     Need to set up for i18n/L10n
##
##     Prompts should be determined by options settings and startup
##     flags -- for now a fixed pair of prompts is used.
##
##     readline/input editing isn't quite right on at least some
##     platforms (tty in particular).
##
## Color support in xterms provided by Deepayan Sarkar. This should
## probably be made configurable in some way.

## Determine whether a string contains a complete expression or set of
## expressions separated by semicolons.
inputComplete <- function(text) {
    tryCatch({ parse(text = text, srcfile = FALSE); TRUE },
             error = function(e) {
                 m <- conditionMessage(e)
                 if (length(grep("unexpected end of input", m)) > 0)
                     FALSE
                 else {
                     e$call <- NULL # eat the call
                     stop(e)
                 }
             })
}

## Read a single complete expression or set of expressions separated
## by semicolons.  The result is an expression object of length zero,
## one, or more.  A parse error in any one of the expressions will be
## detected here; initial correct expressions will be ignored.
readOne <- function(prompt = "-->", cprompt = "--+ ",
                    color = FALSE, incol = "\033[0m\033[31m",
                    outcol = "\033[0m\033[1;34m") {
    ## **** This should compute prompts based on option settings and
    ## **** startup flags.
    lines <- NULL
     repeat {
         ## On AQUA if there is output on the current line it seems
         ## that a newline is inserted before entering a read loop in
         ## readLines().  This does not happen with the prompt printed
         ## by readline(), so use readline() on AQUA. On other
         ## platforms use readLines() because it allows EOF detection.
         ## There seems no obvious EOF key combination for AQUA -- at
         ## least ^D and ^Z do not work. On Windows/Rgui using ^Z
         ## works for EOF.  On Windows/Rgui the drawback of using
         ## readLines is that the prompt printed with cat() is blue
         ## rather than red; using readline() gives the right prompt
         ## color but loses EOF detection.
         if (color) cat(incol)
         if (TRUE) { ## (identical(.Platform$GUI, "AQUA")) {
             line <- readline(prompt)
             if (length(line) == 0) # EOF
                 quit()
         }
         else {
             cat(prompt)
             line <- readLines(stdin(), 1, warn = FALSE)
             if (length(line) == 0) # EOF
                 quit()
         }
         ## **** On unix more is needed for readline integration to
         ## **** work properly but I'm not sure what.  This seems to
         ## **** do the trick on AQUA and I think on Rgui.  It doesn't
         ## **** seem useful for Tk.
         if (nzchar(line)) addHistory(line)
         lines <- c(lines, line)
         if (inputComplete(lines)) {
             if (color) cat(outcol)
             return(parse(text = lines, srcfile = NULL))
         }
         prompt <- cprompt
     }
}

addHistory <- function(line) {
    ## **** this used to work but no longer does
    ## .Internal(addhistory(line))
}

setLastValue <- function(value) {
    ## **** this needs an internal implementation to avoid messing uf the REFCNT on value
    ## **** also won't work if the binding doesn't exist yet, so use tryCatch for now
    tryCatch({
        unlockBinding(".Last.value", baseenv())
        assign(".Last.value", value, baseenv())
        lockBinding(".Last.value", baseenv())
    }, error = function(e) NULL)
}

setLastWarning <- function(w) {
    ## **** this needs an internal implementation to bypass env/binding locking
    ## assign("last.warning", new, baseenv())
}

## This should provide a query option, like q(save = "default").  For
## such an option the 'c' choice should probably invoke an "abort"
## restart, or perhaps use the same restart as the browser's Q.
quit <- function(status = 0) invokeRestart("done", status)

repl <- function(prompt = "--> ", cprompt = "--+ ", greeting = TRUE,
                 color = (Sys.getenv("TERM") == "xterm")) {
    ## The default error handler goes to the first "abort" or
    ## "browser" restart on the stack so we don't need a handler here
    ## at this time; eventually it would be useful to move a lot of
    ## the complicated default error handling logic out of the
    ## internals to R-level code though.
    ##
    ## **** In principle we should set up some sort of top level
    ## **** restart target for Q in browser() to use. A browser repl
    ## **** also needs to keep track of the level and provide a
    ## **** 'continue' restart or some such.

    ## These are global variables used by the following functions to
    ## manage the handling or warnings.
    warningCount <- 0
    warn <- getOption("warn")

    warningHandler <- function(w) {
        if (warn < 0)
            invokeRestart("muffleWarning")
        else if (warn == 0) {
            if (conditionCall(w)[[1]] == "eval.with.vis") ## for toplevel calls
                w$call <- NULL
            new <- list(conditionCall(w))
            names(new)[[1]] <- conditionMessage(w)
            if (warningCount < 50) {
                if (warningCount > 0)
                    new <- c(last.warning, new)
                setLastWarning(new)
                warningCount <<- warningCount + 1
            }
            invokeRestart("muffleWarning") ## to skip internal handling
        }
        else if (warn == 1) {
            if (is.null(conditionCall(w)) ||
                conditionCall(w)[[1]] == "eval.with.vis") ## for toplevel calls
                cat("Warning:", conditionMessage(w), "\n", file = stderr())
            else
                ## **** need to truncate if long
                cat("Warning in", deparse(conditionCall(w)), ":",
                    conditionMessage(w), "\n", file = stderr())
            invokeRestart("muffleWarning")
        }
        else {
            if (conditionCall(w)[[1]] == "eval.with.vis") ## for toplevel calls
                w$call <- NULL
            stop(w)
        }
    }

    printWarnings <- function() {
        if (warningCount > 0) {
            if (warningCount <= 10)
                ## **** it would be better to be able to print
                ## **** directly to stdout without using sink()
                ## **** (which is what capture.output does).
                cat(capture.output(print(warnings())), sep = "\n",
                    file = stderr())
            else if (warningCount < 50)
                cat("There were", warningCount, "warnings",
                    "(use warnings() to see them)\n",
                    file = stderr())
            else
                cat("There were 50 or more warnings",
                    "(use warnings() to see the first 50)\n",
                    file = stderr())
        }
    }
    
    repl1 <- function() {
        ## Look up the warn option at the beginning of each top level
        ## iteration.
        warn <<- getOption("warn")

        ## This handles warnings in the parse (if this is possible)
        ## separately.
        warningCount <<- 0
        exprList <- readOne(prompt = prompt, cprompt = cprompt, color = color)
        printWarnings()

        ## A loop is used since exprList can contain zero, one, or
        ## more expressions.
        for (expr in exprList) {
            warningCount <<- 0
            result <- withVisible(eval(expr, .GlobalEnv))
            setLastValue(result$value)
            if (result$visible)
                print(result$value)
            printWarnings()
            result$value <- NULL ## to drop the reference count
            ## **** add top level handlers here (or outside the for ()?)
        }
    }

    if (greeting)
        ## **** print standard greeting here
        cat("R read-eval-print loop written in R\n",
            "Type 'quit()' to exit.\n\n", sep = "")

    ## Each REPL iteration is executed in a context that establishes
    ##
    ##     an "abort" restart; this is the restart invoked by the
    ##     default error handler
    ##
    ##     a calling handler warnings to implement the REPL warning
    ##     policy
    ##
    ## The REPL loop is executed in a context that establishes a
    ## restart, called "done", that is used by the exit function
    ## quit().
    withRestarts(repeat
                 withCallingHandlers(withRestarts(repl1(),
                                                  abort = function() NULL),
                                     warning = warningHandler),
                 done = function(status) status)
    if (color) cat("\n\033[0m") #??
}
