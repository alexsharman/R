#StopWATCH ####
#A Atopwatch R6class object you can use in your R scprits for timing and benchmarking of your code
#Dependencies: Lubridate, Hms, R6
#Alex Sh. 2018-04-05
library(R6)

StopWatch <- R6Class(classname = "StopWatch",
                      public = list(
                        

                        initialize = function() {
                            private$hasStarted <- FALSE
                            private$hasEnded <- FALSE
                            print("use $start to start the timer")
                        },

                        start = function() {
                            if (isTRUE(self$hasStarted)) {
                                print("StpWatch allready started!")
                                return(NULL)
                            } else {
                                private$times <- list()
                                hasEnded <- FALSE
                                private$addT()
                                cat(paste0("Started running at ", private$formattedCurrTime()))
                                private$hasStarted <- TRUE
                                private$hasEnded <- FALSE
                            }
                        },

                        end = function() {
                            if (isTRUE(self$hasEnded)) {
                                print("StpWatch allready Ended!")
                                return(NULL)
                            } else {
                                private$addT()
                                cat(paste0("Finished running at ", private$formattedCurrTime(), " and took in total: ", private$timeAsFormattedString(private$totalElapsedTime())))
                                private$hasEnded <- TRUE
                                private$hasStarted <- FALSE
                            }
                        },

                        endLap = function() {
                            if (isTRUE(self$hasEnded)) {
                                print("StpWatch allready Ended!")
                                return(NULL)
                            } else {
                                private$addT()
                                cat("Lap run in: ", paste0(private$timeAsFormattedString(private$lastFinishedLapTime())))
                            }
                        }

                        ),
                      private = list(
                        hasStarted = NULL,
                        hasEnded = NULL,
                        times = list(),

                        totalElapsedTime = function() {
                            e <- length(private$times)
                            timeDiff <- (private$times[[e]]) - (private$times[[1]])
                        },

                        lastFinishedLapTime = function() {
                            e <- length(private$times)
                            timeDiff <- (private$times[[e]]) - (private$times[[e - 1]])
                        },

                        formattedCurrTime = function() {
                            paste0(format(Sys.time(), "%H"), ":", format(Sys.time(), "%M"))
                        },

                        timeAsFormattedString = function(timeVal) {
                            paste0(round(lubridate::minute((hms::hms(timeVal))), digits = 2), " mins ", round(lubridate::second((hms::hms(timeVal))), digits = 2), " sec")
                        },

                        addT = function() {
                            private$times[[length(private$times) + 1]] <- Sys.time()
                        }
                      )

                      )
# Example of use
#
# #Start
 ST <- StopWatch$new()
 ST$start()
# 
 ST$endLap()
 ST$endLap()
 ST$endLap()
# #Finish total 
 ST$end()
