print.theoryeval <-
  function(x,...)
  { if (x$print==TRUE){
    cat("\nData:\n----------\n\n")
    print(x$dat)
    cat("\nCases:\n----------\n\n")
    print(x$cases)
    cat("\nFit:\n----------\n\n")
    print(x$fit)
    cat("\n")}
    else {
      cat("\nCases:\n----------\n\n")
      print(x$cases)
      cat("\nFit:\n----------\n\n")
      print(x$fit)
      cat("\n")}
    }  
