theory.evaluation <-
  function(theory, 
           empirics, 
           outcome,
           sol=1,
           print.data=FALSE)
  {
    Tdata <- theory.data(theory=theory, empirics=empirics, outcome=outcome, sol=sol)
    Tcases <- cases.theory.evaluation(Tdata)
    Tfit <- theory.fit(Tdata)
    T <- list()
    T <- list('print'= print.data,
              'data'= Tdata,
              'cases'= Tcases,
              'fit'= Tfit)
    class(T) <- 'theoryeval'
    return(T)
  }
