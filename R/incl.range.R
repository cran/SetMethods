incl.range <-
  function(
    data,
    step = 0.1,
    max.runs = 20,
    outcome,
    conditions,
    incl.cut = 1,
    n.cut = 1,
    include = "",
    dir.exp = "",
    ...
  )
  {
    
    suppressWarnings(init.sol <- minimize(data = data,
                                          outcome  = outcome,
                                          conditions = conditions,
                                          incl.cut = incl.cut,
                                          n.cut = n.cut,
                                          include = include,
                                          dir.exp = dir.exp))
    # Test range raw consistency threshold lower:
    suppressWarnings(sol <- minimize(data = data,
                                     outcome  = outcome,
                                     conditions = conditions,
                                     incl.cut = incl.cut,
                                     n.cut = n.cut,
                                     include = include,
                                     dir.exp = dir.exp))
    if (is.null(init.sol$i.sol)) {
      is = init.sol$solution[[1]]
      s = sol$solution[[1]]
    }
    else {
      is = init.sol$i.sol$C1P1$solution[[1]]
      s = sol$i.sol$C1P1$solution[[1]]
    }
    incl.cut.tl = incl.cut
    while (setequal(is,s))
    { print("Searching for thresholds, this takes me a while for now, sorry...")
      incl.cut.tl = incl.cut.tl - step
      sol <- suppressWarnings(minimize(data = data,
                                       outcome  = outcome,
                                       conditions = conditions,
                                       incl.cut = incl.cut.tl,
                                       n.cut = n.cut,
                                       include = include,
                                       dir.exp = dir.exp))
      if (is.null(init.sol$i.sol)) {
        s = sol$solution[[1]]
      }
      else {
        s = sol$i.sol$C1P1$solution[[1]]
      }
    }
    
    # Test range raw consistency threshold upper:
    suppressWarnings(sol <- minimize(data = data,
                                     outcome  = outcome,
                                     conditions = conditions,
                                     incl.cut = incl.cut,
                                     n.cut = n.cut,
                                     include = include,
                                     dir.exp = dir.exp))
    if (is.null(init.sol$i.sol)) {
      is = init.sol$solution[[1]]
      s = sol$solution[[1]]
    }
    else {
      is = init.sol$i.sol$C1P1$solution[[1]]
      s = sol$i.sol$C1P1$solution[[1]]
    }
    incl.cut.tu = incl.cut
    while (setequal(is,s))
    { print("Searching for thresholds, this takes me a while for now, sorry...")
      incl.cut.tu = incl.cut.tu + step
      sol <- try(suppressWarnings(minimize(data = data,
                                           outcome  = outcome,
                                           conditions = conditions,
                                           incl.cut = incl.cut.tu,
                                           n.cut = n.cut,
                                           include = include,
                                           dir.exp = dir.exp,
                                           ...)), silent = TRUE)
      if (class(sol) == "try-error") {break}
      if (is.null(init.sol$i.sol)) {
        s = sol$solution[[1]]
      }
      else {
        s = sol$i.sol$C1P1$solution[[1]]
      }
    }
    RCT = c(incl.cut.tl+step, incl.cut.tu-step)
    TH <- data.frame(RCT)
    row.names(TH) <- c("Lower bound", "Upper bound")
    cat(c("Raw Consistency T.: ","Lower bound ", incl.cut.tl+step, "Threshold ", incl.cut , "Upper bound ", incl.cut.tu - step, "\n"))
    invisible(TH)
  }  
