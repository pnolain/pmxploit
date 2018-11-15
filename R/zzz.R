.onLoad <- function(libname, pkgname) {
  op <- options()
  op.pmxploit <- list(
    pmxploit.dvvspred.identityline.colour = "red",
    pmxploit.dvvspred.identityline.linetype = "dashed",
    pmxploit.dvvspred.smooth.size = 1.2,
    pmxploit.dvvspred.smooth.colour = "blue",
    pmxploit.dvvspred.dv.alpha = 0.3,

    pmxploit.residualsplot.smooth.size = 1.2,
    pmxploit.residualsplot.mdv.shape = 8,
    pmxploit.residualsplot.alpha = 0.3,
    pmxploit.residualsplot.empiricaldistribution.colour = "black",
    pmxploit.residualsplot.referencedistribution.colour = "red",
    pmxploit.residualsplot.empiricaldistribution.linetype = "solid",
    pmxploit.residualsplot.referencedistribution.linetype = "dashed",

    pmxploit.parametersdistributionsplot.empiricaldistribution.colour = "black",
    pmxploit.parametersdistributionsplot.referencedistribution.colour = "red",
    pmxploit.parametersdistributionsplot.empiricaldistribution.linetype = "solid",
    pmxploit.parametersdistributionsplot.referencedistribution.linetype = "dashed",

    pmxploit.indivplot.dv.size = 3,
    pmxploit.indivplot.dv.shape = 17,
    pmxploit.indivplot.mdv.shape = 8,
    pmxploit.indivplot.prediction.size = 3,

    pmxploit.spaghettiplot.dv.size = 3,
    pmxploit.spaghettiplot.dv.shape = 16,
    pmxploit.spaghettiplot.mdv.colour = "red",
    pmxploit.spaghettiplot.mdv.shape = 8,
    pmxploit.spaghettiplot.alpha = 0.3,

    pmxploit.correlationplot.digits = 3,
    pmxploit.correlationplot.text_color = "#073642",
    pmxploit.correlationplot.dark_color = "#00A65A",
    pmxploit.correlationplot.dark_color2 = "red",
    pmxploit.correlationplot.bandwidth = 10,

    pmxploit.iterationplot.saem.linetype = "dashed",
    pmxploit.iterationplot.saem.colour = "red"
  )

  toset <- !(names(op.pmxploit) %in% names(op))
  if (any(toset)) options(op.pmxploit[toset])

  invisible()
}


# Remove CRAN note on no visible binding for global variable
utils::globalVariables(c("."))
