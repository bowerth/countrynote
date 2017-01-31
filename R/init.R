#' @importFrom magrittr %>%
#' @export

init <- function() {
  load_rdata()
  load_csv()
  load_xlsx()

  icioIndic_reg <<- list_region()
  ui.icioIndic.indic <<- list_indic()
  ui.icioIndic.chart <<- names(ui.icioIndic.indic)
  ui.icioIndic.year <<- c(1995, 2009)

  return("loaded data")
}

load_rdata <- function() {
  data(icioIndic_data)
  icioIndic_data <<-
    icioIndic_data %>%
    data.table::data.table()

  env <- new.env()
  data(stanDim, package = "stan", envir = env)
  ui.icioIndic.cou <<-
    env$STAN.COU[["ICIO"]]
}

load_csv <- function() {
  icioIndic_nameind <<-
    system.file("extdata", "icioIndic_nameind.csv", package = "countrynote") %>%
    read.csv()
  icioIndic_namereg <<-
    system.file("extdata", "icioIndic_namereg.csv", package = "countrynote") %>%
    read.csv()
}

load_xlsx <- function() {
  icioIndic_cntext <<-
    system.file("extdata", "icioIndic_custom_text.xlsx", package = "countrynote") %>%
    XLConnect::readWorksheetFromFile(sheet=1)
}

list_region <- function() {
  icioIndic_reg <- list()
  icioIndic_reg[["oecd"]] <- as.character(icioIndic_namereg$cou[icioIndic_namereg$inoecd==1])
  icioIndic_reg.label <-
    data.frame(
      label = c("EU28", "Americas", "East Asia", "ASEAN", "Ot.ASIA-PACIFIC", "OtherEUR", "Rest of the World"),
      reg = c("eu28", "americas", "eastasia", "asean", "asiapacific", "othereu28", "restoftheworld"),
      stringsAsFactors = FALSE
    )
  ## i
  for (i in seq(along = icioIndic_reg.label$label)) {
    reg <- as.character(icioIndic_reg.label$reg[i])
    icioIndic_reg[[reg]] <- as.character(icioIndic_namereg$cou[icioIndic_namereg$region==as.character(icioIndic_reg.label$label[i])])
  }
  return(icioIndic_reg)
}

list_indic <- function() {
  ui.icioIndic.indic <-
    list(
      exgrdvaex = "EXGRDVA_EX",
      exgrdva = "EXGRDVA",
      exgrfvash = c("EXGR_FVASH", "EXGR_DDCSH", "EXGR_IDCSH", "EXGR_RIMSH"),
      rei = "REI",
      exgrvabsci = "EXGR_VA_BSCI",
      fddvash = c("FDFVASH", "FDDVASH", "EXGRSH", "IMGRSH"),
      tsvafd = c("TSVAFD", "TSGR"),
      servvagr = c("EXGR", "EXGR_DDC_SV", "EXGR_IDC_SV", "EXGR_RIM_SV",
                   "EXGR_FVA_SV"),
      servvagrind = c("EXGR", "EXGR_DDC_SV", "EXGR_IDC_SV", "EXGR_RIM_SV",
                      "EXGR_FVA_SV")
    )
  return(ui.icioIndic.indic)
}
