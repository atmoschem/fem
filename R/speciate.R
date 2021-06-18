#' Speciation of emissions
#'
#' @description \code{speciate} separates emissions in different compounds.
#' It covers black carbon and organic matter from particulate matter. Soon it
#' will be added more speciations
#'
#' @param x Emissions estimation
#' @param spec Character any of "GAS" or "PM"
#' @param code Character any of "01_petroleum", "02_portland_cement", "03_paper_cellulose",
#' "04_bagasse", "05_natural_gas", "06_coal" OR "07_fuel_oil"
#' @param usepa_code Character any of "4713", "3011", "8799", "SUG02", "2442", "1185", "0001",
#' "13505C", "91127", "4733", "SUGP02", "1120310" OR "91114"
#' @param list when TRUE returns a list with number of elements of the list as the number species of pollutants
#' @return dataframe or list
#' @importFrom data.table rbindlist
#'
#' @references
#' Simon, H., Beck, L., Bhave, P. V., Divita, F., Hsu, Y., Luecken, D., ...
#' Strum, M. (2010). The development and uses of EPA SPECIATE database.
#' Atmospheric Pollution Research, 1(4), 196-206.
#' @export
#' @examples {
#' # Do not run
#' pm <- rnorm(n = 100, mean = 400, sd = 2)
#' (df <- spec_industry(pm, code = "01_petroleum"))
#' }
spec_industry <- function(x,
                     spec = "GAS",
                     code,
                     usepa_code,
                     list = FALSE) {
  if(!spec %in% c("GAS", "PM")) {
    cat("Choose any of \n")
    print(c("GAS", "PM"))
    stop("Wrong spec")
  }

  df <- sysdata$spec
  df <- df[df$type == spec, ]

  if(!missing(code) & !missing(usepa_code)) stop("Choose code OR usepa_code")
  if (!missing(code)) {
    codes <- unique(df$brazil_profile)
    if(!code %in% codes) {
      cat("Choose any of \n")
      print(codes)
      stop("Wrong code")
    }
    df <- df[df$brazil_profile == code, ]
  }

  if (!missing(usepa_code)) {
    codes <- unique(df$PROFILE_CODE)
    if(!usepa_code %in% codes) {
      cat("Choose any of \n")
      print(codes)
      stop("Wrong code")
    }
    df <- df[df$PROFILE_CODE == usepa_code, ]
  }

  dfx <- split(df, df$species)


  if (list) {
    dfb <- lapply(seq_along(dfx), function(i) {
      dfx[[i]]$x * x
    })
    names(dfb) <- names(dfx)
  } else {
    dfb <- lapply(seq_along(dfx), function(i) {
      data.frame(x = dfx[[i]]$x * x,
                 species = names(dfx)[i])
    })
    dfb <- data.table::rbindlist(dfb)
  }


  return(dfb)
}
