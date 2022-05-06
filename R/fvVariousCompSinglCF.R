#'Calculates Future Value of Single Cash Flow for various Compounding Frequencies.
#'@details
#'The method \code{fvVariousCompSingleCF()} is developed to calculate Future Value of Single Cash Flow for various Compounding Frequencies. The gives Future Value when values are passed to its four arguments. Here \code{r} is annual rate, \code{n} is number of years, \code{pv} is Present Value and \code{cfreq} is Compounding Frequency value that must be either 1 or 2 or 4 or 12 or 365.
#'@param r A number.
#'@param n A number.
#'@param pv A number.
#'@param cfreq A number.
#'@return Input values to four arguments \code{r} , \code{n} , \code{pv} and \code{cfreq}.
#'@importFrom Rdpack reprompt
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'\insertRef{humm}{tvmComp}
#'@examples
#'fvVariousCompSingleCF( 0.08,10,2000,2)
#'fvVariousCompSingleCF( 0.08,10,2000,4)
#'fvVariousCompSingleCF( 0.08,10,2000,12)
#'fvVariousCompSingleCF( 0.08,10,2000,365)
#'@export
fvVariousCompSingleCF <- function (r, n, pv,cfreq)
{
   if (cfreq != 1 && cfreq != 2 && cfreq != 4 && cfreq != 12 && cfreq != 365) {
    message("Error: not a valid compounding frequency")
  }
  else {
    (fv_VariousSCF=round(((pv * (1 + r/cfreq)^(n*cfreq)) * (-1)), digits=2))
  }
}




