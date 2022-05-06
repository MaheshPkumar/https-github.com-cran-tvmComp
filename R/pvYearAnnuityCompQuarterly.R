#' Computing Present Value of Yearly Paid Ordinary Annuity or Annuity Due that is Compounded Quarterly.
#'@details
#'As explained by \insertCite{humm;textual}{tvmComp}, the description of the compounding frequency is usually contained within the phrase that describes the nominal interest rate. An example would be 8 percent compounded quarterly This means that the nominal, or annual, interest rate of 8 percent is compounded four times each year at 8/4 that is 2 percent, each period.
#'So the method \code{pvYearlyAnnuityCompQuarterly()} is developed to Compute Present Value(PV) of Yearly Paid Ordinary Annuity or Annuity Due that is compounded Quarterly.The method gives PV when values are passed to its four arguments. Here \code{r} is annual rate, \code{n} is number of years,\code{pmt} is amount of one annuity and \code{bgn} is the computational mode. (Enter 1 when annuity payment occurs at the beginning of the period; 0 for end of period payments).
#'@param r A number.
#'@param n A number.
#'@param pmt A number.
#'@param bgn A number.
#'@return Input values to four arguments \code{r} , \code{n} , \code{pmt} and \code{bgn}
#'@importFrom Rdpack reprompt
#'@author MaheshP. Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'\insertRef{humm}{tvmComp}
#'@examples
#'pvYearlyAnnuityCompQuarterly(0.11,4,-300,0)
#'pvYearlyAnnuityCompQuarterly(0.11,4,-300,1)
#'pvYearlyAnnuityCompQuarterly(0.11,4,300,0)
#'pvYearlyAnnuityCompQuarterly(0.11,4,300,1)
#'@export
pvYearlyAnnuityCompQuarterly <- function (r, n, pmt,bgn)
{
  if (bgn != 0 && bgn != 1) {
    message("Error: bgn should be 0 or 1")
  }
  else {
        r <-(1+r/4) ^ 4 -1
        (pv_yAnnuityqrt=round((pmt/r * (1 - 1/(1 + r)^n)) * (1 + r)^bgn * (-1), digits=2))
  }
}


