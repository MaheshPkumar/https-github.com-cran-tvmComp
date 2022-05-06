#' Computing Present Value of Quarterly Paid Ordinary Annuity or Annuity Due that is Compounded Quarterly.
#'@details
#'As explained by \insertCite{brook;textual}{tvmComp}, the way it is possible to find the Present Value of a single cash flow , on the same lines the Present Value of an annuity stream can also be computed.
#'The method \code{pvQuarterlyPaidAnnuityCompQuarterly()} is developed to compute Present Value of Quarterly-Paid Ordinary Annuity or Annuity Due that is Compounded Quarterly.The method gives Present Value when values are passed to its four arguments. Here \code{r} is annual rate, \code{n} is number of years, \code{pmt} is amount of one annuity and \code{bgn} is the computational mode (Enter 1 when annuity payment occurs at the beginning of the period; 0 for end of period payments)
#'@param r A number.
#'@param n A number.
#'@param pmt A number.
#'@param bgn A number.
#'@return Input values to four arguments \code{r} , \code{n} , \code{pmt} and \code{bgn}.
#'@importFrom Rdpack reprompt
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'\insertRef{brook}{tvmComp}
#'@examples
#'pvQuarterlyPaidAnnuityCompQuarterly(0.06,12, -200, 1)
#'pvQuarterlyPaidAnnuityCompQuarterly(0.06,12, -200, 0)
#'pvQuarterlyPaidAnnuityCompQuarterly(0.06,12, 200, 1)
#'pvQuarterlyPaidAnnuityCompQuarterly(0.06,12, 200, 0)
#'@export
pvQuarterlyPaidAnnuityCompQuarterly <- function (r, n, pmt,bgn)
{
   if (bgn != 0 && bgn != 1) {
    message("Error: bgn should be 0 or 1")
  }
  else {
   (pvQtrAnnuityqtr= round((pmt/(r/4) * (1 - 1/(1 + r/4)^(n*4))) * (1 + r)^bgn * (-1), digits=2))
  }
}


