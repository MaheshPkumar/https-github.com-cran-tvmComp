#' Computing Present Value of Yearly Paid Ordinary Annuity or Annuity Due that is Compounded Annually.
#'@details
#'Classification of annuities is done by \insertCite{humm;textual}{tvmComp}, using the date of payment. In an Ordinary Annuity, payments are made at the end of each payment period. In an Annuity Due, payments are made at the beginning of each payment period. Loan payments, mortgage payments, and interest payments on bonds are all examples of Ordinary Annuities.
#'The method \code{pvYearlyPaidAnnuityCompYearly()}is developed to compute Present Value(pv) of Yearly-Paid Ordinary Annuity or Annuity Due that is compounded Annually and gives PResent Value when values are passed to its four arguments. Here \code{r} is annual rate, \code{n} is number of years, \code{pmt} is amount of one annuity and \code{bgn} is the computational mode. (Enter 1 when annuity payment occurs at the beginning of the period; 0 for end of period payments).
#'@param r A number.
#'@param n A number.
#'@param pmt A number.
#'@param bgn A number.
#'@return Input values to four arguments \code{r} , \code{n} , \code{pmt} and \code{bgn}
#'@importFrom Rdpack reprompt
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'\insertRef{humm}{tvmComp}
#'@examples
#'pvYearlyPaidAnnuityCompYearly(0.06,12, -200, 1)
#'pvYearlyPaidAnnuityCompYearly(0.06,12, -200, 0)
#'pvYearlyPaidAnnuityCompYearly(0.06,12, 200, 1)
#'pvYearlyPaidAnnuityCompYearly(0.06,12, 200, 0)
#'@export
pvYearlyPaidAnnuityCompYearly <- function (r, n, pmt,bgn)
{
  if (bgn != 0 && bgn != 1) {
    message("Error: bgn should be 0 or 1")
  }
  else {
    (pv_yAnnuity = round((pmt/r * (1 - 1/(1 + r)^n)) * (1 + r)^bgn * (-1), digits=2))
  }
}


