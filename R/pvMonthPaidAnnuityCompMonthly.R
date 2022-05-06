#' Computing Present Value of Monthly paid Ordinary Annuity or Annuity Due for Monthly Compounding.
#'@details
#'According to \insertCite{brook;textual}{tvmComp}, not all Annuities are Ordinary (end-of-period) Annuities. Some payments are due at the beginning of the time-period. For example, when paying rent on an apartment, the person applies the rent at the first or beginning of the month, which is more like a prepayment. The rental payment is an Annuity Due, whereas the car payment and mortgage payment are Ordinary Annuities. To make this distinction between the beginning of the month and the end of the month clearer, consider that making a rent payment allows you to use the apartment for the remainder of the month; that is, you are paying at the beginning of the period for the use of the apartment for that period. You are thus paying in advance. With a mortgage payment, you are paying down the principal and paying interest on the loan for the prior month. So, in that case, the mortgage payment applies to the previous month or a payment at the end of the month. You are thus paying in arrears.
#'When dealing with such situation, you can use the method \code{pvMonthlyPaidAnnuityCompMonthly()} that gives Present Value when values are passed to its four arguments. Here \code{r} is annual rate, \code{n} is number of years, \code{pmt} is amount of one annuity and \code{bgn} is the computational mode. (Enter 1, when annuity payment occurs at the beginning of the period; 0 for end of period payments)
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
#'pvMonthlyPaidAnnuityCompMonthly(0.06,12, -200, 1)
#'pvMonthlyPaidAnnuityCompMonthly(0.06,12, -200, 0)
#'pvMonthlyPaidAnnuityCompMonthly(0.06,12, 200, 1)
#'pvMonthlyPaidAnnuityCompMonthly(0.06,12, 200, 0)
#'@export
pvMonthlyPaidAnnuityCompMonthly <- function (r, n, pmt, bgn)
{
   if (bgn != 0 && bgn != 1) {
    message("Error: bgn should be 0 or 1")
  }
  else {
    (pvMonthlyAnnuityMonth=round((pmt/(r/12) * (1 - 1/(1 + r/12)^(n*12))) * (1 + r)^bgn * (-1), digits=2))
  }
}



