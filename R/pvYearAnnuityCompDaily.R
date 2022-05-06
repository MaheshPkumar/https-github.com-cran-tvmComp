#' Computing Present Value of Yearly Paid Ordinary Annuity or Annuity Due that is Compounded Daily.
#'@details
#'As described by \insertCite{tman;textual}{tvmComp}, Time Value of Money calculations are essentially comparisons between Present Value, that is what a cash flow is worth today, and Future Value, that is what a cash flow will be worth in the future. The Future Value is impacted by the frequency of compounding. In most of the Time Value of Money situations the compounding period is an Year in length. However, this is not always true. For instance, banks often offer saving accounts that compound the interest every day, month, or quarter. Depositor, in this scenario, should prefer more frequent compounding because more interest is earned when interest is compounded more frequently. Most common compounding situations include daily, monthly, quarterly and yearly compounding.
#'The method \code{pvYearlyAnnuityCompDaily()} is developed to compute Present Value of Yearly-Paid Ordinary Annuity or Annuity Due that is compounded daily. The method gives PResent Value when values are passed to its four arguments. Here \code{r} is annual rate, \code{n} is number of years,\code{pmt} is amount of one annuity and \code{bgn} is the computational mode. (Enter 1 when annuity payment occurs at the beginning of the period; 0 for end of period payments).
#'@param r A number.
#'@param n A number.
#'@param pmt A number.
#'@param bgn A number.
#'@return Input values to four arguments \code{r} , \code{n} , \code{pmt} and \code{bgn}
#'@importFrom Rdpack reprompt
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'\insertRef{tman}{tvmComp}
#'@examples
#'pvYearlyAnnuityCompDaily(0.11,4,-300,0)
#'pvYearlyAnnuityCompDaily(0.11,4,-300,1)
#'pvYearlyAnnuityCompDaily(0.11,4,300,0)
#'pvYearlyAnnuityCompDaily(0.11,4,300,1)
#'@export
pvYearlyAnnuityCompDaily <- function (r, n, pmt,bgn)
{
  if (bgn != 0 && bgn != 1) {
   message("Error: bgn should be 0 or 1")
  }
  else {
        r <-(1+r/365) ^ 365 -1
        (pv_yAnnuityDaily= round((pmt/r * (1 - 1/(1 + r)^n)) * (1 + r)^bgn * (-1), digits=2))
  }
}


