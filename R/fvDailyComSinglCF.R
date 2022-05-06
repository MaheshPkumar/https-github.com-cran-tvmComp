#'Calculates Future Value of a Single Cash Flow for Daily compounding.
#'@details
#'As described by \insertCite{tman;textual}{tvmComp}, Time Value of Money calculations are essentially comparisons between Present Value, that is what a cash flow is worth to you today, and Future Value, that is what a cash flow will be worth in the future. In addition, this Future Value is impacted by the frequency of compounding. In most of the situations the compounding period is an year in length. However, this is not always true. For instance, when banks offer saving accounts that Compound Interest every day, month, or quarter. Depositor in this scenario should prefer more frequent compounding because more interest is earned when more is compounded more frequently. Most common compounding situation includes daily, monthly, quarterly, and yearly compounding. Based on this, method \code{fvDailyCompSingleCF()} calculates Future Value of a Single Cash Flow for daily compounding scenario. The method \code{fvDailyCompSingleCF()} gives \code{fv} when values are passed to its three arguments. Here \code{r}, is annual rate, \code{n} is number of years and \code{pv} is Present Value.
#'The output gives Future Value of a Single Cash Flow for daily compounding scenario, that is rounded off to two decimal places. A minus value like -4450.69 represents Outflow of the Cash. Whereas a positive value like 4450.69 represents an Inflow of the cash.
#'@param r A number.
#'@param n A number.
#'@param pv A number.
#'@return Input values to three arguments  \code{r} , \code{n} and \code{pv}.
#'@importFrom Rdpack reprompt
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'\insertAllCited{}
#'@examples
#'fvDailyCompSingleCF(0.08,10,-2000)
#'fvDailyCompSingleCF(0.08,10,2000)
#'@export
fvDailyCompSingleCF <- function (r, n, pv)
{
  fv_DailySingleCF <- ((pv * (1 + r/365)^(n*365)) * (-1))
  (fv_DailySingleCF = round(fv_DailySingleCF, digits=2))
}


