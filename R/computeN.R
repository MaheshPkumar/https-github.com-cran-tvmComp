#'Calculates the required Number of Years for a specific given Present Value to become equal to an amount that of a given Future Value at given Discount Rate.
#'@details
#'As per an in-depth discussion about computational aspects of time value of money scenarios, \insertCite{berk;textual}{tvmComp} points out that most of the time value of money situations involve computation of Present Value or Future Value. However, in case a deposit is made into a bank account, it may require calculating how long it will take before the balance reaches a certain level. To add to this, \insertCite{brook;textual}{tvmComp} interestingly explains the rule of thumb that financiers developed which was known as the Rule of 72. This rule still works quite accurately for interest rates between 2 percent and 30 percent. As per the Rule of 72, just divide 72 by the interest rate to find the length of time it takes to double your money. For example, to double your money at the interest rate of 8 percent, it shall approximately take 9 (72 divided by 8) years. Further, he supports this with his findings that the Rule of 72 is fairly accurate for the middle range of interest rates. It overestimates the time it takes to double below 8 percent and underestimates over 8 percent. However, to accurately solve for required number of years (n) and for a specific given Present Value to become equal to an amount that of a given Future Value at given Discount Rate, the method \code{computingN()} is developed. This method \code{computingN()} gives n (required number of years) when values are passed to its three arguments. Here \code{r} is annual rate, \code{pv} is Present Value and \code{fv} is Future Value.
#'The output gives value of \code{n},the number of years that are rounded off to three decimal places. So, an output of 6.637 means 6.637 Years.
#'@param r A number.
#'@param fv A number.
#'@param pv A number.
#'@return Input values to three arguments  \code{r} , \code{pv} and \code{fv}.
#'@importFrom Rdpack reprompt
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'\insertRef{berk}{tvmComp}
#'@references
#'\insertRef{brook}{tvmComp}
#'@examples
#'computingN(0.08,-1200,2000)
#'computingN(0.08,1200,-2000)
#'computingN(0.08,1200,2000)
#'@export
computingN <- function(r, pv, fv ){
fv <- abs(fv)
pv <- abs(pv)
div <- fv/pv
n <- log(div)/log(1+r)
n <- abs(n)
(n = round(n, digits=3))
}
