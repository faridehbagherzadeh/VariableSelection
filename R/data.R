#' @title tlgs Data from Tehran Lipid and Glocuse Study.
#'
#'  @description This is a stratified subsample of pre-diabetic women, aged 20 years and over from the TLGS cohort to predict diabetes outcome. The original data contains hundreds of variables of which 11 are included in this package.
#'  Details of the TLGS design have been published before (Azizi and et al. (2000, 2002, 2009)); briefly, it is a population based study performed on a representative sample of residents of Tehran (n=15005, \eqn{aged \ge 3yrs}), who entered the first phase of the study in 1999-2001. Data on subjects was collected through interview, physical examination and blood sampling at baseline, a process which was repeated every three years; in the second phase, 3500 new subjects were added to the study and followed for the two next phases.
#'  Pre-diabetes was defined based on fasting plasma glucose
#'  \eqn{(FPG) \ge 100} and \eqn{FPG \le 126} mg /dl or 2h post-challenge plasma glucose \eqn{(2h PG) \ge 140} and \eqn{2h PG \le 200} mg/dl. Study subjects taking anti-diabetic drugs at baseline were excluded from the study. Type 2 diabetics included subjects with \eqn{FPG \ge 126} or \eqn{2h PG \ge} 200 or taking anti-diabetic medication in any follow-up period.
#'  @usage data(tlgs)
#'  @format A data frame with 100 observations (35 diabetic, 65 nondiabetic). There are 11 predictor variables measured for each individual.
#' \describe{
#' \item{Outcome}{Outcome ("diabetic", "nondiabetic").}
#' \item{Age}{Age (years).}
#' \item{Allsty}{Total length of residence in Tehran (years).}
#' \item{Bs2hr}{2-h post-challange plasma glucose (mg/dl).}
#' \item{TC}{Total cholesterol (mg/dl).}
#' \item{Cr}{Creatinine (mg/dl).}
#' \item{FPG}{Fasting plasma glucose (mg/dl).}
#' \item{HDL-C}{High density lipoprotein cholesterol (mg/dl).}
#' \item{Height}{Height (cm).}
#' \item{HipC}{Hip circumference (cm).}
#' \item{Acedrg}{Use of angiotensin-converting-enzyme (ACE) inhibitor drugs.}
#' \item{DBP}{Diastolic blood pressure (mm Hg).}
#' }
#' @source
#'  Azizi, F., Madjid, M., Rahmani, M., Emami, H., Mirmiran, P. A. R. V. I. N., & Hadjipour, R. (2000). Tehran Lipid and Glucose Study (TLGS): rationale and design. \emph{Iranian journal of endocrinology and metabolism}, \bold{2}(2), 77--86.
#'
#'  Azizi, F., Rahmani, M., Emami, H., Mirmiran, P. A. R. V. I. N., Hajipour, R., Madjid, M., ... & Moeini, S. (2002). Cardiovascular risk factors in an Iranian urban population: Tehran lipid and glucose study (phase 1). \emph{Sozial-und praventivmedizin}, \bold{47}(6), 408--426.
#'
#'  Azizi, F., Ghanbarian, A., Momenan, A. A., Hadaegh, F., Mirmiran, P., Hedayati, M., ... & Zahedi-Asl, S. (2009). Prevention of non-communicable disease in a population in nutrition transition: Tehran Lipid and Glucose Study phase II. \emph{Trials}, \bold{10}(1), 5.
#'
#'  @references
#'  Azizi, F., Madjid, M., Rahmani, M., Emami, H., Mirmiran, P. A. R. V. I. N., & Hadjipour, R. (2000). Tehran Lipid and Glucose Study (TLGS): rationale and design. \emph{Iranian journal of endocrinology and metabolism}, \bold{2}(2), 77--86.
#'
#'  Azizi, F., Rahmani, M., Emami, H., Mirmiran, P. A. R. V. I. N., Hajipour, R., Madjid, M., ... & Moeini, S. (2002). Cardiovascular risk factors in an Iranian urban population: Tehran lipid and glucose study (phase 1). \emph{Sozial-und praventivmedizin}, \bold{47}(6), 408--426.
#'
#'  Azizi, F., Ghanbarian, A., Momenan, A. A., Hadaegh, F., Mirmiran, P., Hedayati, M., ... & Zahedi-Asl, S. (2009). Prevention of non-communicable disease in a population in nutrition transition: Tehran Lipid and Glucose Study phase II. \emph{Trials}, \bold{10}(1), 5.
#'
#'  @examples
#'  library(VariableSelection)
#'  data(tlgs)
#'  summary(tlgs)
#'
"tlgs"
