# fitnessRun_function Oct23_2018_FiTnEss_separate Oct23_2018_FiTnEss

# 4. function to calculate parameters using Nta=median(Nta)

#' Main function for the FiTnEss library.
#'
#' This function calls the various workers inside FiTnEss.
#'
#' @param strain Name of the bacterial strain being analyzed.
#' @param file_location Filename(s) of the numbers of reads by position.
#' @param save_location Output .xlsx filename.
#' @param repeat_time Number of times to repeat the parameter calculations.
#' @return An excel file of the results.
#' @export
fitnessRun <- function(strain, file_location, save_location = "fitness_output.xlsx", 
    repeat_time = 3) {
    ## pre-defined as using Nta=10, and replicate for 5 times for each replicate 1.
    ## Usable tally file preparation
    usable_tally_list <- tallyprepfun(strain, file_location)
    ## 2. Calculating parameters
    parameter_list <- calcparafun(strain, usable_tally_list, save_location, rep_time = repeat_time)
    ## 3. Call Essentials
    result_list <- callessfun(file_location, usable_tally_list, parameter_list)
    ## 4. save final results
    write.xlsx(result_list, file = save_location)
    print("Final results saved, finished running.")
}
