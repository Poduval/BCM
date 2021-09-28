#' bcm_programs
#' @description It returns the list of programs offered by the college.
#' @examples
#' bcm_programs()
#' @import dplyr
#' @export
#' 
bcm_programs <- function() {
  bcmdata %>% 
    group_by(Program) %>% 
    summarise(NoOfPapers = n_distinct(Paper)) %>% 
    ungroup() %>% 
    arrange(Program) -> pr
  structure(list(programs = pr), class = "bcmprogram")
}

#' print.bcmprogram
#' @description It prints the list of programs offered by the college.
#' @param x an object of class bcmprogram
#' @import dplyr
#' @export
#' 
print.bcmprogram <- function(x) {
  cat("================================================================")
  cat("\n")
  cat("BCM, Kotayam offers the following programs (number of papers)")
  cat("\n")
  cat("================================================================")
  cat("\n")
  print.data.frame(x$programs)
}

#' bcm_papers
#' @description It returns the list of papers offered by BCM college per semester.
#' @import dplyr
#' @importFrom utils menu
#' @export
#' 
bcm_papers <- function() {
  if(interactive() & nrow(bcmdata) > 1) {
    choice <- utils::menu(bcm_programs()$programs$Program, title = "Choose one program", 
                          graphics = TRUE)
    program <- bcm_programs()$programs$Program[choice]
  } else program <- "Statistics"
  
  bcmdata %>% 
    filter(Program == program) %>% 
    select(Semester, PaperCode, Paper, PaperCredit) %>% 
    distinct(Semester, Paper, .keep_all = TRUE) -> pp
  
  structure(list(papers = pp), program = program, class = "bcmpapers")
}

#' print.bcmpapers
#' @description It prints the list of papers offered by BCM college per semester.
#' @param x an object of class bcmpapers
#' @import dplyr
#' @export
#' 
print.bcmpapers <- function(x) {
  
  x$papers %>% 
    group_by(Semester) %>% 
    summarise(TotalCreditScore = sum(PaperCredit)) -> pc
  
  cat("================================================================")
  cat("\n")
  cat(paste0("BCM, Kotayam offers the following papers for: ", attr(x, "program")))
  cat("\n")
  cat("================================================================")
  cat("\n")
  cat("Total number of papers per semester -->")
  cat("\n")
  cat("-------------------------------------")
  cat("\n")
  print.data.frame(pc)
  cat("\n")
  cat("Details of the papers -->")
  cat("\n")
  cat("-------------------------------------")
  cat("\n")
  print.data.frame(x$papers)
}

#' bcm_results
#' @description It return the result for each year every year.
#' @import dplyr tidyr
#' @importFrom utils menu
#' @export
#' 
bcm_results <- function() {
  
  if(interactive() & nrow(bcmdata) > 1) {
    choice <- utils::menu(bcm_programs()$programs$Program, title = "Choose one program", 
                          graphics = TRUE)
    program <- bcm_programs()$programs$Program[choice]
  } else program <- "Statistics"
  
  if(interactive() & nrow(bcmdata) > 1) {
    choice_for_years <- sort(unique(bcmdata$Year))
    choice <- utils::menu(choice_for_years, title = "Choose year", 
                          graphics = TRUE)
    year <- choice_for_years[choice]
  } else year <- 2017
  
  bcmdata %>% 
    filter(Program == program, Year == year) %>% 
    group_by(Grade, Paper) %>% 
    summarise(N = n_distinct(StudentId)) %>% 
    ungroup() %>% 
    tidyr::spread(Grade, N, fill = 0) %>% 
    mutate(NoOfStudents = rowSums(select(., -1))) -> rs
  
  structure(list(result = rs), program = program, class = "bcmresult")
}

#' print.bcmresult
#' @description It print the result for each year every year.
#' @param x an object of class bcmresult 
#' @import dplyr
#' @export
#' 
print.bcmresult <- function(x) {
  cat("================================================================")
  cat("\n")
  cat(paste0("Results for the Program: ", attr(x, "program")))
  cat("\n")
  cat("================================================================")
  cat("\n")
  print.data.frame(x$result)
}

