# generate_exam.R

#' Generate random parameters for exam questions
#' @param student_id Numeric student ID used as random seed
#' @return List of parameters for exam questions
generate_exam_params <- function(student_id) {
  # Set seed for reproducibility
  set.seed(as.numeric(student_id))
  
  # Question 1 parameters
  q1_params <- list(
    transaction_threshold = sample(seq(400000, 600000, by = 50000), 1),
    cancellation_mean = sample(25:35, 1),
    extra_cancellations = sample(8:12, 1)
  )
  
  # Question 2 parameters
  q2_params <- list(
    efficiency_threshold = sample(seq(90, 97, by = 0.5), 1),
    confidence_level = sample(c(90, 95, 99), 1),
    machine_id = sample(1:10, 1)
  )
  
  # Question 3 parameters
  q3_params <- list(
    defect_rate = sample(seq(1.5, 2.5, by = 0.1), 1),
    alpha_level = sample(c(0.01, 0.05, 0.10), 1),
    batch_size = sample(c(100, 200, 500), 1)
  )
  
  # Question 4 parameters
  q4_params <- list(
    effect_size = sample(seq(0.2, 0.5, by = 0.05), 1),
    group_names = sample(c("Premium vs Standard", "New vs Existing", "Online vs Offline"), 1)
  )
  
  return(list(
    q1 = q1_params,
    q2 = q2_params,
    q3 = q3_params,
    q4 = q4_params
  ))
}

#' Generate exam questions text
#' @param params List of parameters from generate_exam_params
#' @return String containing formatted exam questions
generate_questions_text <- function(params) {
  sprintf("
# Questions

## Question 1: Probability and Discrete Distributions (25 points)

### Part A (10 points)
Using the sales_data dataset:
1. Calculate the probability of transactions with value above IDR %d
2. Create a histogram to visualize the distribution of transaction values
3. Test whether the transaction value distribution follows a Poisson distribution
4. Interpret your findings in a business context

### Part B (15 points)
An online store has an average of %d order cancellations per day.
1. Calculate the probability of exactly %d cancellations in one day
2. Calculate the probability of more than %d cancellations in one day
3. Simulate cancellation data for 365 days and create a visualization
4. Compare the theoretical probabilities with your simulation results

## Question 2: Continuous Distributions (25 points)

### Part A (15 points)
Using the machine_performance dataset for Machine %d:
1. Test the normality of efficiency data using appropriate statistical tests
2. Create a Q-Q plot for visual assessment of normality
3. If the data is not normal, suggest and apply an appropriate transformation
4. Justify your choice of transformation (if applied)

### Part B (10 points)
Assuming the efficiency data follows a normal distribution:
1. Calculate the probability of machine operating above %.1f%% efficiency
2. Find the efficiency interval that contains %d%% of all measurements
3. Create a visualization showing the normal curve and relevant areas
4. Discuss the practical implications of your findings

## Question 3: Single Sample Hypothesis Testing (25 points)

### Part A (15 points)
The company claims that the defect rate is less than %.1f%%:
1. Formulate appropriate null and alternative hypotheses
2. Conduct a hypothesis test using α = %.2f
3. Calculate and interpret the effect size
4. Analyze data in batches of %d units
5. Provide recommendations based on your analysis

### Part B (10 points)
For the defect rate analysis:
1. Calculate and interpret the %d%% confidence interval
2. Create a visualization showing the distribution and confidence interval
3. Explain what the confidence interval means in business terms
4. Discuss the implications for quality control

## Question 4: Two Sample Hypothesis Testing (25 points)

### Part A (15 points)
Compare customer satisfaction between %s groups:
1. Test for an effect size of %.2f
2. Check assumptions and choose appropriate statistical test
3. Conduct the analysis and interpret results
4. Calculate and interpret the effect size

### Part B (10 points)
Extend your analysis:
1. Create appropriate visualizations comparing the groups
2. Calculate and interpret confidence intervals for the difference
3. Provide practical recommendations based on your findings
4. Discuss limitations of your analysis

Remember to:
- Show all R code and outputs
- Include clear interpretations
- Create professional visualizations
- Provide business context for your findings
",
    params$q1$transaction_threshold,
    params$q1$cancellation_mean,
    params$q1$cancellation_mean + params$q1$extra_cancellations,
    params$q1$cancellation_mean + params$q1$extra_cancellations + 5,
    params$q2$machine_id,
    params$q2$efficiency_threshold,
    params$q2$confidence_level,
    params$q3$defect_rate,
    params$q3$alpha_level,
    params$q3$batch_size,
    params$q2$confidence_level,
    params$q4$group_names,
    params$q4$effect_size
  )
}

#' Generate complete exam
#' @param student_id Student ID number
#' @param student_name Student name
#' @return List containing exam text and parameters
generate_exam <- function(student_id, student_name) {
  # Generate parameters
  params <- generate_exam_params(student_id)
  
  # Generate exam text
  exam_text <- generate_questions_text(params)
  
  # Add student info header
  header <- sprintf("
# Final Examination - Statistics and Probability

**Student ID**: %s

**Student Name**: %s

**Date**: %s\n\n

",
student_id, student_name, format(Sys.Date(), "%Y-%m-%d"))
  
  exam_text <- paste(header, exam_text)
  
  return(list(
    exam_text = exam_text,
    params = params
  ))
}

#' Check if answers match parameters
#' @param student_answers Student's R code and answers
#' @param exam_params Parameters used to generate the exam
#' @return List of checking results
check_answers <- function(student_answers, exam_params) {
  # This function would implement answer checking logic
  # Returns TRUE/FALSE for each question part
  # Not implemented in this version
}

#' Generate solution template
#' @param params Exam parameters
#' @return R code template for solutions
generate_solution_template <- function(params) {
  # This function would generate solution code
  # Based on the specific parameters
  # Not implemented in this version
}

# Example usage:
if(FALSE) {  # Not run by default
  # Generate exam for one student
  exam <- generate_exam("12345", "John Doe")
  cat(exam$exam_text)
  
  # Generate exams for multiple students
  students <- data.frame(
    id = c("12345", "12346", "12347"),
    name = c("John Doe", "Jane Smith", "Bob Johnson")
  )
  
  exams <- Map(function(id, name) generate_exam(id, name),
               students$id, students$name)
}
