extract_drive_id <- function(url) {
  # Pattern untuk berbagai format URL Google Drive
  patterns <- c(
    "drive\\.google\\.com/file/d/([^/]+)",           # Format: /file/d/[ID]/
    "drive\\.google\\.com/open\\?id=([^/&]+)",       # Format: ?id=[ID]
    "drive\\.google\\.com/uc\\?id=([^/&]+)",         # Format: uc?id=[ID]
    "docs\\.google\\.com/[^/]+/d/([^/]+)"            # Format: /d/[ID]
  )
  
  # Coba setiap pattern
  for (pattern in patterns) {
    matched <- regexpr(pattern, url, perl = TRUE)
    if (matched != -1) {
      # Ekstrak group pertama (File ID)
      id <- regmatches(url, regexec(pattern, url, perl = TRUE))[[1]][2]
      return(id)
    }
  }
  
  # Jika tidak ada pattern yang cocok
  return(NULL)
}

get_image_quality_data <- function(){
  iqa_sheet_url <- "https://drive.google.com/file/d/18BiJYol8oLAOhwHjN6az6lor1FnEbr6I/view?usp=sharing"
  iqa_file_id <- extract_drive_id(iqa_sheet_url) # Extract file ID

  temp_iqa_path <- tempfile() # Create a temporary file path
  drive_download(as_id(iqa_file_id), path = temp_iqa_path, overwrite = TRUE) # Download the file

  iqa_data <- read.csv(temp_iqa_path) # Read the downloaded file
  unlink(temp_iqa_path) # Remove the temporary file

  return(iqa_data)

}

set.seed(10307)
num_of_student = 350

generate_survey_data <- function(n = num_of_student){
  # survey dates
  dates <- sample(seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day"), n, replace = TRUE)
  # ages
  ages <- sample(c("Under 18","18-24","25-30", "Over 30"), n, replace = TRUE)
  # student ID
  student_ID <- sprintf("1252001%03d", sample(seq(1,n),n,replace=FALSE))
  # Group
  group_ID <- sample(c("A", "B", "C", "D", "E", "F"), n, replace =  TRUE)
  # Department
  dept_ID <- sample(c("Informatika", 
                      "Sistem Informasi", 
                      "Teknik Sipil", 
                      "Teknik Lingkungan", 
                      "ITP", 
                      "Teknik Industri",
                      "Manajemen",
                      "Akuntansi",
                      "Ilmu Komunikasi",
                      "Ilmu Politik"), 
              n, replace =  TRUE)
  # Gender
  sex <- sample(c("Pria", "Wanita"), n, replace = TRUE)
  # Survey: investment decision
  decision <- sample(c( "1 Milyar", 
                        "2 Milyar", 
                        "3 Milyar",
                        "4 Milyar",
                        "5 Milyar"), 
              n, replace =  TRUE)
  # Survey: spending value
  spending <- exp(rnorm(n, log(100000), 1)) + 10000
  
  # Create dataframe
  survey_data <- data.frame(
    survey_date = dates,
    age  = ages,
    gender = sex,
    Group = group_ID,
    NIM = student_ID,
    Major = dept_ID,
    Decision = decision,
    Spending = spending
  )
  return(survey_data)
}

get_demo_data <- function(){
  survey_data <- generate_survey_data()
  image_quality_data <- get_image_quality_data()

  return(list(
    survey_data = survey_data,
    image_quality_data = image_quality_data
  ))
}
