# Olah Data Semarang
# WhatsApp : +6285227746673
# IG : @olahdatasemarang_
# Flexible mediation analysis using natural effect models Use Package medflex With (In) R Software
install.packages("readxl")
install.packages("httr")
install.packages("medflex")
library("httr")
library("readxl")
library("medflex")
# Import Data Excel Into R From Github Olah Data Semarang (timbulwidodostp)
github_link <- "https://github.com/timbulwidodostp/medflex/raw/main/medflex/medflex.xlsx"
temp_file <- tempfile(fileext = ".xlsx")
req <- GET(github_link, 
# authenticate using GITHUB_PAT
authenticate(Sys.getenv("GITHUB_PAT"), ""),
# write result to disk
write_disk(path = temp_file))
medflex <- readxl::read_excel(temp_file)
# Estimate Flexible mediation analysis using natural effect models Use Package medflex With (In) R Software
medflex_fit <- glm(negaff ~ factor(attbin) + gender + educ + age,family = gaussian, data = medflex)
medflex_2 <- neWeight(medflex_fit)
model_medflex <- neModel(UPB ~ attbin0 + attbin1 + gender + educ + age,family = binomial("logit"), expData = medflex_2)

summary(model_medflex)
exp(confint(model_medflex)[c("attbin01", "attbin11"), ])
# Flexible mediation analysis using natural effect models Use Package medflex With (In) R Software
# Olah Data Semarang
# WhatsApp : +6285227746673
# IG : @olahdatasemarang_
# Finished
