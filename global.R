source('configure.R')
source('functions.R')

gs4_deauth()

list[graduateDf, intergraduateDf, undergraduateDf, postdocDf] <- clean_submission_2025(googlesheets4::read_sheet(googleSheetId2025, sheet = "submission"))

recommendation_2025 <- clean_recommendation_2025(googlesheets4::read_sheet(googleSheetId2025, sheet = "recommendation"))
tutorial_2025 <- clean_tutorial_2025(googlesheets4::read_sheet(googleSheetId2025, sheet = "tutorial"))
rep_2025 <- clean_rep_2025(googlesheets4::read_sheet(googleSheetId2025, sheet = "student rep"))

graduateDf <- merge(merge(merge(graduateDf, recommendation_2025, by = c('student email','advisor email'), all.x = TRUE), tutorial_2025, by = 'student email' , all.x = TRUE), rep_2025, by = 'student email' , all.x = TRUE) 

intergraduateDf <- merge(merge(merge(intergraduateDf, recommendation_2025, by = c('student email','advisor email'), all.x = TRUE), tutorial_2025, by = 'student email' , all.x = TRUE), rep_2025, by = 'student email' , all.x = TRUE) 

undergraduateDf <- merge(merge(merge(undergraduateDf, recommendation_2025, by = c('student email','advisor email'), all.x = TRUE), tutorial_2025, by = 'student email' , all.x = TRUE), rep_2025, by = 'student email' , all.x = TRUE) 

postdocDf <- merge(merge(merge(postdocDf, recommendation_2025, by = c('student email','advisor email'), all.x = TRUE), tutorial_2025, by = 'student email' , all.x = TRUE), rep_2025, by = 'student email' , all.x = TRUE) 

graduateDf <- processDF(graduateDf,"graduate", 80)
intergraduateDf <- processDF(intergraduateDf,"international", min(4,nrow(intergraduateDf)))
undergraduateDf <- processDF(undergraduateDf,"undergraduate",4)
postdocDf <- processDF(postdocDf,"postdoc",10)

combinedDF <- rbind(graduateDf, intergraduateDf, undergraduateDf, postdocDf)

combinedDF$id <- paste0(combinedDF$cat, rownames(combinedDF))

# 3 application were retracted by applicants
missingApplication <- merge(combinedDF, recommendation_2025, by = c('student email','advisor email'), all.y = TRUE)
missingApplication <- missingApplication[is.na(missingApplication$`student name`),]

missingRecommendation <- combinedDF[is.na(combinedDF$student_name_recommendation),]

tableColumnNames<- c("id", "selected", "student name"   , "student email",  "rep",  "tutorial", "student day",  "student poster","student wholeweek" , "advisor_name_recommendation", "inneed",  "randomNumberGenerated" )

outputColumnNames <- c("id",  "cat", "selected", "student name"   , "student email", "student affiliation"  ,  "student country", "student degree program",      "phd years" ,"num workshop",  "student tutorial talk","rep",  "tutorial", "student day",  "student poster","student wholeweek" , "advisor name","advisor email", "student_name_recommendation", "advisor_name_recommendation", "inneed",  "randomNumberGenerated" )

write.csv(combinedDF[,outputColumnNames], file = "combinedDF.csv", row.names = FALSE, na = '')

# sheet_write(combinedDF[is.na(combinedDF[combinedDF$student_name_recommendation,]),], ss = googleSheetId2025, sheet = "Decisions")

# rm(recommendation_2025, tutorial_2025, rep_2025 )


