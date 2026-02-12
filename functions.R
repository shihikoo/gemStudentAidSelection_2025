library(googlesheets4)
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(data.table)
library(plotly)
library(shinythemes)
library(tidyr)
library(stats)
library(gsubfn)

clean_submission_2025 <- function(df2025_submission){
  colnames(df2025_submission) <- tolower(colnames(df2025_submission))
  colnames(df2025_submission) <- gsub('-', ' ', colnames(df2025_submission))
  
  df2025_submission <- df2025_submission[df2025_submission$status == "current",]
  
  df2025_submission$`student email` <- tolower(df2025_submission$`student email`)
  df2025_submission$`advisor email` <- tolower(df2025_submission$`advisor email`)
  
  df2025_submission$duplicate <- duplicated(df2025_submission$`student email`)
  if (sum(df2025_submission$duplicate) > 0) {
    print("Duplicated entry found.")
    print(df2025_submission[df2025_submission$duplicate, ])
  }
  print(paste("Unique submission number: ", nrow(df2025_submission)))
  
  df2025_submission$`student email` <- tolower(df2025_submission$`student email`)

  df2025_submission$us <- df2025_submission$`student country` == "United States" | df2025_submission$`student country` == "USA"| df2025_submission$`student country` == "US" | df2025_submission$`student country` == "United States of America"
  
  df2025_submission$`student country`[df2025_submission$us]='USA'
  
  df2025_submission$`phd years` = sapply(df2025_submission$`phd years`, unlist)
  df2025_submission$`num workshop` = sapply(df2025_submission$`num workshop`, unlist)
  
  df2025_submission$id <- rownames(df2025_submission)
  
  graduateDf <- df2025_submission[(df2025_submission$`student degree program` == "PhD" | df2025_submission$`student degree program` == "Master's") & df2025_submission$us, ] 
  
  intergraduateDf <- df2025_submission[(df2025_submission$`student degree program` == "PhD" | df2025_submission$`student degree program` == "Master's") & !df2025_submission$us, ] 
  
  undergraduateDf <- df2025_submission[(df2025_submission$`student degree program` == "Undergraduate"), ] 
  
  postdocDf <- df2025_submission[(df2025_submission$`student degree program` == "Post Doc or Early Career (PhD + 3 yrs)"), ] 
  # print(colnames(postdocDf)) 
  
  output_columns_names <- c("id","student name","student email" ,"student affiliation","student country", "student degree program", "phd years" , "num workshop","advisor name","advisor email","student tutorial talk","student day","student poster", "student wholeweek") #,"status" , "updated",  "duplicate", "us" )
   
  return(list(graduateDf[,output_columns_names], intergraduateDf[,output_columns_names], undergraduateDf[,output_columns_names], postdocDf[,output_columns_names]) )
}

clean_recommendation_2025 <- function(df2025_recommendation){
  colnames(df2025_recommendation) <- tolower(colnames(df2025_recommendation))
  colnames(df2025_recommendation) <- gsub('-', ' ', colnames(df2025_recommendation))
  
  # df2025_recommendation[is.na(df2025_recommendation$status),'status'] <- "Current"
  df2025_recommendation <- df2025_recommendation[df2025_recommendation$status == "current",]
  df2025_recommendation <- df2025_recommendation[!is.na(df2025_recommendation$`student email`),]
  df2025_recommendation$`student email` <- tolower(df2025_recommendation$`student email`)
  df2025_recommendation$`advisor email` <- tolower(df2025_recommendation$`advisor email`)
  
  df2025_recommendation <- df2025_recommendation[df2025_recommendation$`recommend`== 'Yes' ,]
  df2025_recommendation <- df2025_recommendation[df2025_recommendation$`aware fee`== 'Yes' ,]

  df2025_recommendation$duplicate <- duplicated(df2025_recommendation$`student email`)
  if (sum(df2025_recommendation$duplicate) > 0) {
    print("Duplicated entry found.")
    print(df2025_recommendation[df2025_recommendation$duplicate, ])
    df2025_recommendation <- df2025_recommendation[!df2025_recommendation$duplicate,]
    } 
  df2025_recommendation$student_name_recommendation <- df2025_recommendation$`student name`
  df2025_recommendation$advisor_name_recommendation <- df2025_recommendation$`advisor name`
  df2025_recommendation$`student email` <- tolower(df2025_recommendation$`student email`)
  
  output_columns_names <- c('student email','advisor email','student_name_recommendation','advisor_name_recommendation','inneed')
  
  return(df2025_recommendation[, output_columns_names])
}

clean_tutorial_2025 <- function(df2025_tutorial){
  df2025_tutorial$`student email` <- tolower(df2025_tutorial$`student email`)
  
  df2025_tutorial[,c("student email","tutorial")]
}

clean_rep_2025 <- function(df2025_rep){
  df2025_rep$`student email` <- tolower(df2025_rep$`student email`)
  
  df2025_rep[,c("student email","rep")]
}

selectionRun <- function(x) {
  return(runif(1,min=0.3*(x=='zero'),max=1)) 
}

processDF <- function(graduateDf, category, selectionNum){
  graduateDf$cat <- category
  
  graduateDf$randomNumberGenerated <- sapply(graduateDf$`num workshop`, selectionRun)
  
  graduateDf$randomNumberGenerated[!is.na(graduateDf$inneed) & graduateDf$inneed == "yes"] = graduateDf$randomNumberGenerated[!is.na(graduateDf$inneed) & graduateDf$inneed == "yes"] + 1
  
  graduateDf$randomNumberGenerated[!is.na(graduateDf$inneed) & graduateDf$inneed == "maybe"] = graduateDf$randomNumberGenerated[!is.na(graduateDf$inneed) & graduateDf$inneed == "maybe"] + 0.5
  
  graduateDf$randomNumberGenerated[graduateDf$`student wholeweek` == "No"] = -999
  
  graduateDf$randomNumberGenerated[!is.na(graduateDf$tutorial) & graduateDf$tutorial == "Yes"] = 2
  graduateDf$randomNumberGenerated[!is.na(graduateDf$rep) & graduateDf$rep == "Yes"] = 2
  
  # invalid application
  graduateDf$randomNumberGenerated[is.na(graduateDf$student_name_recommendation)] = -999
  graduateDf$randomNumberGenerated[graduateDf$`student day` == "No"] = -999
  graduateDf$randomNumberGenerated[graduateDf$`student poster` == "No"] = -999
  
  graduateDf <- graduateDf[order(graduateDf$randomNumberGenerated, decreasing = TRUE),]
  
  graduateDf$selected <- FALSE
  graduateDf$selected[1:selectionNum] <- TRUE 
  graduateDf$selected[graduateDf$randomNumberGenerated == -999] <- FALSE
  
  return(graduateDf)
}

remove_invalid <- function(graduateDf) {
    graduateDf <- graduateDf[-c(!graduateDf$randomNumberGenerated == -999),]
}


