#' extraction of online surveys from EMM survey registry
#' Function to extract a selection of surveys and export an excel file
#'
#' @param link a vector of characters containing urls
#'
#' @return function returns nothing but create an excel file
#' @import rvest
#' @import stringr
#' @import xlsx
#' @export

emm_extract<-function(link){
  #for each of given links, the extracted survey will be stored in the template files
  for (i in link){
    url<-i
    #tryCacth the result of the html
    ##invalid as the indicator to know if the page exists. If the page does not exit, then try the next url.
    invalid<-FALSE
    tryCatch(page<-read_html(url),error=function(e){ invalid <<- TRUE})
    if (invalid){
      print(paste0("The given link is not valid: ", url))
      next
    }

    ###progress bar
    pb <- txtProgressBar(min = 0, max = length(link), initial = 0, style=3)
    setTxtProgressBar(pb,which(link==i))


    #if url = valid, then extract
    #question number
    number<-html_text(html_nodes(page,"span[class='text-sm']"))
    #response
    text<-html_attr(html_nodes(page,"parsed-text"),"text")
    #combine the question numbers and the responses in a single data frame
    extracted<-as.data.frame(cbind(number,text))

    #reformat responses in accordance with other files

    #change the date
    #use different conditions to display in "dd/mm/yyyy"
    date_question<-c("1.11.",
                     "1.12.",
                     "2.6.",
                     "2.9.",
                     "11.2.",
                     "11.3.")

    for ( k in date_question){
      if(extracted$text[extracted$number==k]=="Information not available"){
        extracted$text[extracted$number==k]<-"-99.Information not available"
      } else if(extracted$text[extracted$number==k]=="-"){
        extracted$text[extracted$number==k]<-"-"
      } else if(extracted$text[extracted$number==k]=="Not applicable"){
        extracted$text[extracted$number==k]<-"-999.Not applicable"
      } else if(extracted$text[extracted$number==k]=="Don't know"){
        extracted$text[extracted$number==k]<-"-9.Don't know"
      } else if(nchar(extracted$text[extracted$number==k])>10){next}
      else if(nchar(extracted$text[extracted$number==k])==10){
        year<-substr(extracted$text[extracted$number==k],1,4)
        month<-substr(extracted$text[extracted$number==k],6,7)
        day<-substr(extracted$text[extracted$number==k],9,10)
        extracted$text[extracted$number==k]<-paste(day,month,year,sep="/")
      } else if(nchar(extracted$text[extracted$number==k])==4){
        year<-substr(extracted$text[extracted$number==k],1,4)
        extracted$text[extracted$number==k]<-paste("00","00",year,sep="/")
      } else if(nchar(extracted$text[extracted$number==k])==8 & substr(extracted$text[extracted$number==k],3,3)=="-"){
        year<-paste("20",substr(extracted$text[extracted$number==k],1,2),sep="")
        month<-substr(extracted$text[extracted$number==k],4,5)
        day<-substr(extracted$text[extracted$number==k],7,8)
        extracted$text[extracted$number==k]<-paste(day,month,year,sep="/")
      } else if(nchar(extracted$text[extracted$number==k])==7 & substr(extracted$text[extracted$number==k],5,5)=="-"){
        year<-substr(extracted$text[extracted$number==k],1,4)
        month<-substr(extracted$text[extracted$number==k],6,7)
        extracted$text[extracted$number==k]<-paste("00",month,year,sep="/")
      } else{
        print(paste("no condition used for date:", k , x, sep = " "))
      }
    }



    ##Format responses
    #this formatting is about putting the response code number to each response
    extracted$text[extracted$text=="National"]<-"1.National"
    extracted$text[extracted$text=="Subnational"]<-"2.Subnational"
    extracted$text[extracted$text=="Don't know"]<-"-9.Don't know"
    extracted$text[extracted$text=="Cities (densely populated areas)"]<-"1.Predominantly urban / Cities (densely populated areas)"
    extracted$text[extracted$text=="Rural areas (thinly populated areas)"]<-"3.Predominantly rural / Rural areas (thinly populated areas)"
    extracted$text[extracted$text=="Towns and suburbs (intermediate density areas)"]<-"2.Intermediate / Towns and suburbs (intermediate density areas)"
    extracted$text[extracted$text=="Mix (more than one subnational area type)"]<-"4.Mix (more than one subnational area type)"
    extracted$text[extracted$text=="Information not available"]<-"-99.Information not available"
    extracted$text[extracted$text=="Yes"]<-"1.Yes"
    #for "no", I needed to exclude the question "8.5. Access to complete dataset", which has "5.No"#
    extracted$text[extracted$text=="No" & extracted$number !="8.5."]<-"0.No"
    extracted$text[extracted$text=="Single cross-section"]<-"1.Single cross-section"
    extracted$text[extracted$text=="Repeated cross-section (multiple waves with different samples)"]<-"2.Repeated cross-section (multiple waves with different samples)"
    extracted$text[extracted$text=="Longitudinal/panel survey (multiple waves with the same or partially overlapping samples)"]<-"3.Longitudinal/panel survey (multiple waves with the same or partially overlapping samples)"
    extracted$text[extracted$text=="Other (mixed design, please specify)"]<-"4.Other (mixed design, please specify)"
    extracted$text[extracted$text=="Children (up to 12 years-old) only"]<-"1. Children (up to 12 years-old) only"
    ##the type of hyphen used in the html different from the original excel file
    extracted$text[extracted$text=="Youth (between 13 - 25 years-old) only"]<-"2. Youth (between 13 - 25 years-old) only"
    extracted$text[extracted$number=="1.15."& str_detect(extracted$text, "Youth \\(between 13")]<-"2. Youth (between 13 - 25 years-old) only"


    extracted$text[extracted$text=="Children and youth only"]<-"3. Children and youth only"
    extracted$text[extracted$text=="Adult population (18+ or 15+) only"]<-"4. Adult population (18+ or 15+) only"
    extracted$text[extracted$text=="Elder population only (55+)"]<-"5. Elder population only (55+)"
    extracted$text[extracted$text=="A combination of minors and adults"]<-"6. A combination of minors and adults"
    extracted$text[extracted$text=="Men only"]<-"1. Men only"
    extracted$text[extracted$text=="Women only"]<-"2. Women only"
    extracted$text[extracted$text=="Both men and women"]<-"3. Both men and women"
    extracted$text[extracted$text=="Not applicable"]<-"-999.Not applicable"
    extracted$text[extracted$text=="All foreign residents in the city/region/country"]<-"1.All foreign residents in the city/region/country"
    extracted$text[extracted$text=="All residents of foreign origin in the city/region/country"]<-"2.All residents of foreign origin in the city/region/country"
    extracted$text[extracted$text=="All residents who are 1st or 2nd generation migrants in the city/region/country"]<-"3.All residents who are 1st or 2nd generation migrants in the city/region/country"
    extracted$text[extracted$text=="All residents of ethnic minority identification in the city/region/country"]<-"4.All residents of ethnic minority identification in the city/region/country"
    extracted$text[extracted$text=="A selection of residents of foreign/immigrant origin or ancestry in the city/region/country"]<-"5.A selection of residents of foreign/immigrant origin or ancestry in the city/region/country"
    extracted$text[extracted$text=="A selection of residents of ethnic minority identification in the city/region/country"]<-"6.A selection of residents of ethnic minority identification in the city/region/country"
    extracted$text[extracted$text=="Other (e.g. returning emigrants)"]<-"7.Other (e.g. returning emigrants)"
    extracted$text[extracted$text=="Random sampling/selection (i.e. probability sampling, of some kind)"]<-"1.Random sampling/selection (i.e. probability sampling, of some kind)"
    extracted$text[extracted$text=="Non-probability sampling (including snowball/network and purposive samplings)"]<-"2.Non-probability sampling (including snowball/network and purposive samplings)"
    extracted$text[extracted$text=="Mixed sampling procedures (in which there are elements of probability sampling, such as Centre-location sampling)"]<-"3.Mixed sampling procedures (in which there are elements of probability sampling, such as Centre-location sampling)"
    extracted$text[extracted$text=="By data producers with no mentioned formula"]<-"1.By data producers with no mentioned formula"
    extracted$text[extracted$text=="AAPOR"]<-"2.AAPOR"
    extracted$text[extracted$text=="AAPOR RR1"]<-"3.AAPOR RR1"
    extracted$text[extracted$text=="ESS"]<-"4.ESS"
    ##for "other", I added an additionnal condition for question for the sake of clarity ##
    extracted$text[extracted$text=="Other" &extracted$number =="5.4."]<-"5.Other"
    extracted$text[extracted$text=="Other" &extracted$number =="6a5."]<-"5.Other"
    extracted$text[extracted$text=="Other" &extracted$number =="6b5."]<-"5.Other"
    extracted$text[extracted$text=="Other" &extracted$number =="6c5."]<-"5.Other"
    extracted$text[extracted$text=="Other" &extracted$number =="6d5."]<-"5.Other"
    extracted$text[extracted$text=="Other" &extracted$number =="6e5."]<-"5.Other"
    extracted$text[extracted$text=="Professional interviewers only"]<-"1.Professional interviewers only"
    extracted$text[extracted$text=="Cultural mediator only"]<-"2.Cultural mediator only"
    extracted$text[extracted$text=="Non-professional interviewers (e.g. students)"]<-"3.Non-professional interviewers (e.g. students)"
    extracted$text[extracted$text=="A mix" &extracted$number =="7.3."]<-"4.A mix"
    extracted$text[extracted$text=="No, but translator(s) present/ available"]<-"2.No, but translator(s) present/ available"
    extracted$text[extracted$text=="No, nobody had targeted language skills"]<-"3.No, nobody had targeted language skills"
    extracted$text[extracted$text=="Yes, publicly available"]<-"1.Yes, publicly available"
    extracted$text[extracted$text=="Available through a COST Action member"]<-"2.Available through a COST Action member"
    extracted$text[extracted$text=="Available by request"]<-"3.Available by request"
    extracted$text[extracted$text=="Unavailable"]<-"4.Unavailable"
    extracted$text[extracted$text=="Unknown availability"]<-"5.Unknown availability"
    extracted$text[extracted$text=="Yes, micro-data available for download/direct access by researchers"]<-"1.Yes, micro-data available for download/direct access by researchers"
    extracted$text[extracted$text=="Yes, but micro-data only available for online analyses"]<-"2.Yes, but micro-data only available for online analyses"
    extracted$text[extracted$text=="Yes, but micro-data only available to analyse on a secure office/premises of the data archive or data producer"]<-"3.Yes, but micro-data only available to analyse on a secure office/premises of the data archive or data producer"
    extracted$text[extracted$text=="Yes, but with other restrictions"]<-"4.Yes, but with other restrictions"
    extracted$text[extracted$text=="No" & extracted$number =="8.5."]<-"5.No"
    extracted$text[extracted$text=="Data Documentation Initiative"]<-"1.Data Documentation Initiative"
    extracted$text[extracted$text=="Dublin Core"]<-"2.Dublin Core"
    extracted$text[extracted$text=="SDMX"]<-"3.SDMX"
    extracted$text[extracted$text=="No specific standard"]<-"4.No specific standard"

    extracted$text[extracted$text=="Not applicable (full dataset accessible)"]<-"-999.Not applicable (full dataset accessible)"
    extracted$text[extracted$text=="Publicly available"]<-"1. Publicly available"


    #other recoding can be added if necessary



    ##change the display code of country
    country<-c("AT (Austria)","AU (Australia)","BA (Bosnia and Herzegovina)","BE (Belgium)","BG (Bulgaria)","CA (Canada)","CH (Switzerland)","CY (Cyprus)","CZ (Czech Republic)","DE (Germany)","DK (Denmark)","EE (Estonia)",
               "ES (Spain)","FI (Finland)","FR (France)","GB (United Kingdom)","GR (Greece)","HR (Croatia)","HU (Hungary)","IE (Ireland)",
               "IL (Israel)","IS (Iceland)","IT (Italy)","LT (Lithuania)","LU (Luxembourg)","MD (Moldova)","ME (Montenegro)",
               "MK (FYR Macedonia)","MT (Malta) ","NL (Netherlands)","NO (Norway)","NZ (New Zealand)","PL (Poland)","PT (Portugal)","RO (Romania)",
               "RS (Serbia)","SE (Sweden)","SK (Slovakia)","TR (Turkey)","US (United States)")
    country_short<-substr(country,5,nchar(country)-1)

    for (cnt in 1:length(country)){
      if (extracted$text[extracted$number=="1.0."]==country_short[cnt]){
        extracted$text[extracted$number=="1.0."]<-country[cnt]
      }
      else{next}
    }

    #change the colname
    colnames(extracted)[2]<-paste("response",str_sub(url,-4),sep="_")


    #merging
    if(extracted$response[extracted$number=="1.5."]=="1.National"){
      export_national<-merge(export_national,extracted,by="number", all.x=T)
    }
    if(extracted$response[extracted$number=="1.5."]=="2.Subnational"){
      export_subnational<-merge(export_subnational,extracted,by="number", all.x=T)
    }


  }
  ###the end of extraction

  #reorder
  export_national<-export_national[order(export_national$order),]
  export_subnational<-export_subnational[order(export_subnational$order),]

  #change the separator for specific questions
  #for national surveys
  sep_list<-c("1.13.30a. If \"other\" in 1.13, specify",
              "1.14a. If \"other\" in 1.14, specify",
              "2.5. Name of other countries/regions/cities Eng.",
              "3.1a. EMM target population with terms standardized",
              "3.2a. If \"other\" in 3.2., describe which",
              "3.3a. If \"other means\" in 3.3., describe which",
              "4.5. Sampling units",
              "7.5a. Migrant languages in ISO code",
              "7.7a. Questionnaire in migrant language in ISO code",
              "8.9a. Dataset language(s) in ISO code",
              "8.15a. Language(s) of technical survey documentation in ISO code",
              "8.20a. Language(s) of survey questionnaire in ISO code",
              "11.1. Person(s) filling in this file: ")

  for (l in sep_list){
    export_national[export_national$question==l,3:ncol(export_national)]<-gsub(",",";",export_national[export_national$question==l,3:ncol(export_national)])
  }

  #for subnational surveys
  sep_list<-c(sep_list,"1.6. Which subnational level","1.7. Name of region(s) Eng.","1.7a. Region (for sorting)","1.8. Name of region(s) Nat.","1.8a. If subnational, add the NUTS/LAU code of the specific cities/regions")

  for (m in sep_list){
    export_subnational[export_subnational$question==m,3:ncol(export_subnational)]<-gsub(",",";",export_subnational[export_subnational$question==m,3:ncol(export_subnational)])
  }

  #write excel file
  #the exported excel file will be a single excel file named "emm_registry_extracted(dd.mm.yyyy).xlsx"
  #this file will contain two sheets "NATIONAL SURVEYS" and "SUBNATIONAL SURVEYS"

  print(paste0("Exporting ",getwd(),"/",paste0("emm_registry_extracted",format(Sys.Date(), "(%d.%m.%Y)"),".xlsx")))

  write.xlsx(export_national[,-(1:2)], paste0("emm_registry_extracted",format(Sys.Date(), "(%d.%m.%Y)"),".xlsx"), sheetName="NATIONAL SURVEYS", append=FALSE, showNA=FALSE, col.names=F, row.names=F)
  write.xlsx(export_subnational[,-(1:2)], paste0("emm_registry_extracted",format(Sys.Date(), "(%d.%m.%Y)"),".xlsx"), sheetName="SUBNATIONAL SURVEYS", append=T, showNA=FALSE, col.names=F, row.names=F)

}


