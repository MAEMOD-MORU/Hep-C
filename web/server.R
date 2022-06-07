#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#    setwd("D:/HepC-betaweb/HepC-betaweb/web")
#C:\Hep-c

library(shiny)
library(Rcpp)
library(deSolve)
library(plotly)
library(reshape2)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(erer)
library(scales)
library(plyr)
library(shinyjs)
library(DT)
library(openxlsx)
library(writexl)


# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {


  v <- reactiveValues(doPlot = FALSE,
                      tableCost = NULL,
                      tableU = NULL)
  options(scipen=8)
  observeEvent(input$go, {
    # 0 will be coerced to FALSE
    # 1+ will be coerced to TRUE
    v$doPlot <- input$go
    flie_download$parms <- parms()
    flie_download$table <- out_df()
  })

  observeEvent(input$reset, {
    v$doPlot <- FALSE
  })

  #Diagnosis input
  dia <-  reactiveValues(screening_name = "",
                         screening_sens = 0,
                         screening_spec = 0,
                         screening_cost = 0,
                         confirming_name = "",
                         confirming_sens = 0,
                         confirming_spec = 0,
                         confirming_cost = 0
  )

  #Diagnosis input
  Diagnosis <- reactiveValues(Sensitivity = 90,
                              Cost = 10)

  observeEvent(input$Dia_Scr, {
    if(input$Dia_Scr == 1 || input$Dia_Scr == 2){
      enable("Dia_Con")
      shinyjs::hide("NewDiagnosis")

    }else if(input$Dia_Scr == 3){
      disable("Dia_Con")
      shinyjs::hide("NewDiagnosis")

    }else if(input$Dia_Scr == 4){
      disable("Dia_Con")
      shinyjs::show("NewDiagnosis")

    }

  })
  #parameter
  p_t <- reactiveValues(S_screening = 11169018,
                        scr_yr =1,
                        Pos = 0  #Positive True
  )

  Info <-reactiveValues( screening = "By age",
                         treatment = "No novel treatment"

  )

  flie_download <- reactiveValues( parms = 0,
                                   table =0
  )

  observe({
    if(input$screening!=3){

      if(v$doPlot == F){
        shinyjs::hide("Lineplot")
        shinyjs::hide("Lineplot2")
        shinyjs::hide("Lineplot3")
        shinyjs::hide("Lineplot4")

      }else{
        shinyjs::show("Lineplot")
        shinyjs::show("Lineplot2")
        shinyjs::show("Lineplot3")
        shinyjs::show("Lineplot4")

      }
    }else{
      shinyjs::hide("Lineplot")
      shinyjs::hide("Lineplot2")
      shinyjs::hide("Lineplot3")
      shinyjs::hide("Lineplot4")
    }

  })

  #screening Table
  observeEvent(input$screening, {
    if(input$screening==1){
      disable("risk_g")
      enable("age_s")
      enable("Dia_Con")
      enable("Dia_Scr")
      enable("Treatment")
      enable("care")
      shinyjs::hide("Scr_table")
      Info$screening <- "By age"
      updateCheckboxGroupInput(session, "care", "Link to care:",
                               c("HCV genotype testing" = 1,
                                 "Fibroscan stiffness testing" = 2,
                                 "Relevant and safety lab" = 3,
                                 "Others" = 4
                               ))

      # 41-50
      if(input$age_s == 1){
        p_t$S_screening <- 	11169018
        p_t$Pos <- 	11169018*0.0272*0.0169
        #51-60
      }else if(input$age_s == 2){
        p_t$S_screening <- 10371593
        p_t$Pos <- 	10371593*0.0146*0.0093
        #41-60
      }else if(input$age_s == 3){
        p_t$S_screening <- 	21540611
        p_t$Pos <- 	21540611*0.0418*0.0262
      }
      flie_download$parms <- parms()
      flie_download$table <- out_df()
    }
    else if(input$screening==2){
      disable("age_s")
      enable("risk_g")
      enable("Dia_Con")
      enable("Dia_Scr")
      enable("Treatment")
      enable("care")


      x <- input$risk_g
      if(!is.null(x)){
        shinyjs::show("Scr_table")
        shinyjs::show("Scr_th")
        Info$screening <- "By risk group"
        updateCheckboxGroupInput(session, "care", "Link to care:",
                                 c("HCV genotype testing" = 1,
                                   "Fibroscan stiffness testing" = 2,
                                   "Relevant and safety lab" = 3,
                                   "Others" = 4
                                 ))
        if(!length(which(x == 1)) == 0){
          shinyjs::show("Scr_td1")
          rg$hiv_people <- 444000
          rg$hiv_pos <- 444000*(input$HIV_Scr/100)*(input$HIV_Con/100)
        }else{
          shinyjs::hide("Scr_td1")
          rg$hiv_people <- 0
          rg$hiv_pos <-0
        }

        if(!length(which(x == 2)) == 0){
          shinyjs::show("Scr_td2")
          rg$idu_people <-260305
          rg$idu_pos <-260305*(input$IDU_Scr/100)*(input$IDU_Con/100)
        }else{
          shinyjs::hide("Scr_td2")
          rg$idu_people <- 0
          rg$idu_pos <- 0
        }

        if(!length(which(x == 3)) == 0){
          shinyjs::show("Scr_td3")
          rg$msm_people <- 590000
          rg$msm_pos <- 590000*(input$MSM_Scr/100)*(input$MSM_Con/100)
        }else{
          shinyjs::hide("Scr_td3")
          rg$msm_people <- 0
          rg$msm_pos <- 0
        }
        if(!length(which(x == 4)) == 0){
          shinyjs::show("Scr_td4")
          rg$rb_people <- 390000
          rg$rb_pos <- 390000*(input$Rb_Scr/100)*(input$Rb_Con/100)
        }else{
          shinyjs::hide("Scr_td4")
          rg$rb_people <- 0
          rg$rb_pos <- 0
        }
        if(!length(which(x == 5)) == 0){
          shinyjs::show("Scr_td5")
          rg$bd_people <- 89311
          rg$bd_pos <- 89311*(input$Bd_Scr/100)*(input$Bd_Con/100)
        }else{
          shinyjs::hide("Scr_td5")
          rg$bd_people <- 0
          rg$bd_pos <- 0
        }
        if(!length(which(x == 6)) == 0){
          shinyjs::show("Scr_td6")
          rg$pri_people <- 372979
          rg$pri_pos <- 372979*(input$Pri_Scr/100)*(input$Pri_Con/100)
        }else{
          shinyjs::hide("Scr_td6")
          rg$pri_people <- 0
          rg$pri_pos <- 0
        }
        if(!length(which(x == 7)) == 0){
          shinyjs::show("Scr_td7")
          rg$ckd_people <- 128338
          rg$ckd_pos <- 128338*(input$CKD_Scr/100)*(input$CKD_Con/100)
        }else{
          shinyjs::hide("Scr_td7")
          rg$ckd_people <- 0
          rg$ckd_pos <- 0
        }

      }
      else if(is.null(x)){
        shinyjs::hide("Scr_table")
        rg$hiv_people <- 0
        rg$idu_people <- 0
        rg$msm_people <- 0
        rg$rb_people <- 0
        rg$bd_people <- 0
        rg$pri_people <- 0
        rg$ckd_people <- 0
        rg$hiv_pos <- 0
        rg$idu_pos <- 0
        rg$msm_pos <- 0
        rg$rb_pos <- 0
        rg$bd_pos <- 0
        rg$pri_pos <- 0
        rg$ckd_pos <- 0
      }
      p_t$S_screening <- rg$hiv_people + rg$idu_people + rg$msm_people + rg$rb_people + rg$bd_people + rg$pri_people + rg$ckd_people
      p_t$Pos <- rg$hiv_pos + rg$idu_pos + rg$msm_pos + rg$rb_pos + rg$bd_pos + rg$pri_pos + rg$ckd_pos

      flie_download$parms <- parms()
      flie_download$table <- out_df()
    }

    else if(input$screening==3){

      updateCheckboxGroupInput(session, "care", "Link to care:",
                               c("HCV genotype testing" = 1,
                                 "Fibroscan stiffness testing" = 2,
                                 "Relevant and safety lab" = 3,
                                 "Others" = 4
                               ),c(1,2,3))
      updateRadioButtons(session,"Treatment", "Novel Treatment type:",
                         c("No novel treatment" = 0,
                           "Sofosbuvir with Velpatasvir (pan-genotypic treatments)" =1,
                           "Sofosbuvir with Ledipasvir (National List of Essential Medicines)" = 2,
                           "Sofosbuvir with Ravidasvir (pan-genotypic treatments, on-going clinical trial)" = 3,
                           "Another durg" = 4),selected = 1)
      updateRadioButtons(session,"Dia_Scr", "Screening:",
                         c("Rapid strip test ANT HCV" =1,
                           "HCV Antibody" = 2,
                           "Rapid HCV RNA" = 3,
                           "Other test" = 4),selected = 1)
      updateRadioButtons(session,"Dia_Con", "Confirming:",
                         c("HCV RNA" = 1,
                           "CORE Antigen" = 2,
                           "Rapid HCV RNA" = 3),selected = 1)

      disable("age_s")
      disable("risk_g")
      delay(10,disable("Dia_Scr"))
      delay(500,disable("Dia_Con"))
      delay(10,disable("Treatment"))
      delay(10,disable("care"))
      p_t$S_screening <- 0
      p_t$Pos <- 0
      flie_download$parms <- parms_base()
      flie_download$table <- out_df_base()
    }

  })



  #do when checkbox of age_s change.
  observeEvent(input$age_s, {
      # 41-50
    if(input$age_s == 1){
      p_t$S_screening <- 	11169018
      p_t$Pos <- 	11169018*0.0272*0.0169
      #51-60
    }else if(input$age_s == 2){
      p_t$S_screening <- 10371593
      p_t$Pos <- 	10371593*0.0146*0.0093
      #41-60
    }else if(input$age_s == 3){
      p_t$S_screening <- 	21540611
      p_t$Pos <- 	21540611*0.0418*0.0262
    }

  })

  #do when checkbox of scr_yr change.
  observeEvent(input$Sso, {
      #1 year
    if(input$Sso == 1){
      p_t$scr_yr <- 1
      #2 years
    }else if(input$Sso == 2){
      p_t$scr_yr <- 2
      #4 years
    }else if(input$Sso == 3){
      p_t$scr_yr <- 4
    } #10 years
    else if(input$Sso == 4){
      p_t$scr_yr <- 10
    }

  })


  #Screening people Risk_group parameter
  rg <- reactiveValues( hiv_people = 0,
                        idu_people = 0,
                        msm_people = 0 ,
                        rb_people = 0 ,
                        bd_people = 0 ,
                        pri_people = 0 ,
                        ckd_people = 0,
                        hiv_pos = 0,
                        idu_pos = 0,
                        msm_pos = 0 ,
                        rb_pos = 0 ,
                        bd_pos = 0 ,
                        pri_pos = 0 ,
                        ckd_pos = 0
  )


  #Screening Risk_group
  observeEvent(input$risk_g, {
    x <- input$risk_g
    if(!is.null(x)){
      shinyjs::show("Scr_table")
      shinyjs::show("Scr_th")

      if(!length(which(x == 1)) == 0){
        shinyjs::show("Scr_td1")
        rg$hiv_people <- 444000
        rg$hiv_pos <- 444000*(input$HIV_Scr/100)*(input$HIV_Con/100)
      }else{
        shinyjs::hide("Scr_td1")
        rg$hiv_people <- 0
        rg$hiv_pos <-0
      }

      if(!length(which(x == 2)) == 0){
        shinyjs::show("Scr_td2")
        rg$idu_people <-260305
        rg$idu_pos <-260305*(input$IDU_Scr/100)*(input$IDU_Con/100)
      }else{
        shinyjs::hide("Scr_td2")
        rg$idu_people <- 0
        rg$idu_pos <- 0
      }

      if(!length(which(x == 3)) == 0){
        shinyjs::show("Scr_td3")
        rg$msm_people <- 590000
        rg$msm_pos <- 590000*(input$MSM_Scr/100)*(input$MSM_Con/100)
      }else{
        shinyjs::hide("Scr_td3")
        rg$msm_people <- 0
        rg$msm_pos <- 0
      }
      if(!length(which(x == 4)) == 0){
        shinyjs::show("Scr_td4")
        rg$rb_people <- 390000
        rg$rb_pos <- 390000*(input$Rb_Scr/100)*(input$Rb_Con/100)
      }else{
        shinyjs::hide("Scr_td4")
        rg$rb_people <- 0
        rg$rb_pos <- 0
      }
      if(!length(which(x == 5)) == 0){
        shinyjs::show("Scr_td5")
        rg$bd_people <- 89311
        rg$bd_pos <- 89311*(input$Bd_Scr/100)*(input$Bd_Con/100)
      }else{
        shinyjs::hide("Scr_td5")
        rg$bd_people <- 0
        rg$bd_pos <- 0
      }
      if(!length(which(x == 6)) == 0){
        shinyjs::show("Scr_td6")
        rg$pri_people <- 372979
        rg$pri_pos <- 372979*(input$Pri_Scr/100)*(input$Pri_Con/100)
      }else{
        shinyjs::hide("Scr_td6")
        rg$pri_people <- 0
        rg$pri_pos <- 0
      }
      if(!length(which(x == 7)) == 0){
        shinyjs::show("Scr_td7")
        rg$ckd_people <- 128338
        rg$ckd_pos <- 128338*(input$CKD_Scr/100)*(input$CKD_Con/100)
      }else{
        shinyjs::hide("Scr_td7")
        rg$ckd_people <- 0
        rg$ckd_pos <- 0
      }

    }
    else if(is.null(x)){
      shinyjs::hide("Scr_table")
      rg$hiv_people <- 0
      rg$idu_people <- 0
      rg$msm_people <- 0
      rg$rb_people <- 0
      rg$bd_people <- 0
      rg$pri_people <- 0
      rg$ckd_people <- 0
      rg$hiv_pos <- 0
      rg$idu_pos <- 0
      rg$msm_pos <- 0
      rg$rb_pos <- 0
      rg$bd_pos <- 0
      rg$pri_pos <- 0
      rg$ckd_pos <- 0
    }
    p_t$S_screening <- rg$hiv_people + rg$idu_people + rg$msm_people + rg$rb_people + rg$bd_people + rg$pri_people + rg$ckd_people
    p_t$Pos <- rg$hiv_pos + rg$idu_pos + rg$msm_pos + rg$rb_pos + rg$bd_pos + rg$pri_pos + rg$ckd_pos
  })

  #Diagnosis Screening
  observe({
     # Rapid strip test ANT HCV
    if(input$Dia_Scr == 1){
      dia$screening_name <- "Rapid strip test ANT HCV"
      dia$screening_sens <- 94
      dia$screening_spec <- 98
      dia$screening_cost <- input$Scr1_Dia_Cost #THB
      # HCV Antibody
    }else if(input$Dia_Scr == 2){
      dia$screening_name <- "HCV Antibody"
      dia$screening_sens <- 99.5
      dia$screening_spec <- 99.8
      dia$screening_cost <- input$Scr2_Dia_Cost #THB
      #Rapid HCV RNA
    }else if(input$Dia_Scr == 3){
      dia$screening_name <- "Rapid HCV RNA"
      dia$screening_sens <- 100
      dia$screening_spec <- 100
      dia$screening_cost <- input$Con3_Dia_Cost #THB
      #Other test
    }else if(input$Dia_Scr == 4){
      dia$screening_name <- "Other test"
      dia$screening_sens <- input$Input_Dia_Sens
      dia$screening_spec <- input$Input_Dia_Spec
      dia$screening_cost <- input$Input_Dia_Cost

      dia$confirming_name <- "Other test"
      dia$confirming_sens <- input$Input_Dia_Sens
      dia$confirming_spec <- input$Input_Dia_Spec
      dia$confirming_cost <- input$Input_Dia_Cost
    }

  })

  observeEvent(input$Comfirm_dia, {
    dia$screening_sens <- input$Input_Dia_Sens
    dia$screening_spec <- input$Input_Dia_Spec
    dia$screening_cost <- input$Input_Dia_Cost #THB
  })

  output$dia_scr_name_p <- renderText({
    paste("Test :" , dia$screening_name )
  })

  output$dia_scr_sens_p <- renderText({
    paste("sensitivity :" , dia$screening_sens ," %" )
  })

  output$dia_scr_spec_p <- renderText({
    paste("specificity :" , dia$screening_spec ," %")
  })

  output$dia_scr_cost_thb_p <- renderText({
    paste("specificity :" , dia$screening_cost ," THB")
  })
  output$dia_scr_cost_usd_p <- renderText({
    paste("Total cost :" , round(dia$screening_cost/30.41,2) , " USD")
  })

  #Diagnosis Confirming
  observe({
    if(input$Dia_Scr == 3){
      dia$confirming_name <- "None"
      dia$confirming_sens <- 100
      dia$confirming_spec <- 100
      dia$confirming_cost <- 0
    }else{
      # HCV RNA
    if(input$Dia_Con == 1){
      dia$confirming_name <- "HCV RNA"
      dia$confirming_sens <- 81.2
      dia$confirming_spec <- 96.15
      dia$confirming_cost <- input$Con1_Dia_Cost #THB
      # CORE Antigen
    }else if(input$Dia_Con == 2){
      dia$confirming_name <- "CORE Antigen"
      dia$confirming_sens <- 94
      dia$confirming_spec <- 98
      dia$confirming_cost <- input$Con2_Dia_Cost #THB
      # Rapid HCV RNA
    }else if(input$Dia_Con == 3){
      dia$confirming_name <- "Rapid HCV RNA"
      dia$confirming_sens <- 100
      dia$confirming_spec <- 100
      dia$confirming_cost <- input$Con3_Dia_Cost #THB
    }
    }
  })

  #Diagnosis box input Confirm button
  observeEvent(input$Comfirm_dia, {
    dia$screening_sens <- input$Input_Dia_Sens
    dia$screening_spec <- input$Input_Dia_Spec
    dia$screening_cost <- input$Input_Dia_Cost #THB
  })

  output$dia_con_name_p <- renderText({
    paste("Test :" , dia$confirming_name )
  })

  output$dia_con_sens_p <- renderText({
    paste("sensitivity :" , dia$confirming_sens ," %" )
  })

  output$dia_con_spec_p <- renderText({
    paste("specificity :" , dia$confirming_spec ," %")
  })

  output$dia_con_cost_thb_p <- renderText({
    paste("Total cost :" , dia$confirming_cost ," THB")
  })
  output$dia_con_cost_usd_p <-renderText({
    paste("Total cost :" , round(dia$confirming_cost/30.41,2) , " USD")
  })


  #Treatment input checkbox
  Treatment <- reactiveValues(new_cureF0 = 0.1,
                              new_cureF1 = 0.1,
                              new_cureF2 = 0.1,
                              new_cureF3 = 0.1,
                              new_cureC1 = 0.1,
                              new_cureC2 = 0.1,
                              new_cureC3 = 0.1,
                              new_cureC4 = 0.1,
                              cost = 10)

  #do when checkbox of Treatment change.
  observeEvent(input$Treatment, {
    #drug 1
    if(input$Treatment == 0){
      Treatment$new_cureF0 <- 0.6
      Treatment$new_cureF1 <- 0.6
      Treatment$new_cureF2 <- 0.6
      Treatment$new_cureF3 <- 0.6
      Treatment$new_cureC1 <- 0.6
      Treatment$new_cureC2 <- 0.6
      Treatment$new_cureC3 <- 0.6
      Treatment$new_cureC4 <- 0.4
      Treatment$cost <- 0
      Info$treatment <- "No novel treatment"
      #drug 2
    }else if(input$Treatment == 1){
      Treatment$new_cureF0 <- 0.98
      Treatment$new_cureF1 <- 0.98
      Treatment$new_cureF2 <- 0.98
      Treatment$new_cureF3 <- 0.98
      Treatment$new_cureC1 <- 0.98
      Treatment$new_cureC2 <- 0.98
      Treatment$new_cureC3 <- 0.98
      Treatment$new_cureC4 <- 0.98
      Treatment$cost <- input$Tre1_Cost
      Info$treatment <- "Sofosbuvir with Velpatasvir"
      #drug 3
    }else if(input$Treatment == 2){
      Treatment$new_cureF0 <- 0.826
      Treatment$new_cureF1 <- 0.826
      Treatment$new_cureF2 <- 0.826
      Treatment$new_cureF3 <- 0.826
      Treatment$new_cureC1 <- 0.826
      Treatment$new_cureC2 <- 0.826
      Treatment$new_cureC3 <- 0.826
      Treatment$new_cureC4 <- 0.826
      Treatment$cost <- input$Tre2_Cost
      Info$treatment <- "Sofosbuvir with Ledipasvir"
      #drug 4
    }else if(input$Treatment == 3){
      Treatment$new_cureF0 <- 0.874
      Treatment$new_cureF1 <- 0.874
      Treatment$new_cureF2 <- 0.874
      Treatment$new_cureF3 <- 0.874
      Treatment$new_cureC1 <- 0.874
      Treatment$new_cureC2 <- 0.874
      Treatment$new_cureC3 <- 0.874
      Treatment$new_cureC4 <- 0.874
      Treatment$cost <- input$Tre3_Cost
      Info$treatment <- "Sofosbuvir with Ravidasvir"
      #Another drug
    }else{
      Treatment$new_cureF0 <- input$Input_Tre_Eff
      Treatment$new_cureF1 <- input$Input_Tre_Eff
      Treatment$new_cureF2 <- input$Input_Tre_Eff
      Treatment$new_cureF3 <- input$Input_Tre_Eff
      Treatment$new_cureC1 <- input$Input_Tre_Eff
      Treatment$new_cureC2 <- input$Input_Tre_Eff
      Treatment$new_cureC3 <- input$Input_Tre_Eff
      Treatment$new_cureC4 <- input$Input_Tre_Eff
      Treatment$cost <- input$Input_Tre_Cost
      Info$treatment <- "Another durg"
    }

  })

  observeEvent(input$Tre1_Cost, {
    if(input$Treatment == 1){
    Treatment$cost <- input$Tre1_Cost
    }
  })

  observeEvent(input$Tre2_Cost, {
    if(input$Treatment == 2){
    Treatment$cost <- input$Tre2_Cost
    }
  })

  observeEvent(input$Tre3_Cost, {
    if(input$Treatment == 3){
    Treatment$cost <- input$Tre3_Cost
    }
  })

  #comfirm
  observeEvent(input$Comfirm_Tre, {
    if(input$Treatment == 1){
      Treatment$new_cureF0 <- 0.6
      Treatment$new_cureF1 <- 0.6
      Treatment$new_cureF2 <- 0.6
      Treatment$new_cureF3 <- 0.6
      Treatment$new_cureC1 <- 0.6
      Treatment$new_cureC2 <- 0.6
      Treatment$new_cureC3 <- 0.6
      Treatment$new_cureC4 <- 0.4
      Treatment$cost <- input$Tre1_Cost
      #drug 2
    }else if(input$Treatment == 2){
      Treatment$new_cureF0 <- 0.7
      Treatment$new_cureF1 <- 0.7
      Treatment$new_cureF2 <- 0.8
      Treatment$new_cureF3 <- 0.7
      Treatment$new_cureC1 <- 0.7
      Treatment$new_cureC2 <- 0.7
      Treatment$new_cureC3 <- 0.7
      Treatment$new_cureC4 <- 0.4
      Treatment$cost <- input$Tre2_Cost
      #drug 3
    }else if(input$Treatment == 3){
      Treatment$new_cureF0 <- 0.8
      Treatment$new_cureF1 <- 0.8
      Treatment$new_cureF2 <- 0.8
      Treatment$new_cureF3 <- 0.8
      Treatment$new_cureC1 <- 0.8
      Treatment$new_cureC2 <- 0.8
      Treatment$new_cureC3 <- 0.8
      Treatment$new_cureC4 <- 0.5
      Treatment$cost <- input$Tre3_Cost
      #Another drug
    }else{
      Treatment$new_cureF0 <- input$Input_Tre_Eff
      Treatment$new_cureF1 <- input$Input_Tre_Eff
      Treatment$new_cureF2 <- input$Input_Tre_Eff
      Treatment$new_cureF3 <- input$Input_Tre_Eff
      Treatment$new_cureC1 <- input$Input_Tre_Eff
      Treatment$new_cureC2 <- input$Input_Tre_Eff
      Treatment$new_cureC3 <- input$Input_Tre_Eff
      Treatment$new_cureC4 <- input$Input_Tre_Eff
      Treatment$cost <- input$Input_Tre_Cost
    }

  }
  )

  #numericInput in Treatment
  observe({
    if (input$Treatment != 4) {
      shinyjs::hide("Newdurg")

    }else{
      shinyjs::show("Newdurg")
    }
  })


  #textoutput
  output$text1 <- renderText({
    paste("Treatment efficacy :" , round(mean(c(Treatment$new_cureF0
                                                ,Treatment$new_cureF2
                                                ,Treatment$new_cureF3
                                                ,Treatment$new_cureF4
                                                ,Treatment$new_cureC1
                                                ,Treatment$new_cureC2
                                                ,Treatment$new_cureC3
                                                ,Treatment$new_cureC4)),4)*100, "%"
    )


  })
  output$text2 <-renderText({
    paste("Treatment cost :" , Treatment$cost , " THB")
  })

  output$text3 <-renderText({
    paste("Treatment cost :" , round(Treatment$cost/30.41,2) , " USD")
  })

  #Extra
  extra <-  reactiveValues( hcv_cost = 0,
                            fst_cost = 0,
                            rsl_cost = 0,
                            other_cost = 0,
                            total_cost = 0,
                            name = c("HCV genotype testing",
                                     "Fibroscan stiffness testing",
                                     "Relevant and safety lab",
                                     "Others"),
                            name_check = c(F,F,F,F)
                            )


  observe({
    x <- input$care

    if(!is.null(x)){


      if(!length(which(x == 1)) == 0){
        extra$hcv_cost <- input$Extra1_Cost
        extra$name_check[1] <- T
      }else{
        extra$hcv_cost <- 0
        extra$name_check[1] <- F
      }

      if(!length(which(x == 2)) == 0){
        extra$fst_cost <-input$Extra2_Cost
        extra$name_check[2] <- T
      }else{
        extra$fst_cost <- 0
        extra$name_check[2] <- F
      }

      if(!length(which(x == 3)) == 0){
        extra$rsl_cost <-input$Extra3_Cost
        extra$name_check[3] <- T
      }else{
        extra$rsl_cost <-0
        extra$name_check[3] <- F
      }
      if(!length(which(x == 4)) == 0){
        extra$other_cost <-input$Extra4_Cost
        extra$name_check[4] <- T
      }else{
        extra$other_cost <-0
        extra$name_check[4] <- F
      }

    }else{
      extra$hcv_cost <- 0
      extra$fst_cost <- 0
      extra$rsl_cost <- 0
      extra$other_cost <- 0
      extra$name_check <- c(F,F,F,F)
    }

    extra$total_cost <- extra$hcv_cost+extra$fst_cost++extra$rsl_cost+extra$other_cost
  })


  output$extra_name_p <-renderText({
    x <- input$care
    if(!is.null(x)){
      test <- paste(extra$name[extra$name_check], collapse = ", ")
      paste("Diagnostic Test : " , test )
    }else{
      paste("Diagnostic Test : Nothing")
    }
  })
  output$extra_thb_p <-renderText({
    paste("Total cost :" , extra$total_cost , " THB")
  })

  output$extra_usd_p <-renderText({
    paste("Total cost :" , round(extra$total_cost/30.41,2) , " USD")
  })




  #button to reset changed values back to the default values (from ui)
  #section 1
  observeEvent(input$resetSect1, {
    #reset("P0")
    updateSliderInput(session, inputId = "P0",value = 61600000)
    updateSliderInput(session, inputId = "K", value = 68500000)
    updateSliderInput(session, inputId = "r", value = 0.16)
    updateSliderInput(session, inputId = "beta", value = 0.02)
    updateSliderInput(session, inputId = "Fi", value = 1.15)
  })

  #section 2
  observeEvent(input$resetSect2, {
    updateSliderInput(session, inputId = "f0f1", value = 0.117)
    updateSliderInput(session, inputId = "f1f2", value = 0.085)
    updateSliderInput(session, inputId = "f2f3", value = 0.12)
    updateSliderInput(session, inputId = "f3cA", value = 0.116)
  })

  #section3
  observeEvent(input$resetSect3, {
    updateSliderInput(session, inputId = "cAcB", value = 0.044)
    updateSliderInput(session, inputId = "cBcC", value = 0.076)
  })

  #section4
  observeEvent(input$resetSect4, {
    updateSliderInput(session, inputId = "c1bA",value = 0.0068)
    updateSliderInput(session, inputId = "c2bA", value = 0.0068)
    updateSliderInput(session, inputId = "c1bB", value = 0.0099)
    updateSliderInput(session, inputId = "c2bB", value = 0.0099)
    updateSliderInput(session, inputId = "c1bC", value = 0.0029)
    updateSliderInput(session, inputId = "c2bC", value = 0.0029)
    updateSliderInput(session, inputId = "c1bD", value = 0.0068)
    updateSliderInput(session, inputId = "c2bD", value = 0.0068)
    updateSliderInput(session, inputId = "c3bD", value = 0.066)
    updateSliderInput(session, inputId = "c4bD", value = 0.066)
  })

    sourceCpp('p1_scr_SS.cpp')


    parms <- reactive({
      list(
        P0= input$P0,       #popstat(YEAR=1999)
        K=  input$K,         #Maximum population (carrying capacity)
        r = input$r,        #Population growth rate (logistic growth curve)
        flowin = input$Fi*(10^-8),
        caset0 =  input$P0*0.03,      #0.03*P0,
        standard_start = 2004,
        new_start = 2019,
        nscr = 0.005,
        scr_yr = p_t$scr_yr,
        scr_cov = round(p_t$Pos/p_t$scr_yr),      #0.9/scr_yr,
        sens = ((dia$screening_sens/100)*(dia$screening_spec/100)*(dia$confirming_sens/100)*(dia$confirming_spec/100)) ,
        pF0scr = 0.1439,
        pF1scr = 0.2969,
        pF2scr=0.1098,
        pF3scr=0.1466,
        pC1scr=0.1998,
        pC2scr=0.0819,
        pC3scr=0.0096,
        pC4scr=0.0011,

        #Natural rate of death
        natdeath=0.04, #Unrelated to cirrhosis and hepatitis C

        beta= input$beta,              #Transmission coefficient

        #standard treatment allocation
        F0std = 0.05,
        F1std = 0.05,
        F2std = 0.3,
        F3std = 0.3,
        C1std = 0.3,

        std_cureF0=0.7,
        std_cureF1=0.7,
        std_cureF2=0.7,
        std_cureF3=0.7,
        std_cureC1=0.7,
        new_cureF0=Treatment$new_cureF0,
        new_cureF1=Treatment$new_cureF1,
        new_cureF2=Treatment$new_cureF2,
        new_cureF3=Treatment$new_cureF3,
        new_cureC1=Treatment$new_cureC1,
        new_cureC2=Treatment$new_cureC2,
        new_cureC3=Treatment$new_cureC3,
        new_cureC4=Treatment$new_cureC4,

        #Progression of fibrosis
        f0f1=input$f0f1,       #Fibrosis stage F0 to F1
        f1f2=input$f1f2,       #Fibrosis stage F1 to F2
        f2f3=input$f2f3,        #Fibrosis stage F2 to F3
        f3c1=input$f3c1,       #Fibrosis stage F3 to C1

        #Progression of cirrhosis
        c1c2=0.044,       #Fibrosis stage C1 to C2
        c2c3=0.044,       #Fibrosis stage C2 to C3
        c3c4=0.076,       #Fibrosis stage C3 to C4

        #Incidence of developing HCC
        c1bA=input$c1bA,      #Fibrosis stage C1 to bA
        c1bB=input$c1bB,      #Fibrosis stage C1 to bB
        c1bC=input$c1bC,      #Fibrosis stage C1 to bC
        c1bD=input$c1bD,      #Fibrosis stage C1 to bD

        c2bA=input$c2bA,      #Fibrosis stage C2 to bA
        c2bB=input$c2bB,      #Fibrosis stage C2 to bB
        c2bC=input$c2bC,      #Fibrosis stage C2 to bC
        c2bD=input$c2bD,      #Fibrosis stage C2 to bD

        c3bD=input$c3bD,      #Fibrosis stage C3 to bD
        c4bD=input$c4bD,      #Fibrosis stage C4 to bD

        #Death rate from cirrhosis and HCC
        deathc1=0.01,         #Death rate for Cirrhosis Child-Pugh class A
        deathc2=0.01,         #Death rate for Cirrhosis Child-Pugh class B
        deathc3=0.2,          #Death rate for Cirrhosis Child-Pugh class C
        deathc4=0.57,         #Death rate for Cirrhosis Child-Pugh class D

        deathbA=1/(36/12),    #Death rate for HCC_BCLC_A
        deathbB=1/(16/12),    #Death rate for HCC_BCLC_B
        deathbC=1/(6/12),     #Death rate for HCC_BCLC_C
        deathbD=1/(3/12),     #Death rate for HCC_BCLC_D

        deathtran=1/(240/12),

        #Transplantation
        tranc4=0.0015, #Transplantation rate in cirrhosis stage C4
        tranbA=0.0015, #Transplantation rate in HCC_BCLC_A
        tranbB=0.0015

      )
    })

    S <-reactive({
      1*(parms()$P0 - parms()$caset0)
    })


    inits <- reactive({
      c(
        S=S(),
        F0=0.2825*parms()$caset0,
        F1=0.2825*parms()$caset0,
        F2=0.184*parms()$caset0,
        F3=0.124*parms()$caset0,
        C1=0.03175*parms()$caset0,
        C2=0.03175*parms()$caset0,
        C3=0.03175*parms()$caset0,
        C4=0.03174*parms()$caset0,
        HCC_A=0,
        HCC_B=0,
        HCC_C=0,
        HCC_D=0,
        D=0,
        dthC14=0
        ,dthHCC=0
        ,C1std_cured=0,
        C1new_cured=0,
        C2new_cured=0,
        C3new_cured=0,
        C4new_cured=0
        #set up initial death


      )
    })



    out_df <- reactive({

      times <- seq(1999, 2060,by=1)

      out <- ode( y = inits(),times =  times, func = PanHepC, parms = parms())
      # out <- ode( y = inits(),times =  times, func = PanHepC, parms = parms(), maxsteps = 5000, rtol = 1, atol = 1)

      out_df <- as.data.frame(out)
      out_df
    })

    cost_utility_list <- reactive({

      df_base <- out_df_base()
      df_new <- out_df()
      # screening_people <-out_df()[c(21:62), 33]
      # Confirming_people <-out_df()[c(21:62), 33]
      # for (i in 1:p_t$scr_yr) {
      #   screening_people[i] <- p_t$S_screening/p_t$scr_yr
      # }
      #
      # Confirming_cost <- round(screening_people*(dia$screening_sens/100)*(dia$screening_spec/100)*dia$confirming_cost)
      # screening_cost <- screening_people*(dia$screening_cost+extra$total_cost) + Confirming_cost
      # Treatment_people <-data.frame(round(out_df()[c(21:62),32]))
      # Treatment_cost <- Treatment_people*Treatment$cost
      # Total_cost <- screening_cost + Treatment_cost
      # Total_cost_dis <- Total_cost*0.97

      attach(df_base)

      #HCV genotype testing + Fibroscan stiffness testing + Relevant and safety lab
      #4000 + 2500 + 1500
      extra_cost_base <- 4000 + 2500 + 1500
      treat_coas_base <- input$Tre1_Cost
      screening_people <- screen

      #cost of Confirming
      #(94/100) and (98/100) from Sensitivity and of Specificity Screening
      Confirming_cost <- round(screen*(94/100)*(98/100)*input$Con1_Dia_Cost)
      #cost of screening using GeneEXpert
      screen_base <- screen*(dia$screening_cost+extra_cost_base) + Confirming_cost
      #cost of treatment using Sof-Vel
      treat_base <- treat_new*treat_coas_base
      #cost of extra lab per treatment
      # extra_base <- -c((diff(fibrosis )+diff(compensate)+diff(decompensate)+diff(total_HCC)))*extra_cost_base
      #cost per visit
      # indirect_base <- fibrosis*4470+compensate*4380 + decompensate*6060 + total_HCC*9900
      # #cost per visit
      # complicate_base <-  compensate*86236 + decompensate*157755 + total_HCC*197961
      #total cost from 2019 onwards
      total_base <- screen_base[21:62]+treat_base[21:62]
      #total cost with 3% discount
      total_base_dis<-total_base*(1/(1+0.03)^(0:41))
      cost_base_df <- data.frame(df_base[c(21:62),1] , screen_base[21:62],treat_base[21:62],total_base,total_base_dis)
      #total utility from 2019 (quality of life loss due to infection and death)
      utility_base <- fibrosis[21:62]*0.73+compensate[21:62]*0.7+
        decompensate[21:62]*0.58+total_HCC[21:62]*0.58+
        diff(dthC14)[20:61]*27+diff(dthHCC)[20:61]*17
      #total Utility with 3% discount
      utility_base_dis <- utility_base*(1/(1+0.03)^(0:41))

      detach()
      HCC_death_base<-df_base[c(21:62),17]-df_base[c(20:61),17]
      utility_base_df <-data.frame(df_base[c(21:62),1],
                                  round(df_base[c(21:62),34]),
                                  round(df_base[c(21:62),35]),
                                  round(df_base[c(21:62),36]),
                                  round(df_base[c(21:62),c(25)]),
                                  round(df_base[c(21:62),c(26)]),
                                  round(df_base[c(21:62),c(29)]),#incidence
                                  round(df_base[c(21:62),c(28)]),#new death
                                  round(HCC_death_base),
                                  round(df_base[c(21:62),c(15)]),#Total death
                                  round(df_base[c(21:62),c(17)]),#Total death HCC
                                  round(utility_base),
                                  round(utility_base_dis))
      attach(df_new)
      screening_people <- screen*0
      for (i in 1:p_t$scr_yr) {
        screening_people[20+i] <- p_t$S_screening/p_t$scr_yr
      }

      #cost of Confirming
      Confirming_cost <- round(screening_people*(dia$screening_sens/100)*(dia$screening_spec/100)*dia$confirming_cost)
      #cost of screening using GeneEXpert
      screen_new <- screening_people*(dia$screening_cost+extra$total_cost) + Confirming_cost
      #cost of treatment using Sof-Vel
      treat_new <- treat_new*Treatment$cost
      #cost of extra lab per treatment
      # extra_new <- -c((diff(fibrosis)+diff(compensate)+diff(decompensate)+diff(total_HCC)))*extra$total_cost
      #cost per visit
      # indirect_new <- fibrosis*4470+compensate*4380 + decompensate*6060 + total_HCC*9900
      #cost per visit
      # complicate_new <-  compensate*86236 + decompensate*157755 + total_HCC*197961
      #total cost from 2019 onwards
      total_new <- screen_new[21:62]+treat_new[21:62]
      #total cost with 3% discount
      total_new_dis<-total_new*(1/(1+0.03)^(0:41))
      cost_new_df <- data.frame(df_base[c(21:62),1],screen_new[21:62],treat_new[21:62],total_new,total_new_dis)

      #total utility from 2019 (quality of life loss due to infection and death)
      utility_new <- fibrosis[21:62]*0.73+compensate[21:62]*0.7+
        decompensate[21:62]*0.58+total_HCC[21:62]*0.58+
        diff(dthC14)[20:61]*27+diff(dthHCC)[20:61]*17
      #total Utility with 3% discount
      utility_new_dis <- utility_new*(1/(1+0.03)^(0:41))

      detach()
      HCC_death_new<-df_new[c(21:62),17]-df_new[c(20:61),17]
      utility_new_df <-data.frame(df_base[c(21:62),1],
                            round(df_new[c(21:62),34]),
                            round(df_new[c(21:62),35]),
                            round(df_new[c(21:62),36]),
                            round(df_new[c(21:62),c(25)]),
                            round(df_new[c(21:62),c(26)]),
                            round(df_new[c(21:62),c(29)]),#incidence
                            round(df_new[c(21:62),c(28)]),#new death
                            round(HCC_death_new),
                            round(df_new[c(21:62),c(15)]),#Total death
                            round(df_new[c(21:62),c(17)]),#Total death HCC
                            round(utility_new),
                            round(utility_new_dis))
      table_name_cost <-c("Times","screening cost","Treatment cost",
                     "Total cost","Total cost with discount(3%)")
      names(cost_base_df) <- table_name_cost
      names(cost_new_df) <- table_name_cost
      Utility_table_name <-c("Times","Fibrosis","Compensate","Decompensate","Total Infection",
                     "Total HCC","Incidence HCC","New Death","Death HCC","Total Death",
                     "Total Death HCC","Utility","Utility With discount")
      names(utility_base_df) <- Utility_table_name
      names(utility_new_df) <- Utility_table_name
      cost_utility_list <- list(
        "cost_base_df"=cost_base_df,"utility_base_df"=utility_base_df,"cost_new_df"=cost_new_df,"utility_new_df"=utility_new_df
      )
      cost_utility_list
    })



    #Screening cost
    cost_screening_plot <- reactive({

      people_screening <- p_t$S_screening
      year <- p_t$scr_yr
      times <- seq(2019, 2019+year, by = 1)
      cost_screening <- dia$screening_cost+extra$total_cost
      cost_per_year <- 0
      cost_screening_func <- function(time,state,parms){
        list(parms)
      }
      parms <- as.numeric((people_screening/year)*cost_screening)
      out <- ode( y = cost_per_year,times =  times, func = cost_screening_func, parms = parms, method = "rk4")
      cost_screening_plot <- as.data.frame(out)
      colnames(cost_screening_plot)[2] <- "Total_Cost"
      cost_screening_plot
    })

    # dia <-  reactiveValues(screening_name = "",
    #                        screening_sens = 0,
    #                        screening_spec = 0,
    #                        screening_cost = 0,
    #                        confirming_name = "",
    #                        confirming_sens = 0,
    #                        confirming_spec = 0,
    #                        confirming_cost = 0
    # )

    cost_Confirming_plot <- reactive({

      people_Confirming <- p_t$S_screening*dia$screening_sens/100*dia$screening_spec/100
      year <- p_t$scr_yr
      times <- seq(2019, 2019+year, by = 1)
      cost_Confirming <- dia$confirming_cost
      cost_per_year <- 0
      cost_screening_func <- function(time,state,parms){
        list(parms)
      }
      parms <- as.numeric((people_Confirming/year)*cost_Confirming)
      out <- ode( y = cost_per_year,times =  times, func = cost_screening_func, parms = parms, method = "rk4")
      cost_Confirming_plot <- as.data.frame(out)
      colnames(cost_Confirming_plot)[2] <- "Total_Cost"
      cost_Confirming_plot
    })


    #treatment cost
    cost_treatment_plot <- reactive({
      cost_times <- seq(2019, 2060, by = 1)
      cost_per_year <- 0
      cost_treatment_func <- function(time,state,parms){
        list(parms)
      }
      cost <- as.numeric(Treatment$cost)
      out <- ode( y = cost_per_year,times =  cost_times, func = cost_treatment_func, parms = cost, method = "rk4")
      cost_treatment_plot <- as.data.frame(out)
      colnames(cost_treatment_plot)[2] <- "Total_Cost"
      cost_treatment_plot
    })

    ############################Baseline############################################################


    parms_base <- reactive({
      list(
        P0= 61623143,       #popstat(YEAR=1999)
        K= 68508515,        #Maximum population (carrying capacity)
        r = 0.16,        #Population growth rate (logistic growth curve)
        flowin = 1.15*(10^-8),
        caset0 =  61623143*0.03,      #0.03*P0,
        standard_start = 2004,
        new_start = 2019,
        nscr = 0.005,
        scr_yr = 0,
        scr_cov = 0,      #0.9/scr_yr,
        sens = 0.985,
        pF0scr = 0.1439,
        pF1scr = 0.2969,
        pF2scr=0.1098,
        pF3scr=0.1466,
        pC1scr=0.1998,
        pC2scr=0.0819,
        pC3scr=0.0096,
        pC4scr=0.0011,

        #Natural rate of death
        natdeath=0.04, #Unrelated to cirrhosis and hepatitis C

        beta= 0.02,              #Transmission coefficient

        #standard treatment allocation
        F0std = 0.05,
        F1std = 0.05,
        F2std = 0.3,
        F3std = 0.3,
        C1std = 0.3,

        std_cureF0=0.7,
        std_cureF1=0.7,
        std_cureF2=0.7,
        std_cureF3=0.7,
        std_cureC1=0.7,
        new_cureF0=0,
        new_cureF1=0,
        new_cureF2=0,
        new_cureF3=0,
        new_cureC1=0,
        new_cureC2=0,
        new_cureC3=0,
        new_cureC4=0,

        #Progression of fibrosis
        f0f1= 0.117,       #Fibrosis stage F0 to F1
        f1f2= 0.085,       #Fibrosis stage F1 to F2
        f2f3= 0.12,        #Fibrosis stage F2 to F3
        f3c1= 0.116,       #Fibrosis stage F3 to C1

        #Progression of cirrhosis
        c1c2=0.044,       #Fibrosis stage C1 to C2
        c2c3=0.044,       #Fibrosis stage C2 to C3
        c3c4=0.076,       #Fibrosis stage C3 to C4

        #Incidence of developing HCC
        c1bA= 0.0068,      #Fibrosis stage C1 to bA
        c1bB= 0.0099,      #Fibrosis stage C1 to bB
        c1bC= 0.0029,      #Fibrosis stage C1 to bC
        c1bD= 0.0068,      #Fibrosis stage C1 to bD

        c2bA= 0.0068,      #Fibrosis stage C2 to bA
        c2bB= 0.0099,      #Fibrosis stage C2 to bB
        c2bC= 0.0029,      #Fibrosis stage C2 to bC
        c2bD= 0.0068,      #Fibrosis stage C2 to bD

        c3bD= 0.0664,      #Fibrosis stage C3 to bD
        c4bD= 0.0664,      #Fibrosis stage C4 to bD

        #Death rate from cirrhosis and HCC
        deathc1=0.01,         #Death rate for Cirrhosis Child-Pugh class A
        deathc2=0.01,         #Death rate for Cirrhosis Child-Pugh class B
        deathc3=0.2,          #Death rate for Cirrhosis Child-Pugh class C
        deathc4=0.57,         #Death rate for Cirrhosis Child-Pugh class D

        deathbA=1/(36/12),    #Death rate for HCC_BCLC_A
        deathbB=1/(16/12),    #Death rate for HCC_BCLC_B
        deathbC=1/(6/12),     #Death rate for HCC_BCLC_C
        deathbD=1/(3/12),     #Death rate for HCC_BCLC_D

        deathtran=1/(240/12),

        #Transplantation
        tranc4=0.0015, #Transplantation rate in cirrhosis stage C4
        tranbA=0.0015, #Transplantation rate in HCC_BCLC_A
        tranbB=0.0015 #Transplantation rate in HCC_BCLC_B



      )
      })


    P0_base <- 61623143
    caset0_base <- 0.03*P0_base
    inits_base <-  reactive({
      c(S= 1*(P0_base - caset0_base),
              F0=0.2825*caset0_base,
              F1=0.2825*caset0_base,
              F2=0.184*caset0_base,
              F3=0.124*caset0_base,
              C1=0.03175*caset0_base,
              C2=0.03175*caset0_base,
              C3=0.03175*caset0_base,
              C4=0.03174*caset0_base,
              HCC_A=0,
              HCC_B=0,
              HCC_C=0,
              HCC_D=0,
              D=0,
              dthC14=0,
              dthHCC=0,
              C1std_cured=0,
              C1new_cured=0,
              C2new_cured=0,
              C3new_cured=0,
              C4new_cured=0)
    })

    out_df_base <- reactive({

      times_base <- seq(1999, 2060,by=1)

      out_base <- ode( y = inits_base(),times =  times_base, func = PanHepC, parms = parms_base())

      out_df_base <- as.data.frame(out_base)
      out_df_base
    })


    ############################Baseline############################################################



    #output 1
    output$distPlot <- renderPlotly({
      if (v$doPlot == FALSE) return()

      x <- out_df()[,c(1,23)]
      x_base <- out_df_base()[,c(1,23)]

      isolate({
            withProgress(message = 'Calculation in progress', {
              if(input$screening == 3){
                x_melt_base <- reshape2::melt(x_base, id="time")
                p <-ggplot(data = x_melt_base) +
                  labs( x = "Year", y = "Prevalence (%)")+
                  geom_line(mapping = aes(x = time, y = value,color = variable),size = 1)+
                  theme(axis.title = element_text(size = 20))+
                  theme(axis.text = element_text(size = 15, colour="black"))+
                  theme(legend.title = element_blank(),
                        legend.text = element_blank(),
                        legend.position = c(.95, .95),
                        legend.justification = c("right", "top"),)+
                  ggtitle("baseline") +
                  theme(plot.title = element_text(size=30, face="bold"))
                ggplotly(p)%>%
                  add_trace(
                    marker = list(color='green')
                  )

              }
              else{


                x2 <- as.data.frame(rbind(x,x_base))
                x2_melt <- reshape2::melt(x2, id="time")
                name_type <- c(rep("New Screening Scheme",length(out_df()[,1])),rep("Baseline",length(out_df()[,1])))
                x2_melt_named <- data.frame(x2_melt,type=name_type)

                p <-ggplot(data = x2_melt_named) +
                  labs( x = "Year", y = "Prevalence (%)")+
                  geom_line(mapping = aes(x = time, y = value,linetype = type),size = 1)+
                  scale_linetype_manual(values=c( "dotted","solid"))+
                  theme(axis.title = element_text(size = 20))+
                  theme(axis.text = element_text(size = 15, colour="black"))+
                  theme(legend.title = element_blank())
                ggplotly(p)%>%
                  layout(legend = list(x = 0.75, y = 0.9 ,font = list(size = 15) ))

              }

            })
      })
    })

    #output 2
    output$distPlot2 <- renderPlotly({
      if (v$doPlot == FALSE) return()

      x <- out_df()[,c(1,15,16,17)]
      x_base <- out_df_base()[,c(1,15,16,17)]
      names(x)[2] <- "Total"
      names(x)[3] <- "From Cirrhosis "
      names(x)[4] <- "from HCC"

      names(x_base)[2] <- "Total"
      names(x_base)[3] <- "From Cirrhosis "
      names(x_base)[4] <- "from HCC"

      if(input$showNewDeath){

        x <- out_df()[,c(1,15,16,17,28)]
        x_base <- out_df_base()[,c(1,15,16,17,28)]


      }

      isolate({
        withProgress(message = 'Calculation in progress', {

            if(input$screening == 3){

              x_melt_base <- reshape2::melt(x_base, id="time")
              ggplot(data = x_melt_base) +
                labs( x = "Year", y = "Number")+
                geom_line(mapping = aes(x = time, y = value,color = variable),size = 1)+
                theme(axis.title = element_text(size = 20))+
                theme(axis.text = element_text(size = 15, colour="black"))+
                theme(legend.title = element_text(size = 20),
                      legend.text = element_text(size = 15))+
                ggtitle("baseline") +
                theme(plot.title = element_text(size=30, face="bold"))+
                scale_y_continuous(labels = scales::comma)

            }

            else{
              type <- c(rep("New Screening Scheme",length(out_df()[,1])),rep("Baseline",length(out_df()[,1])))
              x_melt <- reshape2::melt(x, id="time")
              x_melt_base <- reshape2::melt(x_base, id="time")
              x2 <- as.data.frame(rbind(x,x_base))
              x2_melt <-melt(x2, id="time")
              x2_melt_named <- data.frame(x2_melt,type=type)

              p <-ggplot(data = x2_melt_named) +
                labs( x = "Year", y = "Number")+
                geom_line(mapping = aes(x = time, y = value,color = variable,linetype = type),size = 1)+
                scale_linetype_manual(values=c( "dotted","solid"))+
                theme(axis.title = element_text(size = 20))+
                theme(axis.text = element_text(size = 15, colour="black"))+
                theme(legend.title = element_blank())+
                scale_y_continuous(labels = scales::comma)
              ggplotly(p)%>%
                layout(legend = list(font = list(size = 15) ))
            }
        })
      })
    })

    #output 3
    output$distPlot3 <- renderPlotly({

      if (v$doPlot == FALSE) return()
      x <- out_df()[,c(1,26,27)]
      x_base <- out_df_base()[,c(1,26,27)]

      isolate({
        withProgress(message = 'Calculation in progress', {

            if(input$screening == 3){

              x_melt_base <- reshape2::melt(x_base, id="time")
              ggplot(data = x_melt_base, y = "Number") +
                labs( x = "Year")+
                geom_line(mapping = aes(x = time, y = value,color = variable),size = 1)+
                theme(axis.title = element_text(size = 20))+
                theme(axis.text = element_text(size = 15, colour="black"))+
                theme(legend.title = element_text(size = 20),
                      legend.text = element_text(size = 15))+
                ggtitle("baseline") +
                theme(plot.title = element_text(size=30, face="bold"))+
                scale_y_continuous(labels = scales::comma)

            }

            else {




              # type <- c(rep("New Screening Scheme",length(out_df()[,1])),rep("Baseline",length(out_df()[,1])))
              # x_melt <- reshape2::melt(x, id="time")
              # x_melt_base <- reshape2::melt(x_base, id="time")
              # x2 <- as.data.frame(rbind(x,x_base))
              # x2_melt <-melt(x2, id="time")
              # x2_melt_named <- data.frame(x2_melt,type=type)
              #
              # p <-ggplot(data = x2_melt_named) +
              #   labs( x = "Year", y = "Number")+
              #   geom_line(mapping = aes(x = time, y = value,color = variable,linetype = type),size = 1)+
              #   scale_linetype_manual(values=c( "dotted","solid"))+
              #   theme(axis.title = element_text(size = 20))+
              #   theme(axis.text = element_text(size = 15, colour="black"))+
              #   theme(legend.title = element_blank())
              # ggplotly(p)%>%
              #   layout(legend = list(font = list(size = 15) ))

              ay <- list(
                tickfont = list(color = "red"),
                overlaying = "y",
                side = "right",
                title = "Number"
              )
              fig <- plot_ly()
              fig <- fig %>% add_lines(x = x[,1], y = x[,3], name =  "New Screening Scheme - Total HCV",line = list(color = 'rgb(0, 0, 0)'))
              fig <- fig %>% add_lines(x = x_base[,1], y = x_base[,3], name =  "Baseline - Total HCV", line = list(color = 'rgb(0, 0, 0)', dash = 'dash'))
              fig <- fig %>% add_lines(x = x[,1], y = x[,2], name =  "New Screening Scheme - Total HCC", yaxis = "y2",line = list(color = 'rgb(255, 0, 0)'))
              fig <- fig %>% add_lines(x = x_base[,1], y = x_base[,2], name =  "Baseline - Total HCC", yaxis = "y2",line = list(color = 'rgb(255, 0, 0)', dash = 'dash'))
              fig <- fig %>% layout(
                yaxis2 = ay,
                xaxis = list(title="Year"),
                yaxis = list (title = "Number"),
                legend = list(font = list(size = 15))
              )
              fig

            }
        })
      })
    })

    output$distPlot8 <- renderPlotly({

      if (v$doPlot == FALSE) return()
      x <- out_df()[c(21:62),c(1,29)]
      x_base <- out_df_base()[c(21:62),c(1,29)]

      isolate({
        withProgress(message = 'Calculation in progress', {

          if(input$screening == 3){

            x_melt_base <- reshape2::melt(x_base, id="time")
            ggplot(data = x_melt_base, y = "Number") +
              labs( x = "Year")+
              geom_line(mapping = aes(x = time, y = value,color = variable),size = 1)+
              theme(axis.title = element_text(size = 20))+
              theme(axis.text = element_text(size = 15, colour="black"))+
              theme(legend.title = element_text(size = 20),
                    legend.text = element_text(size = 15))+
              ggtitle("baseline") +
              theme(plot.title = element_text(size=30, face="bold"))+
              scale_y_continuous(labels = scales::comma)

          }

          else {




            type <- c(rep("New Screening Scheme",length(out_df()[c(21:62),1])),rep("Baseline",length(out_df()[c(21:62),1])))
            x_melt <- reshape2::melt(x, id="time")
            x_melt_base <- reshape2::melt(x_base, id="time")
            x2 <- as.data.frame(rbind(x,x_base))
            x2_melt <-melt(x2, id="time")
            x2_melt_named <- data.frame(x2_melt,type=type)

            p <-ggplot(data = x2_melt_named) +
              labs( x = "Year", y = "Number")+
              geom_line(mapping = aes(x = time, y = value,color = variable,linetype = type),size = 1)+
              scale_linetype_manual(values=c( "dotted","solid"))+
              theme(axis.title = element_text(size = 20))+
              theme(axis.text = element_text(size = 15, colour="black"))+
              theme(legend.title = element_blank())+
              scale_y_continuous(labels = scales::comma)
            ggplotly(p)%>%
              layout(legend = list(font = list(size = 15) ))
          }
        })
      })
    })

    #output 4
    output$distPlot4 <- renderPlotly({

      if (v$doPlot == FALSE) return()
      x <- out_df()[,c(1,32)]
      x_base <- out_df_base()[,c(1,32)]

      isolate({
        withProgress(message = 'Calculation in progress', {

            if(input$screening == 3){

              x_melt_base <- reshape2::melt(x_base, id="time")
              ggplot(data = x_melt_base) +
                labs( x = "Year", y = "Number")+
                geom_line(mapping = aes(x = time, y = value,color = variable),size = 1)+
                theme(axis.title = element_text(size = 20))+
                theme(axis.text = element_text(size = 15, colour="black"))+
                theme(legend.title = element_text(size = 20),
                      legend.text = element_text(size = 15))+
                ggtitle("baseline") +
                theme(plot.title = element_text(size=30, face="bold"))+
                scale_y_continuous(labels = scales::comma)

            }

            else{



              type <- c(rep("New Screening Scheme",length(out_df()[,1])),rep("Baseline",length(out_df()[,1])))
              x_melt <- reshape2::melt(x, id="time")
              x_melt_base <- reshape2::melt(x_base, id="time")
              x2 <- as.data.frame(rbind(x,x_base))
              x2_melt <-melt(x2, id="time")
              x2_melt_named <- data.frame(x2_melt,type=type)

              p <-ggplot(data = x2_melt_named) +
                labs( x = "Year", y = "Number")+
                geom_line(mapping = aes(x = time, y = value,color = variable,linetype = type),size = 1)+
                scale_linetype_manual(values=c( "dotted","solid"))+
                theme(axis.title = element_text(size = 20))+
                theme(axis.text = element_text(size = 15, colour="black"))+
                theme(legend.title = element_blank())+
                scale_y_continuous(labels = scales::comma)
              ggplotly(p)%>%
                layout(legend = list(font = list(size = 15) ))
            }
        })
      })
    })

    #output 5
    output$distPlot5 <- renderPlotly({

      if (v$doPlot == FALSE) return()
      x <- out_df()[,c(1,33)]
      x_base <- out_df_base()[,c(1,33)]
      isolate({
        withProgress(message = 'Calculation in progress', {

            if(input$screening == 3){

              x_melt_base <- reshape2::melt(x_base, id="time")
              ggplot(data = x_melt_base) +
                labs( x = "Year", y = "Number")+
                geom_line(mapping = aes(x = time, y = value,color = variable),size = 1)+
                theme(axis.title = element_text(size = 20))+
                theme(axis.text = element_text(size = 15, colour="black"))+
                theme(legend.title = element_text(size = 20),
                      legend.text = element_text(size = 15))+
                ggtitle("baseline") +
                theme(plot.title = element_text(size=30, face="bold"))+
                scale_y_continuous(labels = scales::comma)

            }

            else {

              type <- c(rep("New Screening Scheme",length(out_df()[,1])),rep("Baseline",length(out_df()[,1])))
              x_melt <- reshape2::melt(x, id="time")
              x_melt_base <- reshape2::melt(x_base, id="time")
              x2 <- as.data.frame(rbind(x,x_base))
              x2_melt <-melt(x2, id="time")
              x2_melt_named <- data.frame(x2_melt,type=type)

              p <-ggplot(data = x2_melt_named) +
                labs( x = "Year", y = "Number")+
                geom_line(mapping = aes(x = time, y = value,color = variable,linetype = type),size = 1)+
                scale_linetype_manual(values=c( "dotted","solid"))+
                theme(axis.title = element_text(size = 20))+
                theme(axis.text = element_text(size = 15, colour="black"))+
                theme(legend.title = element_blank())+
                scale_y_continuous(labels = scales::comma)
              ggplotly(p)%>%
                layout(legend = list(font = list(size = 15) ))
                }
        })
      })
    })

    #output 6
    output$distPlot6 <- renderPlotly({

      if (v$doPlot == FALSE) return()
      x <- cost_treatment_plot()
      isolate({
        withProgress(message = 'Calculation in progress', {

      ggplot(data = x) +
          labs( x = "Year")+
        geom_line(mapping = aes(x = time, y =Total_Cost ),size = 1)+
          theme(axis.title = element_text(size = 20))+
          theme(axis.text = element_text(size = 15, colour="black"))+
          theme(legend.title = element_text(size = 20),
                legend.text = element_text(size = 15))+
            scale_y_continuous(labels = scales::comma)
        })
      })
    })

    #output 7
    output$distPlot7 <- renderPlotly({

      if (v$doPlot == FALSE) return()
      x <- cost_screening_plot()
      y <- cost_Confirming_plot()
      z <- data.frame(time=x[,1],Total_Cost=(x[,2]+y[,2]))
      isolate({
        withProgress(message = 'Calculation in progress', {

          ggplot(data = z) +
            labs( x = "Year")+
            geom_line(mapping = aes(x = time, y =Total_Cost ),size = 1)+
            scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))+
            theme(axis.title = element_text(size = 20))+
            theme(axis.text = element_text(size = 15, colour="black"))+
            theme(legend.title = element_text(size = 20),
                  legend.text = element_text(size = 15))
        })
      })
    })

    output$table_screening <- DT::renderDataTable({
      if (v$doPlot == FALSE) return()
      screening_cost <- dia$screening_cost
      screening_table <-data.frame(out_df()[c(21:62),c(1,33)], screening_cost= out_df()[c(21:62),33]*screening_cost)

      withProgress(message = 'Calculation in progress', {
        DT::datatable(screening_table,
                      rownames = FALSE,
                      options = list( pageLength = length(out_df()[c(21:62),1]),paging = FALSE
                                      )
                      )

      })
    })



    output$table_cost <- DT::renderDataTable({
      if (v$doPlot == FALSE) return()
      # df_new <- out_df()[1:61,]
      # screening_people <-out_df()[c(21:62), 33]
      # # Confirming_people <-out_df()[c(21:62), 33]
      # for (i in 1:p_t$scr_yr) {
      #   screening_people[i] <- p_t$S_screening/p_t$scr_yr
      # }
      # Confirming_cost <- round(screening_people*(dia$screening_sens/100)*(dia$screening_spec/100)*dia$confirming_cost)
      # screening_cost <- screening_people*(dia$screening_cost+extra$total_cost) + Confirming_cost
      # Treatment_people <-data.frame(round(out_df()[c(21:62),32]))
      # Treatment_cost <- Treatment_people*Treatment$cost
      # Total_cost <- screening_cost + Treatment_cost
      # Total_cost_dis <- Total_cost*0.97

      cost_df <- as.data.frame(cost_utility_list()["cost_new_df"])
      screening_cost <- round(cost_df[,2] / 1e6, 2)
      Treatment_cost <- round(cost_df[,3]  / 1e6, 2)
      Total_cost     <- round(cost_df[,4]  / 1e6, 2)
      Total_cost_dis <- round(cost_df[,5]  / 1e6, 2)
      Treatmen_Cost_table <- data.frame(cost_df[,1],screening_cost,Treatment_cost,Total_cost,Total_cost_dis)
      total_row <-  colSums(Treatmen_Cost_table)
      total_row[1] <- "Total"
      Treatmen_Cost_table <- rbind(Treatmen_Cost_table, total_row)
      table_name <-c("Times","screening cost (Million)","Treatment cost (Million)",
                     "Total cost (Million)","Total cost with discount(3%)(Million)")
      names(Treatmen_Cost_table) <- table_name

      v$tableCost <- cbind(table_name,t(Treatmen_Cost_table[c(1:12,17,22,27,32,37,42),]))
      withProgress(message = 'Calculation in progress', {
        DT::datatable(Treatmen_Cost_table,
                      rownames = FALSE,
                      options = list( pageLength = length(out_df()[c(21:62),1]),paging = FALSE
                      )
        )

      })
    })




    output$table_Treatment <- DT::renderDataTable({
      if (v$doPlot == FALSE) return()
      dia_mutiply <- dia$screening_sens/100*dia$screening_spec/100*dia$confirming_sens/100*dia$confirming_spec/100
      Treatment_people <-data.frame(round(out_df()[c(21:62),33]*dia_mutiply))
      Treatment_cost <- Treatment$cost
      Treatmen_table <- data.frame(out_df()[c(21:62),1],Treatment_people,Treatment_people*Treatment_cost)
      names(Treatmen_table)[1] <- "Times"
      names(Treatmen_table)[2] <- "Treatment people"
      names(Treatmen_table)[3] <- "Treatment cost"

      withProgress(message = 'Calculation in progress', {
        DT::datatable(Treatmen_table,
                      rownames = FALSE,
                      options = list( pageLength = length(out_df()[c(21:62),1]),paging = FALSE
                      )
        )

      })
    })

    #utility
    #fibrosis[20:61]*0.73+compensate[20:61]*0.7+
    #decompensate[20:61]*0.58+totalHCC[20:61]*0.58+
    # diff(death_cir)[19:60]*27+diff(death_HCC)[19:60]*17

    output$table_Utility <- DT::renderDataTable({
      if (v$doPlot == FALSE) return()

      Utility <-data.frame(as.data.frame(cost_utility_list()["utility_new_df"]) )
      total_row <-  colSums(Utility)
      Utility <- rbind(Utility, total_row)
      Utility[43,1] <- "Total"
      table_name <-c("Times","Fibrosis","Compensate","Decompensate","Total Infection",
                     "Total HCC","Incidence HCC","New Death","Death HCC","Total Death",
                     "Total Death HCC","Utility","Utility With discount")
      names(Utility) <- table_name

      v$tableU <- cbind(table_name,t(Utility[c(1:12,17,22,27,32,37,42),]))
      withProgress(message = 'Calculation in progress', {
        Utility %>%
          mutate(Fibrosis = formatC(round(Fibrosis), format = "f", big.mark = ",", drop0trailing = TRUE),
                 Compensate = formatC(round(Compensate), format = "f", big.mark = ",", drop0trailing = TRUE),
                 Decompensate = formatC(round(Decompensate), format = "f", big.mark = ",", drop0trailing = TRUE),
                 `Total HCC` = formatC(round(`Total HCC`), format = "f", big.mark = ",", drop0trailing = TRUE),
                 `New Death` = formatC(round(`New Death`), format = "f", big.mark = ",", drop0trailing = TRUE),
                 `Death HCC` = formatC(round(`Death HCC`), format = "f", big.mark = ",", drop0trailing = TRUE),
                 Utility = formatC(round(Utility), format = "f", big.mark = ",", drop0trailing = TRUE),
                 `Utility With discount` = formatC(round(`Utility With discount`), format = "f", big.mark = ",", drop0trailing = TRUE)
                 ) %>%
        DT::datatable(
                      rownames = FALSE,
                      options = list( pageLength = length(out_df()[c(21:62),1]),paging = FALSE
                      )
        )

      })
    })

    #utility plot
    output$utility_plot <- renderPlotly({
      if (v$doPlot == FALSE) return()
      df <- out_df()
      utility <- data.frame(df[c(21:62),1],df[c(21:62),34]*0.73,df[c(21:62),35]*0.7,df[c(21:62),36]*0.58,df[c(21:62),c(26)]*0.58)

      names(utility)[1] <- "Times"
      names(utility)[2] <- "Fibrosis"
      names(utility)[3] <- "Compensate"
      names(utility)[4] <- "Decompensate"
      names(utility)[5] <- "Total HCC"

      utility_melt <- reshape2::melt(utility, id="Times")

      ggplot(data = utility_melt) +
        labs( x = "Year", y = "Number")+
        geom_line(mapping = aes(x = Times, y = value,color = variable),size = 1)+
        theme(axis.title = element_text(size = 20))+
        theme(axis.text = element_text(size = 15, colour="black"))+
        theme(legend.title = element_text(size = 20),
              legend.text = element_text(size = 15))+
        scale_y_continuous(labels = scales::comma)
    })

    output$dif_TotalHCC <- renderPlotly({
      if (v$doPlot == FALSE) return()
          dif_TotalHCC_graph <- data.frame(out_df()[c(21:62),1], ((out_df_base()[c(21:62),26]-out_df()[c(21:62),26])/out_df_base()[c(21:62),26]*100))
          names(dif_TotalHCC_graph)[1] <- "Times"
          names(dif_TotalHCC_graph)[2] <- "Dif_TotalHCC"
          p <-ggplot(data = dif_TotalHCC_graph) +
            labs( x = "Year", y = "Percent (%)")+
            geom_line(mapping = aes(x = Times, y = Dif_TotalHCC),size = 1)+
            theme(legend.title = element_blank())
    })

    output$dif_Prevalence <- renderPlotly({
      if (v$doPlot == FALSE) return()
      dif_TotalHCC_graph <- data.frame(out_df()[c(21:62),1], ((out_df_base()[c(21:62),23]-out_df()[c(21:62),23])/out_df_base()[c(21:62),23]*100))
      names(dif_TotalHCC_graph)[1] <- "Times"
      names(dif_TotalHCC_graph)[2] <- "Dif_Prevalence"
      p <-ggplot(data = dif_TotalHCC_graph) +
        labs( x = "Year", y = "Percent (%)")+
        geom_line(mapping = aes(x = Times, y = Dif_Prevalence),size = 1)+
        theme(legend.title = element_blank())
    })

    output$dif_incidenceHCC <- renderPlotly({
      if (v$doPlot == FALSE) return()
      dif_incidenceHCC_graph <- data.frame(out_df()[c(21:62),1], ((out_df_base()[c(21:62),29]-out_df()[c(21:62),29])/out_df_base()[c(21:62),29]*100))
      names(dif_incidenceHCC_graph)[1] <- "Times"
      names(dif_incidenceHCC_graph)[2] <- "Dif_incidenceHCC"
      p <-ggplot(data = dif_incidenceHCC_graph) +
        labs( x = "Year", y = "Percent (%)")+
        geom_line(mapping = aes(x = Times, y = Dif_incidenceHCC),size = 1)+
        theme(legend.title = element_blank())
    })
    output$dif_deathHCC <- renderPlotly({
      if (v$doPlot == FALSE) return()
      deathHCC_new <- out_df()[c(22:62),17] - out_df()[c(21:61),17]
      deathHCC_base <- out_df_base()[c(22:62),17] - out_df_base()[c(21:61),17]
      dif_deathHCC_graph <- data.frame(out_df()[c(22:62),1], ((deathHCC_base - deathHCC_new)/deathHCC_base*100))
      names(dif_deathHCC_graph)[1] <- "Times"
      names(dif_deathHCC_graph)[2] <- "Dif_deathHCC"
      p <-ggplot(data = dif_deathHCC_graph) +
        labs( x = "Year", y = "Percent (%)")+
        geom_line(mapping = aes(x = Times, y = Dif_deathHCC),size = 1)+
        theme(legend.title = element_blank())
    })
    output$dif_totaldeath <- renderPlotly({
      if (v$doPlot == FALSE) return()
      totaldeath_new <- out_df()[c(22:62),15] - out_df()[c(21:61),15]
      totaldeath_base <- out_df_base()[c(22:62),15] - out_df_base()[c(21:61),15]
      dif_totaldeath_graph <- data.frame(out_df()[c(22:62),1], ((totaldeath_base-totaldeath_new)/totaldeath_base*100))
      names(dif_totaldeath_graph)[1] <- "Times"
      names(dif_totaldeath_graph)[2] <- "Dif_totaldeath"
      p <-ggplot(data = dif_totaldeath_graph) +
        labs( x = "Year", y = "Percent (%)")+
        geom_line(mapping = aes(x = Times, y = Dif_totaldeath),size = 1)+
        theme(legend.title = element_blank())
    })

    output$daa_cost <- renderPlotly({
      if (v$doPlot == FALSE) return()
      cost_daa_new <- Treatment$cost/30
      daa_graph <- data.frame(out_df()[c(21:62),1], out_df()[c(21:62),32]*cost_daa_new,out_df_base()[c(21:62),32]*800)
      names(daa_graph)[1] <- "Times"
      names(daa_graph)[2] <- "DAA_cost_New"
      names(daa_graph)[3] <- "DAA_cost_Baseline"
      daa_graph_melt <-melt(daa_graph, id="Times")
      p <-ggplot(data = daa_graph_melt) +
        labs( x = "Year", y = "Cost (USD)")+
        geom_line(mapping = aes(x = Times, y = value,linetype = variable),size = 1)+
        scale_linetype_manual(values=c( "dotted","solid"))+
        theme(axis.title = element_text(size = 20))+
        theme(axis.text = element_text(size = 15, colour="black"))+
        theme(legend.title = element_blank())
      ggplotly(p)%>%
        layout(legend = list(x = 0.75, y = 0.9 ,font = list(size = 15) ))
    })

    output$ICER <- renderPlot({
      withProgress(message = 'Calculation in progress', {
      if (v$doPlot == FALSE) return()
      # print(((dia$screening_sens/100)*(dia$screening_spec/100)*(dia$confirming_sens/100)*(dia$confirming_spec/100)))
      CUlist <- cost_utility_list()
      ncol_cost <- ncol(data.frame(CUlist["cost_base_df"]))
      ncol_utility <- ncol(data.frame(CUlist["utility_base_df"]))

      total_base_dis <- data.frame(CUlist["cost_base_df"])[,ncol_cost]
      total_new_dis <- data.frame(CUlist["cost_new_df"])[,ncol_cost]
      utility_base_dis <- data.frame(CUlist["utility_base_df"])[,ncol_utility]
      utility_new_dis <- data.frame(CUlist["utility_new_df"])[,ncol_utility]
      #ICER_r1_Inc_Cost (1 Million)
      ICER_r1_Inc_Cost <- (sum(total_base_dis)-sum(total_new_dis))/1000000
      ICER_r1_Qal_gain <- (sum(utility_base_dis)-sum(utility_new_dis))
      ICER_r1 <- data.frame(ICER_r1_Inc_Cost,ICER_r1_Qal_gain)
      # print(ICER_r1)
      axis_y1 <- -150000
      if(ICER_r1_Inc_Cost < -140000){
        round_cost <- round(ICER_r1_Inc_Cost)-20000

        axis_y1 <- round(round_cost,-nchar(round_cost)+3)
      }
      axis_y2 <- axis_y1/2

      WTP.5GDP <- 160000
      colours =c("black") #,"blue","purple", "green", "pink","orange", "grey", "darkred","cyan2","blueviolet", "darkgoldenrod4","brown2","lawngreen")
      #plot(summary_ICER_PSA_compareWORST_SIIL[1,2,],summary_ICER_PSA_compareWORST_SIIL[1,1,])

      plot(0, xlim=c(-1100000, 3700000), ylim=c(axis_y1, 100000), xlab="Incremental QALYs",ylab="Incremental Costs THB (Million)", pch=18, col=colours,
           main="Incremental cost-effectiveness ratio (ICER) plane",yaxt="n",xaxt="n" )

      axis(side = 2,at=c(axis_y1,axis_y2,0,50000,100000),labels = c(as.character(axis_y1),as.character(axis_y2),"0","50,000","100,000"))
      axis(side = 1,at=c(-1000000,-500000,0,1500000,3000000),labels = c("-1,000,000","-500,000","0","1,500,000","3,000,000"))

      qq<-c(-1000000,0,1000000)
      ww<-qq*160000/(10^6)
      lines(qq,ww,col='red',lty=2)

      abline(h=0,v=0)
      points(ICER_r1[1,2],ICER_r1[1,1],pch=15,col="maroon", cex=3)
      text(ICER_r1[1,2], ICER_r1[1,1]-15000,paste(round(ICER_r1[1,2]),",", round(ICER_r1[1,1])),col = 'red',cex = 1.5)

      par(ps=16)
      legend("topleft", inset=.01, title="Screening scheme",
             c("By New Screening Scheme"), fill=c("maroon", "sea green","gold"))
      })
    })



    output$screening_p <- renderText({
      paste("screening people :" , round(p_t$S_screening) )
    })

    output$scr_yr_p <- renderText({
      paste("screening year :" , round(p_t$scr_yr) )
    })

    output$scr_cov_p <- renderText({
      paste("screening people per year :" , round(p_t$S_screening/p_t$scr_yr) )
    })

    output$scr_cov_p <- renderText({
      paste("screening people per year :" , round(p_t$S_screening/p_t$scr_yr) )
    })
    output$scr_pos_p <- renderText({
      paste("positive people :" , round(p_t$Pos) )
    })


    output$Info_Scr <- renderText({
      paste("Screening group :" , Info$screening )
    })
    output$Info_ScrM <- renderText({
      paste("Screening method :" , dia$screening_name )
    })
    output$Info_Conf <- renderText({
      paste("Confirming method :" , dia$confirming_name )
    })
    output$Info_Tre <- renderText({
      paste("Drugs :" , Info$treatment )
    })





    output$downloadData <- downloadHandler(
      filename = function() {
        paste("parms", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(flie_download$parms, file, row.names = FALSE)
      },contentType = "text/csv"
    )

    output$downloadData2 <-

    downloadHandler(
      filename = "result.csv",
      content = function(file) {

        write.csv(flie_download$table, file, row.names = FALSE)

      }, contentType = "text/csv"

    )

    output$downloadTable <- downloadHandler(
      filename = function() {
        paste0("TableData_Cost_Utility", ".xlsx")
      },
      content = function(file) {
        wb <- createWorkbook()
        addWorksheet(wb, sheetName = "Cost")
        addWorksheet(wb, sheetName = "Utility")
        writeData(wb, sheet = 1, x = v$tableCost, startCol = 1, startRow = 1)
        writeData(wb, sheet = 2, x = v$tableU, startCol = 1, startRow = 1)


        saveWorkbook(wb, file = file, overwrite = TRUE)
      }
    )



})
