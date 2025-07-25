library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(flexdashboard)  # For gauge
library(scales)         # For nice scales
library(fontawesome)

# ----------------------------
# DEFINING THE UI
# ----------------------------
ui <- dashboardPage(
  dashboardHeader(title = "SCD Risk Calculator"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Instructions", tabName = "instructions", icon = icon("info-circle")),
      menuItem("Risk Calculator", tabName = "calculator", icon = icon("calculator")),
      menuItem("Assumptions", tabName = "assumptions", icon = icon("list-alt")),
      menuItem("References", tabName = "references", icon = icon("list-alt"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Floating risk panel */
        #riskPanel {
          position: sticky;
          top: 30px;
        }
        
        /* Collapsible box styling */
        .box-header {
          cursor: pointer;
        }
        .box-header h3 {
          font-weight: bold;
          color: #3c8dbc;
        }
      "))
    ),
    
    tabItems(
      # Calculator instructions tab
      tabItem(tabName = "instructions",
              fluidRow(
                column(
                  width = 8,
                  box(title = "Sudden Cardiac Death (SCD) Risk Calculator Instructions", width = 12, status = "warning",
                      h3("Instructions For Use:"),
                      tags$ul(
                        tags$li("This calculator does NOT assume cumulative risk from other potential causes of death in its calculation. The generated risk score only considers sudden cardiac death as the cause of death."),
                        tags$li("This calculator should only be used by clinicians. This calculator SHOULD NOT replace good clinical judgement as it has not been validated in clinical practice."),
                        tags$li("An increased risk score should trigger the clinician to review the patient's medical history to determine if all current and appropriate guideline-based care has been provided."),
                        tags$li("This calculator should only be used for patients 18 years or older that have been diagnosed with chronic heart failure and classified as New York Heart Association (NYHA) class II or III."),
                        tags$li("The calculator requires results from electrocardiogram (ECG), echocardiogram, and cardiac and kidney laboratory testing."),
                        tags$li("The calculator uses an intercept of -5.749604. Risk calculation is completed using the plogis function (Probability = 1 / (1 + exp(-risk_value)). 
                                The output 'Calculated Risk Score' should be assessed in regards to the intercept. A score greater than -5.749604 indicates increasing risk. Assess risk using sound clinical judgement."),
                        tags$li("The risk gauge assigns a low-risk (green color) to a risk score less than 5%, a moderate-risk (orange color) to risk scores greater than 5% but less than 10%, and a high-risk (red
                                color) to risk scores 10% and greater. Incidence rates of sudden cardiac death are relatively low (ranging from 1 per 100,000 person-years at infancy to ~200 per 100,000 in the
                                eighth decade of life) with reports and studies indicating a decline in incidence rates (see references 1-4 for more clinical context).")
                        ),
                      h4("The risk calculation uses the following assumptions for the specified cardiac and laboratory results and measurements:"),
                      tags$ul(
                        tags$li("Left ventricular ejection fraction (LVEF) is treated as continuous rather than scored based on classifications."),
                        tags$li("Serum creatinine is expressed in µmol/L and is treated as continuous in this calculator. Laboratories in the United States typically report serum creatinine in mg/dL.
                                Per the KDIGO 2024 Clinical Practice Guideline for the Evaluation and Management of Chronic Kidney Disease, the appropriate conversion factor is 88.4 (e.g., 88.4 µmol/L = 1.0 mg/dL). See reference 5 for more clinical context."),
                        tags$li("This risk calculator assumes no difference in risk level for intraventricular conduction disorders or ventricular tachycardia types (e.g., non-sustained ventricular tachycardia has the same risk weight as sustained ventricular tachycardia, etc."),
                        tags$li("This calculator also assumes that the NYHA class coefficient obtained in modeling corresponds to NYHA Class III (e.g., NYHA class II is given a weight of zero while NYHA Class III is given the weight corresponding to the coefficient).")
                        ),
                  )
                )
              )),
      
      # SCD Risk Calculator
      tabItem(
        tabName = "calculator",
        fluidRow(
          column(
            width = 8,
            # Collapsible Input Sections
            box(
              title = span(icon("user"), "Demographics and Diagnosis History"), 
              width = 12,
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = FALSE,
              selectInput("nyha_class", "What is the patient's most recent NYHA Class?", c("II", "III")),
              selectInput("pulm_htn", "Are there indications of or a diagnosis of pulmonary venous hypertension?", choices = c("No", "Yes")),
              selectInput("intraventricular_conduction_disorder", "Is an intraventricular conduction disorder present?",
                                      choices = c("No", "Right bundle branch block (RBBB)", "Left bundle branch block (LBBB)",
                                                  "Hemibloqueo anterosuperior", "Hemibloqueo posteroinferior",
                                                  "RBBB + Hemibloqueo anterosuperior", "RBBB + Hemibloqueo posteroinferior", "Other")),
              selectInput("vtach", "Is there a history and/or documentation of ventricular tachycardia?",
                                      choices = c("No", "Non-sustained VT", "Sustained VT", "Torsade de Points"))
              # selectInput("hf_etiology", "What type of heart failure etiology has the patient been diagnosed with?",
                                      # choices = c("Idiopathic Dilated Cardiomyopathy", "Ischemic Dilated Cardiomyopathy", "Enolic Dilated Cardiomyopathy",
                                      # "Valvular Cardiomyopathy", "Toxic Dilated Cardiomyopathy", "Post-myocardial dilated cardiomyopathy", 
                                      # "Hypertrophic Cardiomyopathy", "Hypertensive Cardiomyopathy", "Other (how do we define this???)")),
            ),
            
            box(
              title = span(icon("heart-pulse"), "Cardiac Function, Cardiac Imaging, and ECG Findings"),
              width = 12,
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              numericInput("lvef", "Most recent LVEF (%) measurement", value = 50),
              numericInput("left_atrial_size", "Most recent left atrial size (mm) measurement", value = 35),
              numericInput("lvedd", "Most recent LV end-diastolic diameter (mm) measurement", value = 30),
              selectInput("qwaves", "Is necrosis present on Q-waves?", choices = c("No", "Yes")),
              selectInput("lv_hypertrophy", "Is left ventricular hypertrophy present on imaging?", choices = c("No", "Yes"))
              # selectInput("rv_contractility", "Right ventricle contractility alteration", c("Not altered", "Altered")),
              # numericInput("qrs_duration", "What is the most recent QRS duration (ms)?", value = 100)
            ),
            
            box(
              title = span(icon("flask"), "Labs"), 
              width = 12,
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              numericInput("creatinine", "Most recent creatinine (µmol/L) result", value = 90),
              numericInput("pro_bnp", "Most recent Pro-BNP (ng/L) result", value = 9)
            ),
            
            box(
              title = span(icon("pills"), "Medications"),
              width = 12,
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              selectInput("amiodarone", "Is the patient prescribed amiodarone AND taking as prescribed?", c("No", "Yes")),
              selectInput("ccblocker", "Is the patient prescribed a calcium channel blocker AND taking as prescribed?", c("No", "Yes")),
              selectInput("spironolactone", "Is the patient prescribed spironolactone AND taking as prescribed?", c("No", "Yes"))
              # selectInput("beta_blocker", "Is the patient prescribed a beta-blocker AND taking as prescribed?", c("No", "Yes")),
              # selectInput("arb", "Is the patient prescribed an angiotensin-II receptor blocker AND taking as prescribed?", c("No", "Yes")),
            ),
            
            actionButton("calculate", "Calculate Risk", 
                         style = "width: 100%; background-color: #2c3e50; color: white;"),
            hr(),
            tags$div(
              style = "font-size: 0.8em; color: #666;",
              "Last updated: 28 April 2025")
          ),

          # Floating Risk Panel
          column(
            width = 4,
            div(
              id = "riskPanel",
              box(
                title = "Risk Assessment",
                width = 12,
                status = "info",
                solidHeader = TRUE,
                gaugeOutput("risk_gauge", height = "200px"),
                hr(),
                plotOutput("importance_plot", height = "250px"),
                verbatimTextOutput("detailed_stats")
              )
            )
          )
        )
      ),
      
      # Assumptions tab
      tabItem(tabName = "assumptions",
              fluidRow(
                box(title = "Dataset and Modeling Assumptions", width = 12, status = "warning",
                    h3("Data Source Assumptions"),
                    tags$ul(
                      tags$li("Based on the MUSIC dataset (https://physionet.org/content/music-sudden-cardiac-death/1.0.1/)"),
                      tags$li("Study population: Chronic heart failure patients (NYHA Class II and III)"),
                      tags$li("Endpoint definition: Sudden cardiac death (SCD) versus other causes (Survivor, Pump-failure death)"),
                      tags$li("All missing data for 'Exit of the Study' was assumed to be '0 [Survivor]'.")
                    ),
                    h3("Variable Selection Assumptions"),
                    tags$ul(
                      tags$li("Variables were analysed within the SCD group and only variables with statistical significance (p < .05) were modeled."), 
                      tags$li("Variables that had a coefficient of exactly zero based on LASSO (Least Absolute Shrinkage and Selection Operator) were excluded from inclusion due to no computational value."),
                      tags$li("NYHA class limited to II and III per original study design.")
                    ),
                    h3("Statistical Modeling Assumptions"),
                    tags$ul(
                      tags$li("Stepwise logistic regression paired with LASSO was used for modeling."),
                      tags$li("LVEF, Creatinine, and Pro-BNP treated as continuous."),
                    ),
                    h3("Clinical Assumptions"),
                    tags$ul(
                      tags$li("Medication effects assumed constant over time (i.e., each patient took the medication exactly as prescribed with no missed doses.")),
                ),
                h4("Limitations",
                   tags$ul(
                     tags$li("Derived from single-center observational data."),
                     tags$li("Potential unmeasured confounding factors."),
                     tags$li("Validation required in independent cohorts."),
                     tags$li("Not intended for acute decompensated heart failure patients."))
                   )
                )
              ),

      # Clinical references tab
      tabItem(tabName = "references",
              fluidRow(
                column(
                  width = 8,
                  box(title = "Clinical References", width = 12, status = "warning",
                      h3("References:"),
                      tags$ul(
                        tags$li("1. Zeppenfeld K, Tfelt-Hansen J, de Riva M, et al. 2022 ESC Guidelines for the management of patients with ventricular arrhythmias and the prevention of sudden cardiac death. Eur Heart J. 2022 Oct 21;43(40):3997-4126. doi: 10.1093/eurheartj/ehac262. PMID: 36017572."),
                        tags$li("2. Lynge TH, Risgaard B, Banner J, et al. Nationwide burden of sudden cardiac death: A study of 54,028 deaths in Denmark. Heart Rhythm. 2021 Oct;18(10):1657-1665. doi: 10.1016/j.hrthm.2021.05.005. Epub 2021 May 7. PMID: 33965606."),
                        tags$li("3. Stecker EC, Reinier K, Marijon E, et al. Public health burden of sudden cardiac death in the United States. Circ Arrhythm Electrophysiol. 2014 Apr;7(2):212-7. doi: 10.1161/CIRCEP.113.001034. Epub 2014 Mar 7. PMID: 24610738; PMCID: PMC4041478."),
                        tags$li("4. Empana JP, Lerner I, Valentin E, et al. Incidence of Sudden Cardiac Death in the European Union. J Am Coll Cardiol. 2022 May 10;79(18):1818-1827. doi: 10.1016/j.jacc.2022.02.041. PMID: 35512862."),
                        tags$li("5. Kidney Disease: Improving Global Outcomes (KDIGO) CKD Work Group. KDIGO 2024 Clinical Practice Guideline for the Evaluation and Management of Chronic Kidney Disease. Kidney Int. 2024 Apr;105(4S):S117-S314. doi: 10.1016/j.kint.2023.10.018. PMID: 38490803."))
                  )
                )
              )
      )
    )
  )
)




# ----------------------------
# DEFINING THE SERVER LOGIC (based on user inputs)
# ----------------------------
server <- function(input, output) {
  # Reactive container for all risk data
  risk_data <- reactiveValues(
    perc = NULL,
    color = "#5cb85c",
    interpretation = NULL,
    features = NULL,
    details = NULL
  )
  
  observeEvent(input$calculate, {
    # Numeric and Binary Input Processing --------------------------------------
    lvef <- as.numeric(input$lvef)
    left_atrial_size <- as.numeric(input$left_atrial_size)
    creatinine <- as.numeric(input$creatinine)
    pro_bnp <- as.numeric(input$pro_bnp)
    lvedd <- as.numeric(input$lvedd)
    
    pulm_htn <- ifelse(input$pulm_htn == "Yes", 1, 0)
    qwaves <- ifelse(input$qwaves == "Yes", 1, 0)
    spironolactone <- ifelse(input$spironolactone == "Yes", 1, 0)
    amiodarone <- ifelse(input$amiodarone == "Yes", 1, 0)
    ccblocker <- ifelse(input$ccblocker == "Yes", 1, 0)
    lv_hypertrophy <- ifelse(input$lv_hypertrophy == "Yes", 1, 0)
    nyha_class <- ifelse(input$nyha_class == "III", 1, 0)
    
    # Categorical Input Processing ---------------------------------------------
    intraventricular_conduction_disorder <- case_when(
      input$intraventricular_conduction_disorder == "No" ~ 0,
      input$intraventricular_conduction_disorder == "Right bundle branch block (RBBB)" ~ 1,
      input$intraventricular_conduction_disorder == "Left bundle branch block (LBBB)" ~ 1,
      input$intraventricular_conduction_disorder == "Hemibloqueo anterosuperior" ~ 1,
      input$intraventricular_conduction_disorder == "Hemibloqueo posteroinferior" ~ 1,
      input$intraventricular_conduction_disorder == "RBBB + Hemibloqueo anterosuperior" ~ 1,
      input$intraventricular_conduction_disorder == "RBBB + Hemibloqueo posteroinferior" ~ 1,
      input$intraventricular_conduction_disorder == "Other" ~ 1,
      TRUE ~ 1
      )
    
    vtach <- case_when(
      input$vtach == "No" ~ 0,
      input$vtach == "Non-sustained VT" ~ 1,
      input$vtach == "Sustained VT" ~ 1,
      input$vtach == "Torsade de Points" ~ 1,
      TRUE ~ 1
    )
      
    # rv_contractility <- ifelse(input$rv_contractility == "Altered", 1, 0)
    # qrs_duration <- as.numeric(input$qrs_duration)
    
    # hf_etiology <- case_when(
    #   input$hf_etiology == "Idiopathic dilated cardiomyopathy" ~ 1,
    #   input$hf_etiology == "Ischemic dilated cardiomyopathy" ~ 1,
    #   TRUE ~ 1
    # )
    
    # Core Risk Calculation ----------------------------------------------------
    risk_value <-  -5.749604 + # model intercept
      (lvef * -0.01028083) +
      (creatinine * 0.00001908807) +
      (left_atrial_size * 0.04855174) +
      (pro_bnp * 0.00004042754) +
      (lvedd * 0.005486610) +
      (pulm_htn * 0.1991684) +
      (qwaves * 0.5047251) +
      (spironolactone * -0.1137244) +
      (vtach * 0.1434437) +
      (amiodarone * -0.1611773) +
      (ccblocker * -0.4101614) +
      (lv_hypertrophy * 0.05427531) +
      (nyha_class * 0.2552488) +
      (intraventricular_conduction_disorder * 0.06651253)
      # (rv_contractility * 1.211979) +
      # (hf_etiology * 0.03367386) +
      # (qrs_duration * 0.0009671358)
    
    # Result Processing --------------------------------------------------------
    # Calculate total risk at 4 years using plogis
    risk_perc <- plogis(risk_value) * 100  # plogis = (1 + exp(-risk_value))
    
    risk_data$perc <- risk_perc
    
    risk_data$interpretation <- case_when(
      risk_perc <= 4.9 ~ "Low Risk",
      risk_perc <= 9.9 ~ "Moderate Risk",
      TRUE ~ "High Risk"
    )
    
    # Calculation for key contributing factors display
    risk_data$features <- data.frame(
      Feature = c("LVEF", "Creatinine", "Left Atrial Size", "Pro-BNP", "LV End Diastolic Diameter",
                  "Pulmonary Hypertension", "Necrotic Q-waves", "Spironolactone",
                  "VT", "Amiodarone", "Calcium Channel Blocker", "LV Hypertrophy",
                  "NYHA", "Intraventricular Conduction Disorder"),
      Impact = c(
        (50 - lvef) * 0.01028083,
        creatinine * 0.00001908807,
        left_atrial_size * 0.04855174,
        pro_bnp * 0.00004042754,
        lvedd * 0.005486610,
        pulm_htn * 0.1991684,
        qwaves * 0.5047251,
        spironolactone * -0.1137244,
        vtach * 0.1434437,
        amiodarone * -0.1611773,
        ccblocker * -0.4101614,
        lv_hypertrophy * 0.05427531,
        nyha_class * 0.2552488,
        intraventricular_conduction_disorder * 0.06651253
      )
    ) %>% arrange(desc(abs(Impact)))
  
    # 
    risk_data$details <- paste(
      "Calculated Risk Score:", round(risk_value, 4), "\n",
      "Probability:", round(risk_perc, 2), "%\n",
      "Key Parameters:\n",
      "Q-waves:", ifelse(qwaves == 1, "Necrotic | ", "Normal | "),
      "NYHA:", ifelse(nyha_class == 1, "III | ", "II | "),
      "LVEF:", lvef, "% | ",
      "Left Atrial Size:", left_atrial_size, "mm"
    )
  })
  
  # Output Rendering ----------------------------------------------------------
  output$risk_gauge <- renderGauge({
    req(risk_data$perc, risk_data$color)
    
    gauge(
      risk_data$perc,
      min = 0,
      max = 100,
      symbol = "%",
      label = "4-Year SCD Risk",
      sectors = gaugeSectors(
        success = c(0, 4),
        warning = c(5, 9),
        danger = c(10, 100),
        colors = c("#5cb85c", "#f0ad4e", "#d9534f")
      )
    )
  })
  
  output$risk_interpretation <- renderText({
    req(risk_data$interpretation)
    risk_data$interpretation
  })
  
  output$importance_plot <- renderPlot({
    req(risk_data$features)
    
    ggplot(risk_data$features, aes(x = reorder(Feature, Impact), y = Impact)) +
      geom_col(aes(fill = Impact > 0), width = 0.7) +
      scale_fill_manual(
        values = c("FALSE" = "#4daf4a", "TRUE" = "#e41a1c"),
        labels = c("Risk Reduction", "Risk Increase")
      ) +
      coord_flip() +
      labs(
        title = "Key Contributing Factors",
        x = NULL,
        y = "Impact Score"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  output$detailed_stats <- renderText({
    req(risk_data$details)
    risk_data$details
  })
}

# ----------------------------
# RUN THE APPLICATION
# ----------------------------
shinyApp(ui = ui, server = server)
