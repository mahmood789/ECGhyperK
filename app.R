# app.R - ECG Simulator (Electrolytes, AV Blocks, QT Syndromes)

# --- Load necessary libraries ---
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)

# Helper function for clamping values
clamp <- function(x, min_val, max_val) {
  max(min_val, min(x, max_val))
}

# --- ECG Wave Generation Functions ---
generate_segment <- function(time_points, amplitude, duration, center, width_factor = 4) {
  if (is.na(center) || duration <= 0 || is.na(duration) || is.na(amplitude) || amplitude == 0) return(numeric(length(time_points)))
  sigma_equiv <- duration / width_factor
  if (sigma_equiv == 0 && amplitude != 0) return(ifelse(time_points == center, amplitude, 0))
  if (sigma_equiv == 0 && amplitude == 0) return(numeric(length(time_points)))
  scaled_time <- (time_points - center) / sigma_equiv
  wave <- amplitude * exp(-0.5 * scaled_time^2)
  return(wave)
}

generate_p_wave_electrolyte <- function(time_vector, p_start_time, p_duration, p_amplitude, fs = 250) {
  if (is.na(p_start_time) || is.na(p_duration) || is.na(p_amplitude) || p_duration <= 0 || p_amplitude == 0) return(numeric(length(time_vector)))
  p_wave_segment <- generate_segment(time_vector, p_amplitude, p_duration, p_start_time + p_duration / 2, width_factor = 4)
  return(p_wave_segment)
}

generate_qrs_electrolyte <- function(time_vector, qrs_start_time, qrs_duration, q_amp, r_amp, s_amp, fs = 250) {
  if(is.na(qrs_start_time) || is.na(qrs_duration) || qrs_duration <= 0) return(numeric(length(time_vector)))
  q_dur_prop <- 0.25; r_dur_prop <- 0.50; s_dur_prop <- 0.25
  q_dur <- qrs_duration * q_dur_prop; r_dur <- qrs_duration * r_dur_prop;  s_dur <- qrs_duration * s_dur_prop
  q_peak_time <- qrs_start_time + q_dur / 2
  r_peak_time <- qrs_start_time + q_dur + (r_dur / 2)
  s_peak_time <- qrs_start_time + q_dur + r_dur + (s_dur / 2)
  q_wave <- generate_segment(time_vector, q_amp, q_dur, q_peak_time, width_factor = 4.0)
  r_wave <- generate_segment(time_vector, r_amp, r_dur, r_peak_time, width_factor = 4.5)
  s_wave <- generate_segment(time_vector, s_amp, s_dur, s_peak_time, width_factor = 4.0)
  return(q_wave + r_wave + s_wave)
}

generate_t_wave_electrolyte <- function(time_vector, t_start_time, t_duration, t_amplitude, t_width_factor = 3.5, is_inverted = FALSE, t_morphology_param = 0) {
  if(is.na(t_start_time) || is.na(t_duration) || is.na(t_amplitude) || t_duration <= 0 || t_amplitude == 0) return(numeric(length(time_vector)))
  amp <- if(is_inverted) -abs(t_amplitude) else abs(t_amplitude)
  
  # Basic T-wave
  t_wave_main <- generate_segment(time_vector, amp, t_duration, t_start_time + t_duration / 2, width_factor = t_width_factor)
  
  # Experimental: Add a notch or secondary component for LQTS T-wave morphology
  # This is a placeholder for more complex morphology.
  # if (t_morphology_param > 0 && t_duration > 0.1) { # Example: if LQTS type 1, add a broader base or late component
  #   amp2 <- amp * 0.3
  #   dur2 <- t_duration * 0.6
  #   center2 <- t_start_time + t_duration * 0.7
  #   t_wave_secondary <- generate_segment(time_vector, amp2, dur2, center2, width_factor = t_width_factor * 0.8)
  #   t_wave_main <- t_wave_main + t_wave_secondary
  # }
  return(t_wave_main)
}

generate_u_wave_electrolyte <- function(time_vector, u_start_time, u_duration, u_amplitude, fs = 250) {
  if(is.na(u_start_time) || is.na(u_duration) || is.na(u_amplitude) || u_duration <= 0 || u_amplitude <= 0) return(numeric(length(time_vector)))
  return(generate_segment(time_vector, u_amplitude, u_duration, u_start_time + u_duration / 2, width_factor = 4))
}

# --- Clinical Vignettes Data (Serious Tone) ---
# (Vignette list from previous turn remains here - it's long, so I'll omit it for brevity in this code block but assume it's present)
vignettes_list <- list(
  # Potassium
  list(id="k_hypo1", title="Diuretic-Induced Weakness", scenario="A 68-year-old female with a history of heart failure, recently started on a new diuretic regimen, presents with significant muscle weakness, fatigue, and palpitations.", question="What is a likely electrolyte imbalance given her new medication, and what key ECG changes might be observed?", answer_ecg="Flattened T-waves, prominent U-waves, ST segment depression. Possible premature ventricular contractions (PVCs).", answer_electrolyte="Hypokalemia.", explanation="Loop and thiazide diuretics are common causes of urinary potassium loss..."),
  list(id="k_hypo2", title="Gastrointestinal Losses and Arrhythmia", scenario="A 35-year-old male with a severe episode of gastroenteritis, experiencing profuse vomiting and diarrhea for three days, complains of dizziness and an irregular heartbeat.", question="What electrolyte depletion is highly probable, and what ECG findings would support this?", answer_ecg="T-wave flattening or inversion, U-waves, ST depression. QT interval may appear prolonged due to T-U fusion.", answer_electrolyte="Hypokalemia.", explanation="Significant gastrointestinal fluid losses are a major route for potassium depletion..."),
  list(id="k_hyper1", title="Missed Dialysis Appointment", scenario="A 55-year-old male with end-stage renal disease who missed his last two scheduled hemodialysis sessions presents with nausea, muscle weakness, and paresthesias.", question="What life-threatening electrolyte imbalance is expected in this patient, and what are its characteristic progressive ECG manifestations?", answer_ecg="Initial: Tall, peaked T-waves. Moderate: PR interval prolongation, P-wave flattening/loss, QRS widening. Severe: Further QRS widening, potential sine wave pattern...", answer_electrolyte="Hyperkalemia.", explanation="Kidneys are the primary route for potassium excretion..."),
  list(id="k_hyper2", title="ACE Inhibitor and Salt Substitute", scenario="An 72-year-old male with hypertension, recently started on an ACE inhibitor, decided to use a potassium-based salt substitute... He now reports feeling unwell with palpitations.", question="What potential drug-diet interaction could lead to an electrolyte emergency, and what ECG signs would be concerning?", answer_ecg="Peaked T-waves are the earliest sign. Depending on severity, PR prolongation and QRS widening may also occur.", answer_electrolyte="Hyperkalemia.", explanation="ACE inhibitors can cause hyperkalemia by reducing aldosterone secretion..."),
  # Calcium
  list(id="ca_hypo1", title="Post-Thyroidectomy Paresthesias", scenario="A 42-year-old female, three days post-total thyroidectomy, develops circumoral numbness, tingling in her fingertips, and involuntary muscle spasms...", question="What electrolyte disturbance is common after this type of surgery, and what specific ECG change related to repolarization would be expected?", answer_ecg="Prolongation of the QT interval, primarily due to lengthening of the ST segment.", answer_electrolyte="Hypocalcemia (due to potential iatrogenic hypoparathyroidism).", explanation="Inadvertent damage to or removal of the parathyroid glands..."),
  list(id="ca_hypo2", title="Pancreatitis and Neuromuscular Irritability", scenario="A 50-year-old male with acute pancreatitis presents with marked muscle twitching, a positive Chvostek's sign, and generalized weakness.", question="What electrolyte abnormality is frequently associated with acute pancreatitis, and how does it manifest on the ECG?", answer_ecg="Prolonged QT interval due to ST segment lengthening. Risk of Torsades de Pointes if severe.", answer_electrolyte="Hypocalcemia.", explanation="Acute pancreatitis can cause hypocalcemia through several mechanisms..."),
  list(id="ca_hyper1", title="Malignancy-Associated Symptoms", scenario="A 65-year-old male with a known malignancy presents with constipation, polyuria, polydipsia, lethargy, and confusion...", question="What electrolyte disorder is often seen in this context, and how would it typically affect the QT interval on an ECG?", answer_ecg="Shortening of the QT interval, primarily due to a shortened ST segment.", answer_electrolyte="Hypercalcemia (potentially hypercalcemia of malignancy).", explanation="Hypercalcemia of malignancy is a common paraneoplastic syndrome..."),
  list(id="ca_hyper2", title="Excessive Vitamin D Intake", scenario="A 48-year-old female who has been taking very high doses of over-the-counter Vitamin D supplements... reports fatigue, bone pain, and recurrent kidney stones.", question="What electrolyte imbalance can result from Vitamin D toxicity, and what is its characteristic ECG finding related to the ST segment and QT interval?", answer_ecg="Shortened ST segment leading to a shortened QT interval.", answer_electrolyte="Hypercalcemia.", explanation="Vitamin D toxicity leads to excessive calcium absorption..."),
  # Long QT Syndrome
  list(id="lqts1", title="Recurrent Syncope in a Teenager", scenario="A 16-year-old female experiences recurrent episodes of syncope, particularly during swimming and when startled by loud noises. There is a family history of sudden unexplained death...", question="What underlying cardiac electrical disorder should be suspected, and what is the hallmark ECG finding?", answer_ecg="Markedly prolonged QT interval (often QTc > 460ms in females, >440ms in males). T-wave abnormalities (e.g., broad-based, notched) may also be present.", answer_electrolyte="Long QT Syndrome (LQTS), likely congenital.", explanation="Congenital Long QT Syndrome is a channelopathy that predisposes to life-threatening ventricular arrhythmias..."),
  list(id="lqts2", title="Drug-Induced QT Prolongation", scenario="A 60-year-old male recently started on a new antiarrhythmic medication... complains of dizziness and near-fainting spells. His baseline ECG showed a normal QTc.", question="What potential adverse effect of the new medication should be investigated, and what ECG change would confirm it?", answer_ecg="Acquired QT interval prolongation (QTc significantly increased from baseline).", answer_electrolyte="Drug-induced Long QT Syndrome.", explanation="Numerous medications can prolong the QT interval by blocking cardiac ion channels..."),
  # Short QT Syndrome
  list(id="sqts1", title="Sudden Cardiac Arrest Survivor", scenario="A 25-year-old male is successfully resuscitated from a sudden cardiac arrest that occurred during sleep. His prior medical history is unremarkable, but his brother died suddenly at a young age.", question="What rare genetic channelopathy might be responsible, characterized by an unusually brief ventricular repolarization?", answer_ecg="Markedly short QT interval (typically QTc < 330-340 ms). Tall, peaked T-waves may also be seen.", answer_electrolyte="Short QT Syndrome (SQTS).", explanation="Short QT Syndrome is a rare, inherited disorder associated with an increased risk of atrial and ventricular arrhythmias..."),
  list(id="sqts2", title="Palpitations and Family History (SQTS)", scenario="A 30-year-old female reports recurrent palpitations and atrial fibrillation. Her father experienced a sudden cardiac death in his early 40s. Her screening ECG shows a consistently short QTc.", question="Given the family history and ECG finding, what diagnosis should be considered?", answer_ecg="Persistently short QT interval (e.g., QTc < 340 ms). T-waves might be tall and symmetrically peaked.", answer_electrolyte="Short QT Syndrome (SQTS).", explanation="SQTS predisposes individuals to both atrial and ventricular arrhythmias..."),
  # 1st Degree AV Block
  list(id="avb1_1", title="Incidental ECG Finding (1st Degree)", scenario="A 50-year-old male undergoes a routine pre-employment physical. He is asymptomatic... His ECG shows a regular rhythm but a consistently long PR interval.", question="What type of conduction delay does this represent, and is it typically symptomatic?", answer_ecg="First-degree AV block (PR interval > 0.20 seconds, constant, every P conducts).", answer_electrolyte="1st Degree AV Block.", explanation="First-degree AV block represents a delay in conduction from the atria to the ventricles..."),
  list(id="avb1_2", title="Athlete's Bradycardia and PR Prolongation (1st Degree)", scenario="A 22-year-old endurance athlete presents for a sports physical. He has a resting heart rate of 48 bpm and his ECG reveals a PR interval of 0.24 seconds. He denies any symptoms.", question="What common ECG finding in well-trained athletes might this represent?", answer_ecg="First-degree AV block (PR interval > 0.20 seconds). Sinus bradycardia is also present.", answer_electrolyte="1st Degree AV Block (physiologic).", explanation="Increased vagal tone in highly conditioned athletes can lead to both sinus bradycardia and first-degree AV block..."),
  # 2nd Degree AV Block, Mobitz I
  list(id="avb2_1_w", title="Dizziness and Group Beating (Mobitz I)", scenario="A 70-year-old female reports intermittent episodes of lightheadedness. Her ECG shows a pattern where the PR interval progressively lengthens over several beats, followed by a P wave that is not conducted.", question="What type of second-degree AV block is characterized by this progressive PR prolongation before a non-conducted P wave?", answer_ecg="Second-degree AV block, Mobitz Type I (Wenckebach phenomenon). Progressive PR lengthening until a P wave fails to conduct.", answer_electrolyte="2nd Degree AV Block, Mobitz I.", explanation="Mobitz Type I block is usually due to impaired conduction within the AV node..."),
  list(id="avb2_1_p", title="Nocturnal Dropped Beats (Mobitz I)", scenario="During an overnight Holter monitor study... a 60-year-old male is found to have episodes of grouped beating where the PR interval gradually increases before a P wave is blocked. These occur mostly during sleep.", question="What common, often benign, form of AV block does this pattern suggest, especially when occurring nocturnally?", answer_ecg="Second-degree AV block, Mobitz Type I (Wenckebach). Progressive PR prolongation followed by a non-conducted P wave.", answer_electrolyte="2nd Degree AV Block, Mobitz I.", explanation="Nocturnal Wenckebach is common and often benign, attributed to increased vagal tone during sleep..."),
  # 2nd Degree AV Block, Mobitz II
  list(id="avb2_2", title="Intermittent Syncope, Worrisome Block (Mobitz II)", scenario="A 75-year-old male... experiences several episodes of sudden syncope... His ECG shows P waves occurring at a regular rate, with most P waves followed by a QRS complex with a constant PR interval, but occasionally a P wave is not conducted...", question="What type of second-degree AV block, considered more serious, presents with a constant PR interval before non-conducted P waves?", answer_ecg="Second-degree AV block, Mobitz Type II. Constant PR interval in conducted beats, with intermittent non-conducted P waves.", answer_electrolyte="2nd Degree AV Block, Mobitz II.", explanation="Mobitz Type II block typically involves disease below the AV node... It has a higher risk of progressing to complete heart block..."),
  list(id="avb2_2_s", title="Sudden Fatigue and Bradycardia (Mobitz II)", scenario="An 80-year-old patient... presents with sudden onset of severe fatigue and episodes of marked bradycardia. An ECG strip reveals consistent PR intervals for conducted beats, but frequent, unpredictable non-conducted P waves.", question="This pattern of intermittent dropped beats with a stable PR interval is indicative of which type of AV block?", answer_ecg="Second-degree AV block, Mobitz Type II. P waves are regular, PR interval is constant for conducted beats, but some P waves are not followed by a QRS complex.", answer_electrolyte="2nd Degree AV Block, Mobitz II.", explanation="Mobitz II block indicates a more distal conduction system problem..."),
  # 3rd Degree AV Block
  list(id="avb3_1", title="Severe Bradycardia and AV Dissociation (3rd Degree)", scenario="An 82-year-old female presents with profound weakness, confusion, and a heart rate of 35 bpm. Her ECG shows regular P waves occurring at a rate of 70 bpm, and regular QRS complexes occurring at an independent rate of 35 bpm.", question="What type of complete heart block is described, and what are the key ECG features of AV dissociation?", answer_ecg="Third-degree (complete) AV block. AV dissociation: P waves and QRS complexes are independent. Atrial rate > ventricular rate.", answer_electrolyte="3rd Degree AV Block.", explanation="In third-degree AV block, no atrial impulses conduct to the ventricles..."),
  list(id="avb3_2", title="Stokes-Adams Attack (3rd Degree)", scenario="A 78-year-old male reports episodes of abrupt loss of consciousness lasting a few seconds... During one such episode... his pulse was found to be extremely slow.", question="These syncopal episodes (Stokes-Adams attacks) in the context of profound bradycardia are highly suggestive of what advanced AV block?", answer_ecg="Third-degree (complete) AV block. Atrial activity dissociated from ventricular activity. Slow ventricular escape rhythm.", answer_electrolyte="3rd Degree AV Block.", explanation="Stokes-Adams attacks are caused by transient cerebral hypoperfusion due to the very slow and/or unreliable ventricular escape rhythm in complete heart block...")
)

# --- UI Definition ---
ui <- fluidPage(
  theme = shinytheme("spacelab"),
  titlePanel("ECG Simulator: Electrolytes, AV Blocks & QT Syndromes"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("ECG Simulation Mode"),
      selectInput("simulation_mode", "Select Simulation Type:",
                  choices = c("Normal Sinus Rhythm" = "nsr",
                              "Electrolyte Imbalance (K+, Ca++)" = "electrolytes",
                              "AV Blocks" = "av_blocks",
                              "QT Syndromes (Primary)" = "qt_syndromes")),
      
      # Conditional UI for Electrolytes
      conditionalPanel(
        condition = "input.simulation_mode == 'electrolytes'",
        selectInput("electrolyte", "Select Electrolyte:",
                    choices = c("Potassium (K+)" = "potassium", # Mg removed
                                "Calcium (Ca++)" = "calcium")),
        uiOutput("level_ui")
      ),
      
      # Conditional UI for AV Blocks
      conditionalPanel(
        condition = "input.simulation_mode == 'av_blocks'",
        selectInput("av_block_type", "Select AV Block Type:",
                    choices = c("1st Degree AV Block" = "avb1",
                                "2nd Degree AV Block, Mobitz I (Wenckebach)" = "avb2_mobitz1",
                                "2nd Degree AV Block, Mobitz II" = "avb2_mobitz2",
                                "3rd Degree AV Block (Complete Heart Block)" = "avb3")),
        sliderInput("atrial_rate_avb", "Atrial Rate (P waves) (bpm):", min = 50, max = 120, value = 70, step = 5),
        conditionalPanel(
          condition = "input.av_block_type == 'avb3'",
          sliderInput("ventricular_rate_avb3", "Ventricular Escape Rate (bpm):", min = 20, max = 60, value = 40, step = 5)
        ),
        conditionalPanel(
          condition = "input.av_block_type == 'avb2_mobitz1'",
          p(strong("Note:"), "Dynamic Wenckebach pattern not fully simulated in plot; shows underlying rhythm with prolonged PR as placeholder.")
        ),
        conditionalPanel(
          condition = "input.av_block_type == 'avb2_mobitz2'",
          p(strong("Note:"), "Dynamic Mobitz II dropped beats not fully simulated in plot; shows underlying rhythm as placeholder.")
        )
      ),
      
      # Conditional UI for QT Syndromes
      conditionalPanel(
        condition = "input.simulation_mode == 'qt_syndromes'",
        selectInput("qt_syndrome_type", "Select QT Syndrome:",
                    choices = c("Long QT Syndrome (LQTS)" = "lqts",
                                "Short QT Syndrome (SQTS)" = "sqts")),
        sliderInput("qtc_target_ms", "Target QTc (ms):", min = 250, max = 600, value = 440, step = 10)
      ),
      
      hr(),
      sliderInput("heart_rate", "Base Heart Rate (bpm):", 
                  min = 30, max = 180, value = 75, step = 5),
      p(em("Note: For 3rd Degree AV Block, this becomes the atrial rate if 'AV Blocks' mode is selected and 'Atrial Rate' slider above is not used. For other modes, it's the primary ventricular rate.")),
      
      radioButtons("paper_speed", "Paper Speed:", choices = c("25 mm/s" = 25, "50 mm/s" = 50), selected = 25, inline = TRUE),
      radioButtons("ecg_gain", "ECG Gain:", choices = c("5 mm/mV (0.5x)" = 0.5, "10 mm/mV (1x)" = 1.0, "20 mm/mV (2x)" = 2.0), selected = 1.0, inline = TRUE),
      
      hr(),
      h5("Simulated Intervals / Info:"),
      uiOutput("interval_info_display"),
      hr(),
      h5("Caliper Measurements:"),
      uiOutput("caliper_output"),
      actionButton("clear_calipers", "Clear Calipers", class = "btn-sm btn-warning"),
      hr(),
      p(strong("Simulator Note:"), "ECG plot dynamically simulates K+/Ca++ effects, 1st/3rd Degree AV Blocks, and basic LQTS/SQTS. Dynamic patterns for 2nd Degree AV Blocks are not yet fully rendered."),
      p(paste0("Version ECGSim Pro | ", format(Sys.Date(), "%Y")))
    ),
    mainPanel(
      width = 8,
      h3(textOutput("plot_title_text")),
      plotOutput("ecg_plot", height = "350px", click = "plot_click"),
      hr(),
      h4("Key ECG Findings & Educational Notes"),
      uiOutput("educational_notes_display"),
      hr(), 
      h4("Clinical Vignettes - Test Your Knowledge!"),
      selectInput("selected_vignette", "Choose a Case:", choices = c("Select a vignette..." = "")),
      uiOutput("vignette_scenario_question_display"),
      conditionalPanel(
        condition = "input.selected_vignette != ''", 
        actionButton("show_vignette_answer", "Show Clues & Explanation", class = "btn-info btn-sm", style="margin-top: 5px; margin-bottom: 10px;")
      ),
      uiOutput("vignette_answer_explanation_display")
    )
  )
)

# --- Server Logic ---
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    caliper_points = list(), 
    current_level_selection = NULL, # For electrolyte mode
    current_vignette_id = NULL,
    show_vignette_answer_flag = FALSE
  ) 
  
  isolate({
    v_choices <- setNames(lapply(vignettes_list, `[[`, "id"), lapply(vignettes_list, `[[`, "title"))
    updateSelectInput(session, "selected_vignette", choices = c("Select a vignette..." = "", v_choices))
  })
  
  observeEvent(input$clear_calipers, { rv$caliper_points <- list() })
  
  # Reset relevant inputs when simulation mode changes
  observeEvent(input$simulation_mode, {
    rv$caliper_points <- list()
    # Could reset other mode-specific rv values here if needed
  })
  
  observeEvent(input$electrolyte, { 
    rv$caliper_points <- list() 
    if(input$electrolyte == "potassium") rv$current_level_selection <- "k_normal"
    else if(input$electrolyte == "calcium") rv$current_level_selection <- "ca_normal"
    else rv$current_level_selection <- NULL
  })
  observeEvent(input$level, { 
    rv$caliper_points <- list() 
    rv$current_level_selection <- input$level 
  })
  observeEvent(input$heart_rate, { rv$caliper_points <- list() })
  observeEvent(input$av_block_type, { rv$caliper_points <- list() })
  observeEvent(input$qt_syndrome_type, { rv$caliper_points <- list() })
  
  
  output$level_ui <- renderUI({
    req(input$simulation_mode == "electrolytes", input$electrolyte)
    electrolyte_levels <- list(
      potassium = c("Normal K+" = "k_normal", "Hypokalemia" = "k_hypo", "Hyperkalemia" = "k_hyper"),
      calcium   = c("Normal Ca++" = "ca_normal", "Hypocalcemia" = "ca_hypo", "Hypercalcemia" = "ca_hyper")
    )
    selected_val <- rv$current_level_selection 
    choices <- electrolyte_levels[[input$electrolyte]]
    default_selection <- choices[1] 
    if (is.null(selected_val) || !(selected_val %in% unlist(choices)) || !startsWith(selected_val, substr(input$electrolyte,1,1))) {
      selected_val <- default_selection
    }
    selectInput("level", paste(tools::toTitleCase(input$electrolyte), "Level:"), choices = choices, selected = selected_val)
  })
  
  observeEvent(input$plot_click, { 
    current_points <- rv$caliper_points
    if (length(current_points) >= 2) { current_points <- list() }
    new_point <- data.frame(x = input$plot_click$x, y = input$plot_click$y)
    rv$caliper_points <- c(current_points, list(new_point))
  })
  
  output$caliper_output <- renderUI({ 
    points <- rv$caliper_points
    if (length(points) == 2) {
      p1 <- points[[1]]; p2 <- points[[2]]; dX <- abs(p2$x - p1$x); dY <- p2$y - p1$y
      HTML(sprintf("<b>dX (Time):</b> %.3f s (%.0f ms)<br/><b>dY (Amplitude):</b> %.2f mm", dX, dX*1000, dY))
    } else if (length(points) == 1) { HTML("First point selected. Click a second point.") }
    else { HTML("Click on ECG to place caliper points (max 2).") }
  })
  
  current_ecg_params <- reactive({
    req(input$simulation_mode, input$heart_rate)
    
    fs <- 250
    # Base HR is ventricular rate for NSR/Electrolytes/QT, atrial for AV blocks (can be overridden)
    hr_global <- input$heart_rate 
    
    # --- Base parameters (Normal Sinus Rhythm) ---
    base_p_start_time <- 0.04 
    base_p_dur <- 0.08; base_p_amp <- 1.5
    base_pr_interval <- 0.16 
    base_qrs_dur <- 0.08
    base_q_amp <- -1.0; base_r_amp <- 8.0; base_s_amp <- -2.0
    base_st_dur <- 0.10 
    base_t_dur <- 0.16; base_t_amp <- 2.0; base_t_width_factor <- 3.5
    
    # --- Initialize working parameters with base values ---
    p_start_time <- base_p_start_time; p_dur <- base_p_dur; p_amp <- base_p_amp
    pr_interval <- base_pr_interval; qrs_dur <- base_qrs_dur
    q_amp <- base_q_amp; r_amp <- base_r_amp; s_amp <- base_s_amp
    st_dur <- base_st_dur; t_dur <- base_t_dur; t_amp <- base_t_amp; t_width_factor <- base_t_width_factor
    t_inverted <- FALSE; u_wave_present <- FALSE; u_amp <- 0; base_u_dur <- 0.10; u_dur <- base_u_dur
    st_shift_mm <- 0 
    
    # Rhythm specific parameters
    is_chb <- FALSE
    atrial_rate_chb <- hr_global # Default atrial rate for CHB
    ventricular_rate_chb <- 40  # Default ventricular escape for CHB
    beat_duration_seconds <- 60 / hr_global
    
    # --- Apply Mode-Specific Logic ---
    mode <- input$simulation_mode
    
    if (mode == "electrolytes") {
      req(input$electrolyte, input$level)
      level <- input$level
      if (level == "k_hypo") { 
        t_amp <- base_t_amp * 0.4; t_inverted <- runif(1) < 0.2; st_shift_mm <- -0.75
        u_wave_present <- TRUE; u_amp <- 1.0 * base_t_amp 
        pr_interval <- base_pr_interval + 0.03; qrs_dur <- base_qrs_dur + 0.01
      } else if (level == "k_hyper") { 
        p_amp <- base_p_amp * 0.3; p_dur <- base_p_dur * 0.7
        pr_interval <- base_pr_interval + 0.08; qrs_dur <- base_qrs_dur + 0.06
        t_amp <- base_t_amp * 1.7; t_dur <- base_t_dur * 0.75; t_width_factor <- base_t_width_factor + 1.5
      } else if (level == "ca_hypo") { 
        st_dur <- base_st_dur + 0.08 
      } else if (level == "ca_hyper") { 
        st_dur <- clamp(base_st_dur - 0.06, 0.02, 0.15) 
      }
    } else if (mode == "av_blocks") {
      req(input$av_block_type)
      atrial_rate_chb <- input$atrial_rate_avb # Use specific atrial rate for AV blocks
      beat_duration_seconds <- 60 / atrial_rate_chb # Beat duration refers to atrial cycle
      
      if (input$av_block_type == "avb1") {
        pr_interval <- 0.24 # Example fixed prolonged PR
      } else if (input$av_block_type == "avb3") {
        is_chb <- TRUE
        ventricular_rate_chb <- input$ventricular_rate_avb3
        p_amp <- base_p_amp # Ensure P-waves are visible
        # QRS and T generated by escape rhythm, PR is irrelevant for conduction
      } else if (input$av_block_type == "avb2_mobitz1") {
        # Placeholder: Simulate as 1st degree block with slight PR prolongation for now
        pr_interval <- 0.22 
        # True Wenckebach needs dynamic PR in generated_ecg_trace
      } else if (input$av_block_type == "avb2_mobitz2") {
        # Placeholder: Simulate as NSR or 1st degree for now
        pr_interval <- base_pr_interval 
        # True Mobitz II needs dynamic QRS dropping in generated_ecg_trace
      }
    } else if (mode == "qt_syndromes") {
      req(input$qt_syndrome_type, input$qtc_target_ms)
      qtc_target_s <- input$qtc_target_ms / 1000
      # Bazett's correction: QT = QTc * sqrt(RR)
      rr_interval_s <- 60 / hr_global
      target_qt_s <- qtc_target_s * sqrt(rr_interval_s)
      
      # Current QT = qrs_dur + st_dur + t_dur
      current_qt_s <- qrs_dur + st_dur + t_dur
      qt_diff_s <- target_qt_s - current_qt_s
      
      # Distribute qt_diff_s primarily to t_dur and st_dur
      if (qt_diff_s > 0) { # Lengthen QT
        st_dur <- st_dur + qt_diff_s * 0.4
        t_dur <- t_dur + qt_diff_s * 0.6
      } else { # Shorten QT
        # Prioritize shortening ST segment, then T duration
        st_reduction <- min(st_dur - 0.01, abs(qt_diff_s) * 0.7) # leave at least 0.01s ST
        st_dur <- st_dur - st_reduction
        remaining_diff <- abs(qt_diff_s) - st_reduction
        if (remaining_diff > 0) {
          t_dur <- max(0.05, t_dur - remaining_diff) # T_dur at least 0.05s
        }
      }
      st_dur <- clamp(st_dur, 0.01, 0.30)
      t_dur <- clamp(t_dur, 0.05, 0.50)
      
      if (input$qt_syndrome_type == "lqts") {
        # Broader T-wave for LQTS approximation
        t_width_factor <- base_t_width_factor - 1.0 # Smaller factor = wider Gaussian
        # Potentially add more complex T-wave morphology here if enhancing generate_t_wave
      } else if (input$qt_syndrome_type == "sqts") {
        t_amp <- base_t_amp * 1.3 # Often tall, peaked T-waves
        t_width_factor <- base_t_width_factor + 1.0 # Sharper T-wave
        st_dur <- 0.01 # Minimal ST segment
      }
    }
    
    # --- Final calculations for wave onsets ---
    qrs_onset_calc <- p_start_time + pr_interval 
    if (qrs_onset_calc < p_start_time + p_dur && !is_chb) { # Ensure QRS doesn't overlap P if PR is too short (unless CHB)
      qrs_onset_calc <- p_start_time + p_dur + 0.01
    }
    t_onset_calc <- qrs_onset_calc + qrs_dur + st_dur 
    u_onset_calc <- NA
    if(u_wave_present) { u_onset_calc <- t_onset_calc + t_dur + 0.02 }
    
    final_qt_interval <- (t_onset_calc + t_dur) - qrs_onset_calc
    final_qtc <- if (beat_duration_seconds > 0 && final_qt_interval > 0 && !is_chb) final_qt_interval / sqrt(60 / hr_global) else NA # Use ventricular HR for QTc
    if(is_chb) final_qtc <- final_qt_interval / sqrt(60/ventricular_rate_chb)
    
    
    list(
      fs = fs, heart_rate_sim = hr_global, # Base rate used for non-CHB ventricular or general timing
      simulation_mode = mode,
      # Add specific type if available
      electrolyte_type = if(mode == "electrolytes") input$electrolyte else NA,
      level = if(mode == "electrolytes") input$level else NA,
      av_block_type = if(mode == "av_blocks") input$av_block_type else NA,
      qt_syndrome_type = if(mode == "qt_syndromes") input$qt_syndrome_type else NA,
      
      paper_speed = as.numeric(input$paper_speed), ecg_gain = as.numeric(input$ecg_gain),
      
      is_chb = is_chb,
      atrial_rate_chb = if(is_chb) atrial_rate_chb else hr_global, # For CHB, this is P-wave rate
      ventricular_rate_chb = if(is_chb) ventricular_rate_chb else hr_global, # For CHB, this is QRS rate
      
      # Beat duration is based on ventricular rate, or atrial if that's primary driver
      beat_duration_seconds = if(is_chb) NA else beat_duration_seconds, # Not well-defined for CHB as single cycle
      
      p_onset_time = p_start_time, p_duration = p_dur, p_amplitude = p_amp,
      pr_interval_val = pr_interval, 
      qrs_onset_time = qrs_onset_calc, qrs_duration = qrs_dur,
      q_amplitude = q_amp, r_amplitude = r_amp, s_amplitude = s_amp,
      st_segment_duration = st_dur, st_shift_actual_mv = st_shift_mm, 
      t_onset_time = t_onset_calc, t_duration = t_dur, t_amplitude = t_amp,
      t_inverted = t_inverted, t_wave_width_factor = t_width_factor, 
      u_wave_present = u_wave_present, u_onset_time = u_onset_calc, 
      u_duration = u_dur, u_amplitude = u_amp,
      
      qt_interval_calc = final_qt_interval, qtc_calc = final_qtc
    )
  })
  
  generated_ecg_trace <- reactive({ 
    params <- current_ecg_params(); req(params)
    
    total_duration_to_generate <- 5.0 * (25 / params$paper_speed)
    full_time_vector <- seq(0, total_duration_to_generate - (1/params$fs), by = 1/params$fs)
    final_ecg_signal <- numeric(length(full_time_vector))
    
    if (params$is_chb) {
      # --- 3rd Degree AV Block (CHB) Logic ---
      # P-waves (atrial rhythm)
      atrial_beat_duration <- 60 / params$atrial_rate_chb
      num_p_waves <- ceiling(total_duration_to_generate / atrial_beat_duration)
      for (k in 0:(num_p_waves - 1)) {
        p_abs_start_time <- k * atrial_beat_duration + params$p_onset_time # Use fixed p_onset_time for phasing
        single_p <- generate_p_wave_electrolyte(full_time_vector, p_abs_start_time, params$p_duration, params$p_amplitude, params$fs)
        single_p[full_time_vector < p_abs_start_time | full_time_vector > (p_abs_start_time + params$p_duration + 0.02)] <- 0
        final_ecg_signal <- final_ecg_signal + single_p
      }
      
      # QRS-T complexes (ventricular escape rhythm)
      ventricular_beat_duration <- 60 / params$ventricular_rate_chb
      num_qrs_complexes <- ceiling(total_duration_to_generate / ventricular_beat_duration)
      # Start ventricular rhythm at a slight offset for typical dissociation visual
      ventricular_offset_start <- runif(1, 0.1, ventricular_beat_duration * 0.5) 
      
      for (k in 0:(num_qrs_complexes - 1)) {
        qrs_abs_start_time <- k * ventricular_beat_duration + ventricular_offset_start
        t_abs_start_time <- qrs_abs_start_time + params$qrs_duration + params$st_segment_duration
        
        # Ensure QRS and T are within the plot time
        if(qrs_abs_start_time > total_duration_to_generate) break
        
        qrs_sgl <- generate_qrs_electrolyte(full_time_vector, qrs_abs_start_time, params$qrs_duration, params$q_amplitude, params$r_amplitude, params$s_amplitude, params$fs)
        t_sgl <- generate_t_wave_electrolyte(full_time_vector, t_abs_start_time, params$t_duration, params$t_amplitude, params$t_wave_width_factor, params$t_inverted)
        
        qrs_sgl[full_time_vector < qrs_abs_start_time | full_time_vector > (qrs_abs_start_time + params$qrs_duration + 0.02)] <- 0
        t_sgl[full_time_vector < t_abs_start_time | full_time_vector > (t_abs_start_time + params$t_duration + 0.02)] <- 0
        final_ecg_signal <- final_ecg_signal + qrs_sgl + t_sgl
        
        # ST shift for CHB (if any, usually not primary feature)
        if (params$st_shift_actual_mv != 0) {
          st_segment_start_abs <- qrs_abs_start_time + params$qrs_duration
          st_segment_end_abs <- t_abs_start_time 
          st_start_idx <- findInterval(st_segment_start_abs, full_time_vector, rightmost.closed = TRUE) + 1
          st_end_idx <- findInterval(st_segment_end_abs, full_time_vector, rightmost.closed = TRUE) 
          if (st_start_idx > 0 && st_end_idx >= st_start_idx && st_end_idx <= length(full_time_vector) && st_start_idx <= length(full_time_vector)) {
            final_ecg_signal[st_start_idx:st_end_idx] <- final_ecg_signal[st_start_idx:st_end_idx] + params$st_shift_actual_mv
          }
        }
      }
      
    } else {
      # --- Logic for regular rhythms (NSR, Electrolytes, 1st Deg AVB, QT Syndromes) ---
      # --- Placeholder for Mobitz I/II (currently renders as regular rhythm based on params) ---
      beat_duration_ventricular <- 60 / params$heart_rate_sim # Uses the general heart rate
      num_beats_to_generate <- ceiling(total_duration_to_generate / beat_duration_ventricular) + 2 
      
      for(i in 0:(num_beats_to_generate - 1)) {
        beat_offset_time <- i * beat_duration_ventricular
        current_p_onset = beat_offset_time + params$p_onset_time
        current_qrs_onset = beat_offset_time + params$qrs_onset_time # This already includes PR
        current_t_onset = beat_offset_time + params$t_onset_time
        current_u_onset = if(params$u_wave_present && !is.na(params$u_onset_time)) beat_offset_time + params$u_onset_time else NA
        
        p_wave_sgl <- generate_p_wave_electrolyte(full_time_vector, current_p_onset, params$p_duration, params$p_amplitude, params$fs)
        qrs_sgl <- generate_qrs_electrolyte(full_time_vector, current_qrs_onset, params$qrs_duration, params$q_amplitude, params$r_amplitude, params$s_amplitude, params$fs)
        t_wave_sgl <- generate_t_wave_electrolyte(full_time_vector, current_t_onset, params$t_duration, params$t_amplitude, params$t_wave_width_factor, params$t_inverted)
        u_wave_sgl <- if(params$u_wave_present && !is.na(current_u_onset)) generate_u_wave_electrolyte(full_time_vector, current_u_onset, params$u_duration, params$u_amplitude, params$fs) else numeric(length(full_time_vector))
        
        st_shift_component <- numeric(length(full_time_vector))
        if (params$st_shift_actual_mv != 0 && !is.na(current_qrs_onset) && !is.na(params$qrs_duration) && !is.na(current_t_onset)) {
          st_segment_start_abs <- current_qrs_onset + params$qrs_duration
          st_segment_end_abs <- current_t_onset 
          st_start_idx <- findInterval(st_segment_start_abs, full_time_vector, rightmost.closed = TRUE) + 1
          st_end_idx <- findInterval(st_segment_end_abs, full_time_vector, rightmost.closed = TRUE) 
          if (st_start_idx > 0 && st_end_idx >= st_start_idx && st_end_idx <= length(full_time_vector) && st_start_idx <= length(full_time_vector)) {
            st_shift_component[st_start_idx:st_end_idx] <- params$st_shift_actual_mv
          }
        }
        p_wave_sgl[full_time_vector < current_p_onset | full_time_vector > (current_p_onset + params$p_duration + 0.02)] <- 0
        qrs_sgl[full_time_vector < current_qrs_onset | full_time_vector > (current_qrs_onset + params$qrs_duration + 0.02)] <- 0
        t_wave_sgl[full_time_vector < current_t_onset | full_time_vector > (current_t_onset + params$t_duration + 0.02)] <- 0
        if(params$u_wave_present && !is.na(current_u_onset)) {
          u_wave_sgl[full_time_vector < current_u_onset | full_time_vector > (current_u_onset + params$u_duration + 0.02)] <- 0
        }
        final_ecg_signal <- final_ecg_signal + p_wave_sgl + qrs_sgl + t_wave_sgl + u_wave_sgl + st_shift_component
      }
    }
    final_ecg_signal <- final_ecg_signal * params$ecg_gain 
    data.frame(Time = full_time_vector, Value = final_ecg_signal)
  })
  
  output$plot_title_text <- reactive({
    params <- current_ecg_params(); req(params)
    title_prefix <- "ECG Simulation:"
    specific_title <- "Normal Sinus Rhythm"
    
    mode <- params$simulation_mode
    hr_display <- params$heart_rate_sim # Default HR to display
    
    if (mode == "electrolytes") {
      level_map_elec <- list(k_normal="Normal K+", k_hypo="Hypokalemia", k_hyper="Hyperkalemia",
                             ca_normal="Normal Ca++", ca_hypo="Hypocalcemia", ca_hyper="Hypercalcemia")
      elec_name <- tools::toTitleCase(params$electrolyte_type)
      level_desc <- level_map_elec[[params$level]]
      specific_title <- paste(elec_name, ":", level_desc)
    } else if (mode == "av_blocks") {
      hr_display <- params$atrial_rate_chb # For AV blocks, primary rate displayed is atrial
      level_map_avb <- list(avb1="1st Degree AV Block", avb2_mobitz1="2nd Degree AV Block, Mobitz I",
                            avb2_mobitz2="2nd Degree AV Block, Mobitz II", avb3="3rd Degree AV Block")
      specific_title <- level_map_avb[[params$av_block_type]]
      if (params$is_chb) {
        specific_title <- paste0(specific_title, " (Atrial: ", params$atrial_rate_chb, " bpm, Ventricular: ", params$ventricular_rate_chb, " bpm)")
        hr_display <- NULL # Avoid double printing HR
      }
    } else if (mode == "qt_syndromes") {
      level_map_qt <- list(lqts="Long QT Syndrome", sqts="Short QT Syndrome")
      specific_title <- level_map_qt[[params$qt_syndrome_type]]
    }
    
    final_title <- paste(title_prefix, specific_title)
    if (!is.null(hr_display) && !params$is_chb) { # Don't add bpm if CHB already detailed it or it's NSR default
      final_title <- paste0(final_title, " @ ", hr_display, " bpm")
    } else if (mode == "nsr"){
      final_title <- paste0(title_prefix, " Normal Sinus Rhythm @ ", hr_display, " bpm")
    }
    final_title
  })
  
  
  output$interval_info_display <- renderUI({ 
    params <- current_ecg_params(); req(params)
    
    # For CHB, some intervals are not applicable or need specific handling
    if(params$is_chb){
      pr_text <- "<b>PR Interval:</b> N/A (AV Dissociation)"
      qt_text <- sprintf("<b>Ventricular QT Interval:</b> %.0f ms", params$qt_interval_calc * 1000)
      qtc_text <- if(!is.na(params$qtc_calc)) sprintf("<b>Ventricular QTc:</b> %.0f ms (approx)", params$qtc_calc) else "<b>Ventricular QTc:</b> N/A"
      qrs_text <- sprintf("<b>Ventricular QRS Duration:</b> %.0f ms", params$qrs_duration * 1000)
      HTML(paste(
        sprintf("<b>Atrial Rate:</b> %.0f bpm", params$atrial_rate_chb),
        sprintf("<b>Ventricular Rate:</b> %.0f bpm", params$ventricular_rate_chb),
        pr_text, qrs_text, qt_text, qtc_text,
        sprintf("<b>ST Shift (simulated):</b> %.1f mm", params$st_shift_actual_mv * params$ecg_gain), 
        sep = "<br/>"))
    } else {
      pr_ms <- params$pr_interval_val * 1000; qrs_ms <- params$qrs_duration * 1000
      qt_ms <- params$qt_interval_calc * 1000
      qtc_ms <- if(!is.na(params$qtc_calc)) params$qtc_calc * 1000 else NA
      st_shift_display_mm <- params$st_shift_actual_mv * params$ecg_gain 
      HTML(paste(
        sprintf("<b>Heart Rate (Ventricular):</b> %.0f bpm", params$heart_rate_sim),
        sprintf("<b>PR Interval:</b> %.0f ms", pr_ms),
        sprintf("<b>QRS Duration:</b> %.0f ms", qrs_ms),
        sprintf("<b>QT Interval:</b> %.0f ms", qt_ms),
        if(!is.na(qtc_ms)) sprintf("<b>QTc (Bazett):</b> %.0f ms (approx)", qtc_ms) else "<b>QTc (Bazett):</b> N/A",
        sprintf("<b>ST Shift (simulated):</b> %.1f mm", st_shift_display_mm), 
        sep = "<br/>"))
    }
  })
  
  output$ecg_plot <- renderPlot({ 
    req(input$simulation_mode, input$heart_rate, input$paper_speed, input$ecg_gain)
    if(input$simulation_mode == "electrolytes") req(input$electrolyte, input$level)
    if(input$simulation_mode == "av_blocks") req(input$av_block_type)
    if(input$simulation_mode == "qt_syndromes") req(input$qt_syndrome_type)
    
    df_display <- generated_ecg_trace()
    # ... (rest of plotting code remains largely the same, ensure plot_min_y/max_y are robust) ...
    if (is.null(df_display) || nrow(df_display) == 0 || all(is.na(df_display$Value))) {
      return(ggplot() + theme_void() + annotate("text", x=0.5, y=0.5, label="ECG data error or not applicable for current view.", size=5))
    }
    x_max_plot <- max(df_display$Time, na.rm=TRUE)
    if (is.infinite(x_max_plot) || is.na(x_max_plot) || x_max_plot <=0) x_max_plot <- 5 
    min_val_data <- min(df_display$Value, na.rm = TRUE); max_val_data <- max(df_display$Value, na.rm = TRUE)
    
    plot_min_y <- floor(min(min_val_data, -5, na.rm=TRUE) / 5) * 5 - 2
    plot_max_y <- ceiling(max(max_val_data, 10, na.rm=TRUE) / 5) * 5 + 2
    
    if (is.infinite(plot_min_y) || is.na(plot_min_y) || plot_min_y >= plot_max_y) {
      plot_min_y <- -10; plot_max_y <- 15
    }
    
    p <- ggplot(df_display, aes(x = Time, y = Value)) +
      geom_hline(yintercept = seq(floor(plot_min_y), ceiling(plot_max_y), by = 1), color = "pink", linetype = "dotted", linewidth = 0.4) + 
      geom_vline(xintercept = seq(0, x_max_plot, by = 0.04), color = "pink", linetype = "dotted", linewidth = 0.4) + 
      geom_hline(yintercept = seq(floor(plot_min_y/5)*5, ceiling(plot_max_y/5)*5, by = 5), color = "lightcoral", linetype = "dashed", linewidth = 0.6) + 
      geom_vline(xintercept = seq(0, x_max_plot, by = 0.20), color = "lightcoral", linetype = "dashed", linewidth = 0.6) + 
      geom_line(color = "black", linewidth = 1.0, na.rm = TRUE) + 
      labs(x = "Time (seconds)", y = paste0("Amplitude (mm) at ", input$ecg_gain, "x Gain"), title = NULL) +
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.line = element_line(color = "black"), axis.title = element_text(size = 12)) +
      coord_cartesian(ylim = c(plot_min_y, plot_max_y), xlim = c(0, x_max_plot), expand = FALSE) +
      scale_x_continuous(breaks = seq(0, floor(x_max_plot / 0.2) * 0.2 , by = 0.20), 
                         minor_breaks = seq(0, floor(x_max_plot / 0.04) * 0.04, by = 0.04)) +
      scale_y_continuous(breaks = seq(floor(plot_min_y/5)*5, ceiling(plot_max_y/5)*5, by = 5), 
                         minor_breaks = seq(floor(plot_min_y), ceiling(plot_max_y), by = 1))
    cal_points_data <- rv$caliper_points 
    if (length(cal_points_data) > 0) {
      df_cal_points <- do.call(rbind, lapply(cal_points_data, function(pt) data.frame(x=pt$x, y=pt$y)))
      p <- p + geom_point(data = df_cal_points, aes(x=x, y=y), color="blue", size=3, shape=4, stroke=1.5)
      if (length(cal_points_data) == 2) {
        p <- p + geom_segment(data=df_cal_points, aes(x=x[1], y=y[1], xend=x[2], yend=y[2]), color="blue", linewidth=1, linetype="dashed")
      }
    }
    print(p)
  })
  
  output$educational_notes_display <- renderUI({ 
    params <- current_ecg_params(); req(params)
    header_style <- "font-size: 1.1em; font-weight: bold; margin-bottom: 5px; color: #007bff;"
    list_style <- "margin-left: 20px; margin-bottom: 10px;"
    notes <- ""
    
    mode <- params$simulation_mode
    
    if (mode == "nsr") {
      notes <- paste0("<div style='", header_style, "'>Normal Sinus Rhythm:</div>",
                      "<ul style='", list_style, "'>",
                      "<li><b>P wave:</b> Upright, precedes each QRS. Duration <0.12s, Amplitude <2.5mm.</li>",
                      "<li><b>PR interval:</b> 0.12-0.20s.</li>",
                      "<li><b>QRS complex:</b> Duration 0.06-0.10s.</li>",
                      "<li><b>ST segment:</b> Isoelectric.</li>",
                      "<li><b>T wave:</b> Upright, concordant with QRS.</li>",
                      "<li><b>QT interval:</b> Rate-dependent (QTc normally <440-460ms).</li></ul>")
    } else if (mode == "electrolytes") {
      level_key <- params$level
      if (endsWith(level_key, "_normal")) {
        notes <- paste0("<div style='", header_style, "'>Normal Electrolyte Level (Simulated):</div>",
                        "<ul style='", list_style, "'><li>ECG parameters are at baseline normal values for the selected electrolyte.</li></ul>")
      } else if (level_key == "k_hypo") {
        notes <- paste0("<div style='", header_style, "'>Hypokalemia (Simulated):</div>",
                        "<ul style='", list_style, "'>",
                        "<li>T-wave flattening or inversion.</li><li>ST segment depression.</li>",
                        "<li>Prominent U-waves.</li><li>Apparent QT prolongation (often Q-U interval).</li>",
                        "<li>PR interval prolongation.</li><li>QRS widening (less common).</li><li>Increased risk of arrhythmias.</li></ul>")
      } else if (level_key == "k_hyper") {
        notes <- paste0("<div style='", header_style, "'>Hyperkalemia (Simulated):</div>",
                        "<ul style='", list_style, "'>",
                        "<li>Tall, peaked ('tented') T-waves (early).</li><li>PR interval prolongation.</li>",
                        "<li>P-wave flattening or disappearance.</li><li>QRS complex widening.</li>",
                        "<li>Severe: Risk of sine wave pattern, VF/asystole.</li></ul>")
      } else if (level_key == "ca_hypo") {
        notes <- paste0("<div style='", header_style, "'>Hypocalcemia (Simulated):</div>",
                        "<ul style='", list_style, "'><li>Prolongation of the QT interval (mainly due to ST segment lengthening).</li>",
                        "<li>T-wave duration usually unaffected.</li><li>Risk of Torsades de Pointes.</li></ul>")
      } else if (level_key == "ca_hyper") {
        notes <- paste0("<div style='", header_style, "'>Hypercalcemia (Simulated):</div>",
                        "<ul style='", list_style, "'><li>Shortening of the QT interval (mainly due to ST segment shortening).</li><li>PR prolongation or QRS widening (severe cases).</li></ul>")
      }
    } else if (mode == "av_blocks") {
      avb_type <- params$av_block_type
      if (avb_type == "avb1"){
        notes <- paste0("<div style='", header_style, "'>1st Degree AV Block (Simulated):</div>",
                        "<ul style='", list_style, "'><li>PR interval is prolonged (>0.20 seconds).</li>",
                        "<li>Every P wave is followed by a QRS complex.</li><li>Represents a delay in AV conduction.</li></ul>")
      } else if (avb_type == "avb2_mobitz1"){
        notes <- paste0("<div style='", header_style, "'>2nd Degree AV Block, Mobitz I (Wenckebach):</div>",
                        "<ul style='", list_style, "'><li>Characterized by progressive lengthening of the PR interval until a P wave is not conducted (dropped QRS).</li>",
                        "<li>The P-P interval is regular. The R-R interval shortens progressively.</li><li>The block is usually located in the AV node.</li>",
                        "<li><strong style='color:red;'>Note: Dynamic ECG pattern not fully simulated in plot yet.</strong></li></ul>")
      } else if (avb_type == "avb2_mobitz2"){
        notes <- paste0("<div style='", header_style, "'>2nd Degree AV Block, Mobitz II:</div>",
                        "<ul style='", list_style, "'><li>Characterized by intermittently non-conducted P waves (dropped QRS) without prior PR prolongation.</li>",
                        "<li>The PR interval of conducted beats is constant.</li><li>The block is usually located below the AV node (His-Purkinje system).</li>",
                        "<li>Carries a higher risk of progression to 3rd degree AV block.</li>",
                        "<li><strong style='color:red;'>Note: Dynamic ECG pattern not fully simulated in plot yet.</strong></li></ul>")
      } else if (avb_type == "avb3"){
        notes <- paste0("<div style='", header_style, "'>3rd Degree AV Block (Complete Heart Block) (Simulated):</div>",
                        "<ul style='", list_style, "'><li>Complete absence of AV conduction. P waves and QRS complexes are independent (AV dissociation).</li>",
                        "<li>Atrial rate (P waves) is usually faster than the ventricular rate (QRS complexes).</li>",
                        "<li>Ventricular rhythm is maintained by a junctional or ventricular escape pacemaker.</li><li>QRS width depends on the origin of the escape rhythm.</li></ul>")
      }
    } else if (mode == "qt_syndromes"){
      qt_type <- params$qt_syndrome_type
      if (qt_type == "lqts"){
        notes <- paste0("<div style='", header_style, "'>Long QT Syndrome (LQTS) (Simulated):</div>",
                        "<ul style='", list_style, "'><li>Characterized by a pathologically prolonged QT interval (corrected for heart rate - QTc).</li>",
                        "<li>Predisposes to polymorphic ventricular tachycardia (Torsades de Pointes) and sudden cardiac death.</li>",
                        "<li>Can be congenital (genetic mutations in ion channels) or acquired (e.g., drugs, electrolyte imbalances).</li>",
                        "<li>T-wave morphology can be abnormal (e.g., broad, notched, biphasic - simplified in this simulation).</li></ul>")
      } else if (qt_type == "sqts"){
        notes <- paste0("<div style='", header_style, "'>Short QT Syndrome (SQTS) (Simulated):</div>",
                        "<ul style='", list_style, "'><li>Characterized by an abnormally short QT interval (corrected - QTc).</li>",
                        "<li>Associated with an increased risk of atrial and ventricular fibrillation, and sudden cardiac death.</li>",
                        "<li>Rare inherited channelopathy, typically due to gain-of-function mutations in potassium channels.</li>",
                        "<li>ST segment is often minimal or absent. T-waves may be tall and peaked.</li></ul>")
      }
    }
    HTML(notes)
  })
  
  # --- Vignette Server Logic ---
  observeEvent(input$selected_vignette, {
    if (input$selected_vignette != "") {
      rv$current_vignette_id <- input$selected_vignette
      rv$show_vignette_answer_flag <- FALSE 
      
      # Attempt to set simulation mode based on vignette
      selected_v_data <- Filter(function(v) v$id == rv$current_vignette_id, vignettes_list)[[1]]
      vignette_condition <- selected_v_data$answer_electrolyte # Reused field
      
      if (vignette_condition == "Hypokalemia") {
        updateSelectInput(session, "simulation_mode", selected = "electrolytes")
        updateSelectInput(session, "electrolyte", selected = "potassium")
        # Delay level update slightly to ensure electrolyte dropdown is processed
        shinyjs::delay(100, updateSelectInput(session, "level", selected = "k_hypo"))
      } else if (vignette_condition == "Hyperkalemia") {
        updateSelectInput(session, "simulation_mode", selected = "electrolytes")
        updateSelectInput(session, "electrolyte", selected = "potassium")
        shinyjs::delay(100, updateSelectInput(session, "level", selected = "k_hyper"))
      } else if (vignette_condition == "Hypocalcemia" || endsWith(vignette_condition, "(hypocalcemia).")) {
        updateSelectInput(session, "simulation_mode", selected = "electrolytes")
        updateSelectInput(session, "electrolyte", selected = "calcium")
        shinyjs::delay(100, updateSelectInput(session, "level", selected = "ca_hypo"))
      } else if (vignette_condition == "Hypercalcemia" || endsWith(vignette_condition, "(hypercalcemia).")) {
        updateSelectInput(session, "simulation_mode", selected = "electrolytes")
        updateSelectInput(session, "electrolyte", selected = "calcium")
        shinyjs::delay(100, updateSelectInput(session, "level", selected = "ca_hyper"))
      } else if (vignette_condition == "1st Degree AV Block" || vignette_condition == "1st Degree AV Block (physiologic).") {
        updateSelectInput(session, "simulation_mode", selected = "av_blocks")
        shinyjs::delay(100, updateSelectInput(session, "av_block_type", selected = "avb1"))
      } else if (vignette_condition == "2nd Degree AV Block, Mobitz I.") {
        updateSelectInput(session, "simulation_mode", selected = "av_blocks")
        shinyjs::delay(100, updateSelectInput(session, "av_block_type", selected = "avb2_mobitz1"))
      } else if (vignette_condition == "2nd Degree AV Block, Mobitz II.") {
        updateSelectInput(session, "simulation_mode", selected = "av_blocks")
        shinyjs::delay(100, updateSelectInput(session, "av_block_type", selected = "avb2_mobitz2"))
      } else if (vignette_condition == "3rd Degree AV Block.") {
        updateSelectInput(session, "simulation_mode", selected = "av_blocks")
        shinyjs::delay(100, updateSelectInput(session, "av_block_type", selected = "avb3"))
      } else if (startsWith(vignette_condition, "Long QT Syndrome")) {
        updateSelectInput(session, "simulation_mode", selected = "qt_syndromes")
        shinyjs::delay(100, updateSelectInput(session, "qt_syndrome_type", selected = "lqts"))
      } else if (startsWith(vignette_condition, "Short QT Syndrome")) {
        updateSelectInput(session, "simulation_mode", selected = "qt_syndromes")
        shinyjs::delay(100, updateSelectInput(session, "qt_syndrome_type", selected = "sqts"))
      }
      
      
    } else {
      rv$current_vignette_id <- NULL
      rv$show_vignette_answer_flag <- FALSE
    }
  })
  
  observeEvent(input$show_vignette_answer, {
    rv$show_vignette_answer_flag <- TRUE
  })
  
  output$vignette_scenario_question_display <- renderUI({
    req(rv$current_vignette_id)
    vignette <- Filter(function(v) v$id == rv$current_vignette_id, vignettes_list)[[1]]
    tagList(
      tags$h5(strong(vignette$title)),
      tags$p(vignette$scenario),
      tags$p(tags$em(vignette$question))
    )
  })
  
  output$vignette_answer_explanation_display <- renderUI({
    if (!rv$show_vignette_answer_flag || is.null(rv$current_vignette_id)) return(NULL)
    req(rv$current_vignette_id)
    vignette <- Filter(function(v) v$id == rv$current_vignette_id, vignettes_list)[[1]]
    
    wellPanel(
      style = "margin-top: 10px; background-color: #f9f9f9; border-left: 5px solid #007bff;",
      tags$h6(strong("Likely Diagnosis/Condition:")),
      tags$p(vignette$answer_electrolyte), 
      tags$h6(strong("Expected ECG Clues:")),
      tags$p(vignette$answer_ecg),
      tags$h6(strong("Explanation:")),
      tags$p(vignette$explanation)
    )
  })
  
  # Need to use shinyjs for the delayed updates
  shinyjs::useShinyjs()
  
}
# --- Run the application ---
shinyApp(ui, server)
