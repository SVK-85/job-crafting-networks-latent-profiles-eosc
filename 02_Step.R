#-=-=-=-=-=-=ì-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
#                              ┏┓┏┳┓┏┓┏┓  ┏┓                                   #
#                              ┗┓ ┃ ┣ ┃┃  ┏┛                                   #
#                              ┗┛ ┻ ┗┛┣┛  ┗━                                   #
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
#          Variable Labels, Analysis Dataset, and Bilingual Codebook           #
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 2.1 Construct the exact dataset headers from short instrument prefixes
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 2.1.1 Define the full instrument names
instrument_names <- c("VAR4" = "Job Crafting Scale (JCQ)",
                      "VAR5" = "Job Crafting Questionnaire (JCS)", 
                      "VAR6" = "Work and Meaning Inventory WAMI)", 
                      "VAR7" = "Utrecht Work Engagement Scale (UWES-9)")

# 2.1.2 Define the full instrument abbreviations
instrument_abbreviations <- c("VAR4" = "JCS", 
                              "VAR5" = "JCQ", 
                              "VAR6" = "WAMI",
                              "VAR7" = "UWES-9")

# 2.1.3 Define the short instrument prefixes for the dataset headers
instrument_variable_prefixes <- c("VAR4" = "jcs", 
                                  "VAR5" = "jcq", 
                                  "VAR6" = "wami", 
                                  "VAR7" = "uwes")

# 2.1.4 Initialize the ordered variable registry
variable_registry <- list()

# 2.1.5 Register ID: participant identifier
variable_registry[["ID"]] <- list(variable_labels = c(ID = "ID"), 
value_labels = NULL)

# 2.1.6 Register VAR1: age in years
variable_registry[["VAR1"]] <- list(variable_labels = c(VAR1 = "Age"),
value_labels = NULL)

# 2.1.7 Register VAR2: gender and original response codes
variable_registry[["VAR2"]] <- list(variable_labels = c(VAR2 = "Gender"),
value_labels = c("Male" = 0, "Female" = 1,
                 "I do not identify with any of these options" = 2))

# 2.1.8 Register VAR3: educational level and original response codes
variable_registry[["VAR3"]] <- list(
variable_labels = c(VAR3 = "Educational_level"),
value_labels = c("Incomplete primary education" = 0,
                 "Primary education (EGB or equivalent)" = 1,
           "Secondary education (ESO or intermediate vocational training)" = 2,
                 "High school or advanced vocational training" = 3,
                 "University degree (Diploma, Bachelor's or equivalent)" = 4,
                 "Postgraduate university degree (Master's, Doctorate)" = 5))
# Note: EGB = Educación General Básica (Basic General Education).
#       ESO = Educación Secundaria Obligatoria.

# 2.1.9 Register VAR4.1-VAR4.21: JCS items, allowed values, and endpoints
variable_registry[["VAR4"]] <- list(instrument_name = instrument_names[["VAR4"]],
                    instrument_abbreviation = instrument_abbreviations[["VAR4"]],
                    variable_labels = stats::setNames(
     paste0(instrument_variable_prefixes[["VAR4"]], "_", sprintf("%02d", 1:21)),
     paste0("VAR4.", 1:21)), allowed_values = 1:7, 
                                    value_labels = c("Never" = 1, "Always" = 7))
# Note: Original version: Tims, Bakker, & Derks (2012).
#       doi: 10.1016/j.jvb.2011.05.009
# Note: Spanish version: Bakker et al. (2018).
#       doi: 10.7334/psicothema2016.293

# 2.1.10 Register VAR5.1-VAR5.15: JCQ items, allowed values, and endpoints
variable_registry[["VAR5"]] <- list(instrument_name = instrument_names[["VAR5"]],
                   instrument_abbreviation = instrument_abbreviations[["VAR5"]],
                   variable_labels = stats::setNames(
     paste0(instrument_variable_prefixes[["VAR5"]], "_", sprintf("%02d", 1:15)),
     paste0("VAR5.", 1:15)), allowed_values = 1:6,
                  value_labels = c("Hardly ever" = 1, "Very often" = 6))
# Note: Original version: Slemp & Vella-Brodrick (2013).
#       doi: 10.5502/ijw.v3i2.1
# Note: Spanish version: Letona-Ibanez et al. (2019).
#       doi: 10.1371/journal.pone.0223539

# 2.1.11 Register VAR6.1-VAR6.10: WAMI items, allowed values, and endpoints
variable_registry[["VAR6"]] <- list(instrument_name = instrument_names[["VAR6"]],
                   instrument_abbreviation = instrument_abbreviations[["VAR6"]],
                   variable_labels = stats::setNames(
     paste0(instrument_variable_prefixes[["VAR6"]], "_", sprintf("%02d", 1:10)),
     paste0("VAR6.", 1:10)), allowed_values = 1:5, 
         value_labels = c("Not at all true of me" = 1,"Totally true of me" = 5))

# Note: Original English version: Steger, Dik, & Duffy (2012).
#       doi: 10.1177/1069072711436160
# Note: Spanish-language use reported by Letona-Ibanez et al. (2021).
#       doi: 10.3390/ijerph18105383

# 2.1.12 Register VAR7.1-VAR7.9: UWES-9 items, allowed values, and endpoints
variable_registry[["VAR7"]] <- list(instrument_name = instrument_names[["VAR7"]],
                   instrument_abbreviation = instrument_abbreviations[["VAR7"]],
                   variable_labels = stats::setNames(
      paste0(instrument_variable_prefixes[["VAR7"]], "_", sprintf("%02d", 1:9)),
      paste0("VAR7.", 1:9)), allowed_values = 0:6,
                        value_labels = c("Never" = 0, "Always (every day)" = 6))
# Note: UWES-9 development and international validation:
#       Schaufeli, Bakker, & Salanova (2006)
#       doi: 10.1177/0013164405282471

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 2.2 Register the English and Spanish item wording
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 2.2.1 Register the English JCS item wording
# Source: Tims et al. (2012)
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
variable_registry[["VAR4"]]$item_wording_en <- c(
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_01 = VAR4.1
VAR4.1 = "I try to develop my capabilities",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_02 = VAR4.2
VAR4.2 = "I try to develop myself professionally",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_03 = VAR4.3
VAR4.3 = "I try to learn new things at work",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_04 = VAR4.4
VAR4.4 = "I make sure that I use my capacities to the fullest",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_05 = VAR4.5
VAR4.5 = "I decide on my own how I do things",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_06 = VAR4.6
VAR4.6 = "I make sure that my work is mentally less intense",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_07 = VAR4.7
VAR4.7 = "I try to ensure that my work is emotionally less intense",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_08 = VAR4.8
VAR4.8 = paste0("I manage my work so that I try to minimize contact with ",
"people whose problems affect me emotionally"),
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_09 = VAR4.9
VAR4.9 = paste0("I organize my work so as to minimize contact with people ",
"whose expectations are unrealistic"),
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_10 = VAR4.10
VAR4.10 = paste0("I try to ensure that I do not have to make many difficult ",
"decisions at work"),
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_11 = VAR4.11
VAR4.11 = paste0("I organize my work in such a way to make sure that I do not ",
"have to concentrate for too long a period at once"),
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_12 = VAR4.12
VAR4.12 = "I ask my supervisor to coach me",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_13 = VAR4.13
  VAR4.13 = "I ask whether my supervisor is satisfied with my work",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_14 = VAR4.14
VAR4.14 = "I look to my supervisor for inspiration",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_15 = VAR4.15
VAR4.15 = "I ask others for feedback on my job performance",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_16 = VAR4.16
VAR4.16 = "I ask colleagues for advice",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_17 = VAR4.17
VAR4.17 = paste0("When an interesting project comes along, I offer myself ",
"proactively as project co-worker"),
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_18 = VAR4.18
VAR4.18 = paste0("If there are new developments, I am one of the first to ",
"learn about them and try them out"),
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_19 = VAR4.19
VAR4.19 = paste0("When there is not much to do at work, I see it as a chance ",
"to start new projects"),
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_20 = VAR4.20
VAR4.20 = paste0("I regularly take on extra tasks even though I do not ",
"receive extra salary for them"),
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_21 = VAR4.21
VAR4.21 = paste0("I try to make my work more challenging by examining the ",
 "underlying relationships between aspects of my job"))
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 2.2.2 Register the Spanish JCS item wording
# Source: Bakker et al. (2018)
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
variable_registry[["VAR4"]]$item_wording_es <- c(
# jcs_01 = VAR4.1
VAR4.1 = "Yo trato de desarrollar mis capacidades",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_02 = VAR4.2
VAR4.2 = "Yo trato de desarrollarme profesionalmente",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_03 = VAR4.3
VAR4.3 = "Yo trato de aprender cosas nuevas en el trabajo",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_04 = VAR4.4
VAR4.4 = "Yo me aseguro de que puedo utilizar mis capacidades al máximo",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_05 = VAR4.5
VAR4.5 = "Yo decido por mí mismo cómo hacer las cosas",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_06 = VAR4.6
VAR4.6 = "Yo me cercioro de que mi trabajo sea mentalmente menos intenso",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_07 = VAR4.7
VAR4.7 = paste0("Yo trato de asegurarme de que mi trabajo sea emocionalmente ",
"menos intenso"),
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_08 = VAR4.8
VAR4.8 = paste0("Yo puedo administrar mi trabajo, así que trato de minimizar ",
"el contacto con personas cuyos problemas me afectan emocionalmente"),
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_09 = VAR4.9
VAR4.9 = paste0("Yo organizo mi trabajo con el fin de minimizar el contacto ",
"con las personas cuyas expectativas no son realistas"),
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_10 = VAR4.10
VAR4.10 = paste0("Yo trato de asegurarme de que no tengo que tomar decisiones ",
"difíciles en el trabajo."),
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_11 = VAR4.11
VAR4.11 = paste0("Yo organizo mi trabajo de tal manera que me aseguro que no ",
"tengo que concentrarme durante un período demasiado largo."),
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_12 = VAR4.12
VAR4.12 = "Yo le pido a mi supervisor que me haga de coach",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_13 = VAR4.13
VAR4.13 = "Yo me pregunto si mi supervisor está satisfecho con mi trabajo",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_14 = VAR4.14
VAR4.14 = "Yo miro a mi supervisor para tener inspiración",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_15 = VAR4.15
VAR4.15 = paste0("Yo pido a los demás que me den feedback sobre mi desempeño ",
"en el trabajo"),
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_16 = VAR4.16
VAR4.16 = "Yo pido consejos a los colegas",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_17 = VAR4.17
VAR4.17 = paste0("Cuando aparece un proyecto interesante, yo me ofrezco de ",
"manera proactiva a los compañeros de trabajo para trabajar en él"),
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_18 = VAR4.18
VAR4.18 = paste0("Si hay nuevos desarrollos, yo soy uno de los primeros en ",
"aprender acerca de ellos y probarlos"),
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_19 = VAR4.19
VAR4.19 = paste0("Cuando no hay mucho que hacer en el trabajo, yo lo veo como ",
"una oportunidad para iniciar nuevos proyectos"),
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_20 = VAR4.20
VAR4.20 = paste0("Regularmente yo realizo tareas adicionales a pesar de que ",
"no recibo salario extra por ellas."),
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcs_21 = VAR4.21
VAR4.21 = paste0("Yo trato de hacer el trabajo más difícil para examinar las ",
"relaciones subyacentes entre los distintos aspectos de mi trabajo"))
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 2.2.3 Register the English JCQ item wording in Spanish item order
# Source: Slemp & Vella-Brodrick (2013)
# VAR5.12 corresponds to original English item 13; VAR5.13 to item 12.
variable_registry[["VAR5"]]$item_wording_en <- c(
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcq_01 = VAR5.1
VAR5.1 = "Introduce new approaches to improve your work",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcq_02 = VAR5.2
VAR5.2 = "Change the scope or types of tasks that you complete at work",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcq_03 = VAR5.3
VAR5.3 = paste0("Introduce new work tasks that you think better suit your ",
"skills or interests"),
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcq_04 = VAR5.4
VAR5.4 = "Choose to take on additional tasks at work",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcq_05 = VAR5.5
VAR5.5 = "Give preference to work tasks that suit your skills or interests",
# jcq_06 = VAR5.6
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
VAR5.6 = "Think about how your job gives your life purpose",
# jcq_07 = VAR5.7
VAR5.7 = paste0("Remind yourself about the significance your work has for the ",
"success of the organisation"),
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcq_08 = VAR5.8
VAR5.8 = paste0("Remind yourself of the importance of your work for the ",
"broader community"),
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcq_09 = VAR5.9
VAR5.9 = "Think about the ways in which your work positively impacts your life",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcq_10 = VAR5.10
VAR5.10 = "Reflect on the role your job has for your overall well-being",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcq_11 = VAR5.11
VAR5.11 = "Make an effort to get to know people well at work",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcq_12 = VAR5.12
VAR5.12 = paste0("Organise special events in the workplace (e.g., celebrating ",
"a co-worker's birthday)"),
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcq_13 = VAR5.13
  VAR5.13 = "Organise or attend work related social functions",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcq_14 = VAR5.14
VAR5.14 = "Choose to mentor new employees (officially or unofficially)",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcq_15 = VAR5.15
VAR5.15 = paste0("Make friends with people at work who have similar skills or ",
"interests"))
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 2.2.4 Register the Spanish JCQ item wording
# Source: Letona-Ibañez et al. (2019)
variable_registry[["VAR5"]]$item_wording_es <- c(
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcq_01 = VAR5.1
VAR5.1 = "Incorporar nuevos enfoques para mejorar tu trabajo",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcq_02 = VAR5.2
VAR5.2 = "Cambiar el alcance o tipo de tareas que llevas a cabo en tu trabajo",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcq_03 = VAR5.3
VAR5.3 = paste0("Incorporar en tu trabajo nuevas tareas que crees que se ",
"ajustan mejor a tus habilidades o intereses"),
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcq_04 = VAR5.4
VAR5.4 = "Tomar la decisión de asumir tareas adicionales en el trabajo",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcq_05 = VAR5.5
VAR5.5 = "Dar prioridad a tareas que se ajustan a tus habilidades o intereses",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcq_06 = VAR5.6
  VAR5.6 = "Reflexionar sobre cómo el trabajo da sentido a tu vida",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcq_07 = VAR5.7
VAR5.7 = paste0("Recordarte a ti mismo/a el valor que tiene tu trabajo para ",
"el éxito de la organización"),
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcq_08 = VAR5.8
VAR5.8 = paste0("Recordarte a ti mismo/a la importancia de tu trabajo para la ",
"comunidad"),
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcq_09 = VAR5.9
VAR5.9 = paste0("Identificar las maneras en que tu trabajo influye ",
"positivamente en tu vida"),
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcq_10 = VAR5.10
VAR5.10 = "Valorar el papel que tiene tu trabajo en tu bienestar general",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcq_11 = VAR5.11
VAR5.11 = "Esforzarte por conocer bien a las personas en el trabajo",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcq_12 = VAR5.12
VAR5.12 = paste0("Organizar actividades sociales en el trabajo (por ejemplo, ",
"celebrar el cumpleaños de un compañero/a)"),
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcq_13 = VAR5.13
VAR5.13 = paste0("Organizar o asistir a eventos sociales directamente ",
"relacionados con el trabajo"),
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcq_14 = VAR5.14
VAR5.14 = paste0("Tomar la decisión de actuar como mentor de las nuevas ",
"incorporaciones (ya sea de manera oficial o no)"),
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# jcq_15 = VAR5.15
VAR5.15 = paste0("Entablar amistad con las personas del trabajo que tengan ",
"habilidades o intereses similares a los tuyos"))
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 2.2.5 Register the original English WAMI item wording
# Source: Steger et al. (2012)
variable_registry[["VAR6"]]$item_wording_en <- c(
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# wami_01 = VAR6.1
VAR6.1 = "I have found a meaningful career.",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# wami_02 = VAR6.2
VAR6.2 = "I view my work as contributing to my personal growth",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# wami_03 = VAR6.3
VAR6.3 = "My work really makes no difference to the world.",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# wami_04 = VAR6.4
VAR6.4 = "I understand how my work contributes to my life’s meaning",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# wami_05 = VAR6.5
VAR6.5 = "I have a good sense of what makes my job meaningful.",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# wami_06 = VAR6.6
VAR6.6 = "I know my work makes a positive difference in the world.",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# wami_07 = VAR6.7
VAR6.7 = "My work helps me better understand myself.",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# wami_08 = VAR6.8
VAR6.8 = "I have discovered work that has a satisfying purpose.",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# wami_09 = VAR6.9
VAR6.9 = "My work helps me make sense of the world around me.",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# wami_10 = VAR6.10
VAR6.10 = "The work I do serves a greater purpose.")
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 2.2.6 Register the Spanish WAMI item wording
# Source: Carrasco et al. (2026), Supplementary Material 1.
variable_registry[["VAR6"]]$item_wording_es <- c(
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# wami_01 = VAR6.1
VAR6.1 = "He encontrado una profesión que me llena",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# wami_02 = VAR6.2
VAR6.2 = "Considero que mi trabajo contribuye a mi desarrollo personal",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# wami_03 = VAR6.3
VAR6.3 = "En realidad, mi trabajo no marca una diferencia para la sociedad",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# wami_04 = VAR6.4
VAR6.4 = "Me doy cuenta de cómo mi trabajo contribuye a dar sentido a mi vida",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# wami_05 = VAR6.5
VAR6.5 = paste0("Tengo una idea bastante clara de qué es lo que hace que mi ",
"trabajo tenga sentido para mi"),
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# wami_06 = VAR6.6
VAR6.6 = "Sé que mi trabajo aporta algo positivo a la sociedad",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# wami_07 = VAR6.7
VAR6.7 = "Mi trabajo me ayuda a conocerme mejor",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# wami_08 = VAR6.8
VAR6.8 = "He descubierto que el trabajo puede producir satisfacción",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# wami_09 = VAR6.9
VAR6.9 = "Mi trabajo me ayuda a dar sentido al mundo que me rodea",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# wami_10 = VAR6.10
VAR6.10 = "Mi trabajo sirve a un propósito mayor")
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 2.2.7 Register the English UWES-9 item wording
# Source: Schaufeli & Bakker (2004)
variable_registry[["VAR7"]]$item_wording_en <- c(
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# uwes_01 = VAR7.1
VAR7.1 = "At my work, I feel bursting with energy",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# uwes_02 = VAR7.2
VAR7.2 = "At my job, I feel strong and vigorous",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# uwes_03 = VAR7.3
VAR7.3 = "I am enthusiastic about my job",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# uwes_04 = VAR7.4
VAR7.4 = "My job inspires me",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# uwes_05 = VAR7.5
VAR7.5 = "When I get up in the morning, I feel like going to work",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# uwes_06 = VAR7.6
VAR7.6 = "I feel happy when I am working intensely",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# uwes_07 = VAR7.7
VAR7.7 = "I am proud on the work that I do",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# uwes_08 = VAR7.8
VAR7.8 = "I am immersed in my work",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# uwes_09 = VAR7.9
VAR7.9 = "I get carried away when I’m working")
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 2.2.8 Register the Spanish UWES-9 item wording
# Source: Schaufeli & Bakker (2004)
variable_registry[["VAR7"]]$item_wording_es <- c(
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# uwes_01 = VAR7.1
VAR7.1 = "En mi trabajo me siento lleno de energía",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# uwes_02 = VAR7.2
VAR7.2 = "Soy fuerte y vigoroso en mi trabajo",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# uwes_03 = VAR7.3
VAR7.3 = "Estoy entusiasmado con mi trabajo",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# uwes_04 = VAR7.4
VAR7.4 = "Mi trabajo me inspira",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# uwes_05 = VAR7.5
VAR7.5 = "Cuando me levanto por las mañanas tengo ganas de ir a trabajar",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# uwes_06 = VAR7.6
VAR7.6 = "Soy feliz cuando estoy absorto en mi trabajo",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# uwes_07 = VAR7.7
VAR7.7 = "Estoy orgulloso del trabajo que hago",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# uwes_08 = VAR7.8
VAR7.8 = "Estoy inmerso en mi trabajo",
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# uwes_09 = VAR7.9
VAR7.9 = "Me “dejo llevar” por mi trabajo")
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 2.2.9 Register WAMI reverse-keying for the English and Spanish versions
# Sources: Steger et al. (2012) and Carrasco et al. (2026).
# Item 3 (VAR6.3) is reverse-keyed in both versions; values are unchanged.
# 2.2.9.1 Retrieve the original WAMI item names from the wording registry
wami_items <- names(variable_registry[["VAR6"]]$item_wording_en)
# 2.2.9.2 Flag item 3 as reverse-keyed for both language versions
wami_reverse <- stats::setNames(wami_items == "VAR6.3", wami_items)
variable_registry[["VAR6"]]$reverse_keyed_en <- wami_reverse
variable_registry[["VAR6"]]$reverse_keyed_es <- wami_reverse

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 2.3 Apply the definitive variable-label mapping
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 2.3.1 Collect the final column names under their source-variable names
variable_labels <- do.call(c, unname(lapply(variable_registry, function(entry) {
entry$variable_labels
})))

# 2.3.2 Align the mapping with the imported column order
stopifnot(all(names(raw_imported_data) %in% names(variable_labels)))
variable_labels <- variable_labels[names(raw_imported_data)]

# 2.3.3 Assign the final headers to a copy of the imported dataset
# Examples: VAR4.1 -> jcs_01; VAR5.1 -> jcq_01; VAR6.1 -> wami_01.
data <- raw_imported_data
names(data) <- unname(variable_labels)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 2.4 Assemble the definitive bilingual codebook
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 2.4.1 Convert the registry into one codebook row per variable
codebook <- do.call(rbind, lapply(variable_registry, function(block) {
variables <- names(block$variable_labels)
allowed <- block$allowed_values %||% unname(block$value_labels)
# 2.4.1.1 Build codebook entries, using NA for inapplicable metadata
data.frame(source_variable = variables, 
           analysis_variable = unname(block$variable_labels),
           variable_label_en = if (is.null(block$instrument_name))
           unname(block$variable_labels) else
           paste0(block$instrument_name, " (", 
                  block$instrument_abbreviation, "), item ", 
                  sub("^.*\\.", "", variables)),
          instrument_name = block$instrument_name %||% NA_character_,
          instrument_abbreviation = block$instrument_abbreviation %||% NA_character_,
          allowed_values = if (is.null(allowed)) NA_character_ else
          paste(allowed, collapse = "; "),
          value_labels = if (is.null(block$value_labels)) NA_character_ else
          paste(paste0(unname(block$value_labels), " = ",
          names(block$value_labels)), collapse = "; "),
          item_wording_en = unname(
          (block$item_wording_en %||% NA_character_)[variables]),
          item_wording_es = unname(
          (block$item_wording_es %||% NA_character_)[variables]),
          reverse_keyed_en = unname(
          (block$reverse_keyed_en %||% NA)[variables]),
          reverse_keyed_es = unname(
          (block$reverse_keyed_es %||% NA)[variables]))
}))

# 2.4.2 Match the dataset order and document the JCQ wording correspondence
codebook <- codebook[match(names(variable_labels), codebook$source_variable), ]
rownames(codebook) <- NULL
codebook$item_order_note <- NA_character_
codebook$item_order_note[codebook$source_variable == "VAR5.12"] <-
"Spanish item 12 corresponds to original English item 13."
codebook$item_order_note[codebook$source_variable == "VAR5.13"] <-
"Spanish item 13 corresponds to original English item 12."

# 2.4.3 Attach the English variable labels to the definitive dataset
for (i in seq_len(nrow(codebook))) {
attr(data[[codebook$analysis_variable[i]]], "label") <-
codebook$variable_label_en[i]
}

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 2.5 Export the definitive dataset, codebook, and registry
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 2.5.1 Export the definitive dataset as CSV with explicit NA values
readr::write_csv(data, file.path(output_dir_02, "1.Step02_Analysis_Dataset.csv")
                                 , na = "NA")
# 2.5.1.1 Archive the dataset with its informative headers
saveRDS(data, file.path(output_dir_02, "2.Step02_Analysis_Dataset.rds"))
# 2.5.1.2 Export the dataset as a single-sheet Excel workbook
writexl::write_xlsx(list(Data = data), file.path(output_dir_02,
                                              "3.Step02_Analysis_Dataset.xlsx"))

# 2.5.2 Export the definitive bilingual codebook in the same variable order
readr::write_csv(codebook, file.path(output_dir_02, 
                                     "4.Step02_Codebook.csv"), na = "NA")
# 2.5.2.1 Export the bilingual codebook as a single-sheet Excel workbook
writexl::write_xlsx(list(Codebook = codebook), file.path(output_dir_02,
                                                         "5.Step02_Codebook.xlsx"))

# 2.5.3 Archive the complete registry and the exact column-name mapping
saveRDS(list(variable_registry = variable_registry, 
        variable_labels = variable_labels, instrument_names = instrument_names,
        instrument_abbreviations = instrument_abbreviations),
        file.path(output_dir_02, "6.Step02_Variable_Registry.rds"))
