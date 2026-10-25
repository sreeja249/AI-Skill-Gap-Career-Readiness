# AI-Based Skill Gap Analysis and Career Readiness Prediction
# Interactive R Shiny Dashboard

# Install packages only if they are not already installed.
required_packages <- c("shiny", "ggplot2", "cluster")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

library(shiny)
library(ggplot2)
library(cluster)

set.seed(123)

# Create output folders
if (!dir.exists("data")) dir.create("data", recursive = TRUE)
if (!dir.exists("outputs/graphs")) dir.create("outputs/graphs", recursive = TRUE)

# Skill columns used by this dashboard
skills <- c(
  "Python", "SQL", "Excel", "PowerBI", "Statistics",
  "MachineLearning", "Communication", "DSA", "Git", "ProblemSolving"
)

student_file <- file.path("data", "student_skills.csv")

# Create a sample dataset if the CSV does not exist.
# You can replace this file with your own data using the same column names.
if (!file.exists(student_file)) {
  n <- 150
  sample_data <- data.frame(
    student_id = sprintf("S%03d", seq_len(n)),
    Python = sample(0:5, n, replace = TRUE),
    SQL = sample(0:5, n, replace = TRUE),
    Excel = sample(0:5, n, replace = TRUE),
    PowerBI = sample(0:5, n, replace = TRUE),
    Statistics = sample(0:5, n, replace = TRUE),
    MachineLearning = sample(0:5, n, replace = TRUE),
    Communication = sample(0:5, n, replace = TRUE),
    DSA = sample(0:5, n, replace = TRUE),
    Git = sample(0:5, n, replace = TRUE),
    ProblemSolving = sample(0:5, n, replace = TRUE),
    check.names = FALSE
  )
  write.csv(sample_data, student_file, row.names = FALSE)
}

# Load and validate the dataset
students <- read.csv(student_file, check.names = FALSE, stringsAsFactors = FALSE)
required_columns <- c("student_id", skills)
missing_columns <- setdiff(required_columns, names(students))

if (length(missing_columns) > 0) {
  stop(
    paste0(
      "The file data/student_skills.csv is missing these columns: ",
      paste(missing_columns, collapse = ", "),
      ". Add these columns or let the app create its sample dataset."
    )
  )
}

students <- students[, required_columns, drop = FALSE]
students <- students[!duplicated(students$student_id), , drop = FALSE]
students$student_id <- as.character(students$student_id)
students <- students[!is.na(students$student_id) & nzchar(students$student_id), ,
                     drop = FALSE]

for (skill in skills) {
  students[[skill]] <- suppressWarnings(as.numeric(students[[skill]]))
  invalid <- !is.na(students[[skill]]) &
    (students[[skill]] < 0 | students[[skill]] > 5)
  students[[skill]][invalid] <- NA_real_

  med <- if (all(is.na(students[[skill]]))) 0 else
    median(students[[skill]], na.rm = TRUE)
  students[[skill]][is.na(students[[skill]])] <- med
}

if (nrow(students) == 0) {
  stop("No valid student rows were found in data/student_skills.csv.")
}

# Target career skill requirements (ratings are from 0 to 5)
roles <- c(
  "Data Analyst", "Data Scientist", "Software Developer",
  "Business Analyst", "ML Engineer"
)

requirements <- data.frame(
  Role = rep(roles, each = length(skills)),
  Skill = rep(skills, times = length(roles)),
  Required_Level = c(
    4,4,4,4,4,2,4,2,3,3,
    5,4,2,2,5,5,3,3,3,4,
    3,2,1,1,2,2,3,5,4,5,
    2,4,4,3,3,1,5,1,2,4,
    4,3,1,1,5,5,3,4,4,5
  ),
  Importance = c(
    4,5,5,5,4,2,4,1,2,3,
    5,4,1,1,5,5,3,3,2,4,
    2,2,1,1,2,2,3,5,4,5,
    2,4,4,3,3,1,5,1,2,4,
    4,3,1,1,5,5,2,4,4,5
  )
)

write.csv(requirements, "outputs/job_requirements.csv", row.names = FALSE)

# K-means clustering and PCA, with safeguards for small or repetitive datasets
cluster_features <- skills[
  vapply(students[, skills, drop = FALSE],
         function(x) length(unique(x)) > 1 && is.finite(sd(x)) && sd(x) > 0,
         logical(1))
]

students$Cluster <- 1L
cluster_profiles <- data.frame()
pca_data <- data.frame(PC1 = numeric(), PC2 = numeric(), Cluster = factor())

if (nrow(students) >= 4 && length(cluster_features) >= 2) {
  x <- scale(students[, cluster_features, drop = FALSE])
  x <- as.matrix(x)
  x <- x[complete.cases(x), , drop = FALSE]

  distinct_rows <- unique(as.data.frame(x))
  k <- min(4, nrow(x) - 1, nrow(distinct_rows))

  if (nrow(x) >= 4 && ncol(x) >= 2 && k >= 2 && all(is.finite(x))) {
    set.seed(123)
    km <- tryCatch(
      kmeans(x, centers = k, nstart = 25),
      error = function(e) NULL
    )

    if (!is.null(km)) {
      # Map cluster labels back to the matching student rows.
      students$Cluster <- 1L
      valid_rows <- which(complete.cases(scale(
        students[, cluster_features, drop = FALSE]
      )))
      if (length(valid_rows) == length(km$cluster)) {
        students$Cluster[valid_rows] <- km$cluster
      }

      cluster_profiles <- aggregate(
        students[, skills, drop = FALSE],
        by = list(Cluster = students$Cluster),
        FUN = mean
      )

      pca <- tryCatch(prcomp(x, rank. = 2), error = function(e) NULL)
      if (!is.null(pca) && ncol(pca$x) >= 2) {
        pca_data <- data.frame(
          PC1 = pca$x[, 1],
          PC2 = pca$x[, 2],
          Cluster = factor(km$cluster)
        )
      }
    }
  }
}

write.csv(students, "outputs/student_clusters.csv", row.names = FALSE)
write.csv(cluster_profiles, "outputs/cluster_skill_profiles.csv", row.names = FALSE)

# Dashboard UI
ui <- fluidPage(
  titlePanel("AI-Based Skill Gap Analysis and Career Readiness"),
  sidebarLayout(
    sidebarPanel(
      h4("Student Assessment"),
      selectInput(
        "student", "Select Student",
        choices = students$student_id,
        selected = students$student_id[1]
      ),
      selectInput(
        "role", "Select Target Career",
        choices = roles, selected = "Data Analyst"
      ),
      hr(),
      p("Skill ratings range from 0 (no knowledge) to 5 (expert)."),
      actionButton("analyze", "Analyze Career Readiness", class = "btn-primary"),
      hr(),
      downloadButton("download_gap", "Download Skill Gap CSV"),
      br(), br(),
      downloadButton("download_summary", "Download Summary CSV")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Overview",
          br(),
          h3("Career Readiness Score"),
          h2(textOutput("readiness")),
          textOutput("selected_info"),
          hr(),
          h4("Current Skill Levels"),
          plotOutput("skill_plot", height = "380px"),
          h4("Career Readiness Comparison"),
          plotOutput("role_plot", height = "320px")
        ),
        tabPanel(
          "Skill Gap Analysis",
          br(),
          h3("Skills to Improve"),
          tableOutput("gap_table"),
          plotOutput("gap_plot", height = "380px"),
          h4("Personalized Recommendations"),
          tableOutput("recommendation_table")
        ),
        tabPanel(
          "Machine Learning",
          br(),
          h3("K-Means Student Clusters"),
          p("PCA projects student skill profiles into two dimensions."),
          plotOutput("cluster_plot", height = "450px"),
          h4("Average Skills by Cluster"),
          tableOutput("cluster_table")
        ),
        tabPanel(
          "Dataset",
          br(),
          h3("Student Skill Dataset"),
          tableOutput("data_table"),
          downloadButton("download_data", "Download Cleaned Dataset")
        )
      )
    )
  )
)

# Dashboard server
server <- function(input, output, session) {

  analysis <- eventReactive(input$analyze, {
    req(input$student, input$role)

    student <- students[students$student_id == input$student, , drop = FALSE]
    req(nrow(student) == 1)

    current <- as.numeric(student[1, skills, drop = TRUE])
    r <- requirements[requirements$Role == input$role, , drop = FALSE]
    r <- r[match(skills, r$Skill), , drop = FALSE]

    gap <- data.frame(
      Skill = skills,
      Current_Level = current,
      Required_Level = r$Required_Level,
      Gap = pmax(0, r$Required_Level - current),
      Importance = r$Importance
    )
    gap$Priority_Score <- gap$Gap * gap$Importance

    readiness <- 100 * sum(pmin(current, r$Required_Level)) /
      sum(r$Required_Level)

    gap <- gap[order(-gap$Priority_Score, -gap$Gap), , drop = FALSE]

    role_scores <- data.frame(Role = roles, Readiness = numeric(length(roles)))
    for (i in seq_along(roles)) {
      rr <- requirements[requirements$Role == roles[i], , drop = FALSE]
      rr <- rr[match(skills, rr$Skill), , drop = FALSE]
      role_scores$Readiness[i] <- 100 *
        sum(pmin(current, rr$Required_Level)) / sum(rr$Required_Level)
    }
    role_scores <- role_scores[order(-role_scores$Readiness), , drop = FALSE]

    recommendations <- gap[gap$Gap > 0, , drop = FALSE]
    if (nrow(recommendations) > 0) {
      recommendations$Recommendation <- paste(
        "Improve", recommendations$Skill,
        "from level", recommendations$Current_Level,
        "toward level", recommendations$Required_Level
      )
    } else {
      recommendations <- data.frame(
        Skill = "None", Current_Level = NA_real_, Required_Level = NA_real_,
        Gap = 0, Importance = NA_real_, Priority_Score = 0,
        Recommendation = "No gaps against the selected requirements."
      )
    }

    list(
      student = student, current = current, role = input$role,
      readiness = readiness, gap = gap, role_scores = role_scores,
      recommendations = recommendations
    )
  }, ignoreInit = FALSE)

  output$readiness <- renderText({
    paste0(round(analysis()$readiness, 1), "%")
  })

  output$selected_info <- renderText({
    a <- analysis()
    paste("Student:", a$student$student_id, "| Target career:", a$role)
  })

  output$skill_plot <- renderPlot({
    a <- analysis()
    req(a)
    req_df <- requirements[requirements$Role == a$role, , drop = FALSE]
    req_df <- req_df[match(skills, req_df$Skill), , drop = FALSE]
    plot_df <- data.frame(
      Skill = skills, Current = a$current,
      Required = req_df$Required_Level
    )

    ggplot(plot_df, aes(x = reorder(Skill, Current))) +
      geom_col(aes(y = Required), fill = "grey80") +
      geom_col(aes(y = Current), fill = "steelblue") +
      coord_flip() +
      labs(title = "Current Skills vs Required Skills",
           x = "Skill", y = "Level (0-5)") +
      theme_minimal()
  })

  output$role_plot <- renderPlot({
    a <- analysis()
    ggplot(a$role_scores, aes(x = reorder(Role, Readiness), y = Readiness)) +
      geom_col(fill = "darkseagreen3") +
      coord_flip() +
      scale_y_continuous(limits = c(0, 100)) +
      labs(x = "Career", y = "Readiness (%)") +
      theme_minimal()
  })

  output$gap_table <- renderTable({
    analysis()$gap
  }, digits = 2, striped = TRUE)

  output$gap_plot <- renderPlot({
    gap_df <- analysis()$gap
    ggplot(gap_df, aes(x = reorder(Skill, Gap), y = Gap)) +
      geom_col(fill = "tomato") +
      coord_flip() +
      labs(title = "Skill Gap by Skill", x = "Skill", y = "Missing levels") +
      theme_minimal()
  })

  output$recommendation_table <- renderTable({
    analysis()$recommendations
  }, striped = TRUE)

  output$cluster_plot <- renderPlot({
    validate(need(
      nrow(pca_data) > 0,
      "Not enough different student skill profiles to create the PCA plot."
    ))
    ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster)) +
      geom_point(size = 3, alpha = 0.8) +
      labs(title = "Student Skill Profiles",
           x = "Principal Component 1", y = "Principal Component 2") +
      theme_minimal()
  })

  output$cluster_table <- renderTable({
    validate(need(
      nrow(cluster_profiles) > 0,
      "Cluster profiles are unavailable for this dataset."
    ))
    cluster_profiles
  }, digits = 2)

  output$data_table <- renderTable({
    head(students, 30)
  }, digits = 2)

  output$download_gap <- downloadHandler(
    filename = function() "skill_gap_report.csv",
    content = function(file) write.csv(analysis()$gap, file, row.names = FALSE)
  )

  output$download_summary <- downloadHandler(
    filename = function() "career_readiness_summary.csv",
    content = function(file) {
      a <- analysis()
      highest <- if (any(a$gap$Gap > 0)) {
        a$gap$Skill[which(a$gap$Gap > 0)[1]]
      } else {
        "None"
      }
      summary_data <- data.frame(
        Student_ID = a$student$student_id,
        Target_Role = a$role,
        Readiness_Percent = round(a$readiness, 2),
        Highest_Priority_Skill = highest
      )
      write.csv(summary_data, file, row.names = FALSE)
    }
  )

  output$download_data <- downloadHandler(
    filename = function() "cleaned_student_skills.csv",
    content = function(file) write.csv(students, file, row.names = FALSE)
  )
}

shinyApp(ui = ui, server = server)
