
library(igraph)

files <- list(

  SPA = "C:\\Users\\zohreh\\Desktop\\complex\\data\\spa_news_2020_30K-sentences.txt",
  FRA = "C:\\Users\\zohreh\\Desktop\\complex\\data\\fra_news_2020_30K-sentences.txt",
  DEU = "C:\\Users\\zohreh\\Desktop\\complex\\data\\deu_news_2020_30K-sentences.txt",
  ITA = "C:\\Users\\zohreh\\Desktop\\complex\\data\\ita_news_2020_30K-sentences.txt",
  ENG = "C:\\Users\\zohreh\\Desktop\\complex\\data\\eng_news_2020_30K-sentences.txt",
  FIN = "C:\\Users\\zohreh\\Desktop\\complex\\data\\fin_news_2022_10K-sentences.txt",
  HUN = "C:\\Users\\zohreh\\Desktop\\complex\\data\\hun_news_2022_10K-sentences.txt",
  SRP = "C:\\Users\\zohreh\\Desktop\\complex\\data\\srp_news_2022_10K-sentences.txt",
  CAT="C:\\Users\\zohreh\\Desktop\\complex\\data\\cat_news_2022_30K-sentences.txt",
  EUS= "C:\\Users\\zohreh\\Desktop\\complex\\data\\eus_wikipedia_2021_10K-sentences.txt",
  TUR = "C:\\Users\\zohreh\\Desktop\\complex\\data\\tur_news_2022_10K-sentences.txt"
)
output_folder <- "C:\\Users\\zohreh\\Desktop\\complex\\outputs"

tokenize_function <- function(input_path,max_sentences = 5000){
  lines <- readLines(input_path, encoding = "UTF-8")
  n <- min(length(lines), max_sentences)
  lines <- lines[1:n]
  lines <- sub("^[0-9]+\\s+", "", lines)
  lines <- tolower(lines)
  lines <- gsub("[[:punct:]]", " ", lines)
  lines <- gsub("\\s+", " ", lines)
  lines <- trimws(lines)
  lines <- lines[lines != ""]
  tokenized <- strsplit(lines, " ")
  
  return(tokenized)
  #tokenized is a list of vectors
  #tokrnized[[1]]= first scentenc of the input
  #exp:c("the", "dog", "is", "running")
}
frequency_function<- function(tokenized){
  words<- unlist(tokenized)
  word_freq<-table(words)
  return(word_freq)
  #contains both words and the frequency. should be converted to dataframe later
}

sorted_ID <- function(word_freq, top_n=1000, min_freq=2){
  word_freq_df<- as.data.frame(word_freq)
  colnames(word_freq_df) <- c("word", "freq")
  word_freq_df <- word_freq_df[word_freq_df$freq >= min_freq, ]
  word_freq_df <- word_freq_df[order(word_freq_df$freq, decreasing = TRUE), ]
  word_freq_df$word_ID <- 1:nrow(word_freq_df)
  return(word_freq_df)
  # a data frame which contains the words (sorted by freq)+freq+ID
  # most frequent word has smallest ID
}

tokenized_to_ID <- function(tokenized, word_freq_df){
  sentences_by_ID <- list()
  for(i in 1:length(tokenized)){
    sentence <- tokenized[[i]]
    sentence<- sentence[sentence %in% word_freq_df$word]
    positions <- match(sentence, word_freq_df$word)
    sentences_by_ID[[i]] <- word_freq_df$word_ID[positions]
  }
  return(sentences_by_ID)
  # a list which containes multiple numeric vectors( as sentences)
}

reconstruct_tokens <- function(sentences_by_ID, word_freq_df){
  sentence_by_token <- list()
  for(i in 1:length(sentences_by_ID)){
    sentence <- sentences_by_ID[[i]]
    positions <- match(sentence, word_freq_df$word_ID)
    sentence_by_token[[i]] <- word_freq_df$word[positions]
  }
  return(sentence_by_token)
}

#______________________________
#Network construction

build_edges <- function(sentences_by_ID, window_size){
  
  from_vec <- c()
  to_vec <- c()
  
  for (i in 1:length(sentences_by_ID)){
    
    sentence <- sentences_by_ID[[i]]
    r <- length(sentence)
    
    if(r > 1){
      
      for (j in 1:(r-1)){
        
        max_j <- min(r, j + window_size - 1)
        k <- j + 1
        
        while(k <= max_j){
          
          node1 <- sentence[j]
          node2 <- sentence[k]
          
          from <- min(node1, node2)
          to <- max(node1, node2)
          
          from_vec <- c(from_vec, from)
          to_vec <- c(to_vec, to)
          
          k <- k + 1
        }
      }
    }
  }
  
  edge_list <- data.frame(from = from_vec, to = to_vec)
  
  return(edge_list)
}

weight_edges <- function(edge_list){
  edge_frequency <- table(edge_list$from, edge_list$to)
  edge_freq_df<- as.data.frame(edge_frequency)
  colnames(edge_freq_df) <- c("from", "to","weight")
  edge_freq_df=edge_freq_df[edge_freq_df$weight > 0, ]
  return(edge_freq_df)
}

graph_maker <- function(edge_freq_df, directed = FALSE){
  g <- graph_from_data_frame(edge_freq_df, directed = directed)
  return(g)
}



# outputs

# one node file per language: `wordID word frequency language

file_per_language <- function(word_freq_df,language_name){
  node_df <- word_freq_df
  node_df$language <- language_name
  colnames(node_df) <- c("word", "frequency", "wordID", "language")
  node_df <- node_df[, c("wordID", "word", "frequency", "language")]
  
  return(node_df)
}

# one weighted edge file per language: `wordID1 wordID2 weight`;

weight_file_per_language <- function(edge_freq_df,language_name){
  weight_df<- edge_freq_df
  weight_df$language <- language_name
  colnames(weight_df) <-c("from","to","weight","language")
  return(weight_df)
}
#_______________________________________

#outputs:

files_group1 <- files[c("FRA", "ITA", "DEU", "SPA")]
files_group2 <- files[c("FIN", "HUN", "TUR", "SRP")]
files_group3 <- files[c("EUS", "CAT")]


run_languages <- function(selected_files){
  
  for(language_name in names(selected_files)){
    
    print(paste("Processing", language_name))
    
    input_path <- selected_files[[language_name]]
    
    tokenized <- tokenize_function(input_path, max_sentences = 5000)
    word_freq <- frequency_function(tokenized)
    word_freq_df <- sorted_ID(word_freq, top_n = 500, min_freq = 2)
    sentences_by_ID <- tokenized_to_ID(tokenized, word_freq_df)
    
    edge_list <- build_edges(sentences_by_ID, window_size = 2)
    edge_freq_df <- weight_edges(edge_list)
    g <- graph_maker(edge_freq_df, directed = FALSE)
    
    node_df <- file_per_language(word_freq_df, language_name)
    weight_df <- weight_file_per_language(edge_freq_df, language_name)
    
    write.csv(node_df,
              file = paste0(output_folder, "\\nodes_", language_name, ".csv"),
              row.names = FALSE)
    
    write.csv(weight_df,
              file = paste0(output_folder, "\\edges_", language_name, ".csv"),
              row.names = FALSE)
    
    saveRDS(g,
            file = paste0(output_folder, "\\graph_", language_name, ".rds"))
  }
}
run_languages(files["ENG"])
run_languages(files_group1)
run_languages(files_group2)
run_languages(files_group3)

#__________________________________________
#Robustness outputs:
robustness_output_folder <- "C:\\Users\\zohreh\\Desktop\\complex\\robustness_output"


files_group1 <- files[c("FRA", "ITA", "DEU")]
files_group2 <- files[c("FIN", "HUN", "TUR", "SRP")]
files_group3 <- files[c("EUS", "CAT")]


run_languages <- function(selected_files){
  
  for(language_name in names(selected_files)){
    
    print(paste("Processing", language_name))
    
    input_path <- selected_files[[language_name]]
    
    tokenized <- tokenize_function(input_path, max_sentences = 7000)
    word_freq <- frequency_function(tokenized)
    word_freq_df <- sorted_ID(word_freq, top_n = 500, min_freq = 2)
    sentences_by_ID <- tokenized_to_ID(tokenized, word_freq_df)
    
    edge_list <- build_edges(sentences_by_ID, window_size = 3)
    edge_freq_df <- weight_edges(edge_list)
    g <- graph_maker(edge_freq_df, directed = FALSE)
    
    node_df <- file_per_language(word_freq_df, language_name)
    weight_df <- weight_file_per_language(edge_freq_df, language_name)
    
    write.csv(node_df,
              file = paste0(robustness_output_folder, "\\nodes_", language_name, ".csv"),
              row.names = FALSE)
    
    write.csv(weight_df,
              file = paste0(robustness_output_folder, "\\edges_", language_name, ".csv"),
              row.names = FALSE)
    
    saveRDS(g,
            file = paste0(robustness_output_folder, "\\graph_", language_name, ".rds"))
  }
}
run_languages(files["ENG"])
run_languages(files_group1)
run_languages(files["SPA"])

run_languages(files_group2)
run_languages(files_group3)
########################################################################################
########################################################################################

#analysis_code
library(igraph)

metadata <- read.csv(
  "C:/Users/zohreh/Desktop/complex/data/InfoRateData.csv",
  stringsAsFactors = FALSE
)
graph_files <- list(
  g_SPA = "C:/Users/zohreh/Desktop/complex/outputs/graph_SPA.rds",
  g_FRA = "C:/Users/zohreh/Desktop/complex/outputs/graph_FRA.rds",
  g_DEU = "C:/Users/zohreh/Desktop/complex/outputs/graph_DEU.rds",
  g_ITA = "C:/Users/zohreh/Desktop/complex/outputs/graph_ITA.rds",
  g_ENG = "C:/Users/zohreh/Desktop/complex/outputs/graph_ENG.rds",
  g_FIN = "C:/Users/zohreh/Desktop/complex/outputs/graph_FIN.rds",
  g_HUN = "C:/Users/zohreh/Desktop/complex/outputs/graph_HUN.rds",
  g_EUS = "C:/Users/zohreh/Desktop/complex/outputs/graph_EUS.rds", 
  g_cat = "C:/Users/zohreh/Desktop/complex/outputs/graph_CAT.rds",
  g_SRP = "C:/Users/zohreh/Desktop/complex/outputs/graph_SRP.rds",
  f_TUR = "C:/Users/zohreh/Desktop/complex/outputs/graph_TUR.rds"
)
robustness_graphs <- list(
  g_SPA = "C:/Users/zohreh/Desktop/complex/robustness_output/graph_SPA.rds",
  g_FRA = "C:/Users/zohreh/Desktop/complex/robustness_output/graph_FRA.rds",
  g_DEU = "C:/Users/zohreh/Desktop/complex/robustness_output/graph_DEU.rds",
  g_ITA = "C:/Users/zohreh/Desktop/complex/robustness_output/graph_ITA.rds",
  g_ENG = "C:/Users/zohreh/Desktop/complex/robustness_output/graph_ENG.rds",
  g_FIN = "C:/Users/zohreh/Desktop/complex/robustness_output/graph_FIN.rds",
  g_HUN = "C:/Users/zohreh/Desktop/complex/robustness_output/graph_HUN.rds",
  g_EUS = "C:/Users/zohreh/Desktop/complex/robustness_output/graph_EUS.rds", 
  g_cat = "C:/Users/zohreh/Desktop/complex/robustness_output/graph_CAT.rds",
  g_SRP = "C:/Users/zohreh/Desktop/complex/robustness_output/graph_SRP.rds",
  f_TUR = "C:/Users/zohreh/Desktop/complex/robustness_output/graph_TUR.rds"
)
library(dplyr)
information_theo_var <- function(metadata){
  target_langs <- c(
    "ENG", "ITA", "DEU", "FRA", "SPA",
    "FIN", "HUN", "TUR", "SRP", "EUS","CAT"
  )
  info_df <- metadata %>%
    filter(Language %in% target_langs) %>%
    group_by(Language) %>%
    summarise(
      N_obs = n(),
      Duration = mean(Duration, na.rm = TRUE),
      NS = mean(NS, na.rm = TRUE),
      ShE = mean(ShE, na.rm = TRUE),
      ID = mean(ID, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(match(Language, target_langs))
  
  return(info_df)
}

info_df <- information_theo_var(metadata)
print(info_df)
write.csv(
  info_df,
  "C:/Users/zohreh/Desktop/complex/tables/lang_metadata_table.csv",
  row.names = FALSE
)

#_________________________________________

graph_summary <- function(g, language_name){
  
  degrees <- degree(g)
  comp <- components(g)
  communities <- cluster_louvain(g)
  summary_df <- data.frame(
    language = language_name,
    nodes = vcount(g),
    edges = ecount(g),
    density = edge_density(g),
    mean_degree = mean(degrees),
    max_degree = max(degrees),
    components = comp$no,
    largest_component = max(comp$csize),
    clustering_coefficient =transitivity(g, type = "global"),
    assortativity =assortativity_degree(g, directed = FALSE),
    modularity =modularity(communities)
  )
  
  return(summary_df)
}

graphs <- list()

for(i in 1:length(graph_files)){
  graphs[[i]] <- readRDS(graph_files[[i]])
}

names(graphs) <- names(graph_files)
all_summary <- data.frame()

for(i in 1:length(graphs)){
  g <- graphs[[i]]
  language_name <- sub("^g_", "", names(graphs)[i])
  summary_df <- graph_summary(g, language_name)
  all_summary <- rbind(all_summary, summary_df)
}
View(all_summary)
write.csv(
  all_summary,
  "C:/Users/zohreh/Desktop/complex/Tables/network_summary.csv",
  row.names = FALSE
)

#_______________
#table visualization
output_dir <- "C:/Users/zohreh/Desktop/complex/table_graphs"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

metrics <- c("nodes", "edges", "density", "mean_degree", "max_degree",
             "components", "largest_component", "clustering_coefficient",
             "assortativity", "modularity")

for(metric in metrics){
  output_path <- file.path(output_dir, paste0(metric, ".png"))
  
  values <- as.numeric(all_summary[[metric]])
  langs  <- as.character(all_summary$language)
  
  png(output_path, width = 700, height = 400)
  par(mar = c(4, 6, 3, 2))
  
  barplot(values,
          names.arg = langs,
          main      = metric,
          col       = "#7EC8E3",
          border    = "white",
          las       = 1,
          cex.names = 0.9,
          cex.main  = 1.2,
          ylab      = metric)
  
  dev.off()
  cat("Saved:", output_path, "\n")
}
#___________________________________________
#merging network_summary with metadata


network_summary <- read.csv(
  "C:/Users/zohreh/Desktop/complex/Tables/network_summary.csv",
  stringsAsFactors = FALSE
)

lang_metadata <- read.csv(
  "C:/Users/zohreh/Desktop/complex/Tables/lang_metadata_table.csv",
  stringsAsFactors = FALSE
)

final_table <- merge(
  network_summary,
  lang_metadata,
  by.x = "language",
  by.y = "Language"
)

View(final_table)

write.csv(
  final_table,
  "C:/Users/zohreh/Desktop/complex/tables/merged.csv",
  row.names = FALSE
)
#_____________________________________________________

#degree_distribution


deg_dist <- function(g){
  degrees <- degree(g)
  distribution <- c()
  for (i in 1:max(degrees)){
    distribution[i] <- sum(degrees == i)
  }
  return(distribution)
}

plot_deg_dist <- function(distribution, language_name){
  
  max_degree_show <- 40
  
  distribution <- distribution[1:max_degree_show]
  
  barplot(
    distribution,
    main = paste("Degree Distribution -", language_name),
    xlab = "Degree",
    ylab = "Number of Nodes",
    names.arg = 1:max_degree_show,
    las = 1
  )
}
output_dir <- "C:/Users/zohreh/Desktop/complex/distributions/node_dist"

dir.create(output_dir,
           showWarnings = FALSE,
           recursive = TRUE)

for(i in 1:length(graph_files)){
  
  g <- readRDS(graph_files[[i]])
  
  language_name <- sub("^g_", "", names(graph_files)[i])
  language_name <- sub("^f_", "", language_name)
  
  distribution <- deg_dist(g)
  
  output_file <- file.path(
    output_dir,
    paste0(language_name, "_degree_distribution.png")
  )
  
  png(
    filename = output_file,
    width = 1000,
    height = 600
  )
  
  plot_deg_dist(
    distribution,
    language_name
  )
  
  dev.off()
  
  cat("Saved:", output_file, "\n")
}

#__________________________________________

# Strength distribution

edge_files <- list(
  ENG = "C:/Users/zohreh/Desktop/complex/outputs/edges_ENG.csv",
  ITA = "C:/Users/zohreh/Desktop/complex/outputs/edges_ITA.csv",
  DEU = "C:/Users/zohreh/Desktop/complex/outputs/edges_DEU.csv",
  FRA = "C:/Users/zohreh/Desktop/complex/outputs/edges_FRA.csv",
  SPA = "C:/Users/zohreh/Desktop/complex/outputs/edges_SPA.csv",
  FIN = "C:/Users/zohreh/Desktop/complex/outputs/edges_FIN.csv",
  HUN = "C:/Users/zohreh/Desktop/complex/outputs/edges_HUN.csv",
  TUR = "C:/Users/zohreh/Desktop/complex/outputs/edges_TUR.csv",
  EUS = "C:/Users/zohreh/Desktop/complex/outputs/edges_EUS.csv", 
  CAT = "C:/Users/zohreh/Desktop/complex/outputs/edges_CAT.csv",
  SRP = "C:/Users/zohreh/Desktop/complex/outputs/edges_SRP.csv"
)

strength_dist <- function(edge_df){
  
  all_nodes <- sort(unique(c(edge_df$from, edge_df$to)))
  
  strength_values <- rep(0, length(all_nodes))
  names(strength_values) <- all_nodes
  
  for(i in 1:nrow(edge_df)){
    
    from_node <- as.character(edge_df$from[i])
    to_node   <- as.character(edge_df$to[i])
    w         <- edge_df$weight[i]
    
    strength_values[from_node] <- strength_values[from_node] + w
    strength_values[to_node]   <- strength_values[to_node] + w
  }
  
  strength_values <- as.numeric(strength_values)
  
  return(strength_values)
}

plot_strength_dist <- function(strength_values, language_name){
  
  strength_values <- strength_values[strength_values > 0]
  
  max_show <- quantile(strength_values, 0.95, na.rm = TRUE)
  
  strength_values <- strength_values[strength_values <= max_show]
  
  hist(
    strength_values,
    breaks = 30,
    probability = TRUE,
    main = paste("Strength Distribution -", language_name),
    xlab = "Strength",
    ylab = "Probability density",
    col = "blue",
    border = "black"
  )
}

output_dir <- "C:/Users/zohreh/Desktop/complex/distributions/strength_dist"

for(lang in names(edge_files)){
  
  edge_df <- read.csv(
    edge_files[[lang]],
    stringsAsFactors = FALSE
  )
  
  strength_values <- strength_dist(edge_df)
  
  output_file <- file.path(
    output_dir,
    paste0(lang, "_strength_distribution.png")
  )
  
  png(
    filename = output_file,
    width = 800,
    height = 600
  )
  
  plot_strength_dist(strength_values, lang)
  
  dev.off()
  
  cat("Saved:", output_file, "\n")
}

#________________________________________

#robustness_Network_summary

graphs_r <- list()

for(i in 1:length(robustness_graphs)){
  graphs_r[[i]] <- readRDS(robustness_graphs[[i]])
}

names(graphs_r) <- names(robustness_graphs)
all_summary <- data.frame()

for(i in 1:length(graphs)){
  g <- graphs_r[[i]]
  language_name <- sub("^g_", "", names(graphs)[i])
  summary_df <- graph_summary(g, language_name)
  all_summary <- rbind(all_summary, summary_df)
}
View(all_summary)
write.csv(
  all_summary,
  "C:/Users/zohreh/Desktop/complex/Tables/robustness_network_summary.csv",
  row.names = FALSE
)
#_______________________________________________________________________________

#merging robustness_network_summary with metadata


network_summary <- read.csv(
  "C:/Users/zohreh/Desktop/complex/Tables/robustness_network_summary.csv",
  stringsAsFactors = FALSE
)

lang_metadata <- read.csv(
  "C:/Users/zohreh/Desktop/complex/Tables/lang_metadata_table.csv",
  stringsAsFactors = FALSE
)

final_table <- merge(
  network_summary,
  lang_metadata,
  by.x = "language",
  by.y = "Language"
)

View(final_table)

write.csv(
  final_table,
  "C:/Users/zohreh/Desktop/complex/tables/robustness_merged.csv",
  row.names = FALSE
)
###################################################################################
###################################################################################

#correlation and robustness
library(corrplot)

merged <- read.csv("C:\\Users\\zohreh\\Desktop\\complex\\Tables\\merged.csv")

network_cols <- c(
  "nodes",
  "edges",
  "density",
  "mean_degree",
  "max_degree",
  "largest_component",
  "clustering_coefficient",
  "assortativity",
  "modularity"
)

metadata_cols <- c(
  "Duration",
  "NS",
  "ShE",
  "ID"
)

spearman_matrix <- cor(
  merged[, network_cols],
  merged[, metadata_cols],
  method = "spearman",
  use = "complete.obs"
)

View(round(spearman_matrix, 3))

png(
  "C:/Users/zohreh/Desktop/complex/correlation/spearman_correlation_matrix.png",
  width = 1200,
  height = 1000
)

corrplot(
  spearman_matrix,
  method = "color",
  addCoef.col = "black",
  number.cex = 0.8,
  tl.col = "black",
  tl.srt = 45,
  cl.pos = "r",
  cl.cex = 1,
  is.corr = FALSE
)

dev.off()
####################################################################
#robustness

merged_r <- read.csv("C:/Users/zohreh/Desktop/complex/tables/robustness_merged.csv")

spearman_matrix_r <- cor(
  merged_r[, network_cols],
  merged_r[, metadata_cols],
  method = "spearman",
  use = "complete.obs"
)
png(
  "C:/Users/zohreh/Desktop/complex/correlation/spearman_correlation_matrix_robustness.png",
  width = 1200,
  height = 1000
)

corrplot(
  spearman_matrix_r,
  method = "color",
  addCoef.col = "black",
  number.cex = 0.8,
  tl.col = "black",
  tl.srt = 45,
  cl.pos = "r",
  cl.cex = 1,
  is.corr = FALSE
)

dev.off()
#####################################################################################
#####################################################################################



