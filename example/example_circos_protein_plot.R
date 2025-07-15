### example data for circos ###
n_values <- 50
letters52 <- c(letters,paste0(letters, letters))
proteins <- letters52[1:50]

sample_reverse_mr <- data.frame(protein = proteins,
                                disease = rep("cancer", n_values),
                                method = rep("Inverse variance weighted", n_values),
                                b = runif(n=n_values, min=1, max=2),
                                se = runif(n=n_values, min=-1, max=1),
                                track_id = 1)

sample_forward_mr <- data.frame(protein = proteins,
                                disease = rep("cancer", n_values),
                                method = rep("Inverse variance weighted", n_values),
                                b = runif(n=n_values, min=1, max=2),
                                se = runif(n=n_values, min=-1, max=1),
                                track_id = 2)

sample_obvs <- data.frame(protein = proteins,
                          disease = rep("cancer", n_values),
                          method = rep("observational", n_values),
                          b = runif(n=n_values, min=1, max=2),
                          se = runif(n=n_values, min=-1, max=1),
                          track_id = 3)

sample_prs <- data.frame(protein = proteins,
                         disease = rep("cancer", n_values),
                         method = rep("prs", n_values),
                         b = runif(n=n_values, min=-1, max=1),
                         se = runif(n=n_values, min=-1, max=1),
                         track_id = 4)


sample_extra <- data.frame(protein = proteins,
                           disease = rep("cancer", n_values),
                           method = rep("prs", n_values),
                           b = runif(n=n_values, min=-1, max=1),
                           se = runif(n=n_values, min=-1, max=1),
                           track_id = 5)

segment_data <- data.frame(protein = proteins, segment = rep(c("segment 1", "segment 2", "segment 3", "segment 4", "segment 5"), length(proteins)/5))

sample_data <- rbind(sample_obvs, sample_prs, sample_reverse_mr, sample_forward_mr[as.integer(runif(n=50, min=1, max=n_values)) %>% unique(),])
sample_data <- sample_data %>% inner_join(segment_data)

circos_protein_plot(circos_data = sample_data,
                    total_track_number = 5,
                    track_id_column = "track_id",
                    segment_names_column = "segment",
                    protein_column = "protein",
                    beta_column = "b",
                    se_column = "se",
                    odds_ratios = T,
                    primary_track = 1,
                    error_bar_ends = T)

circos_protein_plot
