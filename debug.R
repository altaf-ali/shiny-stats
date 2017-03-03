library(AER)
data(CASchools)

dataset <- CASchools %>%
  select_if(is.numeric)

input <- list(
  variables = c("teachers", "calworks", "income"),
  exclude = c(0, 0)
)

all_vars <- names(dataset)
selected_vars <- all_vars[all_vars %in% input$variables]

if (length(selected_vars) < 2)
  return()

cor_matrix <- data.frame(cor(dataset, use = "pairwise.complete.obs"))

cor_matrix <- cor_matrix[, selected_vars]
cor_matrix$var1 <- row.names(cor_matrix)

cor_matrix <- cor_matrix %>%
  filter(var1 %in% selected_vars)

cor_matrix <- cor_matrix %>%
  gather(var2, value, 1:(ncol(cor_matrix)-1)) %>%
  mutate(var1 = factor(var1, levels = selected_vars, labels = selected_vars)) %>%
  mutate(var2 = factor(var2, levels = selected_vars, labels = selected_vars)) %>%
  filter(value < input$exclude[1] | value > input$exclude[2]) %>%
  na.omit()

color_palette <- brewer.pal(n = 8, name = "RdBu")

ggplot(data = cor_matrix, aes(var1, var2, fill = value)) +
  geom_tile() +
  # theme(panel.background = element_blank(),
  #       panel.grid.major.y = element_line(color = "lightgray"),
  #       axis.ticks.y = element_blank(),
  #       axis.title.x = element_blank(),
  #       axis.title.y = element_blank(),
  #       axis.text.x  = element_text(angle = 45, vjust = 0.5, size = 16),
  #       axis.text.y  = element_text(vjust = 0.5, size = 16)) +
  # scale_fill_gradient2(low = first(color_palette),
  #                      high = last(color_palette),
  #                      limit = c(-1, 1),
  #                      space = "Lab",
  #                      name = "") +
  # guides(fill = guide_colorbar(barwidth = 1, barheight = 16, title.theme = element_text(angle = 0, size = 14))) +
  geom_text(aes(var2, var1, label = round(value, digits=2)), color = "black", size = 4)


cor_matrix[upper.tri(cor_matrix, diag = TRUE)] <- NA

corrplot::corrplot(cor(dataset, use = "pairwise.complete.obs"))
