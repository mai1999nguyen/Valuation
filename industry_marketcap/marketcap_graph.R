library(extrafont)
theme_set(theme_minimal())

#' Read
ticker_profile <- vroom::vroom(file = file.path(config$folder, "data", "ticker_profile.tsv"))

# Prepare data for ploting: 
df_odered <- ticker_profile %>% filter.(!is.na(marketcap)) %>% 
  mutate.(
  .by = symbol,
  marketcap_category = case.(marketcap/10^9 > 5000, "Large Market cap",
                             marketcap/10^9 > 1000, "Medium Market cap", default = "Small Market cap")
) %>% summarise.(
  .by = c(industryname, marketcap_category),
  n = n.()) %>% 
  mutate.(
    .by = industryname,
  percent = round(100*n/ sum(n)),
  bar_text = paste0(round(100*n/ sum(n)), "%")
)

# Make a draft plot: 
my_colors <- c("#3e6487", "#829cb2", "#c7cdd1", "#edad88", "#e36c33")
my_font <- "Roboto Condensed"

gg <- df_odered %>% 
  ggplot(aes(x = industryname, y = percent, fill = marketcap_category)) + 
  geom_col(width = 0.8) + 
  coord_flip() + 
  scale_fill_manual(values = my_colors[3:1], name = "") + 
  theme(legend.position = "top") + 
  theme(text = element_text(family = my_font)) + 
  guides(fill = guide_legend(reverse = TRUE)) + 
  scale_y_continuous(labels = paste0(seq(0, 100, 25), "%"), expand = c(0, 0)) + 
  theme(plot.title = element_text(size = 18), plot.subtitle = element_text(size = 11, color = "grey20")) + 
  theme(axis.text = element_text(color = "grey20", size = 10.2)) + 
  theme(plot.margin = unit(rep(0.7, 4), "cm")) + 
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.x = element_blank()) + 
  theme(legend.key.height = unit(0.15, "mm")) + 
  labs(x = NULL, y = NULL, 
       title = "Tỉ lệ vốn hóa của các công ty trên từng ngành ", 
       subtitle = "Dự trên số liệu từ SSI.",
       caption = "Mai Nguyễn") +
  theme(plot.title.position = "plot")+
  theme(plot.caption.position = "plot")



# For displaying percent of Small marketcap: 
df_odered %>% 
  filter(marketcap_category == "Small Market cap") %>% 
  filter(percent >= 3) -> df_for_SM

# For displaying percent of Large marketcap: 
df_odered %>% 
  filter(marketcap_category == "Large Market cap") %>% 
  filter(percent >= 3) -> df_for_LM

# Ad text layers: 
gg + 
  geom_text(data = df_for_LM, aes(x = industryname, y = 100 - 2, label = bar_text), size = 4, color = "white", family = my_font) + 
  geom_text(data = df_for_SM, aes(x = industryname, y = 3, label = bar_text), size = 4, color = "white", family = my_font)

