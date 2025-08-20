### Load packages ###
library(tidyverse)   # wrangling + plotting
library(extraDistr)  # Dirichlet sampling
library(cowplot)
library(patchwork)
library(grid)

### Import + setup ###
input <- read.csv("round2_all.csv")
str(input)

dat <- input %>%
  select(-Expert_name, -Confidence, -Notes) %>%
  mutate(across(where(is.character), as.factor),
         Level = factor(Level, levels = c("Minimal","Minor","Moderate","Major","Massive")))

str(dat)
anyNA(dat$Votes)

set.seed(123)

n_levels <- 5
level_names <- c("Minimal","Minor","Moderate","Major","Massive")
votes_per_exp <- 100
col_levels <- c("#d9f0d3","#a6dba0","#5aae61","#1b7837","#00441b")

### Aggregate raw votes ###
agg <- dat %>%
  group_by(Variable, Category, Level) %>%
  summarise(TotalVotes = sum(Votes), .groups = "drop_last") %>%
  mutate(Prob = TotalVotes / sum(TotalVotes),
         LevelNum = as.numeric(Level))

summ_wmean <- agg %>%
  group_by(Variable, Category) %>%
  summarise(WMean = sum(Prob * LevelNum), .groups = "drop") %>%
  arrange(Variable, WMean)

### Dirichlet draws ###
mean_p <- array(0,
                dim = c(length(unique(dat$Category)),
                        length(unique(dat$Level)),
                        length(unique(dat$Variable)),
                        1),
                dimnames = list(unique(dat$Category),
                                unique(dat$Level),
                                unique(dat$Variable),
                                "Basic"))

sims_all <- data.frame()

for (var in unique(dat$Variable)) {
  dvar <- filter(dat, Variable == var)
  cats <- unique(dvar$Category)
  exp_ids <- unique(dvar$Expert_ID)
  
  for (ct in cats) {
    mat <- dvar %>%
      filter(Category == ct) %>%
      arrange(Expert_ID, Level) %>%
      pull(Votes) %>%
      matrix(ncol = n_levels, byrow = TRUE)
    
    exp_ct <- nrow(mat)
    
    draws <- data.frame(matrix(0, exp_ct * votes_per_exp, n_levels),
                        Expert = rep(1:exp_ct, each = votes_per_exp)) %>%
      mutate(Category = ct, Variable = var)
    
    colnames(draws)[1:n_levels] <- paste0("Level", 1:n_levels)
    
    for (e in 1:exp_ct) {
      nonzero <- which(mat[e, ] > 0)
      if (length(nonzero) == 1) {
        draws[draws$Expert == e, nonzero] <- 1
      } else {
        draws[draws$Expert == e, nonzero] <-
          rdirichlet(votes_per_exp, alpha = mat[e, nonzero])
      }
    }
    
    mean_p[ct, , var, 1] <- round(colMeans(draws[,1:n_levels]), 5)
    sims_all <- bind_rows(sims_all, draws)
  }
}

write.csv(sims_all, "C:/Users/Ruby/Downloads/All_sims.csv", row.names = FALSE)

### Summaries ###
cat_order <- c("Eggs","Eggs_C","Eggs_W",
               "First years_C_Summ","First years_W_Summ",
               "First years_C_Wint","First years_W_Wint",
               "Sub-adults_C_HH","Sub-adults_W_HH",
               "Sub-adults_C_NH","Sub-adults_W_NH",
               "Adults_C_HH","Adults_C_NH","Adults_W_NH",
               "Isle of Man","North Wales","South Wales",
               "Ireland","Paradise Park (Captive)",
               "Cornwall","Translocation only ","Improved habitat")

sims_all$Category <- factor(sims_all$Category, levels = cat_order)

sims_all <- sims_all %>%
  mutate(Expected = Level1*1 + Level2*2 + Level3*3 + Level4*4 + Level5*5)

summ <- sims_all %>%
  group_by(Category, Variable) %>%
  summarise(Mean = mean(Expected),
            LCL = quantile(Expected, 0.025),
            UCL = quantile(Expected, 0.975),
            SD = sd(Expected), .groups = "drop")

write.csv(summ, "C:/Users/Ruby/Downloads/Summary_risks.csv", row.names = FALSE)

### Violin plots ###
long_df <- sims_all %>%
  pivot_longer(starts_with("Level"), names_to = "Level", values_to = "Prob") %>%
  mutate(Level = factor(Level, levels = paste0("Level",1:5)))

viol_plots <- long_df %>%
  split(.$Variable) %>%
  map(~ ggplot(., aes(Level, Prob)) +
        geom_violin(aes(fill=Level), color="black", alpha=0.8, scale="width",
                    linewidth=0.2, draw_quantiles=c(.25,.75)) +
        scale_fill_manual(values=col_levels, labels=level_names) +
        stat_summary(fun=mean, geom="point", size=0.8) +
        facet_wrap(~Category) +
        coord_cartesian(ylim=c(0,1)) +
        theme_bw() +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.text.y=element_text(size=12),
              legend.position="bottom",
              legend.title=element_blank()))

### Density plots ###
dens_plots <- sims_all %>%
  split(.$Variable) %>%
  map(~ ggplot(., aes(Expected)) +
        geom_density(bw=0.5) +
        facet_wrap(~Category) +
        scale_x_continuous(limits=c(1,5)) +
        ggtitle(unique(.$Variable)))

### CI plots ###
ci_plots <- summ %>%
  split(.$Variable) %>%
  map(~ ggplot(., aes(x=Mean, y=reorder(Category, Mean))) +
        geom_vline(xintercept=1:5, colour=col_levels, alpha=0.6,
                   linetype="dashed", linewidth=1) +
        geom_point(size=2) +
        geom_errorbarh(aes(xmin=LCL, xmax=UCL), height=0.2) +
        scale_x_continuous(limits=c(1,5), breaks=1:5,
                           name="\nImpact Level") +
        labs(x="\nAverage (Mean) with 95% CI", y="Translocation pathway\n") +
        theme_bw() +
        theme(axis.ticks=element_blank(),
              axis.text=element_text(size=12),
              axis.title.y=element_text(size=18, vjust=2.5),
              plot.title=element_text(size=18, face="bold", hjust=0.5)))

### Raw plots ###
dat$Category <- factor(dat$Category, levels=cat_order)

raw_exp <- dat %>%
  split(.$Variable) %>%
  map(~ ggplot(., aes(Level, Votes, fill=Expert_ID)) +
        geom_bar(stat="identity", color="black", position="dodge") +
        scale_y_continuous(breaks=seq(0, max(dat$Votes,na.rm=T),20)) +
        facet_wrap(~Category) +
        scale_fill_brewer(palette="Set3") +
        theme(axis.text.x=element_text(angle=45,hjust=1)))

raw_box <- dat %>%
  split(.$Variable) %>%
  map(~ ggplot(., aes(Level, Votes, fill=Level)) +
        geom_boxplot(color="black") +
        facet_wrap(~Category) +
        scale_y_continuous(breaks=seq(0, max(dat$Votes,na.rm=T),20)) +
        scale_fill_manual(values=col_levels, labels=level_names) +
        theme(axis.text.x=element_text(angle=45,hjust=1)))

### Report figures ###
welfare_plot <- ci_plots[[5]] / plot_spacer() / viol_plots[[5]] +
  plot_layout(heights=c(1,.01,1.5)) +
  plot_annotation(tag_levels="A") &
  theme(plot.tag=element_text(size=16, face="bold"))

ggsave("C:/Users/Ruby/Downloads/Welfare_fig.jpg",
       welfare_plot, width=10, height=12, dpi=300)

disease_plot <- ci_plots[[1]] / plot_spacer() / viol_plots[[1]] +
  plot_layout(heights=c(1,.01,1.5)) +
  plot_annotation(tag_levels="A") &
  theme(plot.tag=element_text(size=16, face="bold"))

ggsave("C:/Users/Ruby/Downloads/Disease_fig.jpg",
       disease_plot, width=10, height=12, dpi=300)

ci_plots[[4]] <- ci_plots[[4]] + labs(y="Donor population\n")

outbreed_plot <- ci_plots[[4]] / plot_spacer() / viol_plots[[4]] +
  plot_layout(heights=c(1,.01,1.5)) +
  plot_annotation(tag_levels="A") &
  theme(plot.tag=element_text(size=16, face="bold"))

ggsave("C:/Users/Ruby/Downloads/Outbreeding_fig.jpg",
       outbreed_plot, width=10, height=8, dpi=300)

ci_plots[[2]] <- ci_plots[[2]] +
  labs(title="Negative impacts\n", y="Habitat scenario\n") +
  scale_y_discrete(labels=c("Translocation only " = "Translocation\nonly",
                            "Improved habitat" = "Improved\nhabitat"))

ci_plots[[3]] <- ci_plots[[3]] +
  labs(title="Positive impacts\n", y=NULL) +
  scale_y_discrete(labels=c("Translocation only " = "Translocation\nonly",
                            "Improved habitat" = "Improved\nhabitat"))

viol_plots[[3]] <- viol_plots[[3]] +
  labs(y=NULL) +
  theme(legend.position="none")

left_col <- ci_plots[[2]] / plot_spacer() / viol_plots[[2]] +
  plot_layout(heights=c(1,.01,1.2))

right_col <- ci_plots[[3]] / plot_spacer() / viol_plots[[3]] +
  plot_layout(heights=c(1,.01,1.2))

species_plot <- (left_col | right_col) +
  plot_layout(guides="collect") +
  plot_annotation(tag_levels="A") &
  theme(plot.tag=element_text(size=14, face="bold"),
        legend.position="bottom")

ggsave("C:/Users/Ruby/Downloads/Species_fig.jpg",
       species_plot, width=10, height=6, dpi=300)
