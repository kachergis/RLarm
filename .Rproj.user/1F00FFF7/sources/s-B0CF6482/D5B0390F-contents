library(ggplot2)
library(Rmi::Pairedsc)
require(scales) # to access break formatting functions
library(ggsci)
library(ez)
library(ggpubr)
library("RColorBrewer")
library(showtext)
library(paletteer)
require(dplyr)
require(tidyr)

display.brewer.all()
paletteer_d(palette = "RColorBrewer::Paired")
paletteer_d(palette = "RColorBrewer::RdYlBu")
#font_add("ssPro", "/Users/rdekleijn/Library/Fonts/SourceSansPro-Regular.otf")
showtext_auto()

scientific_10 <- function(x) {   parse(text=gsub("e\\+*", " %*% 10^", scales::scientific_format()(x))) }

ff = function(x, patterns, replacements = patterns, fill = NA, ...) {
  stopifnot(length(patterns) == length(replacements))
  ans = rep_len(as.character(fill), length(x))    
  empty = seq_along(x)
  for(i in seq_along(patterns)) {
    greps = grepl(patterns[[i]], x[empty], ...)
    ans[empty[greps]] = replacements[[i]]  
    empty = empty[!greps]
  }
  return(ans)
}



full_df <- read.csv('data/reply_editor_ppo.csv') # Initialize df with column names
full_df$condition <- ff(full_df$file, c("-curr-", "curr8K", "2M", "curr_rev", "200K", "nocur"), c("400K", "800K", "2M", "0", "200K", "never"), NA, ignore.case = TRUE)
full_df$condition <- factor(full_df$condition, levels = c("0", "200K", "400K", "800K", "2M", "never"))

wide_df <- subset(full_df, step>2000000) %>% pivot_wider(names_from = metric, values_from = value)

wide_df %>% ggplot(aes(x=RT, y=`Distance to base`, color=`Environment/Cumulative Reward`)) + 
  geom_point() + theme_classic()

wide_df %>% filter(step > 5e6, condition!="never") %>%
  ggplot(aes(x=RT, y=`Distance to base`, color=`condition`)) + 
  geom_point(alpha=.4) + theme_classic()

wide_df %>% filter(step > 5e6, condition!="never") %>%
  ggplot(aes(x=RT, y=`Environment/Cumulative Reward`, color=`condition`)) + 
  geom_point(alpha=.4) + theme_classic()


# Comparing non-curr to curr
wide_df %>% filter(step > 5e6, (condition=="0" | condition=="400K")) %>%
  ggplot(aes(x=`Environment/Cumulative Reward`, fill=condition)) +
  geom_histogram(binwidth=.01, alpha=.4, position="identity") +
  theme_classic()

wide_df %>% filter(step > 5e6, (condition=="0" | condition=="400K")) %>%
  ggplot(aes(x=`Distance to base`, fill=condition)) +
  geom_histogram(binwidth=.01, alpha=.4, position="identity") +
  theme_classic()

wide_df %>% filter(step > 5e6, (condition=="0" | condition=="400K")) %>%
  ggplot(aes(x=`Distance moved`, fill=condition)) +
  geom_histogram(binwidth=.001, alpha=.4, position="identity") +
  theme_classic()

wide_df %>% filter(step > 5e6, (condition=="0" | condition=="400K")) %>%
  ggplot(aes(x=`Environment/Cumulative Reward`, y=`Distance to base`, color=`condition`)) + 
  geom_point(alpha=.4) + theme_classic()

wide_df %>% filter(step > 5e6, (condition=="0" | condition=="400K")) %>%
  ggplot(aes(x=`Environment/Cumulative Reward`, y=`Distance moved`, color=`condition`)) + 
  geom_point(alpha=.4) + theme_classic()






wide_df %>% filter(step > 5e6, (condition=="0" | condition=="400K")) %>%
  ggplot(aes(x=`Environment/Cumulative Reward`, fill=condition)) +
  facet_wrap(. ~ file) +
  geom_histogram(binwidth=.01, alpha=.4, position="identity") +
  theme_classic()
  
wide_df %>% filter(step > 5e6, (condition=="0" | condition=="400K")) %>%
  ggplot(aes(x=`Distance to base`, fill=condition)) +
  geom_histogram(binwidth=.05, alpha=.4, position="identity") +
  theme_classic()


wide_df %>% filter(step > 5e6, (condition=="0" | condition=="400K")) %>%
  ggplot(aes(x=`Distance to base`, fill=condition)) +
  facet_wrap(. ~ file) +
  geom_histogram(binwidth=.05, alpha=.4, position="identity") +
  theme_classic()

wide_df %>% filter(step > 5e6, (condition=="0" | condition=="400K")) %>%
  #mutate(file_short = strsplit(file, '/')[[1]][3]) %>%
  mutate(file_short = as.character(as.numeric(as.factor(file)))) %>%
  group_by(condition, file_short) %>%
  summarise(Distance = mean(`Distance to base`),
            Reward = mean(`Environment/Cumulative Reward`)) %>%
  ggplot(aes(x=Distance, y=Reward, color=condition, shape=file_short)) +
  geom_point() +
  theme_classic()



# Split on reward

mean(subset(wide_df, condition=="0" & `Environment/Cumulative Reward`<0.18)$`Distance to base`)

wide_df %>% filter(step > 5e6, condition=="0") %>%
  mutate(median_reward = median(`Environment/Cumulative Reward`), # .47
         high_reward = ifelse(`Environment/Cumulative Reward` > .18, 1, 0)) %>%
  group_by(high_reward) %>%
  summarize(meanDistance = mean(`Distance to base`),
            meanReward = mean(`Environment/Cumulative Reward`),
            sdDistance = sd(`Distance to base`))


corplot <- plot(subset(full_df, metric=="RT" & step==6000000)$value, subset(full_df, metric=="Distance to base" & step==6000000)$value)



rew_df <- subset(full_df, (condition=="400K" | condition=="800K" | condition=="2M" | condition=="0" | condition=="200K" | condition=="never") & metric=="Environment/Cumulative Reward")
ggplot(rew_df, aes(x=step ,y=value, color=condition)) +
  stat_summary(geom="ribbon", colour = NA, fun.min=function(x) mean(x) - (qnorm(0.975) * sd(x) / sqrt(length(x))), fun.max=function(x) mean(x) + (qnorm(0.975) * sd(x) / sqrt(length(x))), aes(fill=condition), alpha=0.3) +
  stat_summary(fun=mean, geom="line", lwd=0.8, aes(group=condition, color=condition)) +
  xlab("steps") +
  ylab("cumulative reward") +
  scale_x_continuous(label = scientific_10) + # Log-scale
  theme_bw() +
  scale_fill_manual(values=c("#A50026","#F46D43", "#FDAE61", "#74ADD1", "#4575B4", "#313695")) + scale_color_manual(values=c("#A50026","#F46D43", "#FDAE61", "#74ADD1", "#4575B4", "#313695")) +   theme(text = element_text(size = 12, family = "ssPro")) +
  theme(legend.position = c(.95, .05), legend.justification = c("right", "bottom"), legend.box.just = "right", legend.margin = margin(6, 6, 6, 6) ) +
  labs(colour = "curriculum onset") + guides(fill=FALSE) +
  theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"),axis.ticks = element_line(colour="black")) 
ggsave("ppo_onset_reward.pdf", height=4, width=6)
ggsave("ppo_onset_reward.png", height=4, width=6)
# t.test(value ~ condition, data=subset(full_df, (condition=="curriculum" | condition=="rev curriculum") & step==6000000 & metric=="Environment/Cumulative Reward"))



dtb_df <- subset(full_df, (condition=="400K" | condition=="800K" | condition=="2M" | condition=="no curriculum" | condition=="200K") & metric=="Distance to base")
ggplot(dtb_df, aes(x=step ,y=value, color=condition)) +
  stat_summary(geom="ribbon", colour = NA, fun.min=function(x) mean(x) - (qnorm(0.975) * sd(x) / sqrt(length(x))), fun.max=function(x) mean(x) + (qnorm(0.975) * sd(x) / sqrt(length(x))), aes(fill=condition), alpha=0.3) +
  stat_summary(fun=mean, geom="line", lwd=0.8, aes(group=condition, color=condition)) +
  xlab("steps") +
  ylab("mean distance to optimal position") +
  scale_x_continuous(label = scientific_10) + # Log-scale
  theme_bw() +
  scale_fill_manual(values=c("#1F78B4","#FF7F00", "#6A3D9A", "#33A02C", "#E31A1C")) + scale_color_manual(values=c("#1F78B4","#FF7F00", "#6A3D9A", "#33A02C", "#E31A1C")) +   theme(text = element_text(size = 12, family = "ssPro")) +
  theme(legend.position = c(.95, .95), legend.justification = c("right", "top"), legend.box.just = "right", legend.margin = margin(6, 6, 6, 6) ) +
  labs(colour = "curriculum onset") + guides(fill=FALSE) +
  theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"),axis.ticks = element_line(colour="black")) 
ggsave("ppo_onset_dtb.pdf", height=4, width=6)
ggsave("ppo_onset_dtb.png", height=4, width=6)


distmoved_df <- subset(full_df, (condition=="400K" | condition=="800K" | condition=="2M" | condition=="no curriculum" | condition=="200K") & metric=="Distance moved")
ggplot(distmoved_df, aes(x=step ,y=value, color=condition)) +
  stat_summary(geom="ribbon", colour = NA, fun.min=function(x) mean(x) - (qnorm(0.975) * sd(x) / sqrt(length(x))), fun.max=function(x) mean(x) + (qnorm(0.975) * sd(x) / sqrt(length(x))), aes(fill=condition), alpha=0.3) +
  stat_summary(fun=mean, geom="line", lwd=0.8, aes(group=condition, color=condition)) +
  xlab("steps") +
  ylab("total distance moved") +
  scale_x_continuous(label = scientific_10) + # Log-scale
  theme_bw() +
  scale_fill_manual(values=c("#1F78B4","#FF7F00", "#6A3D9A", "#33A02C", "#E31A1C")) + scale_color_manual(values=c("#1F78B4","#FF7F00", "#6A3D9A", "#33A02C", "#E31A1C")) +   theme(text = element_text(size = 12, family = "ssPro")) +
  theme(legend.position = c(.95, .95), legend.justification = c("right", "top"), legend.box.just = "right", legend.margin = margin(6, 6, 6, 6) ) +
  labs(colour = "curriculum onset") + guides(fill=FALSE) +
  theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"),axis.ticks = element_line(colour="black")) 
ggsave("ppo_onset_distancemoved.pdf", height=4, width=6)
ggsave("ppo_onset_distancemoved.png", height=4, width=6)



RT_df <- subset(full_df, (condition=="400K" | condition=="800K" | condition=="2M" | condition=="no curriculum" | condition=="200K") & metric=="RT")
ggplot(RT_df, aes(x=step ,y=value, color=condition)) +
  stat_summary(geom="ribbon", colour = NA, fun.min=function(x) mean(x) - (qnorm(0.975) * sd(x) / sqrt(length(x))), fun.max=function(x) mean(x) + (qnorm(0.975) * sd(x) / sqrt(length(x))), aes(fill=condition), alpha=0.3) +
  stat_summary(fun=mean, geom="line", lwd=0.8, aes(group=condition, color=condition)) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + # Log-scale
  xlab("steps") +
  ylab("RT") +
  scale_x_continuous(label = scientific_10) + # Log-scale
  theme_bw() +
  scale_fill_manual(values=c("#1F78B4","#FF7F00", "#6A3D9A", "#33A02C", "#E31A1C")) + scale_color_manual(values=c("#1F78B4","#FF7F00", "#6A3D9A", "#33A02C", "#E31A1C")) +   theme(text = element_text(size = 12, family = "ssPro")) +
  theme(legend.position = c(.95, .95), legend.justification = c("right", "top"), legend.box.just = "right", legend.margin = margin(6, 6, 6, 6) ) +
  labs(colour = "curriculum onset") + guides(fill=FALSE) +
  theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"),axis.ticks = element_line(colour="black")) 
ggsave("ppo_onset_RT.pdf", height=4, width=6)
ggsave("ppo_onset_RT.png", height=4, width=6)







ggarrange(plot_dtb, plot_RT,
          # labels = c("A", "B"),
          ncol = 2, nrow = 1)
ggsave("SAC_penaltysize_multi.pdf", height=4, width=10)


