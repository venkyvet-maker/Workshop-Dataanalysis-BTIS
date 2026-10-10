2 + 3
answer <- 2 + 3
answer
name <- "R workshop"
score <- c(12, 15, 18, 11, 20)
length(score)
mean(score)
sd(score)
summary(score)
women_weight <- c(38.9, 61.2, 73.3, 21.8, 63.4, 64.6, 48.4, 48.8, 48.5)
men_weight <- c(67.8, 60, 63.4, 76, 89.4, 73.3, 67.3, 61.3, 62.4)
weights <- data.frame(
  group = rep(c("Woman", "Man"), each = 9),
  weight = c(women_weight, men_weight)
)
head(weights)

practice <- data.frame(
  id = 1:12,
  group = rep(c("Control", "Intervention"), each = 6),
  age = c(22, 25, 31, 28, 35, 29, 24, 27, 33, 30, 38, 26),
  baseline = c(18, 20, 17, 22, 19, 21, 20, 19, 23, 21, 18, 22),
  followup = c(19, 21, 18, 23, 20, 22, 23, 23, 25, 24, 22, 24),
  outcome = c("No", "No", "Yes", "No", "No", "Yes",
              "No", "Yes", "No", "No", "Yes", "No")
)
write.csv(practice, "practice_data.csv", row.names = FALSE)
practice <- read.csv("practice_data.csv")
head(practice)
str(practice)
summary(practice)
names(practice)
practice$group <- factor(practice$group)
practice$outcome <- factor(practice$outcome, levels = c("No", "Yes"))
class(practice$group)
levels(practice$group)
table(practice$outcome)
anyNA(practice)
practice[2, ]
practice[, c("id", "group", "baseline")]
practice$change <- practice$followup - practice$baseline
practice$change_plus_one <- practice$change + 1
practice[2, "age"] <- 26  # demonstration only: edit a known row
write.csv(practice, "practice_clean.csv", row.names = FALSE)
library(dplyr)
practice %>% select(id, group, age, change)
practice %>% filter(age >= 30)
practice %>% filter(group == "Intervention", change >= 2)
practice %>% mutate(change = followup - baseline) %>%
  arrange(desc(change))
group_summary <- practice %>%
  group_by(group) %>%
  summarise(
    n = n(),
    mean_baseline = mean(baseline, na.rm = TRUE),
    mean_followup = mean(followup, na.rm = TRUE),
    mean_change = mean(followup - baseline, na.rm = TRUE),
    .groups = "drop"
  )
group_summary
mean(practice$baseline)
median(practice$baseline)
sd(practice$baseline)
range(practice$baseline)
table(practice$group)
practice %>% group_by(group) %>%
  summarise(n = n(), mean_change = mean(change), sd_change = sd(change), .groups = "drop")
library(ggplot2)
ggplot(practice, aes(x = group, y = change, fill = group)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.08, height = 0, alpha = 0.8) +
  labs(title = "Change by group", x = "Study group", y = "Follow-up minus baseline") +
  theme_minimal() +
  theme(legend.position = "none")
ggplot(practice, aes(x = baseline, y = followup, colour = group)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Baseline and follow-up", x = "Baseline score", y = "Follow-up score") +
  theme_classic()
t.test(change ~ group, data = practice)
# Welch two-sample t-test is the default in R.
# Do not set var.equal = TRUE unless justified.
tab <- table(practice$group, practice$outcome)
tab
chisq.test(tab)
fisher.test(tab)
fit_aov <- aov(change ~ group, data = practice)
summary(fit_aov)
fit_lm <- lm(followup ~ baseline + age + group, data = practice)
summary(fit_lm)
confint(fit_lm)
broom::tidy(fit_lm, conf.int = TRUE)


fit_logit <- glm(outcome ~ age + group, data = practice, family = binomial)
summary(fit_logit)
exp(coef(fit_logit))
#Save results to a file
write.csv(group_summary, "group_summary.csv", row.names = FALSE)
sink("model_output.txt")
print(summary(fit_lm))
sink()






