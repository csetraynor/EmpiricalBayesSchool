.libPaths("C:/RFolder/R-3.4.2/library")
library(Lahman)
library(dplyr)
library(tidyr)
theme_set(theme_bw())


###########get ebbr package
#library(devtools)
#library(githubinstall)
#githubinstall("ebbr")
library(ebbr)


#Grab career batting average of non-pitchers
# (allow players that have pitched <= 3 games, like Ty Cobb)
pitchers <- Pitching %>%
  group_by(playerID) %>%
  summarize(gamesPitched = sum(G)) %>%
  filter(gamesPitched > 3)

# Add player names
player_names <- Master %>%
  tbl_df() %>%
  dplyr::select(playerID, nameFirst, nameLast, bats) %>%
  unite(name, nameFirst, nameLast, sep = " ")

# include the "bats" (handedness) and "year" column for later
career_full <- Batting %>%
  filter(AB > 0) %>%
  anti_join(pitchers, by = "playerID") %>%
  group_by(playerID) %>%
  summarize(H = sum(H), AB = sum(AB), year = mean(yearID)) %>%
  inner_join(player_names, by = "playerID") %>%
  filter(!is.na(bats))

# We don't need all this data for every step
career <- career_full %>%
  select(-bats, -year)

#The distribution of player batting averages look like a beta distribution
require(ggplot2)
library(scales)
career%>%
  filter(AB >= 100) %>% #filter those players that have low at bat register
  ggplot(aes(H / AB)) +
  geom_histogram()

#1- Estimate the prior
require(ebbr)

prior <- career %>%
  filter(AB >= 500) %>%
  ebb_fit_prior(H, AB) #it fits beta through ML, takes the data with the succes/total
prior

#2- Update each observation based on the overall stat model
augment(prior, data = career)

# Notice we’ve now added several columns to the original data, each beginning with . (which is a convention of the augment verb to avoid rewriting e. We have the .alpha1 and .beta1 columns as the parameters for each player’s posterior distribution, as well as .fitted representing the new posterior mean (the “shrunken average”).

# run these two steps in sequence: estimating a model, then using it as a prior for each observation. 
ebb_career <- career %>%
  add_ebb_estimate(H, AB, prior_subset = AB >= 500) # we want to fit the prior only on individuals with at least 500 at-bats.

ebb_career

#explore the model results

ebb_career %>%
  ggplot(aes(.raw, .fitted, color =AB))+
  geom_point()+
  geom_abline(color = "red")+
  scale_color_continuous(trans = "log", breaks = c(1, 10, 100, 1000))+
  geom_hline(yintercept = tidy(prior)$mean, color = "red", lty =2)+
  labs(x = "Raw batting average",
       y = "Shrunken batting avergae")
#This visualisation shows what empirical bayes estimation is doing: moves all averges towards the prior mean

#Credible intervals
yanke_1998 <- c("brosisc10", "jeterde01", "knoblch01",
                "martiti02", "psadjo01", "strawda01", "willibe02")

ebb_career %>%
  filter(playerID %in% yanke_1998) %>%
  mutate(name = reorder(name, .fitted)) %>%
  ggplot(aes(.fitted, name))+
  geom_point(size = 20, shape = 16 )+
  geom_errorbarh(aes(xmin = .low, xmax = .high))+
  labs(x = "Estimated batting average (w/ 95% confidence interval",
       y = "Player")

#Hierarchical modeling

career %>%
  filter(AB >= 10)%>%
  ggplot(aes(AB, H/AB)) + 
  geom_point() + 
  geom_smooth(method ="lm")+
  scale_x_log10()
#Batter with low AB have low amount of data available

#fit a prior that depend on AB
eb_career_ab <- career %>%
  add_ebb_estimate(H, AB, method ="gamlss",   #gamlss fit a prior dependend on AB
                   mu_predictors = ~ log10(AB))
eb_career_ab
# The augmented output is now a bit more complicated: besides the posterior parameters such as .alpha1, .beta1, and .fitted, each observation also has its own prior parameters .alpha0 and .beta0. These are predicted based on the regression on AB.

eb_career_ab %>%
  filter(AB > 10) %>%
  rename(Raw =.raw, Shrunken = .fitted) %>%
  gather(type, estimate, Raw, Shrunken) %>%
  ggplot(aes(AB, estimate)) + 
  geom_point()+
  facet_wrap( ~ type)+
  scale_x_log10() #we can see that now tshrinken towrds the trend rather than towards a constant

#The model can incorporate more useful information for example the year
library(splines)

eb_career_prior <- career_full %>%
  ebb_fit_prior(H, AB, method = "gamlss",
                mu_predictors = ~ 0 + ns(year, df = 5) * bats + log(AB))
