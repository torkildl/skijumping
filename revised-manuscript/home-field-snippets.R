
# HOME FIELD BIAS MODELS WITH 2016 DATA
# 
home16 <- season2016 %>% 
    select(competitorName, place, country, competitorNation, lenpts = lengthPoints) %>% 
    mutate(competitionplace = str_sub(place, start=1,end=-6)) %>% 
    mutate(athome = if_else(country==competitorNation,1,0))

# Model 5: Length model
model5b <- felm(data=home16, lenpts ~ athome + competitionplace | competitorName)
summary(model5b)

# Model 6: Style points model
model6b <- felm(data=scores16, judge ~ athome + competitionplace + judge_name + speed + length| competitorName)
summary(model6b)


## The extant literature on home field advantages

Home field advantage is another type of potential bias in ski jumping. Such advantages, where some competitors benefit from advantages from competing on the home field, may arise in many different ways. Several of the mechanisms that may produce home field advantages are pscyhological sources of superior performances, while others involve judges overrating home field competitors. This point illustrates that the mere presence of a biases is not necessarily illegitimate. In order to improve scoring and fairness in sports, one must understand the mechanisms behind them. 

Some research into the home-field advantage has attempted to move our understanding of the causal mechanisms behind  the advantages. Carmichael and Thomas [-@carmichael_homefield_2005] outline the four most common explanations of this phenomena using soccer as an example. The first is a familiarity explanation – the home team is more familiar with the field, the stadium, the locker room, and so forth, which puts the players more at ease in the situation, allowing them to concentrate on performing at their best. Second, traveling to another field could result in fatigue, invoke various stressors, and cause disruptions in pre-competition routines for the away team. All these factors may lead to inferior performances by the away team, relative to their likely performance at the home field. Third, specific rules may favour the home team, giving them the upper hand. Last, there could be a crowd effect. The size of the crowd at the home field might improve individual or team performance. It might also make the judge to be more lenient for the home team, through psychological mechanisms.

Page and Page [-@page_evidence_2010] attributes some of the crowd effect to crowd noise, noting that the judge is under both time pressure (making a correct decision in a short time) and social pressure (from the crowd). A study by Nevill et al. [-@nevill_influence_2002] showed that crowd noise was related to the home-field advantage. Setting up an experiment with one group of judges watching a football match with sound, and another group without, they found that the group watching with sound called 15.5% fewer fouls against the home team than the group who watched in silence. This could also indicate that judges use crowd noise as a guideline in their judging work. The crowd effect may also take another direction, as Anders and Rotthoff [-@anders_homefield_2014] find that football clubs with notoriously violent fans have a larger home-field advantage than peaceful ones. Here, the theory is that the prospect of fans-to-judge violence causes a more lenient judge.

Research on the home-field advantage has also been done in various winter sports. Balmer et al. [-@balmer_home_2001] analysed home-field advantages the winter Olympics from 1908 to 1998, and found some evidence for it in figure skating, freestyle skiing, ski jumping, alpine skiing and short track speed skating. The authors examined the relevance of three of the explanations listed above (familiarity with local conditions, time zones and crowd effect), and found evidence for home condition familiarity.  Conditional on the time zones travelled, travel time as such did not matter. Most evidence, however, was found for the crowd effect theory, as sports that were judged only subjectively had a bigger home-field advantage than sports in which assessments were based on both subjective and objective criteria.


## Hypotheses

For home field advantages, we propose an additional hypothesis that home field advantages are measurable and impacts athletes performances. Finally, we distinguish between the social aspects of competing on the home field and the physical aspects of competing on the home field, and hypothesize that the physical home field advantage is less salient than the social home field advantage, and thus less important for performances.


## Estimation of home field advantage

To analyse home field advantage we need to separate the problem of bias into the physical and the social domains. That is, whether an athlete actually performs better on the home field or whether he gets better scores by the judges due to social pressure or other factors. Thanks to the natural separation of style scores and length scores in ski jumping, this can be uncovered using similar models.

In order to observe a home field effect in points, be it length points or the judges’ style points, we have to compare scores on a home field with scores on away fields – leading to the assumption that a jumper is somewhat consistent in performance throughout a season. Previously, we used the four other judges as a contrast measure of performance quality. For home field advantage we have to assume that an athlete maintains a relatively constant quality of jumping throughout the season, using his away jumps as a guide to his general performance level. This can be a bold assumption, given that local weather conditions and individual athlete’s “daily form” also may affect these scores, as well as the tiny margins of timing required to “push off” at the right moment. Another problem is that countries with a strong tradition for ski jumping will have both stronger jumpers, host competitions more often due to existing infrastructure, and have more judges in the judge panel.

Again, we construct a dummy-variable using a function, $ω(I=H)$, which will take the value 1 if the jump takes place in country I, the athlete’s home country, and H being the competition country. We then wish to estimate the following model:
    
    \begin{equation}
s_{irs}' = \~B' ∙ω(I=H) + a_{i} +λ_{r} +\epsilon_{ir}
\end{equation}

where $s_{irs}$ are the length points of the athlete, in a specific season, in a specific competition. $\~B\'$

is the estimated home field effect in points and $a_i$ is is a fixed effect for the athlete-season combinations. The fixed effect also removes any effect of individual jumpers that “strong jumpers” from traditionally strong ski jumping nations by out estimating each athlete, in each season, separately. Lastly, taking into account the different sizes of the hills, which may result in a skewness that the K-point system fails to adjust for, we add a fixed effect for the hill represented by $λ_r$.

For the social part of the home field advantage we wish to estimate a similar model where style points awarded by the judges serves as the dependent variable.

\begin{equation}
s_{ijprs} = \~B ∙ ω(I=H) + a_{is} + λ_r + l_j + βx_p + \epsilon_{ijprs}
\end{equation}

In this model, we add fixed effects for judges as in (3) to capture leniency of individual judges. We cannot use jump fixed-effects to capture the inherent objective quality of the jump, because home advantage is invariant within a jump, but we can include two central jump characteristics in our data, namely length and speed. This is because “all else being equal, a ski jumper with the same take-off speed but better form while airborne will travel further” [@zitzewitz_nationalism_2006, 78]. We therefore add these two variables to the model, where they and their coefficients are represented by the $βx_p$.



### Home field advantages

Table 3 presents the results for the two models, where we included effects of the hypothesized home field advantage. Following the distinction between the physical and social dimensions of such advantages, we specified one model for the actual length of the jump and another for style scores. The first column of Table 3 show the parameter estimates for the length model (Model 5). This model includes competitor-season-fixed effects, which implies that variations among different athletes and their form is taken into account and the estimate for differential jump lengths for home field competitors are purged of those influences. Our main interest lies with the dummy variable for the athlete performing in his home country. The parameter estimate for this dummy variable is not significantly different from zero. Athletes thus do not seem to perform better in their home countries, than when competing in other countries. The physical dimension of home field advantage is thus undetectable in our data.


\begin{landscape}
```{r tab3, echo=FALSE, results='asis'}
stargazer(model5, model6, out.header=FALSE, header=FALSE,
single.row=TRUE,  omit = c("judge","competitionplace"), 
column.labels = c("Model 5: Length points", "Model 6: Style points"),
title="Results from regression models of home field advantage", 
covariate.labels = c("Home field"),
dep.var.caption  = "", dep.var.labels.include = FALSE,
model.numbers = FALSE, model.names = FALSE,
add.lines = c(" "),
notes.align="l",
notes = c("Both models include athlete and jumping hill fixed effects, and model 6 also include judge fixed-effects."),
notes.append=TRUE,
df = TRUE, decimal.mark =".",
ci=TRUE, ci.level=0.95, ci.separator = "  ",
omit.stat=NULL,
star.cutoffs = c(0.05,0.01,0.001)
)
```
\end{landscape}

We examine the social dimension of home field advantage in Model 6, where we include a parameter for bias in judge's scores if the athlete is competing in his or her home country. If there is home field advantage, this parameter should be measurably larger than zero. The estimate of this parameter is statistically significant, and larger than zero, suggesting that there is a social dimension of home field advantage in ski jumping. The effect is small and roughly of the same magnitude (0.074) as for nationalistic bias. The bias only amounts to a quarter of a standard deviation within each jump and it is well below the minimum increment for judge scores of 0.5 point. It seems unlikely that home field advantages is a serious threat to the validity of results of any competition.



We conducted separate analyses for home-field bias in performance (as indicated by the length of the jumps) and in judges’ evaluations of the stylistic elements in each jump. The results showed no effects on the jumpers’ actual performance, implying that the jumpers did not benefit from familiarity with the hill or from home-audience support in terms of jump length. Instead, home-field bias showed up in the judges evaluations of the jumps, even though the effect was here again of very small magnitude. Whether the bias stemmed from “crowd-pleasing”—which functions as a conflicting motivation to objectivity in the judges’ performance—or some other bias-generating mechanism is beyond the scope of this study.
