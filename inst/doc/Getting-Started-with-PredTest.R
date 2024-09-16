## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = TRUE, warning = FALSE,
  message = FALSE
)

## ----setup--------------------------------------------------------------------

library(PredTest)

# Cognitive data set from the paclage
data("group_cog_data")

# Fitness data set from the package
data("pre_post_fit")

library(kableExtra)
library(ggplot2)


## ----table1, echo = FALSE-----------------------------------------------------
# end points by study type (subsets of column variables of interest to a researcher)

grp_endpts <- c(
    "mean_suv","blind_moca_uncorrected","craft_verbatim","craft_delay_verbatim",
    "number_span_forward","number_span_backward","fluency_f_words_correct",
    "oral_trail_part_a","oral_trail_part_b","fluency_animals","fluency_vegetables",
    "verbal_naming_no_cue"
)

prepost_endpts <- c(
    "COPM_p", "COPM_s", "A1_work", "A2_work", "Grip_dom",
    "Grip_ndom",  "Flex_right", "Flex_left"
)

# hypotheses for each data set 

cog_hyps <- c("increase", "decrease", "decrease", "decrease", "decrease", "decrease", "decrease", "increase", "increase",
              "decrease", "decrease", "decrease")

fit_cog_hyps<- c("increase", "increase", "increase", "decrease", "increase", "increase", "increase", "increase")

# display of each hypothesis for each variable
tab1 <- data.frame(grp_endpts, cog_hyps)
colnames(tab1) <- c("Variable", "Hypothesized result")
knitr::kable(tab1, align = "ll") %>%   kable_styling(full_width = F)


## ----setup2, echo = TRUE------------------------------------------------------

# Endpoints for the cognitive example
grp_endpts <- c(
    "mean_suv","blind_moca_uncorrected","craft_verbatim","craft_delay_verbatim",
    "number_span_forward","number_span_backward","fluency_f_words_correct",
    "oral_trail_part_a","oral_trail_part_b","fluency_animals","fluency_vegetables",
    "verbal_naming_no_cue"
)


# Specifying predictions for the cognitive data example
cog_hyps <- c("increase", "decrease", "decrease", "decrease", 
              "decrease", "decrease", "decrease", "increase", "increase",
              "decrease", "decrease", "decrease")


# To get the results we pass the appropriate function values
group_results <- pred_results(dataset=group_cog_data, hypothesis=cog_hyps, 
                            vars=grp_endpts, type="group", gtvar="group.factor", 
                            grp_a="Control", grp_b ="ESKD", location="mean")

group_results


## ----weightsEx, echo = TRUE---------------------------------------------------

group_weights <- pred_weights(dataset=group_cog_data, 
                              vars=grp_endpts,gtvar="group.factor", 
                              type="group",corr_method="pearson")

group_weights



## ----testEx, echo=TRUE--------------------------------------------------------

pred_test(weights_vector = group_weights, 
          results_vector = group_results$results,
          test_type = "exact",
          phi_0 = 0.5)



## ----plot,fig.height = 4, fig.width = 6, fig.align = "center", echo = TRUE----

    end <- grp_endpts
    diff <- group_results$differences
    outcome <- group_results$results

    forplot <- as.data.frame(   cbind(end, diff  , outcome  )   )
    forplot$diff <- as.numeric(as.character(forplot$diff))
    forplot$outcome <- as.numeric(as.character(forplot$outcome))

    ymax <- max(1.25*max(forplot$diff), 0+.5*sd(forplot$diff))
    ymin <- min(1.25*min(forplot$diff), 0-.5*sd(forplot$diff))

   ggplot(data = forplot, aes(x=reorder(end, -diff), y=diff, fill = factor(outcome)   )  ) +
        geom_bar(stat='identity') +
        scale_y_continuous(limits=c(ymin,ymax)) +
        geom_bar(forplot, mapping = aes(end) ,alpha=0, size=1, color="black", stat='identity')+
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        labs( x = "", y = "Difference", fill = "Prediction\nresults") +
        scale_fill_manual(values = c('0' = "White",'1'= "Black"), 
                          labels = c("Incorrect", "Correct"), drop = FALSE) +
        theme(plot.title = element_text(hjust = 0.5))

## ----echo = TRUE--------------------------------------------------------------

# Endpoints for the fitness example
prepost_endpts <- c(
    "COPM_p", "COPM_s", "A1_work", "A2_work", "Grip_dom",
    "Grip_ndom",  "Flex_right", "Flex_left"
)


# Specifying predictions for the fitness data set 
fit_cog_hyps<- c("increase", "increase", "increase",
                 "decrease", "increase", "increase", "increase", "increase")



pre_post_fit <- pre_post_fit[complete.cases(pre_post_fit),]
pre_post_results <- pred_results(dataset = pre_post_fit,
                        id = "ID", hypothesis = fit_cog_hyps,
                        vars = prepost_endpts, type = "prepost",
                        gtvar = "Time", grp_a = 0,
                        grp_b = 1, location = "mean")

pre_post_results

## -----------------------------------------------------------------------------
pre_post_weights <- pred_weights(dataset = pre_post_fit,
                        id = "ID",
                        vars = prepost_endpts,
                        gtvar =  "Time", 
                        type = "prepost",
                        pre = 0,
                        post = 1,
                        corr_method = "pearson")
pre_post_weights

## ----testEx2, echo=TRUE-------------------------------------------------------

pred_test(weights_vector = pre_post_weights, 
          results_vector = pre_post_results$results,
          test_type = "exact")

## ----age, echo=FALSE----------------------------------------------------------
plot(group_cog_data$blind_moca_uncorrected~group_cog_data$age,
     xlab = "Age", ylab = "MoCA")
boxplot(group_cog_data$age~group_cog_data$group.factor,
        xlab = "Group", ylab = "Age")



## ----adjusted, echo=TRUE------------------------------------------------------

adjusted <- pred_adjusted(dataset = group_cog_data,
                          hypothesis = cog_hyps,
              vars = grp_endpts,
              covariates = c("age"),
              group = "group.factor", ref = "Control")

pred_test(results_vector = adjusted$results,
          weights_vector = adjusted$weights,
          test_type = "exact")



