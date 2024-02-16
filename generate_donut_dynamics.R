require(tidyverse)
require(parallel)




set.seed(1)

sparsity <- .2
n_items_per_test <- 6
n_tests <- 30
n_items_in_pool <- round(n_items_per_test*n_tests/(1-sparsity))
# n_students <- 10000
n_students <- 5000
process_noise <- 0
typical_growth_in_radians <- .3


#2D trajectory that looks like a donut
#assigning students their abilities (theta 1 and theta 2)
students <- tibble(studentid = 1:n_students) %>%
  mutate(timeid = 1,
         theta1 = rnorm(n()),
         theta2 = rnorm(n())) %>%
  #filter(theta2 > 0) %>% 
  filter(sqrt(theta1^2 + theta2^2) > 1.5) %>%
  mutate(student_train_validate_test = case_when(
    studentid < max(studentid)*.6 ~ 0,
    studentid < max(studentid)*.8 ~ 1,
    TRUE ~ 2,
  ))
students %>%
  count(studentid)

if(n_tests > 1){
  for(i in 2:n_tests){
    students_tp1 <- students %>%
      filter(timeid == (i-1)) %>%
      #filter(studentid == 1) %>% 
      mutate(growth_angle = typical_growth_in_radians) %>% #rnorm(n(), mean=typical_growth_in_radians, sd=process_noise)) %>% 
      mutate(timeid = timeid+1,
             d = sqrt(theta1^2 + theta2^2),
             theta1 = (theta1 * cos(growth_angle) - theta2 * sin(growth_angle)),
             theta2 = (theta1 * sin(growth_angle) + theta2 * cos(growth_angle)),
             #no idea why this scaling is necessary 
             new_d = sqrt(theta1^2 + theta2^2),
             theta1 = theta1*(d/new_d),
             theta2 = theta2*(d/new_d)
      )
    print(students_tp1)
    students <- students %>%
      bind_rows(students_tp1)
  }
}

students %>% count(student_train_validate_test)

# responses %>%
#   filter(studentid == 6063)

# students %>%
#   filter(studentid < 4) %>%
#   ggplot() + 
#   geom_point(aes(theta1,theta2,color=timeid)) + 
#   facet_wrap(studentid ~ .)

items <- tibble(itemid=1:n_items_in_pool) %>%
  mutate(difficulty1 = rnorm(n()),
         difficulty2 = rnorm(n()),
         alignment = ifelse(runif(n()) > 0.5, 1, 2)) #TODO - change back


responses <- students %>%
  group_by(studentid,timeid) %>%
  cross_join(items) %>%
  slice_sample(n=n_items_per_test) %>%
  mutate(item_order=1:n()) %>% 
  ungroup() %>% 
  mutate(p_correct = ifelse(alignment == 1,
                            1/(1+exp(-1*(theta1-difficulty1))),
                            1/(1+exp(-1*(theta2-difficulty2)))),
         correct = 1*(runif(n()) < p_correct)) %>%
  arrange(studentid,timeid,item_order)

#different number of items for each test
# responses <- responses %>%
#   group_by(studentid,timeid) %>%
#   sample_n(sample(c(3, 5, 8, 12, 20, 25),1)) %>%
#   ungroup() 

responses %>%
  count(studentid,timeid) %>%
  ggplot() +
  geom_density(aes(n))

## Actual sparsity 

n_students <- responses %>%
  count(studentid) %>%
  nrow()
n_items <-responses %>%
  count(itemid) %>%
  nrow() 
pct_sparse <- 1-nrow(responses)/(n_students*n_items)
pct_sparse
n_students

responses %>%
  sample_n(1000) %>% 
  ggplot() + 
  geom_point(aes(p_correct,correct))


responses %>%
  filter(studentid < 20) %>% 
  ggplot() + 
  geom_point(aes(theta1,theta2,color=as.factor(timeid))) + 
    facet_wrap(studentid ~ .) + 
  scale_x_continuous(limits=c(-3,3)) + 
  scale_y_continuous(limits=c(-3,3))


responses %>%
  filter(studentid < 1000) %>% 
  ggplot() + 
  geom_point(aes(theta1,theta2)) + 
  facet_wrap(timeid ~ .) + 
  scale_x_continuous(limits=c(-5,5)) + 
  scale_y_continuous(limits=c(-5,5))

data_dir <- paste0(getwd(), "/data/simulated/donut_dynamics_jag")
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

responses_jagged <- responses %>%
    group_by(studentid, timeid) %>%
    # Require >= 2 responses per student
    sample_n(sample(2:n_items_per_test, 1), replace = FALSE) %>%
    mutate(
        student_input_query_z = runif(n()),
        student_input_query = as.integer(student_input_query_z == max(student_input_query_z))
    ) %>%
    ungroup() %>%
    select(-student_input_query_z)

n_responses <- responses_jagged %>% nrow()
n_students <- responses_jagged %>% distinct(studentid) %>% nrow()
n_items <- responses_jagged %>% distinct(itemid) %>% nrow()

# n_responses <- responses %>% nrow()
# n_students <- responses %>% distinct(studentid) %>% nrow()
# n_items <- responses %>% distinct(itemid) %>% nrow()


sparsity <- 1 - n_responses / (n_items * n_students)

responses_jagged %>%
    write_csv(paste0(data_dir, "/responses_df.csv"))

# responses %>% 
#   write_csv('~/git/deep-diagnostic-modeling-aws/data/simulated/donut_dynamics/responses_df.csv')



responses %>%
    distinct(studentid) %>%
    count()


#theoretical AUC w/out dynamics - this should be a CEILING
require(pROC)
auc(responses$correct, responses$p_correct)



# I'm curious to know what the AUC would/should be if a student answered just 2 questions
# aligned to the question they are trying to answer
# with a normal prior

theta_hat = seq(-6,6,by=0.01)
loglik = log(dnorm(theta_hat)) #normal prior

myfunc <- function(.studentid){
    
    #for each student, answer questions except the last one
    temp <- responses %>% filter(studentid == .studentid)
    validation <- temp %>% sample_n(1)
    training <- temp %>% 
      anti_join(validation,by=c('studentid','timeid','itemid'))
    
    #posterior = normalize(prior * prod(likelihoods))
    
    z <- tibble(theta_hat=theta_hat,
                loglik=loglik)
    
    #plot(z$theta_hat,exp(loglik))
    for(i in 1:nrow(training)){
      #add on log likelihood of the observation
      if(training$correct[i] == 1){
        z$loglik <- z$loglik + log(1/(1+exp(-1*(z$theta_hat - training$difficulty1[i]))))
      }else{
        z$loglik <- z$loglik + log(1 - 1/(1+exp(-1*(z$theta_hat - training$difficulty1[i]))))
      }
    }
    z$loglik_normalized <- z$loglik - max(z$loglik)
    #plot(z$theta_hat,exp(z$loglik_normalized))
    #theta_map <- z$theta_hat[which.max(z$loglik_normalized)]
    x <- z$theta_hat
    y <- exp(z$loglik_normalized)
    mean_est <- sum(x * y) / sum(y)
    var_est <- sum(y * (x - mean_est)^2) / sum(y)
    sd_est <- sqrt(var_est)
    
    #plot(z$theta_hat, dnorm(z$theta_hat, mean=mean_est, sd=sd_est))
    #now sample from the estimated theta
    theta_sample <- rnorm(100, mean_est, sd_est)
    #predictions for held-out item
    predictions <- 1/(1+exp(-(theta_sample-validation$difficulty1)))
    actual_response <- rep(validation$correct[1], 100)
    
    tibble(studentid = rep(.studentid,100),
           sample_theta = theta_sample,
           sampled_prediction = predictions,
           actual_response = actual_response,
           map_theta = rep(mean_est, 100),
           map_prediction = rep(1/(1+exp(-(mean_est-validation$difficulty1))),100),
           true_theta = rep(temp$theta1[1],100)) %>%
      return()
}

require(parallel)
res <- mcmapply(myfunc,
                .studentid = responses %>% count(studentid) %>% pull(studentid),
                SIMPLIFY = FALSE,
                mc.cores=1)
results <- data.table::rbindlist(res)
auc(results$actual_response, results$sampled_prediction)
auc(results$actual_response, results$map_prediction)

#say they got the first question wrong


