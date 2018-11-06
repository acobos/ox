# S3 method for ox objects

summary.ox <- function (ox_obj) {

  list(numbers = c(datapoints = nrow(d$data),
                   subjects = length(unique(d$data$subject_key)),
                   sites = length(unique(d$data$study_oid)),
                   events = length(unique(d$data$event_oid)),
                   forms = length(unique(d$data$form_oid)),
                   groups = length(unique(d$data$group_oid)),
                   items = length(unique(d$data$item_oid))),

       events = unique(d$data$event_oid),
       forms = unique(d$data$form_oid),
       groups = unique(d$data$group_oid),
       assessments = with(d$data, table(group_oid, event_oid))
  )
}


