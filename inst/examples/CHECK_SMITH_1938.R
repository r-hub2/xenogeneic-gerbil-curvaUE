library(curvaUE)
configurations <- plot_configurations(example_trial("rice"))
smith <- fit_smith(configurations)
reference <- lm(log(unit_variance) ~ log(area),
                data=configurations,weights=n_plots-1)
stopifnot(isTRUE(all.equal(unname(coef(smith$model)),
                           unname(coef(reference)))))
print(smith$b_prime)
