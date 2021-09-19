pnorm(-0.355, lower.tail = FALSE)

power.t.test(n = 16, delta = 2 / 4, sd=1, type = "one.sample",  alt = "one.sided")$power

power.t.test(n = 16, delta = 2, sd=4, type = "one.sample",  alt = "one.sided")$power

power.t.test(n = 16, delta = 100, sd=200, type = "one.sample", alt = "one.sided")$power

power.t.test(power = .8, delta = 2 / 4, sd=1, type = "one.sample",  alt = "one.sided")$n

power.t.test(power = .8, delta = 2, sd=4, type = "one.sample",  alt = "one.sided")$n

power.t.test(power = .8, delta = 100, sd=200, type = "one.sample", alt = "one.sided")$n