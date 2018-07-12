#GLM with some of the "greatest hits" variables, including all two-way interactions
full <- glm(snag ~ (stratum + NEAR_TERR_DIST + NEAR_TERR_CLASS_31 + pct_prm_wetf + riprap.p + NEAR_FOREST_CLASS_31 + depth.p + pct_terr + riprap.p)^2, data = sites_aa_5m, family = "binomial")
summary(full)

#ok don't do this, this is insane.
step <- step(glm(snag~(stratum + NEAR_TERR_DIST + NEAR_TERR_CLASS_31 + pct_prm_wetf + riprap.p + NEAR_FOREST_CLASS_31 + depth.p + pct_terr + riprap.p)^2, data = sites_aa_5m, family = "binomial"), direction="backward")

#Making a mixed-effects model with `uniq_id` as a random effect variable
mixed.ef <- glmer(snag ~)