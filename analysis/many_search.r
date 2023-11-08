source("~/lib/base.r")
df = read_csv('../model/results/summary_apr26.csv')

# %% --------

df %>% 
    filter(
        prior_bias == 0 &
        # σ_obs == 0 &
        sample_cost == 0 &
        switch_cost == 0 &
        1
    ) %>% ggplot(aes(σ_obs, last_seen_chosen, color=factor(n_arm))) + geom_line()

fig("tmp.png", 6)

min(df$last_seen_chosen)
max(df$best_seen_chosen)

# %% --------


plot_thomas = function(x, y) {
    ggplot(df9, aes({{x}}, {{y}}, group=n_item, color=n_item)) + 
    # geom_violin() + 
    stat_summary(fun.data=mean_se) + 
    theme(legend.position = "none") + mpl_colors
}

# %% --------
plot_thomas(n_item, prop_seen) + ylim(0, 1) + ylab("% of items seen")
# plot_thomas(filter(df, as.numeric(n_item) > 4), aes(n_item, prop_seen)) + ylim(0, 1) + ylab("% of items seen")
fig("many/thomas-4a")

# %% --------
plot_thomas(n_item, rt / 1000) + ylim(0, 14) + ylab("mean rt (s)")
fig("many/thomas-4b")

# %% --------
plot_thomas(n_item, as.numeric(best_seen_chosen)) + ylim(0, 1) + ylab("p(best seen item chosen)")
fig("many/thomas-4c")

# %% --------
plot_thomas(n_item, as.numeric(last_seen_chosen)) + ylim(0, 1) + ylab("p(last seen item chosen")
fig("many/thomas-4d")

# %% --------
ggplot(df4, aes(n_fix_since_best_last_seen, as.numeric(best_seen_chosen))) + 
    stat_summary(geom='bar') +
    xlim(-0.5, 4) +
    ylab("p(best seen item chosen")
fig("many/reutskaja-3b")

# %% --------
ggplot(filter(df4, n_refix <= 5), aes(n_refix, ..prop..)) + 
    geom_bar() +
    facet_wrap(~ n_item) +
    ylab("proportion of trials")
fig("many/reutskaja-3c")



# %% ==================== Fixations ====================

df = read_csv('results/many_options/sim-fix.csv') %>% 
    filter(n_item < 25) %>% 
    mutate(n_item = as.factor(n_item))

# %% --------
df %>% 
    filter(is_initial == 1 & number <= 8) %>% 
    ggplot(aes(number, is_last_initial, color=n_item)) + 
        stat_summary(geom="line") + ylim(0,1) + mpl_colors

fig("many/reutskaja-4a")

# %% --------

df %>% 
    filter(is_initial == 1) %>% 
    ggplot(aes(current_value, is_last_initial, color=n_item)) + 
        stat_summary_bin(geom="line") + ylim(0,1) + mpl_colors + xlim(-3, 3)

fig("many/reutskaja-4b")

# %% --------

df %>% 
    filter(is_initial == 1 & number <= 8) %>% 
    ggplot(aes(cached_value, is_last_initial, color=n_item)) + 
        stat_summary_bin(geom="line") + ylim(0,1) + mpl_colors + xlim(-3, 3)

fig("many/reutskaja-4c")

