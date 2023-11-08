source("~/lib/base.r")
model = read_csv('../model/results/summary_may10.csv') %>% 
    rename(setsize = n_arm) %>% 
    mutate(setsize = factor(setsize))

human = unique(model$setsize) %>%
    map(~ read_csv(glue('../data/thomas-elife/summary_files/setsize-{.x}_desc-data.csv'), col_types=cols())) %>%
    bind_rows %>% 
    select(-starts_with(c("stimulus", "gaze_onset", "cumulative_gaze", "stimulus", "item_value")))

# %% --------
gaze_path = '../data/thomas-elife/subject_files/sub-{.x}_setsize-{.y}_desc-gazes.csv'
gaze_cols = cols(
  subject = col_double(),
  setsize = col_double(),
  trial = col_double(),
  item = col_double(),
  dur = col_double(),
  onset = col_double(),
  stimulus = col_character(),
  gaze_num = col_double(),
  is_returning = col_double(),
  returning_gaze_count = col_double(),
  is_last = col_double(),
  is_first = col_double(),
  item_value = col_double(),
  choice = col_double(),
  is_last_to_choice = col_double()
)
gaze = human %>%
    select(subject, setsize) %>% 
    distinct %>% 
    with(., map2(subject, setsize, ~read_csv(glue(gaze_path), col_types=gaze_cols))) %>% 
    bind_rows()

gaze %>% 
    filter(gaze_num == 1) %>% 
    select(subject, setsize, trial, is_last_to_choice) %>% 
    right_join(human) -> human

gaze %>% 
    group_by(subject, setsize, trial) %>%
    summarise(mean_fixation_duration=mean(dur)) %>% 
    right_join(human) -> human

human = human %>% rename(
    last_seen_chosen = is_last_to_choice,
) %>% mutate(
    prop_seen = seen_items_count / setsize,
    setsize = factor(setsize)
)

# %% --------
rating_paths = "../data/thomas-elife/subject_files/sub-{.x}_desc-liking_ratings.csv"
rating_cols = cols(
  subject = col_double(),
  trial = col_double(),
  rt = col_double(),
  stimulus = col_character(),
  rating = col_double()
)
ratings = human %>%
    with(unique(subject)) %>% 
    map(., ~ read_csv(glue(rating_paths), col_types=rating_cols)) %>% 
    bind_rows() %>% 
    select(-`unnamed: 0`)
    
# %% --------

agg_human = human %>% 
    group_by(subject, setsize) %>% 
    summarise(across(c(last_seen_chosen, best_seen_chosen, rt, prop_seen, mean_fixation_duration), mean)) %>% 
    group_by(setsize) %>% 
    summarise(across(c(last_seen_chosen, best_seen_chosen, rt, prop_seen, mean_fixation_duration), mean))

# model = model %>% 
#     group_by(σ_obs, switch_cost, sample_cost, prior_bias) %>% 
#     mutate(id = cur_group_id())
# %% --------
loss_fn = function(x) {
    sum(((x$last_seen_chosen - agg_human$last_seen_chosen)) ^ 2) + 
    sum(((x$best_seen_chosen - agg_human$best_seen_chosen)) ^ 2) + 
    sum(((x$rt - agg_human$rt) / 14000) ^ 2) + 
    sum(((x$prop_seen - agg_human$prop_seen)) ^ 2) 
    # sum(((x$mean_fixation_duration - agg_human$mean_fixation_duration) / 400) ^ 2)
}

loss_table = function(x) {
    tibble(
        last_seen_chosen=sum(((x$last_seen_chosen - agg_human$last_seen_chosen)) ^ 2),
        best_seen_chosen=sum(((x$best_seen_chosen - agg_human$best_seen_chosen)) ^ 2),
        rt=sum(((x$rt - agg_human$rt) / 14000) ^ 2),
        prop_seen=sum(((x$prop_seen - agg_human$prop_seen)) ^ 2)
        # mean_fixation_duration=sum(((x$mean_fixation_duration - agg_human$mean_fixation_duration) / 400) ^ 2)
    )
}

alt_loss = function(x) {
    sum(((x$last_seen_chosen - agg_human$last_seen_chosen)) ^ 2)
}

loss = model %>%
    group_by(id) %>%
    nest() %>%
    mutate(loss = map_dbl(data, loss_fn)) %>% 
    select(-data) %>% 
    arrange(loss)

best_id = loss$id[1]

model %>% filter(id==best_id) %>% loss_table

model %>% filter(id==best_id)

# %% --------

# gaze %>% ggplot(aes(dur)) + geom_density() + xlim(0, 1000)
gaze %>% filter(dur < 1000) %>% ggplot(aes(dur)) + geom_histogram()
fig()
gaze %>% with(mean(dur < 100))

# %% ==================== Plots ====================

best_model = model %>% filter(id == best_id)
best_model$agent = "model"
agg_human$agent = "human"
keep = c("last_seen_chosen", "best_seen_chosen", "rt", "prop_seen", "mean_fixation_duration", "agent", "setsize")
both = bind_rows(
    agg_human %>% select(all_of(keep)),
    best_model %>% ungroup %>% select(all_of(keep)),
)

plot_thomas = function(y) {
    ggplot(both, aes(setsize, {{y}}, group=agent, color=agent, linetype=agent)) + 
    geom_line() +
    theme() + 
    theme(legend.position = "none") +
    scale_colour_manual(values=c(human="#222222", model="#E53E45"), 
                        aesthetics=c("fill", "colour"))
}

# %% --------
p1 = plot_thomas(prop_seen) + ylim(0, 1) + ylab("% of items seen")
fig("thomas-4a")

p2 = plot_thomas(rt / 1000) + ylim(0, 14) + ylab("mean rt (s)")
fig("thomas-4b")

p3 = plot_thomas(as.numeric(best_seen_chosen)) + ylim(0, 1) + ylab("p(best seen item chosen)")
fig("thomas-4c")

p4 = plot_thomas(as.numeric(last_seen_chosen)) + ylim(0, 1) + ylab("p(last seen item chosen")
fig("thomas-4d")


# %% --------
(p1 + p2 + p3 + p4) + plot_annotation(tag_levels='A')

fig("thomas-fig4", 8, 8)

# %% ==================== Fixations ====================

mfix = read_csv('../model/results/many_options/sim-fix.csv') %>% 
    filter(n_item == 9) %>% 
    mutate(
        agent = "model",
        subject=1,
        trial = cumsum(number == 1)
    )

norm_rating = ratings %>% 
    group_by(subject) %>% 
    mutate(value = scale(rating)) %>% 
    select(subject, stimulus, value)

hfix = gaze %>% 
    filter(setsize == 9) %>% 
    inner_join(norm_rating) %>% 
    transmute(
        subject,
        trial,
        number = gaze_num,
        n_item = setsize,
        duration = dur,
        is_chosen = item == choice,
        is_final = is_last,
        current_value=value,
        is_initial = !is_returning,
        agent = "human"
    )

df = bind_rows(hfix, mfix) %>% 
    mutate(
        n_item = as.factor(n_item),
        type = case_when(
            as.logical(is_final & is_chosen) ~ "last to choice",
            as.logical(is_final) ~ "last to other",
            as.logical(is_initial) ~ "initial",
            T ~ "returning"
        ) %>% fct_relevel("initial", "returning", "last to choice", "last to other")
    ) %>% 
    group_by(agent, subject, trial) %>% 
    mutate(normalized_onset = cumsum(duration) / sum(duration))

# %% --------
red_black = scale_colour_manual(
    values=c(human="#222222", model="#E53E45"), 
    aesthetics=c("fill", "colour")
)

p2e = X %>% 
    ggplot(aes(current_value, duration, group=agent, color=agent)) +
    stat_summary_bin(fun.data=mean_se, breaks=seq(-3.5, 3.5)) +
    # stat_summary_bin(fun=mean, bins=6, geom="line") +
    # coord_cartesian(ylim=c(0,400)) +
    red_black +
    theme(legend.position = "none") +
    labs(x="normalized liking rating")

fig("thomas-2e")

# %% --------
p2i = df %>%
    ggplot(aes(type, duration, color=agent)) +
    stat_summary(fun.data=mean_se) +
    theme(axis.text.x = element_text(angle = 30, hjust=1)) +
    theme(legend.position = "none") +
    red_black

fig("thomas-2i")

# %% --------
p2e + p2i
fig("thomas-fig2-ei", 8, 4)

# %% --------

p3e = df %>% 
    ggplot(aes(normalized_onset, 1-is_initial, color=agent)) +
    stat_summary_bin(fun.data=mean_se, breaks=seq(0, 1.0, 0.1)) +
    labs(x="Normalized time (%)", y="P(returning gaze)") + 
    coord_cartesian(xlim=c(0, 1), ylim=c(0, 1)) +
    red_black + theme(legend.position = "none")
fig("thomas-3e")

# %% --------

p3f = df %>% 
    filter(!is_initial) %>% 
    ggplot(aes(normalized_onset, duration, color=agent)) +
    stat_summary_bin(fun.data=mean_se, breaks=seq(0, 1.0, 0.1)) +
    labs(x="Normalized time (%)", y="Duration of returning gaze") + 
    coord_cartesian(xlim=c(0, 1), ylim=c(0, 600)) +
    red_black + theme(legend.position = "none")
fig("thomas-3f")

# %% --------
p3e + p3f
fig("thomas-fig3-ef", 8, 4)

# %% --------

nts = df %>% 
    group_by(agent, subject, trial) %>%
    mutate(prop_duration = duration / sum(duration)) %>% 
    ungroup() %>% 
    mutate(n_step=round(prop_duration * 100)) %>% 
    uncount(n_step) %>% 
    group_by(trial) %>% 
    mutate(normalized_timestep = row_number() / 100)



nts %>% 
    ggplot(aes(normalized_timestep, 1-is_initial, color=agent)) +
    stat_summary_bin(fun=mean, geom="line", breaks=seq(-.05, 1.05, 0.1)) +
    labs(x="Normalized time (%)", y="P(returning gaze)") + 
    red_black + theme(legend.position = "none")

fig("thomas-3e")
stat_summary_bin(fun=mean, geom="line", breaks=seq(-.05, 1.05, 0.1)) +
    labs(x="Normalized time (%)", y="Duration of returning gaze") + 
    red_black + theme(legend.position = "none")

# %% --------

nts %>% 
    filter(!is_initial) %>% 
    ggplot(aes(normalized_timestep, duration))


# %% ==================== OLD ====================

ggplot(df4, aes(n_fix_since_best_last_seen, as.numeric(best_seen_chosen))) + 
    stat_summary(geom='bar') +
    xlim(-0.5, 4) +
    ylab("p(best seen item chosen")
fig("reutskaja-3b")

# %% --------
ggplot(filter(df4, n_refix <= 5), aes(n_refix, ..prop..)) + 
    geom_bar() +
    facet_wrap(~ n_item) +
    ylab("proportion of trials")
fig("reutskaja-3c")

# %% --------
df %>% 
    filter(is_initial == 1 & number <= 8) %>% 
    ggplot(aes(number, is_last_initial, color=n_item)) + 
        stat_summary(geom="line") + ylim(0,1) + mpl_colors

fig("reutskaja-4a")

# %% --------

df %>% 
    filter(is_initial == 1) %>% 
    ggplot(aes(current_value, is_last_initial, color=n_item)) + 
        stat_summary_bin(geom="line") + ylim(0,1) + mpl_colors + xlim(-3, 3)

fig("reutskaja-4b")

# %% --------

df %>% 
    filter(is_initial == 1 & number <= 8) %>% 
    ggplot(aes(cached_value, is_last_initial, color=n_item)) + 
        stat_summary_bin(geom="line") + ylim(0,1) + mpl_colors + xlim(-3, 3)

fig("reutskaja-4c")

