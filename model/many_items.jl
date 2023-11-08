@everywhere begin
    using TypedTables
    using CSV
    using ProgressMeter
    using Serialization
    include("meta_mdp.jl")
    include("bmps.jl")
    include("bmps_bayesopt.jl")
    include("box.jl")
    include("simulations.jl")
end

# %% ==================== Set up search space ====================

# ±(x, y) = (max(.001, x-y), x+y)
# space = Box(
#     :σ_obs => 2.60 ± 5 * 0.216,
#     :switch_cost => 0.00995 ± 5 * 0.001,
#     :sample_cost => 0.00373 ± 5 * 0.001,
# )


# try_x = let
#     x0 = 0.5ones(n_free(space))
#     mapreduce(vcat, 1:n_free(space)) do i
#         map(0:1/6:1) do xi
#             x = copy(x0)
#             x[i] = xi
#             x
#         end
#     end |> unique
# end


# offsets = Dict{MetaMDP, NamedTuple}()
# jobs = Iterators.product([9,16,25,36], try_x)
# mdps = map(jobs) do (n_arm, x)
#     prm = space(x)
#     m = MetaMDP(;n_arm, prm...)
#     offsets[m] = NamedTuple{(:σ_obs, :switch_cost, :sample_cost)}(Int.(x .* 6 .- 3))
#     m
# end

# may 4 results
# space = Box(
#     :σ_obs => (.1, 2.6, :log),
#     :switch_cost => (.001, .01, :log),
#     :sample_cost => (.0001, .003, :log),
# )

space = Box(
    :σ_obs => (0.7, 1.4),
    :switch_cost => (.005, .01),
    :sample_cost => (.0001, .001, :log),
)

using Sobol
try_x = let
    seq = SobolSeq(n_free(space))
    [Sobol.next!(seq) for i in 1:500]
end
jobs = Iterators.product([9,16,25], try_x)
mdps = map(jobs) do (n_arm, x)
    prm = space(x)
    m = MetaMDP(;n_arm, prm...)
    m
end

serialize("tmp/hash_map", map(hash, mdps))

# %% ==================== Optimize policies ====================

mkpath("tmp/sobol_policies")
policies = @showprogress pmap(mdps) do m
    pol = optimize_bmps(m; α=364, verbose=false, parallel=false)
    serialize("tmp/sobol_policies/$(hash(m))", pol)
end


# %% ==================== Simulate ====================
policies = asyncmap(mdps) do m
    deserialize("tmp/sobol_policies/$(hash(m))")
end
# prior_space = Box(OrderedDict(:β_μ => 0.581 ± 0.118))

@everywhere function make_table(trials)
    map(trials) do t
        n_item = length(t.value)
        seen = unique(t.fixations)
        best_seen = seen[argmax(t.value[seen])]

        (n_item, seen, 
         prop_seen = length(seen) / n_item,
         rt = sum(t.fix_times),
         mean_fixation_duration = mean(t.fix_times),
         n_fix_since_best_last_seen = length(t.fixations) - findall(best_seen .== t.fixations)[end],
         n_refix = length(t.fixations) - length(seen),
         best_seen_chosen = best_seen == t.choice,
         last_seen_chosen = t.fixations[end] == t.choice,)
    end |> Table
end

to_simulate = Iterators.product(policies, -.5:.1:0)
summ = @showprogress pmap(to_simulate) do (policy, prior_bias)
    trials = map(1:10000) do i
        simulate(policy, (prior_bias, 1), randn(policy.m.n_arm); max_steps=200)
    end
    x = make_table(trials)
    feats = map(mean, (;x.prop_seen, x.rt, x.mean_fixation_duration, x.best_seen_chosen, x.last_seen_chosen, x.n_refix))
    m = policy.m
    (;m.n_arm, m.σ_obs, m.switch_cost, m.sample_cost, prior_bias, feats...)
end;
serialize("tmp/summary_may10", summ)

# %% ==================== Analysis ====================

summ = deserialize("tmp/summary_may10")[:] |> Table
summ |> CSV.write("results/summary_may10.csv")
target = (
    last_seen_chosen = (0.5, 1),
    best_seen_chosen = (0.85, 1),
    rt = (4000, 14000),
    prop_seen = (0.8, 1),
    mean_fixation_duration = (275, 400),
)
T = Table(summ; id=repeat(1:3000, inner=3))

T |> CSV.write("results/summary_may10.csv")



# %% --------

X = filter(x->x.n_arm == 9, summ);
loss = map(X) do x
    mapreduce(+, fieldnames(typeof(target))) do k
        y, σ = getfield(target, k)
        l = (getfield(x, k) - y) / σ
        l^2
    end
end

T = Table(X; loss)

T[sortperm(T.loss)] |> CSV.write("results/summary_may10.csv")


using Printf
sprintf(fmt::String,args...) = @eval @sprintf($fmt,$(args...))
function mean_std_str(xs, d)
    fmt = "%.$(d)f ± %.$(d)f"
    sprintf(fmt, juxt(mean, std)(xs)...)
end

x = T[partialsortperm(T.loss, 1:30)]

map((;x.σ_obs, x.switch_cost, x.sample_cost)) do xs
    mean_std_str(xs, 4)
end

loss

# %% ==================== Simulate best again ====================
best_id = 476  # WTF?
x = T[T.n_arm .== 9][best_id]
m = MetaMDP(;x.n_arm, x.σ_obs, x.switch_cost, x.sample_cost)

policy = deserialize("tmp/sobol_policies/$(hash(m))")

trials = map(1:10000) do i
    simulate(policy, (x.prior_bias, 1), randn(policy.m.n_arm); max_steps=200)
end
serialize("tmp/best_sim", trials)

hash_map = deserialize("tmp/hash_map")



#trials |> CSV.write("results/many_options/sim-trials.csv")
# %% --------
trials = deserialize("tmp/best_sim")
df = mapreduce(vcat, trials) do t
    n_item = length(t.value)
    seen = unique(t.fixations)
    best_seen = seen[argmax(t.value[seen])]
    
    seen = Set{Int}()
    is_initial = map(t.fixations) do f
        x = f in seen
        push!(seen, f)
        !x
    end
    is_last_initial = fill(0, length(t.fixations))
    is_last_initial[findlast(is_initial)] = true
    is_chosen = t. fixations .== t.choice
    is_final = fill(0, length(t.fixations))
    is_final[end] = 1

    (n_item = fill(length(t.value), length(t.fixations)),
     duration=t.fix_times,
     is_chosen, is_final,
     cached_value = accumulate(max, t.value[t.fixations]),
     current_value = t.value[t.fixations],
     number = eachindex(t.fixations),
     is_initial, is_last_initial ) |> invert
end |> Vector{NamedTuple} |> Table
# %% --------

df |>  CSV.write("results/many_options/sim-fix.csv")





# %% ==================== Old ====================


K = map(to_simulate) do (policy, prior_bias)
    (;policy.m.n_arm, offsets[policy.m]..., prior_bias)
end

map(K, tables) do k, t
    x = invert(t)
    map(mean, (;k..., x.prop_seen, x.rt, x.mean_fixation_duration, x.best_seen_chosen, x.last_seen_chosen, x.n_refix))
end |> Table |> CSV.write("summary_apr26.csv")
# %% --------

X = map(tables[2:5, :, :]) do t
    mean(t.last_seen_chosen)
end

minimum(X)  # minimum p(last chosen) is .75

# %% --------


maximum(X)  # max p(best chosen) is .61

# %% --------


function make_summary(f)
    X = map(tables) do t
        mean(t.best_seen_chosen)
    end
    T = map(K, tables) do k, tbl
        (;k..., val=f(tbl))
    end |> Table
end

T = make_summary(x->mean(x.last_seen_chosen))
T = make_summary(x->mean(x.best_seen_chosen))
# %% --------
using GLM
lm(@formula(val ~ n_arm + σ_obs + switch_cost + sample_cost + prior_bias), T)

# %% --------
T = make_summary(x->mean(x.last_seen_chosen))

filter(T) do t
    t.prior_bias == -0 &&
    # t.σ_obs == 0 &&
    # t.sample_cost == 0 &&
    # t.switch_cost == 0 &&
    t.n_arm == 16 &&
    true
end

# even with no prior bias, the model chooses the last seen item around 80% of the time

# %% --------

T = make_summary(x->mean(x.best_seen_chosen))



x = filter(T) do t
    t.prior_bias == 0 &&
    t.σ_obs == 0 &&
    # t.sample_cost == 0 &&
    t.switch_cost == 0 &&
    t.n_arm == 16
end




