# %%
using TypedTables
using SplitApplyCombine
using Statistics
using Serialization
using Memoize
using StatsBase: counts
include("utils.jl")

"This allows us to treat trials with the same value sets (but in different
order) as equivalent, which is useful for fitting"
function sort_value(t)
    rank = sortperm(-t.value)
    remap = sortperm(rank)
    (
        value = t.value[rank],
        fixations = remap[t.fixations],
        choice = remap[t.choice],
        fix_times = t.fix_times,
    )
end

@memoize function load_dataset(num::String, fold=:full)
    full = open(deserialize, "data/$(num)_items.jls")
    if fold == :train
        filter!(x->iseven(x.trial), full)
    elseif fold == :test
        filter!(x->isodd(x.trial), full)
    else
        full
    end
end
load_dataset(n::Int, fold=:full) = load_dataset(["", "two", "three"][n], fold)

# function discretize_fixations(t; sample_time=100)
#     mapmany(t.fixations, t.fix_times) do item, ft
#         repeat([item], Int(round(ft/sample_time)))
#     end
# end


# function discretize_fixations2(t; sample_time=100)
#     out = Int[]
#     err = 0.
#     for d in durations
#         push!(out, Int(d ÷ dt))
#         err += d % dt
#         if err > dt/2
#             out[end] += 1
#             err -= dt
#         end
#     end
#     out
# end

function discretize_fixations(t; sample_time=100)
    N = maximum(t.fixations)
    long = mapmany(t.fixations, t.fix_times) do item, ft
        repeat([item], ft)
    end
    out = map(1:sample_time:length(long)) do start
        stop = min(start + sample_time, length(long))
        if stop - start < sample_time/2
            0
        else
            argmax(counts(long[start:stop-1], 1:N))
        end
    end
    if out[end] == 0
        @assert pop!(out) == 0
    end
    out
end


function train_test_split(trials, fold::String)
    fold == "all" && return (train=trials, test=trials)

    test_idx = if occursin("/", fold)
        this, total = parse.(Int, (split(fold, "/")))
        this:total:length(trials)
    else
        Dict(
            "odd" => 1:2:length(trials),
            "even" => 2:2:length(trials),
        )[fold]
    end
    train_idx = setdiff(eachindex(trials), test_idx)
    (train=trials[train_idx], test=trials[test_idx])
end

function get_fold(trials, test_fold::String, fold::Symbol)
    split_trials = train_test_split(trials, test_fold)
    getfield(split_trials, fold)
end

empirical_prior(trials) = juxt(mean, std)(flatten(trials.value))

function make_prior(trials, β_μ)
    μ_emp, σ_emp = empirical_prior(trials)
    (μ_emp * β_μ, σ_emp)
end

