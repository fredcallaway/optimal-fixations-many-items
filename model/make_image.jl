using PackageCompiler
using Pkg
pkgs = split("BayesianOptimization ProgressMeter CSV Glob Distributions Memoize Parameters SplitApplyCombine StaticArrays TypedTables StatsBase Sobol DataStructures Optim QuadGK ArgParse StatsFuns OnlineStats Bootstrap")
Pkg.add(pkgs)

@time create_sysimage(Symbol.(pkgs), sysimage_path=".sysimage.so", precompile_execution_file="precompile.jl")