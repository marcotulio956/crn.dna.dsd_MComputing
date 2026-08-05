using InteractiveUtils
using Pkg

println("="^80)
println("VERSION")
println("="^80)
versioninfo()

println("\nACTIVE PROJECT")
println(Base.active_project())

println("\nDEPOT_PATH")
println.(DEPOT_PATH)

println("\nLOAD_PATH")
println.(LOAD_PATH)

println("\nSTATUS")
Pkg.status()

println("\nMANIFEST")
Pkg.status(mode=Pkg.PKGMODE_MANIFEST)

println("\nLOADING PACKAGES")

for pkg in [
    :DifferentialEquations,
    :DiffEqBase,
    :RCall,
    :Suppressor,
    :SparseArrays,
    :LinearAlgebra
]
    try
        @eval using $(pkg)
        println(pkg, " ✓")
    catch e
        println(pkg, " ✗")
        showerror(stdout, e)
        println()
    end
end