using StatsBase

vl = readlines("input3.txt")
values = [map(x->tryparse(Int, x), split(value, "")) for value in vl]
