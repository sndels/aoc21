if not exist bin (
    mkdir bin
)

stack build && stack exec aoc21 %1 %2

