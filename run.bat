if not exist bin (
    mkdir bin
)

ghc -O3 -isrc -odir bin -hidir bin -o bin/aoc21 src/Main && .\bin\aoc21.exe %1 %2

