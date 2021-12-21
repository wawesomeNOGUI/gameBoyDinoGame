rgbasm -o main.o main.asm
rgblink -o dinoGame.gb main.o
rgbfix -v -p 0 dinoGame.gb
