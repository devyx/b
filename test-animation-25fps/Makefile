all: main.tap
	fuse main.tap 
main.tap: main.asm fighter1.asm fighter4.asm
	pasmo --alocal --tapbas main.asm main.tap main.obj 
