CALCC = ./Compile
CC = gcc
CALCCFLAGS = --cpp --fuel 1000

all :
	echo "nothing to do..."

gc_asm.o : gc_asm.S
	$(CC) $< -g -c -o $@

%-c : %.o gc_asm.o
	$(CC) $^ -o $@

%.o : %.s
	$(CC) $< -g -c -o $@

%.s : %.cal
	$(CALCC) $< --gas $(CALCCFLAGS) -o $@

%.dot : %.cal
	$(CALCC) $< --ral-coal-lv $(CALCCFLAGS) | xdot.py -

clean :
	rm -f *.s *-c *.o
