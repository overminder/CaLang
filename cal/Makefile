CALCC = ./Compile
CC = gcc
CALCCFLAGS = --cpp --fuel 1000

all :
	echo "nothing to do..."

%-c : %.s
	$(CC) $< -o $@

%.s : %.cal
	$(CALCC) $< --gas $(CALCCFLAGS) -o $@

%.dot : %.cal
	$(CALCC) $< --ral-coal-lv $(CALCCFLAGS) | xdot.py -

clean :
	rm -f *.s *-c
