CALCC = ./Compile
CC = gcc
CALCCFLAGS = --fuel 0

all :
	echo "nothing to do..."

%-c : %.s
	$(CC) $< -o $@

%.s : %.cal
	$(CALCC) $< --O0 --cpp $(CALCCFLAGS) -o $@

%.dot : %.cal
	$(CALCC) $< --O0-lv --cpp $(CALCCFLAGS) | xdot.py -

clean :
	rm -f *.s *-c
