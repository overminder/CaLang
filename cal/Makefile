CALCC = ./Compile
CC = gcc

all :
	echo "nothing to do..."

%-c : %.s
	$(CC) $< -o $@

%.s : %.cal
	$(CALCC) $< --O0 --cpp -o $@

%.dot : %.cal
	$(CALCC) $< --O0-lv --cpp | xdot.py -

clean :
	rm -f *.s *-c
