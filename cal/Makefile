CALCC = ./Compile
CC = gcc

%-c : %.s
	$(CC) $< -o $@

%.s : %.cal
	$(CALCC) $< --O0 --cpp -o $@

%.dot : %.cal
	$(CALCC) $< --O0-lv --cpp | xdot.py -

clean :
	rm -f *.s *-c
