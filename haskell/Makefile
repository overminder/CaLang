HS_SRC = $(shell find . -name "*.hs")
CPPFLAGS = -DHOST_ARCH=X64
OUT = Test-c

all : $(OUT)

$(OUT) : $(HS_SRC)
	ghc --make Test.hs -cpp $(CPPFLAGS) -o $(OUT)

clean :
	find -name "*.o" -delete
	find -name "*.hi" -delete
	rm $(OUT)

