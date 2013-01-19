HS_SRC = $(shell find . -name "*.hs")
HCFLAGS = -DHOST_ARCH=X64
HCPROFFLAGS += -prof -auto-all -rtsopts
OUT = Test-c


all : $(OUT)

$(OUT) : $(HS_SRC)
	ghc --make Test.hs -cpp $(HCFLAGS) -o $(OUT)

profile : $(HS_SRC)
	ghc --make Test.hs -cpp $(HCFLAGS) $(HCPROFFLAGS) -o $(OUT)

clean :
	find -name "*.o" -delete
	find -name "*.hi" -delete
	rm $(OUT)

