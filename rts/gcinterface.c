#include "gcinterface.h"

#define NUM_CALLEE_SAVED_REGS 5

static void *gcMapTable;

typedef struct {
    long savedReg[NUM_CALLEE_SAVED_REGS];
    void **framePtr;
    void *retAddr;
    GcMap *gcMap;
} FrameDescr;

struct RootFinderStruct {
    FrameDescr frame;
    int8_t justLeftRuntime;
    int8_t 
    long stackIter;
    long regIter;
};

void
CaLang_initializeGcMapTable(void) {
    gcMapTable = blahblah;
}

void
CaLang_initializeRootFinder(RootFinder *rf) {
    rt->justLeftRuntime = 1;
}

int
CaLang_findNextFrame(RootFinder *rf) {
    GcMap *gcmap;

    if (!rf->justLeftRuntime) {
        long i;

        rf->frame.retAddr =
            rf->frame.framePtr[rf->frame.gcMap->framePtrOffset + 1];
        rf->frame.framePtr =
            rf->frame.framePtr[rf->frame.gcMap->framePtrOffset];

        for (i = 0; i < NUM_CALLEE_SAVED_REGS; ++i) {
            long offset = 
        }
    }
    gcmap = IntHashtable_Lookup(gcMapTable, rf->frame.retAddr);
    if (gcmap) {
        rf->frame.gcMap = gcmap;
        rf->stackIter = 0;
        rf->regIter = 0;
        return 1;
    }
    else {
        return 0;
    }
}

static findNextPointerInReg(RootFinder *rf, void ***out) {
    while (1) {
        switch (rf->regIter) {
        case 0:
            if rf->currGcMap
        case 1:
        case 2:
        case 3:
        case 4:
        case 5:
        }
    }
}

int
CaLang_findNextPointerInFrame(RootFinder *rf, void ***out) {
    GcMap *map = rf->currGcMap;
    long iter = rf->stackIter;
    if (iter >= map->numPtrs) {
        return findNextPointerInReg(rf, out);
    }
    else {
        *out = rf->rbp + map->ptrOffsets[iter++];
        rf->stackIter = iter;
        return 1;
    }
}

