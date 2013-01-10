#include "gcinterface.h"

static void *gcMapTable;

struct RootFinderStruct {
    long rbx;
    long r12;
    long r13;
    long r14;
    long r15;
    void **rbp;
    long retAddr;
    GcMap *currGcMap;
    long gcMapIter;
};

void
CaLang_initializeGcMapTable(void) {
    gcMapTable = blahblah;
}

int
CaLang_findGcMapFor(RootFinder *rf) {
    GcMap *found = IntHashtable_Lookup(gcMapTable, rf->retAddr);
    if (found) {
        rf->currGcMap = found;
        rf->gcMapIter = 0;
        return 1;
    }
    else {
        return 0;
    }
}

int
CaLang_findNextPointerInFrame(RootFinder *rf, void ***out) {
    GcMap *map = rf->currGcMap;
    long iter = rf->gcMapIter;
    if (iter >= map->numPtrs) {
        return 0;
    }
    else {
        *out = rf->rbp + map->ptrOffsets[iter++];
        rf->gcMapIter = iter;
        return 1;
    }
}

