
typedef struct NodeStruct {
} Node;

void *
runCollection(RootFinder *rf, size_t size) {
    /* Note that we haven't started to deal with pointers residing
       in callee-saved registers */
    CaLang_initializeRootFinder(rf);
    while (CaLang_findNextFrame(rf)) {
        Node **ptrLocation;
        while (CaLang_findNextPointerInFrame(rf, (void ***) &ptrLocation)) {
            copyPtr(ptrLocation);
        }
    }
}

