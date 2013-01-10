
void *
runCollection(RootFinder *rf, size_t size) {
    /* Note that we haven't started to deal with pointers residing
       in callee-saved registers */
    while (CaLang_findGcMapFor(rf)) {
        long **ptrLocation;
        while (CaLang_findNextPointerInFrame(rf, &ptrLocation)) {
            copyPtr(ptrLocation);
        }
    }
}

