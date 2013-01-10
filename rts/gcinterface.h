typedef struct RootFinderStruct RootFinder;

typedef (void *) (*CollectFunc) (RootFinder *, size_t);

void CaLang_initializeGcMapTable(void);
void *CaLang_prepareCollection(CollectFunc, size_t);
int CaLang_findGcMapFor(RootFinder *);
int CaLang_findNextPointerInFrame(RootFinder *, void ***);

