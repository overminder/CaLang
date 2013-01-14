typedef struct RootFinderStruct RootFinder;

typedef (void *) (*CollectFunc) (RootFinder *, size_t);

void CaLang_initializeGcMapTable(void);
void *CaLang_prepareCollection(CollectFunc, size_t);
void CaLang_initializeRootFinder(RootFinder *);
int CaLang_findNextFrame(RootFinder *);
int CaLang_findNextPointerInFrame(RootFinder *, void ***);

