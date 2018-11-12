#include "sqroll.h"

/* Finalizes a statement. Used as GC finalizer in `sqlPrepare` in Sqlite3.hs. */
int sqroll_finalize_stmt(sqlite3 *db, sqlite3_stmt *stmt) {
    sqlite3_stmt *i = sqlite3_next_stmt(db, 0);
    int status = 0;

    while(i != 0) {
        if(i == stmt) {
            status = sqlite3_finalize(stmt);
            i = 0;
        } else {
            i = sqlite3_next_stmt(db, i);
        }
    }

    return status;
}


int sqroll_close(sqlite3 *db) {

    /* Use the v2 version of sqlite3_close here which is designed to be used
       with garbage-collected host languages.
       It will make the database inaccessible, but not free its resources
       until the last statement associated with it has been finalized;
       once that happens, the database will close itself.

       This is necessary because garbage collection can call sqlite3_finalize
       on GC-managed statements *after* the user has manually closed
       the database - this is exactly what happens in sqroll_finalize_stmt above,
       which is registered as a statement finalizer in `sqlPrepare` in Sqlite3.hs.
    */
    return sqlite3_close_v2(db);
}
