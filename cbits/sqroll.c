#include <sqroll.h>

int sqroll_finalize_stmt(sqlite3_stmt *stmt) {
    sqlite3 *db = sqlite3_db_handle(stmt);
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
    sqlite3_stmt *i = 0;

    do {
        sqlite3_finalize(i);
        i = sqlite3_next_stmt(db, 0);
    } while (i != 0);

    return sqlite3_close(db);
}
