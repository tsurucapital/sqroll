#include <sqroll.h>

int sqroll_finalize_stmt(sqlite3_stmt *stmt) {
    sqlite3 *db = sqlite3_db_handle(stmt);
    sqlite3_stmt *i = sqlite3_next_stmt(db, 0);
    int status;

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
    sqlite3_stmt *i;

    for(i = sqlite3_next_stmt(db, 0); i != 0;
            i = sqlite3_next_stmt(db, i)) {
        sqlite3_finalize(i);
    }

    return sqlite3_close(db);
}
