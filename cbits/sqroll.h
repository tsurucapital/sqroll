#ifndef SQROLL_H
#define SQROLL_H

#include <sqlite3.h>

/* Wraps sqlite3_finalize in a "safe" by checking that if statement is still
 * open first */
int sqroll_finalize(sqlite3 *db, sqlite3_stmt *stmt);

/* Wraps sqlite3_close by first closing all open statements */
int sqroll_close(sqlite3 *db);

#endif
