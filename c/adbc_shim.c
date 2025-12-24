/*
 * ADBC FFI shim for Lean 4
 * Dynamically loads libduckdb.so to avoid glibc version conflicts
 */
#include <lean/lean.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <dlfcn.h>

/* === ADBC Structures (from Arrow ADBC spec) === */

typedef uint8_t AdbcStatusCode;
#define ADBC_STATUS_OK 0
#define ADBC_STATUS_UNKNOWN 1
#define ADBC_STATUS_NOT_IMPLEMENTED 2
#define ADBC_STATUS_NOT_FOUND 3
#define ADBC_STATUS_ALREADY_EXISTS 4
#define ADBC_STATUS_INVALID_ARGUMENT 5
#define ADBC_STATUS_INVALID_STATE 6
#define ADBC_STATUS_INVALID_DATA 7
#define ADBC_STATUS_INTEGRITY 8
#define ADBC_STATUS_INTERNAL 9
#define ADBC_STATUS_IO 10
#define ADBC_STATUS_CANCELLED 11
#define ADBC_STATUS_TIMEOUT 12
#define ADBC_STATUS_UNAUTHENTICATED 13
#define ADBC_STATUS_UNAUTHORIZED 14

struct AdbcError {
    char* message;
    int32_t vendor_code;
    char sqlstate[5];
    void (*release)(struct AdbcError*);
    void* private_data;
    void* private_driver;
};

struct AdbcDatabase {
    void* private_data;
    void* private_driver;
};

struct AdbcConnection {
    void* private_data;
    void* private_driver;
};

struct AdbcStatement {
    void* private_data;
    void* private_driver;
};

/* === Arrow C Data Interface === */

struct ArrowSchema {
    const char* format;
    const char* name;
    const char* metadata;
    int64_t flags;
    int64_t n_children;
    struct ArrowSchema** children;
    struct ArrowSchema* dictionary;
    void (*release)(struct ArrowSchema*);
    void* private_data;
};

struct ArrowArray {
    int64_t length;
    int64_t null_count;
    int64_t offset;
    int64_t n_buffers;
    int64_t n_children;
    const void** buffers;
    struct ArrowArray** children;
    struct ArrowArray* dictionary;
    void (*release)(struct ArrowArray*);
    void* private_data;
};

struct ArrowArrayStream {
    int (*get_schema)(struct ArrowArrayStream*, struct ArrowSchema* out);
    int (*get_next)(struct ArrowArrayStream*, struct ArrowArray* out);
    const char* (*get_last_error)(struct ArrowArrayStream*);
    void (*release)(struct ArrowArrayStream*);
    void* private_data;
};

/* === ADBC Function Pointers (loaded via dlsym) === */

typedef AdbcStatusCode (*PFN_AdbcDatabaseNew)(struct AdbcDatabase*, struct AdbcError*);
typedef AdbcStatusCode (*PFN_AdbcDatabaseSetOption)(struct AdbcDatabase*, const char*, const char*, struct AdbcError*);
typedef AdbcStatusCode (*PFN_AdbcDatabaseInit)(struct AdbcDatabase*, struct AdbcError*);
typedef AdbcStatusCode (*PFN_AdbcDatabaseRelease)(struct AdbcDatabase*, struct AdbcError*);
typedef AdbcStatusCode (*PFN_AdbcConnectionNew)(struct AdbcConnection*, struct AdbcError*);
typedef AdbcStatusCode (*PFN_AdbcConnectionInit)(struct AdbcConnection*, struct AdbcDatabase*, struct AdbcError*);
typedef AdbcStatusCode (*PFN_AdbcConnectionRelease)(struct AdbcConnection*, struct AdbcError*);
typedef AdbcStatusCode (*PFN_AdbcStatementNew)(struct AdbcConnection*, struct AdbcStatement*, struct AdbcError*);
typedef AdbcStatusCode (*PFN_AdbcStatementSetSqlQuery)(struct AdbcStatement*, const char*, struct AdbcError*);
typedef AdbcStatusCode (*PFN_AdbcStatementExecuteQuery)(struct AdbcStatement*, struct ArrowArrayStream*, int64_t*, struct AdbcError*);
typedef AdbcStatusCode (*PFN_AdbcStatementRelease)(struct AdbcStatement*, struct AdbcError*);

static PFN_AdbcDatabaseNew pAdbcDatabaseNew;
static PFN_AdbcDatabaseSetOption pAdbcDatabaseSetOption;
static PFN_AdbcDatabaseInit pAdbcDatabaseInit;
static PFN_AdbcDatabaseRelease pAdbcDatabaseRelease;
static PFN_AdbcConnectionNew pAdbcConnectionNew;
static PFN_AdbcConnectionInit pAdbcConnectionInit;
static PFN_AdbcConnectionRelease pAdbcConnectionRelease;
static PFN_AdbcStatementNew pAdbcStatementNew;
static PFN_AdbcStatementSetSqlQuery pAdbcStatementSetSqlQuery;
static PFN_AdbcStatementExecuteQuery pAdbcStatementExecuteQuery;
static PFN_AdbcStatementRelease pAdbcStatementRelease;

/* === Global State === */

static void* g_lib = NULL;
static struct AdbcDatabase g_db = {0};
static struct AdbcConnection g_conn = {0};
static int g_initialized = 0;
static FILE* g_log = NULL;

/* === Logging to file === */
static void log_msg(const char* fmt, ...) {
    if (!g_log) g_log = fopen("/tmp/tv.log", "a");
    if (!g_log) return;
    va_list args;
    va_start(args, fmt);
    vfprintf(g_log, fmt, args);
    va_end(args);
    fflush(g_log);
}

/* === Helper: init error struct === */
static void init_error(struct AdbcError* err) {
    memset(err, 0, sizeof(*err));
}

/* === Helper: free error if needed === */
static void free_error(struct AdbcError* err) {
    if (err->release) err->release(err);
}

/* === Lean FFI Functions === */

// | Load ADBC functions from libduckdb.so
static int load_adbc_funcs(void) {
    const char* paths[] = {"/usr/lib/libduckdb.so", "/usr/local/lib/libduckdb.so", "libduckdb.so", NULL};
    for (int i = 0; paths[i]; i++) {
        g_lib = dlopen(paths[i], RTLD_NOW | RTLD_GLOBAL);
        if (g_lib) {
            log_msg("[adbc] loaded %s\n", paths[i]);
            break;
        }
    }
    if (!g_lib) {
        log_msg("[adbc] dlopen failed: %s\n", dlerror());
        return 0;
    }

    pAdbcDatabaseNew = (PFN_AdbcDatabaseNew)dlsym(g_lib, "AdbcDatabaseNew");
    pAdbcDatabaseSetOption = (PFN_AdbcDatabaseSetOption)dlsym(g_lib, "AdbcDatabaseSetOption");
    pAdbcDatabaseInit = (PFN_AdbcDatabaseInit)dlsym(g_lib, "AdbcDatabaseInit");
    pAdbcDatabaseRelease = (PFN_AdbcDatabaseRelease)dlsym(g_lib, "AdbcDatabaseRelease");
    pAdbcConnectionNew = (PFN_AdbcConnectionNew)dlsym(g_lib, "AdbcConnectionNew");
    pAdbcConnectionInit = (PFN_AdbcConnectionInit)dlsym(g_lib, "AdbcConnectionInit");
    pAdbcConnectionRelease = (PFN_AdbcConnectionRelease)dlsym(g_lib, "AdbcConnectionRelease");
    pAdbcStatementNew = (PFN_AdbcStatementNew)dlsym(g_lib, "AdbcStatementNew");
    pAdbcStatementSetSqlQuery = (PFN_AdbcStatementSetSqlQuery)dlsym(g_lib, "AdbcStatementSetSqlQuery");
    pAdbcStatementExecuteQuery = (PFN_AdbcStatementExecuteQuery)dlsym(g_lib, "AdbcStatementExecuteQuery");
    pAdbcStatementRelease = (PFN_AdbcStatementRelease)dlsym(g_lib, "AdbcStatementRelease");

    return pAdbcDatabaseNew && pAdbcDatabaseSetOption && pAdbcDatabaseInit &&
           pAdbcDatabaseRelease && pAdbcConnectionNew && pAdbcConnectionInit &&
           pAdbcConnectionRelease && pAdbcStatementNew && pAdbcStatementSetSqlQuery &&
           pAdbcStatementExecuteQuery && pAdbcStatementRelease;
}

// | Init ADBC (in-memory DuckDB)
lean_obj_res lean_adbc_init(lean_obj_arg world) {
    if (g_initialized) {
        return lean_io_result_mk_ok(lean_box(1));
    }

    // Load functions via dlopen
    if (!load_adbc_funcs()) {
        log_msg( "[adbc] load_adbc_funcs failed\n");
        return lean_io_result_mk_ok(lean_box(0));
    }

    struct AdbcError err;
    init_error(&err);

    // Create database
    if (pAdbcDatabaseNew(&g_db, &err) != ADBC_STATUS_OK) {
        log_msg( "[adbc] DatabaseNew failed: %s\n", err.message ? err.message : "?");
        free_error(&err);
        return lean_io_result_mk_ok(lean_box(0));
    }

    // Set driver to duckdb
    if (pAdbcDatabaseSetOption(&g_db, "driver", "/usr/lib/libduckdb.so", &err) != ADBC_STATUS_OK) {
        log_msg( "[adbc] DatabaseSetOption(driver) failed: %s\n", err.message ? err.message : "?");
        pAdbcDatabaseRelease(&g_db, &err);
        free_error(&err);
        return lean_io_result_mk_ok(lean_box(0));
    }

    // Set entrypoint
    if (pAdbcDatabaseSetOption(&g_db, "entrypoint", "duckdb_adbc_init", &err) != ADBC_STATUS_OK) {
        log_msg( "[adbc] DatabaseSetOption(entrypoint) failed: %s\n", err.message ? err.message : "?");
        pAdbcDatabaseRelease(&g_db, &err);
        free_error(&err);
        return lean_io_result_mk_ok(lean_box(0));
    }

    // Set path="" for in-memory
    if (pAdbcDatabaseSetOption(&g_db, "path", "", &err) != ADBC_STATUS_OK) {
        log_msg( "[adbc] DatabaseSetOption(path) failed: %s\n", err.message ? err.message : "?");
        pAdbcDatabaseRelease(&g_db, &err);
        free_error(&err);
        return lean_io_result_mk_ok(lean_box(0));
    }

    // Init database
    if (pAdbcDatabaseInit(&g_db, &err) != ADBC_STATUS_OK) {
        log_msg( "[adbc] DatabaseInit failed: %s\n", err.message ? err.message : "?");
        pAdbcDatabaseRelease(&g_db, &err);
        free_error(&err);
        return lean_io_result_mk_ok(lean_box(0));
    }

    // Create connection
    if (pAdbcConnectionNew(&g_conn, &err) != ADBC_STATUS_OK) {
        log_msg( "[adbc] ConnectionNew failed: %s\n", err.message ? err.message : "?");
        pAdbcDatabaseRelease(&g_db, &err);
        free_error(&err);
        return lean_io_result_mk_ok(lean_box(0));
    }

    // Init connection
    if (pAdbcConnectionInit(&g_conn, &g_db, &err) != ADBC_STATUS_OK) {
        log_msg( "[adbc] ConnectionInit failed: %s\n", err.message ? err.message : "?");
        pAdbcConnectionRelease(&g_conn, &err);
        pAdbcDatabaseRelease(&g_db, &err);
        free_error(&err);
        return lean_io_result_mk_ok(lean_box(0));
    }

    log_msg( "[adbc] initialized OK\n");
    g_initialized = 1;
    return lean_io_result_mk_ok(lean_box(1));
}

// | Shutdown ADBC
lean_obj_res lean_adbc_shutdown(lean_obj_arg world) {
    if (!g_initialized) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    struct AdbcError err;
    init_error(&err);

    pAdbcConnectionRelease(&g_conn, &err);
    pAdbcDatabaseRelease(&g_db, &err);
    free_error(&err);

    g_initialized = 0;
    memset(&g_conn, 0, sizeof(g_conn));
    memset(&g_db, 0, sizeof(g_db));

    if (g_lib) {
        dlclose(g_lib);
        g_lib = NULL;
    }

    return lean_io_result_mk_ok(lean_box(0));
}

/* === Query Result (opaque to Lean) === */

typedef struct {
    struct ArrowSchema schema;
    struct ArrowArray* batches;
    int64_t n_batches;
    int64_t total_rows;
} QueryResult;

// | Finalize QueryResult
static void qr_finalize(void* p) {
    QueryResult* qr = (QueryResult*)p;
    if (qr->schema.release) qr->schema.release(&qr->schema);
    for (int64_t i = 0; i < qr->n_batches; i++) {
        if (qr->batches[i].release) qr->batches[i].release(&qr->batches[i]);
    }
    free(qr->batches);
    free(qr);
}

// | Foreach noop
static void qr_foreach(void* p, b_lean_obj_arg f) { (void)p; (void)f; }

static lean_external_class* g_qr_class = NULL;

static lean_external_class* get_qr_class(void) {
    if (!g_qr_class) {
        g_qr_class = lean_register_external_class(qr_finalize, qr_foreach);
    }
    return g_qr_class;
}

// | Execute SQL query, return QueryResult
lean_obj_res lean_adbc_query(b_lean_obj_arg sql_obj, lean_obj_arg world) {
    if (!g_initialized) {
        return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string("ADBC not initialized")));
    }

    const char* sql = lean_string_cstr(sql_obj);
    struct AdbcError err;
    init_error(&err);

    // Create statement
    struct AdbcStatement stmt = {0};
    if (pAdbcStatementNew(&g_conn, &stmt, &err) != ADBC_STATUS_OK) {
        const char* msg = err.message ? err.message : "StatementNew failed";
        lean_object* e = lean_mk_io_user_error(lean_mk_string(msg));
        free_error(&err);
        return lean_io_result_mk_error(e);
    }

    // Set SQL
    if (pAdbcStatementSetSqlQuery(&stmt, sql, &err) != ADBC_STATUS_OK) {
        const char* msg = err.message ? err.message : "SetSqlQuery failed";
        lean_object* e = lean_mk_io_user_error(lean_mk_string(msg));
        pAdbcStatementRelease(&stmt, &err);
        free_error(&err);
        return lean_io_result_mk_error(e);
    }

    // Execute
    struct ArrowArrayStream stream = {0};
    int64_t rows_affected = -1;
    if (pAdbcStatementExecuteQuery(&stmt, &stream, &rows_affected, &err) != ADBC_STATUS_OK) {
        const char* msg = err.message ? err.message : "ExecuteQuery failed";
        lean_object* e = lean_mk_io_user_error(lean_mk_string(msg));
        pAdbcStatementRelease(&stmt, &err);
        free_error(&err);
        return lean_io_result_mk_error(e);
    }

    // Alloc result
    QueryResult* qr = calloc(1, sizeof(QueryResult));

    // Get schema
    if (stream.get_schema(&stream, &qr->schema) != 0) {
        free(qr);
        if (stream.release) stream.release(&stream);
        pAdbcStatementRelease(&stmt, &err);
        return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string("get_schema failed")));
    }

    // Collect batches
    int64_t cap = 16;
    qr->batches = malloc(cap * sizeof(struct ArrowArray));
    qr->n_batches = 0;
    qr->total_rows = 0;

    while (1) {
        struct ArrowArray batch = {0};
        if (stream.get_next(&stream, &batch) != 0) break;
        if (batch.release == NULL) break;  // end of stream

        if (qr->n_batches >= cap) {
            cap *= 2;
            qr->batches = realloc(qr->batches, cap * sizeof(struct ArrowArray));
        }
        qr->batches[qr->n_batches++] = batch;
        qr->total_rows += batch.length;
    }

    // Cleanup
    if (stream.release) stream.release(&stream);
    pAdbcStatementRelease(&stmt, &err);
    free_error(&err);

    // Wrap as external
    lean_object* obj = lean_alloc_external(get_qr_class(), qr);
    return lean_io_result_mk_ok(obj);
}

// | Get column count
lean_obj_res lean_qr_ncols(b_lean_obj_arg qr_obj, lean_obj_arg world) {
    QueryResult* qr = (QueryResult*)lean_get_external_data(qr_obj);
    return lean_io_result_mk_ok(lean_box_uint64((uint64_t)qr->schema.n_children));
}

// | Get row count
lean_obj_res lean_qr_nrows(b_lean_obj_arg qr_obj, lean_obj_arg world) {
    QueryResult* qr = (QueryResult*)lean_get_external_data(qr_obj);
    return lean_io_result_mk_ok(lean_box_uint64((uint64_t)qr->total_rows));
}

// | Get column name
lean_obj_res lean_qr_col_name(b_lean_obj_arg qr_obj, uint64_t col, lean_obj_arg world) {
    QueryResult* qr = (QueryResult*)lean_get_external_data(qr_obj);
    if ((int64_t)col >= qr->schema.n_children) {
        return lean_io_result_mk_ok(lean_mk_string(""));
    }
    const char* name = qr->schema.children[col]->name;
    return lean_io_result_mk_ok(lean_mk_string(name ? name : ""));
}

// | Get column format (Arrow type string)
lean_obj_res lean_qr_col_fmt(b_lean_obj_arg qr_obj, uint64_t col, lean_obj_arg world) {
    QueryResult* qr = (QueryResult*)lean_get_external_data(qr_obj);
    if ((int64_t)col >= qr->schema.n_children) {
        return lean_io_result_mk_ok(lean_mk_string(""));
    }
    const char* fmt = qr->schema.children[col]->format;
    return lean_io_result_mk_ok(lean_mk_string(fmt ? fmt : ""));
}

/* === Cell Access Helpers === */

// | Find batch and local row for global row index
static int find_batch(QueryResult* qr, int64_t row, int64_t* batch_idx, int64_t* local_row) {
    int64_t offset = 0;
    for (int64_t i = 0; i < qr->n_batches; i++) {
        if (row < offset + qr->batches[i].length) {
            *batch_idx = i;
            *local_row = row - offset;
            return 1;
        }
        offset += qr->batches[i].length;
    }
    return 0;
}

// | Check if cell is null
static int is_null(struct ArrowArray* arr, int64_t row) {
    if (arr->null_count == 0) return 0;
    if (arr->buffers[0] == NULL) return 0;
    const uint8_t* validity = (const uint8_t*)arr->buffers[0];
    int64_t idx = arr->offset + row;
    return !(validity[idx / 8] & (1 << (idx % 8)));
}

// | Get cell as string (returns static "null" for null, caller must not free)
lean_obj_res lean_qr_cell_str(b_lean_obj_arg qr_obj, uint64_t row, uint64_t col, lean_obj_arg world) {
    QueryResult* qr = (QueryResult*)lean_get_external_data(qr_obj);

    int64_t bi, lr;
    if (!find_batch(qr, (int64_t)row, &bi, &lr)) {
        return lean_io_result_mk_ok(lean_mk_string(""));
    }

    if ((int64_t)col >= qr->schema.n_children) {
        return lean_io_result_mk_ok(lean_mk_string(""));
    }

    struct ArrowArray* batch = &qr->batches[bi];
    struct ArrowArray* arr = batch->children[col];
    const char* fmt = qr->schema.children[col]->format;

    if (is_null(arr, lr)) {
        return lean_io_result_mk_ok(lean_mk_string(""));
    }

    char buf[64];

    // Dispatch on Arrow format
    if (fmt[0] == 'l') {  // int64
        const int64_t* data = (const int64_t*)arr->buffers[1];
        snprintf(buf, sizeof(buf), "%ld", data[arr->offset + lr]);
        return lean_io_result_mk_ok(lean_mk_string(buf));
    }
    if (fmt[0] == 'i') {  // int32
        const int32_t* data = (const int32_t*)arr->buffers[1];
        snprintf(buf, sizeof(buf), "%d", data[arr->offset + lr]);
        return lean_io_result_mk_ok(lean_mk_string(buf));
    }
    if (fmt[0] == 'g') {  // float64
        const double* data = (const double*)arr->buffers[1];
        snprintf(buf, sizeof(buf), "%g", data[arr->offset + lr]);
        return lean_io_result_mk_ok(lean_mk_string(buf));
    }
    if (fmt[0] == 'f') {  // float32
        const float* data = (const float*)arr->buffers[1];
        snprintf(buf, sizeof(buf), "%g", data[arr->offset + lr]);
        return lean_io_result_mk_ok(lean_mk_string(buf));
    }
    if (fmt[0] == 'u' || fmt[0] == 'U' || fmt[0] == 'z' || fmt[0] == 'Z') {
        // utf8 (u), large_utf8 (U), binary (z), large_binary (Z)
        // Variable-length: offsets in buffer[1], data in buffer[2]
        if (fmt[0] == 'u' || fmt[0] == 'z') {
            const int32_t* offsets = (const int32_t*)arr->buffers[1];
            const char* data = (const char*)arr->buffers[2];
            int64_t idx = arr->offset + lr;
            int32_t start = offsets[idx];
            int32_t end = offsets[idx + 1];
            int32_t len = end - start;
            char* s = malloc(len + 1);
            memcpy(s, data + start, len);
            s[len] = '\0';
            lean_object* obj = lean_mk_string(s);
            free(s);
            return lean_io_result_mk_ok(obj);
        } else {
            const int64_t* offsets = (const int64_t*)arr->buffers[1];
            const char* data = (const char*)arr->buffers[2];
            int64_t idx = arr->offset + lr;
            int64_t start = offsets[idx];
            int64_t end = offsets[idx + 1];
            int64_t len = end - start;
            char* s = malloc(len + 1);
            memcpy(s, data + start, len);
            s[len] = '\0';
            lean_object* obj = lean_mk_string(s);
            free(s);
            return lean_io_result_mk_ok(obj);
        }
    }
    if (fmt[0] == 'b') {  // bool
        const uint8_t* data = (const uint8_t*)arr->buffers[1];
        int64_t idx = arr->offset + lr;
        int val = (data[idx / 8] >> (idx % 8)) & 1;
        return lean_io_result_mk_ok(lean_mk_string(val ? "true" : "false"));
    }
    if (fmt[0] == 'd' && fmt[1] == ':') {  // decimal (d:precision,scale,bitwidth)
        // Parse scale from format string manually (avoid atoi/strtol)
        int scale = 0;
        const char* p = fmt + 2;  // skip "d:"
        while (*p && *p != ',') p++;  // skip precision
        if (*p == ',') {
            p++;
            while (*p >= '0' && *p <= '9') {
                scale = scale * 10 + (*p - '0');
                p++;
            }
        }
        // DuckDB uses 128-bit decimals, stored as two 64-bit ints (little-endian)
        const int64_t* data = (const int64_t*)arr->buffers[1];
        int64_t idx = arr->offset + lr;
        int64_t lo = data[idx * 2];
        // Simple case: just use lo
        double val = (double)lo;
        for (int i = 0; i < scale; i++) val /= 10.0;
        snprintf(buf, sizeof(buf), "%.*f", scale, val);
        return lean_io_result_mk_ok(lean_mk_string(buf));
    }

    // Unknown format - return format string for debug
    return lean_io_result_mk_ok(lean_mk_string(fmt));
}

// | Get cell as Int (0 for null/non-int)
lean_obj_res lean_qr_cell_int(b_lean_obj_arg qr_obj, uint64_t row, uint64_t col, lean_obj_arg world) {
    QueryResult* qr = (QueryResult*)lean_get_external_data(qr_obj);

    int64_t bi, lr;
    if (!find_batch(qr, (int64_t)row, &bi, &lr)) {
        return lean_io_result_mk_ok(lean_int64_to_int(0));
    }

    if ((int64_t)col >= qr->schema.n_children) {
        return lean_io_result_mk_ok(lean_int64_to_int(0));
    }

    struct ArrowArray* batch = &qr->batches[bi];
    struct ArrowArray* arr = batch->children[col];
    const char* fmt = qr->schema.children[col]->format;

    if (is_null(arr, lr)) {
        return lean_io_result_mk_ok(lean_int64_to_int(0));
    }

    int64_t val = 0;
    if (fmt[0] == 'l') {
        val = ((const int64_t*)arr->buffers[1])[arr->offset + lr];
    } else if (fmt[0] == 'i') {
        val = ((const int32_t*)arr->buffers[1])[arr->offset + lr];
    } else if (fmt[0] == 's') {  // int16
        val = ((const int16_t*)arr->buffers[1])[arr->offset + lr];
    } else if (fmt[0] == 'c') {  // int8
        val = ((const int8_t*)arr->buffers[1])[arr->offset + lr];
    }

    return lean_io_result_mk_ok(lean_int64_to_int(val));
}

// | Get cell as Float (0.0 for null/non-float)
lean_obj_res lean_qr_cell_float(b_lean_obj_arg qr_obj, uint64_t row, uint64_t col, lean_obj_arg world) {
    QueryResult* qr = (QueryResult*)lean_get_external_data(qr_obj);

    int64_t bi, lr;
    if (!find_batch(qr, (int64_t)row, &bi, &lr)) {
        return lean_io_result_mk_ok(lean_box_float(0.0));
    }

    if ((int64_t)col >= qr->schema.n_children) {
        return lean_io_result_mk_ok(lean_box_float(0.0));
    }

    struct ArrowArray* batch = &qr->batches[bi];
    struct ArrowArray* arr = batch->children[col];
    const char* fmt = qr->schema.children[col]->format;

    if (is_null(arr, lr)) {
        return lean_io_result_mk_ok(lean_box_float(0.0));
    }

    double val = 0.0;
    if (fmt[0] == 'g') {  // float64/double
        val = ((const double*)arr->buffers[1])[arr->offset + lr];
    } else if (fmt[0] == 'f') {  // float32
        val = ((const float*)arr->buffers[1])[arr->offset + lr];
    }

    return lean_io_result_mk_ok(lean_box_float(val));
}

// | Check if cell is null
lean_obj_res lean_qr_cell_is_null(b_lean_obj_arg qr_obj, uint64_t row, uint64_t col, lean_obj_arg world) {
    QueryResult* qr = (QueryResult*)lean_get_external_data(qr_obj);

    int64_t bi, lr;
    if (!find_batch(qr, (int64_t)row, &bi, &lr)) {
        return lean_io_result_mk_ok(lean_box(1));  // out of bounds = null
    }

    if ((int64_t)col >= qr->schema.n_children) {
        return lean_io_result_mk_ok(lean_box(1));
    }

    struct ArrowArray* batch = &qr->batches[bi];
    struct ArrowArray* arr = batch->children[col];

    return lean_io_result_mk_ok(lean_box(is_null(arr, lr) ? 1 : 0));
}
