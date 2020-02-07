//
//  main.c
//  couchbase_minimal
//
//  Created by Heru Susant on 16/01/20.
//  Copyright © 2020 Heru Susant. All rights reserved.
//

#include <stdio.h>
#include <libcouchbase/couchbase.h>
#include <libcouchbase/api3.h>
#include <stdlib.h>
#include <string.h> /* strlen */
#ifdef _WIN32
#define PRIx64 "I64x"
#else
#include <inttypes.h>
#endif

static void
die(lcb_t instance, const char *msg, lcb_error_t err)
{
    fprintf(stderr, "%s. Received code 0x%X (%s)\n",
            msg, err, lcb_strerror(instance, err));
    exit(EXIT_FAILURE);
}
static void
op_callback(lcb_t instance, int cbtype, const lcb_RESPBASE *rb)
{
    fprintf(stderr, "=== %s ===\n", lcb_strcbtype(cbtype));
    if (rb->rc == LCB_SUCCESS)
    {
        fprintf(stderr, "KEY: %.*s\n", (int)rb->nkey, rb->key);
        fprintf(stderr, "CAS: 0x%" PRIx64 "\n", rb->cas);
        if (cbtype == LCB_CALLBACK_GET)
        {
            const lcb_RESPGET *rg = (const lcb_RESPGET *)rb;
            fprintf(stderr, "VALUE: %.*s\n", (int)rg->nvalue, rg->value);
            fprintf(stderr, "FLAGS: 0x%x\n", rg->itmflags);
        }
    }
    else
    {
        die(instance, lcb_strcbtype(cbtype), rb->rc);
    }
}

int main(int argc, const char *argv[])
{
    lcb_error_t err;
    lcb_t instance;
    struct lcb_create_st create_options = { 0 };
    lcb_CMDSTORE scmd = { 0 };
    lcb_CMDGET gcmd = { 0 };
    create_options.version = 3;
    if (argc < 2) {
        fprintf(stderr, "Usage: %s couchbase://host/bucket [ password [ username ] ]\n", argv[0]);
        exit(EXIT_FAILURE);
    }
    create_options.v.v3.connstr = argv[1];
    if (argc > 2) {
        create_options.v.v3.passwd = argv[2];
    }
    if (argc > 3) {
        create_options.v.v3.username = argv[3];
    }
    err = lcb_create(&instance, &create_options);
    // if (err != LCB_SUCCESS) {
    //     die(NULL, "Couldn't create couchbase handle", err);
    // }
    // insert code here...
    printf("Hello, World!\n");
    return 0;
}
