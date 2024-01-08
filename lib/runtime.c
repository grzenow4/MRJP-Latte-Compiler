#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BYTE_SIZE 8
#define CHECK(x)                                  \
        if ((x) == NULL) {                        \
            fprintf(stderr, "Error in malloc\n"); \
            exit(1);                              \
        }

typedef const char* string;
typedef long long int64;
typedef struct Array {
    int64 length;
    void *arr;
} Array;

void printInt(int64 n) {
    printf("%lld\n", n);
}

void printString(string s) {
    printf("%s\n", s);
}

void error() {
    fprintf(stderr, "runtime error\n");
    exit(1);
}

int64 readInt() {
    char *str = NULL;
    size_t size = 0;

    ssize_t read = getline(&str, &size, stdin);

    if (read == -1) {
        fprintf(stderr, "Error reading input\n");
        exit(1);
    }

    return strtoll(str, NULL, 10);
}

string readString() {
    char *str = NULL;
    size_t size = 0;

    ssize_t read = getline(&str, &size, stdin);

    if (read == -1) {
        fprintf(stderr, "Error reading input\n");
        exit(1);
    }

    if (read > 0 && str[read - 1] == '\n') {
        str[read - 1] = '\0';
    }

    return str;
}

string __concatString(string s1, string s2) {
    char *res = malloc(strlen(s1) + strlen(s2) + 1);
    CHECK(res);
    strcpy(res, s1);
    strcat(res, s2);
    return res;
}

void* __allocArray(int64 length) {
    Array *res = malloc(sizeof(Array));
    CHECK(res);
    res->length = length;
    res->arr = malloc(length * BYTE_SIZE);
    CHECK(res->arr);
    return res;
}

void* __allocClass(int64 length) {
    void *res = malloc(length * BYTE_SIZE);
    CHECK(res);
    return res;
}
