/*
 * C version of the perl version, just for fun.
 * 
 * This may have some off-by-one errors for untested inputs.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define NUM_ELEMENTS(a) (sizeof(a)/sizeof((a)[0]))

// It is a sin to use global variables.
static const char *goal;
static size_t goallen;
static const int MAX_SCOREBOARD = 1024*1024*1024;
static char *scoreboard;
static char *p;
static int e[] = { 0, 1 };         /* elf positions */

static inline unsigned long scoreboard_len() {
    return p - scoreboard;
}

void extend() {
    int newscore = (scoreboard[e[0]] - '0' +
                    scoreboard[e[1]] - '0');
    int tens = newscore / 10;
    if (tens) {
        *p++ = tens + '0';
    }
    int ones = newscore % 10;
    *p++ = ones + '0';

    for (unsigned long i = 0; i < NUM_ELEMENTS(e); i++) {
        int cscore = scoreboard[e[i]] - '0';
        e[i] = (e[i] + cscore + 1) % (scoreboard_len());
    }
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "one arg plz k thx bye\n");
        return 1;
    }

    goal = argv[1];
    goallen = strlen(goal);
    scoreboard = calloc(sizeof(char), MAX_SCOREBOARD);
    strcpy(scoreboard, "37");
    p = scoreboard + strlen(scoreboard);

    while (scoreboard_len() < goallen + 1) {
        /* make sure there's enough so we can memcmp below */
        extend();
    }

    while (scoreboard_len() < MAX_SCOREBOARD &&
           memcmp(p - goallen, goal, goallen) &&
           memcmp(p - goallen - 1, goal, goallen)) {
        extend();
    }

    if (0 == memcmp(p - goallen, goal, goallen)) {
        printf("%lu\n", scoreboard_len() - goallen);
    } else if (0 == memcmp(p - goallen - 1, goal, goallen)) {
        printf("%lu\n", scoreboard_len() - goallen - 1);
    } else {
        printf("uhoh\n");
    }

    return 0;
}
