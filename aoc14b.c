#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define NUM_ELEMENTS(a) (sizeof(a)/sizeof((a)[0]))

//#define DPRINTF(s, args...) fprintf(stderr, s "\n", args)
#define DPRINTF(s, args...) /**/

// It is a sin to use global variables.
static const char *goal;
static size_t goallen;
static const int MAX_SCOREBOARD = 1024*1024*1024;
static char *scoreboard;
static char *p;
static int e[] = { 0, 1 };         /* elf positions */

static inline int scoreboard_len() {
    return p - scoreboard;
}

void extend() {
    int newscore = (scoreboard[e[0]] - '0' +
                    scoreboard[e[1]] - '0');
    DPRINTF("newscore %d", newscore);
    int tens = newscore / 10;
    if (tens) {
        DPRINTF("tens %d", tens);
        *p = tens + '0';
        p++;
    }
    int ones = newscore % 10;
    DPRINTF("ones %d", ones);
    *p = ones + '0';
    p++;

    // extend scoreboard somewhat sloppily until it can accomidate
    // sloppy memcmp
    for (int i = 0; i < NUM_ELEMENTS(e); i++) {
        int cscore = scoreboard[e[i]] - '0';
        e[i] = (e[i] + cscore + 1) % (scoreboard_len());
    }
    DPRINTF("p-4=%s", p-4);
    DPRINTF("scoreboard is now %s", scoreboard);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "one arg plz k thx bye\n");
        return 1;
    }

    goal = argv[1];
    DPRINTF("goal %s", goal);
    goallen = strlen(goal);
    scoreboard = calloc(sizeof(char), MAX_SCOREBOARD);
    strcpy(scoreboard, "37");
    p = scoreboard + strlen(scoreboard);
    DPRINTF("p-2=%s", p-2);

    while (scoreboard_len() < goallen + 1) {
        // this might be bogus if the goal is right in the beginning.
        extend();
    }

    while (scoreboard_len() < MAX_SCOREBOARD &&
           memcmp(p - goallen, goal, goallen) &&
           memcmp(p - goallen - 1, goal, goallen)) {
        if (scoreboard_len() > 20) {
            DPRINTF("scoreboard last 20: %s\n", p - 20);
        }

        extend();

        DPRINTF("ends %d", memcmp(p - goallen, goal, goallen));
        DPRINTF("ends-1 %d", memcmp(p - goallen - 1, goal, goallen));
    }

    if (scoreboard_len() == MAX_SCOREBOARD) {
        printf("overflowed allocation\n");
    }

    if (0 == memcmp(p - goallen, goal, goallen)) {
        printf("%lu\n", scoreboard_len() - goallen);
    } else if (0 == memcmp(p - goallen - 1, goal, goallen)) {
        printf("%lu\n", scoreboard_len() - goallen - 1);
    } else {
        printf("uhoh\n");
    }

    if (scoreboard_len() < 20) {
        printf("scoreboard: %s\n", scoreboard);
    } else {
        printf("scoreboard last 20: %s\n", p - 20);
    }

    return 0;
}
