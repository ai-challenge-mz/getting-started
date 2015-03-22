#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>

struct planet {
	int id;
	float x,y;
	int production;
	int owner;
	int population;
	struct planet *next;
};

struct world {
	struct planet *first;
	int myId;
};

struct planet *addPlanet(char line[], struct planet *current) {
	struct planet *np;
	np = (struct planet *) malloc(sizeof(struct planet));
	np->next = current;
	sscanf(line, "P %d %f %f %d %d %d", &np->id, &np->x, &np->y, 
						&np->production, &np->owner, &np->population);
	return np;
}

struct world *readworld() {
	struct world *result;
	result = (struct world *) malloc(sizeof(struct world));
	result -> first = NULL;
	result -> myId = 0;
	char line[200];
	do {
		fgets(line, 200, stdin);
		if (!strlen(line)) continue;
		if (line[0] == 'Y') sscanf(line, "Y %d", &result->myId);
		if (line[0] == 'P') result->first = addPlanet(line, result->first);
	} while (strlen(line)>2 || line[0]!='.'); // "." or ".\n"

	if (result->myId == 0) {free(result); return NULL;}
	else return result;
}

void freePlanets(struct planet *p) {
	if (p == NULL) return;
	freePlanets(p->next);
	free(p);
}

void freeWorld(struct world *w) {
	freePlanets(w->first);
	free(w);
}

void upgradeMe(struct planet *p, int id) {
	if (p == NULL) return;
	if (p->owner == id && p->population > pow(2, p->production) && rand()%2 == 0) {
		printf("B %d\n", p->id);
	}
	upgradeMe(p->next, id);
}

int takeTwoShips(struct planet *p, int id) {
	if (p == NULL) return 0;
	if (p->owner == id && p->population > 1 && rand()%11 == 0) {
		p->population -= 2;
		return p->id;
	} else return takeTwoShips(p->next, id);
}

int findDest(struct planet *p, int id) {
	if (p == NULL) return 0;
	if (p->owner != id && rand()%3 == 0) {
		return p->id;
	} else return findDest(p->next, id);
}

void takeTurn(struct world *w) {
	// printf("F 1 2 3") -- to send 3 ships from the fist to the second planet
	int myPlanet = takeTwoShips(w->first, w->myId);
	int notMyPlanet = findDest(w->first, w->myId);

	if (myPlanet != 0 && notMyPlanet != 0) {
		printf("F %d %d 2\n", myPlanet, notMyPlanet);
	}
	upgradeMe(w->first, w->myId);
	printf(".\n");
}

int main(void) {
	srand(time(NULL));
	struct world *lw;
	while ((lw = readworld())!= NULL) {
		takeTurn(lw);
		freeWorld(lw);
		fflush(stdout);
	}
	return 0;
}
