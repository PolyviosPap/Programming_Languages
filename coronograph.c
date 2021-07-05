/****************************************
* Programming Languages, 2019-2020   	*
* Set 1, Exercise 2: coronograph     	*
* Polyvios Papakonstantinou 03114892	*
****************************************/

#include <stdio.h>
#include <stdlib.h>

/*
 * A "safe" version for malloc function,
 * inspired from "Operating Systems" course.
 */
void *safeMalloc(size_t size) {
	
	void *p;

	if ((p = malloc(size)) == NULL) {
		printf("Error 1 at safeMalloc.\nTerminating the program...\n");
		exit(0);
	}
	return p;
}

/*
 * A function for comparing 2 long ints for calling qsort.
 * Taken from: [https://stackoverflow.com/a/26428052/12785539]
 */
int cmpFun(const void * a, const void * b) {
   	
   	if (*(long int*)a - *(long int*)b < 0) {
        return -1;
   	}

    if(*(long int*)a - *(long int*)b > 0) {
        return 1;
    }

    return 0;
}

/*
 * ┌──────┐	    ┌────────┐     ┌────────┐
 * │node 1│ ──► │nghbr 1a│ ──► │nghbr 1b│ ──► ...
 * └──────┘     └────────┘     └────────┘
 *    │
 *    ▼
 * ┌──────┐	    ┌────────┐     ┌────────┐
 * │node 2│ ──► │nghbr 1a│ ──► │nghbr 2b│ ──► ...
 * └──────┘     └────────┘     └────────┘
 *    │
 *    ▼
 *    .
 *    .
 *    .
 */ 

long int N, nodesVisited, cycles, cycleSize;

typedef struct node {
	
	long int id;
	long int numOfNeighbors;
	int visited;
	int partOfCycle;
	struct neighbor *nextNeighbor;

	struct node *nextNode;
} node;

typedef struct neighbor {

	long int id;

	struct neighbor *nextNeighbor;
} neighbor;

/*
 * 'pointerToNode' will be an array of pointers for faster reaching a node
 * of our linked list via its 'id' field.
 * 'previousNode' will be used in our DFS below.
 */
node **pointerToNode;
long int *previousNode;

struct node* createGraph() {
	
	long int i;

	//Create the first node of our graph (the 'head' of our linked list).
	node *hd = safeMalloc(sizeof(struct node));
	hd->id = 1;
	hd->numOfNeighbors = 0;
	hd->visited = 0;
	hd->partOfCycle = 0;
	hd->nextNeighbor = NULL;
	hd->nextNode = NULL;

	pointerToNode[0] = hd;
	node *currentNode = hd;

	//Create the rest and link them all together one by one.
	for (i = 2; i <= N; ++i) {
		node *tmp = safeMalloc(sizeof(node));
		tmp->id = i;
		tmp->numOfNeighbors = 0;
		tmp->nextNeighbor = NULL;
		tmp->nextNode = NULL;

		pointerToNode[i-1] = tmp;
		currentNode->nextNode = tmp;
		currentNode = tmp;
	}

	return hd;
}

void addEdge(long int idA, long int idB) {

	//Create the neighbor object we will add to nodeA...
	neighbor *tmpNbghr = safeMalloc(sizeof(struct neighbor));
	tmpNbghr->id = idB;
	tmpNbghr->nextNeighbor = NULL;

	//...and connect it to the end of its neighbors.
	if (pointerToNode[idA-1]->nextNeighbor == NULL) {
		pointerToNode[idA-1]->nextNeighbor = tmpNbghr;
	} else {
		neighbor *currentNeighbor = pointerToNode[idA-1]->nextNeighbor;

		while (1) {
			if (currentNeighbor->nextNeighbor == NULL) {
				break;
			} else {
				currentNeighbor = currentNeighbor->nextNeighbor;
			}
		}

		currentNeighbor->nextNeighbor = tmpNbghr;
	}

	//The exact same procedure to add nodeA as a neighbor of nodeB.
	tmpNbghr = safeMalloc(sizeof(struct neighbor));
	tmpNbghr->id = idA;
	tmpNbghr->nextNeighbor = NULL;

	if (pointerToNode[idB-1]->nextNeighbor == NULL) {
		pointerToNode[idB-1]->nextNeighbor = tmpNbghr;
	} else {
		neighbor *currentNeighbor = pointerToNode[idB-1]->nextNeighbor;

		while (1) {
			if (currentNeighbor->nextNeighbor == NULL) {
				break;
			} else {
				currentNeighbor = currentNeighbor->nextNeighbor;
			}
		}

		currentNeighbor->nextNeighbor = tmpNbghr;
	}

	++(pointerToNode[idA-1]->numOfNeighbors);
	++(pointerToNode[idB-1]->numOfNeighbors);
}

void findCycle(node *currentNode, long int prev){

	//If we have no neighbors, then the graph is not connected.
	if (currentNode->numOfNeighbors == 0) {
		return;
	}
	
	//We have completly visited this node, return.
	if (currentNode->visited == 2) {
		return;
	}

	//We have partially visited this node, meaning we hit a cycle.
	if (currentNode->visited == 1) {
		++cycles;
		++cycleSize;

		if (cycles > 1) {
			return;
		}

		//Mark the previous node as part of the cycle.
		pointerToNode[prev-1]->partOfCycle = 1;

		//Backtrack all the nodes that are part of the cycle.
		while(prev != (currentNode->id)) {
			prev = previousNode[prev];
			pointerToNode[prev-1]->partOfCycle = 1;
			++cycleSize;
		}

		return;
	}

	//Update its previous node.
	previousNode[currentNode->id] = prev;

	//This node is partially visited.
	currentNode->visited = 1;

	neighbor *tmp = currentNode->nextNeighbor;
	while (1) {
		if (tmp->id != prev) {
			findCycle(pointerToNode[(tmp->id)-1], currentNode->id);
		}

		if (tmp->nextNeighbor != NULL) {
			tmp = tmp->nextNeighbor;
		} else {
			break;
		}
	}

	//Completely visited.
	currentNode->visited = 2;
	++nodesVisited;
}

/*
 * In this function, given a node which belongs to the cycle of the graph,
 * we will calculate the size of its tree.
 */
long int sizeOfTree(node *currentNode, long int parentId) {

	long int ans = 1;

	//Find all the neighbors...
	neighbor *tmp = currentNode->nextNeighbor;

	while (1) {
		//(if we reached a leaf, return 'ans'.)
		if (tmp == NULL) {
			return ans;
		}

		//...which are not parts of the cycle or the parent node and call recursively the function.
		if ((pointerToNode[(tmp->id)-1]->partOfCycle == 0)&&(tmp->id != parentId)) {
			ans = ans + sizeOfTree(pointerToNode[(tmp->id)-1], currentNode->id);
		}

		tmp = tmp->nextNeighbor;
	}
}

/************************************************************************************************************************************/

int main(int argc, char const *argv[]) {

	int T, i;
	long int M, j, nodeA, nodeB;

	FILE *txt;
	txt = fopen(argv[1], "r");

	if (txt == NULL) {
		printf("There was an error trying to open the file.\n");
		return 0;
	}

	fscanf(txt, "%d", &T);

	//For every following graph...
	for (i = 0; i < T; ++i) {
		cycles = nodesVisited = cycleSize = 0;

		fscanf(txt, "%li", &N);
		fscanf(txt, "%li", &M);

		pointerToNode = safeMalloc(sizeof(struct node) * N);
		previousNode = safeMalloc(sizeof(long int) * N);

		//Create the graph of N vertices.
		node *hd = createGraph();

		//Scan and add the graph's edges.
		for (j = 0; j < M; ++j) {
			fscanf(txt, "%li", &nodeA);
			fscanf(txt, "%li", &nodeB);

			addEdge(nodeA, nodeB);
		}

		findCycle(hd, 0);

		/*
		 * If 'cycles' is not equal to 1, we found more than 1 cycles.
		 * If 'nodesVisited' is not equal to N, the graph is not connected.
		 */
		if ((cycles != 1)||(nodesVisited != N)) {
			printf("NO CORONA\n");
			continue;
		}

		long int sizes[cycleSize];
		node *tmp = hd;

		for (j = 0; j < cycleSize; ++j){
			while (1) {
				if (tmp->partOfCycle == 1) {
					sizes[j] = sizeOfTree(tmp, -1);
					tmp = tmp->nextNode;
					break;
				} else {
					tmp = tmp->nextNode;
				}
			}
		}

		qsort(sizes, cycleSize, sizeof(long int), cmpFun);

		printf("CORONA %li\n", cycleSize);
		for (j = 0; j < cycleSize-1; ++j) {
			printf("%li ", sizes[j]);
		}
		printf("%li\n", sizes[cycleSize-1]);
	}
	
	fclose(txt);

	return 1;
}