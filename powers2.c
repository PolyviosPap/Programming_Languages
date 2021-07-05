/****************************************
* Programming Languages, 2019-2020   	*
* Set 1, Exercise 2: powers2     	*
* Polyvios Papakonstantinou 03114892	*
****************************************/

#include <stdio.h>

/*
 * A "safe" and more efficient way to calculate powers.
 * Taken from: [https://stackoverflow.com/a/101613/12785539]
 */
int customPow(int base, int exp) {
    
    int result = 1;
    
    while (1) {
        if (exp & 1) {
            result *= base;
        }
        exp >>= 1;
        
        if (!exp) {
            break;
        }
        base *= base;
    }

    return result;
}


/*
 * The below function is inspired by this article: [https://www.geeksforgeeks.org/represent-n-as-the-sum-of-exactly-k-powers-of-two-set-2/]
 *
 */
void powersOf2(int N, int K) {
	
	int sum = K, arr[K], i, counter, exp;

	for (i = 0; i < K; ++i) {
		arr[i] = 1;
	}

	for (i = K-1; i >= 0; --i) {
		while (sum + arr[i] <= N) {
			sum += arr[i];
			arr[i] *= 2;
		}
	}

	//We're done, we need to format our answer and print it.
	if (sum != N) {
		printf("[]\n");
	} else {
		printf("[");
		
		counter = exp = i = 0;
		while (i < K) {
			if (customPow(2, exp) == arr[i]) {
				++counter;
				++i;
			} else {
				printf("%ld, ", counter);
				++exp;
				counter = 0;
			}
		}
		printf("%ld]\n", counter);
	}
}

/************************************************************************************************************************************/

int main(int argc, char *argv[]) {
	
	int T, i, N, K;

	FILE * txt;
	txt = fopen(argv[1], "r");
	
	if (txt == NULL) {
		printf("There was an error trying to open the file.\n");
		return 0;
	}

	fscanf(txt, "%d", &T);

	/*
	 * Scan every pair of integers in the file
	 * and call powersOf2() for them.
	 */
	for (i = 0; i < T; ++i) {
		fscanf(txt, "%li", &N);
		fscanf(txt, "%li", &K);
		
		powersOf2(N, K);
	}

	fclose(txt);

	return 1;
}