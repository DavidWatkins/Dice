#include <stdio.h>
#include <stdlib.h>

#define INIT_SIZE 100

struct s {
	int x;
	int y;
};

char* input() {
	int initial_size = INIT_SIZE;
	char* str = malloc(initial_size);
	int index = 0;
	char tmp = '0';
	while((tmp = getchar() )!= '\n') {
		if(index >= initial_size - 1) {
			str = realloc(str, initial_size *= 2);
		}
		str[index++] = tmp;
	}
	str[index] = '\0';
	return str;
}

void rec_init(long* arr, int curr_offset, int* static_offsets, int* indexes, int* dims, int dimc, int dim_curr) {

	//Assign length
	arr[curr_offset] = dims[dim_curr];

	if(dim_curr + 1 >= dimc)
		return;

	//Determine the static offset and the dynamic offset
	int static_offset = static_offsets[dim_curr];
	int dynamic_offset = 0;
	for(int i = 0; i < dim_curr; i++) {
		int tmp = indexes[i];
		for(int j = i + 1; j <= dim_curr; j++) {
			tmp *= dims[j];
		}
		dynamic_offset += tmp;
	}

	//Iterate through position and iniitalize subarrays
	//Set local indexes to pointers to the subarrays
	for(int i = 0; i < dims[dim_curr]; i++) {
		int offset = (static_offset + (dynamic_offset + i) * (dims[dim_curr + 1] + 1));

		long* sub = arr + offset;
		arr[curr_offset + 1 + i] = (long) sub;

		indexes[dim_curr] = i;
		rec_init(arr, offset, static_offsets, indexes, dims, dimc, dim_curr + 1);
	}
}

long* init_arr(int* dims, int dimc) {

	int static_offsets[dimc];
	int total = 0;
	for(int i = 0; i < dimc; i++) {
		static_offsets[i] = 1;
		for(int j = 0; j < i; j++) {
			static_offsets[i] *= dims[j];
		}
		static_offsets[i] *= dims[i] + 1;
		static_offsets[i] += total;
		total = static_offsets[i];
	}

	int indexes[dimc];
	for(int i = 0; i < dimc; i++) {
		indexes[i] = 0;
	}
	
	//Get total length of array
	int length = 0;
	for(int i = 0; i < dimc; i++) {
		int tmp = 1;
		for(int j = i - 1; j >= 0; j--) {
			tmp *= dims[j];
		}
		tmp *= dims[i] + 1;
		length += tmp;
	}

	//Malloc array
	long* arr = malloc(length);

	//Set all values to 0 initially
	for(int i = 0 ; i < length; i++) {
		arr[i] = 0;
	}

	//Initialize the entire array
	rec_init(arr, 0, static_offsets, indexes, dims, dimc, 0);

	return arr;
}

// int main() {

// 	//Array creation
// 	int dims[5] = {2, 3, 4, 5, 6};
// 	int dimc = 5;

// 	long* arr = init_arr(dims, dimc);

// 	//Get total length of array
// 	int length = 0;
// 	for(int i = 0; i < dimc; i++) {
// 		int tmp = 1;
// 		for(int j = i - 1; j >= 0; j--) {
// 			tmp *= dims[j];
// 		}
// 		tmp *= dims[i] + 1;
// 		length += tmp;
// 	}
	
// 	for(int i = 0; i < length; i++) {
// 		printf("val: %ld | addr: %ld\n", arr[i], (long) arr + i);
// 	}
// 	printf("\n");
// }