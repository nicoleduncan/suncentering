/*
The First iteration of c++ sun centering code, I don't expect it to work the first time.
*/

#include <math.h>
#include <stdio.h>
#include <iostream>
#include <stdlib.h>
#include <string.h>
#include <fstream>


using namespace std;

/*************************/
/* So you have to delcare all your functions up here... tricky*/
bool scanbox();


/* ------------------------------------------------------------------------------------ */
/* ------------------------------------------------------------------------------------ */

int main() {

    cout << "It's Aliiiiive" << endl;
    
    bool a;
    
    a = scanbox();
    cout << "Just checking to see if return; works:" << endl;
    cout << a << endl;
    return 0;
}

/* ------------------------------------------------------------------------------------ */
/* ------------------------------------------------------------------------------------ */

bool scanbox () {
    // Let's organize this by steps:
    // Step 1:
    // Open the file
    ifstream::pos_type size;
    char * temp;
    ifstream myFile ("Sun_Images_000000.bmp", ios::in | ios::binary);
    if (myFile.is_open()) {

        myFile.seekg (0, ios::end);
        size = myFile.tellg();
        myFile.seekg (0, ios::beg);
        
        temp = new char[size];
        
        myFile.seekg(0,ios::beg);
        myFile.read(temp, size);
        
        myFile.close();
        cout << "The solar bmp image is "<< temp <<" elements big"<< endl;
        cout << "The solar bmp should be 144129 elements long" << endl;
        delete[] temp;
    }
    myFile.close();

    // Step 2: 
    // Read the file
    // Step 3:
    // Flip the file if necessary
    // Step 4:
    // Set the threshold
    // Step 5:
    // Count the rows to hit the sun
    // Step 6:
    // Make boundary regions
    // Step 7:
    // Crop the image based on the boundary regions
    // Step 8:
    // Return cropped image
    return 0;
}