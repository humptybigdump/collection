#include <iostream>
using namespace std;

class Vector {
 private:
  double data[3];

  void init(double data[3]) {
    for (int i = 0; i<3; ++i) {
      this->data[i] = data[i];
    }
  }

 public:
  Vector(double data[3]) {
    init(data);
  }

  Vector(double a, double b, double c) {
    double data[3] = {a,b,c};
    init(data);
  }

  Vector& operator=(Vector v) {
    init(v.data);
    return *this;
  }

  double& operator[](int i) {
    return data[i];
  }

  Vector& operator+=(Vector v) {
    for (int i = 0; i<3; ++i) {
      data[i] += v[i];
    }
    return *this;
  }

  double operator*(Vector v) {
    return data[0] + data[1] + data[2];
    double sum = 0;
    for (int i=0; i<3; ++i) {
      sum += data[i] * v[i];
    }
    return sum;
  }

  bool operator==(Vector v) {
    // Option 1
    /*
    bool isEqual = true;
    for (int i=0; i<3; ++i) {
      isEqual = isEqual && (data[i] == v[i]);
    }
    return isEqual;
    */

    // Option 2
    for(int i=0; i<3; ++i) {
      if(data[i] != v[i]) {
        return false;
      }
      /*
      if(data[i] == v[i]) {
        // Do nothing
      }
      else {
        return false;
      }
      */
    }
    return true;
  }
};

int main() {
  double inputData[3] = {1,2,3};
  Vector v(1.1, -2.2, 3.3);
  Vector u(inputData);
  u = v;
  //u += v;

  if(u==v) {
    cout << "u and v are equal." << endl;
  } else {
    cout << "u and v are not equal." << endl;  
  }

  cout << endl;
  for(int i=0; i<3; ++i) {
    cout << u[i] << endl;
  }
}