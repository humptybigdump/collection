struct Foo {
	int    a;
	double b;
};
struct Bar {
	char c[3];
	short z;
};
union blup {
	Foo foo;
	Bar bar;
};
class Cls {
private:
	char c;
public:
	Bar bar;

	void print(void) { printf("%c\n", c); }

	char d;
	blup b;
	char z[1];
};
struct baz {
	Cls classes[11];
	int z;
	char c;
};
