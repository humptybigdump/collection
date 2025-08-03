class TryFinally {
	public static int test1() {
		try {
			return 1;
		} finally {
			return 2;
		}
	}

	public static int test2() {
		int x = 1;
		try {
			return x;
		} finally {
			x = 2;
		}
	}

	public static int test3() {
		while (true) {
			try {
				return 1;
			} finally {
				break;
			}
		}
		return 2;
	}

	public static int test4() {
		for (int i = 10; i < 20; ) {
			try {
				return ++i;
			} finally {
				continue;
			}
		}
		return 2;
	}

	public static int test5() {
		a: {
			try {
				return 1;
			} finally {
				break a;
			}
		}
		return 2;
	}

	public static void main(String[] args) {
		System.out.println(test1());
		System.out.println(test2());
		System.out.println(test3());
		System.out.println(test4());
		System.out.println(test5());
	}
}
