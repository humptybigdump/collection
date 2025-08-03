package templatemethod;

public abstract class BubbleSorter {

	public BubbleSorter() {
		super();
	}
	
	protected abstract int getLength();

	protected abstract void swapElements(int index1, int index2);

	protected abstract int compareElements(int index1, int index2);

	/**
	 * Sort using bubblesort.
	 */
	final protected void sortAscending() {
		if (getLength() <= 1) {
			return;
		}
		
		for (int nextToLast = getLength() - 2; nextToLast >= 0; nextToLast--) {
			for (int index = 0; index <= nextToLast; index++) {
				if (compareElements(index, index + 1) > 0)
					swapElements(index, index + 1);
			}
		}
	}
}
