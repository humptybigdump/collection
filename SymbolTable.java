import java.util.Stack;

/**
 * Helper class used for analyzing name uses/definitions during
 * a compilers semantic phase.
 */
public class SymbolTable {
	public static class Scope {
		private int   oldTop;
		private Scope parent;

		protected Scope(Scope parent, int oldTop) {
			this.parent = parent;
			this.oldTop = oldTop;
		}
	}

	private static class StackEntry {
		private Symbol     symbol;
		private Definition oldDefinition;
		private Scope      oldScope;

		public StackEntry(Symbol symbol, Definition oldDefinition, Scope oldScope) {
			this.symbol        = symbol;
			this.oldDefinition = oldDefinition;
			this.oldScope      = oldScope;
		}
	}

	private Stack<StackEntry> stack        = new Stack<StackEntry>();
	private Scope             currentScope = new Scope(null, 0);

	/** opens a new scope inside the current scope */
	public void enterScope() {
		currentScope = new Scope(currentScope, stack.size());
	}

	/**
	 * leave scope entered by enterScope.
	 * You must leave scopes in exactly the reverse order you entered them!
	 */
	public void leaveScope() {
		assert currentScope.parent != null;

		while (stack.size() > currentScope.oldTop) {
			StackEntry entry = stack.pop();
			entry.symbol.setCurrentDefinition(entry.oldScope, entry.oldDefinition);
		}
		currentScope = currentScope.parent;
	}

	/** add Definition for a symbol to current Scope */
	public void enterDefinition(Symbol symbol, Definition definition) {
		StackEntry entry = new StackEntry(symbol, symbol.getCurrentDefinition(), symbol.getScope());
		stack.push(entry);
		symbol.setCurrentDefinition(currentScope, definition);
	}

	/** returns definition in current or outer scopes */
	public Definition getCurrentDefinition(Symbol symbol) {
		return symbol.getCurrentDefinition();
	}

	/**
	 * returns true if symbol has a definition in the current scope
	 */
	public boolean definitionInCurrentScope(Symbol symbol) {
		return symbol.getScope() == currentScope;
	}
}
