import java.io.IOException;
import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

enum TokenType {
    EOF(null),
    LAMBDA("\\\\"),
    LPAREN("\\("),
    RPAREN("\\)"),
    DOT("\\."),
    IDENT("[a-zA-Z]+[a-zA-Z0-9]*", true),
    WHITESPACE("\\s+");

    /**
     * The regular expression matching character sequencing corresponding to this
     * TokenType.
     */
    public final String regexp;

    /**
     * Shall a token of this type remember the characters matched during lexing?
     */
    public final boolean rememberCharacters;

    TokenType(String regexp, boolean rememberCharacters) {
        this.regexp = regexp;
        this.rememberCharacters = rememberCharacters;
    }

    TokenType(String regexp) {
        this(regexp, false);
    }

    static final TokenType[] NOEOF = { LAMBDA, LPAREN, RPAREN, DOT, IDENT, WHITESPACE };
    static {
        assert NOEOF.length == TokenType.values().length - 1;
    }

    public static final Pattern TOKENPATTERN;
    static {
        final StringBuilder tokenPattern = new StringBuilder();
        String sep = "";
        for (TokenType t : TokenType.NOEOF) {
            tokenPattern.append(String.format("%s(?<%s>%s)", sep, t.name(), t.regexp));
            sep = "|";
        }
        TOKENPATTERN = Pattern.compile(tokenPattern.toString());
    }

}

public final class LambdaSimpleParser {
    private Lexer lexer;

    LambdaSimpleParser(String input) {
        lexer = new Lexer(input);
    }

    static ParseError error() {
        throw new ParseError();
    }

    void printTokens() {
        do {
            System.out.print(lexer.current + ", ");
        } while (lexer.lex());
    }

    Expr parseApp() {
        Expr l = parseAbstr();
        return parseAppList(l);
    }

    private Expr parseAbstr() {
        return null;
        // TODO
    }

    private Expr parseAppList(Expr left) {
        return null;
        // TODO
    }

    private Expr parseAtom() {
        return null;
        // TODO
    }

    private Expr parseVar() {
        return null;
        // TODO
    }

    public static void main(String[] args) throws IOException {
        switch (args.length) {
            case 0:
                test();
                break;
            case 1:
                expectCorrect(readFile(Paths.get(args[0]), StandardCharsets.UTF_8));
                break;
            default:
        }
    }

    private static void expectCorrect(String correct) {
        LambdaSimpleParser parser = new LambdaSimpleParser(correct);

        System.out.println("input:");
        System.out.print(correct);
        System.out.print("tokens: ");
        parser.printTokens();
        System.out.println();

        // parse it
        parser = new LambdaSimpleParser(correct);
        final Expr v = parser.parseApp();

        // If tokens remain, the full input string wasn't a legal type, so we signal an
        // error
        if (parser.lexer.current.type != TokenType.EOF)
            throw error();

        System.out.println("parsed: " + v);
    }

    static String readFile(Path path, Charset encoding) throws IOException {
        byte[] encoded = Files.readAllBytes(path);
        return encoding.decode(ByteBuffer.wrap(encoded)).toString();
    }

    private static void test() throws IOException {
        List<String> parsable = new LinkedList<String>();
        for (Path p : Files.newDirectoryStream(Paths.get("data"), "*.{txt}")) {
            parsable.add(readFile(p, StandardCharsets.UTF_8));
        }
        for (String correct : parsable) {
            expectCorrect(correct);
            System.out.println();
        }
    }
}

final class Lexer {
    Token current;
    private final Matcher tokenMatcher;

    public Lexer(String input) {
        if (input == null)
            throw new IllegalArgumentException();

        tokenMatcher = TokenType.TOKENPATTERN.matcher(input);
        lex();
    }

    /**
     * Sets the current input token to the next token read from input.
     *
     * @return true, iff another input token other than {@link TokenType#EOF} was
     *         read.
     *
     * @throws IllegalStateException iff no tokenMatcher was set (see
     *                               {@link #setInput(String)}).
     * @throws LexerError            iff the input cannot be correctly tokenized
     *                               (e.g., if it contains characters not
     *                               corresponding to tokens).
     */
    public boolean lex() {
        if (tokenMatcher == null)
            throw new IllegalStateException();

        do {
            final int previouslyMatchedCharacterIndex = (current == null) ? 0 : tokenMatcher.end();

            if (tokenMatcher.find()) {
                // Abort if tokenMatcher had to skip characters in order to find the next valid
                // token
                if (tokenMatcher.start() > previouslyMatchedCharacterIndex)
                    throw new LexerError();

                // The characters matched correspond to exactly one new Token.
                // Generate this, possibly annotating it with the matched character sequence
                boolean matchingTokenFound = false;
                for (TokenType t : TokenType.NOEOF) {
                    boolean tokenMatches = tokenMatcher.group(t.name()) != null;

                    // The token, as defined in TokenType, are "mutually exclusive":
                    assert !(matchingTokenFound && tokenMatches);

                    matchingTokenFound |= tokenMatches;

                    if (tokenMatches) {
                        current = new Token(t, tokenMatcher.group(t.name()));
                    }
                }
                assert matchingTokenFound;

            } else {
                // Abort if no more tokens could be found, but the input has not been fully read
                // yet.
                if (previouslyMatchedCharacterIndex < tokenMatcher.regionEnd())
                    throw new LexerError();

                current = new Token(TokenType.EOF);
                return false;
            }
        } while (current.type == TokenType.WHITESPACE);

        return true;
    }
}

final class Token {
    public final String text;
    public final TokenType type;

    public Token(TokenType type) {
        this(type, null);
    }

    public Token(TokenType type, String text) {
        if (type.rememberCharacters && (text == null))
            throw new IllegalArgumentException();

        this.type = type;
        this.text = type.rememberCharacters ? text : null;
    }

    @Override
    public String toString() {
        return "<" + type + (text != null ? "(" + text + ")" : "") + ">";
    }

}

abstract class Expr {
}

class Var extends Expr {
    private String s;

    public Var(String name) {
        if (name == null || !name.matches(TokenType.IDENT.regexp))
            throw new IllegalArgumentException();
        this.s = name;
    }

    @Override
    public String toString() {
        return "\"" + s + "\"";
    }
}

class Abstr extends Expr {
    private String name;
    private Expr body;

    public Abstr(String name, Expr body) {
        if (name == null || !name.matches(TokenType.IDENT.regexp))
            throw new IllegalArgumentException();
        this.name = name;
        this.body = body;
    }

    @Override
    public String toString() {
        return "\\\\" + name + ". " + body.toString();
    }
}

class App extends Expr {
    private Expr left;
    private Expr right;

    public App(Expr left, Expr right) {
        this.left = left;
        this.right = right;
    }

    @Override
    public String toString() {
        return "(" + left.toString() + ")" + "(" + right.toString() + ")";
    }
}

class LexerError extends RuntimeException {
    private static final long serialVersionUID = -6554542041382870784L;
}

class ParseError extends RuntimeException {
    private static final long serialVersionUID = -2687440417554884854L;
}
