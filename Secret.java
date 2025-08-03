import java.util.function.Function;

public class Secret {
    public static void main(final String[] args) {
        System.console().format("%s%n", String.valueOf(new Object() {
            public String toString() {
                var q = java.util.List.of("6f732e6e616d65", "66696c652e736570617261746f72", "6f732e61726368");
                Function<String, byte[]> one = java.util.HexFormat.of()::parseHex;
                Function<byte[], String> two = String::new;
                Function<String, String> three = System::getProperty;
                String[] a = q.stream().map(one.andThen(two.andThen(three)))
                        .toArray(String[]::new);
                return String.format("%s\u0020%s\u0020%s", a[0], a[1], a[2]);
            }
        }));
    }
}
