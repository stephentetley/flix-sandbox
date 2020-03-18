package flix.runtime.library;


import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class SystemPrimitives {

    /// This is a "vargs" in Java
    public static Path pathOf1(String path) {
        return Path.of(path);
    }

    // Stub function for `toString` as it is an interface method.
    public static String pathToString(Path path) {
        return path.toString();
    }

    // Stub function for `normalize` as it is an interface method.
    public static Path pathNormalize(Path path) {
        return path.normalize();
    }

    // Stub function for `isAbsolute` as it is an interface method.
    public static boolean pathIsAbsolute(Path path) {
        return path.isAbsolute();
    }

    // Stub function for `getRoot` as it is an interface method.
    // Note - may return null.
    public static Path pathGetRoot(Path path) {
        return path.getRoot();
    }

    // Stub function for `getParent` as it is an interface method.
    // Note - may return null.
    public static Path pathGetParent(Path path) {
        return path.getParent();
    }

    // Stub function for `getFileName` as it is an interface method.
    // Note - may return null.
    public static Path pathGetFileName(Path path) {
        return path.getFileName();
    }

    // Stub function for `resolve` as it is an interface method.
    public static Path pathResolve(Path path, Path other) {
        return path.resolve(other);
    }

    // Stub function for `getNameCount` as it is an interface method.
    public static int pathGetNameCount(Path path) {
        return path.getNameCount();
    }

    // Stub function for `getNameCount` as it is an interface method.
    // Note - may throw an exception.
    public static Path pathGetName(Path path, int index) throws Exception {
        return path.getName(index);
    }

    public static Path currentDirectory() {
        Path currentDir = Path.of(".");
        return currentDir.normalize().toAbsolutePath();
    }

    public static void writeString(Path path, String content) throws Exception {
        Files.writeString(path, content);
        return;
    }

    public static String readString(Path path) throws Exception {
        try {
            String s = Files.readString(path);
            return s;
        } catch (IOException ex) {
            ex.printStackTrace();
            throw new IOException();
        }
    }

    public static String readStringUTF16le(Path path) throws Exception {
        try {
            Charset cs = Charset.forName("UTF-16LE");
            String s = Files.readString(path, cs);
            return s;
        } catch (IOException ex) {
            ex.printStackTrace();
            throw new IOException();
        }
    }
}
