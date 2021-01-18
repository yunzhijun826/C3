import Analyser.Analyser;
import Tokenizer.StringIter;
import Tokenizer.Tokenizer;
import error.CompileError;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class c0test {
    public static void main(String[] args) throws Exception {
        Scanner sc = new Scanner(new File(args[0]));
        StringIter it = new StringIter(sc);
        Tokenizer tokenizer = new Tokenizer(it);
        Analyser analyser = new Analyser(tokenizer);
        analyser.analyseProgram(args[1]);
    }
}
