package Analyser;
import Tokenizer.Tokenizer;
import Tokenizer.StringIter;
import error.CompileError;
import org.junit.Test;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class AnalyserTest {
    private Tokenizer init(){
        File f = new File("/Users/lyx/Desktop/c0-compiler/Analysetest.txt");
        Scanner sc = null;
        try {
            sc = new Scanner(f);
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
        StringIter x = new StringIter(sc);
        Tokenizer tokenizer = new Tokenizer(x);
        return tokenizer;
    }
    @Test
    public void TestlexUInt() throws CompileError {
        Tokenizer tokenizer = init();
        Analyser analyser = new Analyser(tokenizer);
    }
}
