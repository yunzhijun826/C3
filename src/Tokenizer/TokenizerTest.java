package Tokenizer;
import error.TokenizeError;
import org.junit.Test;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;



public class TokenizerTest {
    private Tokenizer init(){
        File file = new File("/Users/lyx/Desktop/c0-compiler/Analysetest.txt");
        Scanner sc = null;
        try {
            sc = new Scanner(file);
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
        StringIter it = new StringIter(sc);
        Tokenizer tokenizer = new Tokenizer(it);
        return tokenizer;
    }

    @Test
    public void TestlexUInt() throws TokenizeError {
        Tokenizer tokenizer = init();
        Token toke=null;
        toke = tokenizer.nextToken();
        System.out.println(toke+" "+toke.getEndPos());
        while(toke.getTokenType() != TokenType.EOF){
            toke = tokenizer.nextToken();
            System.out.println(toke+" "+toke.getEndPos());
        }
    }

}
