package Tokenizer;
import error.ErrorCode;
import error.TokenizeError;
import util.Pos;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Tokenizer {
    private StringIter it;
    String token="";
    public Tokenizer(StringIter it) {
        this.it = it;
    }

    // 这里本来是想实现 Iterator<Token> 的，但是 Iterator 不允许抛异常，于是就这样了
    /**
     * 获取下一个 Token
     *
     * @return
     * @throws TokenizeError 如果解析有异常则抛出
     */
    public Token nextToken() throws TokenizeError {
        it.readAll();

        // 跳过之前的所有空白字符
        skipSpaceCharacters();

        if (it.isEOF()) {
            return new Token(TokenType.EOF, "", it.currentPos(), it.currentPos());
        }

        char peek = it.peekChar();
        if (Character.isDigit(peek)) {
            return lexUIntOrDouble();
        } else if(peek == '\''){
            return lexCHAR();
        } else if(peek == '"'){
            return lexSTRING();
        }else if (Character.isAlphabetic(peek)) {
            return lexIdentOrKeyword();
        } else if(peek == '_') {
            return lexIdentOrKeyword();
        } else {
            return lexOperatorOrUnknown();
        }
    }

    private Token lexUIntOrDouble() throws TokenizeError{  //判断并返回 无符号整数
        //token.setLength(0);
        token="";
        Pos start = it.currentPos();
        // 请填空：
        // 直到查看下一个字符不是数字为止:
        // -- 前进一个字符，并存储这个字符
        //
        // 解析存储的字符串为无符号整数
        // 解析成功则返回无符号整数类型的token，否则返回编译错误
        //
        // Token 的 Value 应填写数字的值

        while(Character.isDigit(it.peekChar())) {// 直到查看下一个字符不是数字为止:
            token+=it.nextChar();
        }

        if(it.peekChar() == '.'){
            token+=it.nextChar();
            while(Character.isDigit(it.peekChar())) {// 直到查看下一个字符不是数字为止:
                token+=it.nextChar();
            }

            if(it.peekChar() == 'e' || it.peekChar() == 'E'){ //有后面的一串
                token+=it.nextChar();
                if(it.peekChar() == '+' || it.peekChar() == '-'){
                    token+=it.nextChar();
                }

                while(Character.isDigit(it.peekChar())) {// 直到查看下一个字符不是数字为止:
                    token+=it.nextChar();
                }
            }
            double num = Double.valueOf(token.toString());
            Token toke = new Token(TokenType.DOUBLE_LITERAL, num, start, it.currentPos());
            return toke;
        }

        if(token.length() != 0){
            long num = Long.valueOf(token.toString());
            Token toke = new Token(TokenType.UINT_LITERAL, num, start, it.currentPos());
            return toke;
        }
        return null;
    }

    private Token lexSTRING() throws TokenizeError {  //判断并返回 字符串常量
        token="";
        Pos start = it.currentPos();

        it.nextChar();
        while(true){
            if(it.isEOF()){
                throw new TokenizeError(ErrorCode.InvalidInput, it.previousPos());
            }
            char peek = it.peekChar();

            if(peek == '"'){
                it.nextChar();
                Token toke = new Token(TokenType.STRING_LITERAL, token, start, it.currentPos());
                return toke;
            }
            char now = it.nextChar();
            if(now == '\\'){
                lexEscape(now);
            }
            else{
                lexRegular(now);
            }
        }
    }

    private Token lexCHAR() throws TokenizeError {
        token="";
        Pos start = it.currentPos();
        Token toke = null;
        it.nextChar();
        char peek = it.peekChar();

        if(peek != '\'' && peek != '\\'){
            toke = new Token(TokenType.CHAR_LITERAL, it.nextChar(), start, it.currentPos());
        }
        else if(peek == '\\'){
            it.nextChar();
            Pattern pat = Pattern.compile("[\\\\\"'nrt]");
            peek = it.peekChar();
            Matcher mat = pat.matcher(String.valueOf(peek));
            boolean matches = mat.matches();
            char cur ;

            if(matches) {
                if (peek == '\\') {
                    cur = '\\';
                } else if (peek == 'n') {
                    cur = '\n';
                } else if (peek == 't') {
                    cur = '\t';
                } else if (peek == '"') {
                    cur = '"';
                } else if (peek == 'r') {
                    cur = '\r';
                } else if (peek == '\'') {
                    cur = '\'';
                } else {
                    throw new TokenizeError(ErrorCode.InvalidInput, it.previousPos());
                }
                it.nextChar();
                toke = new Token(TokenType.CHAR_LITERAL, cur, start, it.currentPos());
            }
        }
        else{
            throw new TokenizeError(ErrorCode.InvalidInput, it.previousPos());
        }
        if(it.peekChar() != '\''){
            throw new TokenizeError(ErrorCode.InvalidInput, it.previousPos());
        }
        it.nextChar();
        return toke;
    }

    private void lexEscape(char now) throws TokenizeError {
        Pattern pat = Pattern.compile("[\\\\\"'nrt]");
        char peek = it.peekChar();

        Matcher mat = pat.matcher(String.valueOf(peek));
        boolean matches = mat.matches();
        if(matches){
            if(peek == '\\'){
                token+='\\';
            }
            if(peek == 'n'){
                token+='\n';
            }
            if(peek == 't'){
                token+='\t';
            }
            if(peek == '"'){
                token+='"';
            }
            if(peek == 'r'){
                token+='\r';
            }
            if(peek == '\''){
                token+='\'';
            }
            it.nextChar();
        }
        else throw new TokenizeError(ErrorCode.InvalidInput, it.previousPos());
    }

    private void lexRegular(char now) throws TokenizeError {
        Pattern pat = Pattern.compile("[^\"\\\\]");
        Matcher mat = pat.matcher(String.valueOf(now));
        boolean matches = mat.matches();
        if(matches){
            token+=now;
        }
        else throw new TokenizeError(ErrorCode.InvalidInput, it.previousPos());
    }

    private Token lexIdentOrKeyword() throws TokenizeError{
        // 请填空：
        // 直到查看下一个字符不是数字或字母为止:
        // -- 前进一个字符，并存储这个字符
        //
        // 尝试将存储的字符串解释为关键字
        // -- 如果是关键字，则返回关键字类型的 token
        // -- 否则，返回标识符
        //
        // Token 的 Value 应填写标识符或关键字的字符串
        String[] keywords = {"FN_KW", "LET_KW", "CONST_KW", "AS_KW", "WHILE_KW", "IF_KW", "ELSE_KW", "RETURN_KW", "BREAK_KW", "CONTINUE_KW"};
        String[] keywordsReal = {"FN", "LET", "CONST", "AS", "WHILE", "IF", "ELSE", "RETURN", "BREAK", "CONTINUE"};
        int len1=keywords.length;
        Pos start = it.currentPos();
        //token.setLength(0);
        token="";

        while(Character.isLetterOrDigit(it.peekChar())||it.peekChar()=='_'){
            token+=it.nextChar();
        }
        if(token.length() != 0){
            for(int i = 0; i < len1; i++){
                if(token.toLowerCase().equals(keywordsReal[i].toLowerCase())){
                    Token toke = new Token(TokenType.valueOf(keywords[i]), token, start, it.currentPos());
                    return toke;
                }
            }
            Token toke = new Token(TokenType.IDENT, token, start, it.currentPos());
            return toke;
        }
        return null;


    }

    private Token lexOperatorOrUnknown() throws TokenizeError {
        switch (it.nextChar()) {
            case '+':
                return new Token(TokenType.PLUS, '+', it.previousPos(), it.currentPos());

            case '-':
                // 填入返回语句
                if(it.peekChar() == '>'){
                    it.nextChar();
                    return new Token(TokenType.ARROW, "->", it.previousPos(), it.currentPos());
                }
                return new Token(TokenType.MINUS, '-', it.previousPos(), it.currentPos());


            case '*':
                // 填入返回语句
                //throw new Error("Not implemented");
                return new Token(TokenType.MUL, '*', it.previousPos(), it.currentPos());

            case '/':
                // 填入返回语句
                //throw new Error("Not implemented");
                if(it.peekChar() == '/'){
                    while (it.nextChar() != '\n'){
                        if(it.isEOF()){
                            throw new TokenizeError(ErrorCode.InvalidInput, it.previousPos());
                        }
                    }
                    return nextToken();
                }
                return new Token(TokenType.DIV, '/', it.previousPos(), it.currentPos());

            // 填入更多状态和返回语句
            case '=':
                if(it.peekChar() == '='){
                    it.nextChar();
                    return new Token(TokenType.EQ, "==", it.previousPos(), it.currentPos());
                }
                return new Token(TokenType.ASSIGN, '=', it.previousPos(), it.currentPos());

            case '!':
                if(it.peekChar() == '='){
                    it.nextChar();
                    return new Token(TokenType.NEQ, "!=", it.previousPos(), it.currentPos());
                }
                else throw new TokenizeError(ErrorCode.InvalidInput, it.previousPos());

            case '>':
                if(it.peekChar() == '='){
                    it.nextChar();
                    return new Token(TokenType.GE, ">=", it.previousPos(), it.currentPos());
                }
                return new Token(TokenType.GT, '>', it.previousPos(), it.currentPos());

            case '<':
                if(it.peekChar() == '='){
                    it.nextChar();
                    return new Token(TokenType.LE, "<=", it.previousPos(), it.currentPos());
                }
                return new Token(TokenType.LT, '<', it.previousPos(), it.currentPos());

            case ';':
                return new Token(TokenType.SEMICOLON, ';', it.previousPos(), it.currentPos());

            case '{':
                return new Token(TokenType.L_BRACE, '{', it.previousPos(), it.currentPos());

            case '}':
                return new Token(TokenType.R_BRACE, '}', it.previousPos(), it.currentPos());

            case '(':
                return new Token(TokenType.L_PAREN, '(', it.previousPos(), it.currentPos());

            case ')':
                return new Token(TokenType.R_PAREN, ')', it.previousPos(), it.currentPos());

            case ',':
                return new Token(TokenType.COMMA, ',', it.previousPos(), it.currentPos());

            case ':':
                return new Token(TokenType.COLON, ':', it.previousPos(), it.currentPos());

            default:
                // 不认识这个输入，摸了
                throw new TokenizeError(ErrorCode.InvalidInput, it.previousPos());
        }
    }


    private void skipSpaceCharacters() {
        while (!it.isEOF() && Character.isWhitespace(it.peekChar())) {
            it.nextChar();
        }
    }
}
