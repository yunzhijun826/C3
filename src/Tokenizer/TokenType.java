package Tokenizer;

public enum TokenType {

    None, //空

    /** 标识符 **/
    IDENT,  //标识符

    /**类型**/
    DOUBLE,
    INT,
    VOID,
    BOOL,


    /**字面量**/
    UINT_LITERAL, //无符号整数
    STRING_LITERAL, //字符串常量
    DOUBLE_LITERAL, //double浮点数
    CHAR_LITERAL, //char

    /**关键字**/
    FN_KW,  //'fn'
    LET_KW,    //'let'
    CONST_KW,  // 'const'
    AS_KW,    // 'as'
    WHILE_KW,  //'while'
    IF_KW,     // 'if'
    ELSE_KW,   // 'else'
    RETURN_KW, // 'return'
    BREAK_KW, //'break'
    CONTINUE_KW, // 'continue'

    /**运算符**/
    PLUS,      // '+'
    MINUS,     // '-'
    MUL,       // '*'
    DIV,      // '/'
    ASSIGN,    // '='
    EQ,        // '=='
    NEQ,       // '!='
    LT,        // '<'
    GT,        // '>'
    LE,        // '<='
    GE,        // '>='
    L_PAREN,  // '('
    R_PAREN,   // ')'
    L_BRACE,   // '{'
    R_BRACE,   // '}'
    ARROW,     // '->'
    COMMA,     // ','
    COLON,     // ':'
    SEMICOLON, // ';'


    EOF,
}
