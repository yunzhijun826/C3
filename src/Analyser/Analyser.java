package Analyser;
import Tokenizer.Token;
import Tokenizer.TokenType;
import Tokenizer.Tokenizer;
import error.*;
import instruction.*;
import util.Pos;
import java.util.*;

public class Analyser {

    Tokenizer tokenizer;
    ArrayList<Instruction> instructions;
    int globalOffset = 0;
    int argsOffset = 0;
    int localOffset = 0;
    int fnOffset = 1;
    int fnPos = 0;
    ArrayList<Instruction> CurrentFnInstruction;
    ArrayList<String> GlobalVariable=new ArrayList<>();
    ArrayList<FnInstruction> fnLists = new ArrayList<>();
    boolean hasMain = false;
    boolean mainType = false;
    ArrayList<TokenType> Symbol = new ArrayList<TokenType>(Arrays.asList(TokenType.AS_KW, TokenType.MUL, TokenType.DIV, TokenType.PLUS, TokenType.MINUS, TokenType.GT, TokenType.LT, TokenType.LE, TokenType.GE, TokenType.EQ, TokenType.NEQ));
    public int[][] SymbolMatrix = {
            {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
            {0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
            {0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
            {0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1},
            {0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1},
            {0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1},
            {0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1},
            {0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1},
            {0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1},
            {0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1},
            {0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1}
    };


    /**
     * 当前偷看的 token
     */
    Token peekedToken = null;

    /**
     * 符号表
     */
    Stack<Symbol> symbolTable = new Stack<Symbol>();
    Stack<Integer> symbolIntStack = new Stack<>();
    HashMap<String, Integer> symbolHash = new HashMap<>();

    /**
     * 下一个变量的栈偏移
     */
    int nextOffset = 0;

    public Analyser(Tokenizer tokenizer) {
        this.tokenizer = tokenizer;
        //this.instructions = new ArrayList<>();
    }

    /**
     * 如果下一个 token 的类型是 tt，则前进一个 token 并返回这个 token
     *
     * @param tt 类型
     * @return 如果匹配则返回这个 token，否则返回 null
     * @throws TokenizeError
     */
    private Token nextIf(TokenType tt) throws TokenizeError {
        Token token = peek();
        if (token.getTokenType() == tt) {
            return next();
        } else {
            return null;
        }
    }

    /**
     * 查看下一个 Token
     *
     * @return
     * @throws TokenizeError
     */
    private Token peek() throws TokenizeError {
        if (peekedToken == null) {
            peekedToken = tokenizer.nextToken();
        }
        return peekedToken;
    }

    /**
     * 如果下一个 token 的类型是 tt，则返回 true
     *
     * @param tt
     * @return
     * @throws TokenizeError
     */
    private boolean check(TokenType tt) throws TokenizeError {
        Token token = peek();
        return token.getTokenType() == tt;
    }

    /**
     * 获取下一个 Token
     *
     * @return
     * @throws TokenizeError
     */
    private Token next() throws TokenizeError {
        if (peekedToken != null) {
            Token token = peekedToken;
            peekedToken = null;
            return token;
        } else {
            return tokenizer.nextToken();
        }
    }

    /**
     * 如果下一个 token 的类型是 tt，则前进一个 token 并返回，否则抛出异常
     *
     * @param tt 类型
     * @return 这个 token
     * @throws CompileError 如果类型不匹配
     */
    private Token expect(TokenType tt) throws CompileError {
        Token token = peek();
        if (token.getTokenType() == tt) {
            return next();
        } else {
            throw new ExpectedTokenError(tt, token);
        }
    }

    /**
     * 添加一个符号
     *
     * @param name       名字
     * @param isConstant 是否是常量
     * @param curPos     当前 token 的位置（报错用）
     * @throws AnalyzeError 如果重复定义了则抛异常
     */
    private void addSymbol(String name, boolean isConstant, TokenType tokenType, SymbolType symbolType, Pos curPos) throws AnalyzeError {
        if (this.symbolHash.get(name) != null && symbolIntStack.peek()<=this.symbolHash.get(name)) {
            throw new AnalyzeError(ErrorCode.DuplicateDeclaration, curPos);
        }
        else if(this.symbolHash.get(name) != null){
                int chainName = this.symbolHash.get(name);
                switch (symbolType) {
                    case args:
                        this.symbolTable.push(new Symbol(name, chainName, tokenType, isConstant, symbolType, argsOffset++));
                        break;

                    case global:
                        this.symbolTable.push(new Symbol(name, chainName, tokenType, isConstant, symbolType, globalOffset++));
                        if(!isConstant){
                            GlobalVariable.add("1");
                        }else{
                            GlobalVariable.add("0");
                        }
                        break;

                    case local:
                        this.symbolTable.push(new Symbol(name, chainName, tokenType, isConstant, symbolType, localOffset++));
                        break;
                }
                int size=symbolTable.size() - 1;
                this.symbolHash.put(name, size);
        }
        else{
            switch (symbolType) {
                case args:
                    this.symbolTable.push(new Symbol(name, -1, tokenType, isConstant, symbolType, argsOffset++));
                    break;

                case global:
                    this.symbolTable.push(new Symbol(name, -1, tokenType, isConstant, symbolType, globalOffset++));
                    if(!isConstant){
                        GlobalVariable.add("1");
                    }else{
                        GlobalVariable.add("0");
                    }
                    break;

                case local:
                    this.symbolTable.push(new Symbol(name, -1, tokenType, isConstant, symbolType, localOffset++));
                    break;
            }
            int size=symbolTable.size() - 1;
            this.symbolHash.put(name, size);
        }
    }

    private Symbol addFnSymbol(String name, Pos curPos) throws AnalyzeError {
        if (this.symbolHash.get(name) != null) {
            throw new AnalyzeError(ErrorCode.DuplicateDeclaration, curPos);
        }
        else {
            this.symbolTable.push(new Symbol(name, true, globalOffset, fnOffset++));
            int size=symbolTable.size();
            this.symbolHash.put(name, size - 1);
            this.symbolIntStack.push(size);
            return this.symbolTable.peek();
        }

    }

    /**
     * 获取下一个变量的栈偏移
     *
     * @return
     */
    private int getNextVariableOffset() {
        return this.nextOffset++;
    }

    public void analyseProgram(String name) throws Exception {
        // 程序 -> 主过程
        // 示例函数，示例如何调用子程序

        analyseMain();
        expect(TokenType.EOF);
        Output.Output(name, GlobalVariable, fnLists);
    }


    private void analyseMain() throws CompileError {
        // 主过程 -> (变量声明|函数声明)*
//        analyseConstantDeclaration();
//        analyseVariableDeclaration();
//        analyseStatementSequence();
        FnInstruction startFnInstruction = new FnInstruction();
        GlobalVariable.add("_start");
        globalOffset++;
        fnLists.add(startFnInstruction);
        while (true) {
            if (check(TokenType.CONST_KW)) {
                    CurrentFnInstruction = startFnInstruction.getBodyItem();
                    analyseConstDeclaration(true);
            }
            else if(check(TokenType.LET_KW)){
                CurrentFnInstruction = startFnInstruction.getBodyItem();
                analyseVariableDeclaration(true);
            }
            else if (check(TokenType.FN_KW)) {
                analyseFunctionDeclaration();
            } else {
                break;
            }
        }
        startFnInstruction.setName(0);
        startFnInstruction.setRet_slots(0);
        startFnInstruction.setParam_slots(0);
        startFnInstruction.setLoc_slots(0);
        if(hasMain){
            if(mainType){
                startFnInstruction.getBodyItem().add(new Instruction(Operation.stackalloc, 1));
            }else{
                startFnInstruction.getBodyItem().add(new Instruction(Operation.stackalloc, 0));
            }
            startFnInstruction.getBodyItem().add(new Instruction(Operation.call, fnPos));
            if(mainType){
                startFnInstruction.getBodyItem().add(new Instruction(Operation.popn,1));
            }
        }
        startFnInstruction.setBodyCount(startFnInstruction.getBodyItem().size());
    }


    private void analyseConstDeclaration(boolean isGlobal) throws CompileError {
        expect(TokenType.CONST_KW);
        Token nameToken = expect(TokenType.IDENT);
        String name = (String) nameToken.getValue();
        if(isGlobal){
            CurrentFnInstruction.add(new Instruction(Operation.globa, globalOffset));
        }else{
            CurrentFnInstruction.add(new Instruction(Operation.loca, localOffset));
        }
        expect(TokenType.COLON);
        Token couToken = expect(TokenType.IDENT);
        if(couToken.getValue().equals("double")){
            couToken.setTokenType(TokenType.DOUBLE);
        }
        else if(couToken.getValue().equals("int")){
            couToken.setTokenType(TokenType.INT);
        }
        else{
            throw new AnalyzeError(ErrorCode.NotDeclared, couToken.getStartPos());
        }
        expect(TokenType.ASSIGN);
        TokenType tT = analyseExpr(true);
        if(couToken.getTokenType() != tT){ //ty '=' expr 类型是否相同
            throw new AnalyzeError(ErrorCode.NotDeclared, couToken.getStartPos());
        }
        CurrentFnInstruction.add(new Instruction(Operation.store64));
        expect(TokenType.SEMICOLON);
        if (!isGlobal) {
            addSymbol(name, true, couToken.getTokenType(), SymbolType.local, nameToken.getStartPos());
        } else {
            addSymbol(name, true, couToken.getTokenType(), SymbolType.global, nameToken.getStartPos());
        }
    }

    private void analyseVariableDeclaration(boolean isGlobal) throws CompileError {
        //let_decl_stmt -> 'let' IDENT ':' ty ('=' expr)? ';'
        // 如果下一个 token 是 var 就继续
//        while (nextIf(TokenType.Var) != null) {
//            // 变量声明语句 -> 'var' 变量名 ('=' 表达式)? ';'
//
//            // 变量名
//            var nameToken = expect(TokenType.Ident);
//
//            // 变量初始化了吗
//            boolean initialized = false;
//
//            // 下个 token 是等于号吗？如果是的话分析初始化
//            if(nextIf(TokenType.Equal)!=null){
//                initialized=true;
//                analyseExpression();
//            }
//
//            // 分析初始化的表达式
//
//            // 分号
//            expect(TokenType.Semicolon);
//
//            // 加入符号表，请填写名字和当前位置（报错用）
//            //String name = /* 名字 */ null;
//            String name = (String) nameToken.getValue();
//            //addSymbol(name, false, false, /* 当前位置 */ null);
//            addSymbol(name, initialized, false, nameToken.getStartPos());
//            // 如果没有初始化的话在栈里推入一个初始值
//            if (!initialized) {
//                instructions.add(new Instruction(Operation.LIT, 0));
//            }
//        }
        expect(TokenType.LET_KW);
        Token nameToken = expect(TokenType.IDENT);
        expect(TokenType.COLON);
        Token couToken = expect(TokenType.IDENT);
        if(couToken.getValue().equals("double")){
            couToken.setTokenType(TokenType.DOUBLE);
        }
        else if(couToken.getValue().equals("int")){
            couToken.setTokenType(TokenType.INT);
        }
        else{
            throw new AnalyzeError(ErrorCode.NotDeclared, couToken.getStartPos());
        }
        if (nextIf(TokenType.ASSIGN) != null) {
            if(!isGlobal){
                CurrentFnInstruction.add(new Instruction(Operation.loca, localOffset));
            }else{
                CurrentFnInstruction.add(new Instruction(Operation.globa, globalOffset));
            }
            TokenType tt = analyseExpr(true);
            if(couToken.getTokenType() != tt){
                throw new AnalyzeError(ErrorCode.NotDeclared, couToken.getStartPos());
            }
            CurrentFnInstruction.add(new Instruction(Operation.store64));
        }
        expect(TokenType.SEMICOLON);
        if (!isGlobal) {
            addSymbol(nameToken.getValue().toString(), false, couToken.getTokenType(), SymbolType.local, nameToken.getStartPos());
        } else {
            addSymbol(nameToken.getValue().toString(), false, couToken.getTokenType(), SymbolType.global, nameToken.getStartPos());

        }
    }

    private void analyseFunctionDeclaration() throws CompileError {
        FnInstruction fnInstruction = new FnInstruction();
        fnLists.add(fnInstruction);
        CurrentFnInstruction = fnInstruction.getBodyItem();
        boolean isReturn = false;
        expect(TokenType.FN_KW);
        Token nameToken = expect(TokenType.IDENT);
        String s=nameToken.getValue().toString();
        GlobalVariable.add(s);
        fnInstruction.setName(globalOffset++);
        if(nameToken.getValue().toString().equals("main")){
            hasMain = true;
            fnPos = fnLists.size()-1;
        }
        String s2=nameToken.getValue().toString();
        Symbol currentSymbol = addFnSymbol(s2, nameToken.getStartPos());
        expect(TokenType.L_PAREN);
        argsOffset = 0;
        if ( check(TokenType.IDENT) ||check(TokenType.CONST_KW)) {
            analyseFunctionParamList();
        }
        expect(TokenType.R_PAREN);
        expect(TokenType.ARROW);
        Token couToken = expect(TokenType.IDENT);
        if(couToken.getValue().equals("double")){
            couToken.setTokenType(TokenType.DOUBLE);
            fnInstruction.setRet_slots(1);
            int size=symbolTable.size()-1;
            for(int i = size; symbolTable.get(i).getSymbolType() == SymbolType.args; i--){
                symbolTable.get(i).setOffset(symbolTable.get(i).getOffset()+1);
            }
            String s3=nameToken.getValue().toString();
            if(s3.equals("main")){
                mainType = true;
            }
        }
        else if(couToken.getValue().equals("int")){
            couToken.setTokenType(TokenType.INT);
            fnInstruction.setRet_slots(1);
            int size=symbolTable.size()-1;
            for(int i = size; symbolTable.get(i).getSymbolType() == SymbolType.args; i--){
                symbolTable.get(i).setOffset(symbolTable.get(i).getOffset()+1);
            }
            String s4=nameToken.getValue().toString();
            if(s4.equals("main")){
                mainType = true;
            }
        }
        else if(couToken.getValue().equals("void")){
            couToken.setTokenType(TokenType.VOID);
            fnInstruction.setRet_slots(0);
            if(nameToken.getValue() == "main"){
                mainType = false;
            }
        }
        else{
            throw new AnalyzeError(ErrorCode.NotDeclared, couToken.getStartPos());
        }
        fnInstruction.setParam_slots(argsOffset);
        currentSymbol.setType(couToken.getTokenType());
        localOffset = 0;
        isReturn = analyseBlockStmt(true, couToken.getTokenType(), false, null, -1);
        fnInstruction.setLoc_slots(localOffset);
        if(couToken.getTokenType()==TokenType.VOID && !isReturn){
            CurrentFnInstruction.add(new Instruction(Operation.ret));
        }
        else if(couToken.getTokenType()!=TokenType.VOID && !isReturn){
            throw new AnalyzeError(ErrorCode.DuplicateDeclaration, new Pos(0,0));
        }
        int size2=fnInstruction.getBodyItem().size();
        fnInstruction.setBodyCount(size2);
    }


    private TokenType analyseExpr(boolean flag) throws CompileError {
        TokenType tokenType = null;
        if (check(TokenType.MINUS)) {
            tokenType = analyseNegateExpr();
            if(tokenType == TokenType.DOUBLE){
                CurrentFnInstruction.add(new Instruction(Operation.negf));
            }
            else if(tokenType == TokenType.INT){
                CurrentFnInstruction.add(new Instruction(Operation.negi));
            }else{
                throw new AnalyzeError(ErrorCode.NotDeclared, new Pos(3,0));
            }
        }
        if (peek().getTokenType() == TokenType.IDENT) {
            Token nameToken = next();
            String s=nameToken.getValue().toString();
            Integer index = symbolHash.get(s);
            if (nextIf(TokenType.ASSIGN) != null) {
                if (index == null) {
                    throw new AnalyzeError(ErrorCode.NotDeclared, nameToken.getStartPos());
                }
                if(symbolTable.get(index).isConst()){
                    throw new AnalyzeError(ErrorCode.NotDeclared, nameToken.getStartPos());
                }
                if(symbolTable.get(index).getSymbolType() == SymbolType.local){
                    CurrentFnInstruction.add(new Instruction(Operation.loca, symbolTable.get(index).getOffset()));
                }
                else if(symbolTable.get(index).getSymbolType() == SymbolType.global){
                    CurrentFnInstruction.add(new Instruction(Operation.globa, symbolTable.get(index).getOffset()));
                }
                else{
                    CurrentFnInstruction.add(new Instruction(Operation.arga, symbolTable.get(index).getOffset()));
                }
                TokenType l_type = symbolTable.get(index).getType();
                TokenType r_type = analyseExpr(true);
                if (l_type != r_type) {
                    throw new AnalyzeError(ErrorCode.NotDeclared, nameToken.getStartPos());
                }
                CurrentFnInstruction.add(new Instruction(Operation.store64));
                tokenType = TokenType.VOID;
            }
            else if (nextIf(TokenType.L_PAREN) != null) {
                int currentGlobal = 0;
                ArrayList<TokenType> callArray = null;
                TokenType returnType;
                if (index == null) {
                    String s1=nameToken.getValue().toString();
                    switch (s1) {
                        case "getint":
                        case "getchar":
                            callArray = new ArrayList<TokenType>();
                            returnType = TokenType.INT;
                            break;
                        case "putint":
                            callArray = new ArrayList<TokenType>() {{
                                add(TokenType.INT);
                            }};
                            returnType = TokenType.VOID;
                            break;
                        case "getdouble":
                            callArray = new ArrayList<TokenType>();
                            returnType = TokenType.DOUBLE;
                            break;
                        case "putchar":
                            callArray = new ArrayList<TokenType>() {{
                                add(TokenType.INT);
                            }};
                            returnType = TokenType.VOID;
                            break;
                        case "putdouble":
                            callArray = new ArrayList<TokenType>() {{
                                add(TokenType.DOUBLE);
                            }};
                            returnType = TokenType.VOID;
                            break;
                        case "putstr":
                            callArray = new ArrayList<TokenType>() {{
                                add(TokenType.INT);
                            }};
                            returnType = TokenType.VOID;
                            break;
                        case "putln":
                            callArray = new ArrayList<TokenType>();
                            returnType = TokenType.VOID;
                            break;
                        default:
                            throw new AnalyzeError(ErrorCode.NotDeclared, nameToken.getStartPos());
                    }
                    String s2=nameToken.getValue().toString();
                    GlobalVariable.add(s2); //把标准库函数存入全局变量
                    currentGlobal = globalOffset ++;
                }
                else {
                    Symbol callIndex = symbolTable.get(index);
                    callArray = callIndex.getParamsList();
                    returnType = callIndex.getType();
                }
                if(returnType == TokenType.INT){
                    CurrentFnInstruction.add(new Instruction(Operation.stackalloc, 1));
                }
                else if(returnType == TokenType.DOUBLE){
                    CurrentFnInstruction.add(new Instruction(Operation.stackalloc, 1));
                }
                else if(returnType == TokenType.VOID){
                    CurrentFnInstruction.add(new Instruction(Operation.stackalloc, 0));
                }

                if (nextIf(TokenType.R_PAREN) != null) { //无参数调用
                    if (callArray.size() == 0) {
                        tokenType = returnType;
                    } else {
                        throw new AnalyzeError(ErrorCode.NotDeclared, nameToken.getStartPos());
                    }
                } else { //有参数调用
                    TokenType paramT = analyseExpr(true); //
                    int i = 0;
                    if (paramT != callArray.get(i)) {
                        throw new AnalyzeError(ErrorCode.NotDeclared, nameToken.getStartPos());
                    }
                    while (nextIf(TokenType.COMMA) != null) {
                        i++;
                        if (callArray.size() < i) {
                            throw new AnalyzeError(ErrorCode.NotDeclared, nameToken.getStartPos());
                        }
                        TokenType param1 = analyseExpr(true);
                        if (param1 != callArray.get(i)) {
                            throw new AnalyzeError(ErrorCode.NotDeclared, nameToken.getStartPos());
                        }
                    }
                    expect(TokenType.R_PAREN);
                    tokenType = returnType;
                }
                if(index == null){
                    CurrentFnInstruction.add(new Instruction(Operation.callname, currentGlobal));
                }else{
                    CurrentFnInstruction.add(new Instruction(Operation.call, symbolTable.get(index).getFnOffset()));
                }
            }
            else {
                if(index==null&&nameToken.getValue().toString().equals("double")){
                    tokenType =TokenType.DOUBLE;
                }
                else if(index==null&&nameToken.getValue().toString().equals("int")){
                    tokenType =TokenType.INT;
                }
                else if (index == null) {
                    throw new AnalyzeError(ErrorCode.NotDeclared, nameToken.getStartPos());
                }
                else{
                    Symbol sym = symbolTable.get(index);
                    if(sym.getSymbolType() == SymbolType.local){
                        CurrentFnInstruction.add(new Instruction(Operation.loca, sym.getOffset()));
                    }
                    else if(sym.getSymbolType() == SymbolType.global){ //取地址
                        CurrentFnInstruction.add(new Instruction(Operation.globa, sym.getOffset()));
                    }
                    else{
                        CurrentFnInstruction.add(new Instruction(Operation.arga, sym.getOffset()));
                    }
                    CurrentFnInstruction.add(new Instruction(Operation.load64)); //取值
                    tokenType = symbolTable.get(index).getType();
                }
            }
        }
        else if (peek().getTokenType() == TokenType.STRING_LITERAL || peek().getTokenType() == TokenType.UINT_LITERAL || peek().getTokenType() == TokenType.CHAR_LITERAL || peek().getTokenType() == TokenType.DOUBLE_LITERAL) {
            if (peek().getTokenType() == TokenType.UINT_LITERAL) {
                tokenType = TokenType.INT;
                CurrentFnInstruction.add(new Instruction(Operation.push, peek().getValue()));
                next();
            }
            else if (peek().getTokenType() == TokenType.DOUBLE_LITERAL) {
                tokenType = TokenType.DOUBLE;
                CurrentFnInstruction.add(new Instruction(Operation.push, Double.doubleToRawLongBits((double)peek().getValue())));
                next();
            }
            else if (peek().getTokenType() == TokenType.STRING_LITERAL) {
                GlobalVariable.add(peek().getValue().toString());
                globalOffset++;
                tokenType = TokenType.INT;
                CurrentFnInstruction.add(new Instruction(Operation.push, (long)globalOffset-1));
                next();
            }
            else if (peek().getTokenType() == TokenType.CHAR_LITERAL) {
                tokenType = TokenType.INT;
                CurrentFnInstruction.add(new Instruction(Operation.push, (long)(char)peek().getValue()));
                next();
            }
        }

        else if (check(TokenType.L_PAREN)) {
            tokenType = analyseGroupExpr();
        }

        if (flag) {
            Stack stack = new Stack();
            stack.push('#');
            Stack stack2 = new Stack<>();
            if (tokenType != null) {
                stack2.push(tokenType);
            }
            while ( check(TokenType.PLUS) || check(TokenType.AS_KW) || check(TokenType.MINUS) || check(TokenType.MUL) ||check(TokenType.EQ) || check(TokenType.NEQ) ||  check(TokenType.DIV) || check(TokenType.LT) || check(TokenType.GT) || check(TokenType.LE) || check(TokenType.GE)) {
                OPGAnalyse(stack, stack2);
                TokenType type2 = analyseExpr(false);
                if (type2 != null) {
                    stack2.push(type2);
                    type2 = null;
                }
            }
            int ch1 = Symbol.indexOf(stack.peek());
            int ch2 = Symbol.indexOf(peek().getTokenType());
            while ((ch2 == -1 || SymbolMatrix[ch1][ch2] == 1) && stack.size() > 1) {
                reduction(stack, stack2);
            }
            tokenType = (TokenType) stack2.pop();
        }
        return tokenType;
    }

    private void OPGAnalyse(Stack<TokenType> s, Stack s2) throws TokenizeError {
        while (true) { //栈内大于当前 规约
            int ch1 = Symbol.indexOf(s.peek());
            int ch2 = Symbol.indexOf(peek().getTokenType());
            if (ch1 == -1 && ch2 == -1) {
                return;
            }
            else if (SymbolMatrix[ch1][ch2] == 0 || ch1 == -1 ) {
                s.push(Symbol.get(ch2));
                next();
                return;
            }
            else if((SymbolMatrix[ch1][ch2] == 1 || ch2 == -1 ) && s.size() > 1){
                reduction(s, s2);
            }
        }
    }

    private void reduction(Stack<TokenType> s, Stack<Object> s2) {
        TokenType pop = s.pop();
        TokenType pop2 = (TokenType) s2.pop();
        TokenType pop1 = (TokenType) s2.pop();
        TokenType item = null;
        if (pop == TokenType.AS_KW) {
            if (pop1 == TokenType.DOUBLE || pop1 == TokenType.INT) {
                if (pop2 == TokenType.INT) {
                    item = TokenType.INT;
                    if(pop1 == TokenType.DOUBLE){
                        CurrentFnInstruction.add(new Instruction(Operation.ftoi));
                    }
                }

                if (pop2 == TokenType.DOUBLE) {
                    item = TokenType.DOUBLE;
                    if(pop1 == TokenType.INT){
                        CurrentFnInstruction.add(new Instruction(Operation.itof));
                    }
                }

            }
            else {
                System.exit(-1);
            }
        }
        else {
            if (pop1 != pop2) {
                System.exit(-1);
            }
            switch (pop) { //
                case PLUS:
                    if(pop1 == TokenType.INT){
                        item = TokenType.INT;
                        CurrentFnInstruction.add(new Instruction(Operation.addi));
                    }else{
                        item = TokenType.DOUBLE;
                        CurrentFnInstruction.add(new Instruction(Operation.addf));
                    }
                    break;
                case MINUS:
                    if(pop1 == TokenType.INT){
                        item = TokenType.INT;
                        CurrentFnInstruction.add(new Instruction(Operation.subi));
                    }else{
                        item = TokenType.DOUBLE;
                        CurrentFnInstruction.add(new Instruction(Operation.subf));
                    }
                    break;
                case MUL:
                    if(pop1 == TokenType.INT){
                        item = TokenType.INT;
                        CurrentFnInstruction.add(new Instruction(Operation.muli));
                    }else{
                        item = TokenType.DOUBLE;
                        CurrentFnInstruction.add(new Instruction(Operation.mulf));
                    }
                    break;
                case DIV:
                    if(pop1 == TokenType.INT){
                        item = TokenType.INT;
                        CurrentFnInstruction.add(new Instruction(Operation.divi));
                    }else{
                        item = TokenType.DOUBLE;
                        CurrentFnInstruction.add(new Instruction(Operation.divf));
                    }
                    break;
                case EQ:
                    if(pop1 == TokenType.INT){
                        item = TokenType.BOOL;
                        CurrentFnInstruction.add(new Instruction(Operation.cmpi));
                        CurrentFnInstruction.add(new Instruction(Operation.not));
                    }else{
                        item = TokenType.BOOL;
                        CurrentFnInstruction.add(new Instruction(Operation.cmpf));
                        CurrentFnInstruction.add(new Instruction(Operation.not));
                    }
                    break;
                case NEQ:
                    if(pop1 == TokenType.INT){
                        item = TokenType.BOOL;
                        CurrentFnInstruction.add(new Instruction(Operation.cmpi));
                    }else{
                        item = TokenType.BOOL;
                        CurrentFnInstruction.add(new Instruction(Operation.cmpf));
                    }
                    break;

                case GT:
                    if(pop1 == TokenType.INT){
                        item = TokenType.BOOL;
                        CurrentFnInstruction.add(new Instruction(Operation.cmpi));
                        CurrentFnInstruction.add(new Instruction(Operation.setgt));
                    }else{
                        item = TokenType.BOOL;
                        CurrentFnInstruction.add(new Instruction(Operation.cmpf));
                        CurrentFnInstruction.add(new Instruction(Operation.setgt));
                    }
                    break;
                case LT:
                    if(pop1 == TokenType.INT){
                        item = TokenType.BOOL;
                        CurrentFnInstruction.add(new Instruction(Operation.cmpi));
                        CurrentFnInstruction.add(new Instruction(Operation.setlt));
                    }else{
                        item = TokenType.BOOL;
                        CurrentFnInstruction.add(new Instruction(Operation.cmpf));
                        CurrentFnInstruction.add(new Instruction(Operation.setlt));
                    }
                    break;


                case GE:
                    if(pop1 == TokenType.INT){
                        item = TokenType.BOOL;
                        CurrentFnInstruction.add(new Instruction(Operation.cmpi));
                        CurrentFnInstruction.add(new Instruction(Operation.setlt));
                        CurrentFnInstruction.add(new Instruction(Operation.not));
                    }else{
                        item = TokenType.BOOL;
                        CurrentFnInstruction.add(new Instruction(Operation.cmpf));
                        CurrentFnInstruction.add(new Instruction(Operation.setlt));
                        CurrentFnInstruction.add(new Instruction(Operation.not));
                    }
                    break;
                case LE:
                    if(pop1 == TokenType.INT){
                        item = TokenType.BOOL;
                        CurrentFnInstruction.add(new Instruction(Operation.cmpi));
                        CurrentFnInstruction.add(new Instruction(Operation.setgt));
                        CurrentFnInstruction.add(new Instruction(Operation.not));
                    }else{
                        item = TokenType.BOOL;
                        CurrentFnInstruction.add(new Instruction(Operation.cmpf));
                        CurrentFnInstruction.add(new Instruction(Operation.setgt));
                        CurrentFnInstruction.add(new Instruction(Operation.not));
                    }
                    break;
                default:
                    System.exit(-1);
            }
        }
        s2.push(item);
    }

    private TokenType analyseNegateExpr() throws CompileError {
        expect(TokenType.MINUS);
        return analyseExpr(true);
    }

    private TokenType analyseGroupExpr() throws CompileError {
        expect(TokenType.L_PAREN);
        TokenType tt = analyseExpr(true);
        expect(TokenType.R_PAREN);
        return tt;
    }

    private void analyseFunctionParamList() throws CompileError {
        analyseFunctionParam();
        while (nextIf(TokenType.COMMA) != null) {
            analyseFunctionParam();
        }
    }

    private void analyseFunctionParam() throws CompileError {
        if (nextIf(TokenType.CONST_KW) != null) {
            Token nameToken = expect(TokenType.IDENT);
            expect(TokenType.COLON);
            Token couToken = expect(TokenType.IDENT);
            String s=couToken.getValue().toString();
            switch (s) {
                case "int":
                    addSymbol(nameToken.getValue().toString(), true, TokenType.INT, SymbolType.args, nameToken.getStartPos()); //常量加入符号栈
                    this.symbolTable.get(this.symbolIntStack.peek() - 1).getParamsList().add(TokenType.INT);
                    break;
                case "double":
                    addSymbol(nameToken.getValue().toString(), true, TokenType.DOUBLE, SymbolType.args, nameToken.getStartPos());
                    this.symbolTable.get(this.symbolIntStack.peek() - 1).getParamsList().add(TokenType.DOUBLE);
                    break;
                default:
                    throw new AnalyzeError(ErrorCode.DuplicateDeclaration, nameToken.getStartPos());
            }
        }
        else {
            Token nameToken = expect(TokenType.IDENT);
            expect(TokenType.COLON);
            Token couToken = expect(TokenType.IDENT); //取值
            String s2=couToken.getValue().toString();
            switch (s2) {
                case "int":
                    addSymbol(nameToken.getValue().toString(), false, TokenType.INT, SymbolType.args, nameToken.getStartPos()); //常量加入符号栈
                    this.symbolTable.get(this.symbolIntStack.peek() - 1).getParamsList().add(TokenType.INT);
                    break;
                case "double":
                    addSymbol(nameToken.getValue().toString(), false, TokenType.DOUBLE, SymbolType.args, nameToken.getStartPos()); //常量加入符号栈
                    this.symbolTable.get(this.symbolIntStack.peek() - 1).getParamsList().add(TokenType.DOUBLE);
                    break;
                default:
                    throw new AnalyzeError(ErrorCode.DuplicateDeclaration, nameToken.getStartPos());
            }
        }
    }

    private boolean analyseStmt(TokenType tyTokenType, boolean isWhile , ArrayList<Integer> breakEndPos, int continuePos) throws CompileError {
        if (check(TokenType.MINUS) || check(TokenType.IDENT) || check(TokenType.UINT_LITERAL) || check(TokenType.L_PAREN) || check(TokenType.DOUBLE_LITERAL) || check(TokenType.STRING_LITERAL) || check(TokenType.CHAR_LITERAL)) { //expr_stmt
            analyseExprStmt();
        }
        if (check(TokenType.CONST_KW)) {
            analyseConstDeclaration(false);
        }

        if (check(TokenType.LET_KW)) {
            analyseVariableDeclaration(false);
        }

        if (check(TokenType.IF_KW)) {
            return analyseIfStmt(tyTokenType, isWhile, breakEndPos, continuePos);
        }

        if (check(TokenType.WHILE_KW)) {
            analyseWhileStmt(tyTokenType);
        }

        if (check(TokenType.BREAK_KW)) {
            if(!isWhile){
                throw new AnalyzeError(ErrorCode.DuplicateDeclaration, new Pos(2,0));
            }
            analyseBreakStmt();
            CurrentFnInstruction.add(new Instruction(Operation.br));
            int breakPos = CurrentFnInstruction.size()-1;
            breakEndPos.add(breakPos);
        }

        if (check(TokenType.CONTINUE_KW)) {
            if(!isWhile){
                throw new AnalyzeError(ErrorCode.DuplicateDeclaration, new Pos(2,0));
            }
            analyseContinueStmt();
            CurrentFnInstruction.add(new Instruction(Operation.br,continuePos-CurrentFnInstruction.size()));
        }

        if (check(TokenType.RETURN_KW)) {
            analyseReturnStmt(tyTokenType);
            return true;
        }

        if (check(TokenType.L_BRACE)) {
            return analyseBlockStmt(false, tyTokenType, isWhile, breakEndPos, continuePos);
        }

        if (check(TokenType.SEMICOLON)) {
            analyseEmptyStmt();
        }
        return false;
    }

    private void analyseEmptyStmt() throws CompileError {
        expect(TokenType.SEMICOLON);
    }

    private boolean analyseBlockStmt(boolean isFn, TokenType tyTT, boolean isWhile, ArrayList<Integer> breakEndPos, int conPos) throws CompileError {
        boolean isReturn = false;
        expect(TokenType.L_BRACE);
        if (!isFn) {
            int size=symbolTable.size();
            symbolIntStack.push(size);
        }
        while (check(TokenType.MINUS) || check(TokenType.IDENT) || check(TokenType.UINT_LITERAL) || check(TokenType.DOUBLE_LITERAL) || check(TokenType.STRING_LITERAL) || check(TokenType.CHAR_LITERAL) || check(TokenType.L_PAREN) || check(TokenType.LET_KW) ||
                check(TokenType.CONST_KW) || check(TokenType.IF_KW) || check(TokenType.WHILE_KW) || check(TokenType.BREAK_KW) || check(TokenType.CONTINUE_KW) || check(TokenType.RETURN_KW) || check(TokenType.SEMICOLON) || check(TokenType.L_BRACE)) {
            if(isReturn){
                analyseStmt(tyTT, isWhile, breakEndPos, conPos);
            }
            else{
                isReturn = analyseStmt(tyTT, isWhile, breakEndPos, conPos);
            }
        }
        expect(TokenType.R_BRACE);
        int index = symbolIntStack.pop();
        while (symbolTable.size() > index) {
            Symbol s = symbolTable.pop();
            if (s.getChain() == -1) {
                symbolHash.remove(s.getName());
            } else {
                symbolHash.put(s.getName(), s.getChain());
            }
        }
        return isReturn;
    }

    private void analyseReturnStmt(TokenType tyTT) throws CompileError {
        expect(TokenType.RETURN_KW);
        if(tyTT == TokenType.INT){
            CurrentFnInstruction.add(new Instruction(Operation.arga, 0));
        }
        else if(tyTT == TokenType.DOUBLE){
            CurrentFnInstruction.add(new Instruction(Operation.arga, 0));
        }

        if (check(TokenType.MINUS) || check(TokenType.IDENT) || check(TokenType.UINT_LITERAL) || check(TokenType.L_PAREN) || check(TokenType.DOUBLE_LITERAL) || check(TokenType.STRING_LITERAL) || check(TokenType.CHAR_LITERAL)) {
            TokenType expT = analyseExpr(true);
            if(expT != tyTT){
                throw new AnalyzeError(ErrorCode.DuplicateDeclaration, new Pos(1,0));
            }
        }
        else{
            if(tyTT != TokenType.VOID){
                throw new AnalyzeError(ErrorCode.DuplicateDeclaration, new Pos(1,0));
            }
        }

        if(tyTT == TokenType.INT){
            CurrentFnInstruction.add(new Instruction(Operation.store64));
        }
        else if(tyTT == TokenType.DOUBLE){
            CurrentFnInstruction.add(new Instruction(Operation.store64));
        }
        CurrentFnInstruction.add(new Instruction(Operation.ret));
        expect(TokenType.SEMICOLON);
    }

    private void analyseContinueStmt() throws CompileError {
        expect(TokenType.CONTINUE_KW);
        expect(TokenType.SEMICOLON);
    }

    private void analyseBreakStmt() throws CompileError {
        expect(TokenType.BREAK_KW);
        expect(TokenType.SEMICOLON);
    }

    private void analyseWhileStmt(TokenType tyTokenType) throws CompileError {
        expect(TokenType.WHILE_KW);
        int size1=CurrentFnInstruction.size();
        int startP =size1-1;
        ArrayList<Integer> breakEP = new ArrayList<>();
        TokenType whileST = analyseExpr(true);
        CurrentFnInstruction.add(new Instruction(Operation.brtrue, 1));
        CurrentFnInstruction.add(new Instruction(Operation.br));
        int size2=CurrentFnInstruction.size();
        int currentPos = size2 -1;
        if(whileST == TokenType.VOID){
            throw new AnalyzeError(ErrorCode.DuplicateDeclaration, new Pos(1,0));
        }
        analyseBlockStmt(false, tyTokenType, true, breakEP, startP);
        int posi=startP -CurrentFnInstruction.size();
        CurrentFnInstruction.add(new Instruction(Operation.br,posi ));
        int posi2=CurrentFnInstruction.size()-1-currentPos;
        CurrentFnInstruction.get(currentPos).setValue(posi2);
        int size= breakEP.size();
        for(int i = 0; i < size; i ++){
            CurrentFnInstruction.get(breakEP.get(i)).setValue(CurrentFnInstruction.size()-1- breakEP.get(i));
        }
    }

    private boolean analyseIfStmt(TokenType tyTT, boolean isWhile, ArrayList<Integer> breakEP, int conPos) throws CompileError {
        expect(TokenType.IF_KW);
        TokenType ifST = analyseExpr(true);
        if(ifST == TokenType.VOID){
            throw new AnalyzeError(ErrorCode.DuplicateDeclaration, new Pos(1,0));
        }
        boolean haveElse = false;
        boolean isReturn = false;
        CurrentFnInstruction.add(new Instruction(Operation.brtrue, 1));
        CurrentFnInstruction.add(new Instruction(Operation.br));
        int curPos = CurrentFnInstruction.size()-1;
        isReturn = analyseBlockStmt(false, tyTT, isWhile, breakEP, conPos);
        CurrentFnInstruction.add(new Instruction(Operation.br));
        int endPos = CurrentFnInstruction.size()-1;
        CurrentFnInstruction.get(curPos).setValue(CurrentFnInstruction.size()-1 - curPos);
        ArrayList<Integer> Pos = new ArrayList<>();
        while (nextIf(TokenType.ELSE_KW) != null) {
            if (nextIf(TokenType.IF_KW) != null) {
                ifST = analyseExpr(true);
                CurrentFnInstruction.add(new Instruction(Operation.brtrue, 1));
                CurrentFnInstruction.add(new Instruction(Operation.br));
                int curPos1 = CurrentFnInstruction.size()-1;
                if(ifST == TokenType.VOID){
                    throw new AnalyzeError(ErrorCode.DuplicateDeclaration, new Pos(1,0));
                }
                isReturn &= analyseBlockStmt(false, tyTT, isWhile, breakEP, conPos);
                CurrentFnInstruction.add(new Instruction(Operation.br));
                int x=CurrentFnInstruction.size()-1;
                Pos.add(x);
                int y=CurrentFnInstruction.size()-1 - curPos1;
                CurrentFnInstruction.get(curPos1).setValue(CurrentFnInstruction.size()-1 - curPos1);
            }
            else if (check(TokenType.L_BRACE)) { //只有else
                isReturn &= analyseBlockStmt(false, tyTT, isWhile, breakEP, conPos);
                haveElse = true;
                break;
            }
        }
        CurrentFnInstruction.get(endPos).setValue(CurrentFnInstruction.size()-1-endPos);
        int size=Pos.size();
        for(int i = 0; i < size; i ++){
            CurrentFnInstruction.get(Pos.get(i)).setValue(CurrentFnInstruction.size()-1-Pos.get(i)); //循环存每一个elseif
        }
        if(haveElse){
            return isReturn;
        }
        else{
            return false;
        }
    }

    private void analyseExprStmt() throws CompileError {
        TokenType tt = null;
        if (check(TokenType.MINUS) || check(TokenType.UINT_LITERAL) || check(TokenType.IDENT) || check(TokenType.L_PAREN) || check(TokenType.STRING_LITERAL) || check(TokenType.DOUBLE_LITERAL) || check(TokenType.CHAR_LITERAL)) {
            tt = analyseExpr(true);
        }
        if(tt != TokenType.VOID){
            CurrentFnInstruction.add(new Instruction(Operation.popn, 1));
        }
        expect(TokenType.SEMICOLON);
    }
}
