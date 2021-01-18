package Analyser;
import Tokenizer.Token;
import Tokenizer.TokenType;
import Tokenizer.Tokenizer;
import error.*;
import instruction.FnInstruction;
import instruction.Instruction;
import instruction.Operation;
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
    ArrayList<String> GlobalVariable=new ArrayList<>();
    ArrayList<FnInstruction> fnLists = new ArrayList<>();
    ArrayList<Instruction> CurrentFnInstruction;
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
    Stack<Integer> symbolInt = new Stack<>();
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

        if (this.symbolHash.get(name) != null && this.symbolHash.get(name) >= symbolInt.peek()) { //如果现在读到的已经在当前块
            throw new AnalyzeError(ErrorCode.DuplicateDeclaration, curPos);
        }
        else if(this.symbolHash.get(name) != null){
          //如果读到的在之前的块里出现过
                int chain = this.symbolHash.get(name);
                switch (symbolType) {
                    case args:
                        this.symbolTable.push(new Symbol(name, chain, tokenType, isConstant, symbolType, argsOffset++));
                        break;

                    case global:
                        this.symbolTable.push(new Symbol(name, chain, tokenType, isConstant, symbolType, globalOffset++));
                        if(isConstant){
                            GlobalVariable.add("0");
                        }else{
                            GlobalVariable.add("1");
                        }
                        break;

                    case local:
                        this.symbolTable.push(new Symbol(name, chain, tokenType, isConstant, symbolType, localOffset++));
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
                    if(isConstant){
                        GlobalVariable.add("0");
                    }else{
                        GlobalVariable.add("1");
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
        } else {
            this.symbolTable.push(new Symbol(name, true, globalOffset, fnOffset++));
            int size=symbolTable.size();
            this.symbolHash.put(name, size - 1);
            this.symbolInt.push(size);
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
        System.out.println();
        for (String s : GlobalVariable) {
            System.out.println(s);
        }
        for (FnInstruction fnList : fnLists) {
            System.out.println(fnList.toString());
        }
        Output.Output(name, GlobalVariable, fnLists); //转二进制
    }


    private void analyseMain() throws CompileError {
        // 主过程 -> (变量声明|函数声明)*
//        analyseConstantDeclaration();
//        analyseVariableDeclaration();
//        analyseStatementSequence();
        FnInstruction startFn = new FnInstruction();
        GlobalVariable.add("_start");
        globalOffset++;
        fnLists.add(startFn);
        while (true) { //这里一起判断了三种：decl_stmt -> let_decl_stmt | const_decl_stmt； function
            if (check(TokenType.CONST_KW)) { //如果读到下一个token类型是const或者let，那么不能前进一个token，说明此时进入decl_stmt
                // 变量声明 -> 变量声明 | 常量声明

                    CurrentFnInstruction = startFn.getBodyItem();
                    analyseConstDeclaration(true); //进入常量声明分析过程 const

            }
            else if(check(TokenType.LET_KW)){
                CurrentFnInstruction = startFn.getBodyItem();
                analyseVariableDeclaration(true);
            }
            else if (check(TokenType.FN_KW)) { //如果下一个token是fn，则前进一个token，并返回这个token（fn），此时应进入function分析过程
                System.out.println("进入fn了噢");
                analyseFunctionDeclaration(); //进入function分析过程
            } else {
                System.out.println("主过程错啦，既不是变量也不是常量！");
                break;
//                throw new AnalyzeError(ErrorCode.InvalidAssignment, );
            }
        }

        startFn.setName(0);
        startFn.setRet_slots(0);
        startFn.setParam_slots(0);
        startFn.setLoc_slots(0);
        if(hasMain){
            if(mainType){
                startFn.getBodyItem().add(new Instruction(Operation.stackalloc, 1));
            }else{
                startFn.getBodyItem().add(new Instruction(Operation.stackalloc, 0));
            }
            startFn.getBodyItem().add(new Instruction(Operation.call, fnPos));
            if(mainType){
                startFn.getBodyItem().add(new Instruction(Operation.popn,1));
            }
        }
        startFn.setBodyCount(startFn.getBodyItem().size());

    }

    /**
     * const常量分析过程
     *
     * @throws CompileError
     */
    private void analyseConstDeclaration(boolean isGlobal) throws CompileError {
        //const_decl_stmt -> 'const' IDENT ':' ty '=' expr ';'
        expect(TokenType.CONST_KW);

        Token nameToken = expect(TokenType.IDENT);

        String name = (String) nameToken.getValue();

        if(isGlobal){
            CurrentFnInstruction.add(new Instruction(Operation.globa, globalOffset));
        }else{
            CurrentFnInstruction.add(new Instruction(Operation.loca, localOffset));
        }

        // 冒号
        expect(TokenType.COLON);

        // ty
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


        // =
        expect(TokenType.ASSIGN);

        // expr
        TokenType t = analyseExpr(true);

        if(couToken.getTokenType() != t){ //ty '=' expr 类型是否相同
            throw new AnalyzeError(ErrorCode.NotDeclared, couToken.getStartPos());
        }

        CurrentFnInstruction.add(new Instruction(Operation.store64));

        // ;
        expect(TokenType.SEMICOLON);

        // 加入符号表
        if (!isGlobal) {
            addSymbol(name, true, couToken.getTokenType(), SymbolType.local, nameToken.getStartPos());
        } else {
            addSymbol(name, true, couToken.getTokenType(), SymbolType.global, nameToken.getStartPos());
        }

        //TODO
        //入栈
//        instructions.add(new Instruction());
    }

    /**
     * variable变量分析过程
     *
     * @throws CompileError
     */
    private void analyseVariableDeclaration(boolean isGlobal) throws CompileError {
        //let_decl_stmt -> 'let' IDENT ':' ty ('=' expr)? ';'

        expect(TokenType.LET_KW);

        Token nameToken = expect(TokenType.IDENT);


        //冒号
        expect(TokenType.COLON);

        // ty
        Token tyToken = expect(TokenType.IDENT);
        System.out.println(tyToken.getValue());
        if(tyToken.getValue().equals("double")){
            tyToken.setTokenType(TokenType.DOUBLE);
        }
        else if(tyToken.getValue().equals("int")){
            tyToken.setTokenType(TokenType.INT);
        }
        else{
            throw new AnalyzeError(ErrorCode.NotDeclared, tyToken.getStartPos());
        }

        if (nextIf(TokenType.ASSIGN) != null) {
            if(!isGlobal){
                CurrentFnInstruction.add(new Instruction(Operation.loca, localOffset));
            }else{
                CurrentFnInstruction.add(new Instruction(Operation.globa, globalOffset));
            }
            TokenType t = analyseExpr(true);
            if(tyToken.getTokenType() != t){ //ty ('=' expr)?
                throw new AnalyzeError(ErrorCode.NotDeclared, tyToken.getStartPos());
            }
            CurrentFnInstruction.add(new Instruction(Operation.store64));
        }


        // ;
        expect(TokenType.SEMICOLON);


        //TODO
        //加入符号表
        if (!isGlobal) {
            addSymbol(nameToken.getValue().toString(), false, tyToken.getTokenType(), SymbolType.local, nameToken.getStartPos());
        } else {
            addSymbol(nameToken.getValue().toString(), false, tyToken.getTokenType(), SymbolType.global, nameToken.getStartPos());

        }


        // TODO
        //入栈
//        instructions.add(new Instruction());
    }

    /**
     * function的分析过程
     */
    private void analyseFunctionDeclaration() throws CompileError {
        //function -> 'fn' IDENT '(' function_param_list? ')' '->' ty block_stmt

        FnInstruction fnInstruction = new FnInstruction();
        fnLists.add(fnInstruction);
        CurrentFnInstruction = fnInstruction.getBodyItem();

        boolean hasReturn = false;

        expect(TokenType.FN_KW);

        Token nameToken = expect(TokenType.IDENT);
        GlobalVariable.add(nameToken.getValue().toString()); //存入全局变量表
        fnInstruction.setName(globalOffset++); //取现在的globalOffset再加一

        System.out.println("fn名字： " + nameToken);

        if(nameToken.getValue().toString().equals("main")){
            hasMain = true;
            fnPos = fnLists.size()-1;
        }
        Symbol currentSymbol = addFnSymbol(nameToken.getValue().toString(), nameToken.getStartPos()); //加入符号表


        // (
        expect(TokenType.L_PAREN);


        //参数offset清零
        argsOffset = 0;

        //function_param_list
        if (check(TokenType.CONST_KW) || check(TokenType.IDENT)) {
            analyseFunctionParamList();
        }




        expect(TokenType.R_PAREN);

        expect(TokenType.ARROW);

        // ty
        Token couToken = expect(TokenType.IDENT);

        if(couToken.getValue().equals("double")){
            couToken.setTokenType(TokenType.DOUBLE);
            fnInstruction.setRet_slots(1);
            for(int i = symbolTable.size()-1; symbolTable.get(i).getSymbolType() == SymbolType.args; i--){
                symbolTable.get(i).setOffset(symbolTable.get(i).getOffset()+1);
            }
            if(nameToken.getValue().toString().equals("main")){
                mainType = true;
            }
        }
        else if(couToken.getValue().equals("int")){
            couToken.setTokenType(TokenType.INT);
            fnInstruction.setRet_slots(1); //return数量置1
            for(int i = symbolTable.size()-1; symbolTable.get(i).getSymbolType() == SymbolType.args; i--){
                symbolTable.get(i).setOffset(symbolTable.get(i).getOffset()+1);
            }
            if(nameToken.getValue().toString().equals("main")){
                mainType = true;
            }
        }
        else if(couToken.getValue().equals("void")){
            couToken.setTokenType(TokenType.VOID);
            fnInstruction.setRet_slots(0); //return数量置0
            if(nameToken.getValue() == "main"){
                mainType = false;
            }
        }
        else{
            throw new AnalyzeError(ErrorCode.NotDeclared, couToken.getStartPos());
        }


        fnInstruction.setParam_slots(argsOffset); //设置参数数量

        currentSymbol.setType(couToken.getTokenType()); //fn的type属性

        // block_stmt
        localOffset = 0;
        hasReturn = analyseBlockStmt(true, couToken.getTokenType(), false, null, -1);
        fnInstruction.setLoc_slots(localOffset);

        if(couToken.getTokenType()!=TokenType.VOID && !hasReturn){ //如果是fn 需要有return
            throw new AnalyzeError(ErrorCode.DuplicateDeclaration, new Pos(0,0));
        }else if(couToken.getTokenType()==TokenType.VOID && !hasReturn){
            CurrentFnInstruction.add(new Instruction(Operation.ret));
        }

        fnInstruction.setBodyCount(fnInstruction.getBodyItem().size());
    }

    /**
     * expr表达式分析过程
     */
    private TokenType analyseExpr(boolean f) throws CompileError {
        //expr->(negate_expr| assign_expr | call_expr | literal_expr | ident_expr | group_expr) {binary_operator expr|'as' ty}

        System.out.println("开始分析expr");
        TokenType type = null;

        //negate_expr
        if (check(TokenType.MINUS)) {
            System.out.println("这是negate_expr");
            type = analyseNegateExpr();
            if(type == TokenType.DOUBLE){
                CurrentFnInstruction.add(new Instruction(Operation.negf));
            }
            else if(type == TokenType.INT){
                CurrentFnInstruction.add(new Instruction(Operation.negi));
            }else{
                throw new AnalyzeError(ErrorCode.NotDeclared, new Pos(3,0));
            }
            System.out.println("negate_expr结束啦");
        }

        //assign | call | ident分析
        if (peek().getTokenType() == TokenType.IDENT) {
            Token nameToken = next();
            //TODO 只有ident

            Integer index = symbolHash.get(nameToken.getValue().toString());

            if (nextIf(TokenType.ASSIGN) != null) {  //assign

                if (index == null) { //符号表没有这个符号
                    throw new AnalyzeError(ErrorCode.NotDeclared, nameToken.getStartPos());
                }

                if(symbolTable.get(index).isConst()){
                    throw new AnalyzeError(ErrorCode.NotDeclared, nameToken.getStartPos());
                }

                if(symbolTable.get(index).getSymbolType() == SymbolType.global){
                    CurrentFnInstruction.add(new Instruction(Operation.globa, symbolTable.get(index).getOffset()));
                }
                else if(symbolTable.get(index).getSymbolType() == SymbolType.local){ //是局部变量
                    CurrentFnInstruction.add(new Instruction(Operation.loca, symbolTable.get(index).getOffset()));
                }
                else{
                    CurrentFnInstruction.add(new Instruction(Operation.arga, symbolTable.get(index).getOffset()));
                }

                TokenType l_type = symbolTable.get(index).getType(); //取l_expr的类型
                System.out.println("这是assign_expr");
                TokenType r_type = analyseExpr(true); //r_expr的类型

                if (l_type != r_type) { //如果不相等 语义报错
                    throw new AnalyzeError(ErrorCode.NotDeclared, nameToken.getStartPos());
                }

                CurrentFnInstruction.add(new Instruction(Operation.store64));
                type = TokenType.VOID; //赋值表达式的值类型永远是 void
                System.out.println("assign_expr结束啦");
            } else if (nextIf(TokenType.L_PAREN) != null) { //call
                System.out.println("这是call_expr");

                int currentGlobal = 0;
                ArrayList<TokenType> call_array = null;
                TokenType return_type;

                if (index == null) {
                    switch (nameToken.getValue().toString()) {
                        case "getint":
                        case "getchar":
                            call_array = new ArrayList<TokenType>();
                            return_type = TokenType.INT;
                            break;
                        case "getdouble":
                            call_array = new ArrayList<TokenType>();
                            return_type = TokenType.DOUBLE;
                            break;
                        case "putint":
                            call_array = new ArrayList<TokenType>() {{
                                add(TokenType.INT);
                            }};
                            return_type = TokenType.VOID;
                            break;
                        case "putdouble":
                            call_array = new ArrayList<TokenType>() {{
                                add(TokenType.DOUBLE);
                            }};
                            return_type = TokenType.VOID;
                            break;
                        case "putchar":
                            call_array = new ArrayList<TokenType>() {{
                                add(TokenType.INT);
                            }};
                            return_type = TokenType.VOID;
                            break;
                        case "putstr":
                            call_array = new ArrayList<TokenType>() {{
                                add(TokenType.INT);
                            }};
                            return_type = TokenType.VOID;
                            break;
                        case "putln":
                            call_array = new ArrayList<TokenType>();
                            return_type = TokenType.VOID;
                            break;
                        default:
                            throw new AnalyzeError(ErrorCode.NotDeclared, nameToken.getStartPos());
                    }
                    GlobalVariable.add(nameToken.getValue().toString()); //把标准库函数存入全局变量
                    currentGlobal = globalOffset ++;
                } else { //取到参数列表和返回类型
                    Symbol call_index = symbolTable.get(index);
                    call_array = call_index.getParams();
                    return_type = call_index.getType();
                    System.out.println("此时调用的函数： "+ call_index.getName());
                    System.out.println("返回类型： "+call_index.getType());
                }

                if(return_type == TokenType.INT || return_type == TokenType.DOUBLE){ //stackalloc 按返回类型判断
                    CurrentFnInstruction.add(new Instruction(Operation.stackalloc, 1));
                }else if(return_type == TokenType.VOID){
                    CurrentFnInstruction.add(new Instruction(Operation.stackalloc, 0));
                }



                if (nextIf(TokenType.R_PAREN) != null) { //无参数调用
                    if (call_array.size() != 0) {
                        throw new AnalyzeError(ErrorCode.NotDeclared, nameToken.getStartPos());
                    } else {
                        System.out.println("call_expr结束啦");
                        type = return_type;
                    }
                } else { //有参数调用
                    TokenType param0 = analyseExpr(true); //
                    int i = 0;
                    if (param0 != call_array.get(i)) {
                        System.out.println("param0:"+param0);
                        System.out.println("call_array get0:" + call_array.get(0));
                        throw new AnalyzeError(ErrorCode.NotDeclared, nameToken.getStartPos());
                    }
                    while (nextIf(TokenType.COMMA) != null) {
                        i++;
                        if (call_array.size() < i) { //参数个数不同 报错
                            throw new AnalyzeError(ErrorCode.NotDeclared, nameToken.getStartPos());
                        }
                        TokenType param = analyseExpr(true);
                        if (param != call_array.get(i)) {
                            throw new AnalyzeError(ErrorCode.NotDeclared, nameToken.getStartPos());
                        }
                    }
                    expect(TokenType.R_PAREN);
                    System.out.println("call_expr结束啦");
                    type = return_type;
                }
                if(index != null){
                    CurrentFnInstruction.add(new Instruction(Operation.call, symbolTable.get(index).getFnoffset()));
                }else{
                    CurrentFnInstruction.add(new Instruction(Operation.callname, currentGlobal));
                }
            } else { //只有IDENT
                if(index==null&&nameToken.getValue().toString().equals("double")){
                    type=TokenType.DOUBLE;
                }
                else if(index==null&&nameToken.getValue().toString().equals("int")){
                    type=TokenType.INT;
                }
                else if (index == null) {
                    throw new AnalyzeError(ErrorCode.NotDeclared, nameToken.getStartPos());
                }
                else{
                    Symbol symbol = symbolTable.get(index);

                    if(symbol.getSymbolType() == SymbolType.local){
                        CurrentFnInstruction.add(new Instruction(Operation.loca, symbol.getOffset()));
                    }
                    else if(symbol.getSymbolType() == SymbolType.global){ //取地址
                        CurrentFnInstruction.add(new Instruction(Operation.globa, symbol.getOffset()));
                    }
                    else{
                        CurrentFnInstruction.add(new Instruction(Operation.arga, symbol.getOffset()));
                    }

                    CurrentFnInstruction.add(new Instruction(Operation.load64)); //取值

                    type = symbolTable.get(index).getType();
                }
            }
        }

        //literal_expr
        else if (peek().getTokenType() == TokenType.UINT_LITERAL || peek().getTokenType() == TokenType.STRING_LITERAL || peek().getTokenType() == TokenType.DOUBLE_LITERAL || peek().getTokenType() == TokenType.CHAR_LITERAL) {
            System.out.println("这是literal_expr");

            if (peek().getTokenType() == TokenType.UINT_LITERAL) { //是无符号整数
                System.out.println("这里有个UINT：" + peek());

                type = TokenType.INT;

                CurrentFnInstruction.add(new Instruction(Operation.push, peek().getValue()));

                next();
                //TODO 注意此时还没有移动指针
            } else if (peek().getTokenType() == TokenType.STRING_LITERAL) {//是字符串
                //字符串需要存在全局变量
                GlobalVariable.add(peek().getValue().toString());
                globalOffset++;
                type = TokenType.INT;

                CurrentFnInstruction.add(new Instruction(Operation.push, (long)globalOffset-1));

                System.out.println("这里有个STRING：" + peek());
                next();
                //TODO 注意此时还没有移动指针
            } else if (peek().getTokenType() == TokenType.DOUBLE_LITERAL) { //double
                System.out.println("这里有个DOUBLE：" + peek());
                type = TokenType.DOUBLE;

                CurrentFnInstruction.add(new Instruction(Operation.push, Double.doubleToRawLongBits((double)peek().getValue())));

                next();
                //TODO 注意此时还没有移动指针
            } else if (peek().getTokenType() == TokenType.CHAR_LITERAL) { //char

                System.out.println("这里有个CHAR：" + peek());

                type = TokenType.INT;

                CurrentFnInstruction.add(new Instruction(Operation.push, (long)(char)peek().getValue()));

                next();
            }
            System.out.println("literal_expr结束啦");
        }

        //group_expr
        else if (check(TokenType.L_PAREN)) {
            System.out.println("这是group_expr");
            type = analyseGroupExpr();
            System.out.println("group分析完之后需要重新入栈的：" + type);
            System.out.println(f);
        }

        if (f) { //OPG 判断operator_expr 和 as_expr
            Stack stack = new Stack();
            stack.push('#');
            Stack Nstack = new Stack<>();
            if (type != null) {
                Nstack.push(type);
                System.out.println("push了（外层）：" + type);
                System.out.println("此时Nstack栈：" + Nstack);
            }
            while (check(TokenType.AS_KW) || check(TokenType.PLUS) || check(TokenType.MINUS) || check(TokenType.MUL) || check(TokenType.DIV) || check(TokenType.EQ) || check(TokenType.NEQ) || check(TokenType.LT) || check(TokenType.GT) || check(TokenType.LE) || check(TokenType.GE)) {
                OPGAnalyse(stack, Nstack);
                TokenType second_type = analyseExpr(false);

                if (second_type != null) {
                    Nstack.push(second_type);
                    System.out.println("push了（内层）：" + second_type);
                    System.out.println("此时Nstack栈：" + Nstack);
                    second_type = null; //还原
                }

            }
            int sch = Symbol.indexOf(stack.peek());
            int ch = Symbol.indexOf(peek().getTokenType());
            while ((ch == -1 || SymbolMatrix[sch][ch] == 1) && stack.size() > 1) { //栈内大于当前 规约
                reduction(stack, Nstack);
            }
            type = (TokenType) Nstack.pop();
        }
        return type;
    }

    /**
     * OPGAnalyse
     */
    private void OPGAnalyse(Stack<TokenType> s, Stack Ns) throws TokenizeError {
        System.out.println("OPG开始分析");
        while (true) { //栈内大于当前 规约
            int sch = Symbol.indexOf(s.peek());
            int ch = Symbol.indexOf(peek().getTokenType());


            if (sch == -1 && ch == -1) { //都为#
                System.out.println("没有符号可以规约啦 都是# 结束！");
                return;
            } else if (sch == -1 || SymbolMatrix[sch][ch] == 0) { //栈内优先级小于当前字符 入栈
                System.out.println("栈内的符号：" + s.peek() + " 栈外的符号：" + peek().getTokenType() + " 栈内优先级小于栈外，入栈！");
                s.push(Symbol.get(ch));

                next();
                System.out.println("此时栈中符号：" + s);
                return;
            } else if((ch == -1 || SymbolMatrix[sch][ch] == 1) && s.size() > 1){
                System.out.println("站内符号：" + s.peek() + " 栈外符号：" + peek().getTokenType() + " 要规约了");
                reduction(s, Ns);
            }
        }
    }

    /**
     * reduction 规约
     */
    private void reduction(Stack<TokenType> s, Stack<Object> Ns) {
        System.out.println("规约了！");

        System.out.println("这时的非终结符栈：" + Ns);
        System.out.println("这时的符号栈：" + s);
        TokenType pop = s.pop(); //符号栈弹一个

        TokenType pop2 = (TokenType) Ns.pop(); //非终结符栈弹两个

        TokenType pop1 = (TokenType) Ns.pop();

        TokenType push = null;

        if (pop == TokenType.AS_KW) { //as指令分析
            if (pop1 == TokenType.DOUBLE || pop1 == TokenType.INT) {
                if (pop2 == TokenType.DOUBLE) {
                    push = TokenType.DOUBLE;
                    if(pop1 == TokenType.INT){
                        CurrentFnInstruction.add(new Instruction(Operation.itof));
                    }
                }
                if (pop2 == TokenType.INT) {
                    push = TokenType.INT;
                    if(pop1 == TokenType.DOUBLE){
                        CurrentFnInstruction.add(new Instruction(Operation.ftoi));
                    }
                }
            } else {
                System.exit(-1);
            }
        } else {
            if (pop1 != pop2) {
                System.exit(-1);
            }


            switch (pop) { //
                case PLUS:
                    if(pop1 == TokenType.INT){
                        push = TokenType.INT;
                        CurrentFnInstruction.add(new Instruction(Operation.addi));
                    }else{
                        push = TokenType.DOUBLE;
                        CurrentFnInstruction.add(new Instruction(Operation.addf));
                    }
                    break;
                case MINUS:
                    if(pop1 == TokenType.INT){
                        push = TokenType.INT;
                        CurrentFnInstruction.add(new Instruction(Operation.subi));
                    }else{
                        push = TokenType.DOUBLE;
                        CurrentFnInstruction.add(new Instruction(Operation.subf));
                    }
                    break;
                case MUL:
                    if(pop1 == TokenType.INT){
                        push = TokenType.INT;
                        CurrentFnInstruction.add(new Instruction(Operation.muli));
                    }else{
                        push = TokenType.DOUBLE;
                        CurrentFnInstruction.add(new Instruction(Operation.mulf));
                    }
                    break;
                case DIV:
                    if(pop1 == TokenType.INT){
                        push = TokenType.INT;
                        CurrentFnInstruction.add(new Instruction(Operation.divi));
                    }else{
                        push = TokenType.DOUBLE;
                        CurrentFnInstruction.add(new Instruction(Operation.divf));
                    }
                    break;
                case EQ:
                    if(pop1 == TokenType.INT){
                        push = TokenType.BOOL;
                        CurrentFnInstruction.add(new Instruction(Operation.cmpi));
                        CurrentFnInstruction.add(new Instruction(Operation.not));
                    }else{
                        push = TokenType.BOOL;
                        CurrentFnInstruction.add(new Instruction(Operation.cmpf));
                        CurrentFnInstruction.add(new Instruction(Operation.not));
                    }
                    break;
                case NEQ:
                    if(pop1 == TokenType.INT){
                        push = TokenType.BOOL;
                        CurrentFnInstruction.add(new Instruction(Operation.cmpi));
                    }else{
                        push = TokenType.BOOL;
                        CurrentFnInstruction.add(new Instruction(Operation.cmpf));
                    }
                    break;
                case LT:
                    if(pop1 == TokenType.INT){
                        push = TokenType.BOOL;
                        CurrentFnInstruction.add(new Instruction(Operation.cmpi));
                        CurrentFnInstruction.add(new Instruction(Operation.setlt));
                    }else{
                        push = TokenType.BOOL;
                        CurrentFnInstruction.add(new Instruction(Operation.cmpf));
                        CurrentFnInstruction.add(new Instruction(Operation.setlt));
                    }
                    break;
                case GT:
                    if(pop1 == TokenType.INT){
                        push = TokenType.BOOL;
                        CurrentFnInstruction.add(new Instruction(Operation.cmpi));
                        CurrentFnInstruction.add(new Instruction(Operation.setgt));
                    }else{
                        push = TokenType.BOOL;
                        CurrentFnInstruction.add(new Instruction(Operation.cmpf));
                        CurrentFnInstruction.add(new Instruction(Operation.setgt));
                    }
                    break;
                case LE:
                    if(pop1 == TokenType.INT){
                        push = TokenType.BOOL;
                        CurrentFnInstruction.add(new Instruction(Operation.cmpi));
                        CurrentFnInstruction.add(new Instruction(Operation.setgt));
                        CurrentFnInstruction.add(new Instruction(Operation.not));
                    }else{
                        push = TokenType.BOOL;
                        CurrentFnInstruction.add(new Instruction(Operation.cmpf));
                        CurrentFnInstruction.add(new Instruction(Operation.setgt));
                        CurrentFnInstruction.add(new Instruction(Operation.not));
                    }
                    break;
                case GE:
                    if(pop1 == TokenType.INT){
                        push = TokenType.BOOL;
                        CurrentFnInstruction.add(new Instruction(Operation.cmpi));
                        CurrentFnInstruction.add(new Instruction(Operation.setlt));
                        CurrentFnInstruction.add(new Instruction(Operation.not));
                    }else{
                        push = TokenType.BOOL;
                        CurrentFnInstruction.add(new Instruction(Operation.cmpf));
                        CurrentFnInstruction.add(new Instruction(Operation.setlt));
                        CurrentFnInstruction.add(new Instruction(Operation.not));
                    }
                    break;
                default:
                    System.exit(-1);
            }
        }

        System.out.println("pop后的Ns： " + Ns);
        System.out.println("pop后的s: " + s);


        Ns.push(push);

        System.out.println("push N规约后 此时假装他是个IDENT,此时Ns：" + Ns);
    }

    /**
     * negate_expr
     */
    private TokenType analyseNegateExpr() throws CompileError {
        expect(TokenType.MINUS);
        return analyseExpr(true);
    }

    /**
     * analyseGroupExpr
     */
    private TokenType analyseGroupExpr() throws CompileError {
        expect(TokenType.L_PAREN);
        TokenType tokenType = analyseExpr(true);
        expect(TokenType.R_PAREN);
        System.out.println("group 分析完了！！！");
        return tokenType;
    }

    /**
     * function_param_list分析入口
     */
    private void analyseFunctionParamList() throws CompileError {
        //function_param_list -> function_param (',' function_param)*
        //function_param -> 'const'? IDENT ':' ty
        analyseFunctionParam();

        while (nextIf(TokenType.COMMA) != null) {
            analyseFunctionParam();
        }
    }

    /**
     * function_param分析
     */
    private void analyseFunctionParam() throws CompileError {
        //function_param -> 'const'? IDENT ':' ty
        if (nextIf(TokenType.CONST_KW) != null) { //如果有const，说明为常量
            Token nameToken = expect(TokenType.IDENT); //取常量名
            expect(TokenType.COLON); // :
            Token tyToken = expect(TokenType.IDENT); //取常量值

            switch (tyToken.getValue().toString()) {
                case "double":
                    //加入符号表
                    addSymbol(nameToken.getValue().toString(), true, TokenType.DOUBLE, SymbolType.args, nameToken.getStartPos()); //常量加入符号栈
                    this.symbolTable.get(this.symbolInt.peek() - 1).getParams().add(TokenType.DOUBLE); //把形参放进fn的paramlist
                    break;
                case "int":
                    //加入符号表
                    addSymbol(nameToken.getValue().toString(), true, TokenType.INT, SymbolType.args, nameToken.getStartPos()); //常量加入符号栈
                    this.symbolTable.get(this.symbolInt.peek() - 1).getParams().add(TokenType.INT);
                    break;
                default:
                    throw new AnalyzeError(ErrorCode.DuplicateDeclaration, nameToken.getStartPos());
            }


            //TODO
        } else { //没有const说明为变量
            Token nameToken = expect(TokenType.IDENT); //取常量名
            expect(TokenType.COLON); // :
            Token tyToken = expect(TokenType.IDENT); //取常量值

            switch (tyToken.getValue().toString()) {
                case "double":
                    //加入符号表
                    addSymbol(nameToken.getValue().toString(), false, TokenType.DOUBLE, SymbolType.args, nameToken.getStartPos()); //常量加入符号栈
                    this.symbolTable.get(this.symbolInt.peek() - 1).getParams().add(TokenType.DOUBLE); //把形参放进fn的paramlist
                    break;
                case "int":
                    //加入符号表
                    addSymbol(nameToken.getValue().toString(), false, TokenType.INT, SymbolType.args, nameToken.getStartPos()); //常量加入符号栈
                    this.symbolTable.get(this.symbolInt.peek() - 1).getParams().add(TokenType.INT);
                    break;
                default:
                    throw new AnalyzeError(ErrorCode.DuplicateDeclaration, nameToken.getStartPos());


                    //TODO
            }
        }
    }


    /**
     * stmt
     */
    private boolean analyseStmt(TokenType tyTokenType, boolean isWhile , ArrayList<Integer> breakEndPos, int continuePos) throws CompileError {
        //stmt ->
        //      expr_stmt
        //    | decl_stmt
        //    | if_stmt
        //    | while_stmt
        //    | break_stmt
        //    | continue_stmt
        //    | return_stmt
        //    | block_stmt
        //    | empty_stmt

        //expr_stmt
        if (check(TokenType.MINUS) || check(TokenType.IDENT) || check(TokenType.UINT_LITERAL) || check(TokenType.L_PAREN) || check(TokenType.DOUBLE_LITERAL) || check(TokenType.STRING_LITERAL) || check(TokenType.CHAR_LITERAL)) { //expr_stmt
            System.out.println("expr_stmt分析");
            analyseExprStmt();
        }

        //decl_stmt
        if (check(TokenType.CONST_KW)) { //decl_stmt
            System.out.println("decl语句开始分析");
            analyseConstDeclaration(false);
        }

        //let_stmt
        if (check(TokenType.LET_KW)) {
            System.out.println("let语句开始分析");
            analyseVariableDeclaration(false);
        }

        //if_stmt
        if (check(TokenType.IF_KW)) { //if_stmt
            System.out.println("if语句开始分析");
            return analyseIfStmt(tyTokenType, isWhile, breakEndPos, continuePos);
        }

        //while_stmt
        if (check(TokenType.WHILE_KW)) {
            System.out.println("while语句开始分析");
            analyseWhileStmt(tyTokenType);
        }

        //break_stmt
        if (check(TokenType.BREAK_KW)) {
            System.out.println("break语句开始分析");
            if(!isWhile){
                throw new AnalyzeError(ErrorCode.DuplicateDeclaration, new Pos(2,0));
            }
            analyseBreakStmt();
            CurrentFnInstruction.add(new Instruction(Operation.br));
            int breakPos = CurrentFnInstruction.size()-1;
            breakEndPos.add(breakPos);
        }

        //continue_stmt
        if (check(TokenType.CONTINUE_KW)) {
            System.out.println("continue语句开始分析");
            if(!isWhile){
                throw new AnalyzeError(ErrorCode.DuplicateDeclaration, new Pos(2,0));
            }
            analyseContinueStmt();
            CurrentFnInstruction.add(new Instruction(Operation.br,continuePos-CurrentFnInstruction.size()));
        }

        //return_stmt
        if (check(TokenType.RETURN_KW)) {
            System.out.println("return 语句开始分析");
            analyseReturnStmt(tyTokenType);
            return true; //有return
        }

        //block_stmt
        if (check(TokenType.L_BRACE)) {
            System.out.println("block语句开始分析");
            return analyseBlockStmt(false, tyTokenType, isWhile, breakEndPos, continuePos);
        }

        //empty_stmt
        if (check(TokenType.SEMICOLON)) {
            System.out.println("empty语句开始分析");
            analyseEmptyStmt();
        }
        return false;
    }


    /**
     * empty_stmt
     *
     * @throws CompileError
     */
    private void analyseEmptyStmt() throws CompileError {
        //empty_stmt -> ';'
        expect(TokenType.SEMICOLON);
    }

    /**
     * block_stmt
     *
     * @throws CompileError
     */
    private boolean analyseBlockStmt(boolean isFn, TokenType tyTokenType, boolean isWhile, ArrayList<Integer> breakEndPos, int continuePos) throws CompileError {
        //block_stmt -> '{' stmt* '}'
        boolean hasReturn = false;
        expect(TokenType.L_BRACE);

        if (!isFn) {
            symbolInt.push(symbolTable.size());
        }
        System.out.println(check(TokenType.MINUS));
        while (check(TokenType.MINUS) || check(TokenType.IDENT) || check(TokenType.UINT_LITERAL) || check(TokenType.DOUBLE_LITERAL) || check(TokenType.STRING_LITERAL) || check(TokenType.CHAR_LITERAL) || check(TokenType.L_PAREN) || check(TokenType.LET_KW) ||
                check(TokenType.CONST_KW) || check(TokenType.IF_KW) || check(TokenType.WHILE_KW) || check(TokenType.BREAK_KW) || check(TokenType.CONTINUE_KW) || check(TokenType.RETURN_KW) || check(TokenType.SEMICOLON) || check(TokenType.L_BRACE)) {
//            System.out.println("这是block里的stmt循环分析！");
            if(!hasReturn){
                hasReturn = analyseStmt(tyTokenType, isWhile, breakEndPos, continuePos);//进入stmt循环分析
            }
            else{
                analyseStmt(tyTokenType, isWhile, breakEndPos, continuePos); //进入stmt循环分析
            }
        }
        expect(TokenType.R_BRACE);

        //删块
        int index = symbolInt.pop();
        while (symbolTable.size() > index) {
            Symbol s = symbolTable.pop();
            if (s.getChain() != -1) { //如果chain不为-1，更新hash表中的对应值
                symbolHash.put(s.getName(), s.getChain());
            } else { //没有重合元素，直接remove
                symbolHash.remove(s.getName());
            }
        }

        return hasReturn;
    }

    /**
     * return_stmt
     */
    private void analyseReturnStmt(TokenType tyTokenType) throws CompileError {
        //return_stmt -> 'return' expr? ';'
        expect(TokenType.RETURN_KW);
        if(tyTokenType == TokenType.INT || tyTokenType == TokenType.DOUBLE){
            CurrentFnInstruction.add(new Instruction(Operation.arga, 0));
        }
        if (check(TokenType.MINUS) || check(TokenType.IDENT) || check(TokenType.UINT_LITERAL) || check(TokenType.L_PAREN) || check(TokenType.DOUBLE_LITERAL) || check(TokenType.STRING_LITERAL) || check(TokenType.CHAR_LITERAL)) {
            TokenType exprType = analyseExpr(true);
            if(exprType != tyTokenType){
                throw new AnalyzeError(ErrorCode.DuplicateDeclaration, new Pos(1,0));
            }
        }else{
            if(tyTokenType != TokenType.VOID){
                throw new AnalyzeError(ErrorCode.DuplicateDeclaration, new Pos(1,0));
            }
        }
        if(tyTokenType == TokenType.INT || tyTokenType == TokenType.DOUBLE){
            CurrentFnInstruction.add(new Instruction(Operation.store64));
        }
        CurrentFnInstruction.add(new Instruction(Operation.ret));
        expect(TokenType.SEMICOLON);
    }

    /**
     * continue_stmt
     */
    private void analyseContinueStmt() throws CompileError {
        //continue_stmt -> 'continue' ';'
        expect(TokenType.CONTINUE_KW);
        expect(TokenType.SEMICOLON);
    }

    /**
     * break_stmt
     */
    private void analyseBreakStmt() throws CompileError {
        //break_stmt -> 'break' ';'
        expect(TokenType.BREAK_KW);
        expect(TokenType.SEMICOLON);
    }

    /**
     * while_stmt
     */
    private void analyseWhileStmt(TokenType tyTokenType) throws CompileError {
        //while_stmt -> 'while' expr block_stmt
        expect(TokenType.WHILE_KW);

        int InitPos=CurrentFnInstruction.size()-1;
        TokenType whileExpr = analyseExpr(true);

        ArrayList<Integer> breakEndPos = new ArrayList<>();


        CurrentFnInstruction.add(new Instruction(Operation.brtrue, 1));

        CurrentFnInstruction.add(new Instruction(Operation.br));
        int currentPos = CurrentFnInstruction.size()-1;

        if(whileExpr == TokenType.VOID){
            throw new AnalyzeError(ErrorCode.DuplicateDeclaration, new Pos(1,0));
        }
        analyseBlockStmt(false, tyTokenType, true, breakEndPos, InitPos);
        CurrentFnInstruction.add(new Instruction(Operation.br, InitPos-CurrentFnInstruction.size()));
        CurrentFnInstruction.get(currentPos).setValue(CurrentFnInstruction.size()-1 - currentPos);
        for(int i = 0; i < breakEndPos.size(); i ++){
            CurrentFnInstruction.get(breakEndPos.get(i)).setValue(CurrentFnInstruction.size()-1-breakEndPos.get(i)); //存每一个break
        }
    }

    /**
     * if_stmt
     */
    private boolean analyseIfStmt(TokenType tyTokenType, boolean isWhile, ArrayList<Integer> breakEndPos, int continuePos) throws CompileError {
        //if_stmt -> 'if' expr block_stmt ('else' 'if' expr block_stmt)* ('else' block_stmt)?
        expect(TokenType.IF_KW);
        TokenType ifexpr = analyseExpr(true);
        if(ifexpr == TokenType.VOID){
            throw new AnalyzeError(ErrorCode.DuplicateDeclaration, new Pos(1,0));
        }
        boolean hasReturn = false;
        boolean hasElse = false;
        System.out.println("进入if的{}块了！");

        CurrentFnInstruction.add(new Instruction(Operation.brtrue, 1));
        CurrentFnInstruction.add(new Instruction(Operation.br));
        int currentPos = CurrentFnInstruction.size()-1; //br指令的当前位置

        hasReturn = analyseBlockStmt(false, tyTokenType, isWhile, breakEndPos, continuePos); //if 第一个block块
        CurrentFnInstruction.add(new Instruction(Operation.br)); //if块结束跳转
        int endPos = CurrentFnInstruction.size()-1;
        CurrentFnInstruction.get(currentPos).setValue(CurrentFnInstruction.size()-1 - currentPos);



        ArrayList<Integer> Pos = new ArrayList<>();
        while (nextIf(TokenType.ELSE_KW) != null) { //如果有else
            System.out.println("有else哦");
            if (nextIf(TokenType.IF_KW) != null) { // 是else if的情况
                ifexpr = analyseExpr(true);
                CurrentFnInstruction.add(new Instruction(Operation.brtrue, 1));
                CurrentFnInstruction.add(new Instruction(Operation.br));
                int currentPos1 = CurrentFnInstruction.size()-1; //br指令的当前位置



                if(ifexpr == TokenType.VOID){
                    throw new AnalyzeError(ErrorCode.DuplicateDeclaration, new Pos(1,0));
                }
                hasReturn &= analyseBlockStmt(false, tyTokenType, isWhile, breakEndPos, continuePos);
                CurrentFnInstruction.add(new Instruction(Operation.br));
                Pos.add(CurrentFnInstruction.size()-1);
                CurrentFnInstruction.get(currentPos1).setValue(CurrentFnInstruction.size()-1 - currentPos1);
            } else if (check(TokenType.L_BRACE)) { //只有else的情况
                hasReturn &= analyseBlockStmt(false, tyTokenType, isWhile, breakEndPos, continuePos);
                hasElse = true;
                break;
            }
        }
        CurrentFnInstruction.get(endPos).setValue(CurrentFnInstruction.size()-1-endPos);
        for(int i = 0; i < Pos.size(); i ++){
            CurrentFnInstruction.get(Pos.get(i)).setValue(CurrentFnInstruction.size()-1-Pos.get(i)); //循环存每一个elseif
        }
        if(!hasElse){
            return false;
        }
        return hasReturn;
    }

    /**
     * expr_stmt
     */
    private void analyseExprStmt() throws CompileError {
        //expr_stmt -> expr ';'
        TokenType t = null;
        if (check(TokenType.MINUS) || check(TokenType.IDENT) || check(TokenType.UINT_LITERAL) || check(TokenType.L_PAREN) || check(TokenType.DOUBLE_LITERAL) || check(TokenType.STRING_LITERAL) || check(TokenType.CHAR_LITERAL)) {
            t = analyseExpr(true);
        }
        if(t != TokenType.VOID){
            CurrentFnInstruction.add(new Instruction(Operation.popn, 1));
        }
        expect(TokenType.SEMICOLON);
    }


}
