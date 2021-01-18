package Analyser;

import Tokenizer.TokenType;

import java.util.ArrayList;

public class Symbol {
    private String name;
    private int chain=-1;
    private TokenType type;
    private boolean isConst=false;
    private boolean isFn=false;
    private ArrayList<TokenType> paramsList =null;
    private SymbolType symbolType;
    private int fnOffset;
    private int offset;

    public Symbol(String name, int chain, TokenType type, boolean isConst, SymbolType symbolType, int offset) {
        this.name = name;
        this.chain = chain;
        this.type = type;
        this.isConst = isConst;
        this.symbolType = symbolType;
        this.offset = offset;
    }

    public Symbol(String name,boolean isFn, int offset, int fnOffset) {
        this.name = name;
        this.isFn = isFn;
        this.paramsList = new ArrayList<TokenType>();
        this.symbolType = SymbolType.global;
        this.offset = offset;
        this.fnOffset = fnOffset;
        this.isConst = true;
    }

    public int getFnOffset() {
        return fnOffset;
    }

    public void setFnOffset(int fnOffset) {
        this.fnOffset = fnOffset;
    }

    public SymbolType getSymbolType() {
        return symbolType;
    }

    public void setSymbolType(SymbolType symbolType) {
        this.symbolType = symbolType;
    }

    public int getOffset() {
        return offset;
    }

    public void setOffset(int offset) {
        this.offset = offset;
    }

    public TokenType getType() {
        return type;
    }

    public void setType(TokenType type) {
        this.type = type;
    }

    public boolean isConst() {
        return isConst;
    }

    public void setConst(boolean aConst) {
        isConst = aConst;
    }

    public boolean isFn() {
        return isFn;
    }

    public void setFn(boolean fn) {
        isFn = fn;
    }

    public ArrayList<TokenType> getParamsList() {
        return paramsList;
    }

    public void setParamsList(ArrayList<TokenType> paramsList) {
        this.paramsList = paramsList;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public int getChain() {
        return chain;
    }

    public void setChain(int chain) {
        this.chain = chain;
    }
}
