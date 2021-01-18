package Analyser;

import Tokenizer.TokenType;

import java.util.ArrayList;

public class Symbol {
    private String name;
    private int chain=-1;
    private TokenType type;
    private boolean isConst=false;
    private boolean isFn=false;
    private ArrayList<TokenType> params=null;
    private SymbolType symbolType;
    private int fnoffset;
    private int offset;

    public Symbol(String name, int chain, TokenType type, boolean isConst, SymbolType symbolType, int offset) {
        this.name = name;
        this.chain = chain;
        this.type = type;
        this.isConst = isConst;
        this.symbolType = symbolType;
        this.offset = offset;
    }

    public Symbol(String name,boolean isFn, int offset, int fnoffset) {
        this.name = name;
        this.isFn = isFn;
        this.params = new ArrayList<TokenType>();
        this.symbolType = SymbolType.global;
        this.offset = offset;
        this.fnoffset = fnoffset;
        this.isConst = true;
    }

    public int getFnoffset() {
        return fnoffset;
    }

    public void setFnoffset(int fnoffset) {
        this.fnoffset = fnoffset;
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

    public ArrayList<TokenType> getParams() {
        return params;
    }

    public void setParams(ArrayList<TokenType> params) {
        this.params = params;
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
