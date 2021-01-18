package instruction;

import java.util.Objects;

public class Instruction {
    private Operation opt;
    private Object value;

    @Override
    public String toString() {
        return opt +
                "(" + value +
                ')';
    }

    public Instruction(Operation opt) {
        this.opt = opt;
        this.value = null;
    }

    public Instruction(Operation opt, Object x) {
        this.opt = opt;
        this.value = x;
    }

    public Operation getOpt() {
        return opt;
    }

    public void setOpt(Operation opt) {
        this.opt = opt;
    }

    public Object getValue() {
        return value;
    }

    public void setValue(Object value) {
        this.value = value;
    }
}
