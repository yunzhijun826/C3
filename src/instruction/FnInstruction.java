
package instruction;

import java.util.ArrayList;

public class FnInstruction {
    private int name;
    private int ret_slots;
    private int param_slots;
    private int loc_slots;
    private int bodyCount;
    private ArrayList<Instruction> bodyItem;

    @Override
    public String toString() {
        String out=
                "name=" + name +
                        ", ret_slots=" + ret_slots +
                        ", param_slots=" + param_slots +
                        ", loc_slots=" + loc_slots +
                        ", bodyCount=" + bodyCount +"\n";
        for (Instruction instruction : bodyItem) {
            out+=instruction.toString()+"\n";
        }
        return out;
    }

    public FnInstruction() {
        this.bodyItem = new ArrayList<Instruction>();
    }

    public int getName() {
        return name;
    }

    public void setName(int name) {
        this.name = name;
    }

    public int getRet_slots() {
        return ret_slots;
    }

    public void setRet_slots(int ret_slots) {
        this.ret_slots = ret_slots;
    }

    public int getParam_slots() {
        return param_slots;
    }

    public void setParam_slots(int param_slots) {
        this.param_slots = param_slots;
    }

    public int getLoc_slots() {
        return loc_slots;
    }

    public void setLoc_slots(int loc_slots) {
        this.loc_slots = loc_slots;
    }

    public int getBodyCount() {
        return bodyCount;
    }

    public void setBodyCount(int bodyCount) {
        this.bodyCount = bodyCount;
    }

    public ArrayList<Instruction> getBodyItem() {
        return bodyItem;
    }

    public void setBodyItem(ArrayList<Instruction> bodyItem) {
        this.bodyItem = bodyItem;
    }
}
