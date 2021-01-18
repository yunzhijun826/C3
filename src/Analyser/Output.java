package Analyser;
import instruction.*;
import java.io.File;
import java.io.FileOutputStream;
import java.util.ArrayList;
public class Output {
    public static byte[] intToByte(int v) {
        byte[] a = new byte[4];
        a[3] = (byte) (v & 0xff);
        a[2] = (byte) ((v >> 8) & 0xff);
        a[1] = (byte) ((v >> 16) & 0xff);
        a[0] = (byte) ((v >> 24) & 0xff);
        return a;
    }
    public static byte[] longToByte(long v) {
        byte[] a = new byte[8];
        a[7] = (byte) (v & 0xff);
        a[6] = (byte) ((v >> 8) & 0xff);
        a[5] = (byte) ((v >> 16) & 0xff);
        a[4] = (byte) ((v >> 24) & 0xff);
        a[3] = (byte) ((v >> 32) & 0xff);
        a[2] = (byte) ((v >> 40) & 0xff);
        a[1] = (byte) ((v >> 48) & 0xff);
        a[0] = (byte) ((v >> 56) & 0xff);
        return a;
    }
    public static void Output(String name, ArrayList<String> global, ArrayList<FnInstruction> fnList) throws Exception{
        FileOutputStream f = new FileOutputStream(new File(name));
        f.write(intToByte(0x72303b3e));
        f.write(intToByte(0x1));
        int gSize=global.size();
        f.write(intToByte(gSize));
        for(int i = 0; i < gSize; i ++){
            if(global.get(i).equals("0")){
                f.write(1);
                f.write(intToByte(8));
                f.write(longToByte(0L));

            }else if(global.get(i).equals("1")){
                f.write(0);
                f.write(intToByte(8));
                f.write(longToByte(0L));
            }
            else{ //函数名、字符串
                f.write(1);
                f.write(intToByte(global.get(i).length()));
                f.write(global.get(i).getBytes());
            }
        }
        int fSize=fnList.size();
        f.write(intToByte(fSize));

        for(int i = 0; i < fSize; i ++){
            f.write(intToByte(fnList.get(i).getName()));
            f.write(intToByte(fnList.get(i).getRet_slots()));
            f.write(intToByte(fnList.get(i).getParam_slots()));
            f.write(intToByte(fnList.get(i).getLoc_slots()));
            f.write(intToByte(fnList.get(i).getBodyCount()));

            ArrayList<Instruction> fn_instructions = fnList.get(i).getBodyItem();
            int fnSize=fn_instructions.size();
            for(int j = 0; j < fnSize; j ++){
                f.write(fn_instructions.get(j).getOpt().getI());
                if(fn_instructions.get(j).getValue() != null){
                    if(fn_instructions.get(j).getOpt() == Operation.push){
                        f.write(longToByte((long)fn_instructions.get(j).getValue()));
                    }
                    else{
                        f.write(intToByte((int)fn_instructions.get(j).getValue()));
                    }
                }
            }
        }
    }
}
