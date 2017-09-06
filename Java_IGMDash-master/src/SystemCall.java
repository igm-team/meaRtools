
import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Vector;
/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
/**
 *
 * @author qw2192
 */
public class SystemCall {


    public static int systemCall(String cmd, javax.swing.JTextArea outputTextArea) {
        int exitValue = 0;
        try {
            int numberOfLines2Show = 7;
            outputTextArea.setText("New Run\n");
            outputTextArea.update(outputTextArea.getGraphics());
            
            Process myProc = Runtime.getRuntime().exec(cmd);
            InputStream is = myProc.getInputStream();
            InputStreamReader isr = new InputStreamReader(is);
            BufferedReader stdInput = new BufferedReader(isr);
            
            BufferedReader stdError = new BufferedReader(new InputStreamReader(myProc.getErrorStream()));

             
            String line;
            //Vector<String> result = new Vector<>();
            Vector<String> result = new Vector<String>();
            while ((line = stdInput.readLine()) != null || (line = stdError.readLine()) != null ) {
                result.add(line);
                
                int lastline = result.size();
                int firstline = 0; 
                while (firstline + numberOfLines2Show < lastline) {
                    firstline++;
                }
                outputTextArea.setText("");
                for (int i = firstline; i < lastline; i++) {
                    outputTextArea.append(result.get(i) + "\n");
                }
                
                outputTextArea.setCaretPosition(outputTextArea.getDocument().getLength());
                outputTextArea.update(outputTextArea.getGraphics());    
            }
            outputTextArea.setText("");
            for (int i = 0; i < result.size(); i++) {
                outputTextArea.append(result.get(i) + "\n");
            }
            
        } catch (Exception e) {
            exitValue = -1;
            //print some error message
        }
        return (exitValue);
    }
    
    //return  vector of strings from thread
    public static Vector<String> systemCall2(String cmd) {
        Vector<String> result = new Vector<String>();
        try {    
            Process myProc = Runtime.getRuntime().exec(cmd);
            InputStream is = myProc.getInputStream();
            InputStreamReader isr = new InputStreamReader(is);
            BufferedReader stdInput = new BufferedReader(isr);
            BufferedReader stdError = new BufferedReader(new InputStreamReader(myProc.getErrorStream()));
            
            String line;
            while ((line = stdInput.readLine()) != null || (line = stdError.readLine()) != null ) {
                result.add(line);
                
            }
            
        } catch (Exception e) {
            result.add(e.toString());
            //print some error message
        }
        return result;
    }

}
