
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
 * @author dh2744
 */
public class PrintToTextArea {
    public static int printToTextArea(Vector<String> toPrint, javax.swing.JTextArea outputTextArea) {
        int exitValue = 0;
        
        try {

            outputTextArea.setText("");
            outputTextArea.update(outputTextArea.getGraphics());
            
               
                for (int i = 0; i < toPrint.size(); i++) {
                    outputTextArea.append(toPrint.get(i) + "\n");
                }
                
                outputTextArea.setCaretPosition(outputTextArea.getDocument().getLength());
                outputTextArea.update(outputTextArea.getGraphics());    
         
            
            
        } catch (Exception e) {
            exitValue = -1;
            //print some error message
        }
        return (exitValue);
    }
}
