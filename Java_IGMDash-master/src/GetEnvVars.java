
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
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
public class GetEnvVars {
    //public static ArrayList<String> getEnvVars(File envVarsTextFile, javax.swing.JTextArea outputTextArea){
    public static Vector<String> getEnvVars(File envVarsTextFile, javax.swing.JTextArea outputTextArea){
        //ArrayList<String> envVars = new ArrayList<>();
        Vector<String> envVars = new Vector<String>();
        int exitValue;
        boolean errorLog;
        //Vector<String> logResult = new Vector<>();
        Vector<String> logResult = new Vector<String>();
        try {
            // FileReader reads text files in the default encoding.
            FileReader fileReader = new FileReader(envVarsTextFile);

            System.out.println("file reader " + envVarsTextFile.toString());
            // Always wrap FileReader in BufferedReader.
            BufferedReader bufferedReader = new BufferedReader(fileReader);

                //Rscript
                String line = bufferedReader.readLine();
                System.out.println(line);
                File Rscript = new File( line );
                System.out.println("Rscript " + Rscript.toString() );
                envVars.add(Rscript.toString());


            
            
            
            //error catch
            if (!Rscript.exists()) {
                logResult.add( "Rscript doesn't exist " + Rscript );
                logResult.add("Please check /Code/systemPaths.txt for correct path to Rscript.exe" );
                envVars.add("error");
                ErrorHandler.errorPanel("Rscript doesn't exist " + Rscript + '\n'
                                + "Please check /Code/systemPaths.txt for correct path to Rscript.exe" );
                        exitValue = -1;
                        errorLog=true;
                        
                        if(outputTextArea != null)
                        PrintToTextArea.printToTextArea(logResult , outputTextArea);
            }
            
            
            
            

        } catch (IOException ex) {
                    logResult.add( "Error reading file systemPaths "   );
                    logResult.add(" IOException " );
                    envVars.add("error");
                    //ErrorHandler.errorPanel( "Error reading systemPaths file");
                    errorLog = true;
                    exitValue = -1;

        }
        
        
        
        
        
        return( envVars );
     }

    // same function as above but without the print to textArea
    public static Vector<String> getEnvVars(File envVarsTextFile){
        //ArrayList<String> envVars = new ArrayList<>();
        Vector<String> envVars = new Vector<String>();
        int exitValue;
        boolean errorLog;
        //Vector<String> logResult = new Vector<>();
        Vector<String> logResult = new Vector<String>();
        try {
            // FileReader reads text files in the default encoding.
            FileReader fileReader = new FileReader(envVarsTextFile);

            System.out.println("file reader " + envVarsTextFile.toString());
            // Always wrap FileReader in BufferedReader.
            BufferedReader bufferedReader = new BufferedReader(fileReader);

                //Rscript
                String line = bufferedReader.readLine();
                System.out.println(line);
                File Rscript = new File( line );
                System.out.println("Rscript " + Rscript.toString() );
                envVars.add(Rscript.toString());


            
            
            
            //error catch
            if (!Rscript.exists()) {
                logResult.add( "Rscript doesn't exist " + Rscript );
                logResult.add("Please check /Code/systemPaths.txt for correct path to Rscript.exe" );
                envVars.add("error");
                ErrorHandler.errorPanel("Rscript doesn't exist " + Rscript + '\n'
                                + "Please check /Code/systemPaths.txt for correct path to Rscript.exe" );
                        exitValue = -1;
                        errorLog=true;

            }


        } catch (IOException ex) {
                    logResult.add( "Error reading file systemPaths "   );
                    logResult.add(" IOException " );
                    envVars.add("error");
                    //ErrorHandler.errorPanel( "Error reading systemPaths file");
                    errorLog = true;
                    exitValue = -1;

        }

        return( envVars );
     }
   
}
