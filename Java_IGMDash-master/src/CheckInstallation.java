
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author dh2744
 */
public class CheckInstallation {
     public static Vector<String>  checkInstallation ()  {
        
        Vector<String> systemReturn = new Vector<String>();
        Vector<String> logFileLines = new Vector<String>();
        
        File fileWD = new File(System.getProperty("java.class.path"));
        File workingDirectory = fileWD.getAbsoluteFile().getParentFile();
        String rootPath = workingDirectory.toString();

        String rootPath2Slash = rootPath.toString();
        
        //needed for paths in PC, won't replace in apple path
        rootPath2Slash = rootPath2Slash.replace("\\", "\\\\");
        
        //get path to Rscript
        String installationLogStrings = rootPath2Slash + File.separator+ File.separator +
                "Code"+ File.separator+ File.separator + "meaRtools_install_log.txt";
        File installationLogFile = new File (installationLogStrings);
        System.out.println("installationLogFile "+ installationLogFile.toString() );
        
        try {
            FileReader fileReader = new FileReader(installationLogFile);
            BufferedReader bufferedReader = new BufferedReader(fileReader);

            //Rscript
            String line = bufferedReader.readLine();
            int i=0;
            String message="meaRtools has not installed properly.";
            while(line!=null){
                logFileLines.add(line);
                System.out.println("line " + i + " " + line.toString() );
                line = bufferedReader.readLine();

                if( line.contains( "Installation complete." )){
                    systemReturn.add( "meaRTools has installed properly" );
                    System.out.println("systemReturn "+ systemReturn.toString() );
                    message="meaRtools installed properly.";
                   
                   break;
                }

                i=i+1;
            }
            if( message.endsWith("meaRtools has not installed properly." )){
                systemReturn.add("Didn't install properly");
            }
        } catch (IOException ex) {
                    System.out.println("error in opening meaRtools install log file");
                    
        }
            
         

        return( systemReturn );
    }
    
}
