
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.JFileChooser;

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author dh2744
 */
public class CheckSystemsPath {
    public static ArrayList<String>  checkSystemsPath (javax.swing.JTextArea outputTextArea) {
        //ArrayList<String> envVars = new ArrayList<>();
        Vector<String> envVars = new Vector<String>();
        //compatibility with java 1.6
        //ArrayList<String> RscriptString = new ArrayList<>();
        ArrayList<String> RscriptString = new ArrayList<String>();
        
        File fileWD = new File(System.getProperty("java.class.path"));
        File rootPath = fileWD.getAbsoluteFile().getParentFile();
        
        String rootPath2Slash = rootPath.toString();
        
        //needed for paths in PC, won't replace in apple path
        rootPath2Slash = rootPath2Slash.replace("\\", "\\\\");
        
        File systemPaths = new File( rootPath2Slash +File.separator+
                File.separator+"Code" +
                File.separator+File.separator+"systemPaths.txt");
           System.out.println("Before envVars");
            envVars=GetEnvVars.getEnvVars( systemPaths, outputTextArea );
            System.out.println( "envVars "+ envVars.toString() );
        
        
        
        //check for error in envVars
        Boolean errorEnvVar=false;
        errorEnvVar=envVars.contains("error");
        System.out.println( "errorEnvVar "+ errorEnvVar.toString() );
        
        
        // get prepare.igm.mea.R with added library location
        String meaRtoolsLibInstallPath = rootPath2Slash + File.separator+ File.separator +
                "packages";
        String meaRtoolsmeaRtoolsPath = rootPath2Slash + File.separator+ File.separator +
                "packages/meaRtools";
        File meaRtoolsmeaRtoolsPathFile = new File (meaRtoolsmeaRtoolsPath);
        System.out.println("meaRtoolsPrepareFile "+ meaRtoolsmeaRtoolsPathFile.toString() );
        
        
        
        
        String meaRtoolsPrepareString = rootPath2Slash + File.separator+ File.separator +
                "Code"+ File.separator+ File.separator + "prepare.meaRtools.R ";
        File meaRtoolsPrepareFile = new File (meaRtoolsPrepareString);
        System.out.println("meaRtoolsPrepareFile "+ meaRtoolsPrepareFile.toString() );
        
        
        

        try {
            if (errorEnvVar){
                //file chooser to get and write file
                JFileChooser chooser = new JFileChooser();
                chooser.setCurrentDirectory(rootPath);
                chooser.setDialogTitle("Find and open Rscript.exe");
                chooser.setMultiSelectionEnabled(false);
                chooser.showOpenDialog(null);
                File RscriptFile = chooser.getSelectedFile();
                System.out.println("RscriptFile.getAbsolutePath() "+ RscriptFile.getAbsolutePath() );
                RscriptString.add( RscriptFile.getAbsolutePath());
                System.out.println("RscriptString.get(0) "+RscriptString.get(0));

                System.out.println("RscriptString "+ RscriptString.get(0) );
                // write file
                
                
                    FileWriter f = null;
                    System.out.println("systemPathsFile "+ RscriptString.get(0));

                    f = new FileWriter(rootPath + File.separator+"Code"+File.separator+ "systemPaths.txt");
                    f.write( RscriptString.get(0));
                    f.write("\n");
                    f.close(); 
                    
                    System.out.println("systemPathsFile "+ RscriptString.get(0) );

                    //FileWriter t = null;
                    //t= new FileWriter("/Users/dh2744/Dropbox/Columbia/Software/github/mea/MEA_analysis/Code/systemPaths_2.txt");
                    //t.write( RscriptString.get(0).toString() );
                    //t.write("\n");
                    //t.write( "Hellow pancake" );
                    //t.close(); 
                    
                
                
            
                System.out.println(" After write systemPathsFile.txt " );
                
                //call prepare.igm.mea
                String cmd0 = RscriptString.get(0).toString() + " " + 
                        meaRtoolsPrepareFile.toString() + " 2>&1";
                System.out.println( "prepare.meaRtools with lib location " + cmd0 );
                System.out.println( "/n "  );
                System.out.println( "/n "  );
                System.out.println( "cmd0 " + cmd0 );
                System.out.println( "/n "  );
                Vector<String> output = SystemCall.systemCall2(cmd0);
                
                // print to a log file
                String installLogString = rootPath2Slash + File.separator+ File.separator +
                "Code"+ File.separator+ File.separator + "meaRtools_install_log.txt";
                File installLogFile = new File (installLogString);
                System.out.println("installLogFile "+ installLogFile.toString() );
                
                // write to log file
                DateFormat df = new SimpleDateFormat("yyyy-MM-dd-hh_mm_ss");
                System.out.println(" df "+df);
                Date dateobj = new Date();
                System.out.println(" dateobj "+ dateobj );
                String dateString= df.format(dateobj);
                System.out.println("dateString "+ dateString );
 


                FileWriter f1 = null;
                try {

                    f = new FileWriter( installLogFile );
                    for (int i = 0; i < output.size(); i++ ) {
                        f.write(output.get(i));
                        f.write("\n");
                    }

                    f.write("\n");
                    f.close();
                 } catch (IOException ex) {
                    System.out.println("error in opening IGMMEA install log file");
                    Logger.getLogger(IGM_MEA_main.class.getName()).log(Level.SEVERE, null, ex);
                }
      
                
                
                //Process myProc = Runtime.getRuntime().exec(cmd0);
                //System.out.println("Waiting for batch file ...");
                //myProc.waitFor();
                System.out.println("write to log file done.");
                
                System.out.println(" After  write prepare.meaRtools.R  " );
                
                
            } else{
                
                RscriptString.add(  envVars.get(0).toString() );
                
                System.out.println("RscriptString.get(0)"+ RscriptString.get(0));
            }
            
            
            
        } catch (Exception e) {
            RscriptString.add( "error" );
            //print some error message
        }
        
        
        
        return( RscriptString );
    }
    
}
