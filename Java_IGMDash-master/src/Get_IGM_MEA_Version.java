/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
import java.io.BufferedReader;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.JFileChooser;
/**
 *
 * @author dh2744
 */
public class Get_IGM_MEA_Version {
    
    public static Vector<String>  get_IGM_MEA_Version (javax.swing.JTextArea outputTextArea) {
        ArrayList<String> RscriptString = new ArrayList<String>();
        Vector<String> systemReturn = new Vector<String>();
        File fileWD = new File(System.getProperty("java.class.path"));
        File workingDirectory = fileWD.getAbsoluteFile().getParentFile();
        String rootPath = workingDirectory.toString();

        String rootPath2Slash = rootPath.toString();
        
        //needed for paths in PC, won't replace in apple path
        rootPath2Slash = rootPath2Slash.replace("\\", "\\\\");
        
        //get path to Rscript
        String systemPathsStrings = rootPath2Slash + File.separator+ File.separator +
                "Code"+ File.separator+ File.separator + "systemPaths.txt";
        File systemPathsFile = new File (systemPathsStrings);
        System.out.println("systemPathsFile "+ systemPathsFile.toString() );
        
        RscriptString=CheckSystemsPath.checkSystemsPath( null );
        String cmd1= RscriptString.get(0).toString()+ " "  + rootPath2Slash + File.separator+ File.separator+
                "Code" + File.separator+ File.separator+ "check_IGM_MEA_version.R"+
                " javaPath=" + rootPath2Slash;
        System.out.println("cmd1 "+ cmd1 );
        System.out.println("Before systemCall ");
        //SystemCall.systemCall( cmd1,  outputTextArea);
        
        systemReturn=SystemCall.systemCall2( cmd1 );
        System.out.println("systemReturn "+ systemReturn.toString() );
        System.out.println("After systemCall ");

        return( systemReturn );
    }
    
    
}
